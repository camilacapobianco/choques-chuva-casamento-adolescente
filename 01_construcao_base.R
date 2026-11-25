# =====================================================================
# 01_chirps_municipios.R
# Precipitação mensal do CHIRPS v2.0 (1981-2016) agregada por município
# Saída: painel município x ano x mês com a precipitação média (mm/mês)
#
# Fontes:
#   - CHIRPS v2.0 mensal, 0,05°, via IRI Data Library (Univ. Columbia)
#   - Malha municipal 2016 do IBGE (geoftp.ibge.gov.br)
# =====================================================================

# ---- 0. Pacotes -------------------------------------------------------
pacotes <- c("terra", "sf", "exactextractr", "dplyr", "tidyr", "stringr")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

options(timeout = 1800)
options(download.file.method = "libcurl")

# ---- 1. Pastas --------------------------------------------------------
# ---- Caminhos (definidos no 00_rodar_tudo.R; o padrão abaixo vale ao rodar
#      este script sozinho) ----------------------------------------------
if (!exists("pasta")) pasta <- "C:/Users/camil/OneDrive/Documentos/trabalho de econo"
dir_brutos <- file.path(pasta, "dados", "brutos")
dir_inter  <- file.path(pasta, "dados", "intermediarios")
pasta_raw  <- file.path(pasta, "chirps")                   # downloads (não enviados)
pasta_tif  <- file.path(pasta, "chirps", "descompactados")  # rasters mensais do Brasil
dir.create(dir_inter, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tif, recursive = TRUE, showWarnings = FALSE)

# ---- 2. Baixar o CHIRPS mensal, recortado para o Brasil ---------------
# Os servidores da UCSB e do NOAA não responderam na nossa rede; a IRI
# Data Library tem o MESMO CHIRPS v2.0 mensal e deixa baixar só o
# retângulo do Brasil. Cada ano vem num .nc, que separamos em 12 GeoTIFFs.
anos_ref <- 1981:2015
anos     <- c(anos_ref, 2016)
meses    <- sprintf("%02d", 1:12)

iri <- paste0("https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/",
              ".v2p0/.monthly/.global/.precipitation/",
              "X/-74.5/-28.5/RANGEEDGES/",      # oeste -> leste
              "Y/5.5/-34/RANGEEDGES/")          # norte -> sul (ordem do IRI)

for (a in anos) {
  # Pula o ano se os 12 meses já estiverem salvos
  ja_tem <- file.exists(file.path(pasta_tif,
                                  sprintf("chirps-v2.0.%d.%s_BR.tif", a, meses)))
  if (all(ja_tem)) next
  
  url    <- paste0(iri, sprintf("T/(Jan%%20%d)/(Dec%%20%d)/RANGEEDGES/", a, a),
                   "data.nc")
  arq_nc <- tempfile(fileext = ".nc")
  
  ok <- tryCatch({
    download.file(url, arq_nc, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("Falhou ", a, ": ", conditionMessage(e))
    FALSE
  })
  if (!ok) next
  
  r <- rast(arq_nc)
  r <- classify(r, cbind(-Inf, 0 - 1e-9, NA))       # -9999 = sem dado -> NA
  # O IRI grava o tempo como "meses desde 1960"; como pedimos jan-dez
  # do ano a, a camada i é o mês i.
  if (nlyr(r) != 12) message("Atenção: ", a, " veio com ", nlyr(r), " camadas")
  for (i in seq_len(nlyr(r))) {
    nome <- sprintf("chirps-v2.0.%d.%02d_BR.tif", a, i)
    writeRaster(r[[i]], file.path(pasta_tif, nome), overwrite = TRUE)
  }
  unlink(arq_nc)
  message("OK: ", a)
}

# Conferência: 12 arquivos por ano
table(str_extract(list.files(pasta_tif, pattern = "_BR\\.tif$"), "\\d{4}"))

# ---- 3. Malha municipal 2016 (IBGE) -----------------------------------
# O pacote geobr falhou no download; baixamos o shapefile direto do IBGE.
zip_malha <- file.path(pasta_raw, "br_municipios_2016.zip")
dir_malha <- file.path(dir_inter, "malha_2016")

if (!file.exists(zip_malha)) {
  download.file(paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/",
                       "malhas_territoriais/malhas_municipais/municipio_2016/",
                       "Brasil/BR/br_municipios.zip"),
                zip_malha, mode = "wb")
}
if (!dir.exists(dir_malha)) unzip(zip_malha, exdir = dir_malha)

shp <- list.files(dir_malha, pattern = "\\.shp$", full.names = TRUE,
                  recursive = TRUE)

# Colunas originais: CD_GEOCMU (código IBGE 7 dígitos) e NM_MUNICIP.
# code_muni é o mesmo código do SIDRA e do PIB Municipal.
ufs <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
         "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
         "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
         "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")

munis <- st_read(shp[1], quiet = TRUE) |>
  st_transform(4326) |>                       # mesmo sistema do CHIRPS
  transmute(code_muni    = as.numeric(CD_GEOCMU),
            name_muni    = NM_MUNICIP,
            abbrev_state = ufs[substr(CD_GEOCMU, 1, 2)])

# Conferências da malha
nrow(munis)                       # 5.572 (5.570 municípios + 2 lagoas no RS)
sum(is.na(munis$abbrev_state))    # 0
head(munis$name_muni, 10)         # acentos corretos?

# ---- 4. Extrair a média de cada mês por município ---------------------
arquivos_br <- list.files(pasta_tif, pattern = "_BR\\.tif$", full.names = TRUE)
pilha <- rast(arquivos_br)
names(pilha) <- str_extract(basename(arquivos_br), "\\d{4}\\.\\d{2}")  # "2016.01"

# exact_extract pondera cada pixel pela fração da sua área dentro do
# município (o pixel tem ~5,5 km; muitos municípios são menores que isso).
# Leva alguns minutos.
precip_wide <- exact_extract(pilha, munis, fun = "mean",
                             append_cols = c("code_muni", "name_muni",
                                             "abbrev_state"))

# Formato largo (uma coluna por mês) -> longo (uma linha por mês)
precip <- precip_wide |>
  pivot_longer(starts_with("mean."), names_to = "ano_mes",
               values_to = "precip_mm") |>
  mutate(ano = as.integer(str_extract(ano_mes, "\\d{4}")),
         mes = as.integer(str_extract(ano_mes, "\\d{2}$"))) |>
  select(code_muni, name_muni, abbrev_state, ano, mes, precip_mm) |>
  arrange(code_muni, ano, mes)

# ---- 5. Conferências --------------------------------------------------
n_distinct(precip$code_muni)      # deve ser igual a nrow(munis)
table(precip$ano)                 # 66.864 linhas por ano (5.572 x 12)
sum(is.na(precip$precip_mm))      # 0
summary(precip$precip_mm)         # sem negativos; máximos plausíveis

# Total de 2016 em três capitais, para comparar com o INMET
precip |>
  filter(ano == 2016, code_muni %in% c(3550308, 2304400, 5300108)) |>
  group_by(name_muni) |>
  summarise(total_2016_mm = sum(precip_mm))

# Conferência visual
plot(pilha[["2016.01"]], main = "CHIRPS - jan/2016 (mm)")
plot(st_geometry(munis), add = TRUE, lwd = 0.05)

# ---- 6. Salvar --------------------------------------------------------
saveRDS(precip, file.path(dir_inter, "precip_municipio_mes.rds"))
write.csv(precip, file.path(dir_inter, "precip_municipio_mes.csv"),
          row.names = FALSE)

# Nas próximas etapas: precip <- readRDS(file.path(dir_inter, "precip_municipio_mes.rds"))

# =====================================================================
# 02_choque_chuva.R
# Constrói o SPI-12 de cada município para 2016:
#   - principal:  ano-safra (janela de 12 meses que termina em 2016)
#   - robustez:   ano civil (jan-dez de 2016)
# Entrada: precip_municipio_mes.rds (gerado pelo 01_chirps_municipios.R)
# Saída:   choque_chuva_2016.rds (uma linha por município)
# =====================================================================

library(dplyr)
library(tidyr)


# ---- 0. PARÂMETROS  -----------------------
# Último mês da janela do ano-safra. A janela termina nesse mês de 2016 e
# começa no mês seguinte de 2015. Ex.: 6 = jul/2015 a jun/2016.
mes_fim_safra <- 6           # jul/2015 a jun/2016

# Anos usados para estimar o "normal" de cada município: normal
# climatológica 1981-2010 (OMM), que deixa de fora a seca de 2012-2015
# no Nordeste. Cada ano é identificado pelo ano em que a janela TERMINA.
# Ano-safra: a primeira janela completa é jul/1981-jun/1982, e a última
# dentro de 1981-2010 é jul/2009-jun/2010 -> 29 janelas.
anos_ref_safra <- 1982:2010
anos_ref_civil <- 1981:2010  # 30 anos

# Limites do SPI para as dummies de choque (seca <= -lim; excesso >= +lim)
limiares <- c(1, 1.5)

ano_alvo <- 2016

if (anyNA(c(mes_fim_safra, anos_ref_safra, anos_ref_civil)))
  stop("Defina os parâmetros da seção 0 antes de rodar.")

# ---- 1. Dados -----------------------------------------------------------
precip <- readRDS(file.path(dir_inter, "precip_municipio_mes.rds")) |>
  filter(!code_muni %in% c(4300001, 4300002))   # lagoas do RS
stopifnot(n_distinct(precip$code_muni) == 5570)

# ---- 2. Chuva acumulada em janelas de 12 meses --------------------------
# Cada janela recebe o ano em que termina. Com mes_fim = 12, é o ano civil.
acumula_12m <- function(df, mes_fim) {
  df |>
    mutate(ano_janela = if_else(mes <= mes_fim, ano, ano + 1L)) |>
    group_by(code_muni, ano_janela) |>
    summarise(chuva_mm = sum(precip_mm), n_meses = n(), .groups = "drop") |>
    filter(n_meses == 12)                       # só janelas completas
}

# ---- 3. SPI (McKee et al., 1993; método recomendado pela OMM) -----------
# Ajusta uma distribuição gama à chuva acumulada do período de referência
# (estimadores de máxima verossimilhança aproximados de Thom, os usados no
# SPI) e converte a probabilidade acumulada do ano-alvo em um valor da
# normal padrão.
spi_gama <- function(x_ref, x) {
  m     <- mean(x_ref)
  A     <- log(m) - mean(log(x_ref))
  forma <- (1 + sqrt(1 + 4 * A / 3)) / (4 * A)
  escala <- m / forma
  qnorm(pgamma(x, shape = forma, scale = escala))
}

calcula_choque <- function(df, mes_fim, anos_ref, ano_alvo) {
  acumula_12m(df, mes_fim) |>
    group_by(code_muni) |>
    summarise(
      chuva_alvo = chuva_mm[ano_janela == ano_alvo],
      n_ref      = sum(ano_janela %in% anos_ref),
      media_ref  = mean(chuva_mm[ano_janela %in% anos_ref]),
      dp_ref     = sd(chuva_mm[ano_janela %in% anos_ref]),
      spi        = spi_gama(chuva_mm[ano_janela %in% anos_ref], chuva_alvo),
      .groups = "drop"
    ) |>
    mutate(z = (chuva_alvo - media_ref) / dp_ref)   # desvio padronizado simples, p/ comparação
}

# Classificação da OMM 
classifica_spi <- function(spi) {
  case_when(
    spi <= -2.00 ~ "Extremamente seco",
    spi <= -1.50 ~ "Muito seco",
    spi <= -1.00 ~ "Moderadamente seco",
    spi <   1.00 ~ "Próximo do normal",
    spi <   1.50 ~ "Moderadamente úmido",
    spi <   2.00 ~ "Muito úmido",
    TRUE         ~ "Extremamente úmido"
  ) |>
    factor(levels = c("Extremamente seco", "Muito seco", "Moderadamente seco",
                      "Próximo do normal", "Moderadamente úmido",
                      "Muito úmido", "Extremamente úmido"))
}

# ---- 4. Calcular para as duas janelas -----------------------------------
safra <- calcula_choque(precip, mes_fim_safra, anos_ref_safra, ano_alvo) |>
  rename_with(~ paste0(.x, "_safra"), -code_muni)

civil <- calcula_choque(precip, 12, anos_ref_civil, ano_alvo) |>
  rename_with(~ paste0(.x, "_civil"), -code_muni)

choque <- precip |>
  distinct(code_muni, name_muni, abbrev_state) |>
  left_join(safra, by = "code_muni") |>
  left_join(civil, by = "code_muni") |>
  mutate(cat_spi_safra = classifica_spi(spi_safra),
         cat_spi_civil = classifica_spi(spi_civil))

# Dummies de choque para cada limite e cada janela.
# Nomes: seca_safra_10 = SPI-safra <= -1,0; excesso_civil_15 = SPI-civil >= +1,5
for (lim in limiares) {
  sufixo <- sprintf("%02d", round(lim * 10))
  for (jan in c("safra", "civil")) {
    spi_col <- choque[[paste0("spi_", jan)]]
    choque[[paste0("seca_",    jan, "_", sufixo)]] <- as.integer(spi_col <= -lim)
    choque[[paste0("excesso_", jan, "_", sufixo)]] <- as.integer(spi_col >=  lim)
  }
}

# ---- 5. Conferências ------------------------------------------------------
nrow(choque)                                    # 5570
colSums(is.na(choque))                          # tudo 0
table(choque$n_ref_safra); table(choque$n_ref_civil)  # nº de anos no normal
summary(choque[, c("spi_safra", "spi_civil")])
table(choque$cat_spi_safra)
table(choque$cat_spi_civil)

# Quantos municípios em cada dummy (n e %)
choque |>
  summarise(across(starts_with(c("seca_", "excesso_")),
                   list(n = sum, pct = ~ round(100 * mean(.x), 1)))) |>
  pivot_longer(everything(), names_to = c("dummy", ".value"),
               names_pattern = "(.*)_(n|pct)$")

# Concordância entre as duas janelas e entre SPI e desvio simples
cor(choque$spi_safra, choque$spi_civil)
cor(choque$spi_safra, choque$z_safra)

# Média do SPI por UF: confira com o que se sabe do El Niño 2015-16
choque |>
  group_by(abbrev_state) |>
  summarise(spi_safra = mean(spi_safra), spi_civil = mean(spi_civil)) |>
  arrange(spi_safra) |>
  print(n = 27)

# ---- 6. Salvar ------------------------------------------------------------
saveRDS(choque, file.path(dir_inter, "choque_chuva_2016.rds"))

# =====================================================================
# 03_casamentos.R
# Importa a Tabela 4412 do SIDRA (Estatísticas do Registro Civil, 2016),
# constrói a proporção de casamentos com noiva menor de 18 anos por
# município e produz descritivas para escolher o corte de municípios
# com poucos casamentos.
# Saída: casamentos_2016.rds (uma linha por município, SEM corte ainda)
# =====================================================================

pacotes <- c("readxl", "readr", "dplyr", "tidyr", "stringr", "stringi", "ggplot2")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

arq_sidra  <- file.path(dir_brutos, "casamentos.csv")   

# ---- 1. Ler o CSV e isolar o bloco de CONTAGENS -----------------------
# O arquivo tem dois blocos: percentuais (em cima) e unidades (embaixo).
# Usamos só as contagens e calculamos a proporção nós mesmos.
# As linhas de título têm menos colunas que as de dados, então lemos o
# arquivo linha a linha e só depois separamos as colunas.
if (str_detect(arq_sidra, "\\.xlsx?$")) {
  # Planilha Excel (com os blocos de percentuais e de contagens)
  bruto <- readxl::read_excel(arq_sidra, col_names = FALSE, col_types = "text")
} else {
  # CSV exportado do SIDRA
  codif  <- guess_encoding(arq_sidra)$encoding[1]      # UTF-8 ou ISO-8859-1
  linhas <- read_lines(arq_sidra, locale = locale(encoding = codif))
  sep    <- if (sum(str_count(linhas, ";")) > sum(str_count(linhas, ","))) ";" else ","
  n_col  <- max(count.fields(textConnection(linhas), sep = sep, quote = "\""),
                na.rm = TRUE)
  bruto  <- read.table(text = linhas, sep = sep, quote = "\"", fill = TRUE,
                       comment.char = "", colClasses = "character",
                       col.names = paste0("V", seq_len(n_col)))
}
names(bruto)[1:4] <- c("muni", "idade_c1", "idade_c2", "n_2016")

# Faixas de idade do 2º cônjuge presentes no arquivo
table(bruto$idade_c2[str_detect(bruto$muni, "\\([A-Z]{2}\\)$")])

# Para separar os menores de 18, o arquivo precisa ter as idades
# 15, 16 e 17 anos SEPARADAS (não só a faixa "15 a 19 anos").
faltando <- setdiff(c("Menos de 15 anos", "15 anos", "16 anos", "17 anos"),
                    unique(bruto$idade_c2))
if (length(faltando) > 0)
  stop("Faltam no arquivo as faixas: ", paste(faltando, collapse = ", "),
       ". Exporte de novo do SIDRA com as idades simples.")

inicio <- which(str_detect(bruto$muni, "\\(Unidades\\)"))
stopifnot(length(inicio) == 1)            # deve achar exatamente 1 bloco

contagens <- bruto[(inicio + 1):nrow(bruto), ] |>
  filter(str_detect(muni, "\\([A-Z]{2}\\)$"))   # só linhas "Município (UF)"

stopifnot(all(contagens$idade_c1 == "Total"))   # 1º cônjuge sempre "Total"

# No SIDRA, "-" significa zero; "..." ou "X" seriam dado indisponível/sigilo.
para_num <- function(x) {
  x <- str_remove_all(str_trim(x), "\\.")   # tira separador de milhar (contagens são inteiras)
  ifelse(x == "-", 0, suppressWarnings(as.numeric(x)))
}

# Coluna n_2016 = casamentos OCORRIDOS em 2016 (a coluna seguinte são os
# registrados em 2016 mas ocorridos em anos anteriores; não usamos).
cas <- contagens |>
  mutate(n = para_num(n_2016),
         faixa = recode(idade_c2,
                        "Total"            = "total",
                        "Menos de 15 anos" = "n_m15",
                        "15 anos"          = "n_15",
                        "16 anos"          = "n_16",
                        "17 anos"          = "n_17",
                        "18 anos"          = "n_18")) |>
  select(muni, faixa, n) |>
  pivot_wider(names_from = faixa, values_from = n)


# Conferências da leitura
nrow(cas)                                   # perto de 5.570
colSums(is.na(cas[, -1]))                   # NA = célula com "..." ou "X"
sum(cas$total, na.rm = TRUE)                # total de casamentos ocorridos em 2016
with(cas, sum(n_m15 + n_15 + n_16 + n_17 > total, na.rm = TRUE))  # deve ser 0

# ---- 2. Variável dependente: noiva menor de 18 -----------------------
# Nos casamentos entre cônjuges masculino e feminino, o 2º cônjuge é a
cas <- cas |>
  mutate(cas_menor18  = n_m15 + n_15 + n_16 + n_17,
         prop_menor18 = if_else(total > 0, cas_menor18 / total, NA_real_),
         uf   = str_extract(muni, "(?<=\\()[A-Z]{2}(?=\\)$)"),
         nome = str_remove(muni, " \\([A-Z]{2}\\)$"))

# ---- 3. Código IBGE do município (o SIDRA veio só com nomes) ----------
# Casamos por nome + UF com a lista de municípios da malha 2016.
normaliza <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}

munis_lista <- readRDS(file.path(dir_inter, "choque_chuva_2016.rds")) |>
  distinct(code_muni, name_muni, abbrev_state) |>
  mutate(chave = paste(normaliza(name_muni), abbrev_state))

cas <- cas |> mutate(chave = paste(normaliza(nome), uf))

# Quem não casou dos dois lados (geralmente nomes que mudaram de grafia)
sem_par_sidra <- cas |> anti_join(munis_lista, by = "chave") |> select(muni, chave)
sem_par_malha <- munis_lista |> anti_join(cas, by = "chave") |> select(name_muni, abbrev_state, chave)
sem_par_sidra
sem_par_malha
# Se aparecerem pares, corrija aqui: "chave do SIDRA" = "chave da malha"
correcoes <- c(
  "SAO LUIZ DO ANAUA RR"         = "SAO LUIZ RR",
  "TABOCAO TO"                   = "FORTALEZA DO TABOCAO TO",
  "ASSU RN"                      = "ACU RN",
  "AREZ RN"                      = "ARES RN",
  "CAMPO GRANDE RN"              = "AUGUSTO SEVERO RN",   # mudou de nome
  "AMPARO DO SAO FRANCISCO SE"   = "AMPARO DE SAO FRANCISCO SE",
  "GRACCHO CARDOSO SE"           = "GRACHO CARDOSO SE",
  "BARAO DO MONTE ALTO MG"       = "BARAO DE MONTE ALTO MG",
  "DONA EUZEBIA MG"              = "DONA EUSEBIA MG",
  "SAO TOME DAS LETRAS MG"       = "SAO THOME DAS LETRAS MG",
  "SANTO ANTONIO DE LEVERGER MT" = "SANTO ANTONIO DO LEVERGER MT"
)
if (length(correcoes) > 0) {
  cas <- cas |>
    mutate(chave = if_else(chave %in% names(correcoes), correcoes[chave], chave))
}

cas <- cas |>
  inner_join(munis_lista |> select(code_muni, chave), by = "chave") |>
  select(code_muni, nome, uf, total, n_m15, n_15, n_16, n_17, any_of("n_18"),
         cas_menor18, prop_menor18)

nrow(cas)                                   # deve ser 5.570
stopifnot(nrow(cas) == 5570, !any(duplicated(cas$code_muni)))

# Municípios sem dado no Registro Civil ("..." no SIDRA) saem da amostra
sum(is.na(cas$total))                       # 498
cas <- cas |> filter(!is.na(total))
nrow(cas)                                   # 5.072

# Indicadores de amostra para cada corte de casamentos mínimos
# (principal = 20; robustez = 10 e 50)
cas <- cas |>
  mutate(amostra_c10 = as.integer(total >= 10),
         amostra_c20 = as.integer(total >= 20),
         amostra_c50 = as.integer(total >= 50))

cas |> summarise(across(starts_with("amostra_"), sum))   # municípios em cada amostra

# ---- 4. Descritivas para escolher o corte de "poucos casamentos" -----
# 4a. Distribuição do total de casamentos por município
summary(cas$total)
quantile(cas$total, c(.01, .05, .10, .25, .50), na.rm = TRUE)

ggplot(cas |> filter(total > 0), aes(total)) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  labs(x = "Casamentos em 2016 (escala log)", y = "Nº de municípios",
       title = "Distribuição do total de casamentos por município")

# 4b. Gráfico de funil: a proporção fica muito mais dispersa onde há poucos
# casamentos, só por acaso. As linhas tracejadas mostram o intervalo de
# 95% esperado se todos os municípios tivessem a proporção média nacional.
media_br <- sum(cas$cas_menor18, na.rm = TRUE) / sum(cas$total, na.rm = TRUE)
funil <- tibble(total = seq(1, max(cas$total, na.rm = TRUE), length.out = 2000)) |>
  mutate(sup = media_br + 1.96 * sqrt(media_br * (1 - media_br) / total),
         inf = pmax(0, media_br - 1.96 * sqrt(media_br * (1 - media_br) / total)))

ggplot(cas |> filter(total > 0), aes(total, prop_menor18)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_line(data = funil, aes(total, sup), linetype = 2) +
  geom_line(data = funil, aes(total, inf), linetype = 2) +
  geom_hline(yintercept = media_br) +
  scale_x_log10() +
  labs(x = "Casamentos em 2016 (escala log)",
       y = "Proporção com noiva < 18",
       title = "Gráfico de funil: proporção x tamanho do município")

# 4c. Dispersão da proporção por faixa de tamanho
cas |>
  filter(total > 0) |>
  mutate(faixa_total = cut(total, c(0, 10, 20, 30, 50, 100, 500, Inf),
                           right = FALSE)) |>
  ggplot(aes(faixa_total, prop_menor18)) +
  geom_boxplot() +
  labs(x = "Total de casamentos em 2016", y = "Proporção com noiva < 18")

cas |>
  filter(total > 0) |>
  mutate(faixa_total = cut(total, c(0, 10, 20, 30, 50, 100, 500, Inf),
                           right = FALSE)) |>
  group_by(faixa_total) |>
  summarise(n_munis = n(), media = mean(prop_menor18),
            dp = sd(prop_menor18), pct_zero = 100 * mean(prop_menor18 == 0))

# 4d. Quanto se perde com cada corte possível
tibble(corte = c(1, 5, 10, 20, 30, 50, 100)) |>
  rowwise() |>
  mutate(munis_excluidos = sum(cas$total < corte, na.rm = TRUE),
         pct_munis       = round(100 * munis_excluidos / nrow(cas), 1),
         pct_casamentos  = round(100 * sum(cas$total[cas$total < corte], na.rm = TRUE) /
                                   sum(cas$total, na.rm = TRUE), 2)) |>
  ungroup()

# ---- 5. Salvar (sem corte) --
saveRDS(cas, file.path(dir_inter, "casamentos_2016.rds"))

# =====================================================================
# 04_pib_agro.R
# Importa a Tabela 5938 do SIDRA (PIB dos Municípios, 2016) e calcula a
# participação da agropecuária no valor adicionado bruto (VAB) total.
# Saída: pib_agro_2016.rds (uma linha por município)
# =====================================================================

pacotes <- c("readxl", "readr", "dplyr", "tidyr", "stringr", "stringi", "ggplot2")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

arq_pib    <- file.path(dir_brutos, "pib.csv")  

# ---- 1. Ler o arquivo  -------------------------------
if (str_detect(arq_pib, "\\.xlsx?$")) {
  bruto <- read_excel(arq_pib, col_names = FALSE, col_types = "text")
} else {
  codif  <- guess_encoding(arq_pib)$encoding[1]
  linhas <- read_lines(arq_pib, locale = locale(encoding = codif))
  sep    <- if (sum(str_count(linhas, ";")) > sum(str_count(linhas, ","))) ";" else ","
  n_col  <- max(count.fields(textConnection(linhas), sep = sep, quote = "\""), na.rm = TRUE)
  bruto  <- read.table(text = linhas, sep = sep, quote = "\"", fill = TRUE,
                       comment.char = "", colClasses = "character",
                       col.names = paste0("V", seq_len(n_col)))
}
names(bruto)[1:2] <- c("muni", "v2016")   # 2ª coluna = ano 2016

# ---- 2. Separar os dois blocos (VAB total e VAB da agropecuária) ------
# Cada bloco começa na linha "Variável - ..." e vai até a próxima linha
# "Tabela ..." ou "Fonte ...".
extrai_bloco <- function(df, padrao_variavel) {
  ini <- which(str_detect(df$muni, padrao_variavel))
  stopifnot(length(ini) == 1)
  # "Fonte:" com dois-pontos, senão o município "Fonte Boa (AM)" encerra o bloco
  fins <- which(str_detect(df$muni, "^(Tabela \\d|Fonte:)") & seq_len(nrow(df)) > ini)
  fim  <- if (length(fins) > 0) min(fins) - 1 else nrow(df)
  df[(ini + 1):fim, ] |>
    filter(str_detect(muni, "\\([A-Z]{2}\\)$")) |>      # só municípios (tira "Brasil")
    mutate(valor = suppressWarnings(as.numeric(str_trim(v2016)))) |>
    select(muni, valor)
}

va_total <- extrai_bloco(bruto, "^Variável - Valor adicionado bruto a preços correntes total")
va_agro  <- extrai_bloco(bruto, "^Variável - Valor adicionado bruto a preços correntes da agropecuária")

nrow(va_total); nrow(va_agro)                   # ~5.570 cada
sum(is.na(va_total$valor)); sum(is.na(va_agro$valor))   # valores não numéricos

pib <- va_total |> rename(vab_total = valor) |>
  inner_join(va_agro |> rename(vab_agro = valor), by = "muni") |>
  mutate(part_agro = vab_agro / vab_total,          # em fração (0 a 1)
         uf   = str_extract(muni, "(?<=\\()[A-Z]{2}(?=\\)$)"),
         nome = str_remove(muni, " \\([A-Z]{2}\\)$"))

# ---- 3. Código IBGE (mesmo procedimento dos casamentos) ---------------
normaliza <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}

munis_lista <- readRDS(file.path(dir_inter, "choque_chuva_2016.rds")) |>
  distinct(code_muni, name_muni, abbrev_state) |>
  mutate(chave = paste(normaliza(name_muni), abbrev_state))

pib <- pib |> mutate(chave = paste(normaliza(nome), uf))

# Mesmas correções de nome dos casamentos (o PIB usa os nomes atuais)
correcoes <- c(
  "SAO LUIZ DO ANAUA RR"         = "SAO LUIZ RR",
  "TABOCAO TO"                   = "FORTALEZA DO TABOCAO TO",
  "ASSU RN"                      = "ACU RN",
  "AREZ RN"                      = "ARES RN",
  "CAMPO GRANDE RN"              = "AUGUSTO SEVERO RN",
  "AMPARO DO SAO FRANCISCO SE"   = "AMPARO DE SAO FRANCISCO SE",
  "GRACCHO CARDOSO SE"           = "GRACHO CARDOSO SE",
  "BARAO DO MONTE ALTO MG"       = "BARAO DE MONTE ALTO MG",
  "DONA EUZEBIA MG"              = "DONA EUSEBIA MG",
  "SAO TOME DAS LETRAS MG"       = "SAO THOME DAS LETRAS MG",
  "SANTO ANTONIO DE LEVERGER MT" = "SANTO ANTONIO DO LEVERGER MT"
)
pib <- pib |>
  mutate(chave = if_else(chave %in% names(correcoes), correcoes[chave], chave))

# Sobrou alguém sem par? (se sim, acrescente em `correcoes` e rode de novo)
pib |> anti_join(munis_lista, by = "chave") |> select(muni, chave)
munis_lista |> anti_join(pib, by = "chave") |> select(name_muni, abbrev_state)

pib <- pib |>
  inner_join(munis_lista |> select(code_muni, chave), by = "chave") |>
  select(code_muni, nome, uf, vab_total, vab_agro, part_agro)

# ---- 4. Conferências ----------------------------------------------------
nrow(pib)                                        # 5.570
stopifnot(!any(duplicated(pib$code_muni)))
summary(pib$part_agro)                           # entre 0 e 1
sum(pib$part_agro < 0 | pib$part_agro > 1, na.rm = TRUE)   # deve ser 0

# Participação nacional (soma dos municípios) - compare com o IBGE
sum(pib$vab_agro, na.rm = TRUE) / sum(pib$vab_total, na.rm = TRUE)

# Média por UF: estados agrícolas (MT, MS, TO, GO...) devem aparecer no topo
pib |> group_by(uf) |>
  summarise(part_agro_media = round(mean(part_agro, na.rm = TRUE), 3)) |>
  arrange(desc(part_agro_media)) |> print(n = 27)

ggplot(pib, aes(part_agro)) +
  geom_histogram(bins = 50) +
  labs(x = "Participação da agropecuária no VAB (2016)", y = "Nº de municípios")

# ---- 5. Salvar ----------------------------------------------------------
saveRDS(pib, file.path(dir_inter, "pib_agro_2016.rds"))

# =====================================================================
# 04b_pib_agro_anos.R
# Importa a Tabela 5938 do SIDRA (PIB dos Municípios) com VÁRIOS anos e
# monta uma base longa: município x ano, com VAB total, VAB da
# agropecuária e participação da agropecuária.
# Saída: pib_agro_anos.rds
# =====================================================================

pacotes <- c("readr", "dplyr", "tidyr", "stringr", "stringi")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

arq_pib    <- file.path(dir_brutos, "tabela5938.csv")

# ---- 1. Ler o CSV ------------------------------------------------------
codif  <- guess_encoding(arq_pib)$encoding[1]
linhas <- read_lines(arq_pib, locale = locale(encoding = codif))
sep    <- if (sum(str_count(linhas, ";")) > sum(str_count(linhas, ","))) ";" else ","
n_col  <- max(count.fields(textConnection(linhas), sep = sep, quote = "\""), na.rm = TRUE)
bruto  <- read.table(text = linhas, sep = sep, quote = "\"", fill = TRUE,
                     comment.char = "", colClasses = "character",
                     col.names = paste0("V", seq_len(n_col)))
names(bruto)[1] <- "muni"

# ---- 2. Extrair um bloco ("Variável - ...") com todos os anos ----------
extrai_bloco <- function(df, padrao_variavel) {
  ini <- which(str_detect(df$muni, padrao_variavel))
  stopifnot(length(ini) == 1)
  # "Fonte:" com dois-pontos, senão o município "Fonte Boa (AM)" encerra o bloco
  fins <- which(str_detect(df$muni, "^(Tabela \\d|Fonte:)") & seq_len(nrow(df)) > ini)
  fim  <- if (length(fins) > 0) min(fins) - 1 else nrow(df)
  bloco <- df[(ini + 1):fim, ]
  
  # Linha de cabeçalho com os anos (ex.: "Brasil e Município";"2008";"2009";...)
  cab <- bloco |>
    filter(if_any(-muni, ~ str_detect(.x, "^\\d{4}$"))) |>
    slice(1) |>
    unlist()
  col_anos <- names(cab)[str_detect(cab, "^\\d{4}$")]
  
  bloco |>
    filter(str_detect(muni, "\\([A-Z]{2}\\)$")) |>      # só municípios
    select(muni, all_of(col_anos)) |>
    pivot_longer(-muni, names_to = "col", values_to = "valor") |>
    mutate(ano   = as.integer(cab[col]),
           valor = str_trim(valor),
           valor = if_else(valor == "-", "0", valor),            # "-" = zero absoluto
           valor = suppressWarnings(as.numeric(valor))) |>       # "..." vira NA
    select(muni, ano, valor)
}

va_total <- extrai_bloco(bruto, "^Variável - Valor adicionado bruto a preços correntes total")
va_agro  <- extrai_bloco(bruto, "^Variável - Valor adicionado bruto a preços correntes da agropecuária")

# Conferências da leitura: municípios por ano e valores ausentes por ano
va_total |> group_by(ano) |> summarise(n = n(), na = sum(is.na(valor)))
va_agro  |> group_by(ano) |> summarise(n = n(), na = sum(is.na(valor)))

pib <- va_total |> rename(vab_total = valor) |>
  inner_join(va_agro |> rename(vab_agro = valor), by = c("muni", "ano")) |>
  mutate(part_agro = vab_agro / vab_total,
         uf   = str_extract(muni, "(?<=\\()[A-Z]{2}(?=\\)$)"),
         nome = str_remove(muni, " \\([A-Z]{2}\\)$"))

# ---- 3. Código IBGE (mesmo procedimento do 04_pib_agro.R) --------------
normaliza <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}

munis_lista <- readRDS(file.path(dir_inter, "choque_chuva_2016.rds")) |>
  distinct(code_muni, name_muni, abbrev_state) |>
  mutate(chave = paste(normaliza(name_muni), abbrev_state))

correcoes <- c(
  "SAO LUIZ DO ANAUA RR"         = "SAO LUIZ RR",
  "TABOCAO TO"                   = "FORTALEZA DO TABOCAO TO",
  "ASSU RN"                      = "ACU RN",
  "AREZ RN"                      = "ARES RN",
  "CAMPO GRANDE RN"              = "AUGUSTO SEVERO RN",
  "AMPARO DO SAO FRANCISCO SE"   = "AMPARO DE SAO FRANCISCO SE",
  "GRACCHO CARDOSO SE"           = "GRACHO CARDOSO SE",
  "BARAO DO MONTE ALTO MG"       = "BARAO DE MONTE ALTO MG",
  "DONA EUZEBIA MG"              = "DONA EUSEBIA MG",
  "SAO TOME DAS LETRAS MG"       = "SAO THOME DAS LETRAS MG",
  "SANTO ANTONIO DE LEVERGER MT" = "SANTO ANTONIO DO LEVERGER MT"
)
pib <- pib |>
  mutate(chave = paste(normaliza(nome), uf),
         chave = if_else(chave %in% names(correcoes), correcoes[chave], chave))

# Sobrou alguém sem par? (deve sair vazio)
pib |> distinct(muni, chave) |> anti_join(munis_lista, by = "chave")

pib <- pib |>
  inner_join(munis_lista |> select(code_muni, chave), by = "chave") |>
  select(code_muni, nome, uf, ano, vab_total, vab_agro, part_agro) |>
  arrange(code_muni, ano)

# ---- 4. Conferências ----------------------------------------------------
stopifnot(!any(duplicated(pib[, c("code_muni", "ano")])))
pib |> count(ano)                                # municípios por ano
# Municípios criados em 2013 não têm dado nos anos anteriores: aparecem com NA
pib |> group_by(ano) |> summarise(sem_dado = sum(is.na(part_agro)))
# Casos com VAB negativo ou participação fora de [0, 1]
pib |> filter(vab_total < 0 | vab_agro < 0 | part_agro < 0 | part_agro > 1)

# ---- 5. Salvar ----------------------------------------------------------
saveRDS(pib, file.path(dir_inter, "pib_agro_anos.rds"))

# =====================================================================
# 05_censo2010.R
# Controles municipais do Censo 2010, a partir da consulta ao Atlas do
# Desenvolvimento Humano no Brasil (PNUD/Ipea/FJP).
# Substitui o 05_censo2022.R.
# Saída: censo2010.rds (uma linha por município)
# =====================================================================

pacotes <- c("readxl", "readr", "dplyr", "stringr", "stringi")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

arq_atlas  <- file.path(dir_brutos, "atlas.xlsx")  

# ---- 1. Ler a planilha ----------------------------------------------------
# Colunas exportadas pelo Atlas (nesta ordem):
#   Territorialidades | População total 2010 | População urbana 2010 |
#   Taxa de analfabetismo - 15 anos ou mais 2010 | Renda per capita 2010 |
#   % de extremamente pobres 2010 | % de pobres 2010
if (str_detect(arq_atlas, "\\.xlsx?$")) {
  bruto <- read_excel(arq_atlas, col_types = "text")
} else {
  codif <- guess_encoding(arq_atlas)$encoding[1]
  linha1 <- read_lines(arq_atlas, n_max = 1, locale = locale(encoding = codif))
  sep <- if (str_count(linha1, ";") > str_count(linha1, ",")) ";" else ","
  bruto <- read_delim(arq_atlas, delim = sep, col_types = cols(.default = "c"),
                      locale = locale(encoding = codif))
}
bruto <- bruto[, 1:7]
names(bruto)                                   # confira se a ordem bate
names(bruto) <- c("muni", "pop_total", "pop_urbana", "analf15",
                  "renda", "ext_pobres", "pobres")

# Números: o Excel já traz ponto decimal; um CSV brasileiro traz vírgula
num <- function(x) as.numeric(str_replace(str_trim(x), ",", "."))

atlas <- bruto |>
  filter(str_detect(muni, "\\([A-Z]{2}\\)$")) |>     # tira "Brasil" e o rodapé
  transmute(nome = str_remove(muni, " \\([A-Z]{2}\\)$"),
            uf   = str_extract(muni, "(?<=\\()[A-Z]{2}(?=\\)$)"),
            pop_total      = num(pop_total),
            pop_urbana     = num(pop_urbana),
            pct_urbana     = 100 * pop_urbana / pop_total,
            alfab_15mais   = 100 - num(analf15),       # % alfabetizados
            renda_pc_media = num(renda),               # R$ de ago/2010
            pct_pobres     = num(pobres),              # renda pc <= R$ 140
            pct_ext_pobres = num(ext_pobres))          # renda pc <= R$ 70

nrow(atlas)                                   # 5.565 municípios em 2010
colSums(is.na(atlas))                         # tudo 0

# ---- 2. Código IBGE (o Atlas veio só com nomes) ---------------------------
normaliza <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}

munis_lista <- readRDS(file.path(dir_inter, "choque_chuva_2016.rds")) |>
  distinct(code_muni, name_muni, abbrev_state) |>
  mutate(chave = paste(normaliza(name_muni), abbrev_state))

atlas <- atlas |> mutate(chave = paste(normaliza(nome), uf))

# Correções de grafia: "chave do Atlas" = "chave da malha 2016".
# As 11 primeiras são as mesmas dos casamentos; só valem se a grafia aparecer.
correcoes <- c(
  # As 11 dos casamentos
  "SAO LUIZ DO ANAUA RR"         = "SAO LUIZ RR",
  "TABOCAO TO"                   = "FORTALEZA DO TABOCAO TO",
  "ASSU RN"                      = "ACU RN",
  "AREZ RN"                      = "ARES RN",
  "CAMPO GRANDE RN"              = "AUGUSTO SEVERO RN",
  "AMPARO DO SAO FRANCISCO SE"   = "AMPARO DE SAO FRANCISCO SE",
  "GRACCHO CARDOSO SE"           = "GRACHO CARDOSO SE",
  "BARAO DO MONTE ALTO MG"       = "BARAO DE MONTE ALTO MG",
  "DONA EUZEBIA MG"              = "DONA EUSEBIA MG",
  "SAO TOME DAS LETRAS MG"       = "SAO THOME DAS LETRAS MG",
  "SANTO ANTONIO DE LEVERGER MT" = "SANTO ANTONIO DO LEVERGER MT",
  # Grafias diferentes no Atlas (2010) e na malha (2016)
  "EMBU SP"                      = "EMBU DAS ARTES SP",
  "PARATI RJ"                    = "PARATY RJ",
  "ELDORADO DOS CARAJAS PA"      = "ELDORADO DO CARAJAS PA",
  "SANTA ISABEL DO PARA PA"      = "SANTA IZABEL DO PARA PA",
  "ITAPAGE CE"                   = "ITAPAJE CE",
  "IGUARACI PE"                  = "IGUARACY PE",
  "MUQUEM DE SAO FRANCISCO BA"   = "MUQUEM DO SAO FRANCISCO BA",
  "SANTA TERESINHA BA"           = "SANTA TEREZINHA BA",
  "BRASOPOLIS MG"                = "BRAZOPOLIS MG",
  "FLORINIA SP"                  = "FLORINEA SP",
  "SAO LUIS DO PARAITINGA SP"    = "SAO LUIZ DO PARAITINGA SP",
  "POXOREO MT"                   = "POXOREU MT",
  # Municípios que mudaram de nome
  "PRESIDENTE JUSCELINO RN"      = "SERRA CAIADA RN",
  "SANTAREM PB"                  = "JOCA CLAUDINO PB",
  "SERIDO PB"                    = "SAO VICENTE DO SERIDO PB"
)
atlas <- atlas |>
  mutate(chave = if_else(chave %in% names(correcoes), correcoes[chave], chave))

# Quem não casou dos dois lados
sem_par_atlas <- atlas |> anti_join(munis_lista, by = "chave") |> select(nome, uf, chave)
sem_par_malha <- munis_lista |> anti_join(atlas, by = "chave") |>
  select(code_muni, name_muni, abbrev_state, chave)
sem_par_atlas      # deve sair vazio
sem_par_malha      # devem sobrar só os 5 municípios criados em 2013
# Só podem sobrar sem par os 5 municípios criados em 2013
stopifnot(nrow(sem_par_atlas) == 0, nrow(sem_par_malha) == 5)

censo <- munis_lista |>
  select(code_muni, name_muni, abbrev_state, chave) |>
  left_join(atlas |> select(-nome, -uf), by = "chave") |>
  select(-chave)

# ---- 3. Conferências -------------------------------------------------------
nrow(censo)                                    # 5.570
stopifnot(!any(duplicated(censo$code_muni)))
sum(is.na(censo$renda_pc_media))               # 5 (criados em 2013)

summary(censo[, c("alfab_15mais", "pct_urbana", "renda_pc_media",
                  "pct_pobres", "pct_ext_pobres")])

# Valores nacionais: compare com a linha "Brasil" do próprio arquivo
# (urbanização 84,4%; população 190.755.799)
censo |> summarise(pop_br        = sum(pop_total, na.rm = TRUE),
                   pct_urbana_br = 100 * sum(pop_urbana, na.rm = TRUE) /
                     sum(pop_total, na.rm = TRUE))

# ---- 4. Salvar ------------------------------------------------------------
saveRDS(censo, file.path(dir_inter, "censo2010.rds"))

# =====================================================================
# 06_base_final.R
# Junta as quatro bases pelo código IBGE do município.
# Ponto de partida: os 5.072 municípios com dado de casamento em 2016.
# Saída: base_final.rds / base_final.csv
# =====================================================================

library(dplyr)


casamentos <- readRDS(file.path(dir_inter, "casamentos_2016.rds"))
choque     <- readRDS(file.path(dir_inter, "choque_chuva_2016.rds"))
pib        <- readRDS(file.path(dir_inter, "pib_agro_2016.rds"))
censo      <- readRDS(file.path(dir_inter, "censo2010.rds"))   # controles de 2010 (Atlas)

base <- casamentos |>
  left_join(choque |> select(-name_muni, -abbrev_state), by = "code_muni") |>
  left_join(pib    |> select(code_muni, vab_total, vab_agro, part_agro), by = "code_muni") |>
  left_join(censo  |> select(-name_muni, -abbrev_state), by = "code_muni") |>
  # Unidades: as proporções vêm em fração (0 a 1); os controles do Censo
  # vêm em % (0 a 100). Criamos versões em % para tudo ficar na mesma escala.
  mutate(pct_menor18 = 100 * prop_menor18,
         pct_agro    = 100 * part_agro)

# ---- Conferências -------------------------------------------------------
nrow(base)                                   # 5.072
stopifnot(!any(duplicated(base$code_muni)))
colSums(is.na(base))[colSums(is.na(base)) > 0]   # só os controles de 2010, nos
# municípios criados em 2013
base |> summarise(across(starts_with("amostra_"), sum))   # 4.731 / 4.079 / 2.599

# ---- Salvar -------------------------------------------------------------
write.csv(base, file.path(dir_inter, "base_final.csv"), row.names = FALSE)



# =====================================================================
# 07_agro_medidas.R
# Medidas de dependência agrícola anteriores ao choque e regra de amostra,
# juntadas à base final.
# (Blocos escritos pela Camila, reunidos num único arquivo.)
# =====================================================================

library(dplyr)


pib_agro_anos <- readRDS(file.path(dir_inter, "pib_agro_anos.rds"))
base_final    <- read.csv(file.path(dir_inter, "base_final.csv"))
base_final$code_muni    <- as.numeric(base_final$code_muni)
pib_agro_anos$code_muni <- as.numeric(pib_agro_anos$code_muni)

# 1) As três medidas, cada uma com o próprio nome de coluna (em %)
#    + nº de anos com dado em 2002–2011 (usado na regra)
agro_medidas <- pib_agro_anos |>
  group_by(code_muni, nome, uf) |>
  summarise(
    agro_2008_2011   = 100 * mean(part_agro[ano >= 2008 & ano <= 2011], na.rm = TRUE),
    agro_2010        = 100 * mean(part_agro[ano == 2010], na.rm = TRUE),
    agro_2002_2011   = 100 * mean(part_agro[ano >= 2002 & ano <= 2011], na.rm = TRUE),
    n_anos_2002_2011 = sum(!is.na(part_agro[ano >= 2002 & ano <= 2011])),
    .groups = "drop"
  )

# 2) Regra de amostra como coluna 0/1: dado nos 10 anos de 2002 a 2011
agro_medidas <- agro_medidas |>
  mutate(amostra_agro = as.integer(n_anos_2002_2011 == 10))

# 3) Juntar tudo à base por code_muni
base_agro <- base_final |>
  left_join(
    agro_medidas |>
      select(code_muni, agro_2008_2011, agro_2010, agro_2002_2011, amostra_agro),
    by = "code_muni"
  )

# ---- Conferência de sanidade ----

# (a) Medida 2008–2011 vs participação de 2016, na amostra principal
amostra_principal <- base_agro |>
  filter(amostra_c20 == 1, uf != "DF")

summary(amostra_principal[, c("agro_2008_2011", "pct_agro")])

# (b) Quantos municípios têm regra = 0
table(agro_medidas$amostra_agro)
sum(amostra_principal$amostra_agro == 0, na.rm = TRUE)

# (c) Quem são os municípios com regra = 0 na amostra principal
amostra_principal |>
  filter(amostra_agro == 0) |>
  select(code_muni, nome, uf, total, agro_2008_2011, renda_pc_media)

# (d) Municípios sem controles do Censo 2010
base_agro |>
  filter(is.na(renda_pc_media)) |>
  select(code_muni, nome, uf, total, amostra_c20)

# ---- Salvar (linha acrescentada na junção dos blocos) ----
saveRDS(base_agro, file.path(dir_inter, "base_agro.rds"))



# =====================================================================
# 08_microrregioes.R
# Baixa da API de Localidades do IBGE a microrregião, a mesorregião e a
# região geográfica imediata de cada município, para usar como grupos
# nos erros-padrão agrupados (robustez).
# Saída: geo_municipios.rds (uma linha por município)
# =====================================================================

pacotes <- c("jsonlite", "dplyr")
instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))


# ---- 1. Download -------------------------------------------------------
url <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"
api <- fromJSON(url, flatten = TRUE)   # uma linha por município

names(api)                             # confira os nomes das colunas

# ---- 2. Selecionar os códigos -------------------------------------------
# Microrregião e mesorregião: divisão de 1989/1990, vigente em 2016.
# Região imediata: divisão de 2017 (alternativa, se preferir).
pega <- function(df, col) if (col %in% names(df)) df[[col]] else NA

geo <- tibble(
  code_muni  = as.numeric(api$id),
  micro_id   = pega(api, "microrregiao.id"),
  micro_nome = pega(api, "microrregiao.nome"),
  meso_id    = pega(api, "microrregiao.mesorregiao.id"),
  meso_nome  = pega(api, "microrregiao.mesorregiao.nome"),
  rgi_id     = pega(api, "regiao-imediata.id"),
  rgi_nome   = pega(api, "regiao-imediata.nome")
)

# ---- 3. Conferências ----------------------------------------------------
nrow(geo)                                   # ~5.570 (ou 5.571, se houver município novo)
stopifnot(!any(duplicated(geo$code_muni)))
colSums(is.na(geo))                         # municípios sem código de grupo

# Cobertura na amostra principal
base_agro <- readRDS(file.path(dir_inter, "base_agro.rds"))
amostra <- base_agro |>
  filter(amostra_c20 == 1, uf != "DF", amostra_agro == 1,
         !is.na(renda_pc_media)) |>
  left_join(geo, by = "code_muni")

nrow(amostra)                               # 4.075
sum(is.na(amostra$micro_id))                # deve ser 0
c(microrregioes = n_distinct(amostra$micro_id),
  mesorregioes  = n_distinct(amostra$meso_id),
  reg_imediatas = n_distinct(amostra$rgi_id))   # nº de grupos na amostra

# ---- 4. Salvar -----------------------------------------------------------
saveRDS(geo, file.path(dir_inter, "geo_municipios.rds"))