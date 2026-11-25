# Análise: choques de chuva e casamento de noivas menores de 18 anos (2016)
# 0. Pacotes e caminhos
library(dplyr)
library(tidyr)
library(sandwich)
library(lmtest)
library(car)
library(modelsummary)
library(sf)
library(ggplot2)

# ---- Caminhos (definidos no 00_rodar_tudo.R; o padrão abaixo vale ao rodar
#      este script sozinho) ----------------------------------------------
if (!exists("pasta")) pasta <- "C:/Users/camil/OneDrive/Documentos/trabalho de econo"
dir_brutos <- file.path(pasta, "dados", "brutos")
dir_inter  <- file.path(pasta, "dados", "intermediarios")

# 1. Dados e amostra final
#    >= 20 casamentos em 2016, sem DF, controles completos, participação agrícola 2008–2011
base_agro <- readRDS(file.path(dir_inter, "base_agro.rds"))
geo       <- readRDS(file.path(dir_inter, "geo_municipios.rds"))

vars_modelo <- c("prop_menor18", "seca_safra_10", "excesso_safra_10",
                 "pct_urbana", "alfab_15mais", "renda_pc_media", "pct_pobres",
                 "agro_2008_2011")

d <- base_agro |>
  filter(amostra_c20 == 1, uf != "DF") |>
  filter(complete.cases(across(all_of(vars_modelo)))) |>
  filter(amostra_agro == 1)

stopifnot(nrow(d) == 4075)
cat("N final:", nrow(d), "\n")

# 2. Variáveis do modelo
#    Sufixo _c: centrada na média da amostra final.
#    agro_c10: participação agrícola centrada, em unidades de 10 p.p.
d <- d |>
  mutate(
    pct_menor18 = 100 * prop_menor18,
    agro_c10    = (agro_2008_2011 - mean(agro_2008_2011)) / 10,
    urb_c       = pct_urbana   - mean(pct_urbana),
    alfab_c     = alfab_15mais - mean(alfab_15mais),
    log_renda_c = log(renda_pc_media) - mean(log(renda_pc_media)),
    pobreza_c   = pct_pobres   - mean(pct_pobres)
  )
cat("Média de agro_2008_2011 usada para centrar:", round(mean(d$agro_2008_2011), 2), "%\n")

# 3. Microrregião (agrupamento para o erro principal)
d_micro <- d |> left_join(geo |> select(code_muni, micro_id), by = "code_muni")
stopifnot(nrow(d_micro) == nrow(d), !anyNA(d_micro$micro_id))
cat("Microrregiões:", n_distinct(d_micro$micro_id), "\n")

# 4. Estatísticas descritivas
#    Seca e excesso são dummies: a média é a proporção de municípios.
desc <- d_micro |>
  select(
    `Casamentos com noiva < 18 (%)` = pct_menor18,
    `Seca (dummy)`                   = seca_safra_10,
    `Excesso (dummy)`                = excesso_safra_10,
    `Agropecuária 2008–2011 (%)`     = agro_2008_2011,
    `Urbanização (%)`                = pct_urbana,
    `Alfabetização 15+ (%)`          = alfab_15mais,
    `Renda per capita (R$)`          = renda_pc_media,
    `Pobres (%)`                     = pct_pobres
  ) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "x") |>
  group_by(variavel) |>
  summarise(n = sum(!is.na(x)), media = mean(x), dp = sd(x),
            min = min(x), mediana = median(x), max = max(x), .groups = "drop")

print(desc, digits = 3)

#5. Figura 1: mapa SPI (amostra principal) e regiões, medianas por região e figura 2: histogramas da renda
# Mapa do SPI-12 (amostra principal)
#malha que usamos pra montar a base
malha <- st_read(file.path(dir_inter, "malha_2016", "BRMUE250GC_SIR.shp")) |>
  filter(!as.character(CD_GEOCMU) %in% c("4300001", "4300002"))   # lagoas do RS, não são municípios
stopifnot(nrow(malha) == 5570)   # confere que sobraram só os municípios

#junta com a amostra e ajusta o código numérico
mapa_spi <- malha |>
  mutate(CD_GEOCMU = as.numeric(CD_GEOCMU)) |>
  left_join(d |> select(code_muni, spi_safra),
            by = c("CD_GEOCMU" = "code_muni"))
#fazer o mapa
ggplot(mapa_spi) +
  geom_sf(aes(fill = spi_safra), color = NA) +
  scale_fill_steps2(
    low = rgb(0.45, 0.16, 0.32), mid = "grey92", high = "#F5CC59",
    midpoint = 0,
    breaks = c(-2, -1.5, -1, 1, 1.5, 2),   # o intervalo normal (−1 a +1) fica com uma cor só
    limits = c(-3, 3), oob = scales::squish,
    labels = c("−2", "−1,5", "−1", "+1", "+1,5", "+2"),
    na.value = "grey60",
    name = "SPI-12 (ano-safra)"
  ) +
  guides(fill = guide_coloursteps(barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"))) +
  labs(caption = paste0(
    "Seca: SPI ≤ −1 (tons de roxo). Excesso: SPI ≥ +1 (tons de amarelo).\n",
    "Cinza claro: chuva normal. Cinza médio: fora da amostra principal.")) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    plot.caption    = element_text(size = 9, hjust = 0.5)
  )

ggsave(file.path(pasta, "fig1_mapa_spi12.png"), width = 6.5, height = 6.5, dpi = 300)


mapa_regiao <- c(
  AC = "Norte", AP = "Norte", AM = "Norte", PA = "Norte", RO = "Norte", RR = "Norte", TO = "Norte",
  AL = "Nordeste", BA = "Nordeste", CE = "Nordeste", MA = "Nordeste", PB = "Nordeste",
  PE = "Nordeste", PI = "Nordeste", RN = "Nordeste", SE = "Nordeste",
  GO = "Centro-Oeste", MT = "Centro-Oeste", MS = "Centro-Oeste",
  ES = "Sudeste", MG = "Sudeste", RJ = "Sudeste", SP = "Sudeste",
  PR = "Sul", RS = "Sul", SC = "Sul"
)

d_micro <- d_micro |>
  mutate(regiao = unname(mapa_regiao[as.character(uf)]))
stopifnot(!anyNA(d_micro$regiao))

medianas_regiao <- d_micro |>
  group_by(regiao) |>
  summarise(
    n                   = n(),
    mediana_renda       = median(renda_pc_media),
    mediana_log_renda   = median(log(renda_pc_media)),
    mediana_pct_menor18 = median(pct_menor18),
    share_seca          = mean(seca_safra_10),
    share_excesso       = mean(excesso_safra_10),
    .groups = "drop"
  )

print(medianas_regiao, digits = 3)

png(file.path(pasta, "figura2_renda.png"), width = 2000, height = 900, res = 200)
par(mfrow = c(1, 2))
hist(d$renda_pc_media, breaks = 50, col = rgb(0.45, 0.16, 0.32), border = "white",
     main = "Nível", xlab = "Renda per capita (R$ mensais, 2010)",
     ylab = "Nº de municípios")
hist(log(d$renda_pc_media), breaks = 50, col = rgb(0.45, 0.16, 0.32), border = "white",
     main = "Log", xlab = "log da renda per capita (R$)",
     ylab = "Nº de municípios")
dev.off()



# 6. Descritivas por condição de chuva
desc_condicao <- d_micro |>
  mutate(condicao = case_when(
    seca_safra_10 == 1    ~ "Seca",
    excesso_safra_10 == 1 ~ "Excesso",
    TRUE                  ~ "Normal"
  )) |>
  group_by(condicao) |>
  summarise(
    n             = n(),
    y_media       = mean(pct_menor18),
    y_mediana     = median(pct_menor18),
    agro_media    = mean(agro_2008_2011),
    urb_media     = mean(pct_urbana),
    alfab_media   = mean(alfab_15mais),
    renda_mediana = median(renda_pc_media),
    pobres_media  = mean(pct_pobres),
    .groups = "drop"
  )
print(desc_condicao, digits = 3)

# Formato com vírgula, 1 casa decimal (para descritivas, 3 casas é excesso)
fmt_desc <- function(x, d = 1) format(round(x, d), nsmall = d, decimal.mark = ",")

rotulos <- c(
  n             = "Observações",
  y_media       = "Casamentos com noiva < 18 anos (média, p.p.)",
  y_mediana     = "Casamentos com noiva < 18 anos (mediana, p.p.)",
  agro_media    = "Dependência agrícola 2008–2011 (média, %)",
  urb_media     = "Urbanização (média, %)",
  alfab_media   = "Alfabetização 15+ anos (média, %)",
  renda_mediana = "Renda domiciliar per capita (mediana, R$)",
  pobres_media  = "Pobreza (média, %)"
)

ordem <- c("Normal", "Seca", "Excesso")

desc_t <- desc_condicao |>
  mutate(
    n             = format(n, big.mark = ".", decimal.mark = ","),
    renda_mediana = format(round(renda_mediana), big.mark = ".", decimal.mark = ","),
    across(c(y_media, y_mediana, agro_media, urb_media, alfab_media, pobres_media),
           ~ fmt_desc(.x, 1))
  ) |>
  pivot_longer(-condicao, names_to = "variavel", values_to = "valor") |>
  pivot_wider(names_from = condicao, values_from = valor) |>
  mutate(variavel = unname(rotulos[variavel])) |>
  select(variavel, all_of(ordem)) |>
  rename(`Variável` = variavel)

datasummary_df(
  desc_t,
  title  = "Características dos municípios por condição de chuva (SPI-12, ano-safra 2015–2016)",
  notes  = "Valores brutos, sem controles. Médias, exceto a renda, reportada pela mediana por assimetria. A mediana da variável dependente também é reportada. Condição Normal: SPI entre −1 e +1.",
  output = file.path(pasta, "tabela_descritiva.docx")
)

# 7. Modelos
#    Principal: renda em log, sem pobreza
#    Robustez: pobreza no lugar da renda
modelo_principal <- lm(
  pct_menor18 ~ seca_safra_10 + excesso_safra_10 + agro_c10 +
    seca_safra_10:agro_c10 + excesso_safra_10:agro_c10 +
    urb_c + alfab_c + log_renda_c + factor(uf),
  data = d_micro
)

modelo_pobreza <- lm(
  pct_menor18 ~ seca_safra_10 + excesso_safra_10 + agro_c10 +
    seca_safra_10:agro_c10 + excesso_safra_10:agro_c10 +
    urb_c + alfab_c + pobreza_c + factor(uf),
  data = d_micro
)

# 8. Matrizes de variância
#    Principal: erro agrupado por microrregião
#    Referência: HC1 sem agrupamento
V_micro     <- vcovCL(modelo_principal, cluster = d_micro$micro_id, type = "HC1")
V_hc1       <- vcovHC(modelo_principal, type = "HC1")
V_micro_pob <- vcovCL(modelo_pobreza,   cluster = d_micro$micro_id, type = "HC1")
V_hc1_pob   <- vcovHC(modelo_pobreza,   type = "HC1")

# 9. Resultados principais e testes conjuntos
linhas <- c("seca_safra_10", "excesso_safra_10", "agro_c10",
            "seca_safra_10:agro_c10", "excesso_safra_10:agro_c10",
            "urb_c", "alfab_c", "log_renda_c")

ct_m <- coeftest(modelo_principal, vcov. = V_micro)
ct_h <- coeftest(modelo_principal, vcov. = V_hc1)

tab_principal <- data.frame(
  coef     = ct_m[linhas, 1],
  ep_micro = ct_m[linhas, 2],
  p_micro  = ct_m[linhas, 4],
  ep_hc1   = ct_h[linhas, 2],
  p_hc1    = ct_h[linhas, 4]
)
print(round(tab_principal, 3))

# Testes conjuntos: interações e choques, com as duas matrizes
h_inter <- c("seca_safra_10:agro_c10 = 0", "excesso_safra_10:agro_c10 = 0")
h_choq  <- c("seca_safra_10 = 0", "excesso_safra_10 = 0")

linearHypothesis(modelo_principal, h_inter, vcov. = V_micro, test = "Chisq")
linearHypothesis(modelo_principal, h_choq,  vcov. = V_micro, test = "Chisq")
linearHypothesis(modelo_principal, h_inter, vcov. = V_hc1,   test = "Chisq")
linearHypothesis(modelo_principal, h_choq,  vcov. = V_hc1,   test = "Chisq")

# 10. Resultados da robustez com pobreza no lugar da renda
linhas_pob <- c(linhas[-8], "pobreza_c")

ct_pob_m <- coeftest(modelo_pobreza, vcov. = V_micro_pob)
ct_pob_h <- coeftest(modelo_pobreza, vcov. = V_hc1_pob)

tab_pobreza <- data.frame(
  coef     = ct_pob_m[linhas_pob, 1],
  ep_micro = ct_pob_m[linhas_pob, 2],
  p_micro  = ct_pob_m[linhas_pob, 4],
  ep_hc1   = ct_pob_h[linhas_pob, 2],
  p_hc1    = ct_pob_h[linhas_pob, 4]
)
print(round(tab_pobreza, 3))

# 11. Heterocedasticidade (Breusch–Pagan) e checagens
bptest(modelo_principal, ~ log(total), data = d_micro)
bptest(modelo_principal, data = d_micro)

cat("Observações:", nobs(modelo_principal), "\n")
cat("Regressores sem constante e UF (deve ser 8):",
    length(coef(modelo_principal)) - 1 - (nlevels(factor(d$uf)) - 1), "\n")
cat("Graus de liberdade dos resíduos (deve ser 4041):", modelo_principal$df.residual, "\n")
cat("Previsões negativas:", sum(fitted(modelo_principal) < 0),
    "| mínimo:", round(min(fitted(modelo_principal)), 2), "p.p.\n")

# 12. Formatadores e rótulos das tabelas
fmt_br <- function(x) format(round(x, 3), nsmall = 3, decimal.mark = ",")

gof <- data.frame(
  raw   = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Observações", "R²", "R² ajustado"),
  fmt   = c(0, 3, 3)
)

# Rótulos da robustez (Tabela 7)
mapa <- c(
  s            = "Seca (SPI ≤ −1)",
  e            = "Excesso (SPI ≥ +1)",
  agro_c10     = "Agro (10 p.p., centrada)",
  "s:agro_c10" = "Seca × Agro",
  "e:agro_c10" = "Excesso × Agro"
)

# Rótulos da Tabela 5
mapa_nomes <- c(
  "seca_safra_10"             = "Seca (SPI ≤ −1)",
  "excesso_safra_10"          = "Excesso (SPI ≥ +1)",
  "agro_c10"                  = "Agro (10 p.p., centrada)",
  "seca_safra_10:agro_c10"    = "Seca × Agro",
  "excesso_safra_10:agro_c10" = "Excesso × Agro",
  "urb_c"                     = "Urbanização (centrada)",
  "alfab_c"                   = "Alfabetização (centrada)",
  "log_renda_c"               = "Log da renda (centrado)",
  "pobreza_c"                 = "% pobres (centrado)"
)

# p-valor conjunto das interações, com a matriz de cada modelo
p_inter <- function(m, V) {
  lh <- linearHypothesis(m,
                         c("seca_safra_10:agro_c10 = 0", "excesso_safra_10:agro_c10 = 0"),
                         vcov. = V, test = "Chisq")
  fmt_br(lh[["Pr(>Chisq)"]][2])
}

# 13. Robustez: estimação das especificações
#     Centraliza dentro da própria amostra e agrupa pela microrregião.
estima <- function(nome, dados, agro, seca, exc, fe = "uf") {
  
  x <- dados |>
    filter(uf != "DF", amostra_agro == 1) |>
    rename(s = all_of(seca), e = all_of(exc), a = all_of(agro)) |>
    filter(complete.cases(s, e, a, prop_menor18, pct_urbana,
                          alfab_15mais, renda_pc_media, pct_pobres)) |>
    left_join(geo |> select(code_muni, micro_id), by = "code_muni") |>
    mutate(
      y           = 100 * prop_menor18,
      agro_c10    = (a - mean(a)) / 10,
      urb_c       = pct_urbana   - mean(pct_urbana),
      alfab_c     = alfab_15mais - mean(alfab_15mais),
      log_renda_c = log(renda_pc_media) - mean(log(renda_pc_media)),
      regiao_col  = unname(mapa_regiao[as.character(uf)])
    )
  
  stopifnot(!anyNA(x$micro_id))
  
  efeito <- switch(fe, uf = "factor(uf)", regiao = "factor(regiao_col)", none = NULL)
  f <- reformulate(c("s", "e", "agro_c10", "s:agro_c10", "e:agro_c10",
                     "urb_c", "alfab_c", "log_renda_c", efeito), response = "y")
  
  m <- lm(f, data = x)
  V <- vcovCL(m, cluster = x$micro_id, type = "HC1")
  
  list(nome = nome, fe = fe, modelo = m, vcov = V, n = nobs(m))
}

# Subamostras
base20 <- base_agro |> filter(amostra_c20 == 1)
base10 <- base_agro |> filter(amostra_c10 == 1)
base50 <- base_agro |> filter(amostra_c50 == 1)

# Especificações de robustez
especificacoes <- list(
  list(nome = "Principal",                      dados = base20, agro = "agro_2008_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "uf"),
  list(nome = "Ano civil",                      dados = base20, agro = "agro_2008_2011",
       seca = "seca_civil_10",  exc = "excesso_civil_10", fe = "uf"),
  list(nome = "Limiar ±1,5",                    dados = base20, agro = "agro_2008_2011",
       seca = "seca_safra_15",  exc = "excesso_safra_15", fe = "uf"),
  list(nome = "Corte 10 casamentos",            dados = base10, agro = "agro_2008_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "uf"),
  list(nome = "Corte 50 casamentos",            dados = base50, agro = "agro_2008_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "uf"),
  list(nome = "Agro 2010 (própria média)",      dados = base20, agro = "agro_2010",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "uf"),
  list(nome = "Agro 2002–2011 (própria média)", dados = base20, agro = "agro_2002_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "uf"),
  list(nome = "Sem EF de UF",                   dados = base20, agro = "agro_2008_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "none"),
  list(nome = "EF de região",                   dados = base20, agro = "agro_2008_2011",
       seca = "seca_safra_10",  exc = "excesso_safra_10", fe = "regiao")
)

res <- lapply(especificacoes, function(s) do.call(estima, s))

# 14. Tabelas de robustez (7A e 7B)
#     Monta uma tabela com as colunas indicadas em `idx`
monta_tabela <- function(idx, titulo, arquivo) {
  sel     <- res[idx]
  nomes   <- sapply(sel, `[[`, "nome")
  modelos <- setNames(lapply(sel, `[[`, "modelo"), nomes)
  vcovs   <- lapply(sel, `[[`, "vcov")
  
  # p-valor conjunto das interações, com a matriz de cada coluna
  p_conj <- sapply(sel, function(r) {
    lh <- linearHypothesis(r$modelo, c("s:agro_c10 = 0", "e:agro_c10 = 0"),
                           vcov. = r$vcov, test = "Chisq")
    fmt_br(lh[["Pr(>Chisq)"]][2])
  })
  
  rotulo_fe <- sapply(sel, function(r) switch(r$fe, uf = "Sim", regiao = "Região", none = "Não"))
  
  linhas_extra <- data.frame(
    term = c("Controles", "Efeito fixo", "p (Seca × Agro = Excesso × Agro = 0)"),
    check.names = FALSE
  )
  for (i in seq_along(nomes)) {
    linhas_extra[[nomes[i]]] <- c("Sim", rotulo_fe[i], p_conj[i])
  }
  attr(linhas_extra, "position") <- c(11, 12, 13)   
  
  modelsummary(
    modelos,
    vcov     = vcovs,
    coef_map = mapa,
    title    = titulo,
    fmt      = fmt_br,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map  = gof,
    add_rows = linhas_extra,
    notes    = "Variável dependente: proporção de casamentos com noiva menor de 18 anos, em pontos percentuais. Erros-padrão entre parênteses, agrupados por microrregião (555 grupos). Constante omitida.",
    output   = arquivo
  )
}

# Tabela 7A: choque, janela e tamanho da amostra
monta_tabela(1:5,
             "Tabela 7A. Robustez: choque, janela e tamanho da amostra",
             file.path(pasta, "tabela_robustez_choque.docx"))

# Tabela 7B: medida de agropecuária e efeitos fixos
monta_tabela(c(1, 6, 7, 8, 9),
             "Tabela 7B. Robustez: medida de agropecuária e efeitos fixos",
             file.path(pasta, "tabela_robustez_agro.docx"))


# 13b. Corte de 50 casamentos: interação seca × agro (já estimado em `res`)
tab50 <- coeftest(res[[5]]$modelo, vcov. = res[[5]]$vcov)
tab50[c("s:agro_c10", "e:agro_c10"), ]
res[[5]]$n


# 15. Balanceamento
# Confirme as variáveis: cada coluna regride a variável nas dummies de choque, com efeito fixo de UF
balanco <- list(
  agro    = lm(agro_c10    ~ seca_safra_10 + excesso_safra_10 + factor(uf), data = d_micro),
  urb     = lm(urb_c       ~ seca_safra_10 + excesso_safra_10 + factor(uf), data = d_micro),
  alfab   = lm(alfab_c     ~ seca_safra_10 + excesso_safra_10 + factor(uf), data = d_micro),
  renda   = lm(log_renda_c ~ seca_safra_10 + excesso_safra_10 + factor(uf), data = d_micro),
  pobreza = lm(pobreza_c   ~ seca_safra_10 + excesso_safra_10 + factor(uf), data = d_micro)
)

names(balanco) <- c("Dependência agrícola (10 p.p.)", "Urbanização (p.p.)",
                    "Alfabetização (p.p.)", "Log da renda", "Pobreza (p.p.)")

V_bal <- lapply(balanco, function(m) vcovCL(m, cluster = d_micro$micro_id, type = "HC1"))

# p-valor conjunto de seca e excesso em cada coluna, com a matriz da coluna
p_bal <- sapply(seq_along(balanco), function(i) {
  lh <- linearHypothesis(balanco[[i]],
                         c("seca_safra_10 = 0", "excesso_safra_10 = 0"),
                         vcov. = V_bal[[i]], test = "Chisq")
  fmt_br(lh[["Pr(>Chisq)"]][2])
})

linhas_bal <- data.frame(
  term = c("Efeito fixo de UF", "p (Seca = Excesso = 0)"),
  check.names = FALSE
)
for (i in seq_along(balanco)) {
  linhas_bal[[names(balanco)[i]]] <- c("Sim", p_bal[i])
}
attr(linhas_bal, "position") <- c(5, 6)   

modelsummary(
  balanco,
  vcov     = unname(V_bal),                  # sem nomes, para não criar a linha "Std.Errors"
  coef_map = c("seca_safra_10"    = "Seca",
               "excesso_safra_10" = "Excesso de chuva"),
  gof_map  = gof[gof$raw %in% c("nobs", "r.squared"), ],
  fmt      = fmt_br,
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  add_rows = linhas_bal,
  title    = "Tabela X. Balanceamento: variáveis de 2010 e agropecuária 2008–2011 sobre os choques de chuva",
  notes    = "Cada coluna regride a variável indicada nas dummies de seca e excesso, com efeitos fixos de UF. Coeficientes na unidade da variável dependente, indicada no título da coluna. Erros-padrão HC1 entre parênteses, agrupados por microrregião (555 grupos). Constante omitida.",
  output   = file.path(pasta, "tabela_balanceamento_v2.docx"))

# 16. Termos quadráticos, com erro agrupado por microrregião
#     Renda (log) e agropecuária, cada um em um modelo separado
modelo_quad_renda <- update(modelo_principal, . ~ . + I(log_renda_c^2))
modelo_quad_agro  <- update(modelo_principal, . ~ . + I(agro_c10^2))

V_quad_renda <- vcovCL(modelo_quad_renda, cluster = d_micro$micro_id, type = "HC1")
V_quad_agro  <- vcovCL(modelo_quad_agro,  cluster = d_micro$micro_id, type = "HC1")

# Renda: quadrado do log da renda e efeito da seca
coeftest(modelo_quad_renda, vcov. = V_quad_renda)[c("seca_safra_10", "I(log_renda_c^2)"), ]

# Agropecuária: quadrado da agro e efeito da seca
coeftest(modelo_quad_agro, vcov. = V_quad_agro)[c("seca_safra_10", "I(agro_c10^2)"), ]

# 17. Seleção e denominador (casamentos por município)
#     [NOVO] Amostra antes do corte de 20 casamentos.
pre <- base_agro |>
  filter(uf != "DF") |>
  filter(complete.cases(seca_safra_10, excesso_safra_10, pct_urbana, alfab_15mais,
                        renda_pc_media, pct_pobres, total, pop_total)) |>
  left_join(geo |> select(code_muni, micro_id), by = "code_muni") |>
  mutate(
    c20         = as.numeric(total >= 20),
    urb_c       = pct_urbana   - mean(pct_urbana),
    alfab_c     = alfab_15mais - mean(alfab_15mais),
    log_renda_c = log(renda_pc_media) - mean(log(renda_pc_media)),
    log_pop_c   = log(pop_total) - mean(log(pop_total))
  )

pre_pos <- pre |> filter(total >= 1)

cat("N pre:", nrow(pre), "| N pre_pos:", nrow(pre_pos),
    "| municípios com total = 0:", sum(pre$total == 0), "\n")

pre_pos <- pre_pos |>
  mutate(
    n_menor18    = round(prop_menor18 * total),
    taxa_menor18 = 1e5 * n_menor18 / pop_total,
    taxa_total   = 1e5 * total / pop_total
  )

cat("Municípios com zero casamentos de menores:", sum(pre_pos$n_menor18 == 0), "\n")

ctrl <- "seca_safra_10 + excesso_safra_10 + urb_c + alfab_c + log_renda_c + log_pop_c + factor(uf)"

# Seleção: P(total ≥ 20) no corte pré-amostra (LPM, erro agrupado por microrregião)
m_sel <- lm(c20 ~ seca_safra_10 + excesso_safra_10 + urb_c + alfab_c +
              log_renda_c + log_pop_c + factor(uf), data = pre)
V_sel <- vcovCL(m_sel, cluster = pre$micro_id, type = "HC1")
coeftest(m_sel, vcov. = V_sel)[c("seca_safra_10", "excesso_safra_10"), ]

# Tamanho: log(total) em pre_pos (o log exige total ≥ 1)
m_tot <- lm(as.formula(paste("log(total) ~", ctrl)), data = pre_pos)
V_tot <- vcovCL(m_tot, cluster = pre_pos$micro_id, type = "HC1")
coeftest(m_tot, vcov. = V_tot)[c("seca_safra_10", "excesso_safra_10"), ]

# Denominador e numerador, em taxas por 100 mil habitantes
m_num <- lm(as.formula(paste("taxa_menor18 ~", ctrl)), data = pre_pos)
m_den <- lm(as.formula(paste("taxa_total ~", ctrl)),   data = pre_pos)

V_num <- vcovCL(m_num, cluster = pre_pos$micro_id, type = "HC1")
V_den <- vcovCL(m_den, cluster = pre_pos$micro_id, type = "HC1")

coeftest(m_num, vcov. = V_num)[c("seca_safra_10", "excesso_safra_10"), ]
coeftest(m_den, vcov. = V_den)[c("seca_safra_10", "excesso_safra_10"), ]

# Média de referência (municípios de chuva normal), para expressar em %
mean(pre_pos$taxa_menor18[pre_pos$seca_safra_10 == 0 & pre_pos$excesso_safra_10 == 0])
mean(pre_pos$taxa_total[pre_pos$seca_safra_10 == 0 & pre_pos$excesso_safra_10 == 0])

# 18. Estabilidade: sem controles e Tabela 5
# Checagem: agro e pobreza estão correlacionadas?
cor(d_micro$agro_2008_2011, d_micro$pct_pobres)
cor(d_micro$agro_2008_2011, d_micro$renda_pc_media)
cor(d$log_renda_c, d$alfab_c)

m_sc <- lm(pct_menor18 ~ seca_safra_10 + excesso_safra_10 + agro_c10 +
             seca_safra_10:agro_c10 + excesso_safra_10:agro_c10 + factor(uf),
           data = d_micro)
V_sc <- vcovCL(m_sc, cluster = d_micro$micro_id, type = "HC1")

nomes_t5 <- c("Sem controles", "Principal", "Principal (HC1)", "Robustez: pobreza")
controles <- c("Não", "Sim", "Sim", "Sim")
p_vals <- c(p_inter(m_sc, V_sc),
            p_inter(modelo_principal, V_micro),
            p_inter(modelo_principal, V_hc1),
            p_inter(modelo_pobreza, V_micro_pob))

linhas_t5 <- data.frame(
  term = c("Controles", "Efeito fixo de UF", "p (Seca × Agro = Excesso × Agro = 0)"),
  check.names = FALSE
)
for (i in seq_along(nomes_t5)) {
  linhas_t5[[nomes_t5[i]]] <- c(controles[i], "Sim", p_vals[i])
}
attr(linhas_t5, "position") <- c(19, 20, 21)  

gof_t5 <- gof[gof$raw %in% c("nobs", "r.squared"), ]

modelsummary(
  list("Sem controles"     = m_sc,
       "Principal"         = modelo_principal,
       "Principal (HC1)"   = modelo_principal,
       "Robustez: pobreza" = modelo_pobreza),
  vcov     = list(V_sc, V_micro, V_hc1, V_micro_pob),
  coef_map = mapa_nomes,
  title    = "Tabela 5. Choques de chuva e proporção de casamentos com noiva menor de 18 anos (p.p.)",
  fmt      = fmt_br,
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map  = gof_t5,
  add_rows = linhas_t5,
  notes    = "Variável dependente: proporção de casamentos com noiva menor de 18 anos, em pontos percentuais. Erros-padrão entre parênteses. Colunas 1, 2 e 4: agrupados por microrregião (555 grupos). Coluna 3: HC1. Efeitos fixos de UF em todas as colunas. Constante omitida.",
  output   = file.path(pasta, "tabela_5_v2.docx")
)

# 19. Agro ou pobreza
#     Regra: cada moderador (renda ou pobreza) entra interagido com seca E com excesso,
#     nas duas versões. As interações com agro continuam no modelo.

# 19a: renda (base: modelo_principal)
m_renda_int <- update(modelo_principal, . ~ . +
                        seca_safra_10:log_renda_c + excesso_safra_10:log_renda_c)
V_renda_int <- vcovCL(m_renda_int, cluster = d_micro$micro_id, type = "HC1")

# 19b: pobreza no lugar da renda (base: modelo_pobreza)
m_pob_int <- lm(pct_menor18 ~ seca_safra_10 + excesso_safra_10 + agro_c10 +
                  seca_safra_10:agro_c10 + excesso_safra_10:agro_c10 +
                  urb_c + alfab_c + pobreza_c +
                  seca_safra_10:pobreza_c + excesso_safra_10:pobreza_c +
                  factor(uf),
                data = d_micro)
V_pob_int <- vcovCL(m_pob_int, cluster = d_micro$micro_id, type = "HC1")

# 19c. Efeitos marginais do excesso

b  <- coef(m_renda_int)
V1 <- V_renda_int
ie <- "excesso_safra_10"
ii <- "excesso_safra_10:log_renda_c"
G1 <- length(unique(d_micro$micro_id))

marg <- function(r) {
  est <- b[ie] + r * b[ii]
  se  <- sqrt(V1[ie, ie] + r^2 * V1[ii, ii] + 2 * r * V1[ie, ii])
  tq  <- qt(0.975, df = G1 - 1)
  c(efeito = est, ep = se, ic_inf = est - tq * se, ic_sup = est + tq * se,
    p = 2 * pt(-abs(est / se), df = G1 - 1))
}

round(rbind(baixa_renda = marg(log(0.6)), alta_renda = marg(log(1.4))), 3)


# Coeficientes: agro e moderadores, em cada modelo
termos_agro  <- c("seca_safra_10:agro_c10", "excesso_safra_10:agro_c10")
termos_renda <- c(termos_agro, "seca_safra_10:log_renda_c", "excesso_safra_10:log_renda_c")
termos_pob   <- c(termos_agro, "seca_safra_10:pobreza_c", "excesso_safra_10:pobreza_c")

coeftest(modelo_principal, vcov. = V_micro)[termos_agro, ]
coeftest(m_renda_int, vcov. = V_renda_int)[termos_renda, ]
coeftest(modelo_pobreza, vcov. = V_micro_pob)[termos_agro, ]
coeftest(m_pob_int, vcov. = V_pob_int)[termos_pob, ]

# Testes conjuntos
linearHypothesis(m_renda_int,
                 c("seca_safra_10:log_renda_c = 0", "excesso_safra_10:log_renda_c = 0"),
                 vcov. = V_renda_int, test = "Chisq")
linearHypothesis(m_renda_int,
                 c("seca_safra_10:agro_c10 = 0", "excesso_safra_10:agro_c10 = 0"),
                 vcov. = V_renda_int, test = "Chisq")
linearHypothesis(m_pob_int,
                 c("seca_safra_10:pobreza_c = 0", "excesso_safra_10:pobreza_c = 0"),
                 vcov. = V_pob_int, test = "Chisq")
linearHypothesis(m_pob_int,
                 c("seca_safra_10:agro_c10 = 0", "excesso_safra_10:agro_c10 = 0"),
                 vcov. = V_pob_int, test = "Chisq")

# 20. H5: efeito do choque na participação agropecuária
pib <- readRDS(file.path(dir_inter, "pib_agro_2016.rds"))
pib16 <- pib[, c("code_muni", "part_agro")]
names(pib16)[2] <- "part_agro16"

d5 <- merge(d_micro, pib16, by = "code_muni")
stopifnot(nrow(d5) == nrow(d_micro))

d5$agro16   <- 100 * d5$part_agro16                  # fração -> %
d5$dif_agro <- d5$agro16 - d5$agro_2008_2011         # variação em p.p. frente a 2008–2011
summary(d5$dif_agro)

h5 <- lm(dif_agro ~ seca_safra_10 + excesso_safra_10 + urb_c + alfab_c + log_renda_c +
           factor(uf), data = d5)

cl   <- d5$micro_id[as.integer(rownames(model.frame(h5)))]
V    <- vcovCL(h5, cluster = cl, type = "HC1")
est  <- coef(h5)[c("seca_safra_10", "excesso_safra_10")]
se   <- sqrt(diag(V))[names(est)]
tval <- est / se
pval <- 2 * pnorm(-abs(tval))
round(cbind(estimativa = est, ep = se, t = tval, p = pval), 4)
nobs(h5)

# 21. H5 sem outliers (|variação| > 20 p.p.)
out <- d5[abs(d5$dif_agro) > 20, c("code_muni", "uf", "agro_2008_2011", "agro16", "dif_agro", "vab_agro")]
nrow(out)
out[order(-abs(out$dif_agro)), ][1:15, ]

h5b <- update(h5, subset = abs(dif_agro) <= 20)
cl2 <- d5$micro_id[as.integer(rownames(model.frame(h5b)))]
V2  <- vcovCL(h5b, cluster = cl2, type = "HC1")
e2  <- coef(h5b)[c("seca_safra_10", "excesso_safra_10")]
s2  <- sqrt(diag(V2))[names(e2)]
round(cbind(estimativa = e2, ep = s2, p = 2 * pnorm(-abs(e2 / s2))), 4)
nobs(h5b)