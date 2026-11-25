# Choques de chuva e casamento adolescente no Brasil

Código do trabalho empírico de Econometria I (FGV EESP): **"Choques de chuva e casamento adolescente no Brasil: evidências municipais do El Niño de 2015–2016"**.

Autoras: Camila Capobianco Natal, Julia Scardoeli Alves. Orientadora: Gabriela Do Couto Fonseca.

O trabalho estima, por MQO, a relação entre choques de chuva no ano-safra de julho de 2015 a junho de 2016 (SPI-12) e a proporção de casamentos com noiva menor de 18 anos em 2016, em um corte transversal de 4.075 municípios.

---

## Como reproduzir

1. Instale o R (versão usada: [preencher, ex.: 4.4.1]) e os pacotes listados abaixo.
2. Abra `00_rodar_tudo.R`, troque o caminho em `pasta` pelo caminho desta pasta no seu computador e rode o arquivo inteiro.

Estrutura da pasta:

```
trabalho de econo/
├── 00_rodar_tudo.R
├── 01_construcao_base.R
├── 02_analise.R
├── README.md
└── dados/
    ├── brutos/            # baixados à mão (SIDRA e Atlas); enviados junto
    │   ├── atlas.xlsx
    │   ├── casamentos.csv
    │   ├── pib.csv
    │   └── tabela5938.csv
    └── intermediarios/    # gerados pelo 01_construcao_base.R; enviados junto
        ├── base_agro.rds
        ├── geo_municipios.rds
        ├── pib_agro_2016.rds
        └── malha_2016/    # shapefile da malha municipal 2016 (IBGE)
```

O script mestre roda os dois scripts na ordem:

| Ordem | Script | O que faz | Tempo aproximado |
|---|---|---|---|
| 1 | `01_construcao_base.R` | Baixa e processa as fontes e monta a base municipal | longo (download do CHIRPS de 1981–2016) |
| 2 | `02_analise.R` | Define a amostra, estima os modelos e gera as tabelas e as figuras | poucos minutos |

**Atalho (recomendado):** os arquivos de `dados/intermediarios/` já vão junto, então dá para rodar só o `02_analise.R`, em poucos minutos, sem refazer o download do CHIRPS. O `01_construcao_base.R` só precisa ser rodado para reconstruir a base desde as fontes originais.

---

## Dados

### Baixados automaticamente pelo código

| Fonte | Conteúdo | Onde no código |
|---|---|---|
| CHIRPS v2.0 mensal, 0,05° (Funk et al., 2015), via IRI Data Library (Columbia) | Precipitação mensal de 1981 a 2016, recortada para o Brasil | `01_construcao_base.R`, bloco 01 |
| Malha municipal 2016 do IBGE (geoftp) | Polígonos dos municípios (`BRMUE250GC_SIR.shp`) | `01_construcao_base.R`, bloco 01 |
| API de Localidades do IBGE | Microrregião de cada município, usada no agrupamento dos erros-padrão | `01_construcao_base.R`, bloco 08 |

Observação: a API de Localidades devolve a divisão vigente na data da consulta. Os códigos usados no trabalho foram baixados em [preencher a data].

### Dados baixados à mão (em `dados/brutos/`)

Os arquivos já vão junto com o código. A tabela registra a origem de cada um, consultada em [preencher as datas].

| Arquivo | Fonte | Filtros da consulta |
|---|---|---|
| `casamentos.csv` | SIDRA/IBGE, Tabela 4412 (Estatísticas do Registro Civil) | Ano 2016; nível territorial: Município; casamentos entre cônjuges masculino e feminino; idade do 1º cônjuge: Total; idade do 2º cônjuge: idades simples (é preciso ter "Menos de 15 anos", "15 anos", "16 anos" e "17 anos" separadas); com as colunas de casamentos ocorridos no ano e em anos anteriores. [conferir os filtros exatos usados] |
| `pib.csv` | SIDRA/IBGE, Tabela 5938 (PIB dos Municípios) | Ano 2016; nível: Município; variáveis: VAB a preços correntes total e VAB a preços correntes da agropecuária |
| `tabela5938.csv` | SIDRA/IBGE, Tabela 5938 | Mesmas variáveis de `pib.csv`, para os anos de 2002 a 2018 |
| `atlas.xlsx` | Atlas do Desenvolvimento Humano no Brasil (PNUD; Ipea; FJP, 2013), atlasbrasil.org.br | Consulta por município, ano 2010, com 7 colunas nesta ordem: Territorialidades; População total; População urbana; Taxa de analfabetismo (15 anos ou mais); Renda per capita; % de extremamente pobres; % de pobres |

Os arquivos do SIDRA são lidos como foram exportados (CSV, com os blocos de cada variável). O código identifica os blocos pelo título "Variável - ...".

---

## O que cada bloco gera

### `01_construcao_base.R`

| Bloco | Entrada | Saída |
|---|---|---|
| 01 – CHIRPS por município | download (IRI) e malha 2016 | `precip_municipio_mes.rds` / `.csv` |
| 02 – Choque de chuva (SPI-12) | `precip_municipio_mes.rds` | `choque_chuva_2016.rds` |
| 03 – Casamentos | `casamentos.csv`, `choque_chuva_2016.rds` | `casamentos_2016.rds` |
| 04 – PIB 2016 | `pib.csv` | `pib_agro_2016.rds` |
| 04b – PIB 2002–2018 | `tabela5938.csv` | `pib_agro_anos.rds` |
| 05 – Controles do Censo 2010 | `atlas.xlsx` | `censo2010.rds` |
| 06 – Base final | as quatro bases acima | `base_final.csv` |
| 07 – Medidas de dependência agrícola | `base_final.csv`, `pib_agro_anos.rds` | `base_agro.rds` |
| 08 – Microrregiões | API do IBGE | `geo_municipios.rds` |

Principais decisões de construção:
- SPI-12 com ajuste gama. Período de referência: 1982–2010 no ano-safra (29 janelas) e 1981–2010 no ano civil.
- Dummies de seca (SPI ≤ −1) e de excesso (SPI ≥ +1), com limiar ±1,5 na robustez.
- Noiva = 2º cônjuge da Tabela 4412.
- 498 municípios sem informação no Registro Civil (`...`) são excluídos.
- Cortes de 10, 20 e 50 casamentos (variáveis `amostra_c10`, `amostra_c20`, `amostra_c50`).
- Cruzamento por nome e UF com a malha de 2016, com correções de grafia listadas no código.

### `02_analise.R`

| Seção do script | Resultado no relatório |
|---|---|
| 1–2 | Amostra principal (4.075 municípios) e variáveis centradas |
| 4 | Tabela 2 (estatísticas descritivas) |
| 5 | Figura 1 (mapa do SPI-12) e Figura 2 (renda em nível e em log) |
| 6 | Tabela 3 (características por condição de chuva) |
| 7–10, 18 | Tabela 4 (modelo principal, HC1, sem controles e pobreza) |
| 11 | Teste de Breusch-Pagan (seção 2.6.1) |
| 13–14 | Tabela 7 (robustez) e robustez de medida agropecuária e efeitos fixos (seção 2.8) |
| 15 | Tabela 5 (balanceamento) |
| 16 | Termos quadráticos (seção 2.8) |
| 17 | Seleção, total de casamentos e taxas por 100 mil habitantes (seção 2.7.4) |
| 19 | Tabela 6 (heterogeneidade por renda e pobreza) e efeitos marginais do excesso |
| 20–21 | Teste de H5 (participação agropecuária em 2016) |

Os números da Tabela 1 (fluxo da amostra) são impressos pelos blocos 03, 05 e 07 do `01_construcao_base.R` e pela seção 1 do `02_analise.R`.

---

## Pacotes

```r
install.packages(c(
  # construção da base
  "terra", "sf", "exactextractr", "dplyr", "tidyr", "stringr", "stringi",
  "readr", "readxl", "jsonlite",
  # análise
  "sandwich", "lmtest", "car", "modelsummary", "ggplot2", "scales"
))
```

---

## Observações

- Os servidores da UCSB e do NOAA não responderam durante o trabalho. Por isso, o CHIRPS é baixado pela IRI Data Library, que distribui a mesma versão (v2.0 mensal).
- Os erros-padrão principais são agrupados por microrregião (`sandwich::vcovCL`, tipo HC1). O HC1 sem agrupamento aparece como comparação.
- A malha de 2016 tem 5.572 polígonos: os 5.570 municípios e duas lagoas do RS (4300001 e 4300002), que são retiradas no bloco 02.
