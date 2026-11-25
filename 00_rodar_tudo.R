# =====================================================================
# 00_rodar_tudo.R
# Roda o trabalho inteiro, na ordem. Troque só o caminho de `pasta`.
# =====================================================================

pasta <- "C:/Users/camil/OneDrive/Documentos/trabalho de econo"

# 1. Construção da base (baixa o CHIRPS de 1981–2016: demora).
#    Os arquivos de dados/intermediarios já vão junto com o trabalho;
#    para rodar só a análise, deixe a linha abaixo comentada.
# source(file.path(pasta, "01_construcao_base.R"), echo = TRUE, encoding = "UTF-8")

# 2. Análise: amostra, modelos, testes, tabelas e figuras.
#    echo = TRUE mostra no console os comandos e os resultados.
source(file.path(pasta, "02_analise.R"), echo = TRUE, max.deparse.length = Inf,
       encoding = "UTF-8")
