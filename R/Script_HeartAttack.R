
# Projeto: Heart Attack
# Setembro 2022
# NATALIA F SOUZA




# Pacotes -------------------------------------------------
library(tidyverse) 
library(dplyr)




# Dados -------------------------------------------------------------------
ha <- readr::read_csv(file = "raw/heart.csv")



# Conhecendo os dados -----------------------------------------------------
  # Insights: 14 variáveis
    str(ha)
    view(ha)
    attributes(ha)
    skimr::skim(ha)

  # Missing Values: não possui
    table(map(mtcars, is.na)) |> 
      knitr::kable()

  # Significados
    tab_sig<- data.frame(Siglas = colnames(ha),
                     Significado = c("Idade do paciente",
                                    "Sexo do paciente",
                                    "Tipo de dor no peito:
                                    1: angina tipica
                                    2: angina atipica
                                    3: dor não angina
                                    4: assintomático",
                                    "Pressão arterial em repouso (mm/Hg)",
                                    "Colesterol em mg/dl",
                                    "Glicemia em jejum > 120 mg/dl:
                                    1: Verdadeiro
                                    0: Falso",
                                    "Eletrocardiográficos em repouso:
                                    1:normal
                                    2:tendo anormalidade da onda ST-T
                                    3:provável hipertrofia ventricular esquerda",
                                    "Frequência cardíaca máxima atingida",
                                    "Angina induzida por exercício:
                                    1:Sim
                                    0:Não",
                                    "Depressão de ST induzida pelo exercício em relação ao repouso",
                                    "Inclinação do segmento ST:
                                    0:sem inclinação
                                    1:plano
                                    2:descendo",
                                    "Número de grandes vasos",
                                    "Talassemia:
                                    0:nulo
                                    1:defeito corrigido
                                    2:normal
                                    3:defeito reversível",
                                    "Diagnóstico de doença cardíaca:
                                    0: < 50% estreitamento do diâmetro. Menos chance de doença cardíaca
                                    1: > 50% de estreitamento do diâmetro. Mais chance de doença cardíaca"))

# Tabela com significados 
# Para consulta durante as análises
  tab_sig |> 
  knitr::kable()

# - Descrição para entender os dados

  # Talassemia: doenças hemolíticas hereditárias causadas por síntese defeituosa 
  # de hemoglobina, disseminada em países mediterrâneos, africanos e asiáticos.:



# Roteiro para Análise ----------------------------------------------------

  # 1. Análise das variáveis
  #     .Tipologia
  #     .Transformações
  
  # 2. Análise Descritiva

  
  
# 1. Analise das variáveis

#    .Tipologia
str(ha)
view(ha)
ha <- ha |> 
      mutate(
      across(.cols = c(sex, cp, fbs, restecg, exng, slp, thall, output),
             .fns = as.factor))


# 2. Análise Descritiva

