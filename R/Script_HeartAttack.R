
# Projeto: Heart Attack
# Setembro 2022
# NATALIA F SOUZA




# Pacotes -------------------------------------------------
library(tidyverse) 
library(dplyr)
library(corrplot)




# Dados -------------------------------------------------------------------
ha <- readr::read_csv(file = "raw/heart.csv")



# Conhecendo os dados -----------------------------------------------------
# Dados extraídos do Kaggle: https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset  


  # Insights: 14 variáveis
    str(ha)
    view(ha)
    attributes(ha)
    skimr::skim(ha)
    summary (ha)
  
  # Missing Values: não possui
    table(map(mtcars, is.na)) |> 
      knitr::kable()

  # Significados das siglas
    # Tabela com significados 
    # Para consulta durante as análises
    tab_sig<- data.frame(Siglas = colnames(ha),
                     Significado = c("**Idade do paciente**",
                                     "**Sexo do paciente**:<br>0| female; <br> 1 = male",
                                     "**Tipo de dor no peito**:<br> 1|angina tipica;<br> 2|angina atipica;<br>3|dor não angina;<br> 4|assintomático",
                                     "**Pressão arterial em repouso** (mm/Hg)",
                                     "**Colesterol** (mg/dl)",
                                     "**Glicemia** (jejum > 120 mg/dl):<br> 1|Verdadeiro;<br> 0|Falso",
                                     "**Eletrocardiográficos** (repouso):<br>1|normal;<br>2|tendo anormalidade da onda ST-T;<br>3|provável hipertrofia ventricular esquerda",
                                     "**Frequência cardíaca máxima** ",
                                     "**Angina induzida por exercício**:<br>1|Sim;<br> 0|Não",
                                     "**Depressão de ST induzida por exercício",
                                     "**Inclinação do segmento ST**:<br>0|sem inclinação;<br>1|plano;<br>2|descendo",
                                     "**Número de grandes vasos**",
                                     "**Talassemia**:<br>0|nulo;<br> 1|defeito corrigido;<br> 2|normal;<br> 3|defeito reversível",
                                     "**Diagnóstico de doença cardíaca**:<br>0|| < 50% estreitamento do diâmetro. Menos chance de doença cardíaca;<br> 1| > 50% de estreitamento do diâmetro. Mais chance de doença cardíaca"))

  tab_sig |> 
  knitr::kable()
  
  
# - Interpretação das variáveis

  # Talassemia: doenças hemolíticas hereditárias causadas por síntese defeituosa 
  # de hemoglobina, disseminada em países mediterrâneos, africanos e asiáticos.:


# -Variáveis do banco de dados
  
  # Age
  # I. Maior parte dos dados está na faixa de 40-69 anos
  ha |> 
    mutate(
      idade = cut(age, breaks = c(20, 29, 39, 49, 59, 69, 79, 89))) |> 
    group_by(idade) |> 
    summarise(
      n = n()) |> 
    ggplot(aes(y = n, x = idade, label = n))+
    geom_bar(stat = "identity", alpha = 1/2, fill= "orange") +    
    geom_label(position = position_stack (vjust = 0.85),alpha = 0.8, 
               colour = "darkgray", fontface = "bold", show_guide  = F)+ 
    scale_y_continuous(breaks=NULL)+
    scale_x_discrete(labels = c("20-29","30-39","40-49","50-59","60-69",">70"))+
    #scale_x_discrete(labels = seq(from = 20, to = 80, 10) )+
    meu_tema
 
  
  
  
# Objetivo ----------------------------------------------------------------
# Verificar os preditores que podem impactar (ou não) nas chances de 
# ataque cardíaco
  
 
# Roteiro para Análise ----------------------------------------------------

  # 1. Análise Descritiva
  #     .Tipologia 
  #     .Transformações 
  #     .Colinearidade 
  #     .Análises Visuais 
  
  
  


# 1.Análise Descritiva -------------------------------------------------

# Padronizando: Tema dos gráficos
  meu_tema <- theme(legend.position = "none",
                    #axis.line.x = element_line(colour = "gray"),
                    #axis.line.y = element_line(colour = "gray"),
                    
                    axis.title = element_text(face = "bold", size = 16),
                    axis.text.x = element_text(face = "plain", size=12),
                    axis.text.y = element_text(face = "plain", size=12),
                    
                    axis.ticks = element_line(colour = "gray", size = 0.2),
                    
                    panel.grid = element_blank(),
                    panel.background = element_blank())

  
#    .Tipologia
str(ha)
view(ha)
ha <- ha |> 
      mutate(
      across(.cols = c(sex, cp, fbs, restecg, exng, slp, thall, output),
             .fns = as.factor))

skimr::skim(ha)



#   .Colinearidade
#     Conclusão: Var. Numéricas possuem baixa colinearidade 
#     entre si. Podemos usar todas.

# Variáveis numéricas
  ha_numeric <-   ha  |>
                    select(where(is.numeric))
# Correlação - Escolha do método de spearman porque não queria assumir pressuposto
# de normalidade das variáveis. 
  corrplot.mixed(cor(ha_numeric, method = "spearman"),lower = "number", upper = 'color')
  

 
  
#   .Análises visuais
  
# Insights: Age
  # I. 
  ha |> 
    ggplot(aes(x = output, y = age, fill = sex))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    
  
  

  ha |> 
    group_by(sex) |> 
    summarise(
      n = n()) |> 
    ggplot(aes(y = n, x = idade, label = n))+
    geom_bar(stat = "identity", alpha = 1/2, fill= "orange") +    
    geom_label(position = position_stack (vjust = 0.85),alpha = 0.8, 
               colour = "darkgray", fontface = "bold", show_guide  = F)+ 
    scale_y_continuous(breaks=NULL)+
    scale_x_discrete(labels = c("20-29","30-39","40-49","50-59","60-69",">70"))+
    #scale_x_discrete(labels = seq(from = 20, to = 80, 10) )+
    meu_tema
  
    
  skimr::skim(ha)
   
    
  
  # a.Numéricas e Inteiras
      ha |> 
        select(where(is.numeric), output) |> 
        pivot_longer(-output) |>
          ggplot(aes(x = value, y = output)) +
            geom_point()+
            stat_smooth(se= F)+
            facet_grid(~name, scales = "free")  



   # Função para Histograma das proporções
      grafico_proporcao <- function(coluna, bins=NULL, breaks = NULL){
        ha |> 
          ggplot(aes(y = as.numeric(output)-1, x = coluna)) +
          stat_summary_bin(size = 1, alpha = 0.1, colour = "white", bins = bins,breaks = breaks,
                           geom = "bar", fill = "royalblue", fun = function(x) 1,
                           na.rm = T) +
          stat_summary_bin(size = 1,alpha = 0.1, colour = "white", bins = bins,
                           breaks = breaks,geom = "bar", fill = "red", na.rm = T) +
          stat_summary_bin(size = 2, alpha = 1, colour = "purple", bins = bins,
                           breaks = breaks, geom = "point", na.rm = T) + 
          stat_smooth(method = "glm", method.args = list(family = "binomial"),
                      se = FALSE, na.rm = T) +
          geom_point() 
        
        
      }
    # Função para tabela das proporções
      
      tabela_proporcao <- function(coluna, breaks = NULL ){
        ha |> 
          mutate(
            coluna_faixa = cut(coluna, breaks = breaks)) |> 
          group_by(coluna_faixa) |> 
          summarise(
            n = n(),
            coluna = mean(coluna),
            p_output = mean(output == 1),
            logit_chance_output = log(p_output/(1-p_output)))
        
                    }
      
        
 # age
      grafico_proporcao(coluna = ha$age, bins = 10)
      tabela_proporcao(coluna = ha$age, breaks = 10)
      
      grafico_proporcao(coluna = ha$age, breaks = c(25,39,45,50,55,60,65,77))
      tabela_proporcao(coluna = ha$age, breaks = c(25,39,45,50,55,60,65,77))
      
       
 
# trtbps
      grafico_proporcao(coluna = ha$trtbps, bins = 10)
      tabela_proporcao(coluna = ha$trtbps, breaks = 10)
      
      grafico_proporcao(coluna = ha$trtbps, breaks = c(93.9,105,
                                                       115,126,136,147,
                                                       158,168,200))
      tabela_proporcao(coluna = ha$trtbps, breaks = c(93.9,105,
                                                      115,126,136,147,
                                                      158,168,200,330))

# chol
      grafico_proporcao(coluna = ha$chol, bins = 15)
      tabela_proporcao(coluna = ha$chol, breaks = 15)
      
      
# thalachh
      grafico_proporcao(coluna = ha$thalachh, bins = 12)
      tabela_proporcao(coluna = ha$thalachh, breaks = 12)
      
# oldpeak
      grafico_proporcao(coluna = ha$oldpeak, bins = 12)
      tabela_proporcao(coluna = ha$oldpeak, breaks = 12)
      
      tabela_proporcao(coluna = ha$oldpeak, breaks = c(-0.0061, 0.517,1.03,1.55,
                                                       2.07,2.58,3.1,3.62,6.21))
      grafico_proporcao(coluna = ha$oldpeak, breaks = c(-0.0061, 0.517,1.03,1.55,
                                                        2.07,2.58,3.1,3.62,6.21))
      
# caa
      grafico_proporcao(coluna = ha$caa, bins = 4)
      tabela_proporcao(coluna = ha$caa, breaks = 4)      
      
      
      # b.Categoricas

  graf_proporcao_categoricos <- ha |> 
                select(is.factor) |> 
                map(grafico_proporcao)
  graf_proporcao_categoricos$sex    
  graf_proporcao_categoricos$cp    
  graf_proporcao_categoricos$fbs    
  graf_proporcao_categoricos$restecg    
  graf_proporcao_categoricos$slp    
  graf_proporcao_categoricos$thall    
  
  
  
  
  
  skimr::skim(ha)    
  tab_sig |> 
    knitr::kable()   
      
  



