
# Projeto: Heart Attack
# Setembro 2022
# NATALIA F SOUZA




# Pacotes -------------------------------------------------
library(tidyverse) 
library(dplyr)
library(corrplot)

# Pacotes para Modelagem --------------------------------------------------
library(ggplot2)
library(skimr)
library(tidymodels)


# Dados -------------------------------------------------------------------
ha <- readr::read_csv(file = "data-raw/heart.csv")



# Conhecendo os dados -----------------------------------------------------
# Dados extraídos do Kaggle: https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset  


  # Insights: 14 variáveis
    str(ha)
    view(ha)
    attributes(ha)
    skimr::skim(ha)
    summary (ha)
  
  # Missing Values: não possui
    table(map(ha, is.na)) |> 
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
  
  

# -Variáveis do banco de dados
  
  # Age
  # I. Maior parte dos dados está na faixa do 50-59 anos
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
  
  

# Padronizando | Funções --------------------------------------------------

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
  
# Função grafico_proporcao ( ) :para Histograma das proporções
  grafico_proporcao <- function(coluna, bins=NULL, breaks = NULL, eixox = NULL){
    
    ha |> 
      ggplot(aes(y = as.numeric(output)-1, x = coluna)) +
      stat_summary_bin(size = 1, alpha = 0.1, colour = "white", bins = bins,breaks = breaks,
                       geom = "bar", fill = "royalblue", fun = function(x) 1,
                       na.rm = T) +
      stat_summary_bin(size = 1,alpha = 0.3, colour = "white", bins = bins,
                       breaks = breaks,geom = "bar", fill = "orange", na.rm = T) +
      stat_summary_bin(size = 2, alpha = 1, colour = "purple", bins = bins,
                       breaks = breaks, geom = "point", na.rm = T) + 
      stat_smooth(method = "glm", method.args = list(family = "binomial"),
                  se = FALSE, na.rm = T) +
      xlab(eixox)+
      ylab("Proporção Doença cardíaca")+
      geom_point() +
      meu_tema
    
    
  }
# Função tabela_proporcao ( ): para tabela das proporções
  
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
  
# Função analise_predicao ( ): para predizer valores de acordo com modelo da regressao
  analise_predicao <- function (modelo){
    fitted_results <- dismo::predict(modelo, newdata = data_test, type = "response")
    fitted_results_cat <- ifelse(fitted_results > 0.7,1,0) #threshold
    
    data_test_pred <- data_test |> 
      add_column(fitted =  fitted_results,
                 fitted_cat = fitted_results_cat ) |>
      mutate(fitted_exp = exp(fitted_results))
    print(data_test_pred)}
  
  
#Função desempenho ( ): para calcular as metricas de desempenho dos modelos
  
  desempenho <-  function(modelo){
    fitted_results <- dismo::predict(modelo, newdata = data_test, type = "response")
    fitted_results_cat <- ifelse(fitted_results > 0.7,1,0) #threshold
    
    data_test_pred <- data_test |> 
      add_column(fitted =  fitted_results,
                 fitted_cat = fitted_results_cat ) |>
      mutate(fitted_exp = exp(fitted_results))
    
    data_test_pred <- data_test_pred |> 
      mutate(fitted_cat = as_factor(fitted_cat))
    
    metricas <- caret::confusionMatrix(data = data_test_pred1$output,
                                       reference = data_test_pred1$fitted_cat)
    print(metricas)}

# 1.Análise Descritiva -------------------------------------------------


  #    .Tipologia
  str(ha)
  view(ha)
  ha <- ha |> 
        mutate(
        across(.cols = c(sex, cp, fbs, restecg, exng, slp, thall, output),
               .fns = as.factor))
  
  skimr::skim(ha)



#   .Colinearidade
# > Correlação - Escolha do método de spearman porque não queria assumir pressuposto
#   de normalidade das variáveis. 
# > Conclusão: Var. Numéricas possuem baixa colinearidade 
#   entre si. Podemos usar todas.
  ha_numeric <-   ha  |>
                    select(where(is.numeric))
  corrplot.mixed(cor(ha_numeric, method = "spearman"),lower = "number", upper = 'color')
  


# 1.a. Análises Visuais ---------------------------------------------------

# ---Numerica


# Idade -------------------------------------------------------------------
  # age  - age vs output
  
 # BoxPlot
    ha |> 
      ggplot(aes(x = output, y = age))+
      geom_boxplot(fill = "orange", alpha = 0.3)+
      scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
      xlab ("Doença Cardíaca")+
      ylab("Idade")+
      meu_tema
    # > Menor chance de ataque: em torno de 60 anos
    # > Maior chance de ataque: em torno de 50
  
 # Histograma: Gráfico de Proporções de Ocorrervs Não Ocorrer
    grafico_proporcao(coluna = ha$age, bins = 10, eixox = "Idade")
    view(tabela_proporcao(coluna = ha$age, breaks = 20))
    
    # Mudando os breaks para não ter classe com apenas 1 observ.
    grafico_proporcao(coluna = ha$age, breaks = c(25,39,45,50,55,60,65,77), eixox = "Idade")
    tabela_proporcao(coluna = ha$age, breaks = c(25,39,45,50,55,60,65,77))
    # 
    # > Proporção de ataque foi maior nas faixas:    (25,39] - 75% tiveram
    #                                                (39,45] - 75% tiveram
    # > Proporção diminui nas faixas de: (55,60] - 37% tiveram
    #                                    (60,65] - 39% tiveram
  
    
# > Interpretação:  25 - 45 anos temos uma proporção de 75% de pacientes com
#   doença cardíaca. A cada idade, a chance de ter doença aumenta em 10%. A partir
#   dos 45 anos a proporção de pacientes com doença cardíaca diminui atingido o 
#   proporção mínima de 37% e 39% (na faixa de 55 - 60 anos e 60 - 65 anos, respectivamente).
#   a partir dos 45 anos a chance de ter doença cardíaca diminui.

#  >p_output: nos informa a proporção de p = 1. output = 1.
#  >logit_chance_output: nos informa a chance de output = 1 (variável resposta) dado
#   o valor de X (variável explicativa). Esta informação está em log. Então, na verdade,
#   é o log da change de p=1 dado o valor de  X.    
    
# Modelo individual
    
model_age <- glm(output ~age, data=ha, family = "binomial")
gtsummary::tbl_regression(model_age, exponentiate = TRUE)

  
#> Interpretação:
#> - Age se relaciona com a chance de doença cardíaca (p<0.05)
#> - A cada aumento em uma unidade de Age, temos uma redução de 5% na 
#>   chance de ter doença cardíaca. (Exponencial de Beta1) exp(-0.05) = 0.95 logo 1-0.95= 0.05



# Pressão arterial --------------------------------------------------------
  # trtbps: vs output
  
  # BoxPlot
  ha |> 
    ggplot(aes(x = output, y = trtbps))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    xlab ("Doença Cardíaca")
  # >  Parece não ter diferença entre as classes
  
  grafico_proporcao(coluna = ha$trtbps, bins = 10, eixox = "Pressão Arterial")
  tabela_proporcao(coluna = ha$trtbps, breaks = 10)
  
# Histograma: Mudando os breaks para não ter pontos com pouca obs.
  grafico_proporcao(coluna = ha$trtbps, breaks = c(93.9,105,
                                                   115,126,136,147,
                                                   158,168,200),
                    eixox = "Pressão Arterial")
  tabela_proporcao(coluna = ha$trtbps, breaks = c(93.9,105,
                                                  115,126,136,147,
                                                  158,168,200,330))
  # > Parece ter um proporção alta de pacientes (85%) com doença cardíaca.
  # quando a pressão arterial em repouso está entre 93.9 - 105. Depois dessa 
  # faixa o aumento da pressão arterial em repouso se mantém praticamente 
  # constante entre os pacientes. Diminuindo a proporção de doenças cardíacas
  # nos pacientes com a pressão arterial acima de 158.
  
  
# Modelo Individual
  
  model_trtbps <- glm(output ~trtbps, data=ha, family = "binomial")
  summary(model_trtbps)
  gtsummary::tbl_regression(model_trtbps, exponentiate = TRUE)

  #> Interpretação: A pressão arterial em repouso é uma variável importante.
  #> Mas ela interfere pouco na chance de doença cardíaca. O aumento da pressão
  #> arterial reduz em 2% a chance de doença cardíaca a cada unidade.


# Colesterol --------------------------------------------------------------
  # chol: vs output
  
  # BoxPlot
  ha |> 
    ggplot(aes(x = output, y = chol))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    xlab ("Doença Cardíaca")
  # > Não parece haver diferença significativa
  
  # Histograma:
  grafico_proporcao(coluna = ha$chol, bins = 15, eixox = "Colesterol")
  tabela_proporcao(coluna = ha$chol, breaks = 15)
  
 # Modelo individual
  model_chol <- glm(output ~ chol, data=ha, family = "binomial")
  summary(model_chol)
  gtsummary::tbl_regression(model_chol, exponentiate = TRUE) 
   
#> Interpretação:
#> Essa variável não altera a chance de doenças cardíacas (p>0.05; OR= 1).


  
# Frequencia cardíaca Máxima ----------------------------------------------
  # thalachh: vs output
  
  # BoxPlot
  ha |> 
    ggplot(aes(x = output, y = thalachh))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    xlab ("Doença Cardíaca")
  # > Parece que valores >150 tem maior chance
  #   valores entre 125-150 tem menor chance
  
   # Histogram
  grafico_proporcao(coluna = ha$thalachh, bins = 12, eixox = "Freq. Cardíaca Max.")
  tabela_proporcao(coluna = ha$thalachh, breaks = 12)  
  
  #> A proporção de pacientes com doenças cardíacas aumenta conforme aumenta
  #> os valores frequencia cardiaca maxima, atingindo proporções acima de 60%
  #> a partir de frequencias cardiacas acima de 147.
  
  
  # Modelo individual
  model_thalachh <- glm(output ~ thalachh, data=ha, family = "binomial")
  summary(model_thalachh)
  gtsummary::tbl_regression(model_thalachh, exponentiate = TRUE)
  
#> Frequencia cardiaca influencia na chance de doenças cardiacas em pacientes
#> O aumento em 1 unidade na frequencia cardiaca máxima, aumenta a chance de
#> de doença cardíaca em 4%. 
 


# Depressão de ST induzida por exercício ----------------------------------
  # oldpeak: vs output
  
  # BoxPlot
  ha |> 
    ggplot(aes(x = output, y = oldpeak))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    xlab ("Doença Cardíaca")
  # > Parece haver diferença: Entre 0-1 maior chance

  # Histograma
  grafico_proporcao(coluna = ha$oldpeak, bins = 12)
  tabela_proporcao(coluna = ha$oldpeak, breaks = 12)
  # ajustando os breaks
  tabela_proporcao(coluna = ha$oldpeak, breaks = c(-0.0061, 0.517,1.03,1.55,
                                                   2.07,2.58,3.1,3.62,6.21))
  grafico_proporcao(coluna = ha$oldpeak, breaks = c(-0.0061, 0.517,1.03,1.55,
                                                    2.07,2.58,3.1,3.62,6.21)) 
  
  #> Maior proporção de doença cardiaca em pacientes com números menores de 
  #> depressão de st induzida por exercício. Com a proporção menor do q 30%
  #> a partir de valores de oldpeak >2.07
  
  # Modelo individual
  model_oldpeak <- glm(output ~ oldpeak, data=ha, family = "binomial")
  summary(model_oldpeak)
  gtsummary::tbl_regression(model_oldpeak, exponentiate = TRUE)
  
#> Depressão de ST tem forte influencia na chance de doença cardiaca em pacientes
#> Ao aumentarmos os valores de oldpeak em 1 unidade, há uma redução de 61%
#> na chance de ocorrencia de doença cardiaca.
  


  
# Número de grandes vasos -------------------------------------------------
  # caa:  vs output
  
  # BoxPlot
  ha |> 
    ggplot(aes(x = output, y = caa))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    xlab ("Doença Cardíaca")
  #> Grafico horrível
  
  # Histograma
  grafico_proporcao(coluna = ha$caa, bins = 4)
  tabela_proporcao(coluna = ha$caa, breaks = 4)    
  #> Conforme o número de caa aumenta, temos uma diminuição na proporção 
  #> de ataques cardíacos;
  
  # Modelo individual
  model_caa <- glm(output ~ caa, data=ha, family = "binomial")
  summary(model_caa)
  gtsummary::tbl_regression(model_caa, exponentiate = TRUE)
  
#> Número de grandes vasos influencia fortemente na chance de doença cardiaca.
#> O aumento no número de vasos reduz a chance de doença cardiaca em 60%.
  
  
  
  # ---Categorica

# Histograma
graf_proporcao_categoricos <- ha |> 
    select(is.factor) |> 
    map(grafico_proporcao)


# Sexo --------------------------------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$sex)
  
  #Histograma
  graf_proporcao_categoricos$sex
  
#> Maior proporção de mulheres com doença cardíaca do que homens
  # checando quantidade de dados em cada categoria
  
  # Modelo individual
  model_sex <- glm(output ~ sex, data=ha, family = "binomial")
  summary(model_sex)
  gtsummary::tbl_regression(model_sex, exponentiate = TRUE)

#> Interpretação:
#> Sexo Masculino tem uma redução de 73% na chance de doença cardíaca em
#> comparação com o sexo feminino.
  
  
  # Tipo de dor no peito ----------------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$cp)

  # Histograma
  graf_proporcao_categoricos$cp    
  
#> Menor proporção de doença cardíaca em pacientes com 
#> Tipo de dor no peito angina típica (0)  

  # Modelo individual
  model_cp <- glm(output ~ cp, data=ha, family = "binomial")
  summary(model_cp)
  gtsummary::tbl_regression(model_cp, exponentiate = TRUE)
  
#> Interpretação:
#> Ter Dor no peito do tipo angina atipica, aumenta 12 vezes a chance de doença 
#> cardiaca comparada angina tipica; aumenta 10 vezes quando a dor é do tipo
#> dor não angina; e em 6 vezes quando assintomático.
  

# Glicemia ----------------------------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$fbs)

  # Histograma
  graf_proporcao_categoricos$fbs

#> Proporção de pacientes com e sem doença cardiaca parecida para glicemia
#> acima e abaixa de 120. Parece não haver influencia.
  
  # Modelo individual
  model_fbs <- glm(output ~ fbs, data=ha, family = "binomial")
  summary(model_fbs)
  gtsummary::tbl_regression(model_fbs, exponentiate = TRUE)
  
  #> Interpretação:
  #> Sem importancia (p>0.05)

  
# Eletrocardiográficos ----------------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$restecg)
  
  # Histograma
  graf_proporcao_categoricos$restecg    
  
#> Eletrocardiograma normal (0): Menor proporção de doença cardíaca
#> Eletrocardiograma com hipertrofia (2): Poucas observações (4)  

  # Modelo individual
  model_restecg <- glm(output ~ restecg, data=ha, family = "binomial")
  summary(model_restecg)
  gtsummary::tbl_regression(model_restecg, exponentiate = TRUE)
  
  #> Interpretação:
  #> Eletrocardiograma com anormalidade aumenta em 99% a chance de doença
  #> cardiaca quando comparado com eletrocardiograma normal.
  

# Inclinação do segmento ST -----------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$slp) 
  
  # Histograma
  graf_proporcao_categoricos$slp    
 
#> Inclinação descendo (2): Maior proporção de doença cardíaca

  # Modelo individual
  model_slp <- glm(output ~ slp, data=ha, family = "binomial")
  summary(model_slp)
  gtsummary::tbl_regression(model_slp, exponentiate = TRUE)
  
  #> Interpretação:
  #> Inclinação do segmento do tipo 2 aumenta 4 vezes a chance de doença cariaca
  #> comparado aos pacientes sem inclinação.


# Talassemia --------------------------------------------------------------
  # checando quantidade de dados em cada categoria
  table(ha$thall) 
  
  # Histograma
  graf_proporcao_categoricos$thall    
 
#> Talassemia Normal (2): Maior proporção de doença cardíaca
#> Poucas observações na categoria (0)
  
  # Modelo individual
  model_thall <- glm(output ~ thall, data=ha, family = "binomial")
  summary(model_thall)
  gtsummary::tbl_regression(model_thall, exponentiate = TRUE)
  
  #> Interpretação:
  #> Poucas observações na categoria zero usada como referencia. 
  #> Tenho que refazer trocando a referencia.
  
 
  # Retirando classe 0
  ha <-  ha |> 
              filter(thall != 0)

  # Refazendo análise anterior
  table(ha$thall)
  summary(ha$thall)
  
  model_thall <- glm(output ~ thall, data=ha, family = "binomial")
  summary(model_thall)
  gtsummary::tbl_regression(model_thall, exponentiate = TRUE)
 
  

# Modelagem ---------------------------------------------------------------
  
  
  # Separar os dados em calibração e validação
  set.seed(1)
  grupo = dismo::kfold(ha, 5)
  data_train <- ha[grupo !=1,] #80% (4/5 dos registros)
  data_test <- ha[grupo ==1,] #20% (1/5 dos registros)
  
  str(data_train)
  str(data_test)
  

# Modelo Logístico
  
  #. Modelo Completo
 modelo_completo <- glm(output ~ ., data = data_train, family = binomial)
 summary(modelo_completo)
   
      
 #> Variáveis Importante:
 #> Sex
 #> cp
 #> oldpeak
 #> caa
 #> exng
 
 #> Variáveis que podem ser retiradas do modelo:
 #> trtbps
 #> chol
 #> fbs
 #> restecg
 #> thalachh
 #> slp
 #> thall
 
# Variance inflation factor (Vif)
 car::vif(modelo_completo)
 
performance::check_model(modelo_completo)
plot(modelo_completo) 
# Conclusão: O fator VIF indica baixa colinearidade entre as variáveis. Avalia o quanto a variância de um coeficiente de regressão estimado aumenta se as suas preditoras estiverem correlacionadas. Se nenhum fator estiver correlacionado, os VIFs serão todos proximos a 1.
 
 
 
  #. Teste: Modelo Completo
 
        # 1. ROC
  output_por_faixa_de_probabilidades <- data_test_pred |> 
    mutate(faixa_de_risco = cut_interval(fitted_exp, 7)) |> 
    group_by(faixa_de_risco) |> 
    summarise(
      n = n(),
      probabilidade_de_output_media = mean(fitted),
      risco_de_output = mean(output == 1),
      logito_do_risco_de_output = log(risco_de_output/(1 - risco_de_output)))
  output_por_faixa_de_probabilidades

library(pROC)
curva_roc <- roc(output_por_faixa_de_probabilidades, output, probabilidade_de_output)
auc(curva_roc)
p <- plot(curva_roc)
p +annotate("text",x = 0.6, y = 0.8, label = "Area under the curve: 0.9496")




# 2. Analisar previsões ---------------------------------------------------

        
      # Predicao com dados teste
      fitted_results <- dismo::predict(modelo_completo, newdata = data_test, type = "response")
      fitted_results_cat <- ifelse(fitted_results > 0.7,1,0) #threshold
      
      
      # Unir: Observados vs Preditos
      data_test_pred <- data_test |> 
                      add_column(fitted =  fitted_results,
                                 fitted_cat = fitted_results_cat ) |>
                      mutate(fitted_exp = exp(fitted_results))
      
      # Verificar quantidade de acertos
      table(data_test_pred$output == data_test_pred$fitted_cat) 
      
      
      # Acurácia do Modelo
      Classifi <- mean(data_test_pred$fitted_cat != data_test_pred$output)
      print(paste('Accuracy',round(1-Classifi, digits = 3)))
      
      
      data_test_pred <- data_test_pred |> 
        mutate(fitted_cat = as_factor(fitted_cat))
      
      
      # Metricas Desempenho: Tabela de confusão
      caret::confusionMatrix(data = data_test_pred$output, reference = data_test_pred$fitted_cat)
      
      
      #> Interpretação:
      #> 
      #> 
    
      
    # Gráfico
      
      #1
      
      data_test_pred %>%
        ggplot(aes(x = fitted_exp, fill = output)) +
        geom_boxplot(alpha = 0.3)
      

      #2
     output_por_faixa_de_probabilidades |> 
        ggplot(aes(x = probabilidade_de_output_media, y = risco_de_output)) +
        geom_point() +
        geom_label(aes(label = scales::percent(probabilidade_de_output_media,1))) +
        meu_tema
      
      

# Modelos Alternativos ----------------------------------------------------

      
#. Modelo 1: somente com variáveis significativas no modelo completo
     #> Acurácia e desempenho diminuiu em comparação com o modelo completo

modelo1 <- glm(output ~ sex + cp+ oldpeak + caa + exng, data = data_train, family = binomial)
 summary(modelo1)
 summary(modelo_completo)

# Analisar predição
analise_predicao(modelo1)  

# Metricas Desempenho: Tabela de confusão
desempenho(modelo1) 




 
  
# Analise: Possibilidade de interações ------------------------------------

# Padronizar a função
graf_inter <- function(y, fill){
  ha |> 
    ggplot(aes(x = output, y = y, fill = fill))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    meu_tema+
    xlab ("Doença Cardíaca")+
    theme(legend.position = "bottom")}

# AGE vs categoricas
graf_inter(y = ha$age, fill = ha$sex)
graf_inter(y = ha$age, fill = ha$cp)
graf_inter(y = ha$age,fill = ha$fbs)
graf_inter(y = ha$age,fill = ha$restecg)
graf_inter(y = ha$age,fill = ha$exng)
graf_inter(y = ha$age,fill = ha$slp)
graf_inter(y = ha$age,fill = ha$thall)


# trtbps vs categoricas
graf_inter(y = ha$trtbps, fill = ha$sex)
graf_inter(y = ha$trtbps, fill = ha$cp)
graf_inter(y = ha$trtbps,fill = ha$fbs)
graf_inter(y = ha$trtbps,fill = ha$restecg)
graf_inter(y = ha$trtbps,fill = ha$exng)
graf_inter(y = ha$trtbps,fill = ha$slp)
graf_inter(y = ha$trtbps,fill = ha$thall)

#chol
graf_inter(y = ha$chol, fill = ha$sex)
graf_inter(y = ha$chol, fill = ha$cp)
graf_inter(y = ha$chol,fill = ha$fbs)
graf_inter(y = ha$chol,fill = ha$restecg)
graf_inter(y = ha$chol,fill = ha$exng)
graf_inter(y = ha$chol,fill = ha$slp)
graf_inter(y = ha$chol,fill = ha$thall)

#thalachh
graf_inter(y = ha$thalachh, fill = ha$sex)
graf_inter(y = ha$thalachh, fill = ha$cp)
graf_inter(y = ha$thalachh,fill = ha$fbs)
graf_inter(y = ha$thalachh,fill = ha$restecg)
graf_inter(y = ha$thalachh,fill = ha$exng)
graf_inter(y = ha$thalachh,fill = ha$slp)
graf_inter(y = ha$thalachh,fill = ha$thall)

#oldpeak
graf_inter(y = ha$oldpeak, fill = ha$sex)
graf_inter(y = ha$oldpeak, fill = ha$cp)
graf_inter(y = ha$oldpeak,fill = ha$fbs)
graf_inter(y = ha$oldpeak,fill = ha$restecg)
graf_inter(y = ha$oldpeak,fill = ha$exng)
graf_inter(y = ha$oldpeak,fill = ha$slp)
graf_inter(y = ha$oldpeak,fill = ha$thall)

#caa
graf_inter(y = ha$caa, fill = ha$sex)
graf_inter(y = ha$caa, fill = ha$cp)
graf_inter(y = ha$caa,fill = ha$fbs)
graf_inter(y = ha$caa,fill = ha$restecg)
graf_inter(y = ha$caa,fill = ha$exng)
graf_inter(y = ha$caa,fill = ha$slp)
graf_inter(y = ha$caa,fill = ha$thall)



# Opção 2: sintetizando para gerar graficos -------------------------------


graf_inter2 <- function(fill){
  ha |> 
    ggplot(aes(x = output, y = age, fill = fill))+
    geom_boxplot()+
    scale_x_discrete(labels = c("Menor Chance", "Maior Chance"))+
    meu_tema+
    xlab ("Doença Cardíaca")+
    theme(legend.position = "bottom")}

ha |> 
  select(where(is.factor), age) |> 
  map(.f = graf_inter2)






