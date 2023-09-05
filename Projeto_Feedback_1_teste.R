# Projeto Curso Formação Cientista de dados da Data Science Academy


# Descrição Projeto -------------------------------------------------------

# Machine Learning em Logística Prevendo o Consumo de Energia de Carros
# Elétricos.
# Projeto proposto no curso Formação Cientista de Dados 3.0
# da Data Science Academy.

# Uma empresa da área de transporte e logística deseja migrar sua frota 
# para carros elétricos com o objetivo de reduzir os custos.
# Antes de tomar a decisão, a empresa gostaria de prever o consumo de 
# energia de carros elétricos com base em diversos fatores de utilização e
# características dos veículos.


# Fonte dados -------------------------------------------------------------

  # Hadasik, Bartłomiej; Kubiczek, Jakub (2021),
  # “Dataset of electric passenger cars with their specifications”,
  # Mendeley Data, V2, doi: 10.17632/tb9yrptydn.2
  
  # https://data.mendeley.com/datasets/tb9yrptydn/2
  # https://www.datascienceacademy.com.br


# Objetivo ----------------------------------------------------------------

# Construir um modelo de Machine Learning capaz de prever o consumo
# de energia de veículos elétricos.
            # Variável preditora Média de consumo


# Diretório -------------------------------------------------------------

setwd("/home/rb/Documents/Learn/DSA/FDS/Cap01-BigDataAnalyticsR_Azure/mod_20-Projeto_com_Feedback")
getwd()
# Define variável global para não mostrar notação científica
options(scipen = 100, digits = 4)

# Install Packages --------------------------------------------------------
# install.packages('naniar')
# install.packages('C50')
# install.packages('DataCombine')
# install.packages("psych")
# install.packages('ggcorrplot')

# Import ------------------------------------------------------------------
require(naniar)
require(tidyverse)
require(visdat)
require(C50)
library(caret)
require(corrplot)
require(randomForest)

# Dados -------------------------------------------------------------------

dados <- readxl::read_excel('FEV-data-Excel.xlsx')
glimpse(dados)

# Ajuste Nomes Colunas ----------------------------------------------------
nomesCol <- c(
  'Nome', 'Marca', 'Modelo', 'Preço_min', 'Potência', 'Torque_Nm', 'Tipo_freios',
  'Tração', 'Capacidade_bateria_kWh', 'Autonomia', 'Dist_eixos_cm', 'Comp_cm',
  'Larg_cm', 'Alt_cm', 'Peso_min_kg', 'Peso_max_kg', 'Carga_max_kg', 'Lugares',
  'Qtde_portas', 'Tam_pneus_pol', 'Vel_max_kph', 'Porta_malas_lt', 'Acel_0_100kph_s',
  'Carga_bat_max_kW', 'Media_cons_kWh_100km'
)
colnames(dados) <- nomesCol
colnames(dados)
rm(nomesCol)
# Explorando --------------------------------------------------------------

vis_dat(dados) # Visual. dos tipos e NA
vis_miss(dados, sort_miss = TRUE) # Visual. NA % x variável

# Todos os incomplete cases
dados_inc <- dados[!complete.cases(dados), ]
View(dados_inc)

# Cria Gráfico e Tabela com Index (case), Qtde. Faltante e percentual
dados %>% 
  gg_miss_case(order_cases = FALSE) +
  labs(x = 'Index', y = 'Faltantes') +
  View(miss_case_summary(dados))

# Mostra a quantidade de dados faltantes por marca
dados %>% 
  gg_miss_var(facet = Marca) +
  labs(x = 'Variáveis', y = 'Qtde. Missing')

# Conclusão sobre exploração ----------------------------------------------

# É possível observar a concentração de missing na região do index 40,
# estes são da marca Tesla, pela importância vou buscar informações 
# sobre os dados faltantes.
# Também defini que devido ao tamanho diminuto do dataset vou tentar encontrar,
# os dados das outras marcas. Caso não tenha procedência ou não encontre 
# irei simplesmente omitir a observação.

# Também se faz necessária a transformação de alguns tipos de variáveis

# Tratamento dos Dados ----------------------------------------------------

# Peso_max-kg e Carga_max-kg, vamos considerar o
# peso mínimo, pois não há informação referente a peso do carro quando calculado o
# consumo. Com relação ao Nome e Marca não serão utilizadas no modelo
# (é irrelevante para a análise).

unique(dados$Tipo_freios)

# Tipo_freios, Mercedes-Benz EQV (long) tem freios a disco nas quatro rodas.
# (Fonte: Bard Google baseado no Site Mercedes_Benz)
dados$Tipo_freios <- replace_na(dados$Tipo_freios, 'disc (front + rear)')

# Aceleraçao fonte bard google
# Peugeot e-2008 é de 8,4 segundos (site da Peugeot)
# Mercedes-Benz EQV (long) tem uma aceleração de 0 a 100 km/h em 9 segundos (site da Mercedes-Benz)
# Nissan e-NV200 Evalia é de 14,0 segundos (site da Nissan)

# Lista carros
list_carros_acel <-  dados[!complete.cases(dados$Acel_0_100kph_s), ] %>% 
  select(c('Nome'))

# Consumo médio kWh/100km
# Citroën ë-C4: 16 kWh/100km (source: European Union Energy Label)
# Peugeot e-2008: 15.5 kWh/100km (source: European Union Energy Label)
# Tesla Model 3 Standard Range Plus: 16 kWh/100km (source: Tesla)
# Tesla Model 3 Long Range: 15.2 kWh/100km (source: Tesla)
# Tesla Model 3 Performance: 16.6 kWh/100km (source: Tesla)
# Tesla Model S Long Range Plus: 19 kWh/100km (source: Tesla)
# Tesla Model S Performance: 20 kWh/100km (source: Tesla)
# Tesla Model X Long Range Plus: 22.6 kWh/100km (source: Tesla)
# Tesla Model X Performance: 23.6 kWh/100km (source: Tesla)

# Lista carros
list_carros_cons <-  dados[!complete.cases(dados$Media_cons_kWh_100km), ] %>% 
  select(c('Nome'))

dados_missing_cons <- tibble(Media_cons_kWh_100km = c(16, 15.5, 16, 15.2, 16.6, 19, 20, 22.6, 23.6), list_carros_cons) 
view(dados_missing_cons)

dados_missing_acel <- tibble(Acel_0_100kph_s = c(8.4, 9, 14), list_carros_acel) 
view(dados_missing_acel)

# ----- Tratando Missing Aceleração
dados <- dados %>% 
  filter(is.na(Acel_0_100kph_s)) %>% # pegamos só as linhas vazias
  select(-Acel_0_100kph_s) %>%  # tiramos a variável aceleração
  left_join(dados_missing_acel, by = "Nome") %>%  # fazemos o join com o outro bd
  bind_rows(dados %>% filter(!is.na(Acel_0_100kph_s))) # empilhamos os dois

# ----- Tratando Missing Média Consumo
dados <- dados %>% 
  filter(is.na(Media_cons_kWh_100km)) %>%
  select(-Media_cons_kWh_100km) %>%
  left_join(dados_missing_cons, by = "Nome") %>%
  bind_rows(dados %>% filter(!is.na(Media_cons_kWh_100km)))

dim(dados)
is_na(dados) # Porque o resultado é FALSE? sendo que possui NAs.
vis_miss(dados, sort_miss = TRUE)
rm(dados_missing_acel, dados_missing_cons, list_carros_acel, list_carros_cons)

# Seleção de variáveis ----------------------------------------------------
# Vou omitir as colunas Peso_max, carga_max, porta_malas. Possuem valores
# missing variáveis não devem influenciar no consumo, pois não temos informação 
# das condições de realização das medições, se com carga ou não, porta malas
# já está incluído no peso minimo.

# Ver o cruzamento dos dados
gg_miss_upset(dados)

dados[!complete.cases(dados), ] %>% 
  select(c("Peso_max_kg", "Carga_max_kg", 'Porta_malas_lt'))

dados_v1 <- dados %>% 
  select(!c('Nome', 'Marca', "Modelo", "Peso_max_kg", "Carga_max_kg", 'Porta_malas_lt'))

dim(dados_v1)

vis_miss(dados_v1, sort_miss = TRUE)

str(dados_v1)

# Tratando Types ----------------------------------------------------------

vis_dat(dados_v1) # visualiza types

### Dados Categóricos
unique(dados_v1$Tipo_freios)
unique(dados_v1$Tração)

names(dados_v1)

# Encoding 
dados_v2 <- dados_v1 %>% 
  mutate(Tipo_freios_enc = if_else(dados_v1$Tipo_freios == "disc (front + rear)", 1, 2)) %>% 
  mutate(Tração_enc = if_else(dados_v1$Tração == '2WD (front)', 1, if_else(dados_v1$Tração == "4WD", 3, 2)))

vis_miss(dados_v2)

df <- dados_v2

# Visualização do DF ------------------------------------------------------
# Alguma noção da distribuição dos dados

# Correlação
psych::pairs.panels(df)

# Evitando overfitting, retirar variáveis preditoras com correlação > 0.80

df_select <- df %>% 
  select(!c('Preço_min', 'Potência', 'Comp_cm', 'Peso_min_kg',
            'Torque_Nm', 'Capacidade_bateria_kWh', 'Vel_max_kph',
            'Tipo_freios', 'Tração', 'Tam_pneus_pol'))

psych::pairs.panels(df_select)

# Testes ------------------------------------------------------------------

# ?corrplot
# m1 = cor(df_num)
# 
# corrplot(m1, method = 'number', type = 'lower', tl.pos = 'ld')
# 
# colunas <- c("Preço_min", "Comp_cm", "Media_cons_kWh_100km", "Acel_0_100kph_s",
#              "Tipo_freios_enc", "Tração_enc")
# 
# m2 <- cor(df_num[, colunas])
# corrplot(m2, method = 'number', type = 'lower', tl.pos = 'ld')
# 
# df_num_fil <- df[, colunas]
# 
# glimpse(df_num_fil)
# 
# pairs(df_num_fil)
# ?boxplot
# boxplot(df_num_fil$Preço_min, main = "Preço")
# boxplot(df_num_fil$Comp_cm, main = "Comprimento")
# boxplot(df_num_fil$Media_cons_kWh_100km, main = "Média Consumo")
# boxplot(df_num_fil$Acel_0_100kph_s, main = "Aceleração 0-100 Km/h")
# 
# summary(df_num_fil$Preço_min)
# dados[which(dados$Comp_cm < 300), ]
# dados[which(dados$Preço_min > 500000), ]

# hist(df$Potência)
# hist(df$Torque_Nm)
# hist(df$Capacidade_bateria_kWh)
# hist(df$Autonomia)
# hist(df$Media_cons_kWh_100km)
# 
# df_num <- df %>% 
#   select_if(is.numeric)
# 
# glimpse(df_num)
# 
# pairs(df_num)

# Normalize data-----------------------------------------------------------

# Resultados da exploração apresentaram inconsistências, vamos normalisar
# para ver se existe alguma interferência pela amplitude dos dados.

non_numeric <- df[, !sapply(df, is.numeric)]

df_norm <- df_select %>% 
  select_if(is.numeric) %>% 
  lapply(scale, center = T) %>% 
  as_tibble()

tab_corr <- df_norm %>% 
  select_if(is.numeric) %>% 
  cor()
view(tab_corr)

corrplot(tab_corr, method = 'square')


df_norm <- as_tibble(cbind(non_numeric, df_norm))
View(df_norm)


# Saída dos gráficos
par(mfrow = c(2,2)) # 2 linhas e 2 colunas (monta 4 gráficos)

names(df_select)

df_select_numeric <- df_select %>% 
  select_if(is.numeric)


# Seleção de variáveis lineares -------------------------------------------

# Variável dependente: Media_cons_kWh_100km
# Variáveis Independentes: Autonomia, Dist_eixos_cm, Alt_cm, Qtde_portas,
# Carga_bat_max_kW, Tração_enc

#  H0: Não há relação linear entre variáveis dependente e independentes.
#  H1: Há relação linear entre variáveis dependente e independentes.

lm_mod <- lm(Media_cons_kWh_100km ~ . , df_select_numeric)
plot(lm_mod, main = 'lm_mod')
summary(lm_mod)

# Variáveis com valor p > 0.05 falhamos em rejeitar H0, sendo para este conjunto
# de dados, há evidências significativas de não haver relação de linearidade entre
# as variáveis.('Larg_cm', 'Lugares', 'Acel_0_100kph_s', 'Tipo_freios_enc')
# Sendo, não serão utilizadas na criação do modelo

# Tudo - Lrg_cm
lm_mod2 <- lm(Media_cons_kWh_100km ~ 
              + Autonomia
              + Dist_eixos_cm
              + Alt_cm
              + Qtde_portas
              + Carga_bat_max_kW
              + Tração_enc
              + Lugares
              + Acel_0_100kph_s
              + Tipo_freios_enc
              , df_select_numeric )
summary(lm_mod2)

# - Larg_cm e - Lugares
lm_mod3 <- lm(Media_cons_kWh_100km ~ 
                + Autonomia
              + Dist_eixos_cm
              + Alt_cm
              + Qtde_portas
              + Carga_bat_max_kW
              + Tração_enc
              + Acel_0_100kph_s
              + Tipo_freios_enc
              , df_select_numeric )
summary(lm_mod3)

# - Larg_cm e - Lugares - Tipo_freios_enc
lm_mod4 <- lm(Media_cons_kWh_100km ~ 
                + Autonomia
              + Dist_eixos_cm
              + Alt_cm
              + Qtde_portas
              + Carga_bat_max_kW
              + Tração_enc
              + Acel_0_100kph_s
              , df_select_numeric )
summary(lm_mod4)

# - Larg_cm e - Lugares - Tipo_freios_enc - Acel_0_100kph_s
lm_mod5 <- lm(Media_cons_kWh_100km ~ 
                + Autonomia
              + Dist_eixos_cm
              + Alt_cm
              + Qtde_portas
              + Carga_bat_max_kW
              + Tração_enc
              , df_select_numeric )
summary(lm_mod5)

# Com valores Normalizados
df_norm <- as_tibble(scale(df_select_numeric))
lm_mod6 <- lm(Media_cons_kWh_100km ~ 
              + Autonomia
              + Dist_eixos_cm
              + Alt_cm
              + Qtde_portas
              + Carga_bat_max_kW
              + Tração_enc
              , df_norm)
summary(lm_mod6)

# Obs: valores Normalizados não apresentaram mudanças nos resultados.

# Normalidade dos resíduos (Shapiro)
# H0: distribuição normal dos dados => p > 0,05
# H1: distribuição não é normal => p <= 0,05

shapiro.test(lm_mod5$residuals)
# Apresenta uma distribuição normal dos resíduos

# outliers resíduos
summary(rstandard(lm_mod5))
# valor min acima de -3 e max abaixo de 3, não apresentam outliers
# mediana tendendo a zero indicando normalidade dos dados.


# Escolha das variáveis preditoras + significativas e Target --------------

names(df)

vetor_var <- c('Autonomia', 
               'Dist_eixos_cm',
               'Alt_cm',
               'Qtde_portas',
               'Carga_bat_max_kW',
               'Tração_enc',
               'Media_cons_kWh_100km')

df_var_sign <- df[ , vetor_var]
dim(df_var_sign)
str(df_var_sign)


# Train/Test --------------------------------------------------------------

# Vou utilizar 85% para treino, devido só haverem 42 observações
set.seed(7)
linhas <- sample(1:nrow(df_var_sign), 0.85 * nrow(df_var_sign))

dados_treino <- df_var_sign[linhas,]
dados_teste <- df_var_sign[-linhas,]
dim(dados_treino)
dim(dados_teste)
str(dados_treino)

# Modelos -----------------------------------------------------------------

#### Regressão linear
mod_reg_l <- lm(Media_cons_kWh_100km ~ . , dados_treino)
summary(mod_reg_l)

mod_reg_l2 <- lm(Media_cons_kWh_100km ~ . 
                 - Qtde_portas,
                 dados_treino)
summary(mod_reg_l2)

mod_reg_l3 <- lm(Media_cons_kWh_100km ~ . 
                 - Qtde_portas
                 - Dist_eixos_cm,
                 dados_treino)
summary(mod_reg_l3)


mod_reg_l4 <- lm(Media_cons_kWh_100km ~  
                 + Autonomia
                 + Carga_bat_max_kW
                 + Tração_enc,
                 dados_treino)
summary(mod_reg_l4)

mod_reg_l5 <- lm(Media_cons_kWh_100km ~ . 
                 - Alt_cm,
                 dados_treino)
summary(mod_reg_l5)

anova(mod_reg_l5)

mod_reg_l6 <- lm(Media_cons_kWh_100km ~ . 
                 - Alt_cm
                 - Qtde_portas
                 - Carga_bat_max_kW,
                 dados_treino)
summary(mod_reg_l6)

anova(mod_reg_l6)

#### Modelo Escolhido mod_reg_l
# Apresenta menor RSE e maior R-square

summary(mod_reg_l)
anova(mod_reg_l)
# Vou manter variável Alt_cm, apesar de no teste apresentar pouca significancia
# quando retirada diminui o R-square.

# Distribuição dos resíduos
plot(rstudent(mod_reg_l) ~ fitted(mod_reg_l), pch = 19, main = 'Dist. Resíduos')
abline(h = 0, lty = 2, col = 'red')
# Parece uma distr. aleatória

# Normaliade dos resíduos
hist(x = mod_reg_l$residuals, col = 'gray', xlab = 'Resíduos', ylab = 'Densidade de Probabilidade',
     probability = TRUE, main = 'Histograma Resíduos') 
lines(density(mod_reg_l$residuals))
# Desenho da Distribuição tem aparencia de uma curva normal.

# Shapiro
# H0 - Apresenta dist. Normal
# H1 - Não Apresenta dist. normal

shapiro.test(mod_reg_l$residuals)
# p > 0.05 há evidências significativas de possuir distribuição normal

#### Random Forest
mod_rf <- randomForest(Media_cons_kWh_100km ~ . , 
                       data = dados_treino, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = FALSE)

print(mod_rf)

mod_rf2 <- randomForest(Media_cons_kWh_100km ~ . , 
                       data = dados_treino, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = FALSE)

# resultados muito próximos entre a regressão linear e a Random Forest, vou optar 
# pela regressão para previsão.


# Previsão ----------------------------------------------------------------

previsao <- predict(mod_reg_l, dados_teste)
print(previsao)

df_regLinear <- tibble(Observado = dados_teste$Media_cons_kWh_100km,
                       Previsto = predict(mod_reg_l, newdata = dados_teste))

df_regLinear <- df_regLinear %>% 
  mutate(residuos = Observado - Previsto)

view(df_regLinear)

mean(abs(df_regLinear$residuos))





