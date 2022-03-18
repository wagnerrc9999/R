
#Instalando os pacotes. para instalar tire o # da linha e execute a linha.
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("AER")


#Carregando Os Pacotes
library(dplyr)
library(ggplot2)
library(stringr)
library("AER")



# Carregar dados
Apli_Saud_2019 <- read.csv("http://dados.tce.rs.gov.br/dados/municipal/saude-indice/2019.csv")
Apli_Saud_2020 <- read.csv("http://dados.tce.rs.gov.br/dados/municipal/saude-indice/2020.csv")


# vamos separar as Colunas de recebimento, despesas e receitas. trabalharemos apenas com elas.
# Selecionar colunas com Dplyer
saude_2019 <-arrange(select(Apli_Saud_2020, NOME, VL_RECEITA, VL_DESPESA, CD_RECEBIMENTO), VL_DESPESA-CD_RECEBIMENTO )
saude_2020 <-arrange(select(Apli_Saud_2019, NOME, VL_RECEITA, VL_DESPESA, CD_RECEBIMENTO),VL_DESPESA-CD_RECEBIMENTO )

   # Imagine que queremos selecionar as LINHAS que estão acima e abaixo da MÈDIA.
   #Para saber qual a média das coluna rode o seguinte comando:
   #2019
   mean(x = saude_2019$CD_RECEBIMENTO)
   mean(x = saude_2019$VL_RECEITA)
   mean(x = saude_2019$VL_DESPESA)
   #2020
   mean(x = saude_2020$CD_RECEBIMENTO)
   mean(x = saude_2020$VL_RECEITA)
   mean(x = saude_2020$VL_DESPESA)
   #Abaixo da média
   Recib_abaix_med <-filter(saude_2019,CD_RECEBIMENTO <"235668.6")
   # Recebimento e as Despesa estão abaixo da média.
   Recib_abaix_med2019 <-filter(saude_2019,VL_RECEITA <45438193 , VL_DESPESA <9529018)
   Recib_abaix_med2020 <-filter(saude_2020,VL_RECEITA <45438193 , VL_DESPESA <9529018)
   
   
   # Recebimento Acima da Média
   Recib_acima_med <-filter(saude_2020,CD_RECEBIMENTO >"235668.6")
   # Recebimento e as Despesa estão acima da média.
   Recib_acima_med2019 <-filter(saude_2019,VL_RECEITA >45438193 , VL_DESPESA >9529018)
   Recib_acima_med2020 <-filter(saude_2020,VL_RECEITA >45438193 , VL_DESPESA >9529018)
   
   
#rode esse comando no terminal
tail(Recib_acima_med2)
tail(Recib_acima_med)


# Gráfico de Dispersão 2019
plot(Recib_abaix_med2019$VL_DESPESA ~ Recib_abaix_med2019$CD_RECEBIMENTO)
plot(Recib_abaix_med2019$VL_RECEITA ~ Recib_abaix_med2019$CD_RECEBIMENTO)
plot(Recib_abaix_med2019$VL_DESPESA ~ Recib_abaix_med2019$VL_RECEITA)

plot(Recib_acima_med2019$VL_DESPESA ~ Recib_acima_med2019$CD_RECEBIMENTO)
plot(Recib_acima_med2019$VL_RECEITA ~ Recib_acima_med2019$CD_RECEBIMENTO)
plot(Recib_acima_med2019$VL_DESPESA ~ Recib_acima_med2019$VL_RECEITA)

# Gráfico de Dispersão 2020
plot(Recib_abaix_med2020$VL_DESPESA ~ Recib_abaix_med2020$CD_RECEBIMENTO)
plot(Recib_abaix_med2020$VL_RECEITA ~ Recib_abaix_med2020$CD_RECEBIMENTO)
plot(Recib_abaix_med2020$VL_DESPESA ~ Recib_abaix_med2020$VL_RECEITA)

plot(Recib_acima_med2020$VL_DESPESA ~ Recib_acima_med2020$CD_RECEBIMENTO)
plot(Recib_acima_med2020$VL_RECEITA ~ Recib_acima_med2020$CD_RECEBIMENTO)
plot(Recib_acima_med2020$VL_DESPESA ~ Recib_acima_med2020$VL_RECEITA)


# Para a Regressão Linear, vamos usar as despesas e as receitas.

#TRANSFORMAÇẼS LOGARITIMOS DO EXPONENCIAL PARA O LOGARITIMO.

                          # Log-linear
# lOG-LINEAR É QUANDO TENTAMOS TRANFORMAR OS DADOS DISPERSOS EM UMA RETA

# Log-linear com as despesas (ONDE UM É A VARIAVEL Y E O OUTRO É A VARIAVEL X)
Recib_abaix_med2019$VL_DESPESA_log <- log(Recib_abaix_med2019$VL_DESPESA)
Recib_acima_med2019$VL_DESPESA_log <- log(Recib_acima_med2019$VL_DESPESA)
Recib_abaix_med2020$VL_DESPESA_log <- log(Recib_abaix_med2020$VL_DESPESA)
Recib_acima_med2020$VL_DESPESA_log <- log(Recib_acima_med2020$VL_DESPESA)
# Gráfico plotagem do log-linear (AQUI UM GRÁFICO DE DISPERSSÃO)
plot(Recib_abaix_med2019$VL_DESPESA_log ~ Recib_abaix_med2019$VL_RECEITA)
plot(Recib_acima_med2019$VL_DESPESA_log ~ Recib_acima_med2019$VL_RECEITA)
plot(Recib_abaix_med2020$VL_DESPESA_log ~ Recib_abaix_med2020$VL_RECEITA)
plot(Recib_acima_med2020$VL_DESPESA_log ~ Recib_acima_med2020$VL_RECEITA)


# log-linear com as Receitas (SEGUINDO A MESMA COISA INVERTIDA)
Recib_abaix_med2019$VL_RECEITA_log <- log(Recib_abaix_med2019$VL_RECEITA)
Recib_acima_med2019$VL_RECEITA_log <- log(Recib_acima_med2019$VL_RECEITA)
Recib_abaix_med2020$VL_RECEITA_log <- log(Recib_abaix_med2020$VL_RECEITA)
Recib_acima_med2020$VL_RECEITA_log <- log(Recib_acima_med2020$VL_RECEITA)
# Gráfico plotagem do log-linear
plot(Recib_abaix_med2019$VL_RECEITA_log ~ Recib_abaix_med2019$VL_DESPESA)
plot(Recib_acima_med2019$VL_RECEITA_log ~ Recib_acima_med2019$VL_DESPESA)
plot(Recib_abaix_med2020$VL_RECEITA_log ~ Recib_abaix_med2020$VL_DESPESA)
plot(Recib_acima_med2020$VL_RECEITA_log ~ Recib_acima_med2020$VL_DESPESA)


# Regressão linear
  # criando vários modelos (mod). UM DEMOLO SIMPLES ONDE CRIAMOS UMA VARIAVEL.
mod <- lm(Recib_abaix_med2019$VL_DESPESA_log~Recib_abaix_med2019$VL_RECEITA)
mod1 <- lm(Recib_acima_med2019$VL_DESPESA_log~Recib_acima_med2019$VL_RECEITA)
mod2 <- lm(Recib_abaix_med2020$VL_DESPESA_log~Recib_abaix_med2020$VL_RECEITA)
mod3 <- lm(Recib_acima_med2020$VL_DESPESA_log~Recib_acima_med2020$VL_RECEITA)

mod4 <- lm(Recib_abaix_med2019$VL_RECEITA_log~Recib_abaix_med2019$VL_DESPESA)
mod5 <- lm(Recib_abaix_med2019$VL_RECEITA_log~Recib_abaix_med2019$VL_DESPESA)
mod6 <- lm(Recib_acima_med2020$VL_RECEITA_log~Recib_acima_med2020$VL_DESPESA)
mod7 <- lm(Recib_acima_med2020$VL_RECEITA_log~Recib_acima_med2020$VL_DESPESA)


#somatório
       # no somatório veremos, standard error: deverar ser o menor possivel.
       # P-value (p valor) deve estar:
summary(mod)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)

# Previsão

prox_dias_2019_baixo <- as.data.frame(250:300)
names(prox_dias_2019_baixo)<-"NOME"

prox_dias_2019_acima <- as.data.frame(98:158)
names(prox_dias_2019_acima)<-"NOME"

prox_dias_2020_baixo <- as.data.frame(250:300)
names(prox_dias_2020_baixo)<-"NOME"

prox_dias_2020_acima <- as.data.frame(102:162)
names(prox_dias_2020_acima)<-"NOME"

prox_dias_2019_acima <- predict(mod, prox_dias_2019_acima)


# Executa previsão (PARA SABER SOBRE OS OUTROS MODELOS TROQUE O MOD POR OUTRO MOD POR EXEMPLO MOD3 OU MOD4
# O MOD QUER DIZER MODELO NOSSO EXEMPLO CRIAMOS 8 MODELOS ALI EM CIMA BASEADOS NO ACIMA DA MÉDIA E ABAIXO 
# DA MÉDIA NOS ANOS DE 2019 E 2020)
prox_dias_2019_baixo <- predict(mod, prox_dias_2019_baixo)
prox_dias_2019_acima <- predict(mod, prox_dias_2019_acima)

