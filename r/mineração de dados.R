loja <- read.csv2("/home/wagner/r/lojas.csv", sep=",", dec=",")->loja2
bolsa <- read.csv("/home/wagner/r/202001_BolsaFamilia_Pagamentos.csv")

Sys.setlocale("LC_ALL", "C")

  str(loja2)
  summary(loja2)
  install.packages("e1071")
  library(e1071)
  
  # An??lise explorat??ria - da loja
  plot(loja2$GMV, type="l")
  # Analise de correlaçãoo entre loja
  plot(loja2$Comissao ~ loja2$GMV)
  
  # Separa????o de bases de treino e teste
  treino <- loja2[1:112,]
  teste  <- loja2[113:124,]
  
  # Modelo (algoritmo)
  mod2 <- lm(Comissao~GMV, data=treino)
  #mod1 <- lm(PIB~PIBi2, data=treino)
  #mod1 <- lm(PIB~PIBi1+PIBi2, data=treino)
  #mod1 <- lm(PIB~PIBi1+PIBi2+PIBi3, data=treino)
  #mod1 <- lm(PIB~PIBi1+PIBi2+PIBi3+PIBi4, data=treino)
  #mod1 <- lm(PIB~PIBi1+PIBi2+PIBi4, data=treino)
  #mod1 <- lm(PIB~PIBi1+PIBi2+PIBi4+PIBi6, data=treino)
  mod1 <- lm(loja2~GMV+Comissao+TR+Bonus, data=treino)
  prev <- predict(mod1, newdata=teste)
  erro <- mean(abs(teste$PIB - prev))
  # Erro absoluto m??dio
  erro
  # Erro percentual m??dio
  erro/mean(teste$PIB)
  
  summary(mod2)  
  
  
  
  # data cleaniny
  library(dplyr)
  loja2 %>% select(c(-1,-2))->loja2
  loja2$GMV<-factor(loja2$GMV,labels-c("yes","no"))
  loja2$Bonus<-factor(loja2$Bonus,labels-c("no","yes"))
  houses$waterfront<- factor(houses$waterfront,labelws-c("no","yes"))
  loja2$fuel<-factor(houses$fuel,labels-c("gas","eletric","oll"))
  houses$sewer<- factory(houses$sewer,labels-s("nome","private","public"))
# Data visualization
library(ggplot2)
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40)
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40,fill-"lightblue",col="blue")
ggplot(datadata_houses,aes(y_price,x-waterfront,fill-waterfront))+geom_boxplot()
ggplot(data=houses,aes(y=price,x=air_cond,fill=air_cond))+geom_boxplot()

ggplot(houses,aes(x=living_area,y=price))+geom_point()+geom_smooth(method="lm",se-f)
