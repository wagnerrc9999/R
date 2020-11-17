###pré processamento de dados###


##Quando se trata de aprendizado de máquina e inteligência artificial, 
#existem apenas algumas linguagens de programação com melhor desempenho 
#para escolher. No tutorial anterior, aprendemos como fazer o pré-processamento
#de dados em Python . Como R está entre os melhores desempenhos em Data 
#Science, neste tutorial, aprenderemos a executar a tarefa de Pré-processamento
#de Dados com R.

#Lidando com dados ausentes
#Lidando com dados categóricos
#Dividindo o conjunto de dados em conjuntos de treinamento e teste
#Escalando os recursos 


# para importar o conjunto de dados.
dataset = read.csv('dataset.csv') 

"Como se pode ver, este é um conjunto de dados simples que consiste em
quatro recursos. O fator dependente é a coluna 'item_comprado'. Se o 
conjunto de dados acima for usado para aprendizado de máquina, a idéia 
será prever se um item foi comprado ou não, dependendo do país, idade e
salário de uma pessoa. Além disso, as células destacadas com o valor 
'NA' denotam valores ausentes no conjunto de dados."

#Como lidar com valores ausentes

  dataset$age = ifelse(is.na(dataset$age),ave(dataset$age, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$age)
#minha tentativa:
dados_despesas$ds_subalinea = ifelse(is.na(dados_receitas$ds_subalinea),ave(dados_receitas$ds_subalinea, FUN = function(x) mean(x, na.rm = 'TRUE')),dados_receitas$ds_subalinea)
# minha tentativa:
dataset$salary = ifelse(is.na(dataset$salary), ave(dataset$salary, FUN = function(x) mean(x, na.rm = 'TRUE')), dataset$salary)


#Os blocos de código acima verificam os valores ausentes nas colunas de 
#idade e salário e atualizam as células ausentes com a média da coluna.

#dataset $ column_header: seleciona a coluna no conjunto de dados 
#especificado após $ (idade e salário).
#is.na (conjunto de dados $ column_header): esse método retorna true 
#para todas as células na coluna especificada sem valores.
#ave (conjunto de dados $ column_header, FUN = function (x) mean
# (x, na.rm = 'TRUE')): esse método calcula a média da coluna
#passada como argumento


dataset$age = as.numeric(format(round(dataset$age, 0)))

#Como não estamos interessados em ter casas decimais para a idade, 
#arredondaremos para cima usando o código acima. O argumento 0 na função
#round significa que não há casas decimais.

#Depois de executar o bloco de código acima, o conjunto de dados
#será semelhante ao mostrado abaixo:

#####Como lidar com dados categóricos######


#Variáveis categóricas representam tipos de dados que podem ser 
#divididos em grupos. Exemplos de variáveis categóricas são: raça,
#sexo, faixa etária, nível educacional etc.

#Em nosso conjunto de dados, temos dois recursos categóricos,
#nação e item_comprado. Em R, podemos usar o método fator para 
#converter textos em códigos numéricos.


dataset$nation = factor(dataset$nation, levels = c('India','Germany','Russia'), labels = c(1,2,3))

dataset$purchased_item = factor(dataset$purchased_item, levels = c('No','Yes'),  labels = c(0,1))


#fator (conjunto de dados $ olumn_header, levels = c (), labels = c ()): o método fator converte os recursos categóricos na coluna especificada em fatores ou códigos numéricos.
#níveis: as categorias na coluna passaram como um vetor. Exemplo c ('Índia', 'Alemanha', 'Rússia')
#etiquetas: os códigos numéricos para as categorias especificadas na mesma ordem. Exemplo c (1,2,3))

#####Divisão do conjunto de dados em conjuntos de treinamento e teste####

install.packages('caTools') #install once
library(caTools) # importing caTools library
set.seed(123)
split = sample.split(dataset$purchased_item, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#set.seed (): A função semente preserva a exclusividade da divisão, ou 
#seja, para cada valor de semente, a divisão será única. É semelhante ao 
#argumento random_state em python.
#sample.split (conjunto de dados $ dependente_fator, SplitRatio = 0.8):
#esse método retornará valores booleanos com o comprimento do conjunto
#de dados original no SplitRatio .0.8 especificado, fornecendo 80 
#porcentagens e 20 porcentagens de falsas. Por exemplo, o bloco de 
#código acima atribuirá a divisão da variável com os valores [VERDADEIRO 
#VERDADEIRO VERDADEIRO VERDADEIRO VERDADEIRO VERDADEIRO VERDADEIRO 
#VERDADEIRO VERDADEIRO VERDADEIRO VERDADEIRO]
#subconjunto (conjunto de dados, divisão == TRUE): esse método retornará
#um subconjunto do conjunto de dados passado como um argumento em que a
#divisão é True. (80% do conjunto de dados original em relação ao código 
# fornecido)
#subconjunto (conjunto de dados, divisão == FALSE): esse método
#retornará um subconjunto do conjunto de dados passado como um 
#argumento em que a divisão é False. (20% do conjunto de dados
#original em relação ao código fornecido)

######Dimensionando os recursos

training_set[,3:4] = scale(training_set[,3:4])
test_set[,3:4] = scale(test_set[,3:4])

#O método de escala em R pode ser usado para dimensionar os recursos
#no conjunto de dados. Aqui estamos escalando apenas os não fatores
#que são a idade e o salário.

https://analyticsindiamag.com/data-preprocessing-with-r-hands-on-tutorial/
  
  
  ffx <- read.csv.ffdf(file="Projeto_Credito_Amostra50k_OnlyIDs_VIVO_v2.csv",
                       nrows=1000,next.rows = 100, VERBOSE=TRUE)



va<-sample(500)
df <- ffx[va[1:500],]


ffx <- read.csv.ffdf(file=csvfile, header=TRUE)


