# Autor: Thales Henrique Barros Cardoso

# Problema de negócio: Tentar predizer se um usuário fará download de um app depois de clicar em um anuncio de
# aplicativo móvel.

# Objetivo: Tratar e explorar os dados disponíveis no dataset sugerido, criar um modelo de machine learning que
# possa obter o resultado prooposto pelo problema de negócio.

# O dataset está disponível em https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

# Escolha do diretório
setwd("C:/CursosDSA/FCD/BigDataRAzure/Cap20/Projeto01")
getwd()

# Visualização do formato das variáveis
str(sampleTrain)

#Visualização das primeiras observações
head(sampleTrain)

# Verificando presença de valores NA por coluna.
colSums(is.na(sampleTrain))

## A variável attributed_time apresenta uma grande quatidade de valores NA.
## Essa variável representava o horário no qual o usuário baixou o app, como poucos
## concretizam o download há uma quantidade enorme de valores NA.

# Contando o número de valores únicos em cada coluna
numUnique <- function(x){
  return(length(unique(x)))
}

apply(sampleTrain, 2, numUnique)

# Verificando a quantidade de linhas duplicadas e a porentagem em relação ao total do dataset.
anyDuplicated(sampleTrain)

100 * (anyDuplicated(sampleTrain) / nrow(sampleTrain))

## Como a porcentagem de valores duplicados são uma pequena parcela do dataset vamos excluí-los.
sampleTrain <- sampleTrain[!duplicated(sampleTrain)]

anyDuplicated(sampleTrain)


