# Autor: Thales Henrique Barros Cardoso

# Problema de negócio: Tentar predizer se um usuário fará download de um app depois de clicar em um anuncio de
# aplicativo móvel.

# Objetivo: Tratar e explorar os dados disponíveis no dataset sugerido, criar um modelo de machine learning que
# possa obter o resultado prooposto pelo problema de negócio.

# O dataset está disponível em https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

#Alterando o algoritmo para o C5.0
modelOptimized <- C5.0(is_attributed ~ ., data = trainingRose)
predictionOpitimized <- predict(modelOptimized, testing, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing$is_attributed,
                      pred = predictionOpitimized), positive = '1')

#Plot da Curva ROC
roc.curve(testing$is_attributed, predictionOpitimized, plotit = T, col = "red")


# Alterando o algoritmo para o rpart
modelOptimized_3 <- rpart(is_attributed ~ ., data = trainingRose)
predictionOpitimized_3 <- predict(modelOptimized_3, testing, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing$is_attributed,
                      pred = predictionOpitimized_3), positive = '1')

#Plot da Curva ROC
roc.curve(testingRose$is_attributed, predictionOpitimized_3, plotit = T, col = "red")

# Criando um modelo random forest com CROSS VALIDATION
modelOptimized_4 <- train(is_attributed ~ ., data = trainingRose, method = 'rf', 
                           trControl = trainControl(method = 'cv', p = 0.2))
predictionOpitimized_4 <- predict(modelOptimized_4, testing, type = 'raw')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing$is_attributed,
                     pred = predictionOpitimized_4), positive = '1')

#Plot da Curva ROC
roc.curve(testingRose$is_attributed, predictionOpitimized_4, plotit = T, col = "red")


## A partir daqui vamos tentar aperfeiçoar o algoritmo atuando nos dados d treino.

## Primeiro vamos criar uma data frame onde estejam apenas as 4 melhores variaveis de acordo com o random forest
trainingRose_2 <- trainingRose[, c(1,2,6,7,8)]
testing_2 <- testing[, c(1,2,6,7,8)]

# Criando o modelo de machine learning
modelNewData <- randomForest(is_attributed ~ ., data = trainingRose_2)
predictionNewData <- predict(modelNewData, testing_2, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing_2$is_attributed,
                      pred = predictionNewData), positive = '1')

#Plot da Curva ROC
roc.curve(testing_2$is_attributed, predictionNewData, plotit = T, col = "red")

## Como se trata de algoritmo de classificação, não há muito a possibilidade de melhora no resultado com a normalização
## dos dados. Suponhe-se que com a quantidade de dados retirados do dataset original possa prejudicar o treinamento e 
## colocar um teto no valor da acuracia do modelo.

## Retirando a variavel IP do data set
trainingRose_3 <- trainingRose[, c(2,5,6,7)]
testing_3 <- testing[, c(2,5,6,7)]

# Criando o modelo de machine learning
modelNewData <- randomForest(is_attributed ~ ., data = trainingRose_3)
predictionNewData <- predict(modelNewData, testing_3, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing_3$is_attributed,
                      pred = predictionNewData), positive = '1')

#Plot da Curva ROC
roc.curve(testingRose$is_attributed, prediction, plotit = T, col = "red")


## Novamente vamos criar um novo data frame onde vamos acrescentar outras variaveis
trainingRose_4 <- trainingRose[, c(1,2,3,4,5,6,7)]
testing_4 <- testing[, c(1,2,3,4,5,6,7)]

# Modificação da variável hora para integer
trainingRose_4$hora <- as.integer(trainingRose_4$hora)
testing_4$hora <- as.integer(testing_3$hora)

# Criação de novas variáveis
# Nesse caso temos variaveis que  representam a multiplicação de um dos atributos pela a hora do clique no anuncio
trainingRose_4$appHora <- trainingRose_4$app * trainingRose_4$hora 
trainingRose_4$deviceHora <- trainingRose_4$device * trainingRose_4$hora
trainingRose_4$osHora <- trainingRose_4$os * trainingRose_4$hora
trainingRose_4$channelHora <- trainingRose_4$channel * trainingRose_4$hora

testing_4$appHora <- testing_4$app * testing_4$hora 
testing_4$deviceHora <- testing_4$device * testing_4$hora
testing_4$osHora <- testing_4$os * testing_4$hora
testing_4$channelHora <- testing_4$channel * testing_4$hora

# Criando o modelo de machine learning
modelNewData_2 <- randomForest(is_attributed ~ ., data = trainingRose_4)
predictionNewData_2 <- predict(modelNewData_2, testing_4, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing_4$is_attributed,
                      pred = predictionNewData_2), positive = '1')

#Plot da Curva ROC
roc.curve(testingRose_3$is_attributed, predictionNewData_2, plotit = T, col = "red")


