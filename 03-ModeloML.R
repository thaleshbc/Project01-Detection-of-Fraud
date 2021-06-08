# Autor: Thales Henrique Barros Cardoso

# Problema de negócio: Tentar predizer se um usuário fará download de um app depois de clicar em um anuncio de
# aplicativo móvel.

# Objetivo: Tratar e explorar os dados disponíveis no dataset sugerido, criar um modelo de machine learning que
# possa obter o resultado prooposto pelo problema de negócio.

# O dataset está disponível em https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data


# Dividindo o dataset (amostra aleatória que representa 90 avos do dataset que já foi dividido inicialmente)
trainingLines <- sampleTrain[sample(1:nrow(sampleTrain), round(nrow(sampleTrain)/70)),-c(6:7, 11:12)]
sampleLines <- sample(1:nrow(trainingLines), 0.8 * nrow(trainingLines))

training <- trainingLines[sampleLines,]
prop.table(table(training$is_attributed))

testing <-trainingLines[-sampleLines,]
prop.table(table(testing$is_attributed))

# Balanceando a variável alvo para treinar o modelo
trainingRose <- ROSE(is_attributed ~ ., data= training, seed = 1)$data
prop.table(table(trainingRose$is_attributed))

# Verificando graficamente se o metodo de balanceamento foi concluído
ggplot(data = trainingRose, aes(x = is_attributed)) +
  geom_bar() + 
  ggtitle("Balanceammento da variável alvo")

## Tranformando novamente algumas variaveis de factor para integer, para poder utilizar o randomForest no feature selection
trainingRose$ip <- as.integer(trainingRose$ip) 
trainingRose$app <- as.integer(trainingRose$app)  
trainingRose$device <- as.integer(trainingRose$device) 
trainingRose$os <- as.integer(trainingRose$os) 
trainingRose$channel <- as.integer(trainingRose$channel) 

modelImportance <- randomForest(is_attributed ~ ., data = trainingRose, ntree = 40, nodesize = 2, importance = T)
varImpPlot(modelImportance)

## Com a importância das variaveis vamos criar um primeiro modelo de machine learning com todas as variaveis e observar
## a acurácia do modelo

## Ajustando as variaveis de teste para integer, para realizar a predição no modeloo
testing$ip <- as.integer(testing$ip) 
testing$app <- as.integer(testing$app)  
testing$device <- as.integer(testing$device) 
testing$os <- as.integer(testing$os) 
testing$channel <- as.integer(testing$channel)

# Criando o modelo de machine learning
model <- randomForest(is_attributed ~ ., data = trainingRose, ntree = 500, nodesize = 100)
prediction <- predict(model, testing, type = 'class')

# Visualizando a matriz de confusão
confusionMatrix(table(dados = testing$is_attributed,
                      pred = prediction), positive = '1')

#Plot da Curva ROC
roc.curve(testing$is_attributed, prediction, plotit = T, col = "red")

## Com esse primeiro modelo temos uma acurácia de 0.9969, vamos alterar os algoritmos para observar se o random foreste
## é realmente o melhor deles para a criação do modelo com esses dadps de treino.



