# Autor: Thales Henrique Barros Cardoso

# Problema de negócio: Tentar predizer se um usuário fará download de um app depois de clicar em um anuncio de
# aplicativo móvel.

# Objetivo: Tratar e explorar os dados disponíveis no dataset sugerido, criar um modelo de machine learning que
# possa obter o resultado prooposto pelo problema de negócio.

# O dataset está disponível em https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

# Escolha do diretório
setwd("C:/CursosDSA/FCD/BigDataRAzure/Cap20/Projeto01")
getwd()

# Maniulação de dados
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(bigreadr))

# Gráficos
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))

# Manipulação de datas
suppressMessages(library(lubridate))

# Machine learning
suppressMessages(library(caret))
suppressMessages(library(ROSE))
suppressMessages(library(randomForest))
suppressMessages(library(rpart))
suppressMessages(library(ROCR))
suppressMessages(library(C50))
suppressMessages(library(e1071))

# Executando a limpeza do environment
rm(list=ls())

# Se não quiser realizar todo o procedimento de divisão e amostragem do arquivo, pular para a ultima linha desse arquivo.
# O arquivo de amostragem utilizao nesse projeto está em anexo nesse repositório. Se preferir utilizar o dataset completo 
# ou fazer sua própria amostragem, o link para o kaggle está no cabeçalho desse arquivo.

## O dataset é por damasiado grande para funcionar de forma adequada dentro das especificações do meu computador.
## Dessa forma foi necessário utilizar artifícios para ca criação de uma amostra menor de forma randômica.
## Para isso vamos utilizar a biblioteca bigreadr que segundo a descrição do pacote "ler arquivos de texto grandes
## dividindo-os em arquivos menores.

# Conseguimos ler a quantidade de linhas do dataset original sem precisar carregar o arquivo.
totalLines <- nlines("train.csv")

# Criar um conjunto com 1/10 das linhas do dataset original
setLines <- totalLines %/% 10

## Com o total de linhas do dataset original dividiremos em arquivos menores para depois fazermos uma
## amostragem randômica de um dataset com 1/10 do tammonho do original.

# Divisão dos dados em 10 conjuntos
split_file("train.csv", every_nlines = setLines, prefix_out = "train", repeat_header = T)

# Visualização dos arquivos no diretório
dir(pattern = ".txt")

## O arquivo train_11.txt surgiu pois o resultado inteiro da divisão do  número total de observações divido 
## por 10 gerou um módulo (resto de divisão) que gerou assim o arquivo train_11.txt
## Esse arquivo pode ser desconsiderado, pois tem poucas observações, portanto podemos deletar o arquivo

nlines("train_11.txt")

# Deletando o arquivo do diretório
file.remove("train_11.txt")

# Colocando o nome dos arquivos em um objeto
setFiles <- c(dir(pattern = ".txt"))

# Criando um data frame com amostras aleatórias dos 20s aquivos gerados
sampleTrain <- data.table()
for (i in setFiles){
  train <- fread(i)
  sampleTrain <- rbind(sampleTrain, train[sample(1:nrow(train), (setLines/10)),], use.names= FALSE)
}

# Salvando o arquivo com a amostragem finalizada
fwrite(sampleTrain, file = "sampleTrain.csv")

# Removendo alguns objetos do ambiente 
rm("i", "setFiles", "setLines", "totalLines", "train")

# Removendo os arquivos ".txt" temporários
file.remove(c(dir(pattern = ".txt")))

# Se não quiser fazer todo o procedimento de divisão e amostragem, realizar apenas a leitura desse arquivo.
# Lendo o arquivo salvo com a amostragem feita (Quando é preciso recomeçar o trabalho)
sampleTrain <- fread("sampleTrain.csv")

