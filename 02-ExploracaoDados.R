# Autor: Thales Henrique Barros Cardoso

# Problema de negócio: Tentar predizer se um usuário fará download de um app depois de clicar em um anuncio de
# aplicativo móvel.

# Objetivo: Tratar e explorar os dados disponíveis no dataset sugerido, criar um modelo de machine learning que
# possa obter o resultado prooposto pelo problema de negócio.

# O dataset está disponível em https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

## A coluna Attributed_time tem muitos valores NA, pois essa coluna segundo o dicionário de dados é referente
## ao horário do donwload do app após o usuário clicar em um anúncio.

# Porcentagem de valores NA na coluna attributed_time
100 * sum(is.na(sampleTrain$attributed_time)) / nrow(sampleTrain)

## Observa-se que quase nenhum click realizado nos anúncios concretizam no download do app

# Transformação das variáveis inteiras em categóricas (de forma manual, pois a restrições do computdor não permite 
# fazer de forma automatizada)

sampleTrain$ip <- factor(sampleTrain$ip) 
sampleTrain$app <- factor(sampleTrain$app)  
sampleTrain$device <- factor(sampleTrain$device) 
sampleTrain$os <- factor(sampleTrain$os) 
sampleTrain$channel <- factor(sampleTrain$channel) 
sampleTrain$is_attributed <- factor(sampleTrain$is_attributed) 

# Verificando o status dos atributos novamente
str(sampleTrain)

# Inicio da análise exploratória
uniqueValues <- data.frame(colnames(sampleTrain), sapply(sampleTrain, numUnique))
colnames(uniqueValues) <- c("categorical", "value")
uniqueValues <- uniqueValues[-c(6:8), ]

# Vizualização da quantidade de valores únicos
ggplot(data = uniqueValues, aes(x = categorical, y = value)) +
  geom_bar(stat="identity") +
  xlab("Categorias") +
  ylab("Quantiddade") +
  ggtitle("Quantidade de valores únicos por categoria")

## É notório  uma quantidade com muitos valores unicos para a categoria IP. Pelo seu prórpio significado de 
## internet protocol, indica que muitos aparelhos diferentes acessam os anúncios. É natural que sejam uma
## quantidade maior em relação aos outros campos como device ou os (sistema operacional), te-los na mesma
## proporção seria impossível. Esse primeiro gráfico mostra coerência com o mundo da informática.

# Criação de variáveis relacionadas a data e horários
sampleTrain$hora <- factor(hour(sampleTrain$click_time))
sampleTrain$dayofweek <- factor(weekdays(sampleTrain$click_time))

# Visualização de cliques nos anúncios por hora em um dia
graph1 <- ggplot(data = sampleTrain, aes(x = hora)) +
  geom_bar() + 
  ggtitle("Cliques por hora do dia") +
  ylab("Nº de cliques")

# Visualização de cliques por dia da semana
graph2 <- ggplot(data = sampleTrain, aes(x = dayofweek)) +
  geom_bar() +
  ggtitle("Cliques por dia da semana") +
  ylab("Nº de cliques")

## No gráfico de cliques por hora do dia podemos observar que há uma diminuição de cliques por volta das 16 horas
## até as 23 horas, onde se retoma a atividade maior de cliques nos anuncios. 
## Dentro da nossa amostragem verifica-se que durante alguns dias da semana não ha registro de cliques, ente eles
## sexta-feira, sábado e domingo. O que seria algo bastante estranho já que no mundo conectado de hoje as pessoas
## não deixam de usar seus telefones no fim de semana. Seria um problema da amostragem?

# Visualização do horário e data dos downloads do aplicativo
sampleTrain$hour_of_download <- factor(hour(sampleTrain$attributed_time))
sampleTrain$day_of_download <- factor(weekdays(sampleTrain$attributed_time))

# Variável temporária para geração do gráfico
temp <- data.table()
temp$hour_of_dowload <- sampleTrain$hour_of_download
temp$day_of_download <- sampleTrain$day_of_download

temp <- na.omit(temp)

# Visualização dos downloads por hora do dia
graph3 <- ggplot(data = temp, aes(x = hour_of_dowload)) +
  geom_bar() +
  ggtitle("dowloads por hora do dia") +
  ylab("Nº de downloads") + 
  xlab("hora")

# Visualização de dowloads por dia da semana
graph4 <- ggplot(data = temp, aes(x = day_of_download)) +
  geom_bar() +
  ggtitle("dowloads por dia da semana") +
  ylab("Nº de downloads")

## Com esses gráficos conseguimos observar que o comportamento dos downloads feitos possuem a mesma caracteristicas 
## de distribuição, considerando as devidas proporções, dos cliques dados nos anúnicos. Para conseguir visualizar melhor
## Vamos plotar os gráficos juntos.

# Visualização dos gráficos em grade.
grid.arrange(graph1, graph3)

grid.arrange(graph2, graph4)

# Vizualização distribuição da variável target
prop.table(table(sampleTrain$is_attributed)) * 100

ggplot(data = sampleTrain, aes(x = is_attributed)) +
  geom_bar() +
  ggtitle("Quantidade de vezes que o app foi baixado") +
  ylab("Quantidade")

## Verifica-se que o dataset é extremamante desbalanceado na variável alvo.  Isso pode  criar uma dificuldade
## na criação do modelo de machine learning, deixando-o com uma generalização menor. Posteriormente faremos
## o balanceamento da variável alvo.

# Verificação dos top 10 de cada categoria que mais clicaram nos anuncios

# Os 10 ip's que mais clicaram em anuncios
g1 <- sampleTrain %>% 
  select(ip) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("IP") +
  ylab("Frequência") +
  ggtitle("Os 10 Ip's que mais clicam em anúncios")

# Os 10 app's que mais clicaram em anuncios
g2 <- sampleTrain %>% 
  select(app) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("App") +
  ylab("Frequência") +
  ggtitle("Os 10 App'sque mais clicam em anúncios")

# Os 10 devices que mais clicaram em anuncios
g3 <- sampleTrain %>% 
  select(device) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Device") +
  ylab("Frequência") +
  ggtitle("Os 10 Devices que mais clicam em anúncios")

# Os 10 os' que mais clicaram em anuncios
g4 <- sampleTrain %>% 
  select(os) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Os") +
  ylab("Frequência") +
  ggtitle("Os 10 Os' que mais clicam em anúncios")

# Os 10 channels que mais clicaram em anuncios
g5 <- sampleTrain %>% 
  select(channel) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Channels") +
  ylab("Frequência") +
  ggtitle("Os 10 Channels que mais clicam em anúncios")

# Os 10 Ip's que mais fazem o download do aplicativo
g6 <- sampleTrain %>% 
  filter(is_attributed == 1) %>%
  select(ip) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("IP") +
  ylab("Frequência") +
  ggtitle("Os 10 Ip's que mais fizeram downloads")

# Os 10 App's que mais fazem o download do aplicativo
g7 <- sampleTrain %>% 
  filter(is_attributed == 1) %>%
  select(app) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("App's") +
  ylab("Frequência") +
  ggtitle("Os 10 App's que mais fizeram downloads")

# Os 10 Devices que mais fazem o download do aplicativo
g8 <- sampleTrain %>% 
  filter(is_attributed == 1) %>%
  select(device) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Devices") +
  ylab("Frequência") +
  ggtitle("Os 10 Devices que mais fizeram downloads")

# Os 10 Os's que mais fazem o download do aplicativo
g9 <- sampleTrain %>% 
  filter(is_attributed == 1) %>%
  select(os) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Os'") +
  ylab("Frequência") +
  ggtitle("Os 10 Os' que mais fizeram downloads")

# Os 10 Channels que mais fazem o download do aplicativo
g10 <- sampleTrain %>% 
  filter(is_attributed == 1) %>%
  select(channel) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Channels") +
  ylab("Frequência") +
  ggtitle("Os 10 Channels que mais fizeram downloads")

# Os 10 Ip's que menos fazem o download do aplicativo
g11 <- sampleTrain %>% 
  filter(is_attributed == 0) %>%
  select(ip) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("IP") +
  ylab("Frequência") +
  ggtitle("Os 10 Ip's que menos fizeram downloads")

# Os 10 App's que menos fazem o download do aplicativo
g12 <- sampleTrain %>% 
  filter(is_attributed == 0) %>%
  select(app) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("App's") +
  ylab("Frequência") +
  ggtitle("Os 10 App's que menos fizeram downloads")

# Os 10 Devices que menos fazem o download do aplicativo
g13 <- sampleTrain %>% 
  filter(is_attributed == 0) %>%
  select(device) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Devices") +
  ylab("Frequência") +
  ggtitle("Os 10 Devices que menos fizeram downloads")

# Os 10 Os's que menos fazem o download do aplicativo
g14 <- sampleTrain %>% 
  filter(is_attributed == 0) %>%
  select(os) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Os'") +
  ylab("Frequência") +
  ggtitle("Os 10 Os's que menos fizeram downloads")

# Os 10 Channels que menos fazem o download do aplicativo
g15 <- sampleTrain %>% 
  filter(is_attributed == 0) %>%
  select(channel) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(n = 10) %>%
  data.frame() %>%
  ggplot(aes(x = ., y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Channels") +
  ylab("Frequência") +
  ggtitle("Os 10 Channels que menos fizeram downloads")

grid.arrange(g1, g6, g11)
grid.arrange(g2, g7, g12)
grid.arrange(g3, g8, g13)
grid.arrange(g4, g9, g14)
grid.arrange(g5, g10, g15)

# Removendo Variáveis do ambiente
rm("graph1", "graph2", "graph3", "graph4")
rm("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8", "g9", "g10", "g11", "g12", "g13", "g14", "g15")

## Observa-se que os Ip's 53454, 114276, 26995, 95766, 17149 e 105475 são ip's que clicam bastante nos anuncios
## porém não realizam downloads na mesma proporção,o que pode indicar fraude. Na verdade os 10 primeiros que mais 
## clicam em anuncios são os mesmos 10 primeiros que menos fazem downloads. Algo interessante a se destacar é que 
## os 4 primeiros  que mais clicam são os mesmos 4 primeiros que mais fazem downloads, o que faz sentido.

## Em relação aos app's podemos observar que quase nenhum dos 10 que mais clicam nos anúncios estão dentro da lista
## dos 10 que mais fazem download, com exceção do app 9 e 3, e que ainda assim não é na mesma proporção dos cliques.
## O app 19 por exemplo,não aparece na lista dos 10 que mais clicam, mas é o app que mais faz download. É necessário
## por parte da empresa observar e tirar os anuncios de alguns app's.

## No atributo device o que chama mais atenção éo de número 1, que é o que mais clica em anuncios, é o que mais baixa
## e o que menos baixa, o que seria um comportamento normal. o de número 0 é o terceiro que mais acessa anuncios e
## o segundoque mais baixa, sendo um bom indicador de que não há fraude nos cliques. Os outros devices que fazem mais
## downloads nãoaparecem na lista dos que  mais clicam nos anuncios.  Provavelmente o device 0, 1 e 2 são aparelhos
## mais populares,  o que não quer dizer que são mais baratos.

## Os Os' com mais cliques provavelmente são os sistemas operacionais mais populares no mercado. Obsrva-se que os que
## mais clicam  são os que menos fazem downloads, porem o 19 e o 13 possuem uma boa taxa de conversão de download.
## O os' 24 tem uma boa taxa de download mesmo sem esta no topo dos que mais clicam em anuncios. O Os' 17 e 18 tammbém
## aparecem na lista dos 10 que mais fazem downloads mas não na mesma proporção dos cliques.

## Em relação aos channel temos que nenhum dos que mais clicam são os que mais fazem downloads. Isso mostra que há a
## possibilidade de ter bots clicando nos anuncios por esses meios.

## De forma geral se vê que os que mais clicam sempre são os que menos fazem downloads. Por vezes, alguns desses que 
## mais clicam são os que mais fazem download, porém numa proporção muito pequena. 