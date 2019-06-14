library(tidytext)
library(RColetum)
library(tidyverse)
library(tm)
library(wordcloud)

## Importando o formulário
token <- "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"
form <- RColetum::GetForms(token)

## Busca o ID do pré-cadastro
id <- form$id[as.numeric(grep("01/2018", form$name))]

## Obtém as respostas
resp <- RColetum::GetAnswers(token = token, id)

## Seleciona colunas de interesse
da <- resp$deixeSeuComentarioParaACoordenacaoEProfessoresDoDsbd69427

## Remove números dos nomes de colunas
names(da) <- gsub("[0-9]", "", names(da))
## Quebra strings nas letras maiúsculas
names(da) <- gsub("([[:upper:]])", " \\1", names(da))
## Arruma case
names(da) <- str_to_title(names(da))

tb <- da %>%
    map(tibble) %>%
    flatten_df()

cps <- VCorpus(VectorSource(x = tb),
               readerControl = list(language = "pt",
                                    load = TRUE))

cps <-tm_map(cps, FUN =content_transformer(tolower))
cps <-tm_map(cps, FUN = removePunctuation)
cps <-tm_map(cps, FUN = removeNumbers)
cps <-tm_map(cps, FUN = stripWhitespace)
cps <-tm_map(cps, FUN = removeWords, words =stopwords("portuguese"))
cps2 <-tm_map(cps, FUN = stemDocument, language = "portuguese")
acen <-function(x)iconv(x, to = "ASCII//TRANSLIT")
cps2 <-tm_map(cps2, FUN =content_transformer(acen))
dtm <-DocumentTermMatrix(cps2)
tdm <-TermDocumentMatrix(cps2)
m <-as.matrix(tdm)
m <- m[order(apply(m, MARGIN = 1, sum), decreasing = TRUE), ]
m <-t(m)
mft <-findFreqTerms(tdm, lowfreq = 5)
trm <-as.matrix(dtm[,c("acompanhar", "dificuldad", "turma")])
ass <-findAssocs(dtm, terms = "acompanhar", corlimit = 0.5)
u <-sort(slam::col_sums(dtm>0), decreasing = TRUE)
tsp <- 1-u/nDocs(dtm)
sps <- 1- cumsum(u)/(nDocs(dtm)*seq_along(u))
rst <-removeSparseTerms(dtm, sparse = 0.95)
d <-data.frame(word =names(u),
               freq = u)

wordcloud(words = d$word,
          freq = d$freq,
          min.freq = 1,
          max.words = 30,
          random.order = FALSE,
          colors =brewer.pal(8, "Dark2"))
