library(RColetum)
library(tidyverse)
## Importando o formulário
token <- "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"
form <- RColetum::GetForms(token)
form

## Busca o ID do pré-cadastro
id <- form$id[as.numeric(grep("Pré", form$name))]

## Obtém as respostas
resp <- RColetum::GetAnswers(token = token, id)
str(resp)
str(resp[[1]])
str(resp[[2]])

## Adicona nome às colunas
names(resp)
names(resp) <- c("a","b")

## Seleciona colunas de interesse
da <- resp[[1]][,2:11]
str(da)
## Remove números dos nomes de colunas
names(da) <- gsub("[0-9]", "", names(da))
## Quebra strings nas letras maiúsculas
names(da) <- gsub("([[:upper:]])", " \\1", names(da))
## Arruma case
names(da) <- str_to_title(names(da))
## Casos não comtemplados
names(da)[4] <- "Cidade-UF  De  Residencia"
names(da)[8] <- "Atuacao/posicao/cargo"
names(da)[9] <- "Empresa/instituicao  Em  Que  Trabalha"  

## Corrige a formação acadêmica
#setwd("/home/dsbd/Desktop/est/DSBD/precadastro/")
#saveRDS(db, file = "my_data.rds")
#db <- readRDS(file = "my_data.rds")
#da[5] <- db

## Converte para tibble
da <- as.tibble(da)
dim(da);class(da)

## Encontrando os cursos de maiores frequência
da.tb <- table(da$`Formacao Academica`)
da.tb <- sort(da.tb,decreasing = T)

## Encontra faixa etária de maior frequência
db.tb <- table(da$`Faixa Etaria`)
db.tb <- sort(db.tb,decreasing = T)

## Tempo de trabalho com dsbd
dc <- da$`Tempo Em Anos Em Que Trabalha Com Big Data Ou Data Science`
dc.tb <- sort(table(dc),decreasing =T)

  #dd <- readRDS(file = "si.rds")
  #da$`Atuacao/posicao/cargo`
  #saveRDS(dd,file="si.rds")
  #da$`Atuacao/posicao/cargo` <- dd

## Cargo
dd.tb <- table(dd)
dd.tb <- sort(dd.tb,decreasing = T)

## Experiencia
da <- da %>% mutate(
  Experiencia = 2019 - da$`Ano De Obtencao Do Titulo De Graduacao`
)
  ## Verifica erros
da$Experiencia[which(da$Experiencia < 0)] <- NA
da$Experiencia[which(da$Experiencia > 100)] <- NA

## Gráficos

##Feito melhor
#ggplot()+
  #geom_bar(aes(x=names(da.tb)[1:5],y=as.numeric(da.tb)[1:5]),stat="identity")

ggplot()+
  geom_bar(aes(x=names(db.tb)[1:5],y=as.numeric(db.tb)[1:5]),stat="identity")

ggplot()+
  geom_bar(aes(x=names(dc.tb)[1:5],y=as.numeric(dc.tb)[1:5]),stat="identity")

ggplot()+
  geom_bar(aes(x=names(dd.tb)[1:5],y=as.numeric(dd.tb)[1:5]),stat="identity")

ggplot(data=da, aes(da$Experiencia)) + 
  geom_histogram(breaks=seq(0, 42, by = 3), 
                 col="red", 
                 aes(fill=..count..))+
  geom_density()




######################## -------- ###################################
fac <- factor(x = da$`Faixa Etaria`, 
              levels = c("< 25", "25 - 30", "31 - 35", "36 - 40", "> 40"))

faixa <- 0
k <- 1

for(j in 1:5){
  for(i in 1:5){
    faixa[k] <- list(
      da %>% filter(
        `Formacao Academica` == names(da.tb)[j] &
        `Faixa Etaria` == levels(fac)[i]
        )
    )
    k=k+1
  }
}


for(i in 1:25){
  faixa.tb[i] <- length(faixa[[i]]$`Formacao Academica`)
}

aux <- as.data.frame(faixa.tb)
aux

data <- matrix(c(as.numeric(aux[,1:5]),
                 as.numeric(aux[,6:10]),
                 as.numeric(aux[,11:15]),
                 as.numeric(aux[,16:20]),
                 as.numeric(aux[,21:25])),
               byrow = F,nrow=5)
rownames(data) <- levels(fac)
colnames(data) <- names(da.tb[1:5])
data

data2 <- data
data2 <- as_tibble(data2)
data2 <- gather(data2)
data2 <- cbind(data2,levels(fac))
data2

ggplot() + 
  geom_bar(data = data2,
           aes(x = key,
               y = value,
               fill=data2$`levels(fac)`),
           stat="identity")

##########################################3

ggplot() + 
  geom_bar(data = data2,
           aes(y = key,
               x = value,
               fill=a),
           stat="identity")
a <- rep(c("estatistica","TADS","inf","adm","econ"),5)
data2$key
db.tb
