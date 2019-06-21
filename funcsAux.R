library(RColetum)
library(tidyverse)
library(ggwordcloud)
library(RColorBrewer)
library(tm)
library(tidytext)

limpa_dados <- function(){
  myAnswers <- read.csv("/home/dsbd/Downloads/01.2018/01.2018.csv", sep = ";", stringsAsFactors = F)
  aux <- ncol(myAnswers) - 6
  da <- myAnswers[,2:aux]
  da2 <- myAnswers$Deixe.seu.comentário.para.a.coordenação.e.professores.do.DSBD.
  names(da) <- gsub("[....]", "A", names(da))
  names(da) <- str_replace_all(names(da), "AAAA", ".")
  names(da) <- str_replace_all(names(da), "A", " ")
  names(da) <- str_to_title(names(da))
  n <- nrow(myAnswers)
  return(list(da, da2, n))
}

organiza_dados <- function(x){
  db <- as_tibble(x) %>% 
    gather()
  db <- db %>%
    separate(key, c("Pergunta", "Disciplina"), sep = "\\.")
  db <- db %>%
    mutate(Disciplina = str_to_title(Disciplina))
  return(db)
}

limpa_dados2 <- function(){
  myAnswers1 <- read.csv("/home/dsbd/Downloads/01.2018/01.2018.csv", 
                         sep = ";", stringsAsFactors = F)
  myAnswers2 <- read.csv("/home/dsbd/Downloads/02.2018/02.2018.csv", 
                         sep = ";", stringsAsFactors = F)
  da1 <- myAnswers1[, 2:97]
  da2 <- myAnswers2[, 2:97]
  aux1 <- myAnswers1$Deixe.seu.comentário.para.a.coordenação.e.professores.do.DSBD.
  aux2 <- myAnswers2$Deixe.seu.comentário.para.a.coordenação.e.professores.do.DSBD.
  texto <- list(aux1,aux2)
  texto <- unlist(texto)
  temp <- data.frame(matrix(NA, ncol = 96, nrow = 6))
  names(temp) <- names(da2)
  da2 <- rbind(da2, temp)
  da <- cbind(da1, da2)
  geral1 <- myAnswers1[, 98:115]
  geral2 <- myAnswers2[,98:115]
  names(geral1) <- names(geral2)
  geral <- rbind(geral1, geral2)
  names(da) <- gsub("[....]", "A", names(da))
  names(geral) <- gsub("[....]", "A", names(geral))
  names(da) <- str_replace_all(names(da), "AAAA", ".")
  names(geral) <- str_replace_all(names(geral), "AAAA", ".")
  names(da) <- str_replace_all(names(da), "A", " ")
  names(geral) <- str_replace_all(names(geral), "A", " ")
  names(da) <- str_to_title(names(da))
  names(geral) <- str_to_title(names(geral))
  geral <- as.tibble(geral)
  db <- da %>% gather()
  db2 <- geral %>% gather()
  db <- db %>%
    separate(key, c("Pergunta", "Disciplina"), sep = "\\.")
  db <- db %>%
    mutate(Disciplina = str_to_title(Disciplina))
  dc <- db %>%
    group_by(Pergunta, Disciplina) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Pergunta, db$Pergunta))
  round(dc[which(dc$Disciplina == unique(dc$Disciplina)[6]),]$media, 2)
  dc2 <- db2 %>% 
    group_by(key) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(key, db2$key))
  n <- nrow(myAnswers1)
  n2 <- nrow(myAnswers2)
  return(list(db, db2, dc, dc2, da, texto, n, n2))
}

analise2 <- function(){
  temp <- limpa_dados2()
  db <- temp[[1]]
  db2 <- temp[[2]]
  dc <- temp[[3]]
  dc2 <- temp[[4]]
  da <- temp[[5]]
  geral <- temp[[6]]
  grupos <- cria_grupos2(db = db)
  gera_graficos3(da = da, db = db, 
                 db2 = db2, dc = dc, dc2 = dc2, grupos = grupos)
  nuvem(geral)
  return(c(temp[[7]], temp[[8]]))
}

analise <- function(){
  temp <- limpa_dados()
  da <- temp[[1]]
  da2 <- temp[[2]]
  db <- organiza_dados(da)
  dc <- organiza_dados2(db)
  grupos <- cria_grupos(db)
  gera_graficos(grupos = grupos, db = db, dc = dc)
  gera_graficos2(da = da, db = db, grupos = grupos)
  nuvem(da2 = da2)
  return(temp[[3]])
}

