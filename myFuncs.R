library(RColetum)
library(tidyverse)
library(ggwordcloud)
library(RColorBrewer)
library(tm)
library(tidytext)

limpa_dados <- function(nome,
                        token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  myForms <- RColetum::GetForms(token = token)
  id <- as.numeric(myForms$id[grep(nome,myForms$name)])
  myAnswers <- RColetum::GetAnswers(token,id)
  aux <- ncol(myAnswers) - 11
  da <- myAnswers[,2:aux]
  da2 <- myAnswers$Deixe.seu.comentário.para.a.coordenação.e.professores.do.DSBD.
  names(da) <- gsub("[0-9]", "", names(da))
  names(da) <- gsub("([[:upper:]])", " \\1", names(da))
  names(da) <- str_to_title(names(da))
  n <- nrow(myAnswers)
  return(list(da, da2, n))
}

limpa_dados2 <- function(nome, 
                         token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  myForms <- GetForms(token)
  id <- as.numeric(myForms$id[grep(nome, myForms$name)])
  myAnswers1 <- GetAnswers(token, id[1])
  myAnswers2 <- GetAnswers(token, id[2])
  aux1 <- myAnswers1$deixeSeuComentarioParaACoordenacaoEProfessoresDoDsbd69427
  aux2 <- myAnswers2$deixeSeuComentarioParaACoordenacaoEProfessoresDoDsbd69427
  texto <- list(aux1,aux2)
  texto <- unlist(texto)
  da1 <- myAnswers1[, 2:97]
  da2 <- myAnswers2[, 2:97]
  temp <- data.frame(matrix(NA, ncol = 96, nrow = 6))
  names(temp) <- names(da2)
  da2 <- rbind(da2, temp)
  da <- cbind(da1, da2)
  geral1 <- myAnswers1[, 98:115]
  geral2 <- myAnswers2[,98:115]
  names(geral1) <- names(geral2)
  geral <- rbind(geral1, geral2)
  names(da) <- gsub("[0-9]", "", names(da))
  names(geral) <- gsub("[0-9]", "", names(geral))
  names(da) <- gsub("([[:upper:]])", " \\1", names(da))
  names(geral) <- gsub("([[:upper:]])", " \\1", names(geral))
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

organiza_dados <- function(x){
  x <- as_tibble(x)
  db <- x %>% gather()
  db <- db %>%
    separate(key, c("Pergunta", "Disciplina"), sep = "\\.")
  db <- db %>%
    mutate(Disciplina = str_to_title(Disciplina))
  return(db)
}

organiza_dados2 <- function(db){
  dc <- db %>%
    group_by(Pergunta, Disciplina) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Pergunta, db$Pergunta))
  return(dc)
}

cria_grupos <- function(db){
  perguntas <- unique(db$Pergunta)
  grupos <- matrix(c("1:7", "Qualidade do corpo e práticas docentes", "disc",
                     "8:13", "Interesse dos alunos", "disc",
                     "14:21", "Interação professor-aluno", "disc",
                     "22:26", "Pertinência do conteúdo do curso", "disc",
                     "27:32", "Qualidade e organização do curso", "disc",
                     "33:41", "Avaliação geral do curso DSBD", "geral",
                     "42:50", "Infraestrutura dos ambientes pedagógicos",
                     "geral"), ncol = 3, byrow = TRUE,
                   dimnames = list(NULL, c("idx", "nome", "tipo")))
  return(grupos)
}

gera_graficos <- function(grupos,db,dc){
  perguntas <- unique(db$Pergunta)
  for(i in 1:nrow(grupos)){
    idx <- eval(parse(text = grupos[i, "idx"]))
    grupo <- perguntas[idx]
    if(grupos[i, "tipo"] == "disc"){
      ggplot(data = subset(db, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = value, y = ..prop..)) +
        labs(title = grupos[i, "nome"], x = "", y = "") +
        ylim(0, 0.8) +
        facet_grid(Pergunta ~ Disciplina,
                   labeller = label_wrap_gen()) +
        coord_flip()
      ggsave(paste0("geral_2", i, ".png"),
             width = 270, height = 325, units = "mm")
      ggplot(data = subset(dc, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = Disciplina, y = media),
                 stat = "identity") +
        labs(title = grupos[i, "nome"], x = "", y = "Média") +
        scale_x_discrete(labels = c("ML", "ME", "PB")) +
        ylim(0, 5) +
        facet_grid(~ Pergunta, labeller = label_wrap_gen())
      ggsave(paste0("media_2", i, ".png"),
             width = 350, height = 150, units = "mm")
    } else{
      ggplot(data = subset(db, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = value, y = ..prop..)) +
        labs(title = grupos[i, "nome"], x = "", y = "") +
        ylim(0, 0.8) +
        #facet_grid(~ Pergunta) +
        facet_wrap(~ Pergunta, labeller = label_wrap_gen()) +
        coord_flip()
      ggsave(paste0("geral_2", i, ".png"),
             width = 270, height = 325, units = "mm")
      ggplot(data = subset(dc, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = reorder(Pergunta, media),
                               y = media), stat = "identity") +
        labs(title = grupos[i, "nome"], x = "", y = "Média") +
        scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
        coord_flip() +
        ylim(0, 5)
      ggsave(paste0("media_2", i, ".png"),
             width = 270, height = 325, units = "mm")
    }
  }
  ggplot(data = subset(dc, Pergunta %in% grupo)) +
    geom_bar(mapping = aes(x = reorder(Pergunta, media),
                           y = media), stat = "identity") +
    labs(title = grupos[i, "nome"], x = "", y = "Média") +
    scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
    coord_flip() +
    ylim(0, 5)
}
gera_graficos2 <- function(da,db,grupos){
  nresp <- nrow(da)
  grupos.vec <- character(0)
  for(i in 1:nrow(grupos)){
    idx <- eval(parse(text = grupos[i, "idx"]))
    n <- length(idx)
    if(grupos[i, "tipo"] == "disc"){
      tmp <- unname(rep(grupos[i, "nome"], n*nresp*3))
    } else{
      tmp <- unname(rep(grupos[i, "nome"], n*nresp))
    }
    grupos.vec <- c(grupos.vec, tmp)
  }
  db$Grupos <- grupos.vec
  dmed <- db %>%
    group_by(Grupos) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Grupos, db$Grupos))
  dmed$Grupos <- factor(dmed$Grupos, levels = dmed$Grupos)
  ggplot(data = dmed) +
    geom_bar(mapping = aes(x = Grupos, y = media),
             stat = "identity") +
    labs(title = "Média geral", x = "", y = "Média") +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5)
  ggsave(paste0("media_geral_todos2.png"),
         width = 250, height = 150, units = "mm")
  ggplot(data = dmed[1:5, ]) +
    geom_bar(mapping = aes(x = Grupos, y = media),
             stat = "identity") +
    labs(title = "Média geral", x = "", y = "Média") +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5)
  ggsave(paste0("media_geral_5principais2.png"),
         width = 250, height = 150, units = "mm")
  dmed2 <- db %>%
    group_by(Grupos, Disciplina) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Grupos, db$Grupos))
  dmed2$Grupos <- factor(dmed2$Grupos, levels = unique(dmed2$Grupos))
  dmed2 <- dmed2[complete.cases(dmed2), ]
  ggplot(data = dmed2) +
    geom_bar(mapping = aes(x = Disciplina, y = media),
             stat = "identity") +
    labs(title = "Média geral por discilpina", x = "", y = "Média") +
    scale_x_discrete(labels = c("ML", "ME", "PB")) +
    ## scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5) +
    facet_grid(~ Grupos, labeller = label_wrap_gen())
  ggsave("media_geral_disciplinas2.png",
         width = 350, height = 150, units = "mm")
}

cria_grupos2 <- function(db){
  grupos <- matrix(c("1:7", "Qualidade do corpo e práticas docentes", "disc",
                     "8:13", "Interesse dos alunos", "disc",
                     "14:21", "Interação professor-aluno", "disc",
                     "22:26", "Pertinência do conteúdo do curso", "disc",
                     "27:32", "Qualidade e organização do curso", "disc"), 
                   ncol = 3, byrow = TRUE,
                   dimnames = list(NULL, c("idx", "nome", "tipo")))
  return(grupos)
}

gera_graficos3 <- function(da, db, db2, dc, dc2, grupos){
  perguntas <- unique(db$Pergunta)
  for(i in 1:5){
    idx <- eval(parse(text = grupos[i, "idx"]))
    grupo <- perguntas[idx]
    ggplot(data = subset(dc, Pergunta %in% grupo)) +
      geom_bar(mapping = aes(x = Disciplina, y = media), stat = "identity") +
      labs(title = grupos[i, "nome"], x = "", y = "Média") +
      scale_x_discrete(labels = c("IE", "IC", "LP", "ML", "ME", "PB")) + 
      ylim(0, 5) + facet_grid(~ Pergunta, labeller = label_wrap_gen())
    ggsave(paste0("media", i, ".png"),
           width = 350, height = 150, units = "mm")
  }
  perguntas2 <- unique(db2$key)
  grupos2 <- matrix(c("1:9", "Avaliação geral do curso DSBD", "geral",
                      "10:18", "Infraestrutura dos ambientes pedagógicos","geral"),
                    ncol = 3, byrow = TRUE, 
                    dimnames = list(NULL, c("idx", "nome", "tipo")))
  for(i in 1:2) {
    idx <- eval(parse(text = grupos2[i, "idx"]))
    grupo <- perguntas2[idx]
    nome <- c("Avaliação Geral do DSBD", "Infraestrutura dos Ambientes pedagógicos")
    ggplot(data = subset(dc2, key %in% grupo)) +
      geom_bar(mapping = aes(x = reorder(key, media),
                             y = media), stat = "identity") +
      labs(title = nome[i], x = "", y = "Média") +
      scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
      coord_flip() +
      ylim(0, 5)
    ggsave(paste0("media_geral", i, ".png"), 
           width = 270, height = 325, units = "mm")
  }
  nresp <- nrow(da)
  G1 <- unique(db$Pergunta)[1:7]
  G2 <- unique(db$Pergunta)[8:13]
  G3 <- unique(db$Pergunta)[14:21]
  G4 <- unique(db$Pergunta)[22:26]
  G5 <- unique(db$Pergunta)[27:32]
  G6 <- unique(db$Pergunta)[33:41]
  G7 <- unique(db$Pergunta)[42:50]
  db$Grupos <- NA
  db[db$Pergunta %in% G1,]$Grupos <- grupos[1,"nome"]
  db[db$Pergunta %in% G2,]$Grupos <- grupos[2,"nome"]
  db[db$Pergunta %in% G3,]$Grupos <- grupos[3,"nome"]
  db[db$Pergunta %in% G4,]$Grupos <- grupos[4,"nome"]
  db[db$Pergunta %in% G5,]$Grupos <- grupos[5,"nome"]
  dmed <- db %>%
    group_by(Grupos) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Grupos, db$Grupos))
  dmed$Grupos <- factor(dmed$Grupos, levels = dmed$Grupos)
  dperg <- db %>%
    group_by(Pergunta) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Pergunta, db$Pergunta))
  ggplot(data = dmed) +
    geom_bar(mapping = aes(x = Grupos, y = media),
             stat = "identity") +
    labs(title = "Média geral", x = "", y = "Média") +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5)
  ggsave(paste0("media_geral_todos.png"),
         width = 250, height = 150, units = "mm")
  ggplot(data = dmed[1:5, ]) +
    geom_bar(mapping = aes(x = Grupos, y = media),
             stat = "identity") +
    labs(title = "Média geral", x = "", y = "Média") +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5)
  ggsave(paste0("media_geral.png"),
         width = 250, height = 150, units = "mm")
  dmed2 <- db %>%
    group_by(Grupos, Disciplina) %>%
    summarize(media = mean(value, na.rm = TRUE)) %>%
    arrange(match(Grupos, db$Grupos))
  dmed2$Grupos <- factor(dmed2$Grupos, levels = unique(dmed2$Grupos))
  dmed2 <- dmed2[complete.cases(dmed2), ]
  ggplot(data = dmed2) +
    geom_bar(mapping = aes(x = Disciplina, y = media),
             stat = "identity") +
    labs(title = "Média geral por discilpina", x = "", y = "Média") +
    scale_x_discrete(labels = c("IE", "IC", "LP", "ML", "ME", "PB")) +
    ## scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    ylim(0, 5) +
    facet_grid(~ Grupos, labeller = label_wrap_gen())
  ggsave("media_geral_disciplinas.png",
         width = 350, height = 150, units = "mm")
  
}

nuvem <- function(da2){
  tb <- da2 %>%
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
  trm <-as.matrix(dtm[,mft[c(1, floor(length(mft)/2), length(mft))]])
  ass <-findAssocs(dtm, terms = mft[1], corlimit = 0.5)
  u <-sort(slam::col_sums(dtm>0), decreasing = TRUE)
  tsp <- 1-u/nDocs(dtm)
  sps <- 1- cumsum(u)/(nDocs(dtm)*seq_along(u))
  rst <-removeSparseTerms(dtm, sparse = 0.95)
  d <-data.frame(word =names(u),
                 freq = u)
  ggwordcloud(words = d$word,
            freq = d$freq,
            min.freq = 1,
            max.words = 30,
            random.order = FALSE,
            colors =brewer.pal(8, "Dark2"))
  ggsave("nuvem de palavras.png")
}

analise <- function(nome,
                    token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  temp <- limpa_dados(nome = nome, token = token)
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

analise2 <- function(nome, token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  temp <- limpa_dados2(nome = nome, token = token)
  db <- temp[[1]]
  db2 <- temp[[2]]
  dc <- temp[[3]]
  dc2 <- temp[[4]]
  da <- temp[[5]]
  geral <- temp[[6]]
  grupos <- cria_grupos2(db = db)
  gera_graficos3(da = da, db = db, db2 = db2, dc = dc, dc2 = dc2, grupos = grupos)
  nuvem(geral)
  return(c(temp[[7]], temp[[8]]))
}

