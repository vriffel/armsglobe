library(RColetum)
library(tidyverse)
limpa_dados <- function(form,
                        token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  myForms <- RColetum::GetForms(token = token)
  id <- as.numeric(myForms$id[grep(form,myForms$name)])
  myAnswers <- RColetum::GetAnswers(token,id)
  ## - 11 pois há colunas que são lixo e 1 que não temos interesse
  aux <- ncol(myAnswers) - 11
  da <- myAnswers[,2:aux]
  ## Remove números dos nomes de colunas
  names(da) <- gsub("[0-9]", "", names(da))
  ## Quebra strings nas letras maiúsculas
  names(da) <- gsub("([[:upper:]])", " \\1", names(da))
  ## Arruma case
  names(da) <- str_to_title(names(da))
  return(da)
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
    ## Seleciona os grupos de perguntas
    idx <- eval(parse(text = grupos[i, "idx"]))
    grupo <- perguntas[idx]
    if(grupos[i, "tipo"] == "disc"){
      ## Gráficos para perguntas por disciplina ----------------------
      ## Gráfico com avaliação geral
      ggplot(data = subset(db, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = value, y = ..prop..)) +
        labs(title = grupos[i, "nome"], x = "", y = "") +
        ylim(0, 0.8) +
        facet_grid(Pergunta ~ Disciplina,
                   labeller = label_wrap_gen()) +
        coord_flip()
      ggsave(paste0("geral_2", i, ".png"),
             width = 270, height = 325, units = "mm")
      ## Gráfico com médias por pergunta
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
      ## Gráficos para perguntas gerais ------------------------------
      ## Gráfico com avaliação geral
      ggplot(data = subset(db, Pergunta %in% grupo)) +
        geom_bar(mapping = aes(x = value, y = ..prop..)) +
        labs(title = grupos[i, "nome"], x = "", y = "") +
        ylim(0, 0.8) +
        #facet_grid(~ Pergunta) +
        facet_wrap(~ Pergunta, labeller = label_wrap_gen()) +
        coord_flip()
      ggsave(paste0("geral_2", i, ".png"),
             width = 270, height = 325, units = "mm")
      ## Gráfico com médias por pergunta
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

analise <- function(nome,
                    token = "t5nr3wrju6ssk8cgkwwk4coo0wwsc8s"){
  da <- limpa_dados(nome,token = token)
  db <- organiza_dados(da)
  dc <- organiza_dados2(db)
  grupos <- cria_grupos(db)
  gera_graficos(grupos = grupos, db = db, dc = dc)
  gera_graficos2(da = da, db = db, grupos = grupos)
}
