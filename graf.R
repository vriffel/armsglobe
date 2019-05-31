# library
library(ggplot2)

#cursos
a <- names(da.tb)[1:5]
b <- c("Estatistica","TADS","Sistemas de Informação","Administração","Economia")
value <- as.numeric(da.tb[1:5])
data=data.frame(b,value)

ggplot(data=data,
       aes(fill=b,y=value,x=reorder(b, value))
)+
  geom_bar(stat="identity",show.legend = F)+
  labs(x = NULL,y = NULL) 

#faixa etaria
a <- names(db.tb)[1:5]
value <- as.numeric(db.tb)
data=data.frame(a,value)

ggplot(data,aes(fill=a,y=value,x=a))+
  geom_bar(stat="identity",position="dodge")

#tempo
a <- names(dc.tb)
value <- as.numeric(dc.tb)
data=data.frame(a,value)

ggplot(data,aes(fill=a,y=value,x=a))+
  geom_bar(stat="identity",position="dodge")