---
title: "Exercício 9.18"
output: html_notebook
---
# Exercício 9.18
  
## a)
data <- data.frame(x = c(1, 4, 5, 9, 11, 13, 23, 23, 28),
                   y = c(64, 71, 54, 81, 76, 93, 77, 95, 109))
mat <- as.matrix(data)
X <- cbind(1, data$x)
tx <- t(X)
# Estimativa dos parâmetros através do modelo de Regressão
(beta.hat <- solve(tx%*%X)%*%tx%*%mat[, "y"])
# Estimativa dos parâmetros através do modelo de Regressão implementado no R
coef(lm(y ~ x, data = data))

## b)
cor(data)[1, 2]

## c)
library(ggplot2)
ggplot(data = data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

