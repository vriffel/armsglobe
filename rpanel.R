library(rpanel)
## Uniforme
unif.panel <- function(panel, mean = TRUE){
    curve(dunif(x, min = panel$min,
                max = panel$max),
          from = panel$interval[1], to = panel$interval[2])
    if (mean) {
        abline(v = (panel$min[1] + panel$max)/2, col = "blue")
    }
    panel
}

panel <- rp.control(interval = c(-10, 15))

rp.slider(panel, min, -5, 4.9, initval = -5, showvalue = T, action = unif.panel)

rp.slider(panel, max, 5, 10, initval=10, showvalue=TRUE, action=unif.panel)

## Exponencial

exp.panel <- function(panel, mean = TRUE){
    curve(dexp(x, rate=panel$rate),
          from=panel$interval[1], to=panel$interval[2])
    if (mean) {
        abline(v = 1/(panel$rate), col = "blue")
    }
    panel
}

panel <- rp.control(interval=c(0,5))

rp.slider(panel, rate, 0.001, 10, initval=1, showvalue=TRUE, action=exp.panel)


## Cauchy
cauchy.panel <- function(panel){
    curve(dcauchy(x, location = panel$location,
                scale = panel$scale),
          from = panel$interval[1], to = panel$interval[2])
    panel
}

panel <- rp.control(interval = c(-20, 20))

rp.slider(panel, location, -20, 20, initval = -20, showvalue = T, action = cauchy.panel)

rp.slider(panel, scale, 0.001, 5, initval=0.001, showvalue=TRUE, action=cauchy.panel)


## Chi Square Distribution
chisq.panel <- function(panel){
    curve(dt(x, df=panel$df, ncp = panel$ncp),
          from=panel$interval[1], to=panel$interval[2])
}

panel <- rp.control(interval=c(0, 15))

rp.slider(panel, df, 1, 10, initval=1, showvalue=TRUE, action=chisq.panel)

rp.slider(panel, ncp, 0, 10, initval=1, showvalue=TRUE, action=chisq.panel)

## Snedecor Distribution
f.panel <- function(panel){
    curve(df(x, df1 = panel$df1, df2 = panel$df2,
             ncp = panel$ncp),
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 15))

rp.slider(panel, df1, 1, 10, initval=2, showvalue=TRUE, action=f.panel)

rp.slider(panel, df2, 1, 10, initval=3, showvalue=TRUE, action=f.panel)

rp.slider(panel, ncp, 1, 10, initval=1, showvalue=TRUE, action=f.panel)

## Gamma Distribution
gamma.panel <- function(panel){
    curve(dgamma(x, shape = panel$shape, rate = panel$rate, ),
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 15))

rp.slider(panel, shape, 0.001, 10, initval=2, showvalue=TRUE, action=gamma.panel)

rp.slider(panel, rate, 0.001, 10, initval=3, showvalue=TRUE, action=gamma.panel)


## Beta Distribution
beta.panel <- function(panel){
    curve(dbeta(x, shape1 = panel$shape1,
                shape2 = panel$shape2,
                ncp = panel$ncp),
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 1))

rp.slider(panel, shape1, 1, 10, initval=2, showvalue=TRUE, action = beta.panel)

rp.slider(panel, shape2, 1, 10, initval=3, showvalue=TRUE, action = beta.panel)

rp.slider(panel, ncp, 0, 10, initval = 0, showvalue=TRUE, action = beta.panel)


## Logistic Distribution
logis.panel <- function(panel){
    curve(dlogis(x, location  = panel$location,
                scale = panel$scale),
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(-6, 6))

rp.slider(panel, location, 0, 5, initval = 0, showvalue=TRUE, action = logis.panel)

rp.slider(panel, scale, 1, 5, initval = 1, showvalue=TRUE, action = logis.panel)

########################################

## Binomial Distribution
binom.panel <- function(panel){
    curve(dbinom(x, size  = panel$size,
                 prob = panel$prob),
          n = (panel$interval[2] + 1), type = "h",
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 20))

rp.slider(panel, size, 0, 20, initval = 0, showvalue=TRUE, action = binom.panel,
          resolution = 1)

rp.slider(panel, prob, 0, 1, initval = 1, showvalue=TRUE, action = binom.panel)


## Poisson Distribution
pois.panel <- function(panel){
    curve(dpois(x, lambda  = panel$lambda), type = "h",
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 20))

rp.slider(panel, lambda, 0, 20, initval = 0, showvalue=TRUE, action = pois.panel,
          resolution = 1)

## Hypergeometric Distribution
hyper.panel <- function(panel){
    curve(dhyper(x, m  = panel$m, n = 10,
                 k = 6),
          type = "h",
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 10))

rp.slider(panel, m, 0, 20, initval = 0, showvalue=TRUE, action = hyper.panel,
          resolution = 1)


## Multinomial Distribution
multinom.panel <- function(panel){
    curve(dmultinom(x, size = 10, prob = panel$prob),
          type = "h",
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 20))

rp.slider(panel, prob, 0, 1, initval = 0, showvalue=TRUE, action = multinom.panel)

## Negative Binomial
dnbinom.panel <- function(panel){
    curve(dnbinom(x, size = panel$size, prob = panel$prob),
          type = "h",
          from=panel$interval[1], to=panel$interval[2])
    panel
}

panel <- rp.control(interval=c(0, 20))

rp.slider(panel, prob, 0.1, 1, initval = 0, showvalue=TRUE, action = dnbinom.panel)

rp.slider(panel, size, 1, 10, initval = 1, showvalue=TRUE, action = dnbinom.panel)
