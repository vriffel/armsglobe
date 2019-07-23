#' @name view_dist
#' @export
#' @author Vinicius Riffel,  \email{viniciusriffel@ufpr.br}
#' @title Visualization of some Probability Functions with Parameters Control
#' @description This function enable a plot window with
#' \href{https://cran.r-project.org/web/packages/rpanel/index.html}{rpanel}
#' sliders that allow to control the parameters of distribution functions.
#' @param dist string It's the name of distribution that you want to plot
#' . The prefix used was the same of the R distribution families default.
#' @param mean logical TRUE indicate that you want to plot the first mome
#' nt (if it exists) of the distribution. FALSE indicate that you don't to
#' plot the first moment.
#' @return NULL
#' @examples
#'
#' view_dist("norm", mean = TRUE)
#' view_dist("binom", mean = FALSE)

if (!requireNamespace("rpanel", quielty = TRUE)) {
    stop(paste("Can not find 'rpanel' package. Please install it."))
}
view_dist <- function(dist, mean = TRUE) {
    switch(dist, "chisq" = chisq_dist(mean),
           "norm" = norm_dist(mean),
           "unif" = unif_dist(mean),
           "exp" = exp_dist(mean),
           "cauchy" = cauchy_dist(),
           "f" = f_dist(mean),
           "gamma" = gamma_dist(mean),
           "beta" = beta_dist(mean),
           "logis" = logis_dist(mean),
           "t" = t_dist(),
           "binom" = binom_dist(mean),
           "pois" = pois_dist(mean),
           "geom" = geom_dist(mean),
           "hyper" = hyper_dist(),
           "nbinom" = nbinom_dist(mean),
           stop("The ", dist, " distribution it's not avaible. ",
                    "Please check the documentation for functions available"))
}


#------------------------------------------------------------
                                        # Auxiliary Functions

chisq_dist <- function(lgl) {
    chisq.panel <- function(panel, mean = lgl){
        curve(dchisq(x, df = panel$df),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = panel$df, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 20))
    rp.slider(panel, df, 0.1, 10, initval = 0.1, showvalue = TRUE,
              action = chisq.panel)
}

norm_dist <- function(lgl) {
    norm.panel <- function(panel){
        ## panel$interval: vector with domain of function
        ## panel$...: They'll be the parameters of the probability function
        curve(dnorm(x, mean = panel$mean, sd = panel$sd),
              from = panel$interval[1], to = panel$interval[2])
        if (lgl) {
            abline(v =  panel$mean, col = "blue")
        }
        panel
    }
        ## It open the window and define the domain
    panel <- rp.control(interval = c(-4,4))
        ## It open the mean's slider
    rp.slider(panel, mean, -4, 4, initval = 0, showvalue = TRUE,
              action = norm.panel)
        ## It open the standard deviation slider
    rp.slider(panel, sd, 0.001, 10, initval = 1, showvalue = TRUE,
              action = norm.panel)
}

## Uniform Distribution
unif_dist <- function(lgl) {
    unif.panel <- function(panel, mean = lgl){
        curve(dunif(x, min = panel$min, max = panel$max),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = (panel$min[1] + panel$max) / 2, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(-10, 15))
    rp.slider(panel, min, -5, 4.9, initval = -5, showvalue = TRUE,
              action = unif.panel)
    rp.slider(panel, max, 5, 10, initval = 10, showvalue = TRUE,
              action = unif.panel)
}

## Exponencial Distribution
exp_dist <- function(lgl){
    exp.panel <- function(panel, mean = lgl){
        curve(dexp(x, rate = panel$rate),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = 1 / (panel$rate), col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0,5))
    rp.slider(panel, rate, 0.001, 10, initval = 1, showvalue = TRUE,
              action = exp.panel)
}

## Cauchy Distribution
cauchy_dist <- function() {
    cauchy.panel <- function(panel){
        curve(dcauchy(x, location = panel$location, scale = panel$scale),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        panel
    }
    panel <- rp.control(interval = c(-20, 20))
    rp.slider(panel, location, -20, 20, initval = -20, showvalue = T,
              action = cauchy.panel)
    rp.slider(panel, scale, 0.1, 5, initval = 0.001, showvalue = TRUE,
              action = cauchy.panel)
}

## Snedecor Distribution
f_dist <- function(lgl) {
    f.panel <- function(panel){
        curve(df(x, df1 = panel$df1, df2 = panel$df2, ncp = panel$ncp),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = (panel$df2) / (panel$df2 - 2), col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 15))
    rp.slider(panel, df1, 1, 10, initval = 2, showvalue = TRUE,
              action = f.panel)
    rp.slider(panel, df2, 1, 10, initval = 3, showvalue = TRUE,
              action = f.panel)
    rp.slider(panel, ncp, 1, 10, initval = 1, showvalue = TRUE,
              action = f.panel)
}

## Gamma Distribution
gamma_dist <- function(lgl) {
    gamma.panel <- function(panel){
        curve(dgamma(x, shape = panel$shape, rate = panel$rate),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = (panel$shape/panel$rate), col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 15))
    rp.slider(panel, shape, 0.001, 10, initval = 2, showvalue = TRUE,
              action = gamma.panel)
    rp.slider(panel, rate, 0.001, 10, initval = 3, showvalue = TRUE,
              action = gamma.panel)
}

## Beta Distribution
beta_dist <- function(lgl) {
    beta.panel <- function(panel){
        curve(dbeta(x, shape1 = panel$shape1, shape2 = panel$shape2,
                    ncp = panel$ncp),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = (panel$shape1) / (panel$shape1 + panel$shape2),
                   col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 1))
    rp.slider(panel, shape1, 1, 10, initval = 2, showvalue = TRUE,
              action = beta.panel)
    rp.slider(panel, shape2, 1, 10, initval = 3, showvalue = TRUE,
              action = beta.panel)
    rp.slider(panel, ncp, 0, 10, initval = 0, showvalue = TRUE,
              action = beta.panel)
}

## Logistic Distribution
logis_dist <- function(lgl) {
    logis.panel <- function(panel){
        curve(dlogis(x, location  = panel$location, scale = panel$scale),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = panel$location, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(-6, 6))
    rp.slider(panel, location, 0, 5, initval = 0, showvalue = TRUE,
              action = logis.panel)
    rp.slider(panel, scale, 1, 5, initval = 1, showvalue = TRUE,
              action = logis.panel)
}

## Gosset Distribution
t_dist <- function(lgl) {
    t.panel <- function(panel){
        curve(dt(x, df = panel$df, ncp = panel$ncp),
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y", xlim = c(0, 15))
        panel
    }
    panel <- rp.control(interval = c(0, 15))
    rp.slider(panel, ncp, 1, 15, init = 1, showvalue = TRUE,
              action = t.panel)
    rp.slider(panel, df, 1, 15, initval = 1, showvalue = TRUE,
              action = t.panel, resolution = 1)
}

## Binomial Distribution
binom_dist <- function(lgl) {
    binom.panel <- function(panel){
        curve(dbinom(x, size  = panel$size, prob = panel$prob),
              n = (panel$interval[2] + 1), type = "h",
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = panel$size * panel$prob, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval=c(0, 20))
    rp.slider(panel, size, 0, 20, initval = 0, showvalue=TRUE,
              action = binom.panel, resolution = 1)
    rp.slider(panel, prob, 0, 1, initval = 1, showvalue=TRUE,
              action = binom.panel)
}

## Poisson Distribution
pois_dist <- function(lgl) {
    pois.panel <- function(panel){
        curve(dpois(x, lambda  = panel$lambda),
              type = "h", from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = panel$lambda, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 20))
    rp.slider(panel, lambda, 0, 20, initval = 0, showvalue = TRUE,
              action = pois.panel, resolution = 1)
}

## Geometric Distribution
geom_dist <- function(lgl) {
    geom.panel <- function(panel){
        curve(dgeom(x, prob  = panel$prob), type = "h",
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = panel$prob/(panel$prob)^2, col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 20))
    rp.slider(panel, prob, 0.01, 1, initval = 0.01, showvalue = TRUE,
              action = geom.panel)
}

## Hypergeometric Distribution
hyper_dist <- function() {
    hyper.panel <- function(panel){
        curve(dhyper(x, m  = panel$m, n = 10, k = 10), type = "h",
              from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        panel
    }
    panel <- rp.control(interval = c(0, 10))
    rp.slider(panel, m, 0, 100, initval = 0, showvalue = TRUE,
              action = hyper.panel, resolution = 1)
}

## Negative Binomial
nbinom_dist <- function(lgl){
    nbinom.panel <- function(panel){
        curve(dnbinom(x, size = panel$size, prob = panel$prob),
              type = "h", from = panel$interval[1], to = panel$interval[2],
              ylab = "y")
        if (lgl) {
            abline(v = (panel$size)/(panel$prob), col = "blue")
        }
        panel
    }
    panel <- rp.control(interval = c(0, 20))
    rp.slider(panel, prob, 0.1, 1, initval = 0.1, showvalue = TRUE,
              action = nbinom.panel)
    rp.slider(panel, size, 1, 10, initval = 1, showvalue = TRUE,
              action = nbinom.panel)
}
