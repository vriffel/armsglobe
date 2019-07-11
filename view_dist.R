#' @export
#' @author Vinicius Riffel,  \email{viniciusriffel@ufpr.br}
#' @title Visualization of Distribution Functions with Parameters Control
#' @description This function enable a plot window with
#' \href{https://cran.r-project.org/web/packages/rpanel/index.html}{rpanel}
#' sliders that allow to control the parameters of distribution functions.
#' Access \code{\link{
#' https://raw.githubusercontent.com/vriffel/material/master/view_dist_aux.R}}
#' to get the auxilliary functions code.
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

source("https://raw.githubusercontent.com/cran/rpanel/master/R/window.r")
source("https://raw.githubusercontent.com/cran/rpanel/master/R/slider.r")
source("https://raw.githubusercontent.com/vriffel/material/master/view_dist_aux.R")

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
