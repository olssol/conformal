#' Simulate From a Skewed Distribution
#'
#' Simulate skew random variable
#'
#'  @export
#'
cfm_simu_skew <- function(n, mu, sig, eta, seed = NULL) {

    if (!is.null(seed)) {
        old_seed <- set.seed(seed)
    }

    w1  <- abs(rnorm(n))
    w2  <- rnorm(n, 0, sig)
    rst <- mu + eta * w1 + w2

    if (!is.null(seed)) {
        set.seed(old_seed)
    }

    rst
}

#' Simulate Y
#'
#' Simulate Y given x and formula
#'
#'  @export
#'
cfm_simu_y <- function(x, eps = 0,
                       par_beta = rep(1, 5),
                       fml  = "~ x + I(x^2) + I(x^3) + I(x^4)") {

    des   <- model.matrix(as.formula(fml), data  = data.frame(x = x))
    ymean <- des %*% par_beta

    data.frame(x      = x,
               y_mean = ymean,
               y      = ymean + eps,
               eps    = eps)
}


#' Simulate Data
#'
#' Function 1
#'
#'
#' @export
#'
cfm_simu_dta_1 <- function(n, x_mean, x_sig, x_eta, eps_sig, eps_eta,
                           ...,
                           seed = NULL) {
    if (!is.null(seed)) {
        old_seed <- set.seed(seed)
    }

    x   <- cfm_simu_skew(n, x_mean, x_sig,   x_eta)
    eps <- cfm_simu_skew(n, 0,      eps_sig, eps_eta)
    rst <- cfm_simu_y(x, eps, ...)

    if (!is.null(seed)) {
        set.seed(old_seed)
    }

    rst
}
