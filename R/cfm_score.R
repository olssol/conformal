#' Score function
#'
#' Score function
#'
#' @export
#'
cfm_score_abs <- function(x, y, f_pred, ...) {
    mu_hat <- f_pred(x, ...)
    abs(y  - mu_hat)
}


#' Score Interval
#'
#' Find Intervals
#'
#' @export
#'
cfm_score_interval <- function(y, indicator) {

    rst   <- NULL
    cur_s <- min(which(1 == indicator))

    while (!is.infinite(cur_s)) {
        s_y <- y[cur_s]
        if (cur_s > 1) {
            y         <- y[-(1:cur_s)]
            indicator <- indicator[-(1:cur_s)]
        }

        cur_e <- min(which(0 == indicator))
        if (is.infinite(cur_e)) {
            e_y   <- y[length(y)]
            cur_s <- Inf
        } else {
            e_y       <- y[cur_e - 1]
            y         <- y[-(1:cur_e)]
            indicator <- indicator[-(1:cur_e)]
            cur_s     <- min(which(1 == indicator))
        }

        rst <- rbind(rst, c(s_y, e_y))
    }

    rst
}
