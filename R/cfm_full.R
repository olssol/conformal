#' Full Conformal Prediction
#'
#' Full Conformal Prediction
#'
#'  @export
#'
cfm_prediction_lm <- function(dat, fml = "y ~ x") {
    rst <- lm(as.formula(fml), data = dat)
    rst
}


#' Full Conformal for Single X and Y
#'
#' Full Conformal for Single X and Y
#'
#'  @export
#'
cfm_prediction_xy_single <- function(y, x, dat,
                                     fml     = "y ~ x",
                                     alpha   = 0.05,
                                     f_score = cfm_score_abs) {

    f_pred <- function(new_x, m_fit) {
        predict(m_fit,
                newdata = data.frame(x = new_x))
    }

    all_scores <- NULL
    for (i in seq_len(nrow(dat))) {
        cur_dat         <- dat
        cur_dat[i, "x"] <- x
        cur_dat[i, "y"] <- y

        cur_fit       <- cfm_prediction_lm(cur_dat, fml = fml)
        cur_score     <- f_score(dat[i, "x"],
                                 dat[i, "y"],
                                 f_pred,
                                 cur_fit)

        all_scores    <- c(all_scores, cur_score)
    }

    cur_fit   <- cfm_prediction_lm(dat, fml = fml)
    new_score <- f_score(x, y, f_pred, cur_fit)
    q_alpha   <- quantile(c(all_scores, Inf), 1 - alpha)

    c(y, new_score <= q_alpha, x, new_score, q_alpha)
}


#' Full Conformal All Scores
#'
#' All Scores
#'
#'  @export
#'
cfm_prediction_xy <- function(vec_y, ...,
                              f_pred = cfm_prediction_xy_single,
                              n_core = 1) {
    rst <- parallel::mclapply(vec_y,
                              function(y) {
                                  print(y)
                                  f_pred(y, ...)
                              },
                              mc.cores = n_core)

    t(simplify2array(rst))
}
