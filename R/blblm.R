#' @import purrr
#' @import stats
#' @import furrr
#' @importFrom magrittr %>%
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' Linear Regression with Little Bag of Bootstraps
#' @param formula a linear formula
#' @param data a dataframe or dataset
#' @param m number of subsets
#' @param B bumber of bootstrap
#'
#' @return an invisible blblm object
#'
#' @example blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000)
#'
#' @export
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Linear Regression with Little Bag of Bootstraps and parallel computation
#' @param formula a linear formula
#' @param data a dataframe or dataset
#' @param m number of subsets
#' @param B bumber of bootstrap
#' @param n_cores number of cores
#'
#' @return an invisible blblm object
#' @example
#' blblm_p(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' @export
blblm_p <- function(formula, data, m = 10, B = 5000, n_cores = 4) {
  suppressWarnings(future::plan(strategy = "multiprocess", workers = n_cores))
  data_list <- split_data(data, m)
  estimates <- furrr::future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#' @param data dataframe
#' @param m interger
#'
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#' @param formula formula
#' @param data dataframe
#' @param n interger
#' @param B interger
#'
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#' @param formula formula
#' @param data dataframe
#' @param n interger
#'
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#' @param formula formula
#' @param data dataframe
#' @param freqs double
#'
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  object <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(object), sigma = blbsigma(object))
}


#' compute the coefficients from object
#' @param object blblm object
#'
blbcoef <- function(object) {
  coef(object)
}


#' compute sigma from object
#' @param object blblm object
#'
blbsigma <- function(object) {
  p <- object$rank
  y <- model.extract(object$model, "response")
  e <- fitted(object) - y
  w <- object$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' Print out the blblm linear model
#'
#' @param x a blblm object
#'
#' @return a string: blblm model: formula
#' @export
#' @example
#' x <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' print.blblm(x)
#'
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' calculate the sigma for blblm
#'
#' @param object a blblm object
#' @param confidence whether do confidence interval. if TRUE, return confidence interval, if FALSE, return sigma only
#' @param level confidence level
#'
#' @return sigma
#' @export
#' @example
#' x <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' sigma.blblm(x, FALSE, 0.98)
#'
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' calculate the blblm avg coeficient
#'
#' @param object a blblm object
#'
#' @return a coefficient
#' @export
#' @example
#' x <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' coef.blblm(x)
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' calculate the blblm avg confidence interval
#'
#' @param object a blblm object
#' @param parm variable in the model
#' @param level confidence level
#'
#' @export
#' @example
#' x <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' confint.blblm(x,c("wt", "hp"))
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' calculate the prediction from blblm
#'
#' @param object a blblm object
#' @param new_data a new dataframe for prediction
#' @param confidence whether do confidence interval
#' @param level confidence level
#'
#' @export
#' @example
#' x<-blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 5000, n_cores = 4)
#' predict.blblm(x, mtcars, FALSE, 0.95)
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(object = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
