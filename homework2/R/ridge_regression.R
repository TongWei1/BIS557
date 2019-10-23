#' @title Fit Ridge Regression
#'
#' @description Fit a ridge regression model based on the provided formula, data and lambda.
#'
#' @param form The formula for linear model. Example: y ~ x.
#' @param data The input dataset.
#' @param lambda regulation parameter, the larger the lambda the more penalization on the coefficients, default = 0
#' @author Michael Kane & Tong Wei
#' @return The result model which contains the coefficients.
#' @export
#' @examples
#' fit_ridge <- ridge_regression(Sepal.Length ~ ., iris)
#' fit_ridge <- ridge_regression(Sepal.Length ~ ., iris, lambda = 0.01)


ridge_regression <- function(form, data, lambda = 0) {
    rownames(data) <- NULL
    X <- model.matrix(form, data)
    Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
    ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y

    model <- list()
    model$coefficients <- ret

    return(model)
}


