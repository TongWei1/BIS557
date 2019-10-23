#' @title Fit Ridge Regression with optimizing lambda
#'
#' @description Fit a ridge regression model based on the provided formula, data.
#'
#' @param form The formula for linear model. Example: y ~ x.
#' @param data The input dataset.
#' @param lambda_seq the sequence of the tested regulation parameter, default = seq(0, 200*2, length.out = 500)
#' @author Tong Wei
#' @return The result model which contains the coefficients.
#' @export
#' @examples
#' fit_ridge <- ridge_regression(Sepal.Length ~ ., iris)


ridge_regression_optlambda <- function(form, data, lambda_seq = seq(0, 200*2, length.out = 500)) {
    rownames(data) <- NULL
    data <- data[sample(nrow(data)),]
    X <- model.matrix(form, data)

    fivefolds <- cut(seq(1,nrow(data)),breaks=5,labels=FALSE)
    k <- length(lambda_seq)
    ridge_beta <- matrix(NA_real_, nrow = k, ncol = ncol(X))
    mse <- NULL

    for (i in 1:k){
        beta_hat <- matrix(NA_real_, nrow = 5, ncol = ncol(X))
        test_mse <- NULL
        for(j in 1:5){
            validation_id <- which(fivefolds==j,arr.ind=TRUE)
            validation <- data[validation_id, ]
            train <- data[-validation_id, ]
            X <- model.matrix(form, train)
            Y <- train[[as.character(form)[2]]][as.numeric(rownames(X))]

            beta_hat[j, ] <- solve( crossprod(X) + diag(rep(lambda_seq[i], ncol(X))) ) %*% t(X) %*% Y
            vali_X <- model.matrix(form, validation)
            vali_Y <- validation[[as.character(form)[2]]][as.numeric(rownames(vali_X))]
            Y_hat <- tcrossprod(vali_X, t(beta_hat[j, ]))
            test_mse[j] <- mean((Y_hat - vali_Y)^2)
        }
        ridge_beta[i,] <- colMeans(beta_hat)
        mse[i] <- mean(test_mse)
    }
    #print(ridge_beta)
    #y_hat <- tcrossprod(X, ridge_beta)
    #mse <- apply((y_hat - Y)^2, 2, mean)
    lambda <- lambda_seq[which.min(mse)]
    X <- model.matrix(form, data)
    Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
    ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y

    model <- list()
    model$coefficients <- ret
    model$optlambda <- lambda
    return(model)
}




