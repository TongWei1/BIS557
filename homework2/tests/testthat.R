library(testthat)
library(homework2)
library(MASS)
library(dplyr)
context("Test the output of homework 2 question 2.")

test_that("You ridge_regression() function works in taxi dataset.", {

    data(taxi)

    taxi <- taxi %>%
        mutate(lat_dist = abs(dropoff_latitude - pickup_latitude),
               lon_dist = abs(dropoff_longitude - pickup_longitude))

    form <- duration ~ hour + weekday + lat_dist + lon_dist
    fit_ridge_regression <- ridge_regression(form, taxi, lambda = 0.1)

    fit_rr <- lm.ridge(form, taxi, lambda = 0.1)

    check_tolerance <- fit_ridge_regression$coefficients * 0.05
    expect_equivalent(coef(fit_rr), fit_ridge_regression$coefficients,
                      tolerance = check_tolerance)
})


test_that("You ridge_regression() function works with iris dataset", {

    data(iris)

    form <- Sepal.Length ~ .
    fit_ridge_regression <- ridge_regression(form, iris, lambda = 0.01)

    fit_rr <- lm.ridge(form, iris, lambda = 0.01)

    expect_equivalent(coef(fit_rr), fit_ridge_regression$coefficients,
                      tolerance = 0.05)

})


test_that("You ridge_regression_optlambda() function works in taxi dataset with default lambda sequence.", {

    data(taxi)

    taxi <- taxi %>%
        mutate(lat_dist = abs(dropoff_latitude - pickup_latitude),
               lon_dist = abs(dropoff_longitude - pickup_longitude))

    form <- duration ~ hour + weekday + lat_dist + lon_dist
    fit_ridge_regression <- ridge_regression_optlambda(form, taxi)

    #cross validation for lm.ridge
    fivefolds <- cut(seq(1,nrow(taxi)),breaks=5,labels=FALSE)
    lambda_seq = seq(0, 200*2, length.out = 500)
    X <- model.matrix(form, taxi)
    ridge_beta <- matrix(NA_real_, nrow = length(lambda_seq), ncol = ncol(X))
    mse <- NULL

    for (i in 1:length(lambda_seq)){
        beta_hat <- matrix(NA_real_, nrow = 5, ncol = ncol(X))
        test_mse <- NULL
        for(j in 1:5){
            validation_id <- which(fivefolds==j,arr.ind=TRUE)
            validation <- taxi[validation_id, ]
            train <- taxi[-validation_id, ]
            fit_rr <- lm.ridge(form, train, lambda = lambda_seq[i])

            vali_X <- model.matrix(form, validation)
            vali_Y <- validation[[as.character(form)[2]]][as.numeric(rownames(vali_X))]
            Y_hat <- tcrossprod(vali_X, t(coef(fit_rr)))
            test_mse[j] <- mean((Y_hat - vali_Y)^2)
        }
        mse[i] <- mean(test_mse)
    }
    lambda <- lambda_seq[which.min(mse)]
    fit_rr <- lm.ridge(form, taxi, lambda = lambda)
    check_tolerance <- fit_ridge_regression$coefficients * 0.05
    expect_equivalent(coef(fit_rr), fit_ridge_regression$coefficients,
                      tolerance = check_tolerance)
})


