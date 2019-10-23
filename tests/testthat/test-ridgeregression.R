library(testthat)

context("Test function ridge_regression().")

test_that("You ridge_regression() function works.", {

  data(iris)

  fit_ridge_regression <- ridge_regression(Sepal.Length ~ ., iris,lambda=0.005)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, as.numeric(fit_ridge_regression),
                    tolerance = 1e-2)
})



test_that("You opt_lambda() function works.", {

  data(iris)

  l=opt_lambda(lambdas=seq(0,0.5,0.005),iris,Sepal.Length~.)

  fit_ridge_regression<-ridge_regression(Sepal.Length ~ ., iris,lambda=l)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, as.numeric(fit_ridge_regression),
                    tolerance = 1e-5)
})
