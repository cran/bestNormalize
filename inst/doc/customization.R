## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7)
library(bestNormalize)

## -----------------------------------------------------------------------------
## Define user-function
cuberoot_x <- function(x, a = NULL, standardize = TRUE) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE)))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x))) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
  
  
  x.t <- (x + a)^(1/3)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  # Get in-sample normality statistic results
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    a = a,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  
  # Assign class
  class(val) <- c('cuberoot_x', class(val))
  val
}


## -----------------------------------------------------------------------------

predict.cuberoot_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  
  # If no data supplied and not inverse
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  
  # If no data supplied and inverse
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  # Actually performing transformations
  
  # Perform inverse transformation as estimated
  if (inverse) {
    
    # Reverse-standardize
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    
    # Reverse-cube-root (cube)
    newdata <-  newdata^3 - object$a
    
    
    # Otherwise, perform transformation as estimated
  } else if (!inverse) {
    # Take cube root
    newdata <- (newdata + object$a)^(1/3)
    
    # Standardize to mean 0, sd 1
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  
  # Return transformed data
  unname(newdata)
}


## -----------------------------------------------------------------------------
print.cuberoot_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'cuberoot(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


## -----------------------------------------------------------------------------

custom_transform <- list(
  cuberoot_x = cuberoot_x,
  predict.cuberoot_x = predict.cuberoot_x,
  print.cuberoot_x = print.cuberoot_x
)

set.seed(123129)
x <- rgamma(100, 1, 1)
(b <- bestNormalize(x = x, new_transforms = custom_transform, standardize = FALSE))

## -----------------------------------------------------------------------------
all.equal(x^(1/3), b$chosen_transform$x.t)
all.equal(x^(1/3), predict(b))

## -----------------------------------------------------------------------------
bestNormalize(x, norm_stat_fn = function(x) nortest::lillie.test(x)$stat)

## -----------------------------------------------------------------------------
(dont_do_this <- bestNormalize(x, norm_stat_fn = function(x) nortest::lillie.test(x)$p))

## -----------------------------------------------------------------------------
best_tranform <- names(which.max(dont_do_this$norm_stats))
(do_this <- dont_do_this$other_transforms[[best_tranform]])

## -----------------------------------------------------------------------------
(do_this <- bestNormalize(x, norm_stat_fn = function(x) 1-nortest::lillie.test(x)$p))

