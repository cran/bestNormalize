## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7)
library(bestNormalize)

## ---- echo = FALSE-------------------------------------------------------

x <- iris$Petal.Width
old_points <- x

x_t <- qnorm(rank(old_points, na.last = 'keep') / (length(old_points) + 1))
plot(old_points, x_t, main = "The orderNorm Transformation", 
     pch = 20, cex = 1, xlim = c(-.5, 3), ylim = c(-3, 3),
     xlab = "Original Value", ylab = "Transformed Value")
abline(h = 0, v = 0, lty = 3)

# Fit and predictions for plot
fit1 <- lm(x_t ~ old_points)
x_indomain <- seq(min(old_points), max(old_points), length = 100) 
x_low <- seq(-.5, min(old_points), length = 100)
x_high <- seq(max(old_points), 3, length = 100)

y_indomain <- predict(fit1, newdata = data.frame(old_points = x_indomain))
y_low <- predict(fit1, newdata = data.frame(old_points = x_low))
y_high <- predict(fit1, newdata = data.frame(old_points = x_high))

lines(x = x_indomain, y_indomain)
lines(x = x_low, y = y_low, lty = 2)
lines(x = x_high, y = y_high, lty = 2)
points(x = range(old_points), y = range(y_indomain), pch = 1, col = 1, cex = 1.5)

# Add extrapolated 
y_low_extra <- predict(fit1, newdata = data.frame(old_points = x_low)) - (min(predict(fit1)) - min(x_t))
y_high_extra <- predict(fit1, newdata = data.frame(old_points = x_high)) - (max(predict(fit1)) - max(x_t))
lines(x = x_low, y = y_low_extra, lwd = 2, col = 'slateblue')
lines(x = x_high, y = y_high_extra, lwd = 2, col = 'slateblue')

# Add interpolated 
approx1 <- approx(old_points, x_t, xout = x_indomain, rule = 1)
lines(approx1 , col = 'slateblue', lwd = 2)

# Add legend
legend('bottomright', 
       c('Original data', 'Tranformed values for new data', 
         'Linear approximation', 'Extrapolation'), 
       bty = 'n', lty = c(0, 1, 1, 2), lwd = c(0, 2,1,1), 
       pch = c(20, NA, NA, NA), 
       col = c(1, "slateblue", 1, 1))

## ------------------------------------------------------------------------

# Generate some data
set.seed(100)
x <- rgamma(1000, 1, 1)
MASS::truehist(x, nbins = 12)

## ------------------------------------------------------------------------
# Lambert's W x F transfromation
(lambert_obj <- lambert(x))
# Box Cox's Transformation
(boxcox_obj <- boxcox(x))
# Yeo-Johnson's Transformation
(yeojohnson_obj <- yeojohnson(x))
# orderNorm Transformation
(orderNorm_obj <- orderNorm(x))

# Pick the best one automatically
(BNobject <- bestNormalize(x))

# Last resort - binarize
(binarize_obj <- binarize(x))


## ------------------------------------------------------------------------
xx <- seq(min(x), max(x), length = 100)
plot(xx, predict(lambert_obj, newdata = xx), type = "l", col = 1, ylim = c(-4, 4),
     xlab = 'x', ylab = "g(x)")
lines(xx, predict(boxcox_obj, newdata = xx), col = 2)
lines(xx, predict(yeojohnson_obj, newdata = xx), col = 3)
lines(xx, predict(orderNorm_obj, newdata = xx), col = 4)

legend("bottomright", legend = c("Lambert WxF", "Box Cox", "Yeo-Johnson", "OrderNorm"), 
       col = 1:4, lty = 1, bty = 'n')

## ---- fig.height=8, fig.width = 7----------------------------------------
par(mfrow = c(2,2))
MASS::truehist(lambert_obj$x.t, main = "Lambert WxF transformation", nbins = 12)
MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)

## ------------------------------------------------------------------------
par(mfrow = c(1,2))
MASS::truehist(BNobject$x.t, main = paste("Best Transformation:", BNobject$method), nbins = 12)
plot(xx, predict(BNobject, newdata = xx), type = "l", col = 1, 
     main = "Best Normalizing transformation", ylab = "g(x)", xlab = "x")

## ---- fig.height=8, fig.width=7------------------------------------------
data("autotrader")
autotrader$yearsold <- 2017 - autotrader$Year
### Using best-normalize
(priceBN <- bestNormalize(autotrader$price))
(mileageBN <- bestNormalize(autotrader$mileage))
(yearsoldBN <- bestNormalize(autotrader$yearsold, allow_orderNorm = FALSE))

par(mfrow = c(3, 2))
MASS::truehist(autotrader$price)
MASS::truehist(priceBN$x.t)
MASS::truehist(autotrader$mileage)
MASS::truehist(mileageBN$x.t)
MASS::truehist(autotrader$yearsold)
MASS::truehist(yearsoldBN$x.t)

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
price.xx <- seq(min(autotrader$price), max(autotrader$price), length = 100)
mileage.xx <- seq(min(autotrader$mileage), max(autotrader$mileage), length = 100)
yearsold.xx <- seq(min(autotrader$yearsold), max(autotrader$yearsold), length = 100)

plot(price.xx, predict(priceBN, newdata = price.xx), type = "l", 
     main = "Price bestNormalizing transformation", 
     xlab = "Price ($)", ylab = "g(price)")
plot(mileage.xx, predict(mileageBN, newdata = mileage.xx), type = "l", 
     main = "Mileage bestNormalizing transformation", 
     xlab = "Mileage", ylab = "g(Mileage)")
plot(yearsold.xx, predict(yearsoldBN, newdata = yearsold.xx), type = "l", 
     main = "Years-old bestNormalizing transformation", 
     xlab = "Years-old", ylab = "g(Years-old)")

## ------------------------------------------------------------------------
autotrader$price.t <- priceBN$x.t
autotrader$mileage.t <- mileageBN$x.t
autotrader$yearsold.t <- yearsoldBN$x.t

fit4 <- lm(price.t ~ mileage.t + yearsold.t,
           data = autotrader)
summary(fit4)

miles.t <- predict(mileageBN, newdata = mileage.xx)
c1 <- coef(fit4)["mileage.t"]

par(mfrow = c(1, 1))
plot(
    mileageBN$x.t,
    priceBN$x.t,
    pch = 16,
    col = grey(.1, alpha = .2),
    main = "Estimated linear effect (using transformed data)",
    xlab = "g(Mileage)",
    ylab = "g(Price)"
)
lines(miles.t,
      coef(fit4)[1] + c1 * miles.t,
      col = "slateblue",
      lwd = 2)

## Mileage effect
plot(
  autotrader$mileage,
  autotrader$price,
  pch = 16,
  col = grey(.1, alpha = .2),
  main = "Mileage effect (re-transformed to original unit)",
  xlab = "Mileage",
  ylab = "Price"
)
line_vals <- miles.t * c1 + coef(fit4)[1]
lines(
    mileage.xx,
    y = predict(priceBN, newdata = line_vals, inverse = TRUE),
    lwd = 2,
    col = "slateblue"
)
# Compare to GAM fit
fit_gam <- mgcv::gam(price ~ s(yearsold) + s(mileage), data = autotrader)
p_gam <- predict(fit_gam, newdata = data.frame(yearsold = mean(autotrader$yearsold), 
                                               mileage = mileage.xx))
lines(mileage.xx, p_gam, lwd = 2, col = 'green3')

legend(
  'topright',
  c("GAM fit", "Transformed linear fit"),
  lwd = 2,
  col = c("green3", "slateblue"),
  bty = "n"
  )

## Years Old effect
yo.t <- predict(yearsoldBN, newdata = yearsold.xx)
c2 <- coef(fit4)["yearsold.t"]

plot(
    jitter(autotrader$yearsold, 1.5),
    autotrader$price,
    pch = 16,
    col = grey(.1, alpha = .2), 
    main = "Years old effect (re-transformed to original unit)",
    xlab = "Age (Jittered)",
    ylab = "Price"
)
line_vals <- yo.t * c2 + coef(fit4)[1]
lines(
    yearsold.xx,
    y = predict(priceBN, newdata = line_vals, inverse = TRUE),
    lwd = 2,
    col = "slateblue"
)

# Compare to GAM fit
p_gam <- predict(fit_gam, newdata = data.frame(yearsold = yearsold.xx, 
                                               mileage = mean(autotrader$mileage)))
lines(yearsold.xx, p_gam, lwd = 2, col = 'green3')

legend(
  'topright',
  c("GAM fit", "Transformed linear fit"),
  lwd = 2,
  col = c("green3", "slateblue"),
  bty = "n"
  )

