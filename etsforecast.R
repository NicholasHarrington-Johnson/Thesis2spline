library(forecast)

# Suppose we want a 7 day forecast:
# Time series approach using at least 2 weeks of data

# Suppose we have data to end of 2013 and we forecast next six months
# Convert to weekly data to capture seasonality.

x <- ts(window(totpeople, end=2013.999), frequency=7)

plot(x)

fit <- ets(x)

plot(forecast(fit),include=50)
lines(fitted(fit),col='red')
# Ugly. We need to ensure prediction intervals stay positive.

# Using log model

logpeople <- ts(window(log(totpeople+1), end=2013.999), frequency=7)

plot(logpeople)

logfit <- ets(logpeople)

plot(forecast(logfit),include=50)
lines(fitted(logfit),col='red')

# Look at in-sample forecasts:
plot(x)
lines(fitted(fit), col='red')

scatter.smooth(x,fitted(fit),pch=".",
               xlab="Actual",ylab="Forecast")
abline(0,1,col='gray')

# In-sample one-step forecast errors:
res <- residuals(fit)
plot(res)
abline(0,0,col='gray')

scatter.smooth(x,res,pch=".",
               xlab="Forecasts",ylab="Forecast error")
abline(0,0,col='gray')

# {log} Look at in-sample forecasts:
plot(logpeople)
lines(fitted(logfit), col='red')

scatter.smooth(logpeople,fitted(logfit),pch=".",
               xlab="Actual",ylab="Forecast")
abline(0,1,col='gray')

# In-sample one-step forecast errors:
logres <- residuals(logfit)
plot(logres)
abline(0,0,col='gray')

scatter.smooth(logpeople,logres,pch=".",
               xlab="Forecasts",ylab="Forecast error")
abline(0,0,col='gray')

# Can you find a better model that is not so biased for large days
# and where the lower PI bound is non-negative?

