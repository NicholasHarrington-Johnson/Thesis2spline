## Select restaurant 4

restnum <-4 
h=14
rest <- tr[[restnum]]

## plot restaurant and public holidays into pdf
pdf(paste("Restaurant",restnum,".pdf"),width=16/2.54,height=10/2.54)
plotpub(rest$totpeople,rest$pubd,rest$pubi,rest$pubny,restnum)
dev.off()

## Forecast

logpeople <- ts(log(rest$totpeople+1), start=1, frequency=7)

# Create x regressor public holiday dummies

xdums <- cbind(as.numeric(rest$pubd),as.numeric(rest$pubi),as.numeric(rest$pubny))

colnames(xdums) <- c("going down","going up","ny")

# Change public holiday dates to numeric
nphols <- as.numeric(as.timeDate(phols$Date))

# Create time series public holiday variable with appropriate dimensions
# Dimensions - 2011 - 2015
pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))

ispubh <- ts(pholt %in% phols, start=2011, frequency=365)

# Dimensions - start when y series ends
end <- tail(time(rest$totpeople),n=1)

fispubh <- window(ispubh,start=end+(1/365))
# Public Holidays with suspected decreases
fpubd <- nphols[which(phols$Holiday=="1")]
fpubd <- ts(as.numeric(pholt %in% fpubd), start=2011,frequency = 365)
# Begin at end of y series
fpubd <- window(fpubd,start=end+(1/365))
# Public Holidays with suspected increases
fpubi <- nphols[which(phols$Holiday=="2")]
fpubi <- ts(as.numeric(pholt %in% fpubi), start=2011,frequency = 365)
# Begin at end of y series
fpubi <- window(fpubi,start=end+(1/365))
# New Years Eve - suspected increases
fpubny <- nphols[which(phols$Holiday=="3")]
fpubny <- ts(as.numeric(pholt %in% fpubny),start=2011,frequency = 365)
# Begin at end of y series
fpubny <- window(fpubny,start=end+(1/365))

# Create marestx of public holidays for forecasting

xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny))

colnames(xfor) <- c("going down","going up","ny")

xny <- as.numeric(fpubny)

# Arima fit with public holidays

fit2 <- auto.arima(logpeople, xreg=xdums)

# Arima fit2 forecast

fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
fc2$mean <- exp(fc2$mean)-1
fc2$lower <- exp(fc2$lower)-1
fc2$upper <- exp(fc2$upper)-1
fc2$x <- rest$totpeople
fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
pdf("Restaurant_4ARIMAForecast.pdf",width = 16/2.54,height = 10/2.54)
plot(fc2,main="Restaurant 4: Arima model with public holidays",include=70,  xlab="Year", ylab="Total people booked")
dev.off()
