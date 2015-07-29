arima2h <- function(first_tri,h1=2,h2=5)
{
  ## This function employs an arima model with public holidays and booking information from one unique time period ago (i.e. 14 days)
  
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  # Using training set of length n-h
  if (h1<h2){
    n <- dim(first_tri)[1]
    tri <- first_tri[1:(n-h1),]
  } else {
    n <- dim(first_tri)[1]
    tri <- first_tri[1:(n-h2),]
  }
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(tri$totpeople+1), start=1, frequency=7)
  
  # Create x regressor public holiday dummies
  
  xdums <- cbind(as.numeric(tri$pubd),as.numeric(tri$pubi),as.numeric(tri$pubny),as.numeric(tri$ph1),as.numeric(tri$ph2))
  
  colnames(xdums) <- c("going down","going up","ny","previous_bookings1","previous_bookings2")
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions - 2011 - 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  ispubh <- ts(pholt %in% phols, start=2011, frequency=365)
  
  # Dimensions - start when y series ends
  endw <- tail(time(first_tri$totpeople),n=h+1)
  
  ############################################################################
  
  # Bookings information
  fph1 <- first_tri$ph1
  # Begin at end[[1]] of y series
  fph1 <- window(fph1,start=endw[[1]]+(1/365))

  # Bookings information
  fph2 <- first_tri$ph2
  # Begin at end[[1]] of y series
  fph2 <- window(fph2,start=endw[[1]]+(1/365))
  
  ############################################################################
  
  ## end window for remaining forecasts
  enddata <- tail(time(fph1),n=1)
  
  fispubh <- window(ispubh,start=endw[[1]]+(1/365),end=enddata)
  # Public Holidays with suspected decreases
  fpubd <- nphols[which(phols$Holiday=="1")]
  fpubd <- ts(as.numeric(pholt %in% fpubd), start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubd <- window(fpubd,start=endw[[1]]+(1/365),end=enddata)
  # Public Holidays with suspected increases
  fpubi <- nphols[which(phols$Holiday=="2")]
  fpubi <- ts(as.numeric(pholt %in% fpubi), start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubi <- window(fpubi,start=endw[[1]]+(1/365),end=enddata)
  # New Years Eve - suspected increases
  fpubny <- nphols[which(phols$Holiday=="3")]
  fpubny <- ts(as.numeric(pholt %in% fpubny),start=2011,frequency = 365)
  # Begin at end[[1]] of y series
  fpubny <- window(fpubny,start=endw[[1]]+(1/365),end=enddata)
  
  # Create matrix of public holidays for forecasting
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),as.numeric(fph))
  
  colnames(xfor) <- c("going down","going up","ny","previous_bookings")
  
  #########################################################
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- first_tri$totpeople
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  plot(fc2,main=paste("Arima model with public holidays and bookings (h=",toString(h),")"))
  return(fit2)
}