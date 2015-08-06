## This file contains all necessary functions

##########################################################
##########################################################

## Function to read in data

readfunction <- function(rstnum,startdate=0,enddate=9999,h=7)
{ 
  # Remove weird numbers. 
  # Negative people, or more than 10000 people, in a single booking.
  # Probably errors
  eztable <- filter(eztable, people > 0, people < 1e4)
  
  # Remove cancellations
  eztable <- filter(eztable, status!="canceled" & status!='no-show')
  
  # List of restaurants in order of popularity
  restaurants <- rev(sort(table(eztable[,restaurant_id])))
  
  # Just look at one restaurant. Could be any of them.
  # Pick the second most popular as an example
  r2 <- subset(eztable, restaurant_id==names(restaurants)[rstnum])
  
  # Convert reservation and booking dates to numerical days
  res <- as.timeDate(r2$reservation_datetime)
  firstres <- min(res)
  res <- as.numeric(res)
  book <- as.numeric(as.timeDate(r2$booking_date))
  
  # Remove rows where booking is after reservation
  j <- (book <= res)
  r2 <- filter(r2, j)
  book <- book[j]
  res <- res[j]
  
  # Create daily totals. i.e., number of people booked for each day
  # Probably much easier ways to do this, but I can't think of them.
  # b = number of discrete bookings
  # p = number of people booked
  res <- as.numeric(res)
  book <- as.numeric(book)
  book <- book - min(res) + 1
  res <- res - min(res) + 1
  b <-  matrix(0L, nrow=max(res), ncol=max(res-book+1))
  rownames(b) <- paste("Day",1:nrow(b))
  colnames(b) <- paste(0:(ncol(b)-1))
  p <- b
  for(i in 1:nrow(b))
  {
    tmp <- filter(r2, res==i) # Bookings for day firstres+i-1.
    if(nrow(tmp)>0)
    {
      bk <- i - book[res==i] # days from reservation to booking
      daystores <- tabulate(bk+1) # Add one so zeros are counted
      b[i,1:length(daystores)] <- daystores
      daystores <- tabulate(rep(bk,tmp$people)+1) # Add one so zeros are counted
      p[i,1:length(daystores)] <- daystores
    }
  }
  
  # Remove head rows with zero bookings (prior to restaurant using system)
  firstnonzero <- min(which(rowSums(b)>0))
  # Remove tail rows with zero bookings
  lastnonzero <- max(which(rowSums(b)>0))
  # Remove some additional rows before all bookings available
  b <- b[firstnonzero:lastnonzero,]
  p <- p[firstnonzero:lastnonzero,]
  
  # Compute cumulative reservations
  # i.e., reservations
  cumB <- t(apply(b,1,function(x){rev(cumsum(rev(x)))}))
  cumP <- t(apply(p,1,function(x){rev(cumsum(rev(x)))}))
  
  # Column 1 represents time series of total bookings
  # Rows represent cumulating reservations for each day.
  
  # Rearrange matrix so each row contains possible predictors.
  # Each row contains bookings available at that date.
  B <- cumB
  P <- cumP
  for(j in 1:(nrow(B)-1))
  {
    if(j < nrow(B)-1)
    {
      zb <- diag(cumB[(j+1):nrow(cumB),2:ncol(cumB)])
      zp <- diag(cumP[(j+1):nrow(cumP),2:ncol(cumP)])
    }
    else
    {
      zb <- cumB[(j+1):nrow(cumB),2:ncol(cumB)]
      zp <- cumP[(j+1):nrow(cumP),2:ncol(cumP)]
    }
    B[j,2:ncol(B)] <- c(zb, rep(NA, ncol(B)-length(zb)-1))
    P[j,2:ncol(P)] <- c(zp, rep(NA, ncol(P)-length(zp)-1))
  }
  
  #################################################################
  
  # Date Stuff
  start <- as.Date(firstres)
  yr <- as.numeric(substr(start,1,4))
  day <- as.numeric(start - as.Date(paste(as.character(yr),"-01-01",sep="")))
  
  ## public holiday stuff
  
  pubd <- window(phols$pubd,start = yr+ (day/365),end = yr+ ((day+nrow(P)-1)/365))
  pubi <- window(phols$pubi,start = yr+ (day/365),end = yr+ ((day+nrow(P)-1)/365))
  pubny <- window(phols$pubny,start = yr + (day/365),end = yr+ ((day+nrow(P)-1)/365))
  
  #################################################################
  
  # Return time series  
  obj <- data.frame(P,pubd,pubi,pubny)
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Truncata truncates data

truncata <- function(frame, prop=0.05,prope=0.05)  
{
  
  # Values at lower and upper 5%
  
  lower <- round(as.numeric(quantile(time(frame$X0),prop)),digits=0)
  upper <- round(as.numeric(quantile(time(frame$X0),1-prope)),digits=0)
  
  # Keep tsp for pubd
  
  starting <- tsp(frame$pubd)[1]+((lower-1)/365)
  ending <- starting+((upper-lower)/365)
  
  # Assign new dimensions
  
  data1 <- frame[lower:upper,1:15]
  data2<-matrix(0L,(nrow(data1)-14),15)
  for (i in 1:15){
    data2[,i] <- data1[(15-i+1):(nrow(data1)-i+1),paste("X",toString(i-1),sep="")]
  }
  
  colnames(data2)<-paste("b_t",((1:15)-1),sep="")
  
  data2 <- cbind(data2,frame[(lower+14):upper,c("pubd","pubi","pubny")])
  starting <- starting+(14/365)
  data2$pubd <- ts(data2$pubd,start=starting,frequency=365)
  data2$pubi <- ts(data2$pubi,start=starting,frequency=365)
  data2$pubny <- ts(data2$pubny,start=starting,frequency=365)
  
  #if (tsp(data2$pubd)[2]!=ending) {print("Error on the dimensions front")}
  
  return(data2)
  
}


##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## truncating based on manual inspection

truncatep <- function(r)
{
  tr <- list()
  
  # Truncate 5% plus any additional manual inspection requires
  prop <- rep(0.05,30)
  prope <- prop
  # Manual Inspection
  prope[1] <- 0.06
  prope[5] <- 0.09
  prope[8] <- 0.08
  prope[10] <- 0.12
  prope[15] <- 0.1
  prope[20] <- 0.35
  prope[22] <- 0.07
  prope[23] <- 0.16
  prope[25] <- 0.18
  # Truncating data
  for(i in numr)
  {
    tr[[i]]<-truncata(r[[i]], prop[i],prope[i])
  }
  return(tr)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################
clean <- function(eztable)
{
  # Read in data for resaurants 1 to 30
  r <- list()
  
  for(i in 1:30)
  {
    print(paste("reading restaurant",i))
    r[[i]] <- readfunction(i)
  }
  # Plot all data
  for(i in 1:30)
  {
    plot.ts(r[[i]]$X0, main=paste("Restaurant",i))
  }
  return(r)  
}
##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Arimamodel applies different arima models to data
arimamodel <- function(tri, h=14){
  # This function employs 5 different models that use a log of the total people attending a restaurant
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  totpeople <- tri$b_t0
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  # Create x regressor public holiday dummies
  
  xdums <- cbind(as.numeric(tri$pubd),as.numeric(tri$pubi),as.numeric(tri$pubny))
  
  colnames(xdums) <- c("going down","going up","ny")
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions - 2011 - 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  ispubh <- ts(pholt %in% phols, start=2011, frequency=365)
  
  # Dimensions - start when y series ends
  end <- tail(time(totpeople),n=1)
  
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
  
  # Create matrix of public holidays for forecasting
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny))
  
  colnames(xfor) <- c("going down","going up","ny")
  
  xny <- as.numeric(fpubny)
  
  #########################################
  ##### Forecasting with Models 1-5 #######
  #########################################
  
  #########################################
  
  ## Model 1
  
  # Arima fit no public holidays
  
  fit1 <- auto.arima(logpeople)
  
  # Arima fit1 forecast
  
  fc1 <- forecast(fit1,h=h)
  fc1$mean <- exp(fc1$mean)-1
  fc1$lower <- exp(fc1$lower)-1
  fc1$upper <- exp(fc1$upper)-1
  fc1$x <- tri$b_t0
  tsp(fc1$x) <- tsp(tri$pubd)
  fc1$mean <- ts(fc1$mean, start = tsp(fc1$x)[2]+1/365, frequency=365)
  tsp(fc1$upper) <- tsp(fc1$lower) <- tsp(fc1$mean)
  plot(fc1,main="Regular arima model")
  
  #########################################  
  
  ## Model 2
  
  # Arima fit with public holidays
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- tri$b_t0
  tsp(fc2$x) <- tsp(tri$pubd)
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  plot(fc2,main="Arima model with public holidays")
  
  #########################################
  
  ## Model 3
  
  # Arima fit to capture regular (weekly) fluctuations in bookings # Some restaurants close regularly
  
  fit3 <- auto.arima(logpeople, xreg=seasonaldummy(logpeople), seasonal=FALSE)
  
  # Arima fit3 forecast
  
  fc3 <- forecast(fit3,xreg=seasonaldummyf(logpeople,h))
  fc3$mean <- exp(fc3$mean)-1
  fc3$lower <- exp(fc3$lower)-1
  fc3$upper <- exp(fc3$upper)-1
  fc3$x <- tri$b_t0
  tsp(fc3$x) <- tsp(tri$pubd)
  fc3$mean <- ts(fc3$mean, start = tsp(fc3$x)[2]+1/365, frequency=365)
  tsp(fc3$upper) <- tsp(fc3$lower) <- tsp(fc3$mean)
  plot(fc3,main="arima with regular seasonal fluctuation")  
  
  #########################################  
  
  ## Model 4
  
  # Arima fit with seasonality and public holidays
  
  fit4 <- auto.arima(logpeople, xreg=cbind(seasonaldummy(logpeople), xdums), seasonal=FALSE)
  
  # Arima fit4 forecast
  
  fc4 <- forecast(fit4,xreg=cbind(seasonaldummyf(logpeople,h),xfor[1:h,]))
  fc4$mean <- exp(fc4$mean)-1
  fc4$lower <- exp(fc4$lower)-1
  fc4$upper <- exp(fc4$upper)-1
  fc4$x <- tri$b_t0
  tsp(fc4$x) <- tsp(tri$pubd)
  fc4$mean <- ts(fc4$mean, start = tsp(fc4$x)[2]+1/365, frequency=365)
  tsp(fc4$upper) <- tsp(fc4$lower) <- tsp(fc4$mean)
  plot(fc4,main="arima with regular seasonal fluctuation and holiday effects")  
  
  #########################################
  
  ## Model 5
  
  #   Arima fit with seasonality and new year
  fit5 <- auto.arima(logpeople, xreg=cbind(seasonaldummy(logpeople), as.numeric(tri$pubny)), seasonal=FALSE)
  
  # Arime fit5 forecast
  
  fc5 <- forecast(fit5,xreg=cbind(seasonaldummyf(logpeople,h),xny[1:h]))
  fc5$mean <- exp(fc5$mean)-1
  fc5$lower <- exp(fc5$lower)-1
  fc5$upper <- exp(fc5$upper)-1
  fc5$x <- tri$b_t0
  tsp(fc5$x) <- tsp(tri$pubd)
  fc5$mean <- ts(fc5$mean, start = tsp(fc5$x)[2]+1/365, frequency=365)
  tsp(fc5$upper) <- tsp(fc5$lower) <- tsp(fc5$mean)
  plot(fc5,main="arima with regular seasonal fluctuation and ny effects") 
  
  ################################
  
  ## Preparing output for "best model"
  
  modelwin <- data.frame(matrix(0,nrow=1,ncol=6))
  colnames(modelwin) <- c("arima_plain","arima_pubs","arima_season","arima_season_pubs","arima_season_ny","ETS")
  
  # Model 1
  fcw1 <- modelwin
  fcw1[1] <- 1
  # Model 2
  fcw2 <- modelwin
  fcw2[2] <- 1
  # Model 3
  fcw3 <- modelwin
  fcw3[3] <- 1
  # Model 4
  fcw4 <- modelwin
  fcw4[4] <- 1
  # Model 5
  fcw5 <- modelwin
  fcw5[5] <- 1
  
  ############################
  
  ## Finalising output of best model
  
  best <- fit1
  modelwin <- fcw1
  if(fit2$aicc < best$aicc){
    best <- fit2
    modelwin <- fcw2
  } else if(fit3$aicc < best$aicc){
    best <- fit3
    modelwin <- fcw3
  } else if(fit4$aicc < best$aicc){
    best <- fit4
    modelwin <- fcw4
  }  else if(fit5$aicc < best$aicc) {
    best <- fit5
    modelwin <- fcw5
  }
  obj <- list(modelwin,best)
  
  return(obj)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Plotting function

plotpub <- function(frame,name)
{
  ## This function plots the total people attending a restaurant booking with public holidays data
  # Public holidays expected to increase the number of people are marked as blue 
  # Public holidays expected to decrease the number of people are marked as red
  # New Year's Eve is marked as green
  
  y <- ts(frame$b_t0,frequency =365)
  tsp(y) <- tsp(frame$pubd)
  xd <- frame$pubd
  xu <- frame$pubi
  xny <- frame$pubny
  
  logpd <- ts(as.logical(xd))
  tsp(logpd) <- tsp(xd)
  
  logpu <- ts(as.logical(xu))
  tsp(logpu) <- tsp(xd)
  
  logpny <- ts(as.logical(xny))
  tsp(logpny) <- tsp(xd)
  
  plot(y,main=paste("Restaurant",name),  xlab="Year", ylab="Total people booked")
  points(time(logpd)[logpd],(y)[logpd],col="red",pch=19)
  points(time(logpu)[logpu],(y)[logpu],col="blue",pch=19)
  points(time(logpny)[logpny],(y)[logpny],col="green",pch=19)
}
##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

## Winning model function
bmod <- function(tr)
{
  # Defining variables to write to
  rmodels <- list()
  rwins <- list()
  
  # Modeling all restaurants with 5 different models
  # Loop returns model with lowest AICC
  for (i in numr)
  {
    rmodels[[i]] <- arimamodel(tr[[i]])
    rwins[[i]] <- rmodels[[i]][[1]]
    print(paste("restaurant",i,"modelling complete"))
  }
  
  # Loop shows which model performed best overall
  rwintot <- rwins[[1]]
  for (i in numr[-1])
  {
    rwintot <- rwins[[i]]+rwintot
  }
  rwintot <- rwintot/29
  
  return(rwintot)
}


##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

arimah <- function(tri,h=7)
{
  ## This function employs an arima model with public holidays and booking information from one unique time period ago (i.e. 14 days)
  
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  totpeople <- tri$b_t0
  
  tsp(totpeople) <- tsp(tri$pubd)
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(totpeople+1), start=1, frequency=7)
  
  # Create x regressor public holiday dummies
  
  xdums <- cbind(as.numeric(tri$pubd),as.numeric(tri$pubi),as.numeric(tri$pubny),tri[(h+1)])
  
  colnames(xdums) <- c("going down","going up","ny","previous_bookings1")
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions - 2011 - 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  ispubh <- ts(pholt %in% phols, start=2011, frequency=365)
  
  # Dimensions - start when y series ends
  endw <- tail(time(totpeople),n=h+1)
  # Bookings information
  fph1 <- ts(tri[(h+1)],start=tsp(tri$pubd)[1],end=tsp(tri$pubd)[2],frequency = 365)
  # Begin at end[[1]] of y series
  fph1 <- window(fph1,start=endw[[1]]+(1/365))
  
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
  
  xfor <- cbind(as.numeric(fpubd),as.numeric(fpubi),as.numeric(fpubny),as.numeric(fph1))
  
  colnames(xfor) <- c("going down","going up","ny","previous_bookings1")
  
  #########################################################
  
  fit2 <- auto.arima(logpeople, xreg=xdums)
  
  # Arima fit2 forecast
  
  fc2 <- forecast(fit2,xreg=xfor[1:h,], h=h)
  fc2$mean <- exp(fc2$mean)-1
  fc2$lower <- exp(fc2$lower)-1
  fc2$upper <- exp(fc2$upper)-1
  fc2$x <- ts(tri$b_t0,frequency=365)
  tsp(fc2$x)<-tsp(tri$pubd)
  fc2$x <- window(fc2$x,end=tsp(tri$pubd)[2])
  fc2$x <- window(fc2$x,start=tsp(tri$pubd)[1])
  fc2$mean <- ts(fc2$mean, start = tsp(fc2$x)[2]+1/365, frequency=365)
  tsp(fc2$upper) <- tsp(fc2$lower) <- tsp(fc2$mean)
  plot(fc2,main=paste("Arima model with public holidays and bookings (h=",toString(h),")"))
  return(fit2)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

choose_k1k1h <- function(k,P,h=7)
  ## This function returns the AICC for a given knot using ONE LINEAR spline in an arima model using public holidays and previous bookings data
{  
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  # Creating log of data with weekly frequency
  
  if (k>=max(P[(h+1)])|k<1){
    return(1e20)
  }
  
  logpeople <- ts(log(P$b_t0+1), start=1, frequency=7)
  
  # Create splinetastics
  splinek0 <- P[(h+1)]
  splinetastic1 <- splinek0-k
  splinetastic1[splinetastic1<0]=0
  
  ########################################################
  
  #splinetastic2 <- P[(h2+1)]-k[2]
  #splinetastic2[splinetastic2<0]=0
  
  #########################################################
  
  xdums <- cbind(as.numeric(P$pubd),as.numeric(P$pubi),as.numeric(P$pubny),splinek0,splinetastic1)
  
  colnames(xdums) <- c("going down","going up","ny",paste("b_t",toString(h),sep=""),paste("spline with knot",toString(k)))
  
  #########################################################
  
  fit <- auto.arima(logpeople, xreg=xdums)
  
  aicc <- fit$aicc
  return(aicc)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################

choose_k2k1h <- function(k,P,h=7)
  ## This function returns the AICC for TWO given knots using ONE LINEAR spline in an arima model using public holidays and previous bookings data
{  
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################
  
  # Check that k is in order
  if (k[1]>=k[2] | k[1]<1){return(1e20)}

  
  if (k[1]>=max(P[(h+1)])){
    return(1e20)
  }
  
  if (k[2]>=max(P[(h+1)])){
    return(1e20)
  }
  
  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(P$b_t0+1), start=1, frequency=7)
  
  # Create splinetastics
  splinek0 <- P[(h+1)]
  splinetastic1 <- splinek0-k[1]
  splinetastic1[splinetastic1<0]=0
  
  ########################################################
  
  splinetastic2 <- splinek0-k[2]
  splinetastic2[splinetastic2<0]=0
  
  #########################################################
  
  xdums <- cbind(as.numeric(P$pubd),as.numeric(P$pubi),as.numeric(P$pubny),splinek0,splinetastic1,splinetastic2)
  
  colnames(xdums) <- c("going down","going up","ny",
                       paste("b_t",toString(h),sep=""),
                       paste("spline with knot",toString(k[1])),paste("spline with knot",toString(k[2])))
  
  #########################################################
  
  fit <- auto.arima(logpeople, xreg=xdums)
  
  aicc <- fit$aicc
  return(aicc)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################


readph <- function(phols){
  
  # Change public holiday dates to numeric
  nphols <- as.numeric(as.timeDate(phols$Date))
  
  # Create time series public holiday variable with appropriate dimensions
  # Dimensions hardcoded 2011 to end 2015
  pholt <- as.numeric(seq(as.Date("2011-01-01"),as.Date("2015-12-31"),by="1 day"))
  
  # Public Holidays with suspected decreases
  pubd <- nphols[which(phols$Holiday=="1")]
  pubd <- ts(as.numeric(pholt %in% pubd), start=2011,frequency = 365)
  
  # Public Holidays with suspected increases
  pubi <- nphols[which(phols$Holiday=="2")]
  pubi <- ts(as.numeric(pholt %in% pubi), start=2011,frequency = 365)
  
  # New Years Eve - suspected increases
  pubny <- nphols[which(phols$Holiday=="3")]
  pubny <- ts(as.numeric(pholt %in% pubny),start=2011,frequency = 365)
  
  obj <- data.frame(pubd,pubi,pubny)
}

##########################################################
##########################################################
#################### End of Function #####################
##########################################################
##########################################################
