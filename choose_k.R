choose_k <- function(k)
  ## This function returns the AICC for a given knot using ONE LINEAR spline in an arima model using public holidays and previous bookings data
{  
  # Zeros have been accounted for by adding 1 to the data
  # Data has weekly frequency
  #########################################
  ##### Creating and Organising Data ######
  #########################################

  # Creating log of data with weekly frequency
  
  logpeople <- ts(log(data$totpeople+1), start=1, frequency=7)
  
  # Create splinetastic
  
  splinetastic <- data$ph-k
  splinetastic[splinetastic<0]=0
  #########################################################
  
  xdums <- cbind(as.numeric(data$pubd),as.numeric(data$pubi),as.numeric(data$pubny),as.numeric(splinetastic))
  
  colnames(xdums) <- c("going down","going up","ny","spline")
  
  #########################################################
  
  fit <- auto.arima(logpeople, xreg=xdums)
    
  aicc <- fit$aicc
  return(aicc)
}