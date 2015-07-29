## Call all file
rm(list=ls())

## Loading packages

source("packages.R")

## Reading data

eztable <- fread("booking_top30_restaurants_all.txt")

## Loading functions

source("function.R")

## Public Holidays
phols <- fread("Public_Holidays_Scaled.csv")
phols <- readph(phols)

## Bookings

r <- clean(eztable)

## Truncating data

tr <- truncatep(r)

## Plotting

for(i in 1:16)
{
  plotpub(tr[[i]]$X0,tr[[i]]$pubd,tr[[i]]$pubi,tr[[i]]$pubny,i)
}

## Best overall model

out <- bmod(tr)
print(out)

## Modelling using booking numbers from 1 weeks ago

for (i in 1:16)
{
  arimah(tr[[i]],7)
}

## Multiple previous data points with splines
h <- c(1,7)
k <- c(18,30)
aicc <- choose_k(h,k,tr[[3]])
print(aicc)

## atm using choose_k with only one input but it would be easier to get optim to recognise multiple inputs and only change one. investigate when this works

optim(10,choose_k)
