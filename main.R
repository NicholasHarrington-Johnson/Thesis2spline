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
numr <- c(1:30)[-17]
r <- clean(eztable)

## Truncating data

tr <- truncatep(r)

## Plotting

for(i in numr)
{
  plotpub(tr[[i]],i)
}

## Best overall model

out <- bmod(tr)
print(out)

## Modelling using booking numbers from 1 weeks ago

for (i in numr)
{
  nam <- paste("Restaurant_", i, sep = "")
  assign(nam, arimah(tr[[i]],7))
}

## Splines

# One knot
# Using restaurant 1
k1 <- 15
k1 <- optim(k1,choose_k1k1h,"Nelder Mead",tr[[1]])

# Two knots
k2 <- c(14,158)
k2 <- optim(k2,choose_k2k1h,"Nelder Mead",tr[[1]])

if (k1$value < k2$value){
  print(paste("One knot is better than two, found using knots at",toString(k1$par)))
} else {
  print(paste("Two knots are better than one, found using knots at",toString(k2$par)))
}
