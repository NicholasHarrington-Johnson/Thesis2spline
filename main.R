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
save(tr,file="truncated_data.Rda")

load("truncated_data.Rda")
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
k1_19 <- 15
k1_19 <- optim(k1_19,choose_k1k1h,"Nelder Mead",tr[[19]])

# Two knots
k2_19 <- c(14,50)
k2_19 <- optim(k2_19,choose_k2k1h,"Nelder Mead",tr[[19]])

if (k1_19$value < k2_19$value){
  print(paste("One knot is better than two, found using knots at",toString(k1_19$par)))
} else {
  print(paste("Two knots are better than one, found using knots at",toString(k2_19$par)))
}

rstnum <-7
plotpub(tr[[rstnum]],rstnum)
out <- mseevaluate(tr[[rstnum]],starttraining=400)

print(out[[1]])
plotmse(out[[2]])
