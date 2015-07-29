y <- ts(rep(0,1000000),start=2011,end=2015,frequency=365)

for (i in 1:30)
  {
  x <- r[[i]]$totpeople
  x <- x/mean(x)
  y <- y + x
}
y <- y/5

pubs <- r[[1]]$ispubh

pubs <- window(pubs,start=tsp(y)[1])
pubs <- window(pubs,end=tsp(y)[2],frequency=365)

pubd <- r[[1]]$pubd

pubd <- window(pubd,start=tsp(y)[1])
pubd <- window(pubd,end=tsp(y)[2],frequency=365)

pubi <- r[[1]]$pubi

pubi <- window(pubi,start=tsp(y)[1])
pubi <- window(pubi,end=tsp(y)[2],frequency=365)

pubny <- r[[1]]$pubny

pubny <- window(pubny,start=tsp(y)[1])
pubny <- window(pubny,end=tsp(y)[2],frequency=365)

plotpub(y,pubd,pubi,pubny,"all restaurants")
points(time(pubs)[pubs],(y)[pubs],col="black",pch=4)
