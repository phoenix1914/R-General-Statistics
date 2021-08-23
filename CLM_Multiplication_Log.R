big <-replicate(10000,prod(1+runif(12,0,0.5)))
small <-replicate(10000,prod(1+runif(1000,0,0.001)))

dens(big)
dens(small)
