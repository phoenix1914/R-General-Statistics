library(rethinking)

w <- 6
n <- 9

p_grid <- seq (from =0, to = 1, length.out = 1000)
posterior <- dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)
s_posterior <- sum(posterior)
posterior <- posterior / sum (posterior)

dens(posterior)
plot(posterior)

# definegrid
p_grid <-seq(from=0,to=1,length.out=20)
# defineprior
prior <-rep(1,20)
# computelikelihoodateachvalueingrid
likelihood <-dbinom(6,size=9,prob=p_grid)
# computeproductoflikelihoodandprior
unstd.posterior <-likelihood*prior
# standardizetheposterior,soitsumsto1
posterior <-unstd.posterior/sum(unstd.posterior)
dens(posterior)

plot( p_grid,posterior,type="b",
      xlab="probability ofwater",ylab="posteriorprobability")
mtext( "20points")