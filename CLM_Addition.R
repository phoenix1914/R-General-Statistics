library(rethinking)
library(ggplot2)
library(reshape2)

pos <-replicate(1000,sum(runif(16,-1,1)))
hist(pos) + plot(density(pos))
{
n <- 1000
steps <- 34

my_df <- data.frame(matrix(0, ncol = n, nrow = 16))

for (i in 1 : n){
  w <- runif(steps, -1, 1)
  my_df[1,i] <- 0
  a <- w
  for (k in 2: (steps + 1)) {
     my_df[k,i] <- my_df[(k-1), i ] + w[k-1]
  }
  
}

my_df_last <- t(tail(my_df, n =1))
my_df$ind <- as.numeric(row.names(my_df))
my_df_m <- melt(my_df, id.vars="ind", value.name="value", variable.name="X")

}



rowMeans(my_df[steps,])

ggplot(my_df_m, aes(x = ind,y = value, group = X)) + geom_line()

hist(my_df_last)
plot(density(my_df_last))
       