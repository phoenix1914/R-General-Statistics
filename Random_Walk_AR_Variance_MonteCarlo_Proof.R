t <- 1000  # The sample size of the model
n <- 1000  # The repeated iterations we want to take for each observation, to find VAR

vars_df <- data.frame(matrix(ncol=n,nrow=(t)))
{
start.time <- Sys.time()
for (x in 1:n){

yt <- 0
et <- rnorm(t, mean = 0, sd =0.5)  # Generating error terms
yt <- et[1] # Initializing the time series
vars_df[1,x] = yt[1]

#Generating the time series
for (i in 2:t) {
  yt[i] <- 0.8 * yt[i-1]+et[i]   # Remove the factor completely if you want Random Walk instead of AR
  vars_df[(i),x] = yt[i]
}


}

end.time <- Sys.time()
time.taken <- end.time - start.time

}

time.taken
apply(vars_df, 1, var)
plot(apply(vars_df, 1, var))
