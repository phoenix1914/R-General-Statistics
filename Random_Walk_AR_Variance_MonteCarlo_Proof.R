t <- 1000 # The sample size of the model
n <- 50 # The repeated VARs we want to obtain and average
ts_df <- data.frame(matrix(ncol = n, nrow = (t)))
acfs <-0
{
  {
    start.time <- Sys.time()
    for (x in 1:n) {
      yt <- 0
      et <- rnorm(t, mean = 0, sd = 0.6)  # Generating error terms
      yt <- et[1] # Initializing the time series
      ts_df[1, x] = yt[1]
      
      #Generating the time series
      for (i in 2:t) {
        yt[i] <- et[i]-(et[i-1]/2)
        ts_df[(i), x] = yt[i]
      }
      
      
    }
    
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
}

#Different metrics for the time series
mean(apply(ts_df, 1, var)) # The mean of the var for each observation
plot(apply(ts_df, 1, var)) # Takes rows and applys the var and plots them
var(apply(ts_df, 2, mean)) # The variation of the mean of the time series
for (z in 1:n) {
  acfs[z] <- acf(ts_df[z], lag.max = 2, plot = F)$acf[2]
}
mean(acfs)


#Graphs of all the time series simulations
{
  library(ggplot2)
  library(reshape2)
  
  
  ts_df <- t(ts_df)
  rownames(ts_df) <- paste("trial", seq(n), sep = "")
  colnames(ts_df) <- paste("time", seq(t), sep = "")
  
  dat <- as.data.frame(ts_df)
  dat$trial <- rownames(dat)
  mdat <- melt(dat, id.vars = "trial")
  mdat$time <- as.numeric(gsub("time", "", mdat$variable))
  
  
  ggplot(mdat, aes(x = time, y = value, group = trial)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_line(size = 0.2, alpha = 0.1)
}
