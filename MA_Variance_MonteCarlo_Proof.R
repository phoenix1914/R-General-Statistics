t <- 1000  # The sample size of the model
n <- 50  # The repeated VARs we want to obtain and average
vars_df <- data.frame(matrix(ncol=n,nrow=(t)))

{
  start.time <- Sys.time()
  for (x in 1:n){
    
    yt <- 0
    et <- rnorm(t, mean = 0, sd =sqrt(0.5))  # Generating error terms
    yt <- (et[1] + et[2])/2 # Initializing the time series
    vars_df[1,x] = yt[1]
    
    #Generating the time series
    for (i in 2:t) {
      yt[i] <- (et[i]+et[(i-1)])/2
      vars_df[(i),x] = yt[i]
    }
    
    
  }
  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
}
time.taken

#Proof that the mean oscilates around zero   
mean(apply(vars_df, 1, var))
plot(apply(vars_df, 1, var))

#Graphs of the time series
{
  library(ggplot2)
  library(reshape2)
  
  vars_df <- t(vars_df)
  rownames(vars_df) <- paste("trial", seq(n), sep = "")
  colnames(vars_df) <- paste("time", seq(t), sep = "")
  
  dat <- as.data.frame(vars_df)
  dat$trial <- rownames(dat)
  mdat <- melt(dat, id.vars = "trial")
  mdat$time <- as.numeric(gsub("time", "", mdat$variable))
  
  
  ggplot(mdat, aes(x=time, y=value, group=trial)) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    geom_line(size=0.2, alpha=0.1)
}
