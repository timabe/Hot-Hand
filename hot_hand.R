# load data
load(url("http://dl.dropbox.com/u/6132890/hot_hand_data.RData"))


# function to convert outcomes into a count of streaks. Arguments 
streaks <- function(name) {
  df <- subset(gsw.shots, player == name)
  x <- c(0)
  cur.run <- 0 # initialize length of current streak
  l <- length(df$outcome)
  for(i in 2:l){
    if(df$outcome[i]==1) cur.run <- cur.run + 1 
    else cur.run <- 0
    
    x <- c(x,cur.run) # replaces 1s in streaks with the number of successive makes
  }
  # now we want just the highest number of each streak
  streak.num <- 0
  y <- c()
  l1 <- length(x)
  for(j in 2:l1){
    if(x[j-1] > x[j]) streak.num <- x[j-1] # looks for breaks in streaks and returns the number prior to the break
    else streak.num <- 0
    
    y <- c(y, streak.num)
  }
  tab <- as.data.frame(table(y[y>0]), dnn= name) # tables frequencies of streaks and coerces into a data frame
  colnames(tab)<- c(name, 'count')
  return(tab)
}