#####################################
### FUNCTIONS TO CALCULATE STREAKS OF SHOTS MADE VERSUS EXPECTED STREAKS GIVEN BERNOULLI DISTRIBUTION
# aka 'the hot hand'
#### by tim abraham


# source all code and use the bottom part as an example to play around 
# data comes from www.basketballgeek.com/data

# load data
load(url("http://dl.dropbox.com/u/6132890/hot_hand_data.RData"))
require(ggplot2)

# function to convert outcomes into a count of streaks. 
### example: streaks('Stephen Curry')
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
  colnames(tab)<- c('streak', 'count')
  tab->>streak_count
}

# simulating a player's season from the bernoulli dist

# calculate historical probability of make and number of attempts
### example historicals('Stephen Curry')
### will output a list of three objects to variable historicals.list
historicals = function(name){
  df <- subset(gsw.shots, player == name)
  p.hat = with(df, sum(outcome)/length(outcome)) 
  n = with(df, length(outcome))
  sims = 1000
  season.matrix = matrix(NA, nrow = sims, ncol = n) # build a matrix 1000 rows for 1000 seasons and n columns for the sequence of shots
  for(i in 1:sims){
    sim_incrementer = rbinom(n, 1, p = p.hat) # simulate one season from the binomial using empirical p
    season.matrix[i,] = sim_incrementer # now we loop through all 1000 seasons
  }
  list(data.frame(p.hat = p.hat, n = n), season.matrix, name)->>historical.list
  
}


# function similiar to streaks but takes matrix as input
streaks_theoretical = function(outcome) {
  x <-c(0)
  cur.run <- 0 # initialize length of current streak
  l <- length(outcome)
  for(i in 2:l){
    if(outcome[i]==1) cur.run = cur.run + 1 
    else cur.run = 0
    
    x<-c(x,cur.run) # replaces 1s in streaks with the number of successive makes
  }
  # now we want just the highest number of each streak
  streak.num <- 0
  y <- c()
  l1 <- length(x)
  for(j in 2:l1){
    if(x[j-1]>x[j]) streak.num = x[j-1] # looks for breaks in streaks and returns the number prior to the break
    else streak.num = 0
    
    y = c(y, streak.num)
  }
  return(as.data.frame(table(y[y>0]))) # tables frequencies of streaks and coerces into a data frame
}

hot_hand = function(streak_count, historical.list) {
t(apply(historical.list[[2]], 1, FUN = streaks_theoretical)) -> results # apply function to simulated matrix
results <- rbind.fill(results) # coherce to data frame
results_expected <- aggregate(Freq ~ Var1, data = results, function(x) sum(x)/1000) # find average of all seasons
colnames(results_expected) <- c(names(streak_count)[1], 'expected_count') # rename columns to match with real results
results_expected[,2] <- as.numeric(as.character(results_expected[,2])) # convert factors to numeric
streak_count[,1] <- as.numeric(as.character(streak_count[,1])) # same as above
hot_hand_result <<- merge(streak_count, results_expected) # now merge both together so you can compare
return(hot_hand_result)

}

##################################
### EXAMPLE: Anthony Morrow
##################################
# 1. source code through line 96
streaks('Anthony Morrow')
### outputs it as data frame to streak_count
historicals('Anthony Morrow')
### outputs it as a list to historical.list
hot_hand(streak_count, historical.list)


# plot the performance
ggplot(hot_hand_result, aes(streak, count - expected_count)) + 
  geom_bar(stat = 'identity',position = 'dodge', fill = 'steelblue', color = 'yellow') + 
  labs(x = 'Streak of makes', y = 'Performance above or below expected') + 
  opts(title = historical.list[[3]])
