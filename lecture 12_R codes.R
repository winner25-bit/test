

########## sampling distribution of odds ratio ###############


# set parameters
n_simulations<-1000
n_E <- 50  # total for exposed group (a + b = 50)
n_not_E <- 50  # total for non-exposed group (c + d = 50)
p_D_given_E <- 0.2  # probability of Disease given Exposure
p_D_given_not_E <- 0.2  # Probability of Disease given no Exposure

# initialize vector to store ORs
odds_ratios<-c()

# simulation
set.seed(123)  # for reproducibility
for (i in 1:n_simulations) {
  # generate 'a' and 'c' based on probabilities
  a <- rbinom(1, n_E, p_D_given_E) # rbinom(n, size, prob)
  b <- n_E - a  # Since a + b = n_E
  
  c <- rbinom(1, n_not_E, p_D_given_not_E)
  d <- n_not_E - c  # Since c + d = n_not_E
  
  # calculate OR (avoid division by zero by checking b and c)
  if (b > 0 & c > 0) { 
    OR <- (a * d) / (b * c)
  } else {
    OR <- NA  # Assign NA if OR can't be calculated
  }
  
  # Store the OR
  odds_ratios<-c(odds_ratios, OR)
}

# results
odds_ratios <- na.omit(odds_ratios)  # Remove any NAs
summary(odds_ratios)  # Summary of the odds ratios
hist(odds_ratios, main = "Distribution of Odds Ratios", xlab = "Odds Ratio", breaks=30) # no. of bins=30



####### transforming the odds ratios ##############

log_odds_ratios <- log(na.omit(odds_ratios))
# Plot histogram of log odds ratios
hist(log_odds_ratios, main = "Distribution of Log Odds Ratios", xlab = "Log Odds Ratio", breaks = 30)
summary(log_odds_ratios)










