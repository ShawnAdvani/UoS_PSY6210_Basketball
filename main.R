# Load Data
data = read.delim("data/Basketball.txt", header = TRUE, sep = "\t", dec = ".")

## Logistic Regression of Basketball Team Points to Wins
# Build the logit glm object
point_win = glm(Win ~ TeamPoints, data = data, family = binomial (link = 'logit'))
summary(point_win)

# Calculate LogOdds, Odds, and Probability Space arrays
point_win_logodds = point_win$coefficients[1]+point_win$coefficients[2]*data$TeamPoints
point_win_odds = exp(point_win_logodds)
point_win_probs = point_win_odds/(1+point_win_odds)

# Graph arrays
plot(data$TeamPoints, point_win_logodds, type = 'p', col = 'red', 
     main='Team Points Win Ratio: LogOdds', xlab='Team Points', ylab='LogOdds')
plot(data$TeamPoints, point_win_odds, type = 'p', col = 'green', 
     main='Team Points Win Ratio: Odds', xlab='Team Points', ylab='Odds')
plot(data$TeamPoints, point_win_probs, type = 'p', col = 'blue', 
     main='Team Points Win Ratio: Probabilities', xlab='Team Points', ylab='Probabilities')

## Team Points to Wins Regression Assessment and Analysis
# Deviance
point_win_deviance = with(point_win, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Hit Table
point_win_hit_table = table(data$Win, point_win$fitted.values > 0.5)
point_win_hit_table
sum(diag(point_win_hit_table)) / sum(point_win_hit_table)

# LogOdds Probability Ratio
plot(point_win_probs, point_win_logodds, type = 'p', col = 'purple',
     main = 'Team Points Win Ratio: LogOdds Probability', xlab = 'Probabilities', ylab = 'LogOdds')

# TODO change all plots to ggplot
# TODO add analysis summary




# TODO Note for team members. Fouls and Assists might be good to check next. We can later combine whatever's significant into a single regression analysis or even the predictive model below (did it for fun, might delete).  




## For Fun Turn It Into A Predictive Model
# split into train and test data
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ] 

model = glm(Win ~ TeamPoints, data = train, family = binomial (link = 'logit'))
pscl::pR2(model)["McFadden"]  # Scores consistently under 0.40, not a good model fit. Still Continuing

# TODO the following tests multicolinearity, can check if we add other independent varaiables to model
# car::vif(model)

predicted <- predict(model, test, type="response")
predicted
