library(tidyverse)

##Reading and preparing data -----------------------------------------------------------------------------------------------------------------------
# Load Data
data = read.delim("data/Basketball.txt", header = TRUE, sep = "\t", dec = ".")

##Team points to wins ------------------------------------------------------------------------------------------------------------------------------
## Logistic Regression of Basketball Team Points to Wins
# Build the logit glm object
point_win = glm(Win ~ TeamPoints, data = data, family = binomial (link = 'logit'))
summary(point_win)

# Calculate LogOdds, Odds, and Probability Space arrays
point_win_logodds = point_win$coefficients[1]+point_win$coefficients[2]*data$TeamPoints
point_win_odds = exp(point_win_logodds)
point_win_probs = point_win_odds/(1+point_win_odds)

# Graph arrays
ggplot(mapping = aes(x = data$TeamPoints, y = point_win_logodds)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Team Points Win Ratio: LogOdds", x = "Team Points", y = "LogOdds")

ggplot(mapping = aes(x = data$TeamPoints, y = point_win_odds)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Team Points Win Ratio: Odds", x = "Team Points", y = "Odds")

ggplot(mapping = aes(x = data$TeamPoints, y = point_win_probs)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Team Points Win Ratio: Probabilities", x = "Team Points", y = "Probabilities")

## Team Points to Wins Regression Assessment and Analysis
# Deviance
point_win_deviance = with(point_win, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Hit Table
point_win_hit_table = table(data$Win, point_win$fitted.values > 0.5)
point_win_hit_table
sum(diag(point_win_hit_table)) / sum(point_win_hit_table)

# LogOdds Probability Ratio
ggplot(mapping = aes(x = point_win_probs, y = point_win_logodds)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Team Points Win Ratio: LogOdds Probability", x = "Probabilities", y = "LogOdds")


# TODO add analysis summary

# TODO Note for team members. Fouls and Assists might be good to check next. We can later combine whatever's significant into a single regression analysis or even the predictive model below (did it for fun, might delete).  


##Three point shots to wins -----------------------------------------------------------------------------------------------------------------------
## Logistic Regression of Basketball threepoint shots to Wins
# Build the logit glm object
x3_win = glm(Win ~ X3PointShots, data = data, family = binomial (link = 'logit'))
summary(x3_win)

# Calculate LogOdds, Odds, and Probability Space arrays
x3_win_logodds = x3_win$coefficients[1]+x3_win$coefficients[2]*data$X3PointShots
x3_win_odds = exp(x3_win_logodds)
x3_win_probs = x3_win_odds/(1+x3_win_odds)

# Graph arrays
ggplot(mapping = aes(x = data$X3PointShots, y = x3_win_logodds)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Threepoint shots Win Ratio: LogOdds", x = "Threepoint shots", y = "LogOdds")

ggplot(mapping = aes(x = data$X3PointShots, y = x3_win_odds)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Threepoint shots Win Ratio: Odds", x = "Threepoint shots", y = "Odds")

ggplot(mapping = aes(x = data$X3PointShots, y = x3_win_probs)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Threepoint shots Win Ratio: Probabilities", x = "Threepoint shots", y = "Probabilities")

## Team Points to Wins Regression Assessment and Analysis
# Deviance
x3_win_deviance = with(x3_win, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Hit Table
x3_win_hit_table = table(data$Win, x3_win$fitted.values > 0.5)
x3_win_hit_table
sum(diag(x3_win_hit_table)) / sum(x3_win_hit_table)

# LogOdds Probability Ratio
ggplot(mapping = aes(x = x3_win_probs, y = x3_win_logodds)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Threepoint shots Win Ratio: LogOdds Probability", x = "Probabilities", y = "LogOdds")


##Assists to wins -----------------------------------------------------------------------------------------------------------------------
## Logistic Regression of assists to wins
# Build the logit glm object
assist_win = glm(Win ~ Assists, data = data, family = binomial (link = 'logit'))
summary(assist_win)

# Calculate LogOdds, Odds, and Probability Space arrays
assist_win_logodds = assist_win$coefficients[1]+assist_win$coefficients[2]*data$Assists
assist_win_odds = exp(assist_win_logodds)
assist_win_probs = assist_win_odds/(1+assist_win_odds)

# Graph arrays
ggplot(mapping = aes(x = data$Assists, y = assist_win_logodds)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Assist Win Ratio: LogOdds", x = "Assists", y = "LogOdds")

ggplot(mapping = aes(x = data$Assists, y = assist_win_odds)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Assist Win Ratio: Odds", x = "Assists", y = "Odds")

ggplot(mapping = aes(x = data$Assists, y = assist_win_probs)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Assist Win Ratio: Probabilities", x = "Assists", y = "Probabilities")

## Assists to Wins Regression Assessment and Analysis
# Deviance
assist_win_deviance = with(assist_win, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# Hit Table
assist_win_hit_table = table(data$Win, assist_win$fitted.values > 0.5)
assist_win_hit_table
sum(diag(assist_win_hit_table)) / sum(assist_win_hit_table)

# LogOdds Probability Ratio
ggplot(mapping = aes(x = assist_win_probs, y = assist_win_logodds)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Assist Win Ratio: LogOdds Probability", x = "Probabilities", y = "LogOdds")


##Predictive Model --------------------------------------------------------------------------------------------------------------------------------
## For fun turn it into a predictive model
# split into train and test data
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ] 

# create model
model = glm(Win ~ TeamPoints, data = train, family = binomial (link = 'logit'))
# test model fit
pscl::pR2(model)["McFadden"]  # Scores consistently under 0.40, not a good model fit. Still Continuing

# TODO the following tests multicolinearity, can check if we add other independent varaiables to model
# car::vif(model)

# make predictions
predicted <- predict(model, test, type="response")
predicted