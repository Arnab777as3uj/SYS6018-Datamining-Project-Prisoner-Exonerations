# Import packages
library(tree)
library(stringr)
library(randomForest)
library(dplyr)


# Read in data
datat = read.csv("cleanex.csv")

# We first need to limit our factor levels for each
# variable down to 32 (since that's how many our package
# can work with)

# First create empty vector for seeing how many factor 
# levels each variable has
numlevels = numeric(length(names(datat)))

# Loop through all variables in dataframe then store 
# the number of factor levels in numlevels vector
i = 1
for(name in names(datat)) {
  
  numlevels[i] = length(unique(datat[, name]))
  i = i + 1

}

# Looking at our variables, we'll remove first and
# last names since we're not looking for any correlation
# yet between names and CTE.
datat = datat[!names(datat) %in% c('Last.Name', 'First.Name')]

# Convert Age variable to numeric
datat$Age = as.numeric(datat$Age)


# We next address our State column. It has many factor
# levels, so we'll likely have to aggregate some values
# later into a sort of "Other" level (based on the number
# of entries), but for now, we combine all Fed entries
# into their state entry (e.g. "Fed-VA" -> "Virginia")

# Create empty ab vector for storing state abbreviations
# in our "Fed-XX" entries, and testvec for storing values
# to replace our datat$State later
ab = character(length(datat$State))
testvec = character(length(datat$State))


# Iterate through all our State entries
for(j in 1:length(datat$State)) {
  
  # Store value of entry
  value = datat$State[j]
  
  # If "Fed" is in the string, store 
  # following substring in ab vector
  if(str_detect(value, "Fed")) {
    ab[j] = str_sub(value, 5:length(value)) 
    
    if(ab[j] == 'DC') {
      ab[j] = 'District of Columbia'
    }
    else if(ab[j] != 'Military') {
      state = state.name[match(ab[j], state.abb)]
      ab[j] = state
    }
    
  }
  
  if(!is.na(ab[j]) & ab[j] != "") {
    testvec[j] = ab[j]
  }
  else {
    testvec[j] = as.character(value)
  }
  
}


# Looking at the number of unique values in our testvec,
# we see we have 54, and we need only to reduce it by one
# more value to work with the randomForest package. We 
# see that our non-state State values of Puerto Rico and Guam
# actually only number 7, so we combine them into a "Territory"
# value.

ig = which(testvec == 'Guam')
ip = which(testvec == 'Puerto Rico')
testvec[c(ig, ip)] = 'Territory'
datat$State = testvec


# We'll perform the same technique as above on counties

# Store the names of the top 52 counties
top_counties = names(sort(table(datat$County), decreasing = TRUE))[1:52]

# Initialize counties vector for safe modifying
counties = as.character(datat$County)

# Iterate through all counties
for(i in 1:length(counties)) {
  
  # If value is not in our top counties, change it to 'Other'
  if(!counties[i] %in% top_counties) {
    counties[i] = 'Other'
  }
}

# Replace County column with our counties vector
datat$County = as.factor(counties)



# Do the same for Sentence variable

# Store the names of the top 52 sentences
top_sentences = names(sort(table(datat$Sentence), decreasing = TRUE))[1:52]

# Initialize counties vector for safe modifying
sentences = as.character(datat$Sentence)

# Iterate through all counties
for(i in 1:length(sentences)) {
  
  # If value is not in our top counties, change it to 'Other'
  if(!sentences[i] %in% top_sentences) {
    sentences[i] = 'Other'
  }
}

# Replace County column with our counties vector
datat$Sentence = as.factor(sentences)



# Drop the Exonerated column because we don't want to
# use both of the only two variables directly involved 
# in the calculation of our target in our model

datat = datat[!names(datat) %in% c('Exonerated')]


# Convert all character columns to factors
data_fac = datat %>% mutate_if(is.character, as.factor)


# Run a random forest
tree.datat = randomForest(CTE ~ ., data_fac, importance = TRUE)
tree.datat
summary(tree.datat$mse)


# EDA revealed a possible increase in accuracy for a log 
# transform of our response, so we'll try that out
logy = log(datat$CTE + 1)
data_logy = data_fac
data_logy$CTE = logy

log.tree = randomForest(CTE ~ ., data_logy, importance = TRUE)
log.tree
summary(log.tree$mse)
# Judging by the relative distributions of the MSEs in log
# transformed and unmodified data, there doesn't seem to be
# much difference, with maybe a lean toward the unmodified.
# We'll continue our analysis on both

# We do need to run these on training and testing data, though,
# so we create indices and run them below


# Creating training indices
train = sample(nrow(datat), 1712)

tree.datat = randomForest(CTE ~ ., data_fac[train,], importance = TRUE)
tree.datat

log.tree = randomForest(CTE ~ ., data_logy[train,], imoprtance = TRUE)
log.tree

# Reduction in R2 for both trees. Hopefully a payoff with 
# statistics on testing sets

par(mfrow = c(1,2))

yhat.bag = predict(tree.datat, newdata = data_fac[-train,])
plot(yhat.bag, data_fac[-train,]$CTE)
abline(0,1)
mean((yhat.bag - data_fac[-train,]$CTE)^2)
reg_R2 = cor(yhat.bag, data_fac[-train,]$CTE)^2
reg_R2
# [1] 0.8836

yhat.log = predict(log.tree, newdata = data_logy[-train,])
plot(yhat.log, data_logy[-train,]$CTE)
abline(0,1)
mean((yhat.log - data_logy[-train,]$CTE)^2)
log_R2 = cor(yhat.log, data_logy[-train,]$CTE)^2
log_R2
# [1] 0.8797

# We get another metric telling us that our unmodified 
# model performs better than our log, this time on unseen
# data. Further analysis will be done only on the unmodified
# response for ease of interpretation.

# Just to make sure, let's try different values of mtry for
# our forest
p = ncol(datat) - 1

# p = 31. Let's do 3 more variable at each increment,
# starting at 3

# Initialize vectors
mtries = seq(3,30,3)
test_MSE_vals = numeric(10)
R2_vals = numeric(10)

# Iterate through each number in mtries, increasing i by 1
i = 1
for(num in mtries) {
  
  # Generate tree, calculate predictions, and get MSE and R2
  test.tree = randomForest(CTE ~ ., data_fac[train,], mtry = num, importance = TRUE)
  yhat = predict(test.tree, newdata = data_fac[-train,])
  MSE = mean((yhat - data_fac[-train,]$CTE)^2)
  R2 = cor(yhat, data_fac[-train,]$CTE)^2
  
  # Store MSE and R2 values
  test_MSE_vals[i] = MSE
  R2_vals[i] = R2
  
  i = i + 1
}

cbind(mtries, test_MSE_vals, R2_vals)
#      mtries test_MSE_vals   R2_vals
# [1,]      3      26.31266 0.7033269
# [2,]      6      24.30948 0.7152597
# [3,]      9      24.44669 0.7080531
# [4,]     12      24.12510 0.7090889
# [5,]     15      24.56226 0.7015151
# [6,]     18      24.21927 0.7043049
# [7,]     21      24.48743 0.6996774
# [8,]     24      24.63958 0.6964839
# [9,]     27      24.70754 0.6946609
# [10,]    30      24.53913 0.6967140

# We see that our MSE values are all very close at
# mtry > 3. And we see our R2 actually tends toward
# lower values at higher mtry. So we'll stick with mtry = 6

tree.imp = randomForest(CTE ~ ., data_fac[train,], mtry = 6, importance = TRUE)
yhat.imp = predict(tree.imp, newdata = data_fac[-train,])
MSE.imp = mean((yhat.imp - data_fac[-train,]$CTE)^2)
R2.imp = cor(yhat.imp, data_fac[-train,]$CTE)^2

# We'll also try boosting, just to check, though our
# current results already seem promising.
library(gbm)
boost.exo = gbm(CTE ~ ., data_fac[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

summary(boost.exo)
# We see that, as one may intuit, Sentence, State, County, and
# Convicted are the most important factors for this model.

yhat.boost = predict(boost.exo, newdata = data_fac[-train,], n.trees = 5000)
mean((yhat.boost - data_fac[-train,]$CTE)^2)
# [1] 1070.021
# This is much larger than our MSE on non-boosted models, so
# we won't explore tuning the lambda parameter.

# We end at our final model, MSE, and R2
model.final = tree.imp
MSE.final = MSE.imp
R2.final = R2.imp

MSE.final
# [1] 24.41353
R2.final
# [1] 0.7145317