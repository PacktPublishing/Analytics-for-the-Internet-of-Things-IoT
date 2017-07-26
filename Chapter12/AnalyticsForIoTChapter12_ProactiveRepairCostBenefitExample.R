#make sure all needed packages are installed
if(!require(caret)){
  install.packages("caret")
}
if(!require(pROC)){
  install.packages("pROC")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(data.table)){
  install.packages("data.table")
}

#Load required libraries
library(caret)
library(pROC)
library(dplyr)
library(data.table)

#Generate sample data
simdata = function(N=1000) {
  #simulate 4 features
  X = data.frame(replicate(4,rnorm(N))) 
  #create a hidden data structure to learn
  hidden = X[,1]^2+sin(X[,2]) + rnorm(N)*1 
  #10% TRUE, 90% FALSE
  rare.class.probability = 0.1
  #simulate the true classification values
  y.class = factor(hidden<quantile(hidden,c(rare.class.probability))) 
  return(data.frame(X,Class=y.class))
}

#make some data structure
model_data = simdata(N=50000)

#train a logistic regression model on the simulated data
training <- createDataPartition(model_data$Class, p = 0.6, list=FALSE)

trainData <- model_data[training,]
testData <- model_data[-training,]

glmModel <- glm(Class~ . , data=trainData, family=binomial)
testData$predicted <- predict(glmModel, newdata=testData, type="response")

#calculate AUC
roc.glmModel <- pROC::roc(testData$Class, testData$predicted)
auc.glmModel <- pROC::auc(roc.glmModel)
print(auc.glmModel)

#Pull together test data and predictions
simModel <- data.frame(trueClass = testData$Class,
                     predictedClass = testData$predicted)

# Reorder rows and columns
simModel <- simModel[order(simModel$predictedClass, decreasing = TRUE), ]
simModel <- select(simModel, trueClass, predictedClass)

simModel$rank <- 1:nrow(simModel)


#Assign costs for failures and proactive repairs
proactive_repair_cost <- 253   # Cost of proactively repairing a part
failure_repair_cost  <- 5000   # Cost of a failure of the part (include all costs such as lost production, etc not just the repair cost)

# Define each predicted/actual combination
fp.cost <- proactive_repair_cost # The part was predicted to fail but did not (False Positive)
fn.cost <- failure_repair_cost  # The part was not predicted to fail and it did (False Negative)
tp.cost <- (proactive_repair_cost - failure_repair_cost) # The part was predicted to fail and it did (True Positive). This will be negative for a savings.
tn.cost <- 0.0                     # The part was not predicted to fail and it did not (True Negative)

#incorporate probability of future failure
simModel$future_failure_prob <- prob_failure

#Funtion to assign costs for each instance
assignCost <- function(pred, outcome, tn.cost, fn.cost, fp.cost, tp.cost, prob){
  
  cost <- ifelse(pred == 0 & outcome == FALSE, tn.cost, # No cost since no action was taken and no failure
                     ifelse(pred == 0 & outcome == TRUE, fn.cost, # The cost of no action and a repair resulted
                            ifelse(pred == 1 & outcome == FALSE, fp.cost, # The cost of proactive repair which was not needed
                                   ifelse(pred == 1 & outcome == TRUE, tp.cost, 999999999))))   # The cost of proactive repair which avoided a failure
  return(cost)
}


# Initialize list to hold final output
master <- vector(mode = "list", length = 100)

#use the simulated model. In practice, this code can be adapted to compare multiple models
test_model <- simModel

# Create a loop to increment through dynamic threshold (starting at 1.0 [no proactive repairs] to 0.0 [all proactive repairs])
threshold <- 1.00
for (i in 1:101) {
  
  #Add predicted class with percentile ranking
  test_model$prob_ntile <- ntile(test_model$predictedClass, 100) / 100
  
  # Dynamically determine if proactive repair would apply based on incrementing threshold
  test_model$glm_failure <- ifelse(test_model$prob_ntile >= threshold, 1, 0)
  
  test_model$threshold <- threshold
  
  # Compare to actual outcome to assign costs
  test_model$glm_impact <- assignCost(test_model$glm_failure, test_model$trueClass, tn.cost, fn.cost, fp.cost, tp.cost, test_model$future_failure_prob)
  
  # Compute cost for not doing any proactive repairs
  test_model$nochange_impact <- ifelse(test_model$trueClass == TRUE, fn.cost, tn.cost) # * test_model$future_failure_prob)
  
  # Running sum to produce the overall impact
  test_model$glm_cumul_impact <- cumsum(test_model$glm_impact) / nrow(test_model)
  test_model$nochange_cumul_impact <- cumsum(test_model$nochange_impact) / nrow(test_model)
  
  # Count the # of classified failures
  test_model$glm_failure_ct <- cumsum(test_model$glm_failure)
  
  # Create new object to house the one row per iteration output for the final plot
  master[[i]] <- test_model[nrow(test_model),]
  
  # Reduce the threshold by 1% and repeat to calculate new value
  threshold <- threshold - 0.01
}

finalOutput <- rbindlist(master)
finalOutput <- subset(finalOutput, 
                      select = c(threshold, 
                                 glm_cumul_impact,
                                 glm_failure_ct,
                                 nochange_cumul_impact)
)

# Set baseline to costs of not doing any proactive repairs
baseline <- finalOutput$nochange_cumul_impact

# Plot the cost curve
par(mfrow = c(2,1))
plot(row(finalOutput)[,1],
     finalOutput$glm_cumul_impact,
     type = "l",
     lwd = 3,
     main = paste("Net Costs: Proactive Repair Cost of $", proactive_repair_cost, ", Failure cost $", failure_repair_cost, sep = ""),
     ylim = c(min(finalOutput$glm_cumul_impact) - 100, 
              max(finalOutput$glm_cumul_impact) + 100),
     xlab = "Percent of Population",
     ylab = "Net Cost ($) / Unit")

# Plot the cost difference of proactive repair program and a 'do nothing' approach
plot(row(finalOutput)[,1],
     baseline - finalOutput$glm_cumul_impact,
     type = "l",
     lwd = 3,
     col = "black",
     main = paste("Savings: Proactive Repair Cost of $", proactive_repair_cost, ", Failure cost $", failure_repair_cost,sep = ""),
     ylim = c(min(baseline - finalOutput$glm_cumul_impact) - 100, 
              max(baseline - finalOutput$glm_cumul_impact) + 100),
     xlab = "% of Population",
     ylab = "Savings ($) / Unit")
abline(h=0,col="gray")
