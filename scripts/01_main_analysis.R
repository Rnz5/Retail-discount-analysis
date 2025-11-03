
#Required packages
library(tidyverse)
library(logistf)

#Dataset: extracted from: https://www.kaggle.com/datasets/ahmadrazakashif/shopping-behavior-dataset/discussion?sort=hotness
data = read.csv("shopping_behavior_updated.csv")

#Transform variable "Discount Applied" from "Yes or No" to binary "1 or 0"
data$Discount_Applied = ifelse(data$Discount.Applied == "Yes", 1, 0)


#Data Observation
print("Data overview:\n")
cat("Total observations:", nrow(data), "\n")
cat("Discount rate:", mean(data$Discount_Applied) * 100, "%\n\n")

#Initial model pre variable selection
model = glm(Discount_Applied ~ Age + Gender + Category + Purchase.Amount..USD. + Location + Season + Review.Rating + Subscription.Status + Shipping.Type + Previous.Purchases + Payment.Method + Frequency.of.Purchases, data = data, family = binomial)

#Test of significance
print("Test of significance")
print("Ho: β₁ = β₂ = ... = βn = 0") 
print("H1: At least one of βn ≠ 0")
print("alpha = 0.05")

#Operations
chi2 = model$null.deviance - model$deviance
cat("Chi-square:", chi2, "\n")

degrees_freedom = model$df.null - model$df.residual
cat("Degrees of freedom:", degrees_freedom, "\n")

p_value = 1-pchisq(chi2,degrees_freedom)
cat("P-value:", format.pval(p_value), "\n\n")

#Conclusion
if(p_value < 0.05) {
  print("Decision: Reject H₀\n")
  print("There is sufficient evidence to conclude that the model has significant predictive power")
} else {
  print("Decision: Fail to reject H₀\n")
  print("There is insufficient evidence to conclude that the model has significant predictive power")
}

#Variable selection, method step "backwards"
refined_model <- stepAIC(model, direction="backward")
summary(refined_model)

#Discount distribution by selected variables
cat("Discount Distribution:\n")

print(table(Gender = data$Gender, Discount = data$Discount_Applied))
print(table(Subscription = data$Subscription.Status, Discount = data$Discount_Applied))

#Build model with Firth's correction (complete separation presented in the dataset)
firth_model <- logistf(Discount_Applied ~ Gender + Subscription.Status, data = data)

print("Firth's model (handles complete separation):\n")
summary(firth_model)

#Odds ratios and percentage changes
odds_ratios <- exp(firth_model$coefficients)
percentage_changes <- (odds_ratios - 1) * 100

#Interpretation
print("Business Interpretation:\n")
for(i in 1:length(percentage_changes)) {
  var_name <- names(percentage_changes)[i]
  change <- percentage_changes[i]
  
  if(change > 0) {
    cat(var_name, ": +", round(change, 1), "% increase\n")
  } else {
    cat(var_name, ": ", round(change, 1), "% decrease\n")
  }
}


#Possible prediction scenarios
scenarios <- data.frame(Scenario = c("Male Subscriber", "Female Subscriber", "Male Non-Subscriber", "Female Non-Subscriber"), Gender = c("Male", "Female", "Male", "Female"), Subscription.Status = c("Yes", "Yes", "No", "No"))

#Calculate probabilities
scenarios$Probability <- predict(firth_model, newdata = scenarios, type = "response")

#Results
print("Discount Probability Predictions:\n")
for(i in 1:nrow(scenarios)) {
    cat(sprintf("%-20s: %5.1f%%\n", scenarios$Scenario[i], scenarios$Probability[i] * 100))
  }

#Final Comments
print("- Complete separation detected in discount allocation\n")
print("- Gender and subscription status are near-perfect predictors\n")
print("- Firth's method provides stable coefficient estimates\n")
print("- Model reveals strong patterns in discount distribution\n")

