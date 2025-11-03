
# 1. Discount Distribution by Gender
plot1 <- ggplot(data, aes(x = Gender, fill = factor(Discount_Applied))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkorange2", "deepskyblue2"), 
                    labels = c("No Discount", "Discount")) +
  labs(title = "Discount Distribution by Gender",
       y = "Proportion") +
  theme_minimal()

print(plot1)

# 2. Discount Distribution by Subscription
plot2 <- ggplot(data, aes(x = Subscription.Status, fill = factor(Discount_Applied))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkorange2", "deepskyblue2"),
                    labels = c("No Discount", "Discount")) +
  labs(title = "Discount Distribution by Subscription Status",
       y = "Proportion") +
  theme_minimal()

print(plot2)

# 3. Simple Results Table
results_table <- data.frame(
  Variable = c("Male vs Female", "Subscriber vs Non-Subscriber"),
  Odds_Ratio = round(odds_ratios[-1], 2),
  Percentage_Change = paste0(round(percentage_changes[-1], 1), "%")
)

print(results_table)

print("All graphs displayed in RStudio Plots tab\n")
