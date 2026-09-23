# Personal Budget and Expense Analyzer
# This program analyzes personal expenses by category.

# Create sample expense data
expenses <- data.frame(
  Category = c(
    "Food", "Transportation", "Rent", "Entertainment",
    "Food", "Transportation", "Utilities", "Entertainment",
    "Food", "Utilities"
  ),
  Amount = c(
    45, 30, 500, 60,
    35, 25, 80, 40,
    50, 75
  )
)

# Display the expense data
cat("PERSONAL BUDGET AND EXPENSE ANALYZER\n")
cat("====================================\n\n")

print(expenses)

# Calculate total spending
total_spending <- sum(expenses$Amount)

# Calculate average expense
average_expense <- mean(expenses$Amount)

# Calculate spending by category
category_totals <- aggregate(
  Amount ~ Category,
  data = expenses,
  FUN = sum
)

# Find the category with the highest spending
highest_category <- category_totals$Category[
  which.max(category_totals$Amount)
]

highest_amount <- max(category_totals$Amount)

# Display results
cat("\nSUMMARY\n")
cat("=======\n")
cat("Total spending: $", total_spending, "\n", sep = "")
cat("Average expense: $", round(average_expense, 2), "\n", sep = "")
cat(
  "Highest spending category: ",
  highest_category,
  " ($",
  highest_amount,
  ")\n",
  sep = ""
)

# Create a bar chart of spending by category
barplot(
  category_totals$Amount,
  names.arg = category_totals$Category,
  main = "Spending by Category",
  xlab = "Category",
  ylab = "Amount Spent",
  las = 2
)