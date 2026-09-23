# Personal Budget and Expense Analyzer
# This program demonstrates basic R programming concepts.

# ---------------------------------------------------------
# 1. Create different R data types
# ---------------------------------------------------------

# Character data type
user_name <- "Taiye"

# Numeric data type
monthly_budget <- 1000

# Integer data type
number_of_expenses <- 10L

# Logical data type
budget_active <- TRUE

# Factor data type
expense_type <- factor(c("Essential", "Optional"))

# Complex data type
complex_number <- 5 + 2i

# ---------------------------------------------------------
# 2. Create expense data
# ---------------------------------------------------------

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

cat("User:", user_name, "\n\n")

print(expenses)

# ---------------------------------------------------------
# 3. Functions
# ---------------------------------------------------------

# Calculate the total amount spent.
calculate_total <- function(amounts) {
  return(sum(amounts))
}

# Calculate the average expense.
calculate_average <- function(amounts) {
  return(mean(amounts))
}

# Find the highest spending category.
find_highest_category <- function(data) {
  totals <- aggregate(
    Amount ~ Category,
    data = data,
    FUN = sum
  )

  highest <- totals$Category[
    which.max(totals$Amount)
  ]

  return(highest)
}

# ---------------------------------------------------------
# 4. Use the functions
# ---------------------------------------------------------

total_spending <- calculate_total(expenses$Amount)

average_expense <- calculate_average(expenses$Amount)

highest_category <- find_highest_category(expenses)

# Find the highest category amount
category_totals <- aggregate(
  Amount ~ Category,
  data = expenses,
  FUN = sum
)

highest_amount <- max(category_totals$Amount)

# ---------------------------------------------------------
# 5. Display summary
# ---------------------------------------------------------

cat("\nSUMMARY\n")
cat("=======\n")

cat(
  "Total spending: $",
  total_spending,
  "\n",
  sep = ""
)

cat(
  "Average expense: $",
  round(average_expense, 2),
  "\n",
  sep = ""
)

cat(
  "Highest spending category: ",
  highest_category,
  " ($",
  highest_amount,
  ")\n",
  sep = ""
)

# ---------------------------------------------------------
# 6. Use a loop with a list
# ---------------------------------------------------------

budget_information <- list(
  Budget = monthly_budget,
  TotalSpent = total_spending,
  AverageExpense = average_expense,
  HighestCategory = highest_category
)

cat("\nBUDGET INFORMATION\n")
cat("==================\n")

for (item in names(budget_information)) {
  cat(
    item,
    ": ",
    budget_information[[item]],
    "\n",
    sep = ""
  )
}

# ---------------------------------------------------------
# 7. Check the budget
# ---------------------------------------------------------

remaining_budget <- monthly_budget - total_spending

cat("\nBUDGET STATUS\n")
cat("=============\n")

if (remaining_budget >= 0) {
  cat(
    "Remaining budget: $",
    remaining_budget,
    "\n",
    sep = ""
  )
} else {
  cat(
    "Budget exceeded by: $",
    abs(remaining_budget),
    "\n",
    sep = ""
  )
}

# ---------------------------------------------------------
# 8. Use case_when
# ---------------------------------------------------------

# Classify each expense according to its amount.
expenses$SpendingLevel <- dplyr::case_when(
  expenses$Amount < 50 ~ "Low",
  expenses$Amount < 100 ~ "Medium",
  TRUE ~ "High"
)

cat("\nEXPENSE LEVELS\n")
cat("==============\n")

print(expenses)

# ---------------------------------------------------------
# 9. Create category totals
# ---------------------------------------------------------

category_totals <- aggregate(
  Amount ~ Category,
  data = expenses,
  FUN = sum
)

cat("\nSPENDING BY CATEGORY\n")
cat("====================\n")

print(category_totals)

# ---------------------------------------------------------
# 10. Create a bar chart
# ---------------------------------------------------------

barplot(
  category_totals$Amount,
  names.arg = category_totals$Category,
  main = "Spending by Category",
  xlab = "Category",
  ylab = "Amount Spent",
  las = 2
)

# ---------------------------------------------------------
# 11. Final message
# ---------------------------------------------------------

cat("\nAnalysis complete.\n")
cat("Thank you for using the Personal Budget and Expense Analyzer!\n")
