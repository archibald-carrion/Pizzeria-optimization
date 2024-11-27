# Load necessary library
# If necessary run: install.packages("dplyr")
library(dplyr)

# Read the CSV files
# CHANGE the path for the one with the file pizza_extra_costs.csv
extra_costs <- read.csv("pizza_extra_costs.csv")
# CHANGE the path for the one with the file pizza_cost_only_ingredients.csv
ingredient_costs <- read.csv("pizza_cost_only_ingredients.csv")

# Calculate the total extra costs per size
total_extra_costs <- extra_costs %>%
  summarise(
    small_extra = sum(small_cost),
    medium_extra = sum(medium_cost),
    large_extra = sum(large_cost)
  )

# Add the extra costs to the pizza ingredient costs
total_costs <- ingredient_costs %>%
  mutate(
    total_small_cost = avg_small_cost + total_extra_costs$small_extra,
    total_medium_cost = avg_medium_cost + total_extra_costs$medium_extra,
    total_large_cost = avg_large_cost + total_extra_costs$large_extra
  ) %>%
  select(pizza_name, total_small_cost, total_medium_cost, total_large_cost)

# Save a csv in your computer, CHANGE path for the one that works for you
write.csv(total_costs, "total_costs.csv", row.names = FALSE)

# Print the results
print(total_costs)

