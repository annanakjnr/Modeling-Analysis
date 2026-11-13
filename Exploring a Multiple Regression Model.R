# Exploring a Multiple Regression Model

# Loading the dataset
housing <- read.csv("ANA 510 week 3 activity housing_prices (1).csv")

# Exploring the dataset 
head(housing)

# Checking the summary statistics of the dataset 
summary(housing)

# Checking for missing columns
colSums(is.na(housing))

# Checking for duplicate rows
sum(duplicated(housing))


# Building the Regression Models
# Simple regression: price and square footage
model_sqft <- lm(house_price ~ sqft_living, data = housing)
summary(model_sqft)

# Simple regression: price and bedrooms
model <- lm(house_price ~ num_bedrooms, data = housing)
summary(model)


# Multiple regression: price, square footage, and bedrooms
model_multi <- lm(house_price ~ sqft_living + num_bedrooms, data = housing)
summary(model_multi)

# Comparing R-squared values
r2_table <- data.frame(
  model = c("Simple: sqft_living", "Simple: num_bedrooms", "Multiple: both"),
  r_squared = c(summary(model_sqft)$r.squared,
                summary(model)$r.squared,
                summary(model_multi)$r.squared),
  adj_r_squared = c(summary(model_sqft)$adj.r.squared,
                    summary(model)$adj.r.squared,
                    summary(model_multi)$adj.r.squared)
)
r2_table


# Scatterplots with regression lines
# Price vs square footage
plot(housing$sqft_living, housing$house_price,
     main = "Price vs Square Footage",
     xlab = "Square Feet", ylab = "House Price", pch = 19)
abline(model_sqft, col = "red")

# Price vs bedrooms
plot(housing$num_bedrooms, housing$house_price,
     main = "Price vs Bedrooms",
     xlab = "Bedrooms", ylab = "House Price", pch = 19)
abline(model, col = "red")

# Actual vs predicted price from the multiple regression
plot(fitted(model_multi), housing$house_price,
     main = "Multiple Regression: Actual vs Predicted",
     xlab = "Predicted Price", ylab = "Actual Price", pch = 19)
abline(0, 1, col = "red")
