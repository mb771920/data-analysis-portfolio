library(tidyverse)
install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)
?mice 
# View the data and missing values
View(airquality)
airquality
# Plot missing data using VIM package
aggr(airquality)
# the number of missing data
nrow(na.omit(airquality))
nrow(airquality)

# Impute the missing data as shown in the video -------------------------------
airquality_imp <-  mice(airquality)

# view the attribute of the imputed data ----------------------------------
attributes(airquality_imp)

# Complete data -----------------------------------------------------------
airquality_comp <- complete(airquality_imp)
aggr(airquality_comp)
View(airquality_comp)

# visualise the data to compare data before and after imputation ------------
par(mfrow = c(2,2))
boxplot(airquality$Ozone, main = " Ozone before imputation")
boxplot(airquality_comp$Ozone, main = " Ozone after imputation")

# Run the t. test ---------------------------------------------------------

t.test(airquality$Ozone, airquality_comp$Ozone)
# from the t.test the p value is 0.7581 which above the 0.05 significance level, 
# so we fail to reject the null hypothesis and conclude that there is no significant difference
# between the means of Ozone before and after imputation.


# visualise the data before and after imputation using the density --------
plot(density(airquality$Ozone, na.rm = TRUE), main = "Data with Na",
     xlab = "Ozone", ylab = "Density") +
lines(density(airquality_comp$Ozone), col = "red", lty = 9)

#  solar.R ----------------------------------------------------------------
plot(density(airquality$Solar.R, na.rm = TRUE), main = "Data without Na",
     xlab = "Solar.R", ylab = "Density") +
  lines(density(airquality_comp$Solar.R), col = "red", lty = 9)

#  END--------------------------------------------------------------


