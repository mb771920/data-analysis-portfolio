#STAT6160 DATA ANALYTICS FOR BUSINESS
#Assignment 1
#Question 2(i) Soluation

dataQa <- read.csv("Questiona.csv", header = TRUE)
freqSorted <- sort(table(dataQa$Percentage), decreasing = TRUE)
dataQa$CumulativePercentage <- cumsum(dataQa$Percentage)

install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins for better visualization
barplot(Percentage, names.arg = Cause, col = "skyblue", main = "Pareto Chart",
        xlab = "Cause", ylab = "Percentage", ylim = c(0, max(counts) * 1.2))
par(new = TRUE)
plot(CumulativePercentage, type = "b", col = "red", axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100))
axis(4)
mtext("Cumulative Percentage", side = 4, line = 3)


ggplot(dataQa, aes(x=Cause, y=Percentage)) +
  geom_col(aes(y=Percentage). fill="blue") +
  geom_line(aes(y=CumulativePercentage), color="red") +
  
  ggplot(dataQa, aes(x = Cause, y = Percentage)) +
  geom_col(aes(y=Cause), fill = "skyblue") +
  
  # Order categories by frequency (descending)
  dataQa <- dataQa[order(-dataQa$Percentage), ]

# Calculate cumulative frequencies
dataQa$CumulativePercentage <- cumsum(dataQa$Percentage)


barplot(frequency_df$Frequency, names.arg = frequency_df$Category, col = "skyblue", 
        main = "Pareto Chart", xlab = "Category", ylab = "Frequency")
lines(1:nrow(frequency_df), frequency_df$CumulativeFrequency, type = "b", pch = 21, col = "red", lty = 1, lwd = 2)
legend("topright", legend = c("Frequency", "Cumulative Frequency"), 
       col = c("skyblue", "red"), lty = c(0, 1), pch = c(15, 21), lwd = c(0, 2))

install.packages("qcc")
library(qcc)
barplot(dataQa$Percentage, names.arg = dataQa$Cause, col = "skyblue", main = "Pareto Chart",
        xlab = "Cause", ylab = "Percentage", ylim = c(0, max(dataQa$Percentage) * 1.2))

# Overlay another plot (for example, a line plot)
lines(1:length(dataQa$Cause), dataQa$Percentage, type = "b", col = "red", pch = 16

pareto.chart(dataQa$Percentage, ylab = "Percentage", col = "skyblue", main = "Pareto Chart")

ggplot(datab, aes(x = operational, y = Asset_Liability_Ratio, fill = category)) +
  geom_boxplot(position = "dodge") +
  labs(x = "operational", y = "Asset_Liability_Ratio", fill = "Category") +
  theme_minimal()