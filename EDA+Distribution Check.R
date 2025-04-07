library(here) # VERY useful package -- with here() function, you don't need to type or copy and paste directory path
library(tidyverse) # Today's data science staple. Includes packages you are likely to use in data analysis including ggplot2 and dplyr
library(plotrix) # For graphics
library(PupillometryR) # Includes function geom_flat_violin
library(Matrix)
library(tidyverse)
library(knitr)
library(kableExtra)
library(MASS)
library(mgcv)
library(lmtest)
library(dplyr)
library(nlme)
library(AICcmodavg)
# Use classic theme and increase font size for all ggplot figures
theme_set(theme_classic()) 

alcohol <- read.table(file = "alcohol.txt", header=TRUE)
data <- alcohol
names(data)
head(data)
summary(data)
par(mfrow = c(2, 2))  # Arrange plots in a 2x3 grid

# Histogram
hist(data$alcuse, main = "Histogram of Alcuse", xlab = "Alcuse", breaks = 20, col = "lightblue")

# Density Plot
plot(density(data$alcuse), main = "Density Plot of Alcuse", xlab = "Alcuse", col = "blue")

# Boxplot
boxplot(data$alcuse, main = "Boxplot of Alcuse", ylab = "Alcuse", col = "lightgreen")

# QQ Plot
qqnorm(data$alcuse, main = "QQ Plot of Alcuse")
qqline(data$alcuse, col = "red")

# Shapiro-Wilk Test: If the p-value is less than 0.05, reject the null hypothesis (data is not normal).
shapiro.test(data$alcuse)

hist(log(alcohol$alcuse), main = "Histogram of log(Alcuse)", xlab = "log(Alcuse)", breaks = 20, col = "lightblue")

hist(sqrt(alcohol$alcuse), main = "Histogram of log(Alcuse)", xlab = "square_root_Alcuse", breaks = 20, col = "lightblue")

plot(density(log(alcohol$alcuse)), main = "Density Plot of log(Alcuse)", xlab = "Alcuse", col = "blue")

plot(density(sqrt(alcohol$alcuse)), main = "Density Plot of sqrt(Alcuse)", xlab = "Alcuse", col = "blue")

data %>%
  ggplot(aes(x = age, y = alcuse, color = as.factor(coa))) +
  geom_point() +
  stat_summary(aes(group = coa), fun = mean, geom="line") +
  labs(y = "Alcohaol use", x = "Age", color = "Alcoholic parents(Yes/No)")+
  theme_minimal()

data %>%
  ggplot(aes(x = age, y = alcuse, color = as.factor(male))) +
  geom_point() +
  stat_summary(aes(group = male), fun = mean, geom="line") +
  labs(y = "Alcohaol use", x = "Age", color = "Gender(Male=1,Female0)")+
  theme_minimal()

data$peer <- round(data$peer)
data %>%
  ggplot(aes(x = age, y = alcuse, color = as.factor(peer))) +
  geom_point(alpha = 0.5) +
  stat_summary(aes(group = peer), fun = mean, geom = "line", size = 1.2) +
  labs(
    y = "Alcohol Use",
    x = "Age",
    color = "Peer Group"
  ) +
  theme_minimal()

data %>%
  group_by(age, male) %>%
  summarise(mean = round(mean(alcuse), 2), .groups = "drop") %>%
  mutate(gender = recode(male, `1` = "male", `0` = "female")) %>%
  dplyr::select(age, gender, mean) %>%
  knitr::kable(caption = "") %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))


data %>%
  group_by(age, coa) %>%
  summarise(mean = round(mean(alcuse), 2), .groups = "drop") %>%
  mutate(parental_alcoholism = recode(coa, `0` = "No", `1` = "Yes")) %>%
  dplyr::select(age, parental_alcoholism, mean) %>%
  knitr::kable(caption = "") %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))


data %>%
  group_by(age, peer) %>%
  summarise(mean = round(mean(alcuse), 2), .groups = "drop") %>%
  knitr::kable(caption = "") %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

data %>%
  filter(peer == 3) %>%
  mutate(alcuse = round(alcuse, 2)) %>%
  knitr::kable(caption = "") %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))