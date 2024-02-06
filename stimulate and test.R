#install.packages("tidyverse")
#install.packages("broom")
#install.packages("ggplot2")
#install.packages("reshape2")

library(tidyverse)
library(broom)
library(ggplot2)
library(reshape2)

# seed
set.seed(20)

# Number of days
days <- 100

# Simulate
# asssume Ash and Jacji are couple
ash_pages <- rnorm(days, mean = 50, sd = 10)
jacki_pages <- 0.5 * ash_pages + rnorm(days, mean = 25, sd = 5)

# normal distribution
matt_pages <- rnorm(days, mean = 30, sd = 5)
# uniform
rol_pages <- runif(days, min = 10, max = 100)
# linear + random noise
mike_pages <- seq(10, 100, length.out = days) + rnorm(days, mean = 0, sd = 10)

# data frame
reading_data <- data.frame(day = 1:days,
                           Matt = matt_pages,
                           Ash = ash_pages,
                           Jacki = jacki_pages,
                           Rol = rol_pages,
                           Mike = mike_pages)

# summary
summary(reading_data[-1])



# Test 1: Normality Test
normality_tests <- reading_data %>% 
  select(-day) %>% 
  gather(key = "undergraduate", value = "pages") %>% 
  group_by(undergraduate) %>% 
  summarise(shapiro_test_p_value = shapiro.test(pages)$p.value)

print(normality_tests)

# Test 2: Independence Test (excluding Ash and Jacki)
cor_matrix <- cor(reading_data[,-c(1,3,4)]) 
print(cor_matrix)

# Test 3: Correlation Test for Ash and Jacki
cor_ash_jacki <- cor(reading_data$Ash, reading_data$Jacki)
print(cor_ash_jacki)


# Test 4: Test for Mean Reading Pages
expected_means <- c(Matt = 30, Ash = 50, Jacki = 50, Rol = 55, Mike = 55)
actual_means <- colMeans(reading_data[-1])
mean_tests <- actual_means == expected_means
print(mean_tests)

# Test 5: Range Test
range_tests <- apply(reading_data[-1], 2, function(x) all(x >= 10 & x <= 100))
print(range_tests)