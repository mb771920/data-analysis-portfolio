install.packages("tidyverse")
library(tidyverse)
starwars
View(starwars)
starwars <- data.frame(starwars)
# sactter plot
starwars %>% 
  filter(mass <200) %>% 
  ggplot(aes(height, mass, color = sex)) +
  geom_point( size = 5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "height and mass by sex")
#smoothed model
starwars %>% 
  filter( mass < 200) %>%
  ggplot(aes( height, mass, color = sex)) +
  geom_point (size = 3, alpha = 0.8) +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw() +
  labs(title = "height and mass by sex")
#t - test
library(gapminder)
view(gapminder)
t_test_plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided", paired = FALSE)
#ANOVA
gapminder %>% 
  filter(year == 2007) %>% 
  filter (continent %in% c("Americas", "Europe", " Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>%
  summary()
# ANOVA with post-hoc test
gapminder %>% 
  filter(year == 2007) %>% 
  filter (continent %in% c("Americas", "Europe", " Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD() %>%
  plot()
#chi-squared test
head(iris)
flowers <- iris %>% 
  mutate(size = cut(Sepal.Length, breaks = 3, labels = c("small", "medium", "large"))) %>%
  select(Species, size)
head(Book1)
mean(Book1$DIFF)
summary(Book1)
outlier(Book1$DIFF)
library(ggplot2)
ggplot(Book1, aes(x = DIFF)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of DIFF", x = "DIFF", y = "Frequency") +
  theme_minimal()
insta