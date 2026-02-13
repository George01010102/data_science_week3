library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
install.packages("performance")
library(performance)
cuckoo <- read_csv(here("data_science_week3","week3","data","cuckoo.csv"))

ggplot(cuckoo, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()


#makes linear model of cuckoo data
cuckoo_lm <- lm(Beg ~ Mass * Species, data = cuckoo)

summary(cuckoo_lm)


#examine diagnostics
check_model(cuckoo_lm, detrend = FALSE)
