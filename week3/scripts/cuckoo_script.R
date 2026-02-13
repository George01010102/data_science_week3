library(tidyverse)

ggplot(cuckoo, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()


#makes linear model of cuckoo data
cuckoo_lm <- lm(Beg ~ Mass * Species, data = cuckoo)

summary(cuckoo_lm)