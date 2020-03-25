###########
##
##  LETS MAKE A QUICK R SCRIPT - DANKITUDE
##
###########
rm(list = ls())

require(tidyverse)

# 1. Simulate some data
my_df <- 
  data.frame(xVar  = rnorm(1000, sd = 10),
             grVar = rep(c("A","B"), each = 500)) %>% 
  mutate(err = rnorm(1000, sd = 5),
         yVar = if_else(grVar == "A", 
                        1.3*xVar + 10 + err, 
                        1.9*xVar + 23 + err)) %>% 
  dplyr::select(-err)

# 2. Have a look
ggplot(my_df, aes(x = xVar, y = yVar, colour = grVar, 
                  fill = grVar, group = grVar)) +
  geom_point(size = 4, alpha = 0.2, show.legend = F) +
  geom_smooth(method = "lm", alpha = 0.5, aes(fill = grVar)) +
  scale_colour_viridis_d(begin = 0.2, end = 0.7, option = "B", 
                         name = "Class", labels = c("Mammalia", "Aves"),
                         aesthetics = c("colour", "fill")) +
  labs(x = "Population dankitutde (km)", 
       y = "Scaled population response") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.13), 
        legend.background = element_blank()) +
  ggsave("dankitude_plot.pdf", width = 7, height = 7)
  
# 3. Statistical model
mod <- lm(yVar ~ xVar*grVar, data = my_df)

broom::tidy(mod)
broom::glance(mod)




