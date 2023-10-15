library(tidyverse)
library(emojifont)
library(showtext)
library(reactable)
library(kableExtra)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

showtext_auto()
set.seed(1234)
population_df = tibble(ID = 1:200, 
                       x = rep(1:20, times = 10),
                       y = rep(1:10, each = 20),
                       Value = rnorm(200, 250, 50))
sample_size = 58
sample_ids = sample(1:200, size = sample_size, replace = FALSE)
sample_df =  filter(population_df, ID %in% sample_ids)

estimate_size = 100
estimate_ids = sample(1:200, size = estimate_size, replace = FALSE)
estimate_df =  filter(population_df, ID %in% estimate_ids)


txt <- "grey20"
bg <- "#f5f1e7"


font_add("monolisa", regular = "assets/fonts/MonoLisaemw-Regular.woff")
showtext.auto()

# 🔡 main plot --------------------------------------------------------------------


ggplot() +
  geom_text(data = population_df,
            mapping = aes(x = x,
                          y = y,
                          label = fontawesome('fa-user')),
            family='fontawesome-webfont', size = 10, colour = "grey") +
  geom_text(data = estimate_df,
            mapping = aes(x = x,
                          y = y,
                          label = fontawesome('fa-user'),
                          colour = Value),
            family='fontawesome-webfont', size = 10) +
  scale_colour_gradient(low = "#F5CCCC", high = "#D00000") +
  labs(title = "31-50% of all pregnancies") +
  theme_void() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10), 
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  family = "monolisa",
                                  size = 36,
                                  margin = margin(b = 10)))
  #+ ggsave("images/all_preg.png")

