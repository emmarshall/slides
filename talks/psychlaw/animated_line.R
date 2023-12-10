# libraries:
library(ggplot2)
library(gganimate)
library(dplyr)
library(haven)

# Read dataset

lhb <- read_sav("LP40_Master.sav")

# Prepare data for gif
df <- lhb |>  
  select(ID, Year, numb_cites, gen_area) |>
  # Convert year from character to numeric 
  mutate (year = as.numeric(Year),
          gen_area = as.character(gen_area)) |>
  mutate (gen_area = case_when(
    gen_area=="1" ~ "jury decision making",
    gen_area=="2" ~ "eyewitnesses",
    gen_area=="7"  ~ "child witnesses",
    gen_area=="8"  ~ "expert witnesses",
    gen_area=="3" ~ "risk assessment",
    gen_area=="6" ~ "public policy",
    gen_area=="11" ~ "juvenile law",
    gen_area=="21" ~ "mental health",
    gen_area=="24" ~ "mental health",
    TRUE ~ "all others"),
    gen_area = factor(gen_area, levels=c("jury decision making", "eyewitnesses", "child witnesses", "expert witnesses", "risk assessment", "public policy", "juvenile law", "mental health", "all others"))
  ) |>
  mutate(
    gen_area = stringr::str_to_title(gen_area),
    gen_area = forcats::fct_lump(gen_area, n = 9)
  ) 


  
# Plot
p <- ggplot(df, aes(x = year, y = numb_cites, group = gen_area, color = gen_area)) +
    geom_line() +
    geom_point() +
    ggtitle("Popularity of Research Areas over 40 years") +
    ylab("Number of Citations") +
    xlab("Year") +
    scale_color_discrete(name = "Research Area") +
    scale_y_continuous(
      limits = c(0,750),
      breaks = 0:750*100,
      labels = scales::comma_format(
        suffix = " citations"
      ),
      name = NULL
    ) +
    theme_minimal() +
    theme(
    text = element_text(family = "Metropolis", size = 12),
    legend.text = element_text(family = "Metropolis", size = 10),
    legend.title = element_text(family = "Metropolis", size = 12, face = "bold"),
    axis.text = element_text(family = "Metropolis", size = 10),
    axis.title = element_text(family = "Metropolis", size = 12, face = "bold")
  ) +
    transition_reveal(year) 

p


animation

# Save at gif:
anim_save("images/animated-line.gif")
