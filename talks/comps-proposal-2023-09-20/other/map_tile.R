library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(geofacet)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 51)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey90"
bg <- "grey10"
line <- "grey20"

pal <- c('#92001d', '#cb1a1e', '#ee5634', '#fb9e4f', '#fdda7e', '#ffffb2', '#d9f0f6', '#9cd0e3', '#629cc6', '#3660a5', '#252383')[1:9]

font_add_google("Karla", "karla")
font_add_google("Merriweather Sans", "ms")
showtext_auto()

ft <- "karla"
ft_title <- "ms"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$weather_forecasts |>
  filter(
    high_or_low == "high",
    !is.na(forecast_outlook),
    !is.na(forecast_hours_before)
  ) |>
  mutate(
    error = forecast_temp - observed_temp,
    hrs_before = paste("hrs", forecast_hours_before)
  )

df_us <- dat$ufo_sightings |>
  filter(country_code == "US") |>
  count(state) |>
  left_join(df_pop, by = "state") |>
  mutate(
    r = n/pop*10000,
    x = min_max(r, 0.2, 1),
    xmin = (1-x)/2,
    xmax = x + (1-x)/2,
    alpha = r/max(r)
  )



# 🔡 text --------------------------------------------------------------------

subtitle <-
  "Weather forecasts 48hrs before are very accurate
although tend to be slightly worse for states
Alaska, Wisconsin and Vermont

Forecast outlooks that are predicting freezing
rain / drizzle or a snow event tend to be
forecast up to 2F higher than what is observed"


# 📊 plot --------------------------------------------------------------------

g_us <- df_us |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, alpha = alpha), fill = accent) +
  geom_text(aes(0.5, 0.7, label = state), lineheight = 0.25, family = ft, size = 10, colour = bg, fontface = "bold") +
  geom_text(aes(0.5, 0.3, label = round(r, 1)), lineheight = 0.25, family = ft, size = 8, colour = bg, fontface = "bold") +
  facet_geo(~state) +
  scale_alpha_identity() +
  coord_fixed() +
  labs(subtitle = "Sightings per 10k population") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.subtitle = element_text(family = ft1, size = 40, hjust = 1, colour = txt, margin = margin(b = 10)),
    legend.position = "none"
  )


g_state <- df_state |>
  filter(hrs_before == "hrs 48") |>
  ggplot(aes(fill = abs(mean))) +
  ggchicklet:::geom_rrect(aes(xmin=0, xmax = 1, ymin = 0, ymax = 1, fill = mean), radius = grid::unit(6, "pt")) +
  geom_text(aes(0.5, 0.5, label = state), family = ft, size = 12, colour = bg, fontface = "bold") +
  facet_geo(~state) +
  scale_fill_gradientn(colours = rev(pal)) +
  labs(fill = "Mean error (F)") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    text = element_text(family = ft, size = 32, colour = txt, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(family = ft, size = 32, colour = txt, hjust = 0.5, lineheight = 0.3, margin = margin(t = 20)),
    plot.subtitle = element_markdown(family = ft, size = 48, colour = txt, hjust = 0.15, margin = margin(b = 20), halign = 0),
    plot.margin = margin(b = 20, t = 30, r = 120, l = 120),
    legend.box.margin = margin(t = 30),
    legend.position = "bottom"
  )

ggsave("scripts/2022/week 51 weather/weather.png", height = 12, width = 16)
