library(tidyverse)
library(maps)
library(showtext)
library(ggpattern)

#get state map coordinates
state_map <- map_data("state")

#default font from showtext
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

# Create the data frame with fed circuit information
circuit_data <- data.frame(
  Circuit = c("First Circuit", "First Circuit", "First Circuit", "First Circuit", "First Circuit",
              "Second Circuit", "Second Circuit", "Second Circuit",
              "Third Circuit", "Third Circuit", "Third Circuit", "Third Circuit",
              "Fourth Circuit", "Fourth Circuit", "Fourth Circuit", "Fourth Circuit", "Fourth Circuit",
              "Fifth Circuit", "Fifth Circuit", "Fifth Circuit",
              "Sixth Circuit", "Sixth Circuit", "Sixth Circuit", "Sixth Circuit",
              "Seventh Circuit", "Seventh Circuit", "Seventh Circuit",
              "Eighth Circuit", "Eighth Circuit", "Eighth Circuit", "Eighth Circuit", "Eighth Circuit", "Eighth Circuit", "Eighth Circuit",
              "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit", "Ninth Circuit",
              "Tenth Circuit", "Tenth Circuit", "Tenth Circuit", "Tenth Circuit", "Tenth Circuit", "Tenth Circuit",
              "Eleventh Circuit", "Eleventh Circuit", "Eleventh Circuit",
              "District of Columbia Circuit"
  ),
  State = c(
    "Maine", "Massachusetts", "New Hampshire", "Puerto Rico", "Rhode Island",
    "Connecticut", "New York", "Vermont",
    "Delaware", "New Jersey", "Pennsylvania", "U.S. Virgin Islands",
    "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia",
    "Louisiana", "Mississippi", "Texas",
    "Kentucky", "Michigan", "Ohio", "Tennessee",
    "Illinois", "Indiana", "Wisconsin",
    "Arkansas", "Iowa", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota",
    "Alaska", "Arizona", "California", "Guam", "Hawaii", "Idaho", "Montana", "Nevada", "Northern Mariana Islands", "Oregon", "Washington",
    "Colorado", "Kansas", "New Mexico", "Oklahoma", "Utah", "Wyoming",
    "Alabama", "Florida", "Georgia",
    "Washington, D.C."
  )
)

# Remove territories from the data frame
circuit_data <- circuit_data[!circuit_data$State %in% c("Puerto Rico", "U.S. Virgin Islands", "Guam", "Northern Mariana Islands"), ]

# Merge dataset with original map data
map_data <- state_map |> 
  mutate(region = str_to_title(region))

# Merge circuit data with map data
merged_data <- map_data |> 
  left_join(circuit_data, by = c("region" = "State"))

# Reorder Circuit as an ordered factor for legend
merged_data$Circuit <- fct_inorder(merged_data$Circuit)

# Define patterns and colors for each circuit
patterns <- c("stripe", "crosshatch", "weave", "bricks", "hexagons", "checkerboard",
              "circles", "diagonal_lines", "dashed_lines", "dots", "squares", "herringbone")
colors <- c("#86171B", "#C17264", "#C4D1D6", "#D9AD7C", "#BFA5B2", "#A1A38C",
            "#C3C2C8", "#8DB9CA", "#F4CA63", "#B6CCB6", "#E8A97A", "#99A8B5")


# Create a named vector with patterns and colors
pattern_colors <- setNames(colors, patterns)

# Plot
ggplot(merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Circuit)) +
  scale_fill_manual(values = pattern_colors) +
  coord_map() +
  labs(title = "Where Abortion Is Prohibited",
       caption = "Data as of July 8, 2022 (New York Times)\nRecreated by @tanya_shapiro") +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.05, color = "grey50", vjust = -12, size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 25), family = "Noto Serif"))