load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_5807/datasets/ilo_data.RData"))


library(tidyverse)


european_countries <- c("Finland","France","Italy","Norway",
                        "Spain","Sweden","Switzerland","United Kingdom",
                        "Belgium","Ireland","Luxembourg","Portugal",
                        "Netherlands","Germany","Hungary","Austria",
                        "Czech Rep.")

# Only retain European countries
ilo_data <- ilo_data %>%
  filter(country %in% european_countries)

# Group and summarize the data
ilo_data %>%
  group_by(year) %>%
  summarize(mean_hourly_compensation = mean(hourly_compensation),
            mean_working_hours = mean(working_hours))

# Filter for 2006
plot_data <- ilo_data %>%
  filter(year == 2006)

# Create the plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  # Add labels
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# Save your current plot into a variable: ilo_plot
ilo_plot <- ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

# View it
ilo_plot


library(showtext)
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Montserrat", "Montserrat")

showtext_auto()


ilo_plot <- ilo_plot +
  theme_minimal() +
  # Customize the "minimal" theme with another custom "theme" call
  theme(
    text = element_text(family = "Montserrat", face = "italic"),
    title = element_text(color = "gray25"),
    plot.caption = element_text(color = "gray30"),
    plot.subtitle = element_text(size = 12)
  )

# Render the plot object
ilo_plot


ilo_plot +
  # "theme" calls can be stacked upon each other, so this is already the third call of "theme"
  theme(
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5,10,5,10), units = "mm")
  )

# Filter ilo_data to retain the years 1996 and 2006
ilo_data <- ilo_data %>%
  filter(year == 1996 | year == 2006)


# Again, you save the plot object into a variable so you can save typing later on
ilo_plot <- ggplot(ilo_data, aes(x = working_hours, y = hourly_compensation)) +
  geom_point() +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  ) +
  # Add facets here
  facet_grid(facets = . ~ year)

ilo_plot

# Creamos tema personalizado ----
# Define your own theme function below
theme_ilo <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Montserrat", color = "gray25"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}


# Apply your theme function (dont't forget to call it with parentheses!)
ilo_plot <- ilo_plot +
  theme_ilo()

# Examine ilo_plot
ilo_plot

ilo_plot +
  # Add another theme call
  theme(
    # Change the background fill and color
    strip.background = element_rect(fill = "gray60", color = "gray95"),
    # Change the color of the text
    strip.text = element_text(color = "white")
  )

# DOT PLOT ----

# Create the dot plot
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country))


ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            # Add an arrow to each path
            arrow = arrow(length = unit(1.5, "mm"), type = "closed"))


ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  # Add a geom_text() geometry
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1))
  )


library(forcats)

# Reorder country factor levels
ilo_data <- ilo_data %>%
  # Arrange data frame
  arrange(year) %>%
  # Reorder countries by working hours in 2006
  mutate(country = fct_reorder(country,
                               working_hours,
                               last))

# Plot again
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1))
  )


# Save plot into an object for reuse
ilo_dot_plot <- ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  # Specify the hjust aesthetic with a conditional value
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1),
        hjust = ifelse(year == "2006",1.4,-0.4)
    ),
    # Change the appearance of the text
    size = 3,
    family = "Bookman",
    color = "gray25"
  )

ilo_dot_plot


# APLICAMOS NUESTRO TEMA Y ADAPTAMOS EL TAMAÑO PARA EVITAR OVERLAPPING CON LOS BORDES ----
# Reuse ilo_dot_plot
ilo_dot_plot <- ilo_dot_plot +
  # Add labels to the plot
  labs(
    x = "Working hours per week",
    y = "Country",
    title = "People work less in 2006 compared to 1996",
    subtitle = "Working hours in European countries, development since 1996",
    caption = "Data source: ILO, 2017"
  ) +
  # Apply your theme
  theme_ilo() +
  # Change the viewport
  coord_cartesian(xlim = c(25, 41))

# View the plot
ilo_dot_plot


# Optimization for mobile devices ----
# Compute temporary data set for optimal label placement
median_working_hours <- ilo_data %>%
  group_by(country) %>%
  summarize(median_working_hours_per_country = median(working_hours)) %>%
  ungroup()

# Have a look at the structure of this data set
str(median_working_hours)

ilo_dot_plot +
  # Add label for country
  geom_text(data = median_working_hours,
            aes(y = country,
                x = median_working_hours_per_country,
                label = country),
            vjust = 2,
            family = "Bookman",
            color = "gray25") +
  # Remove axes and grids
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # Also, let's reduce the font size of the subtitle
    plot.subtitle = element_text(size = 9)
  )
