# Loading libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(htmltools)
library(ggbump)
library(ggimage)
library(ggstar)
library(ggarrow)
library(here)
# library(scales)

# Importing fonts
font_add_google("Roboto Slab", family = "Roboto Slab")
font_add_google("Roboto", family = "Roboto")
font_add(family = "FontAwesomeBrands", regular = "C:/Users/Raymundo/Documentos/R/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")

# showtext_opts(dpi = 300)
showtext_auto(enable = FALSE)

# Source utility functions
source(here::here("2024/month-01/p02-senna_prost_mansell_piquet/R/social_icons.R"))

max_point <- 10

# Datasets
## Senna
df_senna <- data.frame(
  year = c(1984:1994),
  points = c(13, 38, 55, 57, 90, 60, 78, 96, 50, 73, 0),
  position = c(9, 4, 4, 3, 1, 2, 1, 1, 4, 2, 38))

## Piquet
df_piquet <- data.frame(
  year = c(1978:1991),
  points = c(0, 3, 54, 50, 20, 59, 29, 21, 69, 73, 22, 12, 43, 26.5),
  position = c(28, 16, 2, 1, 11, 1, 5, 8, 3, 1, 6, 8, 3, 6)
)


## Prost
df_prost <- data.frame(
  year = c(1980:1993),
  points = c(5, 43, 34, 57, 71.5, 73, 72, 46, 87, 76, 71, 34, 0, 99),
  position = c(16, 5, 4, 2, 2, 1, 1, 4, 2, 1, 2, 5, 99, 1)
) 

## Mansell
df_mansell <- data.frame(
  year = c(1980:1995),
  points = c(0, 8, 7, 10, 13, 31, 70, 61, 12, 38, 37, 72, 108, 0, 13, 0),
  position = c(30, 14, 14, 13, 10, 6, 2, 2, 9, 4, 5, 2, 1, 99, 9, 29)
)

# Combine all datasets into a single dataframe
df <- bind_rows(
  mutate(df_senna, driver = "Senna"),
  mutate(df_piquet, driver = "Piquet"),
  mutate(df_prost, driver = "Prost"),
  mutate(df_mansell, driver = "Mansell")
)

# Adjust ranks and create a season column
df <- df %>%
  mutate(season = paste0("'", substr(year, 3, 4))) %>%
  mutate(rank = if_else(position > 10,
                        11L,
                        position))

# Identify the first and last season for each driver
df <- df %>%
  group_by(driver) %>%
  mutate(first_season = min(year),
         last_season = max(year),
         d_first_season = if_else(year == first_season, 1, 0)) %>%
  ungroup()

# Create groups for the "active" top 10. 
# This is needed to supress geom_bump to draw lines more than to the next season.
df <- df %>%
  arrange(driver, season) %>%
  group_by(driver) %>%
  mutate(lag_zero = if_else(lag(rank) %in% c(max_point + 1, NA) & rank <= max_point, 1, 0, 0)) %>%
  ungroup() %>%
  mutate(group = cumsum(lag_zero))

# Define custom colors for drivers
pal_colors <- c(
  "Senna" = "#fad02c",
  "Prost" = "#f41823",
  "Piquet" = "#0d733c",
  "Mansell" = "#025492"
)

# # Add colors to the main dataframe 
# df <- df %>%
#   left_join(pal_colors, by = "driver")

summary(df)
anyNA(df)  # Check for missing values
print(df)

# Images for drivers and legends information
images <- df %>%
  filter(d_first_season == 1) %>%
  select(driver, year) %>%
  arrange(year) %>%
  mutate(x_pos = c(1978, 1980.5, 1979.9, 1983.7),
         y_pos = c(11, 11, 11, 9),
         x_leg = rep(1978, 4),
         x_text = rep(1978.3, 4),
         y_leg = c(13, 16, 14.5, 17.5),
         path = paste0(
           "2024/month-01/p02-senna_prost_mansell_piquet/images/",
           str_replace_all(tolower(driver), " ", "_"),
           ".png"))

# Define palette colors
pal_bg <- '#050200'         # background color
pal_text <- 'gray90'        # text color
pal_title <- '#FFD700'      # title color (gold)

# Prepare caption
# caption <- tagList(
#   tags$span("Source: Wikipedia"),
#   tags$br(),
#   tags$span(HTML("&#xf099;"), style = 'color:#1DA1F2; font-family:"FontAwesomeBrands";'),
#   tags$span("@raymundoooooooo "),
#   tags$span(HTML("&#xf09b;"), style = 'color:#171515; font-family:"FontAwesomeBrands";'),
#   tags$span("raymz1990 "),
#   tags$span(HTML("&#xf08c;"), style = 'color:#0077B5; font-family:"FontAwesomeBrands";'),
#   tags$span("raymundo_pilz")
# )

caption <- tagList(
  tags$span("Source: Wikipedia")
)

# Plot
p <-
  ggplot(
    data = df,
    aes(x = year, y = rank, group = driver))+
  
    # General lines
    geom_bump(
      aes(color = driver),
      smooth = 15,
      linewidth = 1.5,
      alpha = 0.2) +
    
    scale_y_reverse() +
    
    # Top 10 lines
    geom_bump(
      data = df %>%
        filter(rank <= 10),
      aes(x = year, y = rank, group = group, color = driver),
      smooth = 15,
      linewidth = 1.5,
      inherit.aes = F
      ) +
  
    # Driver images
    geom_image(
      data = images, 
      mapping = aes(x = x_pos, y = y_pos, image = path), 
      size = 0.07 
    ) +
    
    # Stars for championships
    geom_star(
      data = df %>%
        filter(rank == 1),
      aes(starshape = "pentagram", color = driver, fill = driver),
      size = 5
    ) +
    
    # Custom colors
    scale_color_manual(values = pal_colors) +
    scale_fill_manual(values = pal_colors) +
    
    # X-axis
    scale_x_continuous(
      breaks = df$year %>% 
        unique() %>%
        sort(),
      labels = df %>% 
        distinct(year, season) %>% 
        arrange(year) %>%
        pull(season),
      # expand = expand_scale(mult = .1),
      position = 'top'
    ) + 
    
    # Annotations
    ## Senna
    geom_textbox(
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = 1994.1,
        y = 11.5,
        label = "<p style='font-family:Roboto;font-size:10pt;'>
        <span style='color:#e5e5e5;'><b>Senna's</b> fatal crash </span> </p>")
    ) +
    
    # geom_curve(
    #   mapping = aes(
    #     x = 1994.2,
    #     xend = 1993.99,
    #     y = 11.5,
    #     yend = 11
    #   ),
    #   color = "grey90",
    #   curvature = -0.5,
    #   # linewidth = 0.1,
    #   arrow = arrow(length = unit("0.05", "in"))
    # ) +
  
    geom_arrow_curve(
      mapping = aes(
        x = 1994.2,
        xend = 1993.99,
        y = 11.5,
        yend = 11
      ),
      color = "grey90",
      curvature = -0.5,
      linewidth = 0.5,
      lineend = "butt",
      angle = 90
    ) +
    
    # Mansell's retirement
    geom_textbox(
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = 1993.4,
        y = 12,
        label = "<p style='font-family:Roboto;font-size:10pt;'>
        <span style='color:#e5e5e5;'><b>Mansell's</b> first retirement </span> </p>")
    ) +
    
    geom_arrow_curve(
      mapping = aes(
        x = 1993.5,
        xend = 1993,
        y = 12,
        yend = 11
      ),
      color = "grey90",
      curvature = -0.5,
      linewidth = 0.5,
      lineend = "butt",
      angle = 90
    ) +
    
    # Prost's retirement
    geom_textbox(
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = 1992.5,
        y = 12.5,
        label = "<p style='font-family:Roboto;font-size:10pt;'>
        <span style='color:#e5e5e5;'><b>Prost's</b> first retirement </span> </p>"
      )
    ) +
    
    geom_arrow_curve(
      mapping = aes(
        x = 1992.5,
        xend = 1992,
        y = 12.5,
        yend = 11
      ),
      color = "grey90",
      curvature = -0.5,
      linewidth = 0.5,
      lineend = "butt",
      angle = 90
    ) +
    
    # Reference points (Y-axis)
    geom_point(
      data = tibble(x = 1977, y = 1:10),
      aes(x = x, y = y),
      inherit.aes = F,
      color = pal_text,
      size = 10,
      pch = 21
    ) +
    
    geom_text(
      data = tibble(x = 1977, y = 1:10),
      aes(x = x, y = y, label = y),
      inherit.aes = F,
      color = pal_text
    ) +
    
    # Driver legends
    geom_image(
      data = images,
      mapping = aes(x = x_leg, y = y_leg, image = path),
      size = 0.07
    ) +
    
    geom_textbox(
      data = images %>%
        filter(driver == "Piquet"),
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = x_text,
        y = y_leg,
        label = "<p style='font-family:Roboto;font-size:12pt;'>
        <span style='color:#0d733c;'><b>Nelson Piquet</b> (1978-1991) </span> </p>")
    ) +
    
    geom_textbox(
      data = images %>%
        filter(driver == "Prost"),
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = x_text,
        y = y_leg,
        label = "<p style='font-family:Roboto;font-size:12pt;'>
        <span style='color:#f41823;'><b>Alain Prost</b> (1980-1991, 1993) </span> </p>")
    ) +
    
    geom_textbox(
      data = images %>%
        filter(driver == "Mansell"),
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = x_text,
        y = y_leg,
        label = "<p style='font-family:Roboto;font-size:12pt;'>
        <span style='color:#025492;'><b>Nigel Mansell</b> (1980-1992, 1994-1995) </span> </p>")
    ) +
    
    geom_textbox(
      data = images %>%
        filter(driver == "Senna"),
      fill = "#050200",
      box.size = NA,
      hjust = 0,
      width = unit(4, "in"),
      mapping = aes(
        x = x_text,
        y = y_leg,
        label = "<p style='font-family:Roboto;font-size:12pt;'>
        <span style='color:#fad02c;'><b>Ayrton Senna</b> (1984-1994) </span> </p>")
    ) +
  
  # Titles and captions
  labs(
    x = NULL,
    title = "The 80s F1 Rivalry: A Clash of Titans",
    subtitle = "A Decade of Dominance: Prost vs Senna vs Piquet vs Mansell in F1",
    caption = caption
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    text = element_text(family = "Roboto Slab"),
    plot.margin = margin(t = 20, b = 10, l = 15, r = 15),
    # aspect.ratio = 12/16,
    plot.title = element_text(
      face = "bold",
      family = "Roboto Slab",
      size = 24,
      hjust = 0.09,
      color = pal_text),
    plot.subtitle = element_text(
      family = "Roboto Slab",
      size = 16,
      hjust = 0.1,
      color = pal_text,
      margin = margin(b = 20)), 
    plot.caption = element_textbox_simple(
      hjust = 1,
      size = 10,
      color = pal_text,
      margin = margin(t = 10)),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = 2, size = 12, color = pal_text),
    panel.background = element_rect(fill = pal_bg, color = "transparent"),
    plot.background = element_rect(fill = pal_bg)
  )

p

# Save final plot
width <- 5460 
height <- 2880

Cairo::Cairo(
  file = "2024/month-01/p02-senna_prost_mansell_piquet/plot/80_rivalry.png",
  width = width,  
  height = height, 
  units = "px",
  type = "png", 
  bg = "transparent",
  # pointsize = 12*300/72,
  dpi = 300
)

plot(p)
dev.off()

# ggsave(
#   "2024/month-01/p02-senna_prost_mansell_piquet/plot/80_rivalry1.png",
#   plot = p,
#   width = width,  
#   height = height, 
#   dpi = 300,
#   units = "px",
#   bg = "transparent"
# )
