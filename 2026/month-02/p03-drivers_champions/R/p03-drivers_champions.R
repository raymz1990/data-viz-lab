## Challenge: #plot03
## Title:     Formula 1 World Drivers' Champions
## Author:    Raymundo Eduardo Pilz
## Date:      2026-02-16


## 1. LOAD PACKAGES & SETUP ----

### |- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, ggimage, sysfonts, showtext,
  here, camcorder, magick, glue, htmltools
)

### |- source utility functions ----
source(here::here("utils/R/fonts.R"))
source(here::here("utils/R/social-media.R"))

### |- shortcut ----
shortcut <- here::here("2026/month-02/p03-drivers_champions")
plot_dir  <- file.path(shortcut, "plot")

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("temp_plots"), recursive = TRUE, showWarnings = FALSE)


### |- ggplot rendering ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width = 11,
  height = 15,
  units = "in",
  dpi = 320
)


## 2. VISUAL IDENTITY ----

### |- fonts ----
font_add_google("Montserrat", family = "Montserrat")
showtext_auto()
showtext_opts(dpi = 320)

font_title <- "Montserrat"
font_body  <- "Montserrat"

### |- palette ----
pal_bg        <- "#D9D4C5"
pal_text      <- "#1E1E1E"
pal_muted     <- "#6E6A61"
pal_grid      <- "#2B2B2B"
pal_wins      <- "#E56A73"
pal_points    <- "#E5D466"
pal_accent    <- "#111111"

### |- title ----
title <- "Formula One World Drivers' Champions"

### |- caption ----
caption_text <- create_social_caption(
  subcaption  = "",
  source_text = "Wikipedia"
)


## 3. LOAD DATA ----

source(here::here("2026/month-02/data/wikipedia/scrap_wikipedia.R"))

tbl_constructor <- tbl_constructor %>%
  select(Season, Constructor) %>%
  rename(Constructor_Champ = Constructor)

tbl <- tbl_drivers %>%
  left_join(tbl_constructor, by = "Season")


## 4. AUXILIARY DATA ----

tbl_flags <- tibble(
  Country = c(
    "Italy", "Argentina", "United Kingdom", "Australia", "United States",
    "New Zealand", "Austria", "Brazil", "South Africa", "Finland",
    "France", "Germany", "Canada", "Spain", "Netherlands"
  ),
  Code_Country = c(
    "it", "ar", "gb", "au", "us",
    "nz", "at", "br", "za", "fi",
    "fr", "de", "ca", "es", "nl"
  )
) %>%
  mutate(flag = paste0("https://flagcdn.com/80x60/", Code_Country, ".png"))


## 5. DATA WRANGLING ----

tbl <- tbl %>%
  left_join(tbl_flags, by = "Country") %>%
  mutate(
    Season = as.integer(Season),
    Points = as.numeric(Points),
    `% Win` = as.numeric(`% Win`),
    n = row_number()
  )


driver_titles <- tbl %>%
  count(Driver, name = "n_titles")

tbl <- tbl %>%
  left_join(driver_titles, by = "Driver") %>%
  mutate(
    Winner_Annot = if_else(n_titles >= 3, glue("{Driver}"), Driver) ## for bold highlight, insert **{Driver}**
  )


## 6. LAYOUT POSITIONS ----

x_pos <- list(
  year_left    = -1000,
  winner_flag  = 300,
  winner_name  = 700,
  team         = 4300,
  poles        = 7900,
  wins         = 9500,
  win_rate     = 10800,
  points       = 16400,
  constructor  = 21800,
  year_right   = 26000
)

x_line <- tibble(
  x = c(-2000, 0, 4000, 7000, 8800, 10400, 16000, 21400, 25000, 27000)
)

x_head <- tibble(
  x = c(-1800, 200, 4200, 7200, 9000, 10600, 16200, 21600, 25200)
)


add_header <- function(x, y, label, hjust = 0, size = 3.5, family = font_body) {
  annotate(
    "text",
    x = x,
    y = y,
    label = toupper(label),
    hjust = hjust,
    family = family,
    size = size
  )
}


## 7. PLOT ----

plot <- ggplot(tbl) +
  
  ## left year ----
geom_richtext(
  aes(x = x_pos$year_left, y = n, label = Season),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  family = font_body, size = 2.5
) +
  
  ## right year ----
geom_richtext(
  aes(x = x_pos$year_right, y = n, label = Season),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  family = font_body, size = 2.5
) +
  
  ## flags ----
geom_image(
  aes(x = x_pos$winner_flag, y = n, image = flag),
  size = 0.01
) +
  
  ## driver ----
geom_richtext(
  aes(x = x_pos$winner_name, y = n, label = Winner_Annot),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  hjust = 0, family = font_body, size = 2.5
) +
  
  ## team ----
geom_richtext(
  aes(x = x_pos$team, y = n, label = Constructor),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  hjust = 0, family = font_body, size = 2.5
) +
  
  ## poles ----
geom_richtext(
  aes(x = x_pos$poles, y = n, label = Poles),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  hjust = 0.5, family = font_body, size = 2.5
) +
  
  ## wins ----
geom_richtext(
  aes(x = x_pos$wins, y = n, label = Wins),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  hjust = 0.5, family = font_body, size = 2.5
) +
  
  ## win rate area ----
geom_ribbon(
  aes(
    xmin = x_pos$win_rate,
    xmax = x_pos$win_rate + `% Win` * 50,
    y = n
  ),
  fill = pal_wins,
  alpha = 0.9,
  orientation = "y"
) +
  geom_point(
    aes(x = x_pos$win_rate + `% Win` * 50, y = n),
    size = 0.55,
    color = pal_accent
  ) +
  geom_richtext(
    aes(
      x = x_pos$win_rate + `% Win` * 50 + 120,
      y = n,
      # label = round(`% Win`, 1)
      label = paste0(round(`% Win`, 1), "%")
    ),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font_body, size = 2.3
  ) +
  
  ## total points area ----
geom_ribbon(
  aes(
    xmin = x_pos$points,
    xmax = x_pos$points + Points * 7,
    y = n
  ),
  fill = pal_points,
  alpha = 0.9,
  orientation = "y"
) +
  geom_point(
    aes(x = x_pos$points + Points * 7, y = n),
    size = 0.55,
    color = pal_accent
  ) +
  geom_richtext(
    aes(
      x = x_pos$points + Points * 7 + 120,
      y = n,
      label = round(Points, 1)
    ),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font_body, size = 2.3
  ) +
  
  ## constructor champion ----
geom_richtext(
  aes(x = x_pos$constructor, y = n, label = Constructor_Champ),
  fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
  hjust = 0, family = font_body, size = 2.5
) +
  
  ## structure lines ----
annotate(
  "segment",
  x = x_line$x,
  xend = x_line$x,
  y = -4, yend = nrow(tbl) + 1,
  linewidth = 0.3,
  color = pal_grid
) +
  annotate(
    "segment",
    x = -2000, xend = 27000,
    y = c(-4, -1, nrow(tbl) + 1),
    yend = c(-4, -1, nrow(tbl) + 1),
    linewidth = 0.3,
    color = pal_grid
  ) +
  
  ## headers ----
add_header(
  x = x_head$x,
  y = -2.5,
  label = c("year", "driver", "team", "poles", "wins", "win %", "points", "constructor", "year"),
  hjust = 0
) +
  
  ## scales ----
annotate(
  "text",
  x = c(x_pos$win_rate - 100, 15800),
  y = 0,
  label = c("0", "100%"),
  hjust = c(0, 1),
  family = font_body,
  size = 3,
  color = pal_text
) +
  annotate(
    "text",
    x = c(x_pos$points - 100, 21200),
    y = 0,
    label = c("0", ceiling(max(tbl$Points) / 100) * 100),
    hjust = c(0, 1),
    family = font_body,
    size = 3,
    color = pal_text
  ) +
  
  ## credits ----
# annotate(
#   "text",
#   x = -2000,
#   y = nrow(tbl) + 2.5,
#   label = "Source: Wikipedia",
#   hjust = 0,
#   family = font_body,
#   size = 2.2,
#   color = pal_text
# ) +


## theme ----
coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(-2300, 27300),
    expand = expansion(add = 1)
  ) +
  scale_y_reverse(expand = expansion(add = 0)) +
  
  ## labels
  labs(
    x = NULL,
    y = NULL,
    title = title,
    caption = caption_text
  ) +
  
  theme_void(base_family = font_body) +
  theme(
    plot.background = element_rect(fill = pal_bg, colour = pal_bg),
    plot.margin = margin(40, 20, 20, 20),
    
    plot.title = element_text(
      hjust = 0.01,
      size = 28,
      family = font_title,
      colour = pal_text,
      margin = margin(0, 0, 2, 0)
    ),
    
    plot.caption = element_markdown(
      hjust = 0,
      size = 8,
      family = font_body,
      colour = pal_text,
      lineheight = 1.1,
      margin = margin(1, 0, 0, 0)
    )
  )

## 8. PRINT ----
print(plot)


## 9. MOVE PLOT TO PROJECT FOLDER ----
last_plot <- list.files(
  here::here("temp_plots"),
  pattern = "\\.png$",
  full.names = TRUE
)

if (length(last_plot) > 0) {
  last_plot <- sort(last_plot) |> tail(1)
  file.rename(last_plot, file.path(plot_dir, "p03-drivers_champions.png"))
}
