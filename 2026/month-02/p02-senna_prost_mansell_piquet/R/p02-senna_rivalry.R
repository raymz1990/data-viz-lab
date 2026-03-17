## Challenge: #plot02
## Title:     The 80s F1 Rivalry: A Clash of Titans
## Author:    Raymundo Eduardo Pilz
## Date:      2026-02-09


## 1. LOAD PACKAGES & SETUP ----

### |- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, ggimage, sysfonts, showtext,
  ggplot2, ggbump, ggstar, ggarrow,
  here, camcorder, magick, htmltools
)

### |- source utility functions ----
source(here::here("utils/R/fonts.R"))
source(here::here("utils/R/social-media.R"))

### |- shortcut ----
shortcut <- here::here("2026/month-02/p02-senna_prost_mansell_piquet")
img_dir  <- file.path(shortcut, "images")
plot_dir <- file.path(shortcut, "plot")

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("temp_plots"), recursive = TRUE, showWarnings = FALSE)

### |- recording and figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 5460,
  height = 2880,
  units  = "px",
  dpi    = 300
)

setup_fonts()


## 2. CREATE DATA ----

### |- define ranking threshold ----
max_point <- 10

### |- Senna dataset ----
df_senna <- tibble(
  year     = 1984:1994,
  points   = c(13, 38, 55, 57, 90, 60, 78, 96, 50, 73, 0),
  position = c(9, 4, 4, 3, 1, 2, 1, 1, 4, 2, 38)
)

### |- Piquet dataset ----
df_piquet <- tibble(
  year     = 1978:1991,
  points   = c(0, 3, 54, 50, 20, 59, 29, 21, 69, 73, 22, 12, 43, 26.5),
  position = c(28, 16, 2, 1, 11, 1, 5, 8, 3, 1, 6, 8, 3, 6)
)

### |- Prost dataset ----
df_prost <- tibble(
  year     = 1980:1993,
  points   = c(5, 43, 34, 57, 71.5, 73, 72, 46, 87, 76, 71, 34, 0, 99),
  position = c(16, 5, 4, 2, 2, 1, 1, 4, 2, 1, 2, 5, 99, 1)
)

### |- Mansell dataset ----
df_mansell <- tibble(
  year     = 1980:1995,
  points   = c(0, 8, 7, 10, 13, 31, 70, 61, 12, 38, 37, 72, 108, 0, 13, 0),
  position = c(30, 14, 14, 13, 10, 6, 2, 2, 9, 4, 5, 2, 1, 99, 9, 29)
)

### |- main dataset ----
df <- bind_rows(
  mutate(df_senna, driver = "Senna"),
  mutate(df_piquet, driver = "Piquet"),
  mutate(df_prost, driver = "Prost"),
  mutate(df_mansell, driver = "Mansell")
)

### |- adjust rank and helper columns ----
df <- df %>%
  mutate(season = paste0("'", substr(year, 3, 4))) %>%
  mutate(rank = if_else(position > max_point,
                        11L,
                        position))

### |- Identify the first and last season for each driver ----
df <- df %>%
  group_by(driver) %>%
  mutate(first_season = min(year),
         last_season = max(year),
         d_first_season = if_else(year == first_season, 1, 0)) %>%
  ungroup()

### |- create active groups for top 10 segments ----
df <- df %>%
  arrange(driver, year) %>%
  group_by(driver) %>%
  mutate(
    lag_zero = if_else(lag(rank) %in% c(max_point + 1L, NA) & rank <= max_point, 1L, 0L, 0L)
  ) %>%
  ungroup() %>%
  mutate(group = cumsum(lag_zero))

### |- define custom colors ----
pal_colors <- c(
  "Senna"   = "#fad02c",
  "Prost"   = "#f41823",
  "Piquet"  = "#0d733c",
  "Mansell" = "#025492"
)


## 3. CHECK DATA ----
glimpse(df)


## 4. PREPARE AUXILIARY DATA ----

### |- driver images and custom legend positions ----
images <- df %>%
  filter(d_first_season == 1) %>%
  select(driver, year) %>%
  arrange(year) %>%
  mutate(
    x_pos  = c(1978.0, 1980.5, 1979.9, 1983.7),
    y_pos  = c(11.0, 11.0, 11.0, 9.0),
    x_leg  = rep(1978.0, 4),
    x_text = rep(1978.3, 4),
    y_leg  = c(13.0, 16.0, 14.5, 17.5),
    path   = file.path(
      img_dir,
      paste0(str_replace_all(tolower(driver), " ", "_"), ".png")
    )
  )

### |- champion seasons ----
df_champions <- df %>%
  filter(rank == 1)

### |- reference labels for y-axis ----
df_reference <- tibble(
  x = 1977,
  y = 1:10
)


## 5. DEFINE VISUAL ELEMENTS ----

### |- palette ----
pal_bg    <- "#050200"
pal_text  <- "gray90"
pal_title <- "#FFD700"

### |- title ----
title <- "The 80s F1 Rivalry: A Clash of Titans"

### |- subtitle ----
subtitle <- "A Decade of Dominance: Prost vs Senna vs Piquet vs Mansell in Formula 1"

### |- caption ----
caption_text <- create_social_caption(
  subcaption  = "",
  source_text = "Wikipedia"
)


## 6. BUILD MAIN PLOT ----
plot <- ggplot(
  data = df,
  aes(x = year, y = rank, group = driver)
) +
  
  ## background bump lines for all seasons
  geom_bump(
    aes(color = driver),
    smooth = 15,
    linewidth = 1.5,
    alpha = 0.2
  ) +
  
  ## highlight top-10 segments only
  geom_bump(
    data = df %>% filter(rank <= max_point),
    aes(x = year, y = rank, group = group, color = driver),
    smooth = 15,
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  
  ## driver portraits near first appearance
  geom_image(
    data = images,
    aes(x = x_pos, y = y_pos, image = path),
    size = 0.07,
    inherit.aes = FALSE
  ) +
  
  ## championship stars
  geom_star(
    data = df_champions,
    aes(starshape = "pentagram", color = driver, fill = driver),
    size = 5
  ) +
  
  ## annotation: Senna
  geom_textbox(
    data = tibble(
      x = 1994.1,
      y = 11.5,
      label = "<p style='font-family:Roboto;font-size:10pt;'>
              <span style='color:#e5e5e5;'><b>Senna's</b> fatal crash</span>
              </p>"
    ),
    aes(x = x, y = y, label = label),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_arrow_curve(
    data = tibble(
      x = 1994.2,
      xend = 1993.99,
      y = 11.5,
      yend = 11
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "grey90",
    curvature = -0.5,
    linewidth = 0.5,
    lineend = "butt",
    angle = 90,
    inherit.aes = FALSE
  ) +
  
  ## annotation: Mansell
  geom_textbox(
    data = tibble(
      x = 1993.4,
      y = 12.0,
      label = "<p style='font-family:Roboto;font-size:10pt;'>
              <span style='color:#e5e5e5;'><b>Mansell's</b>&nbsp;first retirement</span>
              </p>"
    ),
    aes(x = x, y = y, label = label),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_arrow_curve(
    data = tibble(
      x = 1993.5,
      xend = 1993,
      y = 12,
      yend = 11
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "grey90",
    curvature = -0.5,
    linewidth = 0.5,
    lineend = "butt",
    angle = 90,
    inherit.aes = FALSE
  ) +
  
  ## annotation: Prost
  geom_textbox(
    data = tibble(
      x = 1992.5,
      y = 12.5,
      label = "<p style='font-family:Roboto;font-size:10pt;'>
              <span style='color:#e5e5e5;'><b>Prost's</b> first retirement</span>
              </p>"
    ),
    aes(x = x, y = y, label = label),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_arrow_curve(
    data = tibble(
      x = 1992.5,
      xend = 1992,
      y = 12.5,
      yend = 11
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "grey90",
    curvature = -0.5,
    linewidth = 0.5,
    lineend = "butt",
    angle = 90,
    inherit.aes = FALSE
  ) +
  
  ## custom y reference dots
  geom_point(
    data = df_reference,
    aes(x = x, y = y),
    color = pal_text,
    size = 10,
    pch = 21,
    inherit.aes = FALSE
  ) +
  
  geom_text(
    data = df_reference,
    aes(x = x, y = y, label = y),
    color = pal_text,
    inherit.aes = FALSE
  ) +
  
  ## custom legend portraits
  geom_image(
    data = images,
    aes(x = x_leg, y = y_leg, image = path),
    size = 0.07,
    inherit.aes = FALSE
  ) +
  
  ## custom legend labels
  geom_textbox(
    data = images %>% filter(driver == "Piquet"),
    aes(
      x = x_text,
      y = y_leg,
      label = "<p style='font-family:Roboto;font-size:12pt;'>
               <span style='color:#0d733c;'><b>Nelson Piquet</b> (1978-1991)</span>
               </p>"
    ),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_textbox(
    data = images %>% filter(driver == "Prost"),
    aes(
      x = x_text,
      y = y_leg,
      label = "<p style='font-family:Roboto;font-size:12pt;'>
               <span style='color:#f41823;'><b>Alain Prost</b> (1980-1991, 1993)</span>
               </p>"
    ),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_textbox(
    data = images %>% filter(driver == "Mansell"),
    aes(
      x = x_text,
      y = y_leg,
      label = "<p style='font-family:Roboto;font-size:12pt;'>
               <span style='color:#025492;'><b>Nigel Mansell</b> (1980-1992, 1994-1995)</span>
               </p>"
    ),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  geom_textbox(
    data = images %>% filter(driver == "Senna"),
    aes(
      x = x_text,
      y = y_leg,
      label = "<p style='font-family:Roboto;font-size:12pt;'>
               <span style='color:#fad02c;'><b>Ayrton Senna</b> (1984-1994)</span>
               </p>"
    ),
    fill = pal_bg,
    box.size = NA,
    hjust = 0,
    width = unit(4, "in"),
    inherit.aes = FALSE
  ) +
  
  ## scales
  scale_color_manual(values = pal_colors) +
  scale_fill_manual(values = pal_colors) +
  
  scale_x_continuous(
    breaks = df %>% distinct(year) %>% arrange(year) %>% pull(year),
    labels = df %>% distinct(year, season) %>% arrange(year) %>% pull(season),
    position = "top"
  ) +
  
  # scale_y_reverse(
  #   limits = c(18, 1),
  #   expand = expansion(mult = c(0.01, 0.01))
  # ) +
  
  scale_y_reverse() +
  
  ## labels
  labs(
    x = NULL,
    y = NULL,
    title = title,
    subtitle = subtitle,
    caption = caption_text
  ) +
  
  ## theme
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    text = element_text(family = "Roboto Slab"),
    plot.margin = margin(t = 20, b = 10, l = 15, r = 15),
    plot.title = element_text(
      face = "bold",
      family = "Roboto Slab",
      size = 24,
      hjust = 0.09,
      color = pal_text
    ),
    plot.subtitle = element_text(
      family = "Roboto Slab",
      size = 16,
      hjust = 0.1,
      color = pal_text,
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0.01,
      size = 10,
      color = "#818990",
      margin = margin(t = 10)
    ),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, color = pal_text),
    panel.background = element_rect(fill = pal_bg, color = "transparent"),
    plot.background = element_rect(fill = pal_bg)
  )


## 7. BUILD FINAL COMPOSITION ----
# plot_final <- ggplot() +
#   annotation_custom(
#     ggplotGrob(plot),
#     xmin = 3, xmax = 7,
#     ymin = 1.25, ymax = 4.75
#   ) +
#   scale_x_continuous(limits = c(3, 7), expand = c(0, 0)) +
#   scale_y_continuous(limits = c(1, 5), expand = c(0, 0)) +
#   theme_void()


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
  file.rename(last_plot, file.path(plot_dir, "p02_80_rivalry.png"))
}