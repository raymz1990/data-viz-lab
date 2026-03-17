## Challenge: #plot01
## Data:      The Formula One Legacy of Ayrton Senna
## Author:    Raymundo Eduardo Pilz
## Date:      2026-02-02


## 1. LOAD PACKAGES & SETUP ----

### |- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, ggimage, sysfonts, showtext,
  here, camcorder, magick
)

### |- source utility functions ----
source(here::here("utils/R/fonts.R"))
source(here::here("utils/R/social-media.R"))

### |- shortcut ----
shortcut <- here::here("2026/month-02/p01-senna_carrer")
img_dir  <- file.path(shortcut, "images")
plot_dir <- file.path(shortcut, "plot")

trophy_path <- file.path(img_dir, "trophy.png")
helmet_path <- file.path(img_dir, "senna-helmet.png")

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("temp_plots"), recursive = TRUE, showWarnings = FALSE)

### |- recording and figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 16,
  height = 9,
  units  = "in",
  dpi    = 300
)

setup_fonts()


## 2. CREATE DATA ----
df <- tibble::tribble(
  ~year, ~team, ~model_car, ~pts, ~position,
  1984, "Toleman",  "Toleman TG184",   13, 9,
  1985, "Lotus",    "Lotus 97T",       38, 4,
  1986, "Lotus",    "Lotus 98T",       55, 4,
  1987, "Lotus",    "Lotus 99T",       57, 3,
  1988, "McLaren",  "McLaren MP44",    90, 1,
  1989, "McLaren",  "McLaren MP45",    60, 2,
  1990, "McLaren",  "McLaren MP45B",   78, 1,
  1991, "McLaren",  "McLaren MP46",    96, 1,
  1992, "McLaren",  "McLaren MP47",    50, 4,
  1993, "McLaren",  "McLaren MP48",    73, 2,
  1994, "Williams", "Williams FW16",    0, 0
)

pal_colors <- tibble::tribble(
  ~team,      ~pal_color,
  "Toleman",  "#1F77B4",
  "Lotus",    "#050200",
  "McLaren",  "#D62728",
  "Williams", "#0033A0"
)

df <- df %>%
  left_join(pal_colors, by = "team")


## 3. CHECK DATA ----
glimpse(df)


## 4. PREPARE AUXILIARY DATA ----

### |- extract data for championships (world champion years) ----
champion <- df %>%
  filter(position == 1) %>%
  select(year, pts)

### |- data for team segment connections ----
df_team_ranges <- df %>%
  group_by(team, pal_color) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    x = mean(year),
    .groups = "drop"
  )

### |- data for images bellow chart ----
images <- tibble(
  name = unique(df$model_car),
  pos  = seq(1984, 1994, length.out = 11)
) %>%
  mutate(
    path = file.path(
      img_dir,
      paste0(str_replace_all(tolower(name), " ", "_"), ".png")
    )
  )

### |- dataframe for connector segments between years and images ----
connectors <- tibble(
  team = c("Toleman", rep("Lotus", 3), rep("McLaren", 3), "Williams"),
  x    = c(1984, 1985, 1986, 1987, 1988, 1990.5, 1993, 1994),
  xend = c(1984, 1985, 1986, 1987, 1988, 1990.5, 1993, 1994),
  y    = c(-34, -28, -34, -28, -28, -34, -28, -34),
  yend = c(-20, -20, -28, -20, -20, -28, -20, -20)
) %>%
  left_join(pal_colors, by = "team")

### |- gold trophy
trophy_gold <- image_read(trophy_path) |>
  image_colorize(opacity = 100, color = "#FFD700")

image_write(trophy_gold, "trophy_gold.png")

### |- legend ----
df_legend <- tibble(
  image = "trophy_gold.png",
  text  = "World Champion",
  x_pos = 1984,
  y_pos = -45
)


## 5. DEFINE VISUAL ELEMENTS ----

### |-  theme palette colors ----
pal_bg    <- "#F2F2F2"
pal_text  <- "#404040"
pal_title <- "#FFD700"

### |- title for the plot (ggtext::element_textbox_simple) ----
title <- paste0(
  '<span style="color:#000000;font-weight:bold;">THE </span>',
  '<span style="color:#FF1801;font-weight:bold;">FORMULA ONE </span>',
  '<span style="color:#000000;font-weight:bold;">LEGACY OF </span>',
  '<span style="color:#009440;font-weight:bold;">AYRTON </span>',
  '<span style="color:#FFD700;font-weight:bold;">SENNA</span>'
)

### |- data for custom legend ----
caption_text <- create_social_caption(
  subcaption = "",
  source_text = "Wikipedia"
)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.1*a", channel = "alpha")
}


## 6. BUILD MAIN PLOT ----
plot <- ggplot(df, aes(x = year)) +
  
  ## horizontal gridlines
  geom_segment(
    data = tibble(y = seq(0, 100, 10)),
    aes(x = 1984, xend = 1994, y = y, yend = y),
    linewidth = 0.2, alpha = 0.12
  ) +
  
  ## bars for total points
  geom_segment(
    aes(xend = year, y = 0, yend = pts, color = pal_color),
    linewidth = 20
  ) +
  
  ## add point on bars
  geom_text(
    aes(y = pts + 2, label = pts, color = pal_color),
    size = 5,
    fontface = "bold",
    vjust = 0.1
  ) +
  
  ## text for years
  geom_text(
    aes(y = -5, label = year, color = pal_color),
    size = 5,
    fontface = "bold",
    vjust = 0.3
  ) +
  
  ## add trophy on champion bars
  geom_image(
    data = champion,
    aes(x = year, y = pts - 5, image = "trophy_gold.png"),
    size = 0.05
  ) +
  
  ## custom legend
  geom_image(
    data = df_legend,
    aes(x = x_pos, y = y_pos, image = image),
    size = 0.05
  ) +
  
  geom_text(
    data = df_legend,
    aes(x = x_pos + 0.2, y = y_pos, label = text),
    color = pal_text,
    fontface = "bold",
    hjust = 0
  ) +
  
  ## team segment lines
  geom_segment(
    data = df_team_ranges,
    aes(x = min_year, xend = max_year, y = -28, yend = -28, color = pal_color),
    linewidth = 1
  ) +
  
  ## connector segments 
  geom_segment(
    data = connectors,
    aes(x = x, xend = xend, y = y, yend = yend, color = pal_color),
    linewidth = 1
  ) +
  
  ## images below chart
  geom_image(
    data = images,
    aes(x = pos, y = -14, image = path),
    size = 0.07
  ) +
  
  geom_richtext(
    data = df_team_ranges,
    aes(x = x, y = -37, label = team, color = pal_color),
    fill = NA,
    label.color = NA,
    hjust = 0.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  ## adjust into the scale y
  scale_y_continuous(
    limits = c(-50, 100),
    breaks = seq(0, 100, 10),
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_color_identity() +
  
  ## labels and theme
  labs(
    title = title,
    x = NULL,
    y = "Total Points",
    caption = caption_text
  ) +
  
  theme(
    panel.background = element_rect(fill = pal_bg, color = NA),
    plot.background  = element_rect(fill = pal_bg),
    plot.title = element_textbox_simple(
      size = 20,
      halign = 0.5,
      color = pal_title,
      face = "bold",
      margin = margin(b = 20)
    ),
    text = element_text(color = pal_text, size = 12),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10),
    plot.caption = element_textbox_simple(
      hjust = 0.01,
      color = "#818990",
      margin = margin(b = 10),
      size = 10
    ),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = pal_text, size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


## 7. BUILD FINAL COMPOSITION ----
plot_final <- ggplot() +
  
  # add main plot
  annotation_custom(
    ggplotGrob(plot),
    xmin = 3, xmax = 7,
    ymin = 1.25, ymax = 4.75
  ) +
  
  # add helmet image
  geom_image(
    aes(x = 3.3, y = 4, image = helmet_path),
    image_fun = transparent,
    size = 0.4
  ) +
  
  # add caption
  scale_x_continuous(limits = c(3, 7), expand = c(0, 0)) +
  scale_y_continuous(limits = c(1, 5), expand = c(0, 0)) +
  theme_void()


## 8. PRINT ----
print(plot_final)

## 9. MOVE PLOT TO PROJECT FOLDER ----
last_plot <- list.files(here::here("temp_plots"), pattern = "\\.png$", full.names = TRUE)

if (length(last_plot) > 0) {
  last_plot <- sort(last_plot) |> tail(1)
  file.rename(last_plot, file.path(plot_dir, "senna.png"))
}
