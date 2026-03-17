## Challenge: #plot04
## Title:     Formula 1 Champions and Their Winning Teams
## Author:    Raymundo Eduardo Pilz
## Date:      2026-02-23


## 1. LOAD PACKAGES & SETUP ----

### |- libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, ggimage, sysfonts, showtext,
  here, camcorder, magick, glue, htmltools,
  ggraph, patchwork, tidygraph, shadowtext
)

### |- source utility functions ----
source(here::here("utils/R/fonts.R"))
source(here::here("utils/R/social-media.R"))

### |- shortcut ----
shortcut <- here::here("2026/month-02/p04-constructors_champions")
plot_dir <- file.path(shortcut, "plot")

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("temp_plots"), recursive = TRUE, showWarnings = FALSE)

### |- ggplot rendering ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 11,
  units  = "in",
  dpi    = 320
)


## 2. VISUAL IDENTITY ----

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- color palette ----
pal_bg   <- "#F2E7DC"
pal_text <- "#181818"

### |- title and subtitle ----
title_text    <- "Formula 1 World Champions"
subtitle_text <- "and Their Winning Teams"

### |- caption ----
caption_text <- create_social_caption(
  subcaption = "(*) Juan Manuel Fangio competed in the 1954 season for two teams.",
  source_text = "Wikipedia"
)


## 3. LOAD & PREPARE DATA ----

### |- raw data ----
source(here::here("2026/month-02/data/wikipedia/scrap_wikipedia.R"))

tbl_raw <- tbl_drivers

### |- prepare champion-team combinations ----
tbl_plot <- tbl_raw %>%
  separate_rows(Constructor, sep = "/") %>%
  select(Driver, Constructor, Season) %>%
  distinct() %>%
  mutate(
    Season = paste0("'", substr(Season, 3, 4)),
    Season = if_else(Season == "'54", "'54*", Season),
    root   = "F1"
  ) %>%
  arrange(Season)

### |- collapse championship years by driver-team pair ----
tbl_plot <- tbl_plot %>%
  group_by(root, Constructor, Driver) %>%
  summarise(
    Titles = paste(Season, collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    Driver = paste0(Driver, " (", Titles, ")")
  ) %>%
  select(root, Constructor, Driver)

### |- constructor color palette ----
constructor_colors <- c(
  "Ferrari"    = "#A71E14",
  "McLaren"    = "#C95A02",
  "Mercedes"   = "#009F95",
  "Williams"   = "#0D1225",
  "Red Bull"   = "#000C1A",
  "Lotus"      = "#C29B00",
  "Brabham"    = "#062A3A",
  "Tyrrell"    = "#1A3A5A",
  "Renault"    = "#C4A000",
  "Benetton"   = "#067502",
  "BRM"        = "#0E0F49",
  "Cooper"     = "#6B73C2",
  "Matra"      = "#081A4A",
  "Alfa Romeo" = "#1F1B1C",
  "Maserati"   = "#24292E",
  "Brawn"      = "#615D02"
)

### |- count champions per constructor ----
constructor_freq <- tbl_plot %>%
  distinct(Constructor, Driver) %>%
  count(Constructor, name = "n_champions") %>%
  mutate(
    shrink_factor = case_when(
      n_champions == 1 ~ 0.50,
      n_champions == 2 ~ 0.70,
      TRUE ~ 1.00
    )
  )


## 4. BUILD GRAPH STRUCTURE ----

### |- first hierarchy level: root -> constructor ----
tbl_lvl1 <- tbl_plot %>%
  distinct(from = root, to = Constructor)

### |- second hierarchy level: constructor -> driver ----
tbl_lvl2 <- tbl_plot %>%
  distinct(from = Constructor, to = Driver)

### |- edge list ----
tbl_edges <- bind_rows(tbl_lvl1, tbl_lvl2)

### |- graph object ----
tbl_graph <- as_tbl_graph(tbl_edges)


## 5. EXTRACT NODE POSITIONS ----

### |- helper dendrogram used only to capture coordinates ----
p_helper <- ggraph(tbl_graph, layout = "dendrogram", circular = FALSE) +
  geom_edge_diagonal() +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  coord_fixed() +
  theme_void()

### |- build helper once ----
build_helper <- ggplot_build(p_helper)

### |- node labels ----
node_labels <- build_helper$data[[3]] %>%
  select(x, y, label) %>%
  mutate(
    is_driver      = label %in% tbl_plot$Driver,
    is_constructor = label %in% tbl_plot$Constructor
  ) %>%
  left_join(constructor_freq, by = c("label" = "Constructor"))

### |- assign colors to nodes ----
node_labels <- node_labels %>%
  mutate(
    color = case_when(
      is_constructor ~ constructor_colors[label],
      is_driver ~ map_chr(label, \(x) {
        team_name <- tbl_plot %>%
          filter(Driver == x) %>%
          pull(Constructor) %>%
          .[1]
        
        scales::alpha(constructor_colors[team_name], 0.9)
      }),
      TRUE ~ pal_text
    ),
    shrink_factor = replace_na(shrink_factor, 1),
    y_constructor = if_else(is_constructor, y * shrink_factor, y)
  )

### |- edge coordinates ----
tbl_edges_plot <- build_helper$data[[1]] %>%
  filter(group > 8, y < 0.95)

### |- constructor nodes only ----
constructor_nodes <- node_labels %>%
  filter(is_constructor) %>%
  select(constructor = label, x_c = x, y_c = y, shrink_factor)

### |- match each edge group to its constructor ----
group_constructor_map <- tbl_edges_plot %>%
  group_by(group) %>%
  nest() %>%
  mutate(
    constructor = map_chr(data, \(df_group) {
      dists <- constructor_nodes %>%
        mutate(
          dist = map_dbl(seq_len(n()), \(i) {
            min((df_group$x - x_c[i])^2 + (df_group$y - y_c[i])^2)
          })
        )
      
      dists %>%
        slice_min(dist, n = 1) %>%
        pull(constructor)
    })
  ) %>%
  select(-data) %>%
  left_join(
    constructor_nodes %>% select(constructor, y_c, shrink_factor),
    by = "constructor"
  )

### |- get driver endpoint by group ----
group_driver_max <- tbl_edges_plot %>%
  group_by(group) %>%
  summarise(
    y_driver = max(y, na.rm = TRUE),
    .groups = "drop"
  )

### |- join metadata back to edge coordinates ----
tbl_edges_plot <- tbl_edges_plot %>%
  left_join(group_constructor_map, by = "group") %>%
  left_join(group_driver_max, by = "group") %>%
  mutate(
    shrink_factor = replace_na(shrink_factor, 1),
    y_c_new = y_c * shrink_factor
  ) %>%
  mutate(
    y_adj = case_when(
      is.na(y_c) ~ y,
      y <= y_c ~ y * shrink_factor,
      y > y_c & y_driver > y_c ~ y_c_new + (y - y_c) * ((y_driver - y_c_new) / (y_driver - y_c)),
      TRUE ~ y
    )
  )


## 6. BUILD MAIN PLOT ----

p_main <- 
  ggplot(
    tbl_edges_plot, aes(x, y_adj)
  ) +
  
  ### |- connecting branches ----
  geom_path(
    aes(group = group),
    color = pal_text,
    linewidth = 0.3
  ) +
  
  ### |- driver labels ----
  shadowtext::geom_shadowtext(
    data = node_labels %>% filter(is_driver),
    aes(
      x = x,
      y = y,
      label = label,
      color = color
    ),
    family   = fonts$text,
    size     = 3.5,
    fontface = "bold",
    angle    = 90,
    hjust    = 0,
    nudge_y  = 0.05,
    bg.color = pal_bg
  ) +
  
  ### |- constructor labels ----
  geom_text(
    data = node_labels %>% filter(is_constructor),
    aes(
      x = x,
      y = y_constructor,
      label = str_wrap(label, 10),
      color = color
    ),
    family     = fonts$text,
    size       = 4,
    fontface   = "bold",
    angle      = 90,
    hjust      = 1,
    lineheight = 0.75
  ) +
  
  ### |- scales and coordinates ----
  scale_y_reverse() +
  scale_color_identity() +
  coord_radial(
    rotate.angle = TRUE,
    inner.radius = 0.3,
    clip         = "off",
    start        = 0.25 * pi,
    end          = 1.75 * pi
  ) +
  
  ### |- labels ----
  labs(
    title    = title_text,
    subtitle = subtitle_text
  ) +
  
  ### |- theme ----
  theme_void() +
  theme(
    plot.background = element_rect(fill = pal_bg, colour = NA),
    plot.margin     = margin(t = 10, r = 60, b = 80, l = 60),
    
    plot.title = element_text(
      family     = fonts$title,
      face       = "bold",
      size       = 50,
      lineheight = 1.1,
      colour     = pal_text,
      hjust      = 0.5
    ),
    
    plot.subtitle = element_text(
      family     = fonts$subtitle,
      size       = 30,
      lineheight = 1.2,
      colour     = pal_text,
      hjust      = 0.5,
      margin     = margin(t = 10, b = -120)
    )
  )


## 7. FINAL PLOT ----

plot <- p_main +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.background = element_rect(fill = pal_bg, colour = NA),
      plot.caption = element_markdown(
        family = fonts$caption,
        size   = rel(0.75),
        colour = pal_text,
        hjust  = 0
      )
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
  file.rename(last_plot, file.path(plot_dir, "p04-champions_teams.png"))
}