# Loading libraries ----
library(tidyverse)
library(tidygraph)
library(ggraph)
library(camcorder)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)
# library(htmltools)
library(patchwork)

gg_record(dir = "2025/month-01/p04-constructors_champions/plot/temp", device = "png", width = 14, height = 11, units = "in", dpi = 320)


# Loading the data
source("2025/month-01/data/wikipedia/scrap_wikipedia.R")
suppressMessages(source("2025/utils/R/fonts.R"))
source("2025/utils/R/social-media.R")

# loading data ----
tbl <- tbl_drivers 

# Summarise table
tbl <- tbl %>%
  separate_rows(Constructor, sep = "/") %>% 
  add_count(Driver, Constructor, name = "n") %>% 
  arrange(Season) %>% 
  select(Driver, Constructor, Season) %>%
  distinct() %>% 
  mutate(Season = paste0("'", substr(Season, 3, 4))) %>%
  mutate(Season = ifelse(Season == "'54", "'54*", Season)) %>%
  mutate(name = "F1", .before = 1)

tbl <- tbl %>%
  group_by(name, Constructor, Driver) %>%
  summarise(Titles = paste(Season, collapse = ", "), .groups = 'drop') %>%
  mutate(Driver = paste0(Driver, " (", Titles, ")")) %>%
  select(name, Constructor, Driver)


# Definindo as cores clássicas dos construtores
constructor_colors <- c(
  "Ferrari" = "#DA291C",       # Vermelho Ferrari
  "McLaren" = "#FF7300",       # Laranja McLaren
  "Mercedes" = "#00F5D0",     # 000000
  "Williams" = "#121a38",      # Azul Williams
  "Red Bull" = "#00162b",      # Azul Red Bull
  "Lotus" = "#F4C12A",         # Dourado Lotus
  "Brabham" = "#08314A",       # Verde Brabham
  "Tyrrell" = "#254A74",       # Verde Tyrrell
  "Renault" = "#FFD700",       # Amarelo Renault
  "Benetton" = "#09A603",      # Verde Benetton
  "BRM" = "#131563",           # Vermelho BRM
  "Cooper" = "#9196F2",        # Azul Cooper
  "Matra" = "#102372",         # Azul Matra
  "Alfa Romeo" = "#262324",    # Vermelho Alfa Romeo
  "Maserati" = "#343a40",
  "Brawn" = "#8C8303"
)

# Atribuindo a cor do time para cada piloto baseado na equipe com mais títulos
pilot_colors <- tbl %>%
  group_by(Driver, Constructor) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Driver) %>%
  slice_max(n, with_ties = FALSE) %>%  # Mantém a equipe onde o piloto tem mais títulos
  ungroup() %>%
  mutate(color = constructor_colors[Constructor])


# Definindo a paleta de cores
pal <- c(
  "#F2E7DC",  # Fundo do gráfico
  "#181818"   # Cor de texto
)


# level 1 table
tbl_lvl1 <- tbl %>% 
  distinct(from = name, to = Constructor) 

# level 2 table
tbl_lvl2 <- tbl %>% 
  distinct(from = Constructor, to = Driver) 

# list 
tbl_list <- rbind(tbl_lvl1, tbl_lvl2) 

tbl_graph <- as_tbl_graph(tbl_list)

p <- ggraph(tbl_graph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

node_labels <- ggplot_build(p) %>% 
  .$data %>% 
  .[[3]] %>% 
  select(1:3) %>% 
  mutate(
    driver = if_else(label %in% tbl$Driver, TRUE, FALSE),
    constructor = if_else(label %in% tbl$Constructor, TRUE, FALSE)
  )

# Adicionando cores aos nós
node_labels <- node_labels %>%
  mutate(color = case_when(
    constructor ~ constructor_colors[label],
    driver ~ sapply(label, function(x) {
      constr <- tbl %>% filter(Driver == x) %>% pull(Constructor)
      if (length(constr) > 1) {
        scales::alpha(constructor_colors[constr[1]], 0.8)
      } else {
        scales::alpha(constructor_colors[constr], 0.8)
      }
    }),
    TRUE ~ "#000000"  # Cor padrão para outros nós
  ))

    
tbl_points <- ggplot_build(p) %>% 
  .$data %>% 
  .[[1]] %>% 
  filter(group > 8) %>%
  filter(y < 0.95)

f1 <- "Montserrat"
f1b <- "Montserrat"

### |-  titles and caption ----
title_text <- str_glue("Formula 1 Champions")

subtitle_text <- str_glue("and Their Winning Teams")

# Create caption
caption_text <- create_social_caption(
  subcaption = "(*) Juan Manuel Fangio competed in the 1954 season for two teams.",
  source_text =  "Wikipedia" 
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()


ggplot(data = tbl_points,
             aes(x, y)) +
  
  geom_path(
    aes(group = group),
    color = pal[2],
    size = 0.3) +
  
  # Pilotos
  shadowtext::geom_shadowtext(
    data = node_labels %>%
      filter(driver),
    aes(x, y, label = label, size = 3.5, fontface = "bold", color = color),
    family = f1b,
    angle = 90,
    hjust = 0,
    nudge_y = 0.05,
    bg.color = pal[1]) +
  
  # Construtores
  geom_text(
    data = node_labels %>%
      filter(constructor),
    aes(x, y, label = str_wrap(label, 10), color = color),
    family = f1b,
    angle = 90,
    hjust = 1,
    nudge_y = 0.05,
    fontface = "bold",
    lineheight = 0.75) +
  
  scale_y_reverse() +
  scale_size_identity() +
  scale_color_identity() +
  coord_radial(
    rotate.angle = TRUE,
    inner.radius = 0.3,
    clip = "off",
    end = 1.75 * pi,
    start = 0.25 * pi) +
  
  labs(
    title = title_text,
    subtitle = subtitle_text
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = pal[1], color = NA),
    # plot.title = element_blank(),
    # plot.subtitle = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 60, b = 80, l = 60),
    plot.title = element_text(
      family = fonts$title,
      # margin = margin(50, 0, -60, 0),
      face = "bold",
      size = 50,
      lineheight = 1.1,
      color = pal[2],
      # margin = margin(t = 10, b = -100),
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size   = 30,
      family = fonts$subtitle,
      color = pal[2],
      lineheight = 1.2,
      margin = margin(t = 10, b = -120),
      hjust = 0.5
    )
    # plot.caption = element_textbox_simple(
    #   hjust = 0.01,
    #   vjust = 10,
    #   color = pal[2],
    #   margin = margin(b = 10),
    #   size = 10
    # )
  )

p1

# Final Plot
p1 +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.background = element_rect(fill = pal[1], color = NA),
      plot.caption = element_markdown(
        size   = rel(0.75),
        family = fonts$caption,
        color = pal[2],
        hjust  = 0
      )
    )
  )

