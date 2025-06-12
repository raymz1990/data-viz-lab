# Clear workspace ----
rm(list = ls())

# Load required libraries ----
suppressPackageStartupMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    tidyverse,
    # waffle, # remotes::install_github("hrbrmstr/waffle")
    ggtext,
    ggimage,
    showtext,
    # janitor,
    # skimr,
    scales,
    glue,
    here,
    # lubridate,
    # ggpubr,
    patchwork#,
    # camcorder,
    # ggrepel,
    # directlabels,
    # gghighlight,
    # sysfonts,
    # rvest,
    # purrr,
    # Cairo
  )
})

# Load data sources ----
source("2025/month-01/p05-f1/R/p05-database.R")
suppressMessages(source("2025/utils/R/fonts.R"))
source("2025/utils/R/social-media.R")

# Color palette ----
pal <- c(
  "#38383f",  # backgorund,
  "#f7f4f1",
  "#E10600",  # graphic collor,
  "#15151E",
  "#f6f058"
)

# |-  fonts ----
setup_fonts()
fonts <- get_font_families()

# images directory ----
img <- "2025/month-01/p05-f1/images/"

# DRIVERS
g1_summary <- g1_data %>% 
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(ymax = cumsum(freq),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         img = ifelse(type == "champion", paste0(img, "champion.png"), paste0(img, "pilot.png")),
         group = "Driver")

# CONSTRUCTORS
g2_summary <- g2_data %>% 
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(ymax = cumsum(freq),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         img = ifelse(type == "champion", paste0(img, "champion.png"), paste0(img, "car.png")),
         group = "Constructor")

# JUNÇÃO
g_all <- bind_rows(g1_summary, g2_summary)



## create the donut chart ----
g1 <- 
  ggplot(
    data = g1_data,
    aes(xmax = 4, xmin = 3, ymax = ymax, ymin = ymin, fill = factor(type))) +
  
  geom_rect() +
  
  coord_polar(theta = "y") +
  xlim(c(1, 4.5)) +
  
  scale_fill_manual(values = c(pal[5], pal[3])) + 
  
  ## add images into the plot
  geom_image(
    mapping = aes(x = 3.7, y = labelPosition, image = img), 
    size = 0.06) +
  
  ## add category values
  geom_text(
    mapping = aes(x = 4.3, y = labelPosition, label = n),
    size = 2.5,
    color = pal[4],
    family = fonts$text,
    fontface= "bold") + 
  
  ## add total drivers count in the center
  geom_text(
    mapping = aes(x = 1, y = 0, label = sum(n)),
    size = 7,
    # vjust = 0.5
    color = pal[4],
    family = fonts$text,
    fontface= "bold") +
  
  ## title
  labs(title = "DRIVERS") +
  
  ## theme
  theme_void() +
  theme(
    panel.background = element_rect(fill = pal[2], color = NA),
    plot.background = element_rect(fill = pal[2], color = NA),
    plot.title = element_textbox_simple(
      family = fonts$title,
      size = 16,
      halign = 0.5,
      color = pal[4],
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.position = "none"
  )

# Gráfico circular por grupo com roscas para campeões e não campeões
ggplot(
  data = g_all,
  aes(x = group, y = freq, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = pal[2]) +
  coord_polar(theta = "y") +
  facet_wrap(~ group, ncol = 2) +
  
  # Paleta customizada para campeões e não campeões
  scale_fill_manual(values = c("champion" = pal[5], "nonchampion" = pal[3])) +
  
  # Remove eixos, legendas e personaliza fundo
  theme_void() +
  theme(
    strip.text = element_text(
      family = fonts$title,
      size = 20,
      face = "bold",
      color = pal[4]
    ),
    panel.background = element_rect(fill = pal[2], color = NA),
    plot.background = element_rect(fill = pal[2], color = NA),
    legend.position = "none"
  )


## print plot g1 ----  
# g1
  
# g2 ----
## prepare dataset ----
g2_data <- g2_data %>% 
  arrange(desc(type)) %>%
  group_by(type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(ymax = cumsum(freq)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelPosition = (ymax + ymin) / 2) %>%
  mutate(img = ifelse(type == "champion", paste0(img, "champion.png"),
                      paste0(img, "car.png")))

## create the donut chart ----
g2 <- 
  ggplot(
    data = g2_data,
    aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = factor(type))) +
  
  geom_rect() +
  
  coord_polar(theta = "y") +
  xlim(c(1, 4.5)) +
  
  scale_fill_manual(values = c(pal[5], pal[3])) + 
  
  ## add images into the plot
  geom_image(
    mapping = aes(x = 3.7, y = labelPosition, image = img), 
    size = 0.06) +
  
  ## add category values
  geom_text(
    mapping = aes(x = 4.3, y = labelPosition, label = n),
    size = 2.5,
    color = pal[4],
    family = fonts$text,
    fontface= "bold") + 
  
  ## add total drivers count in the center
  geom_text(
    mapping = aes(x = 1, y = 0, label = sum(n)),
    size = 7,
    # vjust = 0.5
    color = pal[4],
    family = fonts$text,
    fontface= "bold") +
  
  ## title
  labs(title = "CONSTRUCTORS") +
  
  ## theme
  theme_void() +
  theme(
    panel.background = element_rect(fill = pal[2], color = NA),
    plot.background = element_rect(fill = pal[2], color = NA),
    plot.title = element_textbox_simple(
      family = fonts$title,
      size = 16,
      halign = 0.5,
      color = pal[4],
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.position = "none"
  )

## Print plot g2 ----
# g2

# g3 ----
## prepare dataset ----

# Function to get the main image from Wikipedia
get_wikipedia_image <- function(wiki_url) {
  page <- read_html(wiki_url)
  
  img_url <- page %>%
    html_node("table.infobox img") %>%
    html_attr("src")
  
  if (!is.na(img_url)) {
    return(paste0("https:", img_url))  # Ensure the URL is complete
  } else {
    return(NA)
  }
}

# Break names
g3_data <- g3_data %>%
  mutate(name = gsub(" ", "\n", name))

# Apply function to fetch images for top 6 champions
g3_data <- g3_data %>%
  mutate(wiki_image = sapply(url, get_wikipedia_image))

# Create replicated trophy images for each champion
g3_data <- g3_data %>%
  mutate(trophy_img_path = map(total_titles, ~ rep(paste0(img, "trophy.png"), .x)))

# Unnest the trophies so each instance is a separate row
g3_trophies <- g3_data %>%
  unnest_longer(trophy_img_path) %>%
  group_by(name) %>%
  mutate(trophy_x = row_number()) %>%  # Track each trophy's position
  ungroup()

# Plot Top 6 Champions ----
g3 <- ggplot(
  data = g3_data, 
  aes(x = reorder(rank, -total_titles))) +
  
  # Wikipedia images
  geom_image(
    aes(y = 0.3, image = wiki_image), 
    size = 0.4,
    by = "height",
    asp = 1) +
  
  # Driver names
  geom_text(
    aes(y = 0.1, label = name),
    size = 3,
    color = pal[4],
    family = fonts$text,
    fontface= "bold",
    hjust = 0.5,
    vjust = 0) +
  
  # Trophy count (as images)
  geom_image(
    data = g3_trophies,
    aes(x = rank + (trophy_x * 0.1) - 0.4, y = 0.05, image = trophy_img_path),
    size = 0.05,
    hjust = 0.5) +
  
  # Total titles (numeric)
  geom_text(
    aes(x = rank + (total_titles * 0.1) - 0.3, y = 0.05, label = total_titles),
    size = 2.5, 
    color = pal[4],
    family = fonts$text,
    fontface= "bold",
    vjust = 0.5,
    hjust = 0) +
  
  # set Y-axis limits
  scale_y_continuous(
    limits = c(0, 0.5),
    # breaks = seq(from = 0, to = 100, by = 10), 
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.05))) +
  
  ## title
  labs(title = "TOP CHAMPIONS") +
  
  ## theme
  theme_void() +
  theme(
    panel.background = element_rect(fill = pal[2], color = NA),
    plot.background = element_rect(fill = pal[2], color = NA),
    plot.title = element_textbox_simple(
      family = fonts$title,
      size = 18,
      hjust = 0,
      vjust = 1,
      halign = 0.5,
      color = pal[4],
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.margin = margin(t = 10, l = 10, r = 10),
    legend.position = "none"
  )

## Print plot g3 ----
# g3



# Final Plot ----

### |-  titles and caption ----
title_text <- str_glue("Formula 1 history")

# Caption with social media
caption_text <- create_social_caption(
  subcaption = "",
  source_text =  "{kaggle: https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020}" 
)

# Montagem dos plots
# final_plot <- 
#   (
#     (g1 + g2) +                     # linha 1: g1 e g2 lado a lado
#       plot_layout(ncol = 2)
#   ) /
#   g3 +                              # linha 2: g3
#   plot_layout(heights = c(1, 1.2)) +  # proporção das linhas
#   plot_annotation(
#     title = title_text,
#     caption = caption_text,
#     theme = theme(
#       plot.background = element_rect(fill = pal[1], color = NA),
#       plot.title = element_text(
#         size = 40,
#         family = fonts$title,
#         color = pal[4],
#         face = "bold",
#         hjust = 0.5,
#         margin = margin(t = 20, b = 20)
#       ),
#       plot.caption = element_markdown(
#         size   = rel(0.75),
#         family = fonts$caption,
#         color = pal[2],
#         hjust  = 0
#       )
#     )
#   )

# Parte superior (titulo)
title_plot <- ggplot() +
  annotate("text", x = 1, y = 1, label = "Formula 1 History", size = 14,
           family = fonts$title, fontface = "bold", color = pal[4]) +
  theme_void() +
  theme(plot.background = element_rect(fill = pal[1], color = NA))

# Montagem geral
final_plot <- 
  title_plot /
  ((g1 + g2) + plot_layout(ncol = 2)) /
  g3 +
  plot_layout(heights = c(0.15, 1, 1.3)) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.background = element_rect(fill = pal[1], color = NA),
      plot.caption = element_markdown(
        size = rel(0.75),
        family = fonts$caption,
        color = pal[2],
        hjust = 0
      )
    )
  )


# Visualiza o final_plot
final_plot



