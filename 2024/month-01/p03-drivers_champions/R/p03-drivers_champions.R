# Loading libraries ----
library(tidyverse)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(lubridate)
library(glue)
library(magick)
library(here)
library(ggimage)
library(fontawesome)


# Loading the data
source(here::here("2024/month-01/data/wikipedia/scrap_wikipedia.R"))

# Importing fonts
font_add_google("Montserrat", family = "Montserrat")

# font_add(family = "FontAwesomeBrands", regular = "C:/Users/Raymundo/Documentos/R/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
# showtext_auto()
# showtext_opts(dpi = 320)

# color pallete ----
font <- "Montserrat"
pal_text <- "#F3F2EE"
pal_graph <- "#F22233"
pal_points <- "#FCDF33"
pal_bkg <- "#D9D4C5"

# loading data ----

tbl_constructor <- tbl_constructor %>%
  select(Season, Constructor)

tbl <- tbl_drivers %>%
  left_join(tbl_constructor, by = "Season")

# tbl1 <- tbl
tbl <- tbl1

# Loading flags
tbl_flags <- data.frame(
  Country = c("Italy", "Argentina", "United Kingdom", "Australia", "United States",
              "New Zealand", "Austria", "Brazil", "South Africa", "Finland", 
              "France", "Germany", "Canada", "Spain", "Netherlands"),
  Code_Country = c("it", "ar", "gb", "au", "us",
                   "nz", "at", "br", "za", "fi", 
                   "fr", "de", "ca", "es", "nl")) %>% 
  mutate(flag = paste0(
    "https://flagcdn.com/80x60/", Code_Country,
    ".png"
  ))

# urls <- unique(tbl_flags$path)
# library(magick)
# load_flag <- function(url) {
#   image_read(url)
# }
# flag <- lapply(urls, load_flag)
# for (i in seq_along(flag)) {
#   print(flag[[i]])
#   Sys.sleep(1)
# }

# Final data
tbl <- tbl %>%
  left_join(tbl_flags, by = "Country") %>%
  mutate(
    Points = as.numeric(Points),
    Wins_Consecutive = with(rle(Driver), rep(lengths, times = lengths)),   # count consecutive wins for bold names
    Season_Group = case_when(                                              # make group labels, to annotate constructor champs
      as.numeric(Season) < 1958 ~ 1,
      TRUE ~ 2
    ),
    Winner_Annot = ifelse(Wins_Consecutive > 2, glue("**{Driver}**"),
                          glue("{Driver}")),
    n = row_number()
  )

# x-axis distance ----
x_positions <- list(
  season1 = -1000,
  winner = 400,
  team = 4200,
  poles = 7800,
  wins = 9500,
  wins_rate = 10800,
  points = 16400,
  constructor = 21800,
  season2 = 26000
)

x_line <- data.frame(x_pos = c(-2000, 0,    4000, 7000, 8800, 10400, 16000, 21400, 25000, 27000))
x_head <- data.frame(x_pos = c(-1800, 200,  4200, 7200, 9000, 10600, 16200, 21600, 25200))

# Function to create annotations
create_annotation <- function(x, y, label, hjust = 0.5, family = font, size = 3.5) {
  annotate(
    "text",
    x = x, y = y, label = toupper(label),
    hjust = hjust, family = family, size = size
  )
}

# Social media icons
twitter_icon <- image_read_svg("https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/twitter.svg")
github_icon <- image_read_svg("https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/github.svg")
linkedin_icon <- image_read_svg("https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/linkedin.svg")


# Plot ----
ggplot(data = tbl) +
  
  ## Season note ----
  geom_richtext(
    aes(x = x_positions$season1, y = n, label = Season, fill = pal_bkg),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    family = font, size = 2.5
  ) +
  geom_richtext(
    aes(x = x_positions$season2, y = n, label = Season),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    family = font, size = 2.5
  ) +
  
  ## Winner note ----
  ### Flags
  geom_image(
    aes(x = x_positions$winner, y = n, image = flag), 
    size = 0.01) +
  
  geom_richtext(
    aes(x = x_positions$winner + 200, y = n, label = Winner_Annot),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2.5
  ) +
  
  ## Team note ----
  geom_richtext(
    aes(x = x_positions$team, y = n, label = Constructor.x),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2.5
  ) +
  
  ## Poles note ----
  geom_richtext(
    aes(x = x_positions$poles, y = n, label = Poles),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0.5, family = font, size = 2.5
  ) +

  ## Wins note ----
  geom_richtext(
    aes(x = x_positions$wins, y = n, label = Wins),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0.5, family = font, size = 2.5
  ) +

  ## Wins rate ----
  geom_richtext(
    aes(x = x_positions$wins_rate + `% Win` * 50 + 100, y = n, label = round(`% Win`, 1)),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2.5
  ) +
  geom_ribbon(
    aes(xmin = x_positions$wins_rate, xmax = x_positions$wins_rate + `% Win` * 50, y = n),
    fill = pal_graph, alpha = 0.6, orientation = "y", position = "identity"
  ) +
  geom_point(
    aes(x = x_positions$wins_rate + `% Win` * 50, y = n), size = 0.5) +

  ## Points ----
  geom_richtext(
    aes(x = x_positions$points + Points * 7 + 100, y = n, label = round(Points, 1)),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2.5
  ) +
  geom_ribbon(
    aes(xmin = x_positions$points, xmax = x_positions$points + Points * 7, y = n),
    fill = pal_points, alpha = 0.6, orientation = "y", position = "identity"
  ) +
  geom_point(
    aes(x = x_positions$points + Points * 7, y = n), size = 0.5) +

  ## Constructor winner note ----
  geom_richtext(
    aes(x = x_positions$constructor, y = n, label = Constructor.y),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2.5
  ) +

  ## Custom annotations, lines, and boxes ----
  
  ### Lines
  
  #### verticals lines
  annotate(
    "segment",
    x = x_line$x_pos,
    xend = x_line$x_pos,
    y = -4, yend = 76, 
    size = 0.3
  ) +
  
  #### horizontal lines
  annotate(
    "segment",
    x = -2000, xend = 27000,
    y = c(-4, -1, 76), yend = c(-4, -1, 76),
    size = 0.3
  ) +
  
  ### label head
  create_annotation(
    x = x_head$x_pos,
    y = -2.5,
    label = c("season", "winner", "team", "poles", "wins", "% win rate", "total points", "constructor", "season"),
    hjust = 0
  ) +
  
  ### label % Wins
  annotate(
    "text",
    x = c(x_positions$wins_rate - 100, 15800), y = 0,
    label = c("0", "100%"),
    hjust = c(0, 1), family = font, size = 3
  ) +
  annotate(
    "text",
    x = c(x_positions$points - 100, 21200), y = 0,
    label = c("0", ceiling(max(tbl$Points)/100)*100),
    hjust = c(0, 1), family = font, size = 3
  ) +
 
  ## Credits ----
  # annotate(
  #   "text",
  #   x = -2000, y = 77,
  #   label = "Source: Wikipedia",
  #   hjust = 0, family = font, size = 2
  # ) +
  
  geom_richtext(
    aes(x = -2000, y = 77,  
        label = glue(
          "Source: Wikipedia"
        )),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 0, family = font, size = 2,
  ) +
  geom_richtext(
    aes(x = 27000, y = 77,  
    label = glue(
      "<img src='{image_write(twitter_icon, tempfile(fileext = '.png'))}' width='5'/> @raymundoooooooo | ",
      "<img src='{image_write(github_icon, tempfile(fileext = '.png'))}' width='5'/> raymz1990 | ",
      "<img src='{image_write(linkedin_icon, tempfile(fileext = '.png'))}' width='5'/> raymundopilz"
    )),
    fill = NA, label.color = NA, label.padding = unit(0.1, "lines"),
    hjust = 1, family = font, size = 2,
  ) +
  
  ## Theme ----
  coord_cartesian(clip = 'off') +
    scale_x_continuous(limits = c(-2300, 27300), expand = expansion(add = 1)) +
    scale_y_reverse(expand = expansion(add = 0)) +
    labs(title = "Formula One World Drivers' Champions") +
    theme_void(base_family = font) +
    theme(
      plot.background = element_rect(fill = pal_bkg, colour = pal_bkg),
      plot.margin = margin(40, 20, 20, 20),
      plot.title = element_text(hjust = 0.01, size = 28, family = font, margin = margin(0, 0, 1, 0))
    )

ggsave(here::here("2024", "month-01", "p03-drivers_champions", "plot",
                  paste0("drivers_champions", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 11, height = 15)

