# loading libraries
library(tidyverse)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)
library(htmltools)
# library(patchwork)
library(ggbump)
library(MetBrewer)
library(scales)

# importing fonts
font_add_google("Roboto Slab", family = "Roboto Slab")
font_add_google("Roboto", family = "Roboto")
font_add(family = "FontAwesomeBrands", regular = "C:/Users/Raymundo/Documentos/R/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# dataset
df_senna <- data.frame(
  year = c(1984:1994),
  points = c(13, 38, 55, 57, 90, 60, 78, 96, 50, 73, 0),
  position = c(9, 4, 4, 3, 1, 2, 1, 1, 4, 2, 38))

df_piquet <- data.frame(
  year = c(1978:1991),
  points = c(0, 3, 54, 50, 20, 59, 29, 21, 69, 73, 22, 12, 43, 26.5),
  position = c(28, 16, 2, 1, 11, 1, 5, 8, 3, 1, 6, 8, 3, 6)
)

df_prost <- data.frame(
  year = c(1980:1991, 1993),
  points = c(5, 43, 34, 57, 71.5, 73, 72, 46, 87, 76, 71, 34, 99),
  position = c(16, 5, 4, 2, 2, 1, 1, 4, 2, 1, 2, 5, 1)
) 

df_mansell <- data.frame(
  year = c(1980:1992, 1994, 1995),
  points = c(0, 8, 7, 10, 13, 31, 70, 61, 12, 38, 37, 72, 108, 13, 0),
  position = c(30, 14, 14, 13, 10, 6, 2, 2, 9, 4, 5, 2, 1, 9, 29)
)

# Combine all data
df <- bind_rows(
  mutate(df_senna, driver = "Senna"),
  mutate(df_piquet, driver = "Piquet"),
  mutate(df_prost, driver = "Prost"),
  mutate(df_mansell, driver = "Mansell")
)

# data for images bellow chart
images <- data.frame(
  driver = c("Prost", "Senna", "Piquet", "Mansell"),
  name = c("Alain Prost", "Ayrton Senna", "Nelson Piquet", "Nigel Mansell"),
  x_pos = c(1985, 1986.5, 1988, 1989.5),
  color = c(pal_prost, pal_senna, pal_piquet, pal_mansell)) %>% 
  mutate(path = paste0(
    "2024/month-01/p02-senna_prost_mansell_piquet/images/",
    str_replace_all(tolower(driver), " ", "_"),
    ".png"
  ))

#custom labels for legend - include image + label - works with ggtext::element_markdown

# Ordering drivers
order = c("Prost", "Senna", "Piquet", "Mansell")

df$driver = factor(df$driver, levels = order)

# palette colors
pal_bg <- 'gray90'         # background color
pal_text <- '#050200'       # text color
pal_title <- '#FFD700'      # title color (gold)

pal_senna <- "#fad02c"
pal_prost <- '#f41823'
pal_piquet <- '#0d733c'
pal_mansell <- '#025492'

# create caption with html tools to pass into plot.caption theme with ggtext::element_textbox_simple
# must download font awesome brands locally to work: https://fontawesome.com/download
caption <- tagList(
  tags$span("Source: Wikipedia"),
  tags$br(),
  tags$span(HTML("&#xf099;"), style = 'color:#1DA1F2; font-family:"FontAwesomeBrands";'),
  tags$span("@raymundoooooooo"),
  tags$span(HTML("&#xf09b;"), style = 'color:#171515; font-family:"FontAwesomeBrands";'),
  tags$span("raymz1990"),
  tags$span(HTML("&#xf08c;"), style = 'color:#0077B5; font-family:"FontAwesomeBrands";'),
  tags$span("raymundo_pilz")
)



ggplot(
  data = df,
  aes(x = year, y = position, group = driver)) +
  geom_bump(linewidth = 0.6, color = "gray90") +
  geom_bump(aes(color = driver), linewidth = 1,
            data = ~.) +
  geom_point(color = pal_bg, size = 3) +
  geom_point(color = "gray90", size = 2) +
  geom_point(aes(color = driver), size = 2,
             data = ~.) + 
  # geom_text(aes(label = driver), x = 1996, hjust = 0,
  #           color = "black", size = 3.5,
  #           data = df %>%
  #             slice_max(year, by = driver)) +
  scale_color_manual(values = c(pal_prost, pal_senna, pal_piquet, pal_mansell)) +
  
  #create new x axis labels
  geom_text(mapping = aes(
    x = year,
    label = paste0("'", substr(year, 3, 4)),
    y = -2
  ),
  color = pal_text,
  size = 4,
  # fontface = "bold",
  vjust = 0.3) +
  
  
  ## adjust into the scale y

  scale_y_reverse(
    limits = c(40, -10),
                  breaks = c(1, 5, 10, 15, 20, 25, 30, 35), 
                  expand = c(0.02, 0),
                  labels = scales::number_format(suffix = ".")) +
  scale_fill_manual(
    values = c(pal_prost, pal_senna, pal_piquet, pal_mansell),
    labels = c("Prost", "Senna", "Piquet", "Mansell"),
    guide = guide_legend(nrow = 1, override.aes = list(fill = NA))

  )+
  
  # custom legend
  
  ## images below chart
  geom_image(
    data = images, 
    mapping = aes( y = - 8, x = x_pos, image=path), 
    size = 0.1) +
  
  geom_richtext(
    data = images,
    mapping = aes( y= -4, x = x_pos, label = name),
    fill = NA, label.color = NA, hjust = 0.4,
    show.legend = FALSE, fontface = "bold") +
  
  # scale_color_identity() +
  
  
  labs(fill = "Position", x = "", y = "Position",
       title = "80's Year Rivalry",
       subtitle = "A Prost vs Senna vs Piquet vs Mansell dispute over the years",
       caption = caption) +
  theme_void(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    panel.background = element_rect(fill=pal_bg, color=NA),
    plot.background = element_rect(fill=pal_bg),
    panel.grid.minor = element_blank(), 
    legend.position="none",
    #     legend.title=element_blank(),
    #     # legend.text=element_blank(),
    #     legend.text = element_blank(),
        text=element_text(family="Roboto Slab"),
        plot.margin = margin(t=20, b=10, l=15, r=15),
        # axis.title.x  = element_text(margin=margin(t=10), size=10),
        axis.text.x = element_blank(),
    axis.title.y = element_text(angle = 90),
    axis.text.y = element_text(),
        plot.title = element_text(face="bold", family="Roboto Slab"),
        plot.caption = element_textbox_simple(size=8),
        plot.subtitle = element_textbox_simple(margin=margin(b=10), size=12))

# add colors to the dataframe

# df <- df %>%
#   mutate(pal_chart = ifelse(position == 1, pal_title, pal_text))

pal_colors <-  tibble::tribble(
  ~team, ~pal_color,
  "Toleman", "#1F77B4",
  "Lotus", "#050200",
  "McLaren", "#D62728",
  "Williams", "#0033A0")

df <- df %>%
  left_join(pal_colors, by = "team")

# extract data for championships (world champion years)
champion <- df %>%
  filter(position == 1) %>%
  select(year, pts)

# data for team segment connections
order <- df %>%
  group_by(team, pal_color) %>%
  summarise(min_pts = min(year),
            max_pts = max(year),
            x = mean(year))

# data for images bellow chart
images <- data.frame(
  name = unique(df$model_car),
  pos = seq(from = 1984,
            to = 1994,
            length.out = 11)) %>% 
  mutate(path = paste0(
    "month-01/p01-senna_carrer/images/",
    str_replace_all(tolower(name), " ", "_"),
    ".png"
  ))

# data frame for connector segments between years and images
connectors <- data.frame(
  team = c("Toleman",
           rep("Lotus", 3),
           rep("McLaren", 3),
           "Williams"), 
  x = c(1984,
        1985, 1986, 1987,
        1988, 1990.5, 1993,
        1994),
  xend = c(1984,
           1985, 1986, 1987,
           1988, 1990.5, 1993,
           1994),
  y = c(-34, 
        -28, -34, -28,
        -28, -34, -28,
        -34),
  yend = c(-20, 
           -20, -28, -20,
           -20, -28, -20,
           -20)
)

connectors <- connectors %>%
  left_join(pal_colors, by = "team")

# add transparency to images
# solution from: https://stackoverflow.com/questions/60820565/is-there-a-way-to-add-an-alpha-value-within-geom-image-in-ggplot
transparent <- function(img) {
  magick::image_fx(img, expression = "0.1*a", channel = "alpha") # if too dark images, change alpha to 0.5*a
}

# title for the plot (ggtext::element_textbox_simple)
title <- '<span style="color:#000000;font-weight: bold;">THE </span>
          <span style="color:#FF1801;font-weight: bold;">FORMULA ONE </span>
          <span style="color:#000000;font-weight: bold;">LEGACY OF </span>
          <span style="color:#009440;font-weight: bold;">AYRTON </span>
          <span style="color:#FFD700;font-weight: bold;">SENNA</span>'

# data for custom legend
df_legend <- data.frame(
  image = "month-01/p01-senna_carrer/images/trophy.png",
  text = "World Champion",
  x_pos = 1984,
  y_pos = -45
)

# create caption with html tools to pass into plot.caption theme with ggtext::element_textbox_simple
# must download font awesome brands locally to work: https://fontawesome.com/download
caption <- tagList(
  tags$span("Source: Wikipedia"),
  tags$br(),
  tags$span(HTML("&#xf099;"), style = 'color:#1DA1F2; font-family:"FontAwesomeBrands";'),
  tags$span("@raymundoooooooo"),
  tags$span(HTML("&#xf09b;"), style = 'color:#171515; font-family:"FontAwesomeBrands";'),
  tags$span("raymz1990"),
  tags$span(HTML("&#xf08c;"), style = 'color:#0077B5; font-family:"FontAwesomeBrands";'),
  tags$span("raymundo_pilz")
)

# geom_image(
#   mapping = aes(x = 1984, y = -45, image = 'month-01/p01-senna_carrer/images/trophy.png', color = pal_title),
#   size = 0.05) +
# 
# annotate(
#   geom = "text", x = 1984.2, y = -45, label = "World Champion",
#   color = pal_text,
#   hjust = 0) +


# main plot
plot <- 
  ggplot(
    data = df, 
    aes(x = year)) +
  
  ## horizontal gridlines  
  geom_segment(
    data = data.frame(y = seq(from = 0, to = 100, by = 10)),
    mapping = aes(x = 1984, xend = 1994, y = y, yend = y),
    size = 0.1, alpha = 0.01) +
    
  ## bars for total points
  geom_segment(
    mapping = aes(x = year, xend = year, y = 0, yend = pts, color = pal_color),
    size = 20) +
  
  ## add point on bars
  geom_text(
    mapping = aes(y = pts + 2, label = pts,  color = pal_color),
    size = 5,
    fontface = "bold",
    vjust = 0.1) +
    
  ## adjust into the scale y
  scale_y_continuous(
    limits = c(-50, 100),
    breaks = seq(from = 0, to = 100, by = 10), 
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.05))) +
    
  ## text for years
  geom_text(
    mapping = aes(x = year, label = year, y = -5, color = pal_color), 
    size = 5,
    fontface = "bold",
    vjust = 0.3) +
  
  ## add trophy on champion bars
  geom_image(
    data = champion, 
    mapping = aes(x = year, y = pts - 5, image = 'month-01/p01-senna_carrer/images/trophy.png', color = pal_title), 
    size = 0.05) +
    
  ## custom legend

  geom_image(
    data = df_legend,
    mapping = aes(x = x_pos, y = y_pos, image = image, color = pal_title),
    size = 0.05) +

  geom_text(
    data = df_legend,
    mapping = aes(x = x_pos + 0.2, y = y_pos, label = text, color = pal_text),
    fontface = "bold",
    hjust = 0) +
  
  scale_size_identity() +
    
  ## team segment lines
  geom_segment(
    data = order, 
    mapping = aes(x = min_pts, xend = max_pts, y = -28, yend = -28, color = pal_color, size = 1)) +
  # geom_segment(data=order, mapping=aes(x=min_pts, xend=min_pts, y=-20, yend=-19), color="blue") +
  # geom_segment(data=order, mapping=aes(x=max_pts, xend=max_pts, y=-20, yend=-19), color="red") +
  
  ## connector segments  
  geom_segment(
    data = connectors,
    mapping = aes(x = x, xend = xend, y = y, yend = yend, color = pal_color, size = 1)) +
  
  ## images below chart
  geom_image(
    data = images, 
    mapping = aes( y = - 14, x=pos, image=path), 
    size = 0.07) +
  
  geom_richtext(
    data = order,
    mapping = aes( y= -37, x = x, color = pal_color, label = team),
    fill = NA, label.color = NA, hjust = 0.4,
    show.legend = FALSE, fontface = "bold") +
  
  scale_color_identity() +
  
    #add ball image 
    # geom_image(mapping=aes(x=0, y=0, image='month-01/p01-senna_carrer/images/senna.png'), image_fun = transparent, size=.4)+
    
  ## labels and theme
  labs(
    title = title,
    x = "",
    y = "Total Points",
    caption = caption) +

  theme(
    panel.background = element_rect(fill=pal_bg, color=NA),
    plot.background = element_rect(fill=pal_bg),
    plot.title = element_textbox_simple(
      size = 20,             
      halign = 0.5,          
      color = pal_title,    
      face = "bold",         
      margin = margin(b = 20) 
    ),
    text = element_text(
      color = pal_text,       
      size = 12               
    ),
    plot.margin = margin(t=30, l=10, r=10),
    plot.caption = element_textbox_simple(hjust=0.01, color="#818990", margin=margin(b=10), size=10),
    panel.grid = element_blank(),
    axis.text.y = element_text(
      color = pal_text, 
      size = 12,        
      face = "bold"    
    ),
    panel.grid.major.y = element_line(
      color = "gray80", 
      linewidth = 0.5       
    ),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# print
plot

## overlay final plot with helmet image
plot2 <-
  ggplot() +
  # add main plot
  annotation_custom(
    ggplotGrob(plot),
    xmin = 3, xmax = 7, ymin = 1.25, ymax = 4.75) +
  # add helmet image
  geom_image(
    mapping = aes(x = 3.3, y = 4, image = "month-01/p01-senna_carrer/images/senna-helmet.png"),
    image_fun = transparent, size = 0.4) +
  # add caption
  # geom_textbox(mapping=aes(x=3.3, y=1.25, label=caption), size=3, color="#6E6E6E", hjust=0, vjust=1, box.size=NA, fill=NA,  width = unit(3.5, "inch"))+
  scale_x_continuous(limits = c(3, 7), expand = c(0, 0)) +
  scale_y_continuous(limits = c(1, 5), expand = c(0, 0)) +
  # coord_equal() +
  theme_void()

## print
plot2

# ggsave("month-01/p01-senna_carrer/plot/senna.png")

# saving the plot
# ggsave("month-01/p01-senna_carrer/plot/senna1.png",
#        width = 30,
#        height = 30,
#        # unit = 'px',
#        limitsize = FALSE)

# Save final plot
Cairo::Cairo(
  16, #length
  9, #width
  file = "month-01/p01-senna_carrer/plot/senna.png",
  type = "png", 
  bg = "transparent",
  dpi = 300,
  units = "in"
)
plot(plot2)
dev.off()
