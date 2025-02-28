## social_icons.R

# Social media icons configuration
get_social_icons <- function() {
  list(
    linkedin = str_glue("<span style='color:#0077B5; font-family:fa6-brands'>&#xf08c;</span>"),
    github   = str_glue("<span style='color:#171515; font-family:fa6-brands'>&#xf09b;</span>"),
    twitter  = str_glue("<span style='color:#1DA1F2; font-family:fa6-brands'>&#xf099;</span>")     
  )
}

# Create social media caption (tidytuesday)
create_social_caption <- function(subcaption, source_text) {
  icons <- get_social_icons()
  
  tt_text <- str_glue("{subcaption} <br><br> Source: {source_text}<br>")
  social_text <- str_glue("{icons$twitter} @raymundoooooooo &bull; {icons$github} raymz1990 &bull; {icons$linkedin} raymundopilz")
  
  str_glue("{tt_text} {social_text}")
}

# Create SWD social media caption
create_swd_caption <- function(subcaption, source_text) {
  # Get icons from existing function
  icons <- get_social_icons()
  
  # Create SWD challenge header
  swd_text <- str_glue("{subcaption} <br>; Source: {source_text}<br>")
  
  # Create social media handles
  social_text <- str_glue("{icons$twitter} @raymundoooooooo &bull; {icons$github} raymz1990 &bull; {icons$linkedin} raymundopilz")
  
  # Combine texts
  str_glue("{swd_text} {social_text}")
}

# # Example usage:
# caption_text <- create_swd_caption(
#   year = 2025,
#   month = "Jan",
#   source_text = "Source: Scrapped from goodreads & librarthing"
# )