# Packages and libraries ----
# install.packages("rvest")
# install.packages("httr")
# install.packages("tidyverse")

library(rvest)
library(httr)
library(tidyverse)

# World Drivers' Champions extract ----
# Page's URL
url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_World_Drivers%27_Champions"

# HTTP requisition and read the page
page <- read_html(url)

# Extract the table (table 2)
tbl <- page %>%
  html_nodes("table.wikitable") %>%  
  .[[2]] %>%                         
  html_table(fill = TRUE)            

# Removing rows (the first and 2 last ones)
tbl <- tbl[-1, ]              
tbl <- tbl[-nrow(tbl), ]    
tbl <- tbl[-nrow(tbl), ]    

# Removing duplicates in column names
colnames(tbl) <- make.unique(colnames(tbl))

# Scraping countries
countries <- page %>%
  html_nodes("table.wikitable") %>%                         
  .[[2]] %>%                                               
  html_nodes("td:nth-child(2)") %>%                      
  html_nodes(".flagicon img") %>%                           
  html_attr("alt")                                          

# Creating a vector for countries
countries_correct <- rep(NA, nrow(tbl))                  
index_countries <- 1                                          

# Fixing Fangio's 1954 problem
for (i in 1:nrow(tbl)) {
  if (grepl("Juan Manuel Fangio", tbl$Driver[i]) &&
      tbl$Season[i] == 1957) {                           
    countries_correct[i] <- "Argentina"
  } else {
    countries_correct[i] <- countries[index_countries]
    index_countries <- index_countries + 1
  }
}

# Add Country to dataframe
tbl$Country <- countries_correct

# Removing values between brackets
tbl <- tbl %>%
  mutate(
    Season = gsub("\\[.*\\]", "", Season),       
    Driver = gsub("\\[.*\\]", "", Driver),      
    Constructor = gsub("\\[.*\\]", "", Constructor),  
    `Clinched[17]` = gsub("\\[.*\\]", "", `Clinched[17]`)  
  )

# Creating the Rounds column
tbl <- tbl %>%
  rename(Rounds = `Clinched[17]`) %>%            
  mutate(Rounds = gsub(".* of ", "", Rounds))   

# Adjusting Fangio's 1954 problem (combining rows)
tbl <- tbl %>%
  group_by(Season, Country, Driver, Poles, Wins, Points, Rounds) %>%
  summarise(Constructor = paste(Constructor, collapse = "/"), .groups = "drop")

# Selecting the correct columns
tbl <- tbl %>%
  select(Season, Country, Driver, Constructor, Poles, Wins, Points, Rounds)

# Calculating the % Win
tbl <- tbl %>%
  mutate(
    `% Win` = round((as.numeric(Wins) / as.numeric(Rounds)) * 100, 2)  
  )

# World Drivers' Champions data
tbl_drivers <- tbl


# World Constructors' Champions extract ----
# Page's URL
url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_World_Constructors%27_Champions"

# HTTP requisition and read the page
page <- read_html(url)

# Extract the table (table 2)
tbl <- page %>%
  html_nodes("table.wikitable") %>%  
  .[[2]] %>%                         
  html_table(fill = TRUE)            

# Removing rows (the first and 2 last ones)
tbl <- tbl[-1, ]              
tbl <- tbl[-nrow(tbl), ]    
tbl <- tbl[-nrow(tbl), ]    

# Removing duplicates in column names
colnames(tbl) <- make.unique(colnames(tbl))

# Selecting the correct columns
tbl <- tbl %>%
  select(Season, Constructor, `Drivers[a]`, Poles, Wins, `Points[b]`, `Clinched[21]`)

# Removing values between brackets
tbl <- tbl %>%
  mutate(
    Season = gsub("[†¶§•]", "", Season)
  )

# Rename collumns
names(tbl) <- c("Season", "Constructor", "Drivers", "Poles", "Wins", "Points", "Rounds")

# Adjusting rounds' collumn
tbl <- tbl %>%
  mutate(Rounds = gsub(".* of ", "", Rounds))   

# Calculating the % Win
tbl <- tbl %>%
  mutate(
    `% Win` = round((as.numeric(Wins) / as.numeric(Rounds)) * 100, 2)  
  )

# World Drivers' Champions data
tbl_constructor <- tbl

# Setup environment ----
# keeping only the main dataframe in the environment 
rm(list = setdiff(ls(), c("tbl_constructor", "tbl_drivers")))



