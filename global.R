library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(plotly)
library(ggcorrplot)
library(summarytools)
library(intubate)
library(broom)
library(ggrepel)
library(FactoMineR)

invisible(walk(list.files("./modules", full.names = TRUE), source))
invisible(walk(list.files("./helpers", full.names = TRUE), source))

dataset <- read_csv("./data/BandungData.csv") %>% 
  select(-no) %>% 
  mutate(
    year = as.character(year)
  ) %>% 
  select(-code, year, type, aq, everything())
  

# description <- read_csv("./data/datadescriptor.csv") %>%
#   as_tibble() %>%
#   `[`(-c(1:4, 37:65), -1)
