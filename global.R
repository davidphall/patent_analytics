library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(bslib)
library(feather)
library(DT)


load('data/unique_cpc_group.Rdata')
assignee <- read_feather('data/g_assignee_disambiguated_2012_2021.feather')
location <- read_feather('data/g_location_disambiguated_2012_2021.feather')
patent <- read_feather('data/g_patent_2012_2021.feather')
cpc <- fread('data/g_cpc_current_2012_2021.csv')
cpc$patent_id <- as.character(cpc$patent_id)

# Resources
  # https://shiny.rstudio.com/
  # https://shiny.rstudio.com/gallery/
  # Icons https://fontawesome.com/search?o=r&m=free
  # HTML tags https://shiny.rstudio.com/articles/tag-glossary.html
  # Inputs https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# Agenda:
  # Server
    # Reactivity
      # Reactive values
      # Inputs
      # Server logic
      # Rendering outputs
      # Displaying in UI
    # Observers
    # Debugging - browser()
    # Work together in class:
      # Add code from competetive positioning assignemnt to make your app interactive
    




