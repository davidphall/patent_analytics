library(httr)
library(tidyverse)
library(data.table)
library(feather)


################################################################################
# Working with large files 

  # Data downloads and dictionaries
    # https://patentsview.org/download/data-download-tables
    # https://patentsview.org/download/data-download-dictionary

# Read downloaded files, filter, and save
  # patents file (grant date)
  patent <- fread('data/patent/g_patent.tsv')
  # disambiguated assignee (company name)
  assignee <- fread('data/assignee/g_assignee_disambiguated.tsv')
  # term grant (expiration date)
  term <- fread('data/term/g_us_term_of_grant.tsv')
  # application (to calculate expiration date)
  application <- fread('data/application/g_application.tsv')
  # cpc codes (for heatmap)
  cpc <- fread('data/cpc/g_cpc_current.tsv')
  # location
  location <- fread('data/location/g_location_disambiguated.tsv')
  
# Filter
patent <- patent %>% filter(year(patent_date) %in% 2012:2021)
assignee <- assignee %>% filter(patent_id %in% patent$patent_id)
term <- term %>% filter(patent_id %in% patent$patent_id)
application <- application %>% filter(patent_id %in% patent$patent_id)
cpc <- cpc %>% filter(patent_id %in% patent$patent_id)
location <- location %>% filter(location_id %in% assignee$location_id)

# Save as
  # csv
  write.csv(patent,'data/patent/g_patent_2012_2021.csv',row.names = F)
  write.csv(assignee,'data/assignee/g_assignee_disambiguated_2012_2021.csv',row.names = F)
  write.csv(term,'data/term/g_us_term_of_grant_2012_2021.csv',row.names = F)
  write.csv(application,'data/application/g_application_2012_2021.csv',row.names = F)
  write.csv(cpc,'data/cpc/g_cpc_current_2012_2021.csv',row.names = F)
  write.csv(location,'data/location/g_location_disambiguated_2012_2021.csv',row.names = F)
  
    patent <- fread('data/patent/g_patent_2012_2021.csv')
    assignee <- fread('data/assignee/g_assignee_disambiguated_2012_2021.csv')
    term <- fread('data/term/g_us_term_of_grant_2012_2021.csv')
    application <- fread('data/application/g_application_2012_2021.csv')
    cpc <- fread('data/cpc/g_cpc_current_2012_2021.csv')
    location <- fread('data/location/g_location_disambiguated_2012_2021.csv')
      
    patent_text <- patent[,c('patent_id','patent_title','patent_abstract')]
    write.csv(patent_text,'data/patent/patent_text.csv',row.names = F)
    
    
  # Rdata
  save(patent,file = 'data/patent/g_patent_2012_2021.Rdata')
  save(assignee,file = 'data/assignee/_assignee_disambiguated_2012_2021.Rdata')
  save(term,file = 'data/term/g_us_term_of_grant_2012_2021.Rdata')
  save(application,file = 'data/application/g_application_2012_2021.Rdata')
  save(cpc,file = 'data/cpc/g_cpc_current_2012_2021.Rdata')
  save(location,file = 'data/location/g_location_disambiguated_2012_2021.Rdata')
  
  # Feather (https://posit.co/blog/feather/)
  write_feather(patent,'data/patent/g_patent_2012_2021.feather')
  write_feather(assignee,'data/assignee/_assignee_disambiguated_2012_2021.feather')
  write_feather(term,'data/term/g_us_term_of_grant_2012_2021.feather')
  write_feather(application,'data/application/g_application_2012_2021.feather')
  write_feather(cpc,'data/cpc/g_cpc_current_2012_2021.feather')
  write_feather(location,'data/location/g_location_disambiguated_2012_2021.feather')


  # Run load time comparison
  benchmark <- c()
    # read.table 
    start_time <- Sys.time()
    cpc <- read.table(file = 'data/cpc/g_cpc_current_2012_2021.csv',sep = ',')
    end_time <- difftime(Sys.time(),start_time,units = 'mins' )
    benchmark <- c(benchmark,'read.table' = end_time)
    # read.csv 
    start_time <- Sys.time()
    cpc <- read.delim(file = 'data/cpc/g_cpc_current_2012_2021.csv',sep = ',')
    end_time <- difftime(Sys.time(),start_time,units = 'mins' )
    benchmark <- c(benchmark,'read.csv' = end_time)
    # fread 
    start_time <- Sys.time()
    cpc <- fread(file = 'data/cpc/g_cpc_current_2012_2021.csv')
    end_time <- difftime(Sys.time(),start_time,units = 'mins' )
    benchmark <- c(benchmark,'fread' = end_time)
    # feather 
    start_time <- Sys.time()
    cpc <- read_feather(path = 'data/cpc/g_cpc_current_2012_2021.feather')
    end_time <- difftime(Sys.time(),start_time,units = 'mins' )
    benchmark <- c(benchmark,'feather' = end_time)
    # Rdata
    start_time <- Sys.time()
    load('data/cpc/g_cpc_current_2012_2021.Rdata')
    end_time <- difftime(Sys.time(),start_time,units = 'mins' )
    benchmark <- c(benchmark,'Rdata' = end_time)
  
    benchmark

    
    ###############################################################################
    # R Shiny preview while we wait: https://davidpeterhall.shinyapps.io/gcstudy/ #
    ###############################################################################
    
    
    # Some thoughts:
      # feather is nice because it can be read in R or python and is close to the speed of fread
      # Rdata files will take up less storage space
      # fread is part of the data.table package, which is great for working with large files once they are loaded

    
