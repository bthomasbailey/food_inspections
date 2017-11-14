library(xml2)
library(httr)
library(tidyverse)
library(stringr)
library(lubridate)

get_inspection_ids <- function(id_facility) {

  parse_date_type <- function(date_type) {
    date_type <- unlist(str_split(date_type, "-"))
    type <- str_trim(date_type[1])
    date <- mdy(str_trim(date_type[2]))
    tibble(date = date, type = type)
  }
  
  print(id_facility)
  
  base_url <- "http://il.healthinspections.us/champaign/estab.cfm"
  
  url <- modify_url(base_url,
                    query = list(facilityID = id_facility)
  )
  
  resp <- read_html(url)
  
  a_nodes <- xml_find_all(resp, "//tr//td//a")
  hrefs <- xml_attr(a_nodes, "href")
  hrefs_ind <- grep("_report_full.cfm\\?inspectionID=", hrefs)
  
  inspect_date_type <- xml_text(a_nodes[hrefs_ind])
  inspect_date_type <- bind_rows(lapply(inspect_date_type, parse_date_type))
  
  hrefs <- hrefs[hrefs_ind]
  matches <- str_match(hrefs, ".*inspectionID=(\\d+)&domainID.*")
  inspection_ids <- matches[, 2]
  
  dat_inspections <- tibble(id_facility = id_facility, 
                            id_inspection = inspection_ids) %>% 
                      bind_cols(inspect_date_type)
  
  write_csv(dat_inspections,
            file.path("inspections", str_c(id_facility, ".csv"))
  )

}

restaurants <- read_csv("restaurants.csv")
lapply(restaurants$id_facility, get_inspection_ids)

files <- list.files("inspections", full.names = T)
inspections <- bind_rows(lapply(files, read_csv))
inspections$type <- as.factor(inspections$type)
file.remove(files)
write_csv(inspections, file.path("inspections", "inspections.csv"))
