library(XML)
library(tidyverse)
library(stringr)
library(httr)


scrape_inspection_report <- function(id_inspection, id_facility, date, type) {
  
  print(str_c("Facility ID: ", id_facility, ", Inspection ID: ", id_inspection))
  
  base_url <- "http://il.healthinspections.us/_templates/90/"
  base_url <- str_c(base_url, "Food_Establishment_Inspection/_report_full.cfm")
  
  url <- modify_url(base_url,
                    query = list(inspectionID = id_inspection,
                                 domainID = 90,
                                 publicSite= "yes")
  )
  
  
  doc <- htmlParse(url)
  tables <- readHTMLTable(doc)
  
  
  # Get pass/fail info for each item of the inspection
  items <- tables[[9]]
  names(items) <- c("item", "wt", "in_compliance", "description")
  items <- items %>% 
    filter(!is.na(in_compliance)) %>% 
    filter(row_number() > 2) %>% 
    filter(item != "ITEM") %>% 
    mutate(critical = grepl("\\*", item)) %>% 
    mutate(item = gsub("\\*|\\.", "", item)) %>% 
    mutate_at(vars(item, wt), function(x) as.numeric(as.character(x))) %>% 
    mutate(in_compliance = as.factor(as.character(in_compliance))) %>% 
    mutate(description = as.character(description))
  
  # Get scores
  scoring_info <- names(tables[[13]])
  score <- as.numeric(str_extract(scoring_info[1], "\\d{1,3}"))
  critical_demerits <- as.numeric(gsub("^.*=\\s*", "", scoring_info[2]))
  repeat_demerits <- as.numeric(gsub("^.*=\\s*", "", scoring_info[3]))
  adj_score <- as.numeric(str_extract(scoring_info[4], "\\d{1,3}"))
  
  
  # Get details on items that were out of compliance
  details <- tables[[14]]
  
  names(details) <- c("item", "status", "rule_nbr", "rule", "observed")
  
  comments <- details %>% 
                filter(grepl("inspector comments", item, ignore.case = T)) %>% 
                mutate(status = as.character(status)) %>% 
                select(status) %>% 
                unlist() %>% 
                unname()
  
  comments <- ifelse(length(comments) == 0, NA, comments)
  
  details <- details %>% 
              filter(grepl("\\d{1,2}\\.", item)) %>% 
              mutate(item = gsub("\\*|\\.", "", item)) %>% 
              mutate(item = as.numeric(as.character(item))) %>% 
              mutate(rule_nbr = gsub("\\.$", "", rule_nbr)) %>% 
              mutate_at(vars(-item), as.character)
  
  # Create one df w/ item level info
  violation_level_output <- items %>% 
                              left_join(details, by = "item") %>% 
                              mutate(id_inspection = id_inspection) %>% 
                              select(id_inspection, everything()) %>% 
                              mutate_if(is.character, funs(ifelse(. == "", NA, .)))
  
  
  # Create one df w/ inspection level info (inspection ID, scores, comments)
  inspection_level_output <- tibble(id_inspection = id_inspection,
                                    id_facility = id_facility,
                                    date = date,
                                    type = type,
                                    score = score,
                                    critical_demerits = critical_demerits,
                                    repeat_demerits = repeat_demerits,
                                    adj_score = adj_score,
                                    comments = comments)
  
  write_csv(inspection_level_output, 
            file.path("inspections_pt2", "inspection_level", 
                      str_c(id_inspection, ".csv"))
  )
  
  write_csv(violation_level_output, 
            file.path("inspections_pt2", "violation_level", 
                      str_c(id_inspection, ".csv"))
  )
  
}



inspections <- read_csv("inspections//inspections.csv")

invisible(mapply(scrape_inspection_report,
                 inspections$id_inspection,
                 inspections$id_facility,
                 inspections$date,
                 inspections$type,
                 SIMPLIFY = F)
          )

files_inspections <- list.files("inspections_pt2//inspection_level", full.names = T)
files_violations <- list.files("inspections_pt2//violation_level", full.names = T)

list_inspections <- lapply(files_inspections, 
                           read_csv, 
                           col_types = cols(.default = "i", 
                                            date = "D",
                                            type = "c",
                                            comments = "c")
                           )


list_violations <- lapply(files_violations,
                          read_csv,
                          col_types = cols(.default = "c", 
                                           id_inspection = "i",
                                           item = "i",
                                           wt = "i")
                          )



all_inspections <- bind_rows(list_inspections)
all_violations <- bind_rows(list_violations)

# delete individual CSV files
file.remove(files_inspections)
file.remove(files_violations)


# Write CSV
write_csv(all_inspections, "inspections.csv")
write_csv(all_violations, "violations.csv")
