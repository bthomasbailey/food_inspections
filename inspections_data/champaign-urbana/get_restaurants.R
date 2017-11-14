library(xml2)
library(httr)
library(tidyverse)
library(stringr)

scrape_page <- function(letter) {

  parse_address <- function(txt_div) {
    txt_div <- unlist(str_split(txt_div, "\\r|\\n|\\t"))
    txt_div <- txt_div[txt_div != ""]
    txt_div <- str_trim(txt_div)
    address <- txt_div[1]  
    address <- gsub("\\s+", " ", address)
    zip <- str_extract(txt_div[2], "\\d+")  
    city_state <- gsub("\\d", "", txt_div[2])
    city_state <- unlist(str_split(city_state, ","))
    city <- str_trim(city_state[1])
    city <- gsub("\\s+", " ", city)
    state <- str_trim(city_state[2])
    
    tibble(address = address,
           city = city,
           state = state,
           zip = zip)
  }
  
  
  print(letter)
  
  base_url <- "http://il.healthinspections.us/champaign/search.cfm" 
  
  url <- modify_url(base_url,
                    query = list(searchType = "letter",
                                 srchLetter = letter)
  )
  
  
  resp <- read_html(url)
  b_nodes <- xml_find_all(resp, "//tr/td//b")
  node_match <- grep("facilities matched", xml_text(b_nodes), value = T, ignore.case = T)
  nbr_facilities <- as.integer(str_extract(node_match, "\\d+"))
  
  if (nbr_facilities > 0) {
    
    i <- 1
    
    while (i <= nbr_facilities) {
    
      print(i)
        
      url <- modify_url(base_url,
                        query = list(start = i,
                                     searchType = "letter",
                                     srchLetter = letter)
      )
    
      resp <- read_html(url)
      
      # Facility IDs
      a_nodes <- xml_find_all(resp, "//tr//td//a")
      hrefs <- xml_attr(a_nodes, "href")
      href_ind <- grep("\\?facilityID=", hrefs)
      hrefs <- hrefs[href_ind]
      
      # Addresses
      xpath_address <- "//div[@style='margin-bottom:10px;']"
      divs <- xml_find_all(resp, xpath_address)
      txt_divs <- xml_text(divs)
      addresses <- bind_rows(lapply(txt_divs, parse_address))
      
      # Restaurant Names
      restaurant_a_nodes <- a_nodes[href_ind]
      names <- xml_text(restaurant_a_nodes)

      # Facility/Restaurant IDs
      facility_ids <- str_extract(hrefs, "\\d+$")
    
      output <- tibble(id_facility = facility_ids, name = names) %>% 
                  bind_cols(addresses)
      
      write_csv(output, file.path("restaurants", 
                                  str_c(str_c(letter, i, i + 9, sep = "_"), ".csv")))
      
      i <- i + 10
      
    }
  
  }

}


lapply(c(5, 7, 9, LETTERS), scrape_page)


# Combine into one CSV file with all restaurants
files <- list.files("restaurants", full.names = T)
restaurants <- bind_rows(lapply(files, read_csv))
file.remove(files)
write_csv(restaurants, "restaurants.csv")


