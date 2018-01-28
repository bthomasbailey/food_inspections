library(XML)
library(RCurl)
library(stringr)
library(readr)
library(tibble)

get_inspection_hrefs <- function(id, html) {

  hrefs_inspections <- html[grep("td\\.href$", names(html))]
  
  tibble(id_restaurant = id,
         href_inspection = hrefs_inspections)
}

get_restaurant_address <- function(id, name, html) {

  div_td_text <- html[grep("body.div.td.text", names(html))]
  loc_address <- grep("facility\\s+location:", div_td_text, ignore.case = T) + 1
  
  tibble(id_restaurant = id,
         name = name,
         address = div_td_text[loc_address])
}


# Read in Restaurant URLs -------------------------------------------------
urls <- read_csv("restaurant_hrefs.csv") %>% 
          mutate(href = str_c("https://healthspace.com", href))


inspection_hrefs <- tibble(id_restaurant = character(),
                           href_inspection = character())

restaurants <- tibble(id_restaurant = character(),
                      name = character(),
                      address = character())


for (i in 1:nrow(urls)) {
  print(i)
  
  name <- urls$name[i]
  url <- urls$href[i]
  id <- str_match(url, "&RestrictToCategory=(.+)" )[1,2]
  
  resp <- getURL(url)
  doc <- htmlTreeParse(resp, asText = T)
  html <- unlist(xmlToList(doc$children$html))
  
  inspection_hrefs <- rbind(inspection_hrefs, get_inspection_hrefs(id, html))
  restaurants <- rbind(restaurants, get_restaurant_address(id, name, html))
  
}

write_csv(inspection_hrefs, "inspection_hrefs.csv")
write_csv(restaurants, "restaurants.csv")
