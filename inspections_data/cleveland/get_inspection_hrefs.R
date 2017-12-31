library(XML)
library(RCurl)
library(stringr)
library(readr)

get_inspection_hrefs <- function(url) {
  id <- str_match(url, "&RestrictToCategory=(.+)" )[1,2]
  
  print(id)
  
  resp <- getURL(url)
  doc <- htmlTreeParse(resp, asText = T)
  html <- unlist(xmlToList(doc$children$html))
  
  hrefs_inspections <- html[grep("td\\.href$", names(html))]
  
  tibble::tibble(id_restaurant = id,
                 href_inspection = hrefs_inspections)
}

urls <- read_csv("restaurant_hrefs.csv")
urls <- str_c("https://healthspace.com", urls$href)

inspection_hrefs <- dplyr::bind_rows(lapply(urls, get_inspection_hrefs))
write_csv(inspection_hrefs, "inspection_hrefs.csv")
