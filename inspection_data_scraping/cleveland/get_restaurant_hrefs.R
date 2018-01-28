library(XML)
library(RCurl)
library(stringr)

url_base <- "https://healthspace.com/clients/ohio/cuyahoga/cuyahoga_web_live.nsf/"
url_base <- str_c(url_base, "Food-List-ByFirstLetterInName?OpenView&count=1000000")
url_base <- str_c(url_base, "&RestrictToCategory=")

scrape_letter <- function(first_letter) {
  
  print(first_letter)
  
  url <- str_c(url_base, first_letter)  
  resp <- getURL(url)
  doc <- htmlTreeParse(resp, asText = T)
  
  html <- doc$children$html[[2]][[2]]
  html <- unlist(xmlToList(html))

  names <- html[grep("(tr\\.)?td1$", names(html))]
  hrefs <-html[grep("href$", names(html))]

  tibble::tibble(name = names,
                 href = hrefs)
  
}

first_letters <- c(1:8, LETTERS[!LETTERS %in% c("Y", "Z")])
hrefs <- dplyr::bind_rows(lapply(first_letters, scrape_letter))

readr::write_csv(hrefs, "restaurant_hrefs.csv")
