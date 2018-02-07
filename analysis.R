library(tidyverse)
library(RMySQL)
library(ini)

restaurants <- read_csv("data/restaurants.csv")
inspections <- read_csv("data/inspections.csv")
violations <- read_csv("data/violations.csv")

linked_ids <- read_csv("data/linked_ids.csv")


# Use Yelp ID instead of restaurant ID from now on
restaurants <- linked_ids %>% 
  inner_join(restaurants, by = "ID_Rest") %>% 
  select(-X1, -ID_Rest)

inspections <- linked_ids %>% 
  inner_join(inspections, by = "ID_Rest") %>% 
  select(-X1, -ID_Rest)

violations <- inspections %>% 
  inner_join(violations, by = "ID_Inspect") %>%
  mutate(ID_Violation = seq(1:nrow(.))) %>% 
  select(ID_Violation, ID_Inspect, Description)


# Read in Yelp tables
config <-  read.ini("/home/brent/config.ini")
mysql_user <-  config$MySQL$user 
mysql_pwd <-  config$MySQL$password

conn <- dbConnect(MySQL(), user = mysql_user, password = mysql_pwd, dbname = "yelp_db", host = "127.0.0.1")

sql_business <- "SELECT id AS ID_yelp, name AS Name, city AS City, state AS State, stars AS Stars, review_count AS Review_count "
sql_business <- str_c(sql_business, "FROM business WHERE state IN ('ON', 'NV', 'IL', 'OH', 'PA');")
rs_business <- dbSendQuery(conn, sql_business)
business <-  fetch(rs_business, n=-1)
business <- business %>% 
              filter(ID_yelp %in% unique(restaurants$ID_yelp))

sql_review <- "SELECT r.id AS ID_Review, r.business_id AS ID_yelp, r.stars AS Stars, r.date AS Date, r.text AS Text "
sql_review <- str_c(sql_review, "FROM review AS r INNER JOIN business AS b ON r.business_id = b.id ")
sql_review <- str_c(sql_review, "WHERE b.state IN ('ON', 'NV', 'IL', 'OH', 'PA');")
rs_review <- dbSendQuery(conn, sql_review)
review <- fetch(rs_review, n=-1)
review <- review %>% 
            filter(ID_yelp %in% unique(restaurants$ID_yelp))



# Find avg number of violations per inspection for each restaurant
no_violations <- inspections %>% 
                  left_join(violations, by = "ID_Inspect") %>% 
                  filter(is.na(ID_Violation)) %>%
                  mutate(n = 0) %>% 
                  select(ID_Inspect, n)
  
gtr0_violations <- inspections %>% 
                    inner_join(violations, by = "ID_Inspect") %>% 
                    group_by(ID_Inspect) %>% 
                    count() %>% 
                    ungroup()

nbr_violations_per_inspect <- rbind(no_violations, gtr0_violations)

  
avg_by_rest <- nbr_violations_per_inspect %>% 
                inner_join(inspections, by = "ID_Inspect") %>% 
                group_by(ID_yelp) %>% 
                summarize(avg_vio_per_inspect = mean(n))
  
summ <- avg_by_rest %>% 
  inner_join(business, by = "ID_yelp") %>% 
  mutate(State = as.factor(State))


lm <- with(summ, lm(avg_vio_per_inspect ~ Stars + Review_count))
summary(lm)


p_stars <- ggplot(summ, aes(Stars, avg_vio_per_inspect))
p_stars + geom_jitter(size=0.8, alpha=0.5, aes(color = State)) + 
  labs(y = "Average Violations Per Inspection", 
       title = "Average Number of Violations per Health Inspection for a Restaurant vs. Yelp Rating")

p_review_count <- ggplot(summ, aes(Review_count, avg_vio_per_inspect))
p_review_count + geom_point(size=0.8, alpha=0.5, aes(color = State)) +
  labs(y = "Average Violations Per Inspection",
       x = "Number of Yelp Reviews",
       title = "Average Number of Violations per Health Inspection for a Restaurant vs. Number of Yelp Reviews")


# summ_lt1000reviews <- summ[summ$Review_count < 1000, ]
# summ$Popularity <- cut(summ$Review_count, 4)
# levels(summ$Popularity) <- 1:4

# summ %>% 
#   split(.$Popularity) %>% 
#   map(~ lm(avg_vio_per_inspect ~ Stars, data = .x)) %>% 
#   map(summary)

# 
# # NLP on textual reviews
# library(tokenizers)
# tokenize_word_stems(reviews_bobo$Text, stopwords = stopwords())
# 
# library(text2vec)
# prep_fun <- tolower
# tok_fun <- word_tokenizer
# 
# train_tokens <- reviews_bobo$Text %>% prep_fun %>% tok_fun
# it_train <- itoken(train_tokens,
#                    ids = reviews_bobo$ID_Review)
# 
# vocab <- create_vocabulary(it_train)
# vectorizer <- vocab_vectorizer(vocab)
# dtm_train <- create_dtm(it_train, vectorizer)
