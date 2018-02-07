# -*- coding: utf-8 -*-
"""
This script uses the recordlinkage package to link restaurants from health
inspection reports to businesses from Yelp's dataset. Match on Google Place ID first
and then pick the restaurant with the closest name match (over a certain threshold, i.e. 0.75).
"""

import pandas as pd
import mysql.connector
import recordlinkage as rl
import configparser
import re

# Create function to clean restuarant/business names in each dataset to 
# improve chances of returning a successful match
def clean_names(df):
#    re_punct = r"(^#\d*\s*)|(.[#@]\d+.*)|(\(.*\))"
    pat_common = r"""
        restaurant|bar|grille?|coffee|caff?e|diner|deli(catessen)?|
        market|store|shop|kitchen|bakery|foods?|eatery|pub|the|limited"""
    
    re_common = re.compile(pat_common, re.VERBOSE|re.IGNORECASE)
    
    df["Name_clean"] = df.Name.str.replace(r"\d+", "")
    df["Name_clean"] = df.Name_clean.str.replace(r"\W+", "")
    df["Name_clean"] = df.Name_clean.str.replace(re_common, "")
    df["Name_clean"] = df.Name_clean.str.replace(r"\s+", " ")
    df["Name_clean"] = df.Name_clean.str.lower()


config = configparser.ConfigParser()
config.read("/home/brent/config.ini")

goog_rest_inspect = pd.read_csv("data/inspected_restaurants_google_addresses.csv")
rest_inspect = pd.read_csv("data/restaurants.csv")
rest_inspect = pd.merge(rest_inspect, goog_rest_inspect, left_on="ID_Rest", right_on="ID")
rest_inspect = rest_inspect[["ID_Rest", "Name", "google_place_id", "formatted_address"]]


goog_yelp = pd.read_csv("data/yelp_google_addresses.csv")
mysql_user = config["MySQL"]["user"] 
mysql_pwd = config["MySQL"]["password"] 
conn = mysql.connector.connect(user = mysql_pwd, password = mysql_pwd, database = "yelp_db")
yelp = pd.read_sql("SELECT id AS ID_yelp, " + 
                           "name AS Name, " + 
                           "address AS Address, " + 
                           "city AS City, " + 
                           "state AS State, " + 
                           "postal_code AS Zip " +
                   "FROM business " +  
                   "WHERE state IN ('ON', 'NV', 'IL', 'OH', 'PA');", conn)

yelp = pd.merge(yelp, goog_yelp, left_on="ID_yelp", right_on="ID")
yelp = yelp[["ID_yelp", "Name", "google_place_id", "formatted_address"]]

clean_names(rest_inspect)
clean_names(yelp)

indexer = rl.BlockIndex(on = "google_place_id")
pairs = indexer.index(rest_inspect, yelp)
c = rl.Compare()
c.string("Name_clean", "Name_clean", method="levenshtein", label = "match_pct")

features = c.compute(pairs, rest_inspect, yelp)
features["idx_rest_inspect"] = [i[0] for i in features.index]
features["idx_yelp"] = [i[1] for i in features.index]

features = pd.merge(features, rest_inspect[["ID_Rest","Name", "Name_clean"]], 
                    left_on="idx_rest_inspect", right_index=True)
features = pd.merge(features, yelp[["ID_yelp", "Name", "Name_clean"]], 
                    left_on="idx_yelp", right_index=True)

features = features[features.match_pct >= 0.75]
features.reset_index(inplace=True)


# In case there are multiple matches for a given idx_rest_inspect, return
# only the record with the highest name match percentage
idx_max_pcts = features.groupby(["idx_rest_inspect"])["match_pct"].idxmax()
features = features.loc[idx_max_pcts]
features[["ID_Rest", "ID_yelp"]].to_csv("data/linked_ids.csv", encoding="utf8")
