# -*- coding: utf-8 -*-
"""
This script uses the recordlinkage package to link restaurants from health
inspection reports to businesses from Yelp's dataset. State (and Zip if 
it exists) must be an exact match between the 2 datasets, 
while name and address can be approximate.
"""

import pandas as pd
import mysql.connector
import recordlinkage as rl

conn = mysql.connector.connect(user = "root", password = "root", database = "yelp_db")
b = pd.read_sql("SELECT id, name, address, city, state, postal_code " +
                "FROM business " +  
                "WHERE state IN ('ON', 'NV', 'IL', 'OH', 'PA');", conn)

r = pd.read_csv("data/restaurants.csv", dtype = object)
b.rename(columns = {"postal_code":"Zip", "state":"State"}, inplace = True)

# Clean restaurant names to improve matching
r["Name_clean"] = r.Name.str.replace(r"(^#\d*\s*)|(.[#@]\d+.*)|(\(.*\))", "")

# Strip out punctuation and extra whitespace from both names and addresses in both datasets
r["Name_clean"] = r.Name_clean.str.replace(r"\W+", "").str.lower()
r["Address_clean"] = r.Address.str.replace(r"\W+", "").str.lower()

b["Name_clean"] = b.name.str.replace(r"\W+", "").str.lower()                                                        
b["Address_clean"] = b.address.str.replace(r"\W+", "").str.lower()                                                        


# Too many records to find pairs all at once; subset by state and link 
rTO = 

indexer = rl.BlockIndex(on = "State")
pairs = indexer.index(r, b)

c = rl.Compare()

c.exact("postal_code", "postal_code", label = "postal_code")
c.string("Name_clean", "Name_clean", method="levenshtein", label = "Name_clean")
c.string("Address_clean", "Address_clean", method="levenshtein", label = "Address_clean")

features = c.compute(pairs, r, b)
matches = features[(features.name_clean >= 0.7) & (features.address_clean >= 0.6)]

r_matches = r.loc[[i[0] for i in matches.index]]
b_matches = b.loc[[i[1] for i in matches.index]]

r_matches = r_matches[["client_id", "name", "address", "city", "state", 
                       "postal_code", "municipality", "cat_code"]]

r_matches.rename(columns = {"name":"restaurant_name", "zip":"postal_code", 
                            "client_id":"restaurant_id"}, inplace = True)

b_matches = b_matches[["id", "name", "address"]]
b_matches.rename(columns = {"id":"id_yelp"})

r_matches.reset_index(inplace = True)
b_matches.reset_index(inplace = True)

linked_df = pd.concat([r_matches, b_matches], axis=1)

