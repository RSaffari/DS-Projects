'''
Takes reddit data and saves them in mongodb. 
To be started by docker (see ../docker-compose.yml)
'''

import requests
from headline_info import tokens
from bs4 import BeautifulSoup
import time
import logging
import pymongo

time.sleep(5)
# 1. Create mongo database

# establish connection to client
client = pymongo.MongoClient('headline_mongo', port=27017)  # when within docker pipeline: replace 'localhost' with the name of the service

# define db 
db = client.headline_db # 'my_db' is name of db

# empty the database
client.drop_database('headline_db')

# define collection
dbcoll = db.headline_collection # includes database and collection name


url = 'https://www.newsnow.co.uk/{}'

session = requests.Session()
r = session.get(url.format('h/World+News?type=ln'))
content = BeautifulSoup(r.text, 'html.parser')

headlines = []
timelines = []
sources = []
mongo_input = {}
counter = 0

for hl in content.find_all('div', class_='hl'):
    #time.sleep(10)
    
    counter += 1
    if counter < 101:
    
        t = hl.find('span', class_='time')
        s = hl.find('span', class_='src src-part')
        h = hl.find('a', class_='hll')
        
        if (t and s and h):
            timeline = t.get_text()
            src = s.get_text()
            headline = h.get_text()
            
            if headline not in headlines:
            
                timelines.append(timeline)
                sources.append(src)
                headlines.append(headline)
                
                mongo_input={'id': counter, 'current_time': time.asctime(), 'headline_time': timeline, 'source': src, 'headline': headline}
                dbcoll.insert_one(mongo_input)
                
                if dbcoll.find_one(mongo_input):
                    logging.critical(f"\n---- Headline block number {counter} successfully inserted to Mongo database. ----\n")
                time.sleep(5)
                
            
    
    

