"""
Example file to simulate an ETL process within a docker pipeline
- Extracts from a mongo db
- Transforms the collections
- Loads the transformed collections to postgres db

To be started by docker (see ../docker-compose.yml)
        
For inspecting that ETL worked out: docker exec -it pipeline_example_my_postgres_1 psql
"""


import pymongo
import sqlalchemy  # use a version prior to 2.0.0 or adjust creating the engine and df.to_sql()
import psycopg2
import time
import logging
import requests
import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

time.sleep(10)

# mongo db definitions
client = pymongo.MongoClient('headline_mongo', port=27017)  
db = client.headline_db
dbcoll = db.headline_collection


# postgres db definitions. HEADS UP: outsource these credentials and don't push to github.
USERNAME_PG = 'postgres'
PASSWORD_PG = 'postgres'
HOST_PG = 'headline_postgres'  # my_postgres is the hostname (= service in yml file)
PORT_PG = 5432
DATABASE_NAME_PG = 'headline_pgdb'

conn_string_pg = f"postgresql://{USERNAME_PG}:{PASSWORD_PG}@{HOST_PG}:{PORT_PG}/{DATABASE_NAME_PG}"

pg = sqlalchemy.create_engine(conn_string_pg)
pg_connect = pg.connect()

# Create the table
create_table_string = sqlalchemy.text("""CREATE TABLE IF NOT EXISTS headlines (
                                            headline_time TEXT,
                                            source TEXT,
                                            headline TEXT,
                                            compound NUMERIC
                                        );
                                      """)

pg_connect.execute(create_table_string)

create_table_string = sqlalchemy.text("""CREATE TABLE IF NOT EXISTS slack (
                                            headline_time TEXT,
                                            source TEXT,
                                            headline TEXT,
                                            compound NUMERIC
                                        );
                                      """)

pg_connect.execute(create_table_string)

def extract(skip_n=0):
    """
    reads collections from a mongo database and converts them into a pandas
    dataframe. 

    Parameters
    ----------
    skip_n : int, optional
        DESCRIPTION. The first skip_n collections are skipped

    Returns
    -------
    new_reddits : pandas dataframe

    """

    new_mongo_docs = dbcoll.find().skip(skip_n)
    new_headlines = pd.DataFrame.from_records(new_mongo_docs)
    
    n_headlines = new_headlines.shape[0]

    logging.critical(f"\n---- {n_headlines} headline extracted ----\n")
    return new_headlines


def transform(new_headlines):
    """
    transforms a dataframe containing reddits in a dictionary to a clean
    dataframe and adds a column for the (dummy/length) sentiment of the reddit

    Parameters
    ----------
    new_reddits : unclean pandas dataframe

    Returns
    -------
    new_reddits_df : cleaned pandas dataframe including "sentiments"
    """

    s = SentimentIntensityAnalyzer()
    new_headlines_df = new_headlines.copy()
    
    new_headlines_df.drop(columns=new_headlines_df.columns[0], axis=1, inplace=True )
    scores = new_headlines_df['headline'].apply(s.polarity_scores).apply(pd.Series)
    new_headlines_df['compound']= scores['compound']
    new_headlines_df['negative']= scores['neg']
    new_headlines_df['positive']= scores['pos']
    new_headlines_df['neutral']= scores['neu']
        
    return new_headlines_df


def load(new_headlines_df):
    """
    saves cleaned reddits including their sentiments to a postgres database

    Parameters
    ----------
    song : TYPE
        DESCRIPTION.
    sentiment : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """
    new_headlines_df[['headline_time', 'source', 'headline', 'compound']].to_sql('slack', pg_connect, if_exists='replace', index=False)
 
    dic = []
    dic = new_headlines_df[['headline_time', 'source', 'headline', 'compound']].head(1)
    
    webhook_url = "https://hooks...."

    text_list = dic.to_dict('records')
    text_dict = text_list[0]

    ht = text_dict['headline_time']
    sr = text_dict['source']
    hd = text_dict['headline']
    cm = text_dict['compound']
    text_out = f"Time: {ht}\nSource: *{sr}*\nHeadline: _{hd}_\nCompound: {cm}"

    dictionary = {'text': text_out}
    logging.critical(dictionary)

    logging.critical("\n\n++++++++++++ NEW POST HAS SENT TO SLACKBOT. ++++++++++++++\n\n")

    new_headlines_df[['headline_time', 'source', 'headline', 'compound']].to_sql('headlines', pg_connect, if_exists='append', index=False)

    logging.critical("\n\n----------- NEW HEADLINE HAS LOADED TO POSTGRES DATABASE. -------------\n\n")
    
    return None



n_old_headlines = 0

while True:
    time.sleep(5)  # get fake reddits all 5 seconds
   
    new_headlines = extract(skip_n=n_old_headlines)
    n_old_headlines += new_headlines.shape[0]

    new_headlines_df = transform(new_headlines)

    load(new_headlines_df)
    
