import sqlite3

conn = sqlite3.connect('tweets_redes.db')

# definindo um cursor
cursor = conn.cursor()

# criando a tabela (tb_search_tweets)
cursor.execute("""
CREATE TABLE "tb_search_tweets" (
	"id_statuses"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"create_date"	TEXT,
	"id_tweet"	TEXT,
	"text_tweet"	TEXT,
	"retweet_count"	INTEGER,
	"user_id_tweet"	TEXT,
	"user_screen_name"	TEXT
);
""")

# criando a tabela (tb_relationship)
cursor.execute("""
CREATE TABLE "tb_relationship" (
	"id_relationship"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"user_id_tweet"	TEXT,
	"user_id_friends"	TEXT,
	"screen_name_friends"  TEXT
);
""")

conn.commit()

print('Tabela criada com sucesso.')

conn.close()