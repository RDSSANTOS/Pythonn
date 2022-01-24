# modulo (lib) padrão do Pyton para autenticação com API's externas
from rauth import OAuth1Service
import json
import sqlite3

#Imprime erro caso exista
try:
    read_input = raw_input
except NameError:
    read_input = input


conn = sqlite3.connect('tweets_redes.db')

# definindo um cursor
cursor = conn.cursor()

#link para gera as chaves da aplicação https://dev.twitter.com/apps/new
twitter = OAuth1Service(
    name='twitter',
    consumer_key='dSmbmYYVheT4VWgJbcrftlzM8',
    consumer_secret= 'oDGhgIc6VgQeYLwQD6IyyUyxuAqABtS5J2qwluxFEbmfNmtlYx',
    request_token_url='https://api.twitter.com/oauth/request_token',
    access_token_url='https://api.twitter.com/oauth/access_token',
    authorize_url='https://api.twitter.com/oauth/authorize',
    base_url='https://api.twitter.com/1.1/')

#m�todo gera tokem para urls de autenticação do usuario get_request_token()
request_token, request_token_secret = twitter.get_request_token()

#monta url para autorizar a aplicação e acessar os dados do usuario
authorize_url = twitter.get_authorize_url(request_token)

#exibe url de autorização
print('Visit this URL in your browser: {url}'.format(url=authorize_url))

#recebe PIN de autorização
pin = read_input('Enter PIN from browser: ')

#abre a session para a aplicação realizar as consultas
session = twitter.get_auth_session(request_token,
                                   request_token_secret,
                                   method='POST',
                                   data={'oauth_verifier': pin})

#parametros da consulta
params = {'q': 'palmeiras',  # busca palavra
          }

#chamada da API com parametros
r = session.get('search/tweets.json', params=params, verify=True)


for i, tweets in enumerate(r.json()["statuses"],1):
    date_tweet = tweets['created_at']
    id_tweet = tweets['id_str']
    text_tweet = tweets['text']
    retweet_count = tweets['retweet_count']
    user_id = tweets['user']['id_str']
    user_screen_name = tweets['user']['screen_name']
    #print(u'{0}- UserID:{1} - Name: @{2} | ReTweetCount:{3} - Teewt: @{4}'.format(i, user_id, user_screen_name, retweet_count, text_tweet))
    cursor.execute("INSERT into tb_search_tweets (create_date, id_tweet, text_tweet, retweet_count, user_id_tweet,user_screen_name) VALUES(?, ?, ?, ?, ?, ?)",
                  (date_tweet, id_tweet, text_tweet, retweet_count, user_id, user_screen_name ))
    conn.commit()

    params2 = {'include_user_entities' : 1,
                #'count': 10,
                'user_id': user_id
          }

    # lista que os usuarios seguem
    s = session.get('followers/list.json', params=params2, verify=True)

    #loop na variavel tweet que é os usuários do json result
    for i, twitter_user in enumerate(s.json()["users"],1):
        user_id_friend = twitter_user['id_str']
        screen_name_friend = twitter_user['screen_name']
        #print(u'{0}. {1}'.format(i, handle))
        cursor.execute("INSERT into tb_relationship (user_id_tweet, user_id_friends, screen_name_friends) VALUES( ?, ?, ?)",
                      (user_id, user_id_friend, screen_name_friend))
    conn.commit()

conn.close()