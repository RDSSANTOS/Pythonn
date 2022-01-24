# modulo (lib) padrão do Pyton para autenticação com API's externas
from rauth import OAuth1Service
import json

#Imprime erro caso exista
try:
    read_input = raw_input
except NameError:
    read_input = input

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
params = {'q': 'python',  # busca palavra
          }

#chamada da API com parametros
r = session.get('search/tweets.json', params=params, verify=True)

tweets = json.loads(r.content)
print (tweets['statuses'][0]['text'])
