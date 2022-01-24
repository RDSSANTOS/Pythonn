## Robô com os modelos de predição utilizados para os alunos abaixo:
# **📚 MBA - Business Analytics & Big Data - T9 - FGV Paulista**
 
# ↪Bruno Pioli
 
# ↪Guilherme Alves
 
# ↪João Pedro Dannemann
 
# ↪Ricardo Santos

#!/usr/bin/env python
# coding: utf-8

#toda a parte de ações da API foram mantidas identicas ao Simple_robot.py
#foram retiradas as análises históricas e modelos de predição
#apesar de alguns tentativas de utilizar dados como: Dia da semana negociado, hora da negociação e tamanho da fonte de dados inferior ou superior aos 500 ultimos minutos, acabamos trabalhando apenas com a informação de fechamento e a tabela dos últimos 500 minutos, pois não conseguimos desenhar mecanimos que aparentassem trazer mais resultados.
#mais comentários sobre os modelos utilizados neste documento e nos documentos .ipynb

#é importante destacar que desenhamos 2 modelos LSTM para este trabalho, cada um rodando com um arquivo .ipynp
#Caso seja necessário escolher um modelo para valiação, o modelo escolhido foi o Modelo 2, definido aqui pela função : LSTMvetor
# In[1]:


import numpy as np
import pandas as pd
import requests
import statsmodels.api as sm
import math
import pickle
import time
from numpy import array
from numpy import hstack
from keras.models import Sequential
from keras.layers import LSTM
from keras.layers import Dense
from keras.layers import Bidirectional

urlbase = 'https://mighty-bastion-45199.herokuapp.com/'


def get_result(x):
    try:
        result = pd.DataFrame.from_dict(x.json())
    except:
        result = x.text
    return result

def api_post(route, payload):
    url = urlbase + route
    x = requests.post(url, data = payload)
    df = get_result(x)
    if type(df) == pd.core.frame.DataFrame:
        if 'datetime' in df.columns:
            df['datetime'] = pd.to_datetime(df['datetime'], unit='ms')
	
    return df

def api_get(route):
    url = urlbase + route
    x = requests.get(url)
    df = get_result(x)
    return df

def compute_quantity(coin_value, invest_value, significant_digits):
    a_number = invest_value/coin_value
    rounded_number =  round(a_number, significant_digits - int(math.floor(math.log10(abs(a_number)))) - 1)
    return rounded_number

def how_much_i_have(ticker, token):
    status = api_post('status', payload = {'token': token})
    status_this_coin = status.query(f"ticker == '{ticker}'")
    if status_this_coin.shape[0] > 0:
        return status_this_coin['quantity'].iloc[0]
    else:
        return 0
 
 #Modelo 1 - Previsão bidirecional para o próximo valor de fechamento:  

    #Divisor do array origem em steps
def split_sequence(raw_seq, n_steps):
	X, y = list(), list()
	for i in range(len(raw_seq)):
		# definir posição final após steps
		end_ix = i + n_steps
		# limitador do tamanho do array
		if end_ix > len(raw_seq)-1:
			break
		# define o padrão do input e output que será utilizado na predição
		seq_x, seq_y = raw_seq[i:end_ix], raw_seq[end_ix]
		X.append(seq_x)
		y.append(seq_y)
	return array(X), array(y)

    #LSTM Bidirecional para prever o próximo valor de fechamento da moeda
    #O modelo apresentou um resultado interessante e foi escolhido seguindo a fonte¹ listada no fim do documento
    #A idéia aqui é muito similar a original do robô, realizar compra ou venda do papel caso a tendência com o valor previsto do próximo fechamento seja superior a 0,02% de movimentação.
    
    
def LSTMunivar(raw_seq,n_steps):
    # Roda o Divisor do Array
    X, y = split_sequence(raw_seq, n_steps)
    # arruma o array para o formato de entrada no Modelo LSTM
    n_features = 1
    X = X.reshape((X.shape[0], X.shape[1], n_features))
    # define o modelo
    model = Sequential()
    model.add(Bidirectional(LSTM(50, activation='relu'), input_shape=(n_steps, n_features))) 
    #Foi realizado o teste com três tipos de ativação: "Relu", "Sigmoide" e "Tangente Hiperbólica"
    #segundo as fontes, o modelo de Tangente Hiperbólica seria mais apropriado para este tipo de análise, porém só conseguimos resultados coerentes utilizando a ativação 'Relu'.
    model.add(Dense(1))
    model.compile(optimizer='adam', loss='mse')
    # realiza o fit
    model.fit(X, y, epochs=100, verbose=0)
    # traz o valor previsto para o próximo ponto
    x_input = array(raw_seq[len(raw_seq)-n_steps:])
    x_input = x_input.reshape((1, n_steps, n_features))
    yhat = model.predict(x_input, verbose=0)
    return yhat #yhat é um array de 1 valor com a próxima predição

    #um dos problemas identificados neste modelo, é que como a quantidade de compra da moeda era fixa, não conseguiamos entrar ou sair de grandes posições mesmo quando a tendencia apresentava níveis elevados, tendo que aguardar por rodadas de 1 em 1 minuto aumentar ou reduzir mais a posição.
    #outro problema é devido a demora das análises e loops, o robo demorava em torno de 1m40s para aplicar cada rodada de compra e venda (respeitando a regra de "sleep" de 60s), isto fazia com que a cada 2 rodadas, tinhamos em média 3 entradas de dados novas, aumentando o risco de perder uma janela de virada de posição.
    
    #por estes motivos desenhamos um segundo modelo, levemente mais robusto que nos indicava um vetor das próximas duas predições de valores.
    #este modelo então nos deu mais variáveis para traçar a estratégia de compra e venda, além de agora também trabalharmos com um desenho variável de entrada em cada operação, que poderia variar entre 10 dolares e 1000 dolares de acordo com a proporção da tendência (regra explicitada no documento Modelo 2.ypinp
    #agora com a visão dos dois próximos valores, as regras de entrada ou saída de posição possuem 2 condições:
        # a primeira condição é de que ambas as previsões indiquem a mesma direção, o robô irá realizar operação de compra ou venda, independente do % de tendência (lembrando que o tamanho financeiro desta ordem continua proporcional a grandeza da tendência).
        # caso as previsões indiquem sentidos opostos (por exemplo, a primeira previsão mostre uma tendência positiva e a segunda previsão mostre uma tendência negativa em relação a primeira previsão), a ordem só é executada caso tendência final (comparação entre a segunda previsão e o ultimo valor disponível) seja superior a 0,05%
        #também foi reduzido para 50 epochs a análise deste modelo, pois o resultado com 100 epochs pareceu muito similar e apenas mais demorado.
        
    #Modelo 2: LSTM com saída vetorial, prevendo os próximos 2 valores de fechamento da moeda, para uma análise mais complexa.
def split_sequenceVet(raw_seq, n_steps_in, n_steps_out):
	X, y = list(), list()
	for i in range(len(raw_seq)):
		# find the end of this pattern
		end_ix = i + n_steps_in
		out_end_ix = end_ix + n_steps_out
		# check if we are beyond the sequence
		if out_end_ix > len(raw_seq):
			break
		# gather input and output parts of the pattern
		seq_x, seq_y = raw_seq[i:end_ix], raw_seq[end_ix:out_end_ix]
		X.append(seq_x)
		y.append(seq_y)
	return array(X), array(y)

def LSTMvetor(raw_seq, n_steps_in, n_steps_out):
    # split into samples
    X, y = split_sequenceVet(raw_seq, n_steps_in, n_steps_out)
    # reshape from [samples, timesteps] into [samples, timesteps, features]
    n_features = 1
    X = X.reshape((X.shape[0], X.shape[1], n_features))
    # define o modelo
    model = Sequential()
    model.add(LSTM(100, activation='relu', return_sequences=True, input_shape=(n_steps_in, n_features)))
    model.add(LSTM(100, activation='relu'))
    model.add(Dense(n_steps_out))
    model.compile(optimizer='adam', loss='mse')
    # fit model
    model.fit(X, y, epochs=50, verbose=0)
    # demonstrate prediction
    x_input = array(raw_seq[len(raw_seq)-n_steps_in:])
    x_input = x_input.reshape((1, n_steps_in, n_features))
    yhatVet = model.predict(x_input, verbose=0)
    return yhatVet


 ####################
 # Fontes utilizadas:
 # https://machinelearningmastery.com/how-to-develop-lstm-models-for-time-series-forecasting/
 # https://keras.io/api/layers/recurrent_layers/lstm/
 # https://analyticsindiamag.com/hands-on-guide-to-lstm-recurrent-neural-network-for-stock-market-prediction/
 # https://machinelearningmastery.com/choose-an-activation-function-for-deep-learning/
# In[ ]:




