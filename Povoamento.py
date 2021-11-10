import time
from numpy import empty
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.keys import Keys


df = pd.read_csv('E:/Área de trabalho/Códigos/R/rs_dados_populacao_sexo_faixa_etaria/data/POVOAMENTO.csv', sep = ';')

pd.set_option('display.max_colwidth', None)
df.info()

subtemas = str(df['SUBTEMA'])
subtemal = list(subtemas.split('\n'))
subtemal.pop()

subtemal1 = ''
for i in subtemal:
    subtemal1 += str(i[3:]).strip() + ','
subtemal1 = subtemal1[:-1]

subtemal1 = list(subtemal1.split(','))
#print(subtemal1)

perguntas = str(df['PERGUNTA'])
perguntal = list(perguntas.split('\n'))
perguntal.pop()

perguntal1 = ''
for i in perguntal:
    perguntal1 += str(i[3:]).strip() + ','
perguntal1 = perguntal1[:-1]

perguntal1 = list(perguntal1.split(','))
#print(perguntal1)



codigos = str(df['NOME_ARQUIVO_JS'])
codigol= list(codigos.split('\n'))
codigol.pop()

codigol1 = ''
for i in codigol:
    codigol1 += str(i[3:]).strip() + '|'
codigol1 = codigol1[:-1]
codigol1 = list(codigol1.split('|'))
#print(codigol1)


driver = webdriver.Chrome()

driver.get('http://reconhecendosalvador.ufba.br/admin/#/collections/perguntas/new')

time.sleep(40)

for i in range(0,len(df)):

    code = list(pd.read_csv('E:/Área de trabalho/Códigos/R/rs_dados_populacao_sexo_faixa_etaria/data/'+codigol1[i],sep=';', encoding= 'latin-1'))

    subtema = driver.find_element_by_id('subtema-field-1')
    subtema.send_keys(subtemal1[i])
    time.sleep(5)
    subtema.send_keys(Keys.ENTER)

    pergunta = driver.find_element_by_id('title-field-2')
    pergunta.send_keys(perguntal1[i])

    codigo = driver.find_element_by_id('code-field-3')
    codigo.send_keys(code)
    driver.find_element_by_xpath('//span[text()="Publicar"]').click()
    time.sleep(3)
    driver.find_element_by_xpath('//span[text()="Publicar e criar novo(a)"]').click()
    time.sleep(15)
    driver.refresh()
    time.sleep(10)

