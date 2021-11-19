######################################################
# 1) Carregar bibliotecas

library(tidyverse)
library(magrittr)
#library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)

# # Library para importar dados SQL
# library(DBI)
# library(RMySQL)
# library(pool)
# library(sqldf)
# library(RMariaDB)
# 
# # Carragamento de banco de dados
# 
# # Settings
# db_user <-'admin'
# db_password <-'password'
# db_name <-'cdnaep'
# #db_table <- 'your_data_table'
# db_host <-'127.0.0.1' # for local access
# db_port <-3306
# 
# # 3. Read data from db
# # drv=RMariaDB::MariaDB(),
# mydb <-  dbConnect(drv =RMariaDB::MariaDB(),user =db_user, 
#                    password = db_password ,
#                    dbname = 'cdnaep', host = db_host, port = db_port)
# 
# dbListTables(mydb)
# 
# s <- paste0("SELECT * from", " consumo_agua")
# rs<-NULL
# rs <- dbSendQuery(mydb, s)
# 
# dados<- NULL
# dados <-  dbFetch(rs, n = -1)
# dados
# #dbHasCompleted(rs)
# #dbClearResult(rs)

library(readr)
ssa_painel_saneamento_brasil <- read_delim("data/ssa_painel_saneamento_brasil.csv", na = c("-", "NA"), 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(ssa_painel_saneamento_brasil)

# Selecao de parte do banco que responde as perguntas da planilha de povoamento



dados <- ssa_painel_saneamento_brasil %>% select(Indicador,
         `População total (pessoas) (IBGE)`,
         `População feminina total (pessoas) (IBGE)`,
         `População masculina total (pessoas) (IBGE)`,
         `População com idade de 0 a 4 anos (pessoas) (IBGE)`,
         `População com idade de 5 a 14 anos (pessoas) (IBGE)`,                                                                                                
         `População com idade de 15 a 19 anos (pessoas) (IBGE)`,                                                                                               
         `População com idade de 20 a 29 anos (pessoas) (IBGE)`,                                                                                              
         `População com idade de 30 a 39 anos (pessoas) (IBGE)`,                                                                                               
         `População com idade de 40 a 59 anos (pessoas) (IBGE)`,                                                                                               
         `População com idade de 60 a 79 anos (pessoas) (IBGE)`,                                                                                               
         `População com 80 anos ou mais (pessoas) (IBGE)`) 

##  Perguntas e titulos 
T_ST_P_No_POPULACAO <- read_csv("data/TEMA_SUBTEMA_P_No - POPULACAO.csv")


names(dados) = c("ano",
                 "q1","q2","q3",
                 "q41",
                 "q42",
                 "q43",
                 "q44",
                 "q45",
                 "q46",
                 "q47",
                 "q48")


dados %<>% gather(key = classe,
                  value = consumo,-ano) 
#dados %<>% select(-id)
# Temas Subtemas Perguntas



## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_POPULACAO %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)

classes <- NULL
classes <- levels(as.factor(dados$classe))

# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#175676','#62acd1','#8bc6d2','#20cfef',
                         '#d62839','#20cfef','#fe4641','#175676',
                         '#175676','#62acd1','#8bc6d2','#20cfef')

for ( i in 1:length(classes)) {
  
  objeto_0 <- dados %>%
    filter(classe %in% c(classes[i])) %>%
    select(ano,consumo) %>% filter(ano<2019) %>%
    arrange(ano) %>%
    mutate(ano = as.character(ano)) %>% list()               
  
  exportJson0 <- toJSON(objeto_0)
  
  
  titulo<-T_ST_P_No_POPULACAO$TITULO[i]
  subtexto<-"Painel do Saneamento"
  link <- T_ST_P_No_POPULACAO$LINK[i]
  
  data_axis <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$ano)),
                                    collapse = ' ')),']',sep = '')
  
  data_serie <- paste('[',gsub(' ',',',
                               paste(paste(as.vector(objeto_0[[1]]$consumo)),
                                     collapse = ' ')),']',sep = '')
  
  texto<-paste('{"title":{"text":"',titulo,
               '","subtext":"',subtexto,
               '","sublink":"',link,'"},',
               '"tooltip":{"trigger":"axis"},',
               '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":45,"show":true,',
               '"feature":{"dataZoom":{"yAxisIndex":"none"},',
               '"dataView":{"readOnly":false},"magicType":{"type":["line","bar"]},',
               '"restore":{},"saveAsImage":{}}},"xAxis":{"type":"category",',
               '"data":',data_axis,'},',
               '"yAxis":{"type":"value","axisLabel":{"formatter":"{value}"}},',
               '"series":[{"data":',data_serie,',',
               '"type":"bar","color":"',corsec_recossa_azul[i],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},',
               '"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[i],'","borderWidth":2}}]}',sep='')
  
  #  SAIDA_POVOAMENTO$CODIGO[i] <- texto   
  texto<-noquote(texto)
  
  
  write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_POPULACAO$NOME_ARQUIVO_JS[i]),
                                 '.json',sep =''))
  write(texto,file = paste('data/',T_ST_P_No_POPULACAO$NOME_ARQUIVO_JS[i],
                           sep =''))
  
}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))