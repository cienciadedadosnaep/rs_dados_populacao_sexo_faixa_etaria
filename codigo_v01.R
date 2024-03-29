######################################################
# 1) Carregar bibliotecas

library(tidyverse)
library(magrittr)
#library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)
library(jsonlite)

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

## Reorganizando a escala

dados %<>% mutate(`População com idade de 15 a 19 anos (pessoas) (IBGE)`=round(`População com idade de 15 a 19 anos (pessoas) (IBGE)`/1000,1))
dados %<>% mutate(`População com idade de 20 a 29 anos (pessoas) (IBGE)`=round(`População com idade de 20 a 29 anos (pessoas) (IBGE)`/1000,1))
dados %<>% mutate(`População com idade de 30 a 39 anos (pessoas) (IBGE)`=round(`População com idade de 30 a 39 anos (pessoas) (IBGE)`/1000,1))
dados %<>% mutate(`População com idade de 40 a 59 anos (pessoas) (IBGE)`=round(`População com idade de 40 a 59 anos (pessoas) (IBGE)`/1000,1))
dados

##  Perguntas e titulos 
T_ST_P_No_POPULACAO <- read_csv("data/TEMA_SUBTEMA_P_No - POPULACAO_v00.csv")

# Lembrar de substituir nomes de 
#names(dados) = c("ano","q1","q2","q3","q41","q42",
#                 "q43","q44","q45","q46","q47","q48")

names(dados) = c("ano","total","feminino","masculino",
                 "0-4","5-14","15-19","20-29","30-39",
                 "40-59","60-79","80+")



#dados %<>% gather(key = classe,
#                  value = consumo,-ano) 
dados_ad <- dados %>% select(ano,`15-19`,`20-29`,`30-39`,`40-59`) %>% arrange(ano)
nomes <- names(dados_ad)
#dados_ad_t <- t(dados_ad)

#dados_ad_tn <- data.frame(as.character(row.names(dados_ad_t)),dados_ad_t)

#row.names(dados_ad_tn) <- NULL

#dados_ad_t_anos <- dados_ad_tn[1,]
#names(dados_ad_t_anos) <- NULL 
#dados_ad_t_anos <- as.character(dados_ad_t_anos)

#dados_ad_tl <-  dados_ad_tn[-c(1),]

#teste_ad <- list(dados_ad_t_anos,dados_ad_tl)

#testejson_ad <- jsonlite::toJSON(teste_ad,dataframe = "values") 

#teste2_ad <- gsub('\\[\\[','[',testejson_ad)
#teste3_ad <- gsub('\\]\\]\\]',']',teste2_ad)
#teste3_ad 

#data_serie <- teste3_ad

#data_serie <- paste('[',teste3,']',sep = '')
#data_serie_mod <- gsub('\\\"','"',data_serie)

#dados_adulto <- dados %>% filter(classe %in% c('q43','q44','q45','q46'))
#dados_idoso <- dados %>% filter(classe %in% c('q47','q48'))
#dados %<>% select(-id)

# Temas Subtemas Perguntas



## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_POPULACAO %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)

#classes <- NULL
#classes <- levels(as.factor(dados_ca$classe))

# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#175676','#62acd1','#8bc6d2','#20cfef',
                         '#d62839','#20cfef','#fe4641','#175676',
                         '#175676','#62acd1','#8bc6d2','#20cfef')

simbolo_linhas <- c('emptyCircle','emptyTriangle','emptySquare',
                    'emptyDiamond','emptyRoundRect')

#for ( i in 1:length(classes)) {

objeto_0 <- dados_ad %>%
  #filter(classe %in% c(classes[1])) %>%
  select(ano,`15-19`,`20-29`,`30-39`,`40-59`) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(ano = as.character(ano)) %>% list()               

exportJson0 <- toJSON(objeto_0)

data_axis <- paste('["',gsub(' ','","',
                             paste(paste(as.vector(objeto_0[[1]]$ano)),
                                   collapse = ' ')),'"]',sep = '')

data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$`15-19`)),
                                   collapse = ' ')),']',sep = '')

data_serie1 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`20-29`)),
                                    collapse = ' ')),']',sep = '')
data_serie2 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`30-39`)),
                                    collapse = ' ')),']',sep = '')

data_serie3 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`40-59`)),
                                    collapse = ' ')),']',sep = '')



#for ( i in 1:length(classes)) {
#dados <- NULL
#dados <- data_serie


#  objeto_0 <- dados %>% list()
#    filter(classe %in% c(classes[i])) %>%
#    select(ano,consumo) %>% filter(ano<2019) %>%
#    arrange(ano) %>%
#    mutate(ano = as.character(ano)) %>% list()               

#exportJson0 <- toJSON(teste3_ad)


titulo<-T_ST_P_No_POPULACAO$TITULO[2]
subtexto<-"Painel do Saneamento"
link <- T_ST_P_No_POPULACAO$LINK[2]


texto<-paste('{"title":{"text":"',titulo,
             '","subtext":"',subtexto,
             '","sublink":"',link,'"},',
             '"tooltip":{"trigger":"item","responsive":"true","position":"top","formatter":"{c0} mil"},',
             '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":20,"show":true,',
             '"feature":{"dataZoom":{"yAxisIndex":"none"},',
             '"dataView":{"readOnly":false},',
             '"restore":{},"saveAsImage":{}}},"legend":{"show":true,"bottom":30},"grid":{"bottom":80},"xAxis":{"type":"category",',
             '"data":',data_axis,'},',
             '"yAxis":{"type":"value","axisLabel":{"formatter":"{value}mil"}},',
             '"graphic":[{"type":"text","left":"center","top":"bottom","z":100, "style":{"fill":"gray","text":"Obs: Ponto é separador decimal", "font":"8px sans-srif","fontSize":12}}],',
             '"series":[{"name":"',nomes[2],'","data":',data_serie,',',
             '"type":"bar","color":"',corsec_recossa_azul[2],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[1],
             '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[2],'","borderWidth":2}},',
             '{"name":"',nomes[3],'","data":',data_serie1,',',
             '"type":"bar","color":"',corsec_recossa_azul[3],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[2],
             '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[3],'","borderWidth":2}},',
             '{"name":"',nomes[4],'","data":',data_serie2,',',
             '"type":"bar","color":"',corsec_recossa_azul[4],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[2],
             '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[4],'","borderWidth":2}},',
             '{"name":"',nomes[5],'","data":',data_serie3,',',
             '"type":"bar","color":"',corsec_recossa_azul[1],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[2],
             '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[1],'","borderWidth":2}}',
             ']}',sep='')



## OBS - Incluir 
## Se for necessario coloca mais colunas além das 2 do default, e escolher 
## uma cor pelo vetor corsec_recossa_azul[i],

#{"type":"bar",','"seriesLayoutBy":"row","color":"',corsec_recossa_azul[3],
#               '","showBackground":true,"backgroundStyle":{"color":"rgba(180, 180, 180, 0)}"},',
#               '"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[3],
#               '","borderWidth":2}},',


#  SAIDA_POVOAMENTO$CODIGO[i] <- texto   
texto<-noquote(texto)


write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_POPULACAO$NOME_ARQUIVO_JS[2]),
                               '.json',sep =''))
write(texto,file = paste('data/',T_ST_P_No_POPULACAO$NOME_ARQUIVO_JS[2],
                         sep =''))

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))