#####################################################################################
#instalacao dos pacotes necessarios
install.packages("tidyverse")
install.packages("lubridate")
install.packages("stringr")
install.packages('dplyr')
install.packages('tibble')
install.packages('plm')
install.packages('reshape2')
install.packages('geobr')
install.packages('xlsx')
install.packages('psych')
install.packages('MatchIt')

#carregando bibliotecas
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)
library(geobr)
library(xlsx)
library(psych)
library(readr)

######################################################################


#JUNTANDO A BASE QUE TEM TUDO MENOS 2017 E A QUE TEM 2017

######################
obitoind0 <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/obitoind.xlsx")



#nome das colunas
names(obitoind0) <- c('dsei', 'polobase', 'municipio' , 'ano' , 'cid' , 'causa')

obitoind0<- obitoind0%>%
  filter(ano  == '2017')

obitoind1 <- read_excel("C:/Users/Matheus/Desktop/obitodadecada.xlsx", 
                        sheet = "Plan1", col_types = c("skip", 
                                                       "text", "numeric", "text", "text", 
                                                       "text", "text", "text", "numeric", 
                                                       "date", "text", "text", "skip"))




names(obitoind1) <- c('dsei','npolobase' ,'polobase','ibge', 'municipio' ,
                     'uf', 'sexo', 'idade', 'data' , 'cid' , 'causa')

obitoind <- obitoind1%>%
 mutate(ano = as.numeric(format(data,'%Y')))%>%
select(dsei, polobase, municipio , ano , cid , causa)%>%
  add_row(obitoind0)
################



#IDENTIFICACAO DE CIDS

###############################
obitoind <-obitoindcomibge%>%
  mutate(cid3 = str_sub(cid, end= 3))%>%
  mutate(cid2 = str_sub(cid, end= 2))%>%
  mutate(cap = str_sub(cid, end= 1))%>%
  mutate(morte = 1)%>%
  mutate(resp = case_when(cap == 'J' ~ 1,
                          cap != 'J' ~ 0))%>%
  #definindo suicidio
  mutate(suicid = case_when(cid2=="X6" | cid2=="X7" |  cid3 == "X80" |cid3 == "X81" | cid3 == "X82"|cid3 == "X83" |cid3 == "X84" ~ 1,
                            cid2!="X6" | cid2!="X7" | cid2 != "X8"~0))%>%
  #definindo morte por conflito
  mutate(agressao = case_when(cid3 == "X85" |cid3 == "X86" |cid3 == "X87" 
                            |cid3 == "X88" |cid3 == "X89"|
                              cid2 == "X9" | cid2 == "Y0" ~ 1,
                            cid3 != "X85" |cid3 != "X86" |cid3 != "X87"
                            |cid3 != "X88" |cid3 != "X89"|
                              cid2 != "X9" | cid2 != "Y0" ~0 ))%>%
  #definindo mortes por ruins condicoes sanitarias
  mutate(saneamento = case_when(cid2 == 'A0'~ 1, cid2!= 'A0' ~ 0 ))%>%
  #prev1 = reduziveis por imunoprevencao
  mutate(prev1 = case_when(cid3 == 'A15' | cid3 == 'A16'|cid3 == 'A17'|
                             cid3 == 'A34'|cid3 == 'A35'|cid3 == 'A36'|
                             cid3 == 'A37'| cid3 == 'A80'| cid3 == 'B05'|
                             cid3 == 'B06'| cid3 == 'B16'| cid == 'G00.0'~1),
         #definindo prev2 - previniveis transmissiveis por atuacao do sistema de saude       
         prev2 = case_when(cid3 == 'A00'| cid3 == 'A01'| cid3 == 'A02'|
                             cid3 == 'A03'| cid3 == 'A04'| cid3 == 'A05'|
                             cid3 == 'A06'| cid3 == 'A07'| cid3 == 'A08'|
                             cid3 == 'A09'| cid3 == 'B20'| cid3 == 'B21'|
                             cid3 == 'B22'| cid3 == 'B23'| cid3 == 'B24'|
                             cid3 == 'B15'| cid3 == 'B17'| cid3 == 'B18'|cid3 == 'B19'|
                             cid3 == 'A50'|cid3 == 'A51'|
                             cid3 == 'A52'|cid3 == 'A53'|cid3 == 'A54'|
                             cid3 == 'A55'| cid3 == 'A56'|cid3 == 'A57'|cid3 == 'A58'|
                             cid3 == 'A59'|cid3 == 'A63'|cid3 == 'A64'| cid3 == 'N70'|
                             cid3 == 'N71' | cid3 == 'N72' | cid == 'N73.1' | cid3 == 'N73.2'|
                             cid3 == 'N73.3'| cid3 == 'N73.4'| cid == 'N73.5' | cid == 'N73.8'|
                             cid == 'N73.9'| cid3 == 'N75'| cid3 == 'N76'|
                             cid3 == 'A23'| cid3 == 'A24' | cid3 == 'A25' | cid3 == 'A26'|
                             cid3 == 'A28'| cid3 == 'A29'| cid3 == 'A30'| cid3 == 'A31'| cid3 == 'A32'| 
                             cid3 == 'A39'|cid3 == 'A40'|cid3 == 'A41'| 
                             cid3 == 'A46'|cid == 'A69.2'|cid == 'J02.0'|
                             cid == 'J03.0'|cid3 == 'B50'|cid3 == 'B51'|
                             cid3 == 'B52'|cid3 == 'B53'|cid3 == 'B54'|
                             cid == 'G00.1'|cid == ' G00.2'|cid == ' G00.3'|
                             cid == ' G00.4'|cid == ' G00.5'|cid == ' G00.6'|
                             cid == ' G00.7'|cid == ' G00.8'|cid == ' G00.9'|
                             cid3 == ' G01'|cid3 == 'I00'|cid3 == 'I01'| cid3 == 'I02'|
                             cid3 == 'I03'|cid3 == 'I04'|cid3 == 'I05'|cid3 == 'I06'|
                             cid3 == 'I07'|cid3 == 'I08'|cid3 == 'I09'|cid3 == 'J00'|
                             cid3 == 'J01'|cid == 'J02.8'|cid == 'J02.9'|cid == 'J03.8'|
                             cid == 'J03.9'|cid3 == 'J04'|cid3 == 'J05'|cid == 'J06.0'|
                             cid3 == 'J10'|cid3 == 'J11'|cid3 == 'J12'|cid3 == 'J13'|
                             cid3 == 'J14'|cid3 == 'J15'|cid3 == 'J16'|cid3 == 'J17'|
                             cid3 == 'J18'|cid3 == 'J19'|cid3 == 'J20'|cid3 == 'J21'|
                             cid3 == 'J22'|cid3 == 'L02'|cid3 == 'L03'|cid3 == 'L04'|
                             cid3 == 'L05'|cid3 == 'L06'|cid3 == 'L07'|cid3 == 'L08'|
                             cid3 == 'A20'|cid3 == 'A21'|cid3 == 'A22'|cid3 == 'A27'|
                             cid3 == 'A30'| cid3 == 'A77'|cid3 == 'A82'|cid3 == 'A90'|
                             cid == 'A92.3'|cid3 == 'A95'|cid == 'A98.5'|cid3 == 'B03'|
                             cid3 == 'B55'|cid == 'B57.0'|cid == 'B57.1'|cid3 == 'B65'|
                             cid == 'N39.0'~1),
         #prev3 - previniveis nao-transmissiveis por atuacao do sistema de saude
         prev3 = case_when(cid == 'B57.2'| cid3 == 'C00'| cid3 == 'C43'| cid3 == 'C44'|
                             cid3 == 'C22'| cid3 == 'C16'| cid3 == 'C18'| cid3 == 'C19'|
                             cid3 == 'C20'|cid3 == 'C21'|cid3 == 'C01'|cid3 == 'C02'|
                             cid3 == 'C03'|cid3 == 'C04'|cid3 == 'C05'|cid3 == 'C06'|
                             cid3 == 'C09'|cid3 == 'C10'|cid3 == 'C12'|cid3 == 'C13'|
                             cid3 == 'C14'|cid3 == 'C32'|cid3 == 'C15'|cid3 == 'C33'|
                             cid3 == 'C34'|cid3 == 'C50'|cid3 == 'C53'|cid3 == 'C54'|
                             cid3 == 'C55'|cid3 == 'C62'|cid3 == 'C73'|cid3 == 'C81'|
                             cid3 == 'C91'|cid3 == 'E01'|cid3 == 'E02'|cid3 == 'E03'|
                             cid3 == 'E04'|cid3 == 'E05'|cid3 == 'E00'|cid3 == 'E25.0'|
                             cid3 == 'E70.0'|cid3 == 'E74.2'|cid3 == 'E10'|cid3 == 'E11'|
                             cid3 == 'E12'|cid3 == 'E13'|cid3 == 'E14'|cid3 == 'E40'|
                             cid3 == 'E41'|cid3 == 'E42'|cid3 == 'E43'|cid3 == 'E44'|
                             cid3 == 'E45'|cid3 == 'E46'|cid3 == 'E50'|cid3 == 'E51'|
                             cid3 == 'E52'|cid3 == 'E53'|cid3 == 'E54'|cid3 == 'E55'|
                             cid3 == 'E56'|cid3 == 'E57'|cid3 == 'E58'|cid3 == 'E59'|
                             cid3 == 'E60'|cid3 == 'E61'|cid3 == 'E62'|cid3 == 'E63'|
                             cid3 == 'E64'|cid3 == 'D50'|cid3 == 'D51'|cid3 == 'D52'|
                             cid3 == 'D53'|cid3 == 'E86'|cid3 == 'F10'|cid == 'I42.6'|
                             cid == 'K29.2'|cid3 == 'K70'|cid3 == 'I85'|cid3 == 'G40'|
                             cid3 == 'G41'|cid3 == 'I10'|cid3 == 'I11'|cid3 == 'I12'|
                             cid3 == 'I13'|cid3 == 'I20'|cid3 == 'I21'|cid3 == 'I22'|
                             cid3 == 'I23'|cid3 == 'I24'|cid3 == 'I25'|cid3 == 'I70'|
                             cid3 == 'I50'|cid3 == 'I61'|cid == 'I63.0'|cid == 'I63.5'|
                             cid3 == 'I63.8'|cid3 == 'I63.9'|cid3 == 'I64'|cid3 == 'I65'|
                             cid3 == 'I66'|cid3 == 'J40'|cid3 == 'J41'|cid3 == 'J42'|
                             cid3 == 'J43'|cid3 == 'J45'|cid3 == 'J46'|cid3 == 'K25'|
                             cid3 == 'K28'|cid3 == 'K35'|cid3 == 'J60'|cid3 == 'J61'|
                             cid3 == 'J62'|cid3 == 'J63'|cid3 == 'J64'|cid3 == 'J65'|
                             cid3 == 'J66'|cid3 == 'J67'|cid3 == 'J68'|cid3 == 'J69'|
                             cid3 == 'J70'|cid3 == 'K40'|cid3 == 'K41'|cid3 == 'K42'|
                             cid3 == 'K43'|cid3 == 'K44'|cid3 == 'K45'|cid3 == 'K46'|
                             cid3 == 'K56'| cid3 == 'K80'|cid3 == 'K81'|cid3 == 'K82'|
                             cid3 == 'K83'|cid3 == 'N18'|cid3 == 'O00'|cid3 == 'O01'|
                             cid3 == 'O02'|cid3 == 'O03'|cid3 == 'O04'|cid3 == 'O05'|
                             cid3 == 'O06'|cid3 == 'O07'|cid3 == 'O08'|cid3 == 'O09'|
                             cid3 == 'O10'|cid3 == 'O11'|cid3 == 'O12'|cid3 == 'O13'|
                             cid3 == 'O14'|cid3 == 'O15'|cid3 == 'O16'|cid3 == 'O17'|
                             cid3 == 'O18'|cid3 == 'O19'|cid3 == 'O20'|cid3 == 'O21'|
                             cid3 == 'O22'|cid3 == 'O23'|cid3 == 'O24'|cid3 == 'O25'|
                             cid3 == 'O26'|cid2 == 'O3'|cid2 == 'O4'|cid2 == 'O5'|
                             cid2 == 'O6'|cid2 == 'O7'|cid2 == 'O8'|cid2 == 'O9'|
                             cid2 == 'V0'|cid2 == 'V1'|cid2 == 'V2'| cid2 == 'V3'|
                             cid2 == 'V4'|cid2 == 'V5'|cid2 == 'V6'|cid2== 'V7'|
                             cid2 == 'V8'|cid2 == 'V9'|cid3== 'W65'|cid3 == 'W66'|
                             cid3 == 'W67'|cid3 == 'W68'|cid3 == 'W69'|cid3 == 'W70'|
                             cid3 == 'W71'|cid3 == 'W72'|cid3 == 'W73'|cid3 == 'W74'|
                             cid2 == 'X0'| cid2 == 'X4' | cid2 == 'X6' |cid2 == 'X7'| 
                             cid3 == "X80" |cid3 == "X81" | cid3 == "X82"|cid3 == "X83" |cid3 == "X84"|
                             cid3 == "X85" |cid3 == "X86" |cid3 == "X87" |cid3 == "X88" |cid3 == "X89"|
                             cid2 == "X9" | cid2 == "Y0"|cid2 == "Y1" | cid2 == "Y2" |cid3 == "Y31" |cid3 == "Y32" |
                             cid3 == "Y33" |cid3 == "Y34" | cid2 == "W0" | cid2 == "W1" | cid2 == "Y6" |cid3 == "Y83" |
                             cid3 == "Y84" ~ 1),
         cardiaco = case_when(cap == 'I'~1),
         naoespec = case_when(cid3 == 'R99' | cid3 == 'R98' ~1))%>%
 ungroup() %>%
  replace_na(list(morte=0, suicid=0, agressao=0, 
                  resp=0, saneamento=0, prev=0,
                  prev1=0, prev2=0, prev3=0, cardiaco =0, naoespec =0))%>%
mutate(prev = prev1+prev2+prev3 )
  
##############

#COLAPSANDO POR MUNICIPIO

###############################
basepronta <- obitoind%>%
  #aqui pus para agrupar as mortes por municipio
  group_by(Codigo2,ano)%>%
  summarize(mortes_totais = sum(morte), suicidio = sum(suicid),
            agressao = sum(agressao), resp = sum(resp), saneamento = sum(saneamento),
            prev = sum(prev), prevum = sum(prev1), prevdois = sum(prev2), prevtres = sum(prev3),
            cardiaco = sum(cardiaco), naoespec = sum(naoespec))%>%
  mutate(regiao = substr(Codigo2, 1,1))
# ungroup()%>%
#   filter(ano == "2010" | ano == "2011"|ano == "2012" |ano == "2013" |ano == "2014" |ano == "2015" |
#            ano == "2016" | ano == "2017"| ano == "2018" | ano == "2019")

# 
# basemortesdodsei <- obitoind%>%
#   #aqui pus para agrupar as mortes por municipio
#   group_by(dsei,ano)%>%
#   summarize(dmortes_totais = sum(morte), dsuicidio = sum(suicid),
#             dagressao = sum(agressao), dresp = sum(resp), dsaneamento = sum(saneamento),
#             dprev = sum(prev), dprevum = sum(prev1), dprevdois = sum(prev2), dprevtres = sum(prev3))%>%
#   ungroup()%>%
#   filter(ano == "2010" | ano == "2011"|ano == "2012" |ano == "2013" |ano == "2014" |ano == "2015" |
#            ano == "2016" | ano == "2017"| ano == "2018" | ano == "2019")
# 
# basepronta <- basepronta%>%
#   left_join(basemortesdodsei, by='dsei')




#CODIGO IBGE, PIB, CFEM

#colocando codigo do ibge em cada municipio - recurso com nome de cada municipio e codigo ibge

mundsei <- read_excel("C:/Users/Matheus/Desktop/pedidosai/respostapedidopopulacao/Recurso Demografico 2010-2019.xlsx",
          sheet = "MUNICIPIO", col_types = c("text","text", "text"))


mundsei <- mundsei %>%
  transmute( ibge = CO_MUNICIPIO_IBGE, dsei = DSEI_GESTAO)

baseibge <- basepronta%>%
  mutate(ibge = as.character(Codigo2))%>%
  mutate(ano = as.character(ano))%>%
  left_join(mundsei, by = 'ibge')

############### 
# #  2) CFEM
# #adicionando mineracao - resulta na baseibge2
# 
#  cfem <- read_dta("C:/Users/Matheus/Desktop/economia/mineracao/cfem.dta")
# 
#  cfem <- cfem%>%
#    transmute(ibge = IBGE, ano, cfem)%>%
#    mutate(ano = as.character(ano))
# 
#  baseibge2 <- baseibge %>%
#    mutate(ano = as.character(ano))%>%
#  left_join(cfem, by = c('ibge', 'ano'))%>%
#    replace_na(list(cfem = 0))
# 
# 
# 
# 
# 
# 
# #  3) UF- 
# #criando uf para colocar terra indigena - baseibge3
# ufcod <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/ufcod.xlsx")
# 
# ufcod %>%
#   mutate(uf = as.double(uf))
# 
# 
# baseibge2 <- baseibge%>%
#   mutate(uf = str_sub(ibge, end=2))%>%
#   mutate(uf = as.double(uf))
# 
# baseibge3 <- baseibge2%>%
#   left_join(ufcod , by= 'uf')%>%
#   mutate(uf = as.character(uf))
# 
# 
# 
# 
# 
# 
# 
# #  4) PIB-
# #base4 = adicionando pib municipal medio e valor agropecuario de anos anteriores
# 
# PIB <- read_dta("C:/Users/Matheus/Desktop/PIB2.dta")
# 
# 
# pib <- PIB %>%
#   transmute(ibge = ibgecode2, ano = year, vabagro = vabagropecuária, pibmun =pibpreçoscorrentes, 
#             popmun =população, imposto = impostos)%>%
#   filter(ano < 2016, ibge != "")%>%
#   replace_na(list(pibmun = 0, popmun=0,vabagro=0))%>%
#   mutate( pibmun = as.numeric(pibmun), popmun = as.numeric(popmun), vabagro= as.numeric(vabagro))%>%
#   mutate(porcentagem_rural = vabagro/pibmun)%>%
#   mutate(rural = if_else(porcentagem_rural > 0.5, 1,0 ))%>%
#   mutate(porcentagem_imposto = imposto/pibmun)%>%
#   mutate(mtimposto = if_else(porcentagem_imposto > 0.20,1,0 ))%>%
#   group_by(ibge)%>%
#   summarize(anoscommtimposto = sum(mtimposto), anossendorural = sum(rural))
#   
#   
#   #mutate(pibpcap = pibmun/popmun)%>%
#   #mutate(agropcap = vabagro/popmun)%>%
#   #group_by(ibge)%>%
#   #summarize(medpibmun = mean(pibpcap), medvabagro = mean(agropcap))
# 
# 
# 
# baseibge4 <- baseibge3 %>%
#   left_join(pib, by ='ibge')
#   
# 
# 
# 
# 
# #COLOCANDO POLOS BASE POR DSEI  
PIB <- read_dta("C:/Users/Matheus/Desktop/PIB2.dta")
pib2<- PIB%>%
  filter(year == 2015)%>%
  mutate(pibpercapita = pibpreçoscorrentes/população)%>%
  transmute(ibge = ibgecode2, pibpcapita=pibpercapita)

baseibge<- baseibge%>%
  left_join(pib2, by = 'ibge')
#infraestrutura de saúde e numero de aldeias e etnias

saudeinfra <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/saudeinf/saudeinfra.xlsx",
                           sheet = "polos base")

etnias_dsei <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/saudeinf/saudeinfra.xlsx",
                              sheet = "etnia")

aldeias_dsei <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/saudeinf/saudeinfra.xlsx",
                            sheet = "aldeias")

#mudando de wide para long


saudeinfra <- melt(saudeinfra, measure.vars = c(2:11))%>%
                     rename(dsei = DSEI, ano = variable, polobase = value )

etnias_dsei<- melt(etnias_dsei, measure.vars = c(2:11))%>%
  rename(dsei = DSEI, ano = variable, netnias = value )

aldeias_dsei <- melt(aldeias_dsei, measure.vars = c(2:11))%>%
  rename(dsei = DSEI, ano = variable, naldeias = value )

caract_dsei <- saudeinfra %>%
  inner_join(etnias_dsei, by = c('dsei', 'ano'))%>%
  inner_join(aldeias_dsei, by= c('dsei', 'ano'))%>%
  mutate(ano = as.character(ano) )


baseibge <- baseibge %>%
  mutate(ano = as.character(ano))%>%
  left_join(caract_dsei, by = c('dsei', 'ano'))%>%
  mutate( poloporaldeia = polobase/naldeias)


######################################################


#  6) POPULAÇÃO POR DSEI
#adicionando população

pop_por_dsei <- read_excel("C:/Users/Matheus/Desktop/pedidosai/respostapedidopopulacao/Recurso Demografico 2010-2019.xlsx", 
             sheet = "DEMOGRAFICO")


names(pop_por_dsei) <- c('dsei', 'pop2010', 'pop2011','pop2012','pop2013','pop2014','pop2015','pop2016','pop2017',
                         'pop2018', 'pop2019')
pop_por_dsei<-pop_por_dsei%>%
  mutate(pop2020 = pop2019)


pop_por_dsei <- melt(pop_por_dsei, measure.vars = c(2:12))
  
pop_por_dsei<- pop_por_dsei %>%
  mutate(ano = str_sub(variable, start=4))%>%
  rename(popind = value)%>%
  select(dsei, ano, popind)

baseibge2 <- baseibge%>%
  left_join(pop_por_dsei, by = c('dsei', "ano"))

##############



# #  7) TERRAS INDÍGENAS
# #precisa de rodar a parte de terra indigena
# baseibgefinal <- baseibge5%>%
#   left_join(pop_por_dsei, by = c('dsei', 'ano'))%>%
#   #deixei sem ano na base da suposicao que as terras indigenas tem as suas condicoes representadas fixamente
#   left_join(munti_g, by = c('ibge'))%>%
#   filter(ano != 2020)

  
#adicionando informacoes de terras indigenas para 2 anos


#mergindo com a base poli16 que tem para cada municipio valores dos candidatos

#right join?
baseauxdsei <- baseibge2 %>%
  # left_join(poli16, by = 'ibge')%>%
  # mutate (muntevecand  = if_else (cand_ind>0 ,1,0))%>%
  # mutate (munelegeucand  = if_else (ind_eleito>0 ,1,0))
  group_by(dsei, ano)%>%
  summarize(m_totais_do_dsei = sum(mortes_totais),
         suicidio_total_dsei = sum(suicidio),
         agressao_total_dsei = sum(agressao),
         saneamento_total_dsei = sum(saneamento),
         resp_total_dsei = sum(resp),
         cardiaco_total_dsei = sum(cardiaco),
         naoespec_total_dsei = sum(naoespec),
         prev_total_dsei = sum(prev))%>%
  ungroup()


basecompleta16<- baseibge2%>%
  left_join(baseauxdsei, by = c('dsei', 'ano'))%>%
  mutate(tx_mortalidade_dsei = (m_totais_do_dsei/popind)*1000)%>%
  mutate(part_nas_mortes = (mortes_totais/m_totais_do_dsei)*100)%>%
  mutate(txmsuicidio = (suicidio_total_dsei/popind)*1000,
         txmsaneamento = (saneamento_total_dsei/popind)*1000,
         txmagressao = (agressao_total_dsei/popind)*1000,
         txmresp = (resp_total_dsei/popind)*1000,
         txmcard = (cardiaco_total_dsei/popind)*1000,
         txmnaoespec = (naoespec_total_dsei/popind)*1000,
         txmprev =(prev_total_dsei/popind)*1000 )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#8 ) MODELOS ERRADOS
############
# novoprobit2 <- glm(muntevecand ~ tx_mortalidade_dsei + poloporaldeia + m_integridade_amb+ m_ameaca_infra + anossendorural + anoscommtimposto,
#                    family = binomial(link = "probit"), 
#                    data = basecompleta16)
# summary(novoprobit2)
# 
# novoprobit3 <- glm(muntevecand ~ part_nas_mortes + poloporaldeia + m_integridade_amb+ 
#                      m_integridade_territorial + anossendorural + anoscommtimposto
#                    + m_pressao_infra ,
#                    family = binomial(link = "probit"), 
#                    data = basecompleta16)
# summary(novoprobit3)
# 
# 
# novoprobit4 <- glm(munelegeucand ~ part_nas_mortes + poloporaldeia + m_integridade_amb+ 
#                      m_integridade_territorial + anossendorural + anoscommtimposto
#                    + m_pressao_infra ,
#                    family = binomial(link = "probit"), 
#                    data = basecompleta16)
# summary(novoprobit4)
# 
# 
# novoprobit5 <- glm(munelegeucand ~ part_nas_mortes + vereador_ind + m_integridade_amb+ 
#                      m_integridade_territorial + anossendorural + m_estab_legal + anoscommtimposto
#                    + m_pressao_infra ,
#                    family = binomial(link = "probit"), 
#                    data = basecompleta16)
# summary(novoprobit5)
# 
# 
# novoprobit6 <- glm(munelegeucand ~ part_nas_mortes + vereador_ind + m_integridade_amb+ 
#                      m_integridade_territorial + anossendorural + m_estab_legal + anoscommtimposto
#                    + m_pressao_infra + m_governanciati ,
#                    family = binomial(link = "probit"), 
#                    data = basecompleta16)
# summary(novoprobit6)
# 
#####

######################################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%















