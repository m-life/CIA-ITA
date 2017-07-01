# -------------------------------------------------------- #
# Title: CIA Data Reporting 
# Maintener: Marco Vita
# Season: 2016/2017
# Purpose: Monitor referees lists composition and activity 
# -------------------------------------------------------- #

require(gdata)
require(dplyr)
library(tidyr)
library(BBmisc)
library(rjson)
library(data.table)
library(stringr)

###########################
## -- Data collection -- ##
###########################

referees <- read.xls("referees.xlsx", sheet = 1, stringsAsFactors = FALSE, fileEncoding='latin1') 

referees <- referees %>% 
  filter(Tesseramento %in% c("Internazionale", 
                             "Arbitro Nazionale", 
                             "Arbitro Regionale", 
                             "Arbitro Amatoriale", 
                             "Mini Arbitro"),
         Stato.tesseramento == "Rinnovato") %>%
  mutate(Data.nascita = as.Date(Data.nascita)) %>%
  select(Codice.FIP, Abilitazione, Cognome, Nome, Data.nascita, Regione, Provincia)

province <- read.csv("province.csv", sep = ";", fileEncoding='latin1', stringsAsFactors = FALSE) %>%
  mutate(Provincia = toupper(Provincia))

referees <- merge(referees, province, all.x = TRUE)
referees$Pr[is.na(referees$Pr)] <- "NA"
colnames(referees) <- tolower(colnames(referees))

referees$categoria <- "R"
referees$categoria[referees$abilitazione == "Serie A"] <- "A"
referees$categoria[referees$abilitazione == "Serie A2 e A1/F"] <- "A2"
referees$categoria[referees$abilitazione == "Serie A2 e A1/F con deroga Serie A"] <- "A2"
referees$categoria[referees$abilitazione == "Serie B e A2/F"] <- "B"
referees$categoria[referees$abilitazione == "Serie B e A2/F con deroga A1/F"] <- "B"
referees$categoria[referees$abilitazione == "Serie C"] <- "C"
referees$categoria[referees$abilitazione == "Serie C con deroga A1/F"] <- "C"
referees$categoria[referees$abilitazione == "Serie C con deroga A2/F"] <- "C"
referees$categoria[referees$abilitazione == "Serie C con deroga A2/F"] <- "C"
referees$categoria[referees$abilitazione == "MiniArbitro"] <- "Mini"

referees <- referees %>% 
  select(tessera = codice.fip, cognome, nome, nascita = data.nascita, regione, pr, categoria)

## save tessere.csv

write.csv(referees %>% select(tessera) %>% arrange(tessera), "tessere.csv")

####

games <- read.csv("all.csv", sep = ";", stringsAsFactors = F, encoding = "latin1")
games$data = as.Date(games$data, "%d/%m/%Y")

games <- games %>%
  mutate(check = substr(gara,1,1)) %>%
  filter(check %in% c("0","1","2","3","4","5","6","7","8","9")) %>%
  filter(stato != "Manifestazione") %>%
  select(tessera, data, stato, gara)

games$ngara = unlist(strsplit(games$gara,':'))[2*(1:length(games$gara))-1]
games$dett = unlist(strsplit(games$gara,':'))[2*(1:length(games$gara))]
games$comitato = substr(games$dett, 1, 4)
games$camp = unlist(str_split(games$dett,'-', n=2))[2*(1:length(games$gara))-1]
games$camp = str_sub(games$camp, 5)
games$camp = gsub(" ", "", games$camp)
games$comitato = gsub(" ", "", games$comitato)
games$squadre = unlist(str_split(games$dett,'-', n=2))[2*(1:length(games$gara))]
games <- games %>% select(tessera, data, stato, comitato, ngara, camp, squadre) %>%
  filter(data < '2017-04-01')

###

regioni <- games %>%
  group_by(comitato, camp, ngara) %>%
  summarise(n=n()) %>%
  select(comitato, camp, ngara)

regioni_camp <- regioni %>%
  group_by(comitato, camp) %>%
  summarise(n=n())

regioni_tot <- regioni %>%
  group_by(comitato) %>%
  summarise(n=n()) %>%
  mutate(camp="totale") %>%
  select(comitato, camp, n)

regioni_tot <- data.frame(regioni_tot)
regioni_camp <- data.frame(regioni_camp)
gare_regioni <- rbind(regioni_camp, regioni_tot) %>% arrange(comitato)


regioni_camp = regioni_camp %>%
  mutate(giovanile = ifelse(camp %in% c("C1", "C2", "D", "B/F", "PM", "C/F", "PF", "1DM", "OPENM", "OPENF"), "no", "si")) %>%
  mutate(giovanile = ifelse(comitato =="NAZ", "no", giovanile))

regioni_seniores <- regioni_camp %>%
  filter(giovanile=="no") %>%
  group_by(comitato) %>%
  summarise(n=sum(n)) %>%
  mutate(camp="seniores") %>%
  select(comitato, camp, n)

regioni_giovanili <- regioni_camp %>%
  filter(giovanile=="si") %>%
  group_by(comitato) %>%
  summarise(n=sum(n)) %>%
  mutate(camp="giovanili") %>%
  select(comitato, camp, n)
  

gare_regioni <- rbind(gare_regioni, regioni_giovanili, regioni_seniores) %>% arrange(comitato)


##

dati_regioni <- data.frame(regione = unique(gare_regioni$comitato))

dati_regioni <- merge(dati_regioni, gare_regioni[gare_regioni$camp=='totale',], by.x = 'regione', by.y = 'comitato')
dati_regioni <- dati_regioni %>%
  mutate(gare_totali = n) %>%
  select(regione, gare_totali)

dati_regioni <- merge(dati_regioni, gare_regioni[gare_regioni$camp=='seniores',], by.x = 'regione', by.y = 'comitato')
dati_regioni <- dati_regioni %>%
  mutate(gare_senior = n) %>%
  select(regione, gare_totali, gare_senior)


dati_regioni <- merge(dati_regioni, gare_regioni[gare_regioni$camp=='giovanili',], by.x = 'regione', by.y = 'comitato', all.x = TRUE)
dati_regioni <- dati_regioni %>%
  mutate(gare_giovanili = n) %>%
  select(regione, gare_totali, gare_senior, gare_giovanili)

write.csv2(dati_regioni, "gare_regioni.csv")

##


accettate <- games %>%
  filter(stato == "Accettata") %>% 
  mutate(tag = ifelse(camp %in% c("B/F", "PM", "C/F", "PF", "1DM", "OPENM", "OPENF"), "senior", "giovanile")) %>%
  mutate(tag = ifelse(camp %in% c("C1", "C2"), "C", tag)) %>%
  mutate(tag = ifelse(camp == "D", "D", tag)) %>%
  mutate(tag = ifelse(comitato =="NAZ", "nazionale", tag)) %>%
  select(-c(data, stato, squadre)) 

accettate <- accettate[accettate$tessera != 34686, ]
 

gare_arbitro <- accettate %>%
  group_by(tessera, tag) %>%
  summarise(n = n()) %>%
  select(tessera, tag, n)

gare_arbitro <- spread(gare_arbitro, tag, n, fill = 0)
gare_arbitro <- gare_arbitro %>%
  mutate(totale = nazionale + C + D + senior + giovanile)

##


df <- merge(referees, gare_arbitro, by.x = 'codice.fip', by.y = 'tessera', all.x = TRUE)
df <- df %>%
  mutate(tessera = codice.fip, nascita = data.nascita) %>%
  select(totale, nazionale, C, D, senior, giovanile, tessera, cognome, nome, 
         categoria, nascita, regione, pr, provincia, comune, telefono, email)
df[is.na(df)] <- 0

df <- df %>%
  mutate(stato = ifelse(totale > 0, "attivo", "inattivo"),
         max_cat = ifelse(categoria %in% c("Serie A", "Serie A2", "Serie B"), "N", "AS"),
         max_cat = ifelse(categoria == "Serie C", "C", max_cat),
         max_cat = ifelse(categoria == "MiniArbitro", "AJ", max_cat))

df[df$max_cat=="AS" & df$nascita > '1999-01-01', ]$max_cat <- "AJ"
df[df$max_cat=="AS" & df$D > 0, ]$max_cat <- "D"
df[df$max_cat=="AJ" & df$D > 0, ]$max_cat <- "D"

df <- df %>%
  mutate(anno = substr(nascita, 1, 4), eta = as.integer((Sys.Date()-nascita)/365))
df$eta[df$eta > 50] <- 50

df <- df %>% 
  mutate(fascia = ifelse(anno > "1970", "71-75", "66-70")) %>%
  mutate(fascia = ifelse(anno > "1975", "76-80", fascia)) %>%
  mutate(fascia = ifelse(anno > "1980", "81-85", fascia)) %>%
  mutate(fascia = ifelse(anno > "1985", "86-90", fascia)) %>%
  mutate(fascia = ifelse(anno > "1990", "91-94", fascia)) %>%
  mutate(fascia = ifelse(anno > "1994", "95-97", fascia)) %>%
  mutate(fascia = ifelse(anno > "1997", "98-99", fascia)) %>%
  mutate(fascia = ifelse(anno > "1999", "00-01", fascia)) %>%
  mutate(fascia = ifelse(anno > "2001", "02-03", fascia))



peranno = df %>% group_by(anno) %>% summarise(n = n())
  
  
bb <- df %>%
  group_by(max_cat, stato) %>%
  summarise(n = n()) 


catperfascia <- df %>%
  group_by(max_cat, fascia) %>%
  summarise(n = n()) 

statoperfascia <- df %>%
  group_by(fascia, stato) %>%
  summarise(n = n()) 

statoperfascia <- spread(statoperfascia, stato, n, fill = 0) %>%
  mutate(perc = round(attivo / (attivo + inattivo) * 100, 2))

df <- df %>% arrange(desc(tessera)) %>%
  mutate(fascia_tessera = 9)

for (i in 0:7){df[(500*i+1):(500*(i+1)),23] = i+1}

split_tessere = c()
for (i in 0:8){split_tessere <- c(split_tessere, df$tessera[i*500+1])}
split_tessere <- c(split_tessere, df$tessera[length(df$tessera)])

aa <- split_tessere
for (i in 1:9){aa[i] <- paste(split_tessere[i], "-", split_tessere[i+1])}
split_tessere <- aa[1:9]
split_tessere

df$fascia_tessera <- as.factor(df$fascia_tessera)
levels(df$fascia_tessera) <- split_tessere

statopertessera <- df %>%
  group_by(fascia_tessera, stato) %>%
  summarise(n = n()) 

statopertessera <- spread(statopertessera, stato, n, fill = 0) %>%
  mutate(perc = round(attivo / (attivo + inattivo) * 100, 2))


scatter.smooth(df$tessera[df$totale<100], df$totale[df$totale<100])



write.csv2(df, "totale_arbitri.csv")





##### new dati regioni con numero designazioni




new_regioni <- games %>%
  filter(stato=="Accettata") %>%
  group_by(comitato, camp) %>%
  summarise(n=n()) %>%
  select(comitato, camp, n)

new_regioni <- new_regioni %>%
  mutate(giovanile = ifelse(camp %in% c("C1", "C2", "D", "B/F", "PM", "C/F", "PF", "1DM", "OPENM", "OPENF"), "no", "si")) %>%
  mutate(giovanile = ifelse(comitato =="NAZ", "no", giovanile))

new_regioni_tot <- new_regioni %>%
  group_by(comitato) %>%
  summarise(tot = sum(n))
  
new_regioni_gio <- new_regioni %>%
  filter(giovanile=="si") %>%
  group_by(comitato) %>%
  summarise(gio = sum(n))

new_dati_regione <- merge(new_regioni_tot, new_regioni_gio, all.x = TRUE)
new_dati_regione[is.na(new_dati_regione)] <- 0
new_dati_regione <- new_dati_regione %>%
  mutate(sen = tot - gio)


write.csv2(new_dati_regione, "new_regioni.csv")



##### check 

aa <- read.csv("last.csv", sep = ";", stringsAsFactors = F, encoding = "latin1") %>% 
  filter(stato == "Accettata") %>%
  arrange(gara)

control <- read.csv("control.csv")

for (i in control$id){
  aa = aa %>% filter(tessera != i)
}


bb = aa %>%
  group_by(gara) %>%
  summarise(n=n())

cc = merge(aa, bb) 

cc = cc %>% arrange(desc(n), gara, tessera)

cc$ngara = unlist(strsplit(cc$gara,':'))[2*(1:length(cc$gara))-1]
cc$dett = unlist(strsplit(cc$gara,':'))[2*(1:length(cc$gara))]
cc$comitato = substr(cc$dett, 1, 4)
cc$camp = unlist(str_split(cc$dett,'-', n=2))[2*(1:length(cc$gara))-1]
cc$camp = str_sub(cc$camp, 5)
cc$camp = gsub(" ", "", cc$camp)
cc$comitato = gsub(" ", "", cc$comitato)

dd <- cc %>%
  filter(camp != "A2/M") %>%
  filter(camp != "A1/M") %>%
  filter(camp != "CI/M") %>%
  filter(camp != "C1") %>%
  filter(camp != "C2") %>%
  filter(camp != "D")