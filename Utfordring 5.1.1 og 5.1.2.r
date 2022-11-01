library(ggplot2)
library(gglorenz)
library(tidyverse)
library(PxWebApiData)
library(dplyr)
library(rjstat)
library(ineq)
library(janitor)
library(httr)

## laster inn datasett ved hjelp av api-spørringen som vi henter hos SSB.
url <- "https://data.ssb.no/api/v0/no/table/05185/"

data <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "1", "2" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "agg:Verdensdel2", "values": [ "b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6", "b8", "b9" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022" ] } } ], "response": { "format": "json-stat2" } }'

data1 <- POST(url, body = data, encode = "json", verbose())

## oppretter datasettet
bakgrunn <- fromJSONstat(content(data1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()

## Vi summerer kvinner og menn til en ny variabel kalt "tot_innvandring"
## fjerner også kvinner fra kjønn for å hindre dublikater.
bakgrunn <- bakgrunn %>% 
  group_by(landbakgrunn, ar) %>% 
  mutate(tot_innvandring = sum(value)) %>% 
  filter(kjonn == "Menn")

## bytter navn på landbakgurnn, ar og kjonn.
bakgrunn <- bakgrunn %>% 
  rename("region" = "landbakgrunn",
         "år" = "ar",
         "kjønn" = "kjonn")

## Lager plottet hvor år ligger på x-akse og antall innvandrere per region
## ligger på y-akse. Det er også lagt til farge for å illustrere hvilke regioner ligger høyest.
bakgrunn %>% 
  ggplot(aes(år, tot_innvandring, group = region, col = region)) +
  geom_line(aes(group = region),size = 1.25) +
  labs(x = "År",
       y = "Antall innvandrere per region",
       title = "Oversikt over total innvandring til Norge") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,400000, 25000)) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

### -------------------------------------------------------------------------------------------------------

## laster inn datasett ved hjelp av api-spørringen SSB gir.

url2 = "https://data.ssb.no/api/v0/no/table/13215/"

df = '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "0" ] } }, { "code": "Alder", "selection": { "filter": "item", "values": [ "15-74" ] } }, { "code": "InnvandrKat", "selection": { "filter": "item", "values": [ "B" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "item", "values": [ "015a" ] } }, { "code": "NACE2007", "selection": { "filter": "agg:NACE260InnvGrupp2", "values": [ "SNI-00-99", "SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2021" ] } } ], "response": { "format": "json-stat2" } }'

df1 <- POST(url2, body = df, encode = "json", verbose())

sysselsatte <- fromJSONstat(content(df1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()

## bytter navn til næring og år.
sysselsatte <- sysselsatte %>% 
  rename("næring" = "naering_sn2007",
         "år" = "ar")

## fjerner alle rader med tall og - i seg. samt fjerner rad nr 1 fordi det er en summering av alle næringer.
sysselsatte$næring <- gsub("[0-9.-]","", sysselsatte$næring)
sysselsatte <- sysselsatte[-1,]

## plotter med næring på x-akse og sysselsatte på y-akse. Farger skal illustrere hver enkelt næring.
sysselsatte %>% 
  ggplot(aes(næring, value, fill = næring)) +
  geom_col() +
  labs(y = "Antall sysselsatte",
       title = "Sysselsatte per næring") +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
  
