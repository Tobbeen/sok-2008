library(rjstat)
library(PxWebApiData)
library(httr)
library(janitor)
library(dplyr)
library(gglorenz)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ineq)

URL <- "https://data.ssb.no/api/v0/no/table/12558/"

# Laster inn data fra ssb slik vi lærte i sok-1005 og sok-1016. Hentet query fra ssb sin side.
data <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Kommune",
        "values": [
          "5401",
          "1902"
        ]
      }
    },
    {
      "code": "InntektSkatt",
      "selection": {
        "filter": "item",
        "values": [
          "00"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "VerdiDesil"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2020"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


## Fletter url lenken med query dataen vi hentet fra ssb.

data <- POST(URL, body = data, encode = "json", verbose())

## Lager dataframe av dataen som er blitt hentet fra ssb.
Troms?.inntekt <- fromJSONstat(content(data, "text")) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  rename(?r = ar)


#filtrerer eget datasett med inntekt fra 2005.
ineq_2005 <- Troms?.inntekt %>%
  filter(region == "Troms? (-2019)") %>% 
  select(region, value, ?r, desil) %>% 
  na.omit()
#filtrerer eget datasett med inntekt fra 2020.
ineq_2020 <- Troms?.inntekt %>% 
  filter(region=="Troms?") %>% 
  select(region, value, ?r, desil) %>% 
  na.omit()
#Dette gjør jeg for å få gini-tallet inn i plottet.


## Her plotter vi lorenz-kurven ved hjelp av gglorenz datapakken.
## x-aksen viser personer i Tromsø i kvartil, og y-aksen viser inntekt for hvert kvartil.

Troms?.inntekt %>%
  group_by(region) %>% 
  filter(?r %in% c("2005", "2020")) %>%
  na.omit() %>% 
  ggplot(aes(x = value, colour = ?r)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  scale_x_continuous() +
  scale_y_continuous() +
  labs(x = "Personer i Troms? i desil",
       y = "Inntekter i Troms? fordelt p? per desil",
       title = "Inntektsulikheten i Troms?")+
  #Her legger jeg inn gini-tallet fra inntektene i 2005 og 2020.
  annotate_ineq(ineq_2020$value, measure_ineq = "Gini", y = 0.8, decimals = 3) +
  annotate_ineq(ineq_2005$value, measure_ineq = "Gini", decimals = 3)


## Det vi kan oppfatte av figuren er at inntektsulikheten har økt med 0.02 siden 2005. Så 0.2% altså.
## Dette er en veldig liten økning som ikke forteller oss veldig mye.
