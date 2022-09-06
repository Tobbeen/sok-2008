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

# Laster inn data fra ssb slik vi lÃ¦rte i sok-1005 og sok-1016. Hentet query fra ssb sin side.
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
Tromsø.inntekt <- fromJSONstat(content(data, "text")) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  rename(År = ar)


#filtrerer eget datasett med inntekt fra 2005.
ineq_2005 <- Tromsø.inntekt %>%
  filter(region == "Tromsø (-2019)") %>% 
  select(region, value, År, desil) %>% 
  na.omit()
#filtrerer eget datasett med inntekt fra 2020.
ineq_2020 <- Tromsø.inntekt %>% 
  filter(region=="Tromsø") %>% 
  select(region, value, År, desil) %>% 
  na.omit()
#Dette gjÃ¸r jeg for Ã¥ fÃ¥ gini-tallet inn i plottet.


## Her plotter vi lorenz-kurven ved hjelp av gglorenz datapakken.
## x-aksen viser personer i TromsÃ¸ i kvartil, og y-aksen viser inntekt for hvert kvartil.

Tromsø.inntekt %>%
  group_by(region) %>% 
  filter(År %in% c("2005", "2020")) %>%
  na.omit() %>% 
  ggplot(aes(x = value, colour = År)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  scale_x_continuous() +
  scale_y_continuous() +
  labs(x = "Personer i Tromsø i desil",
       y = "Inntekter i Tromsø fordelt på per desil",
       title = "Inntektsulikheten i Tromsø")+
  #Her legger jeg inn gini-tallet fra inntektene i 2005 og 2020.
  annotate_ineq(ineq_2020$value, measure_ineq = "Gini", y = 0.8, decimals = 3) +
  annotate_ineq(ineq_2005$value, measure_ineq = "Gini", decimals = 3)


## Det vi kan oppfatte av figuren er at inntektsulikheten har Ã¸kt med 0.02 siden 2005. SÃ¥ 0.2% altsÃ¥.
## Dette er en veldig liten Ã¸kning som ikke forteller oss veldig mye.
