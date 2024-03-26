library(tidyverse)
library(haven)
library(dplyr)
library(sjmisc)
library(sjlabelled)
install.packages("labelled")
library(labelled)
library(lme4)
library(lmerTest)

## Step 1 -- Create dataset for analysis from raw Reuters Institute data

## Raw Data is not available in a combined dataset. Income is measured slightly differently in each country and requires country specific recoding.
## Therefore, data is cleaned and combined by country first before recoding IVs and DVs.

# Argentina

argentina = as.data.frame(Reuters_DNR_2021_Argentina)

# Recode income, drop no responses 
argentina$income_num = as.numeric(argentina$income_ar)
argentina = argentina %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

# Recode region 
frq(argentina$region_ar_grouped)
argentina$region_factor = as_factor(to_character(argentina$region_ar_grouped))
class(argentina$region_factor)
frq(argentina$region_factor)

argentina$country = "ARG"


reuters_combined = subset(argentina, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                                "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                                "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                                "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                                "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                                "Div2_2021_4", 
                                                "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(reuters_combined) = c("country", "gender", "age",  "news_frq", "news_interest", "politics_interest", "region" ,
                            "ideology", "financing1", "financing2", "financing3" , 
                            "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                            "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                            "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")



## Australia

## Recode Demos

australia = as.data.frame(Reuters_DNR_2021_Australia)

## Income
class(australia$income_Australia)
frq(australia$income_Australia)
australia$income_num = as.numeric(australia$income_Australia)

australia = australia %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

australia$country = "AUS"

australia$region_factor = as_factor(to_character(australia$region_Australia))
frq(australia$region_factor)


needed = subset(australia, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                      "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                      "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                      "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                      "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                      "Div2_2021_4", 
                                      "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                               "ideology", "financing1", "financing2", "financing3" , 
                               "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                               "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                               "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(reuters_combined,needed)

frq(combined$region)

## Austria

austria = as.data.frame(Reuters_DNR_2021_Austria)

## Income
frq(austria$income_Austria)
austria$income_num = as.numeric(austria$income_Austria)

austria = austria %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

austria$country = "AUT"

austria$region_factor = as_factor(to_character(austria$region_Austria))
frq(austria$region_factor)


needed = subset(austria, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Belgium

belgium = as.data.frame(Reuters_DNR_2021_Belgium)

## Income
frq(belgium$income_BE_M)
belgium$income_num = as.numeric(belgium$income_BE_M)

belgium = belgium %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

belgium$country = "BEL"

belgium$region_factor = as_factor(to_character(belgium$region_BE_quota))
frq(belgium$region_factor)

needed = subset(belgium, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Brazil

brazil = as.data.frame(Reuters_DNR_2021_Brazil)

## Income
frq(brazil$income_Brazil)
brazil$income_num = as.numeric(brazil$income_Brazil)

brazil = brazil %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

brazil$country = "BRA"

brazil$region_factor = as_factor(to_character(brazil$region_Brazil))
frq(brazil$region_grouped_Brazil)


needed = subset(brazil, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Bulgaria

bulgaria = as.data.frame(Reuters_DNR_2021_Bulgaria)

## Income
frq(bulgaria$income_bg)
bulgaria$income_num = as.numeric(bulgaria$income_bg)

bulgaria = bulgaria %>%
  filter(income_num != 15) %>%
  filter(income_num != 16)


bulgaria$country = "BUL"

bulgaria$region_factor = as_factor(to_character(bulgaria$region_grouped_bg))
frq(bulgaria$region_factor)

needed = subset(bulgaria, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                     "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                     "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                     "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                     "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                     "Div2_2021_4", 
                                     "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Canada

canada = as.data.frame(Reuters_DNR_2021_Canada)

## Income
frq(canada$income_CA_M)
canada$income_num = as.numeric(canada$income_CA_M)

canada = canada %>%
  filter(income_num != 20) %>%
  filter(income_num != 21)

canada$country = "CAN"

canada$region_factor = as_factor(to_character(canada$region_grouped_ca_M))
frq(canada$region_factor)


needed = subset(canada, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Chile

chile = as.data.frame(Reuters_DNR_2021_Chile)

## Income
frq(chile$income_cl)
chile$income_num = as.numeric(chile$income_cl)

chile = chile %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

chile$country = "CHL"

chile$region_factor = as_factor(to_character(chile$region_cl))
frq(chile$region_factor)


needed = subset(chile, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)
## Colombia

colombia = as.data.frame(Reuters_DNR_2021_Colombia)

## Income
frq(colombia$income_co)
colombia$income_num = as.numeric(colombia$income_co)

colombia = colombia %>%
  filter(income_num != 15) %>%
  filter(income_num != 16)

colombia$country = "COL"

colombia$region_factor = as_factor(to_character(colombia$region_grouped_co))
frq(colombia$region_factor)


needed = subset(colombia, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                     "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                     "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                     "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                     "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                     "Div2_2021_4", 
                                     "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Croatia

croatia = as.data.frame(Reuters_DNR_2021_Croatia)

## Income
frq(croatia$income_hr)
croatia$income_num = as.numeric(croatia$income_hr)

croatia = croatia %>%
  filter(income_num != 19) %>%
  filter(income_num != 20)

croatia$country = "HRZ"

croatia$region_factor = as_factor(to_character(croatia$region_hr_grouped))
frq(croatia$region_factor)


needed = subset(croatia, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Czech

Czech_Republic = as.data.frame(Reuters_DNR_2021_Czech_Republic)

## Income
frq(Czech_Republic$income_Czech)
Czech_Republic$income_num = as.numeric(Czech_Republic$income_Czech)

Czech_Republic = Czech_Republic %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

Czech_Republic$country = "CZE"

Czech_Republic$region_factor = as_factor(to_character(Czech_Republic$region_Czech))
frq(Czech_Republic$region_factor)


needed = subset(Czech_Republic, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                           "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                           "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                           "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                           "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                           "Div2_2021_4", 
                                           "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Denmark

denmark = as.data.frame(Reuters_DNR_2021_Denmark)

## Income
frq(denmark$income_Denmark)
denmark$income_num = as.numeric(denmark$income_Denmark)

denmark = denmark %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

denmark$country = "DNK"

denmark$region_factor = as_factor(to_character(denmark$region_Denmark))
frq(denmark$region_factor)


needed = subset(denmark, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)


## Finland

finland = as.data.frame(Reuters_DNR_2021_Finland)

## Income
frq(finland$income_Finland)
finland$income_num = as.numeric(finland$income_Finland)

finland = finland %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

finland$country = "FIN"

finland$region_factor = as_factor(to_character(finland$region_Finland))
frq(finland$region_factor)


needed = subset(finland, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## France 

france = as.data.frame(Reuters_DNR_2021_France)

## Income
frq(france$income_France)
france$income_num = as.numeric(france$income_France)

france = france %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

france$country = "FRA"

france$region_factor = as_factor(to_character(france$region_France))
frq(france$region_factor)


needed = subset(france, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Germany

germany = as.data.frame(Reuters_DNR_2021_Germany)

## Income
frq(germany$income_German)
germany$income_num = as.numeric(germany$income_German)

germany = germany %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

germany$country = "DEU"

germany$region_factor = as_factor(to_character(germany$region_nielsen))
frq(germany$region_factor)


needed = subset(germany, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Hungary

hungary = as.data.frame(Reuters_DNR_2021_Hungary)

## Income
frq(hungary$income_HU)
hungary$income_num = as.numeric(hungary$income_HU)

hungary = hungary %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

hungary$country = "HUN"

hungary$region_factor = as_factor(to_character(hungary$region_hu_grouped))
frq(hungary$region_factor)


needed = subset(hungary, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Greece

greece = as.data.frame(Reuters_DNR_2021_Greece)

## Income
frq(greece$income_GR)
greece$income_num = as.numeric(greece$income_GR)

greece = greece %>%
  filter(income_num != 19) %>%
  filter(income_num != 20)

greece$country = "GRE"

greece$region_factor = as_factor(to_character(greece$region_grouped_gr))
frq(greece$region_factor)


needed = subset(greece, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Indonesia ## Check what is missing

indonesia = as.data.frame(Reuters_DNR_2021_Indonesia)

## Income
class(indonesia$income_indonesia)
frq(indonesia$income_ID)
indonesia$income_num = as.numeric(indonesia$income_ID)

indonesia = indonesia %>%
  filter(income_num != 16) %>%
  filter(income_num != 17)

indonesia$country = "IDN"

indonesia$region_factor = as_factor(indonesia$region_id)
frq(indonesia$region_factor)

indonesia$income_num = indonesia$income_num/15

needed = subset(indonesia, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                      "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                      "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                      "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                      "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                      "Div2_2021_4", 
                                      "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$country)


## Ireland

ireland = as.data.frame(Reuters_DNR_2021_Ireland)

## Income
frq(ireland$income_Ireland)
ireland$income_num = as.numeric(ireland$income_Ireland)

ireland = ireland %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

ireland$country = "IRE"

ireland$region_factor = as_factor(to_character(ireland$region_Ireland))
frq(ireland$region_factor)


needed = subset(ireland, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Italy

italy = as.data.frame(Reuters_DNR_2021_Italy)

## Income
frq(italy$income_italy)
italy$income_num = as.numeric(italy$income_italy)

italy = italy %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

italy$country = "ITA"

italy$region_factor = as_factor(to_character(italy$region_Italy))
frq(italy$region_factor)

needed = subset(italy, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)


## Japan

japan = as.data.frame(Reuters_DNR_2021_Japan)

## Income
frq(japan$income_japan)
japan$income_num = as.numeric(japan$income_japan)

japan = japan %>%
  filter(income_num != 16) %>%
  filter(income_num != 17)

japan$country = "JPN"

japan$region_factor = as_factor(to_character(japan$region_Japan))
frq(japan$region_factor)


needed = subset(japan, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$region)

## Kenya ## Missing Values!

kenya = as.data.frame(Reuters_DNR_2021_Kenya)

## Income
class(kenya$income_kenya)
frq(kenya$income_KN)
kenya$income_num = as.numeric(kenya$income_KN)

kenya = kenya %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

kenya$country = "KEN"

kenya$region_factor = as_factor(kenya$region_KN)
frq(kenya$region_factor)

kenya$income_num = kenya$income_num/17

needed = subset(kenya, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

frq(combined$country)

## Malaysia -- Check for missing

frq(kenya$income_KN)
kenya$income_num = as.numeric(kenya$income_KN)

kenya = kenya %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

kenya$country = "KEN"

kenya$region_factor = as_factor(kenya$region_KN)
frq(kenya$region_factor)

kenya$income_num = kenya$income_num/17

needed = subset(kenya, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Mexico

mexico = as.data.frame(Reuters_DNR_2021_Mexico)

frq(mexico$income_mx)
mexico$income_num = as.numeric(mexico$income_mx)

mexico = mexico %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

mexico$country = "MEX"

mexico$region_factor = as_factor(to_character(mexico$region_mx_grouped))
frq(mexico$region_factor)


needed = subset(mexico, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Netherlands

netherlands = as.data.frame(Reuters_DNR_2021_Netherlands)

frq(netherlands$income_NL)
netherlands$income_num = as.numeric(netherlands$income_NL)

netherlands = netherlands %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

netherlands$country = "NLD"

netherlands$region_factor = as_factor(to_character(netherlands$region_NL))
frq(netherlands$region_factor)


needed = subset(netherlands, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                        "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                        "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                        "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                        "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                        "Div2_2021_4", 
                                        "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Nigeria

## Norwary

norway = as.data.frame(Reuters_DNR_2021_Norway)

frq(norway$income_NO)
norway$income_num = as.numeric(norway$income_NO)

norway = norway %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

norway$country = "NOR"

norway$region_factor = as_factor(to_character(norway$region_NO))
frq(norway$region_factor)


needed = subset(norway, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)
## Peru

peru = as.data.frame(Reuters_DNR_2021_Peru)

frq(peru$income_pr)
peru$income_num = as.numeric(peru$income_pr)

peru = peru %>%
  filter(income_num != 17) %>%
  filter(income_num != 18)

peru$country = "PER"

peru$region_factor = as_factor(to_character(peru$region_pr_rc))
frq(peru$region_factor)


needed = subset(peru, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                 "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                 "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                 "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                 "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                 "Div2_2021_4", 
                                 "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Philippines

philippines = as.data.frame(Reuters_DNR_2021_Philippines)

frq(philippines$income_PH)
philippines$income_num = as.numeric(philippines$income_PH)

philippines = philippines %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

philippines$country = "PHI"

philippines$region_factor = as_factor(to_character(philippines$region_ph_w))
frq(philippines$region_factor)


needed = subset(philippines, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                        "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                        "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                        "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                        "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                        "Div2_2021_4", 
                                        "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Poland

poland = as.data.frame(Reuters_DNR_2021_Poland)

frq(poland$income_Poland)
poland$income_num = as.numeric(poland$income_Poland)

poland = poland %>%
  filter(income_num != 17) %>%
  filter(income_num != 18)

poland$country = "POL"

poland$region_factor = as_factor(to_character(poland$region_Poland))
frq(poland$region_factor)


needed = subset(poland, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Portugal

portugal = as.data.frame(Reuters_DNR_2021_Portugal)

frq(portugal$income_Port)
portugal$income_num = as.numeric(portugal$income_Port)

portugal = portugal %>%
  filter(income_num != 19) %>%
  filter(income_num != 20)

portugal$country = "POR"

portugal$region_factor = as_factor(to_character(portugal$region_Port))
frq(portugal$region_factor)


needed = subset(portugal, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                     "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                     "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                     "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                     "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                     "Div2_2021_4", 
                                     "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Romania

romania = as.data.frame(Reuters_DNR_2021_Romania)

frq(romania$income_ro)
romania$income_num = as.numeric(romania$income_ro)

romania = romania %>%
  filter(income_num != 19) %>%
  filter(income_num != 20)

romania$country = "ROM"

romania$region_factor = as_factor(to_character(romania$region_ro))
frq(romania$region_factor)


needed = subset(romania, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                    "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                    "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                    "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                    "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                    "Div2_2021_4", 
                                    "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Singapore

singapore = as.data.frame(Reuters_DNR_2021_Singapore)

frq(singapore$income_sg)
singapore$income_num = as.numeric(singapore$income_sg)

singapore = singapore %>%
  filter(income_num != 21) %>%
  filter(income_num != 22)

singapore$country = "SGP"

singapore$region_factor = as_factor(to_character(singapore$region_sg_rec))
frq(singapore$region_factor)


needed = subset(singapore, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                      "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                      "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                      "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                      "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                      "Div2_2021_4", 
                                      "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

combined = subset(combined, country != "SGP")
frq(combined$country)

## Slovakia

slovakia = as.data.frame(Reuters_DNR_2021_Slovakia)

frq(slovakia$income_sk)
slovakia$income_num = as.numeric(slovakia$income_sk)

slovakia = slovakia %>%
  filter(income_num != 20) %>%
  filter(income_num != 21)

slovakia$country = "SLK"

slovakia$region_factor = as_factor(to_character(slovakia$region_sk_grouped))
frq(slovakia$region_factor)


needed = subset(slovakia, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                     "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                     "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                     "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                     "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                     "Div2_2021_4", 
                                     "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## South Korea

southkorea = as.data.frame(Reuters_DNR_2021_South_Korea)

frq(southkorea$income_KR)
southkorea$income_num = as.numeric(southkorea$income_KR)

southkorea = southkorea %>%
  filter(income_num != 20) %>%
  filter(income_num != 21)

southkorea$country = "SKR"

southkorea$region_factor = as_factor(to_character(southkorea$region_KR))
frq(southkorea$region_factor)


needed = subset(southkorea, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                       "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                       "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                       "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                       "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                       "Div2_2021_4", 
                                       "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Spain

spain = as.data.frame(Reuters_DNR_2021_Spain)

frq(spain$income_Spain)
spain$income_num = as.numeric(spain$income_Spain)

spain = spain %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

spain$country = "SPN"

spain$region_factor = as_factor(to_character(spain$region_Spain))
frq(spain$region_factor)


needed = subset(spain, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Sweden

sweden = as.data.frame(Reuters_DNR_2021_Sweden)

frq(sweden$income_SE)
sweden$income_num = as.numeric(sweden$income_SE)

sweden = sweden %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

sweden$country = "SWE"

sweden$region_factor = as_factor(to_character(sweden$region_SE))
frq(sweden$region_factor)


needed = subset(sweden, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                   "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                   "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                   "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                   "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                   "Div2_2021_4", 
                                   "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Swiss

swiss = as.data.frame(Reuters_DNR_2021_Switzerland)

frq(swiss$income_CH_M)
swiss$income_num = as.numeric(swiss$income_CH_M)

swiss = swiss %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

swiss$country = "SWI"

swiss$region_factor = as_factor(to_character(swiss$region_ch_grouped_M))
frq(swiss$region_factor)


needed = subset(swiss, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## Thailand



## Turkey

## UK

uk = as.data.frame(Reuters_DNR_2021_UK)

frq(uk$income_uk)
uk$income_num = as.numeric(uk$income_uk)

uk = uk %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

uk$country = "UK"

uk$region_factor = as_factor(to_character(uk$region_UK))
frq(uk$region_factor)


needed = subset(uk, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                               "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                               "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                               "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                               "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                               "Div2_2021_4", 
                               "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)

## US

usa = as.data.frame(Reuters_DNR_2021_US)

frq(usa$income_US)
usa$income_num = as.numeric(usa$income_US)

usa = usa %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

usa$country = "AAUSA"

usa$region_factor = as_factor(to_character(usa$region_US))
frq(usa$region_factor)

frq(usa$Q_IMPARTIAL2_2021)

needed = subset(usa, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                "Div2_2021_4", 
                                "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3"))

names(needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust")


combined = full_join(combined,needed)



## Recoding variables 

## Perceptions of fairness towards social groups

combined$polgroup_recoded = as.numeric(combined$polgroup)
combined$polgroup_recoded = recode(combined$polgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

combined$agegroup_recoded = as.numeric(combined$agegroup)
combined$agegroup_recoded = recode(combined$agegroup_recoded, `1` = 1, `2` = -1, `998` = 0)

combined$gendergroup_recoded = as.numeric(combined$gendergroup)
combined$gendergroup_recoded = recode(combined$gendergroup_recoded, `1` = 1, `2` = -1, `998` = 0)

combined$classgroup_recoded = as.numeric(combined$classgroup)
combined$classgroup_recoded = recode(combined$classgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

combined$ethnicgroup_recoded = as.numeric(combined$ethnicgroup)
combined$ethnicgroup_recoded = recode(combined$ethnicgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

combined$regionalgroup_recoded = as.numeric(combined$regionalgroup)
combined$regionalgroup_recoded = recode(combined$regionalgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

library(psych)

vars = c("polgroup_recoded", "agegroup_recoded", "gendergroup_recoded", "classgroup_recoded", "ethnicgroup_recoded", "regionalgroup_recoded")
df = combined[vars]

print(alpha(df))

## Raw alpha = 0.83. Good internal consistency. Can be combined into one variable.

combined$socialgroups = (combined$polgroup_recoded + combined$agegroup_recoded + combined$gendergroup_recoded + combined$classgroup_recoded
                         + combined$ethnicgroup_recoded + combined$regionalgroup_recoded)/6

frq(combined$socialgroups)

mean(combined$socialgroups)
sd(combined$socialgroups)
## Recode generalized trust in the news 

combined$g_trust = as.numeric(combined$trust)
frq(combined$g_trust)

mean(combined$g_trust)
sd(combined$g_trust)
## Recode expectations of impartiality 

frq(combined$impartial1)
combined$impartial1_recoded = as.numeric(combined$impartial1)
combined$impartial1_recoded = recode(combined$impartial1_recoded, `1` = 1, `2` = -1, `998` = 0)

frq(combined$impartial2_recoded)
combined$impartial2_recoded = as.numeric(combined$impartial2)
combined$impartial2_recoded = recode(combined$impartial2_recoded, `2` = 1, `1` = -1, `998` = 0)

frq(combined$impartial3)
combined$impartial3_recoded = as.numeric(combined$impartial3)
combined$impartial3_recoded = recode(combined$impartial3_recoded, `1` = 1, `2` = -1, `998` = 0)

vars2 = c("impartial1_recoded", "impartial2_recoded", "impartial3_recoded")
df2 = combined[vars2]

print(alpha(df2))

## Raw alpha = 0.43. Not good enough to combine. 

## Recode Controls 

## Age, Gender, Income, Ideology, Interest in politics

combined$age = as.numeric(combined$age)

frq(combined$gender)
combined$gender = as.numeric(combined$gender)

frq(combined$income)
combined$income_recoded = as.numeric(combined$income)

frq(combined$ideology)

frq(combined$politics_interest)

class(combined$region)

combined$country = as.factor(combined$country)



levels(combined$country)

mean_socialgroup_by_country <- aggregate(socialgroups ~ country, combined, mean, na.rm = TRUE)

# Print the result
print(mean_socialgroup_by_country)

country_with_highest_mean <- mean_socialgroup_by_country$country[which.max(mean_socialgroup_by_country$socialgroup)]

# Print the result
print(country_with_highest_mean)

country_with_lowest_mean <- mean_socialgroup_by_country$country[which.min(mean_socialgroup_by_country$socialgroup)]

# Print the result
print(country_with_lowest_mean)

mean_socialgroup_by_country <- mean_socialgroup_by_country[order(mean_socialgroup_by_country$socialgroup), ]

# Print the result
print(mean_socialgroup_by_country)

mean_trust_by_country <- aggregate(g_trust ~ country, combined, mean, na.rm = TRUE)
mean_trust_by_country <- mean_trust_by_country[order(mean_trust_by_country$g_trust), ]

# Print the result
print(mean_trust_by_country)

