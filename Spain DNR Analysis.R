## Spain Case Study of Trust in Specifc News Outlets (comparison between trust majority Spanish and other news organizations)

spain = as.data.frame(Reuters_DNR_2021_Spain)

frq(spain$income_Spain)
spain$income_num = as.numeric(spain$income_Spain)

## Remove NA/No Answer

spain = spain %>%
  filter(income_num != 18) %>%
  filter(income_num != 19)

spain$country = "SPN"

spain$region_factor = as_factor(to_character(spain$region_Spain))
frq(spain$region_factor)

frq(spain$Q6_2018_trust_rb_1)

spain_needed = subset(spain, select = c("country", "gender_int", "age_int", "Q1b_NEW" , "Q1c" , "Q2_new2018" , "region_factor" ,
                                  "Q1F", "Financing1_2021" , "Financing2_2021", "Financing3_2021" ,
                                  "Div1_2021_1", "Div1_2021_2",  "Div1_2021_3", "Div1_2021_4" ,
                                  "Div1_2021_5", "Div1_2021_6", "income_num", "Q_IMPARTIAL1_2021",
                                  "Q_IMPARTIAL2_2021a", "Q_IMPARTIAL3_2021", "Div2_2021_1", "Div2_2021_2", "Div2_2021_3",
                                  "Div2_2021_4", 
                                  "Div2_2021_5", "Div2_2021_6", "Q6_2016_1", "Q6_2016_6", "Q6_2018_2", "Q6_2018_3",
                                  "Q6_2018_trust_rb_106" , "Q6_2018_trust_rb_107" , "Q6_2018_trust_rb_108" , "Q6_2018_trust_rb_108" ,
                                  "Q6_2018_trust_rb_111" , "Q6_2018_trust_rb_112" , "Q6_2018_trust_rb_113" , "Q6_2018_trust_rb_114" ,
                                  "Q6_2018_trust_rb_115" , "Q6_2018_trust_rb_116" ,"Q6_2018_trust_rb_117" ,"Q6_2018_trust_rb_118" ,
                                  "Q6_2018_trust_rb_119"))

names(spain_needed) = c("country", "gender", "age", "news_frq", "news_interest", "politics_interest", "region" ,
                  "ideology", "financing1", "financing2", "financing3" , 
                  "polgroup_frq", "agegroup_frq" , "gendergroup_frq", "classgroup_frq", "ethnicgroup_frq", "regiongroup_frq",
                  "income", "impartial1", "impartial2", "impartial3", "polgroup", "agegroup" , "gendergroup" , "classgroup", 
                  "ethnicgroup", "regionalgroup", "trust", "mynews_trust", "sm_news_trust", "se_news_trust",
                  "Antena3", "RTVE", "LaSexta", "Telecinco", "El Pais", "El Mundo", "ABC", "20 Minutos", "CadenaSPR", "COPE", 
                  "ElPeriodico", "LaVanguardia", "ElConfidencial")

## Recode values 

frq(spain_needed$socialgroups)
spain_needed$polgroup_recoded = as.numeric(spain_needed$polgroup)
spain_needed$polgroup_recoded = recode(spain_needed$polgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$agegroup_recoded = as.numeric(spain_needed$agegroup)
spain_needed$agegroup_recoded = recode(spain_needed$agegroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$gendergroup_recoded = as.numeric(spain_needed$gendergroup)
spain_needed$gendergroup_recoded = recode(spain_needed$gendergroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$classgroup_recoded = as.numeric(spain_needed$classgroup)
spain_needed$classgroup_recoded = recode(spain_needed$classgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$ethnicgroup_recoded = as.numeric(spain_needed$ethnicgroup)
spain_needed$ethnicgroup_recoded = recode(spain_needed$ethnicgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$regionalgroup_recoded = as.numeric(spain_needed$regionalgroup)
spain_needed$regionalgroup_recoded = recode(spain_needed$regionalgroup_recoded, `1` = 1, `2` = -1, `998` = 0)

spain_needed$socialgroups = (spain_needed$polgroup_recoded + spain_needed$agegroup_recoded + spain_needed$gendergroup_recoded + spain_needed$classgroup_recoded
                         + spain_needed$ethnicgroup_recoded + spain_needed$regionalgroup_recoded)/6


frq(spain_needed$Antena3)

spain_needed$age = as.numeric(spain_needed$age)

frq(spain_needed$gender)
spain_needed$gender = as.numeric(spain_needed$gender)

frq(spain_needed$income)
spain_needed$income_recoded = as.numeric(spain_needed$income)

frq(spain_needed$ideology)

spain_needed$ideology = as.numeric(spain_needed$ideology)

spain_needed = spain_needed %>%
  filter(ideology != 8) 

## Recode Regions -- majority Spanish speaking = 0, minority language states = 1

frq(spain_needed$region)

spain_needed$region = to_factor(spain_needed$region)
frq(spain_needed$region)
spain_needed$region = as.numeric(spain_needed$region)


spain_needed$region[spain_needed$region == 1 | spain_needed$region == 2 | spain_needed$region == 3 | spain_needed$region == 4 | spain_needed$region == 5 |
                      spain_needed$region == 6 | spain_needed$region == 7 | spain_needed$region == 10 | spain_needed$region == 13 | spain_needed$region == 16
             | spain_needed$region == 18 | spain_needed$region == 19] = 0

spain_needed$region[spain_needed$region == 8 | spain_needed$region == 9 | spain_needed$region == 11 | spain_needed$region == 12 | spain_needed$region == 14 |
               spain_needed$region == 15 | spain_needed$region == 17] = 1

frq(spain_needed$region)
spain_needed$region = as.factor(spain_needed$region)

frq(spain_needed$`El Mundo`)

## Regressions

summary(lm(socialgroups ~ age + gender + income + ideology + region, data = spain_needed))

summary(lm(Antena3 ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(RTVE ~ age + gender + income + ideology + polgroup_recoded + region, data = spain_needed))

summary(lm(LaSexta ~ age + gender + income + ideology + polgroup_recoded + region, data = spain_needed))

summary(lm(Telecinco ~ age + gender + income + ideology + polgroup_recoded + region, data = spain_needed))

summary(lm(`El Pais` ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(`El Mundo` ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(ABC ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(`20 Minutos` ~ age + gender + income + ideology + polgroup_recoded  + region, data = spain_needed))

summary(lm(CadenaSPR ~ age + gender + income + ideology + polgroup_recoded + region, data = spain_needed))

summary(lm(COPE ~ age + gender + income + ideology + polgroup_recoded + region, data = spain_needed))

summary(lm(ElPeriodico ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(LaVanguardia ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(ElConfidencial ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

### Regressions with social group as predictor

summary(lm(Antena3 ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

#summary(lm(RTVE ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

#summary(lm(LaSexta ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

#summary(lm(Telecinco ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(`El Pais` ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(`El Mundo` ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(ABC ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(`20 Minutos` ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(CadenaSPR ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(COPE ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(ElPeriodico ~ age + gender + income + ideology + socialgroups*region, data = spain_needed))

summary(lm(LaVanguardia ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

summary(lm(ElConfidencial ~ age + gender + income + ideology + polgroup_recoded*region, data = spain_needed))

### Regressions with region group as predictor

summary(lm(Antena3 ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

#summary(lm(RTVE ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

#summary(lm(LaSexta ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

#summary(lm(Telecinco ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(`El Pais` ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(`El Mundo` ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(ABC ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(`20 Minutos` ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(CadenaSPR ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(COPE ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(ElPeriodico ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(LaVanguardia ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))

summary(lm(ElConfidencial ~ age + gender + income + ideology + regionalgroup_recoded*region, data = spain_needed))
