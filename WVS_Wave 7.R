wvs_evs = as.data.frame(ZA7505_v3_0_0)

install.packages("brms")

library(brms)         
library(tidybayes)    # Manipulate Stan objects in a tidy way
library(broom)        # Convert model objects to data frames
library(broom.mixed) 

frq(wvs_evs$reg_nuts2)

frq(wvs_evs$country_factor)

## Recode variables 

## Recode country

wvs_evs$country_factor = as.factor(wvs_evs$cntry_AN)

## Recode strength of national identification

frq(wvs_evs$G006)
wvs_evs$national_identity = as.numeric(wvs_evs$G006)
frq(wvs_evs$national_identity)

wvs_evs$national_identity = recode(wvs_evs$national_identity, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

frq(wvs_evs$G256)
wvs_evs$national_identity2 = as.numeric(wvs_evs$G256)
frq(wvs_evs$national_identity2)

wvs_evs$national_identity2 = recode(wvs_evs$national_identity2, `1` = 4, `2` = 3, `3` = 2, `4` = 1)


## Recode trust in the press

wvs_evs$presstrust = as.numeric(wvs_evs$E069_04)
frq(wvs_evs$presstrust)


wvs_evs$parliamenttrust = as.numeric(wvs_evs$E069_07)
wvs_evs$governmenttrust = as.numeric(wvs_evs$E069_11)
wvs_evs$partytrust = as.numeric(wvs_evs$E069_12)

wvs_evs$presstrust = recode(wvs_evs$presstrust, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

wvs_evs$parliamenttrust = recode(wvs_evs$parliamenttrust, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

wvs_evs$governmenttrust = recode(wvs_evs$governmenttrust, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

wvs_evs$partytrust = recode(wvs_evs$partytrust, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

wvs_evs$politicaltrust = (wvs_evs$parliamenttrust + wvs_evs$governmenttrust + wvs_evs$partytrust)/3

## Gender

frq(wvs_evs$X001)
wvs_evs$gender = as.numeric(wvs_evs$X001)

wvs_evs$female = recode(wvs_evs$gender, `1` = 0, `2` = 1)
frq(wvs_evs$female)

## Age

frq(wvs_evs$X003R)
wvs_evs$age = as.numeric(wvs_evs$X003R)


## Education 

frq(wvs_evs$X025R)
wvs_evs$education = as.numeric(wvs_evs$X025R)

## Interest in politics

frq(wvs_evs$E023)

wvs_evs$interest_in_politics = as.numeric(wvs_evs$E023)
wvs_evs$interest_in_politics = recode(wvs_evs$interest_in_politics, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

frq(wvs_evs$interest_in_politics)

## Ideology

frq(wvs_evs$E033)
wvs_evs$ideology = as.numeric(wvs_evs$E033)

class(wvs_evs$E033)

results_ICC = lmer(presstrust ~ (1|country_factor), data = wvs_evs)
summary(results_ICC)  

results_RI = lmer(presstrust ~ female + age + education + national_identity + interest_in_politics + ideology + (1|country_factor), data = wvs_evs)
summary(results_RI)


results_RIRC = lmer(presstrust ~ female + age + education + politicaltrust + national_identity + interest_in_politics + ideology + (1 + national_identity|country_factor), data = wvs_evs)
summary(results_RIRC)

results_RIRC = lmer(presstrust ~ female + age + education + national_identity2 + interest_in_politics + ideology + (1 + national_identity2|country_factor), data = wvs_evs)
summary(results_RIRC)

results_RIRC = lmer(presstrust ~ female + age + education + politicaltrust + national_identity*national_id_sd + interest_in_politics + ideology + (1 + national_identity|country_factor), data = trust_sds)
summary(results_RIRC)

wvs_evs$nat_identity = plyr::mapvalues(wvs_evs$national_identity, c(1,2,3,4), c(4,3,2,1))
frq(wvs_evs$nat_identity)

## Convert Nat ID into country level factor indicating deviations at regional level

region_sd <- wvs_evs %>%
  group_by(country_factor, region) %>%
  summarise(region_trust_sd = sd(trust, na.rm = TRUE))

# Calculate the mean of the regional standard deviations for each country
country_score <- region_sd %>%
  group_by(country) %>%
  summarise(country_score = mean(region_trust_sd, na.rm = TRUE))

# Merge this data back into your original dataframe
wvs <- wvs %>%
  left_join(country_score, by = "country")

## Convert Nat ID into country level factor indicating SD 

trust_sds <- wvs_evs %>%
  group_by(country_factor) %>%
  mutate(press_trust_sd = sd(presstrust, na.rm = TRUE))

trust_sds <- wvs_evs %>%
  group_by(country_factor) %>%
  mutate(national_id_sd = sd(national_identity, na.rm = TRUE))


# Merge this data back into your original dataframe
wvs_evs <- merge(wvs_evs, trust_sds[, c("country_factor", "press_trust_sd")], by = "country_factor")

## WVS Wave 6 Data 

## Recode variables

wvs_w6 = as.data.frame(WV6_Data_sav_v20201117)

## Recode variables 

## Recode country

frq(wvs_w6$B_COUNTRY_ALPHA)
wvs_w6$B_COUNTRY_ALPHA[wvs_w6$B_COUNTRY_ALPHA == "USA"] = "AAUSA"

wvs_w6$country_factor = as.factor(wvs_w6$B_COUNTRY_ALPHA)
frq(wvs_w6$country_factor)

## Recode strength of national identification

frq(wvs_w6$V214)
wvs_w6$national_identity = as.numeric(wvs_w6$V214)
frq(wvs_w6$national_identity)

wvs_w6$national_identity = recode(wvs_w6$national_identity, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

frq(wvs_w6$G256)
wvs_w6$national_identity2 = as.numeric(wvs_w6$G256)
frq(wvs_w6$national_identity2)

wvs_w6$national_identity2 = recode(wvs_w6$national_identity2, `1` = 4, `2` = 3, `3` = 2, `4` = 1)


## Recode trust in the press

wvs_w6$presstrust = as.numeric(wvs_w6$V110)
frq(wvs_w6$presstrust)

wvs_w6$presstrust = recode(wvs_w6$presstrust, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

## Gender

frq(wvs_w6$V240)
wvs_w6$gender = as.numeric(wvs_w6$V240)

wvs_w6$female = recode(wvs_w6$gender, `1` = 0, `2` = 1)
frq(wvs_w6$female)

## Age

frq(wvs_w6$V242)
wvs_w6$age = as.numeric(wvs_w6$V242)


## Education 

frq(wvs_w6$V248)
wvs_w6$education = as.numeric(wvs_w6$V248)

## Interest in politics

frq(wvs_w6$V84)

wvs_w6$interest_in_politics = as.numeric(wvs_w6$V84)
wvs_w6$interest_in_politics = recode(wvs_w6$interest_in_politics, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

frq(wvs_w6$interest_in_politics)

## Ideology

frq(wvs_w6$V95)
wvs_w6$ideology = as.numeric(wvs_w6$V95)

## Media Frequency 

frq(wvs_w6$V217)
wvs_w6$newspaper = as.numeric(wvs_w6$V217)
wvs_w6$newspaper = recode(wvs_w6$newspaper, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
frq(wvs_w6$newspaper)

frq(wvs_w6$V219)
wvs_w6$tv = as.numeric(wvs_w6$V219)
wvs_w6$tv = recode(wvs_w6$tv, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
frq(wvs_w6$tv)

frq(wvs_w6$V221)
wvs_w6$mobile = as.numeric(wvs_w6$V221)
wvs_w6$mobile = recode(wvs_w6$mobile, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
frq(wvs_w6$mobile)

frq(wvs_w6$V223)
wvs_w6$internet = as.numeric(wvs_w6$V223)
wvs_w6$internet = recode(wvs_w6$internet, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
frq(wvs_w6$internet)


## Multilevel Model

results_ICC = lmer(presstrust ~ (1|country_factor), data = wvs_w6)
summary(results_ICC)  

results_RI = lmer(presstrust ~ female + age + education + newspaper + tv + mobile + internet + national_identity + interest_in_politics + ideology + (1|country_factor), data = wvs_w6)
summary(results_RI)

results_RIRC = lmer(presstrust ~ female + age + education + newspaper + tv + mobile + internet + national_identity + interest_in_politics + ideology + (1 + national_identity|country_factor), data = wvs_w6)
summary(results_RIRC)

results_RIRC = lmer(presstrust ~ female + age + education + tv*national_identity + newspaper + mobile + internet + interest_in_politics + ideology + (1 + national_identity|country_factor), data = wvs_w6)
summary(results_RIRC)

random_effects <- ranef(results_RIRC)

