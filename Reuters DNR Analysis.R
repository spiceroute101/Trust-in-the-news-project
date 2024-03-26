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

## Regressions

library(lmerTest)

trust2_sds <- combined %>%
  group_by(country) %>%
  mutate(socialgroups_sd = sd(socialgroups, na.rm = TRUE))

trust2_sds2 <- combined %>%
  group_by(country) %>%
  summarise(socialgroups_sd = sd(socialgroups, na.rm = TRUE)) %>%
  arrange(socialgroups_sd)

## Step 1 

results_ICC = lmer(g_trust ~ (1|country), data = combined)
summary(results_ICC)  

## Step 2 Random Intercept Fixed Slope 

results_RI = lmer(g_trust ~ gender + age + income + socialgroups  + socialgroups_sd + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1|country), data = trust2_sds)
summary(results_RI)

## Step 3 Random Intercept Random Slope 

results_RIRC = lmer(g_trust ~ gender + age + income + socialgroups + socialgroups_sd + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1 + socialgroups|country), data = trust2_sds)
summary(results_RIRC)

results_RIRC = lmer(g_trust ~ gender + age + income + socialgroups*socialgroups_sd + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1 + socialgroups|country), data = trust2_sds)
summary(results_RIRC)

results_RI2 = lmer(g_trust ~ gender + age + income + polgroup_recoded + ethnicgroup_recoded + gendergroup_recoded + agegroup_recoded + regionalgroup_recoded + classgroup_recoded + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1|country), data = trust2_sds)
summary(results_RI2)

results_RIRC2 = lmer(g_trust ~ gender + age + income + polgroup_recoded + ethnicgroup_recoded + gendergroup_recoded + agegroup_recoded + regionalgroup_recoded + classgroup_recoded + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1 + socialgroups|country), data = trust2_sds)
summary(results_RIRC2)

results_RI2 = lmer(g_trust ~ gender + age + income + polgroup_recoded + ethnicgroup_recoded + gendergroup_recoded + agegroup_recoded + regionalgroup_recoded + classgroup_recoded + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1|country), data = trust2_sds)
summary(results_RI2)

results_RIRC2 = lmer(g_trust ~ gender + age + income + polgroup_recoded + ethnicgroup_recoded + gendergroup_recoded + agegroup_recoded + regionalgroup_recoded + classgroup_recoded + impartial1_recoded + impartial2_recoded + impartial3_recoded + (1 + socialgroups|country), data = combined)
summary(results_RIRC2)

## Visualizing 

re <- ranef(results_RIRC)$socialgroups_sd

newdata <- expand.grid(
  socialgroups = seq(min(trust2_sds$socialgroups), max(trust2_sds$socialgroups), length.out = 100),
  socialgroups_sd = c(min(trust2_sds$socialgroups_sd), mean(trust2_sds$socialgroups_sd), max(trust2_sds$socialgroups_sd)),
  gender = mean(trust2_sds$gender),
  age = mean(trust2_sds$age),
  income = mean(trust2_sds$income),
  impartial1_recoded = mean(trust2_sds$impartial1_recoded),
  impartial2_recoded = mean(trust2_sds$impartial2_recoded),
  impartial3_recoded = mean(trust2_sds$impartial3_recoded),
  country = unique(trust2_sds$country)
)

newdata <- expand.grid(
  socialgroups = seq(min(trust2_sds$socialgroups), max(trust2_sds$socialgroups), length.out = 100),
  socialgroups_sd = unique(trust2_sds$socialgroups_sd),
  gender = mean(trust2_sds$gender),
  age = mean(trust2_sds$age),
  income = mean(trust2_sds$income),
  impartial1_recoded = mean(trust2_sds$impartial1_recoded),
  impartial2_recoded = mean(trust2_sds$impartial2_recoded),
  impartial3_recoded = mean(trust2_sds$impartial3_recoded),
  country = unique(trust2_sds$country)
)

# add the predictions to the new data frame
newdata$pred <- predict(results_RIRC, newdata = newdata, re.form = ~ (1 + socialgroups|country))

# create the plot
ggplot(newdata, aes(x = socialgroups, y = pred, color = socialgroups_sd)) +
  geom_line() +
  scale_color_gradientn(colors = c("lightblue", "darkblue")) +
  labs(x = "Social Groups", y = "Predicted Trust", color = "Social Groups SD") +
  theme_minimal()
######

