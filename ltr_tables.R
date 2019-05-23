library(tidyverse)
library(knitr)
library(janitor)
library(kableExtra)


number.of.tags <- all.bs.67.converted.com %>%
  select(object, utterance_type, object_present, speaker, comprehensive.codes, tier, lena.comprehensive.codes) %>%
  gather(value, type, -object, -utterance_type, -object_present, -speaker, -tier) %>%
  tabyl(type, value) %>%
  adorn_totals("row") %>%
  rename("Speaker type" = 'type', 
         "Human codes"= 'comprehensive.codes',
         "LENA codes" = 'lena.comprehensive.codes')

#Table for other errors (Noise/silence)
other.errors <- all.bs.67.other.errors %>%
  select(comprehensive.codes, lena.comprehensive.codes) %>%
  tabyl(comprehensive.codes, lena.comprehensive.codes) %>%
  adorn_totals("row") %>%
  select(Type = comprehensive.codes, 
         Noise = Noise, 
         Silence = Silence)

#Accuracy by utterance type table
utterance.type.table <- accuracy.calculation %>%
  select(accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy) %>%
  mutate(prop.cor = `1`/(`0`+`1`)) %>%
  select(UtteranceType = utterance_type,
         Incorr = `0`,
         Corr = `1`,
         '%Corr' = prop.cor) 

utterance.type.table.speaker <- accuracy.calculation %>%
  select(accuracy, utterance_type, comprehensive.codes) %>%
  group_by(comprehensive.codes, accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy, comprehensive.codes)

#accuracy by utterance type child adult comparison
utterance.type.table.ac.join <- accuracy.calculation.ac %>%
  select(accuracy, utterance_type) %>%
  group_by(utterance_type, accuracy) %>%
  summarize(count = n()) %>%
  mutate(comparison = "ac")

utterance.type.table.mf.join <- accuracy.calculation.mf %>%
  select(accuracy, utterance_type) %>%
  group_by(utterance_type, accuracy) %>%
  summarize(count = n()) %>%
  mutate(comparison = "mf")


utterance.type.table.ac <- accuracy.calculation.ac %>%
  select(accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy) %>%
  mutate(prop.cor = `1`/(`0`+`1`)) %>%
  select(UtteranceType = utterance_type,
         Incorrect = `0`,
         Correct = `1`,
         ProportionCorrect = prop.cor) 

#accuracy by utterance type male female comparison
utterance.type.table.mf <- accuracy.calculation.mf %>%
  select(accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy) %>%
  mutate(prop.cor = `1`/(`0`+`1`)) %>%
  select(UtteranceType = utterance_type,
         Incorrect = `0`,
         Correct = `1`,
         ProportionCorrect = prop.cor)

#accuracy by utterance type target other child comparison
utterance.type.table.tn <- accuracy.calculation.tn %>%
  select(accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy) %>%
  mutate(prop.cor = `1`/(`0`+`1`)) %>%
  select(UtteranceType = utterance_type,
         Incorrect = `0`,
         Correct = `1`,
         ProportionCorrect = prop.cor)
  
#accuracy by utterance type overlap electronic comparison
utterance.type.table.on <- accuracy.calculation.on %>%
  select(accuracy, utterance_type) %>%
  tabyl(utterance_type, accuracy) %>%
  mutate(prop.cor = `1`/(`0`+`1`)) %>%
  select(UtteranceType = utterance_type,
         Incorrect = `0`,
         Correct = `1`,
         ProportionCorrect = prop.cor)

utterance.type.table.all <- left_join(utterance.type.table.ac, utterance.type.table.mf, by = "UtteranceType") %>%left_join(utterance.type.table.tn,by = "UtteranceType") %>%
  left_join(utterance.type.table.on, by = "UtteranceType") %>%
  select(UtteranceType = UtteranceType,
         Incorr = Incorrect.x,
         Corr = Correct.x,
         '%Corr' = ProportionCorrect.x,
         Incorr = Incorrect.y,
         Corr = Correct.y,
         '%Corr' = ProportionCorrect.y,
         Incorr = Incorrect.x.x,
         Corr = Correct.x.x,
         '%Corr' = ProportionCorrect.x.x,
         Incorr = Incorrect.y.y,
         Corr = Correct.y.y,
         '%Corr' = ProportionCorrect.y.y)


