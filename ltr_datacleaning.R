library(tidyverse)
library(broom)
library(caret)
library(magrittr)

#mode function
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

#Converted codes by hand for 6 and 7 month audio data points
#Read the file with converted speaker codes back in. Rename column name that have spaces in them

all.bs.67.converted <- read_csv("Data/all_basic_level_ransubjs.csv",
                                col_types = cols(X1 = col_skip(),
                                                 ordinal = col_integer(),
                                                Adult.Child = col_character(),
                                                Male.Female = col_character(),
                                                Target.Other.child = col_character()))


all.bs.67.converted <- all.bs.67.converted %>% 
  rename(
    comprehensive.codes = 'Comprehensive codes',
    adult.child = "Adult.Child",
    male.female = "Male.Female", 
    target.other = "Target.Other.child", 
    overlap.electronic = "Overlap.Noise")

#Convert LENA tier codes to match the speaker codes done by hand. Columns are the same as the ones made by hand, but with 'lena' in front of them. Then subset to include only relevant data points, and make values factors. 

#Comprehensive codes for near LENA codes only

all.bs.67.prefilter <- all.bs.67.converted %>% 
  mutate(tier= factor(tier, levels = c("*CHN", "*CXN", "*FAN", "*MAN", "*OLN", "*TVN", "missing"))) %>%
  mutate(lena.comprehensive.codes =
           fct_recode(tier, 
                      "Child"="*CHN", 
                      "Child"="*CXN", 
                      "Adult"="*FAN", 
                      "Adult"="*MAN", 
                      "NULL" = "missing",
                      "Overlap" = "*OLN", 
                      "Electronic" = "*TVN"))

all.bs.67.converted.com <- all.bs.67.prefilter %>%
  filter(comprehensive.codes %in% c("Adult", "Child", "Electronic", "Overlap") &
           lena.comprehensive.codes %in% c("Adult", "Child", "Electronic", "Overlap")) %>%
  mutate(comprehensive.codes=factor(comprehensive.codes),
         lena.comprehensive.codes = factor(lena.comprehensive.codes)) %>%
  mutate(lena.comprehensive.codes = 
           fct_relevel(lena.comprehensive.codes, "Adult", "Child", "Electronic", "Overlap"))

#Adult and child codes
all.bs.67.converted.ac <- all.bs.67.converted %>%
  mutate(tier= factor(tier, levels = c("*CHN", "*CXN", "*FAN", "*MAN", "missing"))) %>%
  mutate(lena.adult.child=
           fct_recode(tier, 
                      "Child"="*CHN", 
                      "Child"="*CXN", 
                      "Adult"="*FAN", 
                      "Adult"="*MAN", 
                      "NULL"="missing")) %>%
  filter(adult.child %in% c("Adult", "Child") &
           lena.adult.child %in% c("Adult", "Child")) %>%
  mutate(adult.child=factor(adult.child),
         lena.adult.child = factor(lena.adult.child)) %>%
  mutate(lena.adult.child=
           fct_relevel(lena.adult.child, "Adult", "Child"))

#Male and female codes
all.bs.67.converted.mf <- all.bs.67.converted %>%
  mutate(tier= factor(tier, levels = c("*FAN", "*MAN", "missing"))) %>%
  mutate(lena.male.female=
           fct_recode(tier, 
                      "NULL"="missing", 
                      "Female"="*FAN", 
                      "Male"="*MAN")) %>%
  filter(male.female %in% c("Male", "Female") &
           lena.male.female %in% c("Male", "Female")) %>%
  mutate(male.female = factor(male.female), 
         lena.male.female=factor(lena.male.female))

#target child and other child codes
all.bs.67.converted.tn <- all.bs.67.converted %>%
  mutate(tier= factor(tier, levels = c("*CHN", "*CXN","missing"))) %>%
  mutate(lena.target.other =
           fct_recode(tier, 
                      "Target"="*CHN", 
                      "Other"="*CXN", 
                      "NULL"="missing")) %>%
  filter(target.other %in% c("Target", "Other") &
           lena.target.other %in% c("Target", "Other")) %>%
  mutate(target.other=factor(target.other, levels = c("Other", "Target")), 
         lena.target.other=factor(lena.target.other))

#Need to add 'Target' as a level for this factor even though it never occurred, or the confusion matrix won't work because the factor levels are not the same (in factor function above)

#electronic and overlap codes
all.bs.67.converted.on <-all.bs.67.converted %>%
  mutate(tier= factor(tier, levels = c("*OLN", "*TVN", "*SIL", "missing"))) %>%
  mutate(lena.overlap.electronic =
           fct_recode(tier, 
                      "NULL"="missing", 
                      "Overlap"="*OLN", 
                      "Exclude"="*SIL", 
                      "Electronic"="*TVN")) %>%           
  filter(overlap.electronic %in% c("Overlap", "Electronic") &
           lena.overlap.electronic %in% c("Overlap", "Electronic")) %>%
  mutate(overlap.electronic =factor(overlap.electronic), 
         lena.overlap.electronic = factor(lena.overlap.electronic)) %>%
  mutate(lena.overlap.electronic=
           fct_relevel(lena.overlap.electronic, "Electronic", "Overlap"))

#convert comprehensive codes to numbers for overall correlation
all.bs.67.converted.com <- all.bs.67.converted.com %>% 
  mutate(lena.comprehensive.codes.num = 
           fct_recode(lena.comprehensive.codes, 
                      "1" = "Adult" , 
                      "2" = "Child", 
                      "3" = "Electronic", 
                      "4" = "Overlap")) %>%
  mutate(comprehensive.codes.num = 
           fct_recode(comprehensive.codes, 
                      "1" = "Adult" , 
                      "2" = "Child", 
                      "3" = "Electronic", 
                      "4" = "Overlap"))  %>%
  mutate(lena.comprehensive.codes.num = as.numeric(as.character(lena.comprehensive.codes.num))) %>%
  mutate(comprehensive.codes.num = as.numeric(as.character(comprehensive.codes.num)))

#number of tags per child
tags.per.child <- all.bs.67.converted.com %>%
  group_by(subj) %>%
  tally

#tags values
tags.values <- tags.per.child %>%
  summarize(mean = mean(n), min = min(n), sd = sd(n), max = max(n), total = sum(n))

#comprehensive confusion matrix
conf_mat_comp <- confusionMatrix(all.bs.67.converted.com$lena.comprehensive.codes, all.bs.67.converted.com$comprehensive.codes, dnn = c("Lena", "Human"))
conf_mat_comp
conf_mat_comp$table
conf_mat_comp$overall

comp.matrix <-tidy(conf_mat_comp, by_class = TRUE)%>%
  spread(term, estimate) %>%
  select(class, sensitivity, precision, f1) %>%
  slice(1:4) %>%
  mutate('LTR Report Sensitivity' = c(.82, .76, .71, .76)) %>%
  select(Type = class,
         Recall = sensitivity, 
         Precision = precision,
         F1 = f1, 
         'LTR Report' = 'LTR Report Sensitivity')

#comprehensive plot prep
human_as_gold <- conf_mat_comp$table %>% 
  tidy() %>% 
  spread(Lena, n) %>% 
  mutate(human_total = Adult+Child+Electronic+Overlap) %>% 
  gather(lena_tags, Adult:Overlap, value =n) %>% 
  arrange(Human) %>%
  mutate(prop_cat = n/human_total)

#correlation of accuracy proportions 
lena = c(82, 2, 4, 12, 7, 76, 0, 17, 8, 0, 71, 21, 14, 4, 6, 76)
human = round(human_as_gold$prop_cat*100, 0)
percentages = data.frame(lena, human) %>%
  mutate(human_minus_lena = human - lena) %>% 
  mutate(abs_human_minus_lena = abs(human_minus_lena))


#adult child matrix
adult.child <- confusionMatrix(all.bs.67.converted.ac$lena.adult.child, all.bs.67.converted.ac$adult.child, dnn = c("Lena", "Human"))
adult.child$overall
adult.child.matrix <-tidy(adult.child, by_class = TRUE)%>%
  spread(term, estimate) %>%
  select(class, sensitivity, precision, f1) %>%
  slice(1)

#adult child graph prep
adult.child.graph <- adult.child$table %>% 
  tidy() %>% 
  spread(Lena, n) %>% 
  mutate(human_total = Adult+Child) %>% 
  gather(lena_tags, Adult:Child, value =n) %>% 
  arrange(Human) %>%
  mutate(prop_cat = n/human_total)

#male female matrix
male.female <- confusionMatrix(all.bs.67.converted.mf$lena.male.female, all.bs.67.converted.mf$male.female, dnn=c("Lena", "Human"))
male.female$overall
male.female.matrix <-tidy(male.female, by_class = TRUE)%>%
  spread(term, estimate) %>%
  select(class, sensitivity, precision, f1) %>%
  slice(1)

#male female graph prep
male.female.graph <- male.female$table %>% 
  tidy() %>% 
  spread(Lena, n) %>% 
  mutate(human_total = Male+Female) %>% 
  gather(lena_tags, Male:Female, value =n) %>% 
  arrange(Human) %>%
  mutate(prop_cat = n/human_total)

#target other confusion matrix
#Kappa not produced since it's not a true matrix
target.other <- confusionMatrix(all.bs.67.converted.tn$lena.target.other, all.bs.67.converted.tn$target.other,  dnn=c("Lena", "Human"))
target.other$overall
target.other.matrix <-tidy(target.other, by_class = TRUE)%>%
  spread(term, estimate) %>%
  select(class, sensitivity, precision, f1) %>%
  slice(1)

#target other graph prep
target.other.graph <- target.other$table %>% 
  tidy() %>% 
  spread(Lena, n) %>% 
  mutate(human_total = Target+ Other) %>% 
  gather(lena_tags, Target:Other, value =n) %>% 
  arrange(Human) %>%
  mutate(prop_cat = n/human_total) %>%
  filter(Human == "Other")

#electronic overlap confusion matrix
electronic.overlap <- confusionMatrix(all.bs.67.converted.on$lena.overlap.electronic, all.bs.67.converted.on$overlap.electronic, dnn=c("Lena", "Human"))
electronic.overlap$overall
electronic.overlap.matrix <-tidy(electronic.overlap, by_class = TRUE)%>%
  spread(term, estimate) %>%
  select(class, sensitivity, precision, f1) %>%
  slice(1)

#electronic other graph prep
electronic.overlap.graph <- electronic.overlap$table %>% 
  tidy() %>% 
  spread(Lena, n) %>% 
  mutate(human_total = Electronic + Overlap) %>% 
  gather(lena_tags, Electronic:Overlap, value =n) %>% 
  arrange(Human) %>%
  mutate(prop_cat = n/human_total)

#other errors - noise and silence LENA tags
all.bs.67.other.errors <- all.bs.67.converted %>% 
  mutate(tier= factor(tier, levels = c("*NON", "*SIL", "missing"))) %>%
  mutate(lena.comprehensive.codes = 
           fct_recode(tier, 
                      "Noise" = "*NON", 
                      "Silence" = "*SIL", 
                      "NULL" = "missing")) %>%
  filter(lena.comprehensive.codes %in% c("Noise", "Silence")) 

#considering utterance type in mistakes
accuracy.calculation <- all.bs.67.converted.com %>%
  mutate(accuracy = ifelse(lena.comprehensive.codes==comprehensive.codes, 1,0)) %>% 
  select(object, utterance_type, object_present, speaker, comprehensive.codes, lena.comprehensive.codes, accuracy, subj) %>%
  filter(utterance_type != "u") %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s")) %>%
  mutate(utterance_type = factor(utterance_type))

options(contrasts=c("contr.sum","contr.poly"))
contrasts(accuracy.calculation$utterance_type)

utterance.type <- glm(accuracy ~ utterance_type, data = accuracy.calculation, family = binomial(link="logit"))

utterance.type.glm <- anova(utterance.type, test = "Chisq") %>% tidy()

utterance.type.subj <- accuracy.calculation %>%
  group_by(subj, utterance_type) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

#utterance type adult child
accuracy.calculation.ac <- all.bs.67.converted.ac %>%
  mutate(accuracy = ifelse(lena.adult.child == adult.child, 1,0)) %>%
  select(object, utterance_type, object_present, speaker, accuracy, subj) %>%
  filter(utterance_type != "u") %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s")) %>%
  mutate(utterance_type = factor(utterance_type)) %>%
  mutate(comparison = "AdultvsChild")

utterance.type.ac <- glm(accuracy ~ utterance_type, data = accuracy.calculation.ac, family = binomial(link="logit"))

utterance.type.ac.glm <- anova(utterance.type.ac, test = "Chisq") %>% tidy()

utterance.type.ac.subj <- accuracy.calculation.ac %>%
  group_by(subj, utterance_type) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

#utterance type male female 
accuracy.calculation.mf <- all.bs.67.converted.mf %>%
  mutate(accuracy = ifelse(lena.male.female==male.female, 1,0)) %>%
  select(object, utterance_type, object_present, speaker, accuracy, subj) %>%
  filter(utterance_type != "u") %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s")) %>%
  mutate(utterance_type = factor(utterance_type)) %>%
  mutate(comparison = "MalevsFemale")

utterance.type.mf <- glm(accuracy ~ utterance_type, data = accuracy.calculation.mf, family = binomial(link = "logit"))

utterance.type.mf.glm <- anova(utterance.type.mf, test = "Chisq") %>% tidy()

utterance.type.mf.subj <- accuracy.calculation.mf %>%
  group_by(subj, utterance_type) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

#utterance type target other child
accuracy.calculation.tn <- all.bs.67.converted.tn %>%
  mutate(accuracy = ifelse(lena.target.other==target.other, 1,0)) %>%
  select(object, utterance_type, object_present, speaker,  accuracy, subj) %>%
  filter(utterance_type != "u") %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s")) %>%
  mutate(utterance_type = factor(utterance_type)) %>%
  mutate(comparison = "TargetvsOtherChild")

utterance.type.tn <- glm(accuracy ~ utterance_type, data = accuracy.calculation.tn, family = binomial(link = "logit"))

utterance.type.tn.glm <- anova(utterance.type.tn, test = "Chisq") %>% tidy()

utterance.type.tn.subj <- accuracy.calculation.tn %>%
  group_by(subj, utterance_type) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

#utterance type electronic overlap 
accuracy.calculation.on <- all.bs.67.converted.on %>%
  mutate(accuracy = ifelse(lena.overlap.electronic==overlap.electronic, 1,0)) %>%
  select(object, utterance_type, object_present, speaker, accuracy, subj) %>%
  filter(utterance_type != "u") %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s")) %>%
  mutate(utterance_type = factor(utterance_type))%>%
  mutate(comparison = "ElectronicvsOverlap")

accuracy.calculation.all <- rbind(accuracy.calculation.on,accuracy.calculation.tn, accuracy.calculation.mf, accuracy.calculation.ac)

utterance.type.on <- glm(accuracy ~ utterance_type, data = accuracy.calculation.on, family = binomial(link = "logit"))

utterance.type.on.glm <- anova(utterance.type.on, test = "Chisq") %>% tidy()

utterance.type.on.subj <- accuracy.calculation.on %>%
  group_by(subj, utterance_type) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

#count number of speakers by participant
count_speakers <- all.bs.67.prefilter %>% 
  group_by(subj) %>%
  summarize(n = n_distinct(speaker))
