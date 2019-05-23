library(tidyverse)
library(irr)

summarized_wordcount <- read_csv("Data/its_randsubjs.csv") %>%
  select(subj, month, AWC, FAN_Word_Count, MAN_Word_Count) %>%
  group_by(subj) %>%
  summarize(AWC = sum(AWC),
            MAN_Word_Count = sum(MAN_Word_Count),
            FAN_Word_Count = sum(FAN_Word_Count)) %>%
  mutate(prop_FAN = FAN_Word_Count/AWC)

summarized_all_bl <- all.bs.67.prefilter %>%
  select(subj, month, object, male.female) %>%
  group_by(subj, male.female) %>%
  filter(male.female == "Female" | male.female == "Male") %>%
  summarize(Noun_count = n()) %>%
  spread(male.female, Noun_count) %>%
  mutate(noun_prop_FAN = Female/(Female + Male)) %>%
  mutate(total_noun = Female + Male)

summarized_wordcount_nouns <- merge(summarized_wordcount, summarized_all_bl, by = "subj") %>% as_tibble() %>%
  mutate(prop_noun = total_noun/AWC)

mean(summarized_wordcount_nouns$prop_noun)

ut_type <- all.bs.67.prefilter %>%
  select(object, utterance_type) %>%
  group_by(utterance_type) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  mutate(Soderstrom = c(0.33, .14, .11, .305, NA, NA, NA)) %>%
  filter(utterance_type != "u")

summarized_ut <- all.bs.67.prefilter %>%
  select(male.female, object, utterance_type) %>%
  group_by(male.female, utterance_type) %>%
  summarize(count = n()) %>%
  filter(male.female == "Female" | male.female == "Male") %>%
  spread(male.female, count) %>%
  mutate(total = Female + Male) %>%
  mutate(prop.female = Female/total, 
         prop.male = Male/total) %>%
  filter(utterance_type != "u")

ut_comparison_table <- merge(ut_type, summarized_ut, by = "utterance_type") %>%
  select(utterance_type = utterance_type,
         Count = count,
         Proportion = prop,
         "Soderstrom et al., (2008)" = Soderstrom,
         Female = Female,
         Male = Male,
         Prop.Female = prop.female,
         Prop.Male = prop.male) %>%
  mutate(utterance_type =
           fct_recode(utterance_type, 
                      "Declarative" = "d", 
                      "Imperative" = "i",
                      "Reading" = "r", 
                      "Short Phrase" = "n", 
                      "Question" = "q",
                      "Singing" = "s"))
  
cor.test(summarized_ut$Female, summarized_ut$Male)

count_unique_speakers <- all.bs.67.prefilter %>%
  select(subj, speaker, tier, object) %>%
  mutate(speaker = factor(speaker),
         tier = factor(tier)) %>%
  group_by(subj) %>%
  summarize(n_distinct(speaker),
            n_distinct(tier)) %>%
  select(subj,
         n_speaker = 'n_distinct(speaker)',
         n_tier = 'n_distinct(tier)')

range(count_unique_speakers$n_speaker)
#can't correlate because n_tier has no SD
cor.test(count_unique_speakers$n_speaker, count_unique_speakers$n_tier)


#speaker-tags reliability
reliability <- read_csv("Data/reliability_randsubjs.csv",
                        col_types = cols(FemaleAdult = col_character(),
                                         Child = col_character(),
                                         Overlap = col_character(),
                                         FirstPass = col_character()))


reliability_cleaned <- reliability %>%
  filter(FirstPass == "x") %>%
  mutate(FAN =
           fct_recode(FemaleAdult, 
                      "FAN"="x"),
         MAN = fct_recode(MaleAdult,
                          "MAN" = "x"),
         CXN = fct_recode(Child, 
                          "CXN" = "x"),
         TVN = fct_recode(Electronic,
                          "TVN" = "x"),
         OLN = fct_recode(Overlap,
                          "OLN" = "x"))%>%
  mutate(FAN = as.character(FAN),
         MAN = as.character(MAN),
         CXN = as.character(CXN),
         TVN = as.character(TVN),
         OLN = as.character(OLN),
         subj = as.numeric(subj),
         month = as.numeric(month)) %>%
  replace_na(list(FAN = "", MAN = "", CXN = "", TVN = "", OLN = "")) %>%
  unite(recode_speaker, FAN, MAN, CXN, TVN, OLN, remove = FALSE, sep = "") %>%
  select(-FemaleAdult, -MaleAdult, -Child, -Electronic, -Overlap,  -FirstPass, -onset_500, -offset_500, -FAN, -MAN, -CXN, -TVN, -OLN)
  
all.bs.67.reliability <- all.bs.67.converted %>% 
  mutate(tier= factor(tier, levels = c("*CHN", "*CXN", "*FAN", "*MAN", "*OLN", "*TVN", "missing"))) %>%
  mutate(lena.all.codes =
           fct_recode(tier, 
                      "CHN"="*CHN", 
                      "CXN"="*CXN", 
                      "Female"="*FAN", 
                      "Male"="*MAN", 
                      "NULL" = "missing",
                      "Overlap" = "*OLN", 
                      "Electronic" = "*TVN")) %>%
  mutate(human.all.codes = case_when(comprehensive.codes == "Adult" ~ paste(male.female),
                                     comprehensive.codes == "Child" ~ paste(target.other),
                                     comprehensive.codes == "Electronic" ~ paste(overlap.electronic),
                                     comprehensive.codes == "Overlap" ~ paste(overlap.electronic))) %>%
  mutate(human.all.codes = 
           fct_recode(human.all.codes, 
                      "TVN" = "Electronic",
                      "OLN" = "Overlap",
                      "FAN" = "Female",
                      "MAN" = "Male",
                      "CXN" = "Other")) %>%
  select(-adult.child, -male.female, -target.other,  -overlap.electronic, -tier, -lena.all.codes, -utterance_type, -object_present, -basic_level, -comprehensive.codes)

basic_level_reliability_combined <- merge(all.bs.67.reliability, reliability_cleaned, by = c("subj", "month", "onset","offset","object")) %>%
  as_tibble() %>%
  mutate(recode_speaker = as.character(recode_speaker),
         human.all.codes = as.character(human.all.codes)) %>%
  unique()

levels(basic_level_reliability_combined$human.all.codes)
levels(basic_level_reliability_combined$recode_speaker)
speaker_rel <-kappa2(cbind(basic_level_reliability_combined$human.all.codes, basic_level_reliability_combined$recode_speaker), weight = "unweighted") 
speaker_agree <- agree(cbind(basic_level_reliability_combined$human.all.codes, basic_level_reliability_combined$recode_speaker))


  
