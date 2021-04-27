#### Analyzing Respiration of Intertidal Invertebrates in Diversity Treatments

### Load Libraries
library(tidyverse)
library(here)
library(PNWColors)
#library(car) # qqp() and Anova()
#library(emmeans) # post-hoc test emmeans()
#library(agricolae) # HSD.test() 
#library(lme4) # testing random effects
#library(lmerTest) # get anova results from lmer
#library(MASS)
library(patchwork)

rm(list=ls())

### Read in Data
Rdata<-read_csv(here("Output","TT_Rates.csv"))
head(Rdata)

# umol.g.hr: Oxygen depletion rate normalized to dry weight (umol/g/hour)
# Rate.ln: natural log of umol.g.hr; natural log of the Oxygen depletion rate normalized to dry weight
# Value: raw recorded Oxygen value (umol/L)
# Temp.C: Average temperature during measurement run (degree C)
# Temp: raw recorded Temperature value (degree C)


### Data Analysis

# clean data frame
respo <- Rdata %>% 
  dplyr::select(Run.ID, Run, Chamber, Group.ID, Assemblage.ID, Assemblage, umol.g.hr, Rate.ln, Temp.C) %>% 
  distinct()

# make factor type
respo$Run.ID <- factor(respo$Run.ID)
respo$Run <- factor(respo$Run)
respo$Chamber <- factor(respo$Chamber)
respo$Group.ID <- factor(respo$Group.ID)
respo$Assemblage.ID <- factor(respo$Assemblage.ID)
summary(respo)

### Summarize and Plot mean log-transformed respiration rates
respo %>%
  group_by(Assemblage.ID) %>% 
  summarise(mean = mean(Rate.ln),
            sd = sd(Rate.ln),
            se = sd/sqrt(length(Rate.ln))) %>% 
  ggplot(aes(x = Assemblage.ID, y = mean, color = Assemblage.ID)) +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), stat="identity", position=position_dodge(width=0.9), width=0.1)
  


########################################
### Are the observed respiration rates additive or synergistic/antagonistic?
########################################

# get assemblage categories
A1 <- respo %>% # Mussels, Predatory Snail, Herbivory Snail
  filter(Assemblage.ID == 'A1') %>% 
  dplyr::select(Assemblage)
A2 <- respo %>% # Mussels and Predatory Snail
  filter(Assemblage.ID == 'A2') %>% 
  dplyr::select(Assemblage)
A3 <- respo %>% # Mussels and Herbivory Snail
  filter(Assemblage.ID == 'A3') %>% 
  dplyr::select(Assemblage)
A4 <- respo %>% # Herbivory Snail and Predatory Snail
  filter(Assemblage.ID == 'A4') %>% 
  dplyr::select(Assemblage)


# calculate the sum of respiration rates of grouped individuals to compare to assemblage rates
A5_A6.add <- respo %>% 
  group_by(Group.ID) %>% 
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A6') %>% 
  mutate(Assemblage.ID = "A5_A6",
         Assemblage = as.character(A2[1,]),
         umol.g.hr = sum(umol.g.hr),
         Rate.ln = log(umol.g.hr + 0.1)) %>% 
  dplyr::select(Run, Chamber, Group.ID, Assemblage.ID, Assemblage, umol.g.hr, Rate.ln, Temp.C)

A5_A7.add <- respo %>% 
  group_by(Group.ID) %>% 
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = "A5_A7",
         Assemblage = as.character(A3[1,]),
         umol.g.hr = sum(umol.g.hr),
         Rate.ln = log(umol.g.hr + 0.1)) %>% 
  dplyr::select(Run, Chamber, Group.ID, Assemblage.ID, Assemblage, umol.g.hr, Rate.ln, Temp.C)

A6_A7.add <- respo %>% 
  group_by(Group.ID) %>% 
  filter(Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = "A6_A7",
         Assemblage = as.character(A4[1,]),
         umol.g.hr = sum(umol.g.hr),
         Rate.ln = log(umol.g.hr + 0.1)) %>% 
  dplyr::select(Run, Chamber, Group.ID, Assemblage.ID, Assemblage, umol.g.hr, Rate.ln, Temp.C)

A5_A6_A7.add <- respo %>% 
  group_by(Group.ID) %>% 
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = "A5_A6_A7",
         Assemblage = as.character(A1[1,]),
         umol.g.hr = sum(umol.g.hr),
         Rate.ln = log(umol.g.hr + 0.1)) %>% 
  dplyr::select(Run, Chamber, Group.ID, Assemblage.ID, Assemblage, umol.g.hr, Rate.ln, Temp.C)

# join dataframes
respo.add <- respo %>% 
  full_join(A5_A6.add) %>% 
  full_join(A5_A7.add) %>% 
  full_join(A6_A7.add) %>% 
  full_join(A5_A6_A7.add)



########################################
### Summarize difference between Measured and Expected respiration rates
########################################

# Pivot wider data frame
respo.wide <- respo.add %>% 
  dplyr::select(-c(Run.ID, Run, Chamber)) %>% 
  pivot_wider(values_from = Rate.ln, names_from = Assemblage.ID) %>% 
  dplyr::select(-c(umol.g.hr, Temp.C)) %>% 
  distinct() %>% 
  group_by(Group.ID) %>% 
  summarise(diff_A1 = (mean(A1,na.rm=T) - mean(A5_A6_A7,na.rm=T))/mean(A5_A6_A7,na.rm=T), # (observed - expected) / expected
            diff_A2 = (mean(A2,na.rm=T) - mean(A5_A6,na.rm=T))/mean(A5_A6,na.rm=T),
            diff_A3 = (mean(A3,na.rm=T) - mean(A5_A7,na.rm=T))/mean(A5_A7,na.rm=T),
            diff_A4 = (mean(A4,na.rm=T) - mean(A6_A7,na.rm=T))/mean(A6_A7,na.rm=T),
            A1 = mean(A1,na.rm=T), # Mussels, Predatory Snail, Herbivory Snail
            A2 = mean(A2,na.rm=T), # Mussels and Predatory Snail
            A3 = mean(A3,na.rm=T), # Mussels and Herbivory Snail
            A4 = mean(A4,na.rm=T), # Herbivory Snail and Predatory Snail
            A5 = mean(A5,na.rm=T), # Mussels
            A6 = mean(A6,na.rm=T), # Predaotry Snail
            A7 = mean(A7,na.rm=T), # Herbivory Snail
            A5_A6_A7 = mean(A5_A6_A7,na.rm=T), # Mussels, Predatory Snail, Herbivory Snail
            A5_A6 = mean(A5_A6,na.rm=T), # Mussels and Predatory Snail
            A5_A7 = mean(A5_A7,na.rm=T), # Mussels and Herbivory Snail
            A6_A7 = mean(A6_A7,na.rm=T)) %>% # Herbivory Snail and Predatory Snail
  distinct()

respo.long <- respo.wide %>% 
  pivot_longer(cols = diff_A1:A6_A7, names_to = "Assemblage.ID", values_to = "Rate.ln") %>% 
  drop_na()

write.csv(respo.long, here("Output","Resp_Rates_Long.csv"))

########################################
### Summary Stats ###
########################################

# Mean data: points with error bars
respo.long.sum<-respo.long %>%
  filter(!str_detect(Assemblage.ID, "diff")) %>%  # remove diff values
  group_by(Assemblage.ID) %>% 
  summarise(mean = mean(Rate.ln),
            sd = sd(Rate.ln),
            se = sd/sqrt(length(Rate.ln)))
p1<-respo.long.sum %>% 
  ggplot(aes(x = fct_reorder(Assemblage.ID, mean, .desc = T), y = mean, 
             color = fct_reorder(Assemblage.ID, mean, .desc = T))) +
  labs(color = 'Assemblage ID',
       x = 'Assemblage ID', y = 'mean log(Respiration Rate) (umol/g/hr)') +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), stat="identity", 
                position=position_dodge(width=0.9), width=0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001))


# Raw data: boxplot with jitter points
p2<-respo.long %>% 
  filter(!str_detect(Assemblage.ID, "diff")) %>% # remove difference
  ggplot(aes(x = fct_reorder(Assemblage.ID, Rate.ln, .desc = T),  y = Rate.ln, 
             color = fct_reorder(Assemblage.ID, Rate.ln, .desc = T))) +
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  labs(color = 'Assemblage ID',
       x = 'Assemblage ID', y = 'log(Respiration Rate) (umol/g/hr)') +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.001))



# both together
p2 + p1

########################################
### Individual Trend Plots ###
########################################
#4/27/21 with Danielle

respo.long %>%
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) +
  geom_point() + 
  facet_wrap(~Assemblage.ID)

#Danielle did this

respo.long %>% 
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_point() +
  facet_wrap(~Assemblage.ID, scales = "fixed")

respo.long %>% 
  filter(Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Assemblage.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  theme_bw()

########################################
### Assemblage Trend Plots ###
########################################

### Mussels, Predatory Snail, Herbivory Snail
#comparing observed vs expected
  #4/27/21 with Danielle

respo.long<- respo.long %>%
  group_by(Assemblage.ID) %>%
  mutate(mean = mean(Rate.ln))

p1<-respo.long %>% 
  filter(Assemblage.ID == 'A1' | Assemblage.ID == 'A5_A6_A7') %>%
  ggplot(aes(x = Group.ID, y = Rate.ln)) +
  geom_point() +
  geom_hline(aes(yintercept = mean)) +
  facet_wrap(~Assemblage.ID)
  
p2<-respo.long %>% 
  filter(Assemblage.ID == 'A2' | Assemblage.ID == 'A5_A6') %>%
  ggplot(aes(x = Group.ID, y = Rate.ln)) +
  geom_point() +
  geom_hline(aes(yintercept = mean)) +
  facet_wrap(~Assemblage.ID)
  
p3<-respo.long %>% 
  filter(Assemblage.ID == 'A3' | Assemblage.ID == 'A5_A7') %>%
  ggplot(aes(x = Group.ID, y = Rate.ln)) +
  geom_point() +
  geom_hline(aes(yintercept = mean)) +
  facet_wrap(~Assemblage.ID)

p4<-respo.long %>% 
  filter(Assemblage.ID == 'A4' | Assemblage.ID == 'A6_A7') %>%
  ggplot(aes(x = Group.ID, y = Rate.ln)) +
  geom_point() +
  geom_hline(aes(yintercept = mean)) +
  facet_wrap(~Assemblage.ID)

(p1 + p2) /
  (p3 + p4)

#Danielle Did this 
### Mussels, Predatory Snail, Herbivory Snail
respo.long %>% 
  filter(Assemblage.ID == 'A1' | Assemblage.ID == 'A5_A6_A7' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A1" = "Measured: Mussels, Tegula, & Acanthina",
                                       "A5_A6_A7" = "Expected: Mussels, Tegula, & Acanthina",
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_point() +
  facet_wrap(~Assemblage.ID, scales = "fixed")

respo.long %>% 
  filter(Assemblage.ID == 'A1' | Assemblage.ID == 'A5_A6_A7' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A1" = "Measured: Mussels, Tegula, & Acanthina",
                                       "A5_A6_A7" = "Expected: Mussels, Tegula, & Acanthina",
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Assemblage.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  theme_bw()

# respo.long %>% 
#   filter(str_detect(Assemblage.ID, "diff_A1")) %>% 
#   mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
#                                        "diff_A1" = "Mussels, Tegula, & Acanthina")) %>% 
#   ggplot(aes(x = fct_reorder(Group.ID, Rate.ln, .desc = T), y = Rate.ln, 
#              color = Group.ID, fill = Group.ID)) +
#   geom_col() +
#   facet_wrap(~Assemblage.ID) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d()

### Mussels and Predatory Snail
respo.add %>% 
  filter(Assemblage.ID == 'A2' | Assemblage.ID == 'A5_A6' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A6') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A2" = "Measured: Mussels & Acanthina",
                                       "A5_A6" = "Expected: Mussels & Acanthina",
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina")) %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_point() +
  facet_wrap(~Assemblage.ID, scales = "fixed")

respo.long %>% 
  filter(Assemblage.ID == 'A2' | Assemblage.ID == 'A5_A6' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A6') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A2" = "Measured: Mussels & Acanthina",
                                       "A5_A6" = "Expected: Mussels & Acanthina",
                                       "A5" = "Mussels",
                                       "A6" = "Acanthina")) %>%
  ggplot(aes(x = Assemblage.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  theme_bw()

# respo.long %>% 
#   filter(str_detect(Assemblage.ID, "diff_A2")) %>% 
#   mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
#                                        "diff_A2" = "Mussels & Acanthina")) %>% 
#   ggplot(aes(x = fct_reorder(Group.ID, Rate.ln, .desc = T), y = Rate.ln, 
#              color = Group.ID, fill = Group.ID)) +
#   geom_col() +
#   facet_wrap(~Assemblage.ID) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d()

### Mussels and Herbivory Snail
respo.add %>% 
  filter(Assemblage.ID == 'A3' | Assemblage.ID == 'A5_A7' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A3" = "Measured: Mussels & Tegula",
                                       "A5_A7" = "Expected: Mussels & Tegula",
                                       "A5" = "Mussels",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_point() +
  facet_wrap(~Assemblage.ID, scales = "fixed")

respo.long %>% 
  filter(Assemblage.ID == 'A3' | Assemblage.ID == 'A5_A7' | 
           Assemblage.ID == 'A5' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A3" = "Measured: Mussels & Tegula",
                                       "A5_A7" = "Expected: Mussels & Tegula",
                                       "A5" = "Mussels",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Assemblage.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  theme_bw()


# respo.long %>% 
#   filter(str_detect(Assemblage.ID, "diff_A3")) %>% 
#   mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
#                                        "diff_A3" = "Mussels & Tegula")) %>% 
#   ggplot(aes(x = fct_reorder(Group.ID, Rate.ln, .desc = T), y = Rate.ln, 
#              color = Group.ID, fill = Group.ID)) +
#   geom_col() +
#   facet_wrap(~Assemblage.ID) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d()

### Herbivory Snail and Predatory Snail
respo.add %>% 
  filter(Assemblage.ID == 'A4' | Assemblage.ID == 'A6_A7' | 
           Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A4" = "Measured: Acanthina & Tegula",
                                       "A6_A7" = "Expected: Acanthina & Tegula",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_point() +
  facet_wrap(~Assemblage.ID, scales = "fixed")

respo.long %>% 
  filter(Assemblage.ID == 'A4' | Assemblage.ID == 'A6_A7' | 
           Assemblage.ID == 'A6' | Assemblage.ID == 'A7') %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       "A4" = "Measured: Acanthina & Tegula",
                                       "A6_A7" = "Expected: Acanthina & Tegula",
                                       "A6" = "Acanthina",
                                       "A7" = "Tegula")) %>%
  ggplot(aes(x = Assemblage.ID, y = Rate.ln, color = Assemblage.ID)) + 
  geom_boxplot() +
  geom_jitter(position = "dodge") +
  theme_bw()

# respo.long %>% 
#   filter(str_detect(Assemblage.ID, "diff_A4")) %>% 
#   mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
#                                        "diff_A4" = "Tegula & Acanthina")) %>% 
#   ggplot(aes(x = fct_reorder(Group.ID, Rate.ln, .desc = T), y = Rate.ln, 
#              color = Group.ID, fill = Group.ID)) +
#   geom_col() +
#   facet_wrap(~Assemblage.ID) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d()

#USE THIS ONE 
### All faceted
respo.long %>% 
  filter(str_detect(Assemblage.ID, "diff")) %>% 
  #mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                       #"diff_A1" = "Mussels, Tegula, & Acanthina",
                                       #"diff_A2" = "Mussels & Acanthina",
                                       #"diff_A3" = "Mussels & Tegula",
                                       #"diff_A4" = "Tegula & Acanthina")) %>% 
  ggplot(aes(x = Group.ID, y = Rate.ln, 
             color = Group.ID, fill = Group.ID)) +
  geom_col() +
  facet_wrap(~Assemblage.ID) +
  scale_color_viridis_d() +
  scale_fill_viridis_d()



########################################
### Statistics and Analysis ###
########################################

### Is the difference between expected and observed significantly different within groups?

a1<- respo.long %>% 
  filter(Assemblage.ID == 'A1' | Assemblage.ID == 'A5_A6_A7')
t.test(a1, paired = T)
t.test(Length~Location, paired=TRUE, data=mydata, na.rm=TRUE)


a2<- respo.long %>% 
  filter(Assemblage.ID == 'A2' | Assemblage.ID == 'A5_A6')
t.test(a2, paired = T)

a3<- respo.long %>% 
  filter(Assemblage.ID == 'A3' | Assemblage.ID == 'A5_A7')
t.test(a3, paired = T)

a4<- respo.long %>% 
  filter(Assemblage.ID == 'A4' | Assemblage.ID == 'A6_A7')
t.test(a4$Assemblage.ID ~ a4$Rate.ln, paired = T)


# set color palette
pal = pnw_palette('Starfish', 4)

# summarize differences by group and assemblage, then plot differences
# (observed - expected) / expected
respo.wide  %>% 
  pivot_longer(cols = diff_A1:diff_A4, names_to = 'Assemblage.ID', values_to = 'Rate.ln') %>% 
  drop_na() %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                "diff_A1" = "Mussels, Tegula, Acanthina",
                                "diff_A2" = "Mussels & Acanthina",
                                "diff_A3" = "Mussels & Tegula",
                                "diff_A4" = "Tegula & Acanthina")) %>% 
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID, fill = Assemblage.ID)) +
  theme_bw() +
  geom_col(position = "dodge") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = ) +
  facet_wrap(~Assemblage.ID, scales = "fixed")

# observed - expected
respo.wide %>% 
  group_by(Group.ID) %>% 
  summarise(diff_A1 = (mean(A1,na.rm=T) - mean(A5_A6_A7,na.rm=T)), 
            diff_A2 = (mean(A2,na.rm=T) - mean(A5_A6,na.rm=T)),
            diff_A3 = (mean(A3,na.rm=T) - mean(A5_A7,na.rm=T)),
            diff_A4 = (mean(A4,na.rm=T) - mean(A6_A7,na.rm=T))) %>% 
  distinct() %>% 
  pivot_longer(cols = diff_A1:diff_A4, names_to = 'Assemblage.ID', values_to = 'Rate.ln') %>% 
  drop_na() %>% 
  mutate(Assemblage.ID = dplyr::recode(Assemblage.ID, 
                                "diff_A1" = "Mussels, Tegula, Acanthina",
                                "diff_A2" = "Mussels & Acanthina",
                                "diff_A3" = "Mussels & Tegula",
                                "diff_A4" = "Tegula & Acanthina")) %>% 
  ggplot(aes(x = Group.ID, y = Rate.ln, color = Assemblage.ID, fill = Assemblage.ID)) +
  theme_bw() +
  geom_col(position = "dodge") +
  #facet_wrap(~Assemblage.ID) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)








### Test difference in respiration rates within each treatment (assemblage)

## One-way ANOVA: Difference across runs
mymodel.1<-lm(Rate.ln ~ Run, data = respo)
qqp(residuals(mymodel.1), "norm")
Anova(mymodel.1, type = "III")
# no difference across runs p > 0.7


## Repeated Measures ANOVA: Difference across assemblages within groups
model <- lmer(Rate.ln ~ Assemblage.ID + (1|Assemblage.ID:Group.ID), data = respo)
# fixed: assemblage
# random: group nested within assemblage treatment
qqp(residuals(model), "norm")
Anova(model, type = "III")
# significant difference between assemblage respiration rates p ~ 6.5e-5

## Check AIC Values for best fit model
# 'model' is the full model with the random effect
model.b <- lm(Rate.ln ~ Assemblage.ID, data = respo) # model without the random effect

## Likelihood Ratio Test (gives AIC values for both models)
anova(model, model.b)
# look for lowest AIC; gives us a p-value for whether one is a better fit
# model.b has very slightly lower AIC value; will stick with model.a

## Estimated means: Repeated Measures ANOVA
graphdata<-as.data.frame(emmeans(model, ~Assemblage.ID)) #ignoring the random factor
graphdata


### Plot Repeated Measures ANOVA with emmeans and standard error
ggplot(data=graphdata, aes(x=Assemblage.ID, y=emmean, fill=Assemblage.ID)) +
  theme_bw()+
  theme(axis.text.x=element_text(face="bold", color="black", size=13), 
        axis.text.y=element_text(face="bold", color="black", size=12), 
        axis.title = element_text(color="black", size=14, face="bold"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank()) +
  geom_bar(colour="black", fill="DodgerBlue", width=0.5, stat="identity") + 
  guides(fill=FALSE) + 
  labs(y = "log(respiration rate) (umol/g/hr)") +
  geom_errorbar(aes(ymax=emmean +SE, ymin=emmean - SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+
  geom_text(aes(label=round(emmean,digits = 3)), position = position_dodge(width=0.9), vjust = c(-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5))+ # add significance letters and adjust location on each bar
  # geom_text(aes(label=c("a","b","c","d"), position = position_dodge(width=0.9), vjust = c(2,3.5,-2,-3.5))+
  geom_hline(yintercept=0)

#t-tests
#4/27/21 With Danielle
A1<- respo.long %>%
  filter(Assemblage.ID == "A1" | Assemblage.ID == "A5_A6_A7")
Model1<-lm(Rate.ln ~ Assemblage.ID, data=A1)
anova(Model1) #Different

A2<- respo.long %>%
  filter(Assemblage.ID == "A2" | Assemblage.ID == "A5_A6")
Model2<-lm(Rate.ln ~ Assemblage.ID, data=A2)
anova(Model2) #different

A3<- respo.long %>%
  filter(Assemblage.ID == "A3" | Assemblage.ID == "A5_A7")
Model3<-lm(Rate.ln ~ Assemblage.ID, data=A3)
anova(Model3) #Different

A4<- respo.long %>%
  filter(Assemblage.ID == "A4" | Assemblage.ID == "A6_A7")
Model4<-lm(Rate.ln ~ Assemblage.ID, data=A4)
anova(Model4) #Different


