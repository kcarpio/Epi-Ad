# November 1st, 2023
# plots for UPSIDE/Magee hCG paper
# data: merged_451_with_stress_and_parity.csv, UPSIDE_ONLY.csv

# hCG (y) vs. gestational age (linear, quadratic) with effect modification by race
# hCG (y) vs. gestational age (linear, quadratic) with effect modification by infant sex

library(scales)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

cbp <- c("#D55E00", "#0072B2") # create color vector to differentiate sex from race plots

# hcg-a + sex
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnhCG.A.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-alpha (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())


# hcg-b + sex
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnhCG.B.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-beta (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())


# hcg-h + sex
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnHCG.h.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-hyperglycosylated (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(limits = c(5,12))


# hcg-intact + sex
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnHCG.iu)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-intact (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())
  


# alternative 
# ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(`Infant sex`)), aes(lnHCG.iu, BldPcaWeeks, color = `Infant sex`)) +
#geom_point(aes(fill = `Infant sex`), size = 2) +
#stat_smooth(aes(fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
#labs(x = "hCG-intact (log)", y = "hCG-intact (log)") + theme_classic()

# next set of plots, by race
cbp1 <- c("#0072B2", "#009E73", "#D55E00", "#F0E442",
          "#CC79A7", "#0072B2", "#D55E00", "#CC79A7")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# merge "mixed" with "others"
# recode the levels
merged_451_with_stress_and_parity <- merged_451_with_stress_and_parity %>%
  mutate(race_new = case_when(
    race_new %in% c("Others", "Mixed") ~ "Mixed/Other",
    TRUE ~ as.character(race_new)
  ))

merged_451_with_stress_and_parity$Race <- merged_451_with_stress_and_parity$race_new

# hcg-a + race
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(Race)), aes(BldPcaWeeks, lnhCG.A.pmol)) +
  geom_point(aes(color = Race), size = 2) +
  geom_smooth(aes(color = Race, fill = Race), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-alpha (pmol/l, log)") +
  scale_fill_manual(values = cbp1) + 
  scale_colour_manual(values=cbp1)+
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())

# hcg-b + race
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(Race)), aes(BldPcaWeeks, lnhCG.B.pmol)) +
  geom_point(aes(color = Race), size = 2) +
  geom_smooth(aes(color = Race, fill = Race), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-beta (pmol/l, log)") +
  scale_fill_manual(values = cbp1) + 
  scale_colour_manual(values=cbp1)+
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())

# hcg-h + race
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(Race)), aes(BldPcaWeeks, lnHCG.h.pmol)) +
  geom_point(aes(color = Race), size = 2) +
  geom_smooth(aes(color = Race, fill = Race), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-hyperglycosylated (pmol/l, log)") +
  scale_fill_manual(values = cbp1) + 
  scale_colour_manual(values=cbp1)+
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())

# hcg-intact + race
ggplot(data = subset(merged_451_with_stress_and_parity, !is.na(Race)), aes(BldPcaWeeks, lnHCG.iu)) +
  geom_point(aes(color = Race), size = 2) +
  geom_smooth(aes(color = Race, fill = Race), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-intact (pmol/l, log)") +
  scale_fill_manual(values = cbp1) + 
  scale_colour_manual(values=cbp1)+
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(breaks= pretty_breaks())


# redone XXplots
# UPSIDE only

ggplot(data = subset(UPSIDE_ONLY, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnhCG.A.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-alpha (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(limits = c(6,13))


ggplot(data = subset(UPSIDE_ONLY, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnhCG.B.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-beta (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(limits = c(5,13))


ggplot(data = subset(UPSIDE_ONLY, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnHCG.h.pmol)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-hyperglycosylated (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(limits = c(6,13))

ggplot(data = subset(UPSIDE_ONLY, !is.na(`Infant sex`)), aes(BldPcaWeeks, lnHCG.iu)) +
  geom_point(aes(color = `Infant sex`), size = 2) +
  geom_smooth(aes(color = `Infant sex`, fill = `Infant sex`), method = "lm", formula = y ~ x + I(x^2), size = 0.5, alpha = 0.3) +
  labs(x = "Gestational age (weeks)", y = "hCG-intact (pmol/l, log)") +
  scale_fill_manual(values = cbp) + 
  scale_colour_manual(values=cbp)+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_x_continuous(breaks= pretty_breaks())+
  scale_y_continuous(limits = c(10,13))
