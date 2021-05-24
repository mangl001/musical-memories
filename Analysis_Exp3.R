################################################################################
# File:     Analysis Script for Experiment 3
#
# Author:   Manuel Anglada-Tort
################################################################################
library(tidyverse)
library(plyr)
library(multcomp)
library(lsmeans)
library(car)
library(diptest)
library(readxl)
library(ggridges)

SIZE = 12

# data
data_exp3_long <- read_excel("data/exp3-long-all.xlsx") 
data_exp3_long$Time <- as.factor(data_exp3_long$Time)

# Analysis - quantitative: song nominations
##  Year Release Song
Exp3_MusicNomination <- data_exp3_long[c(1,18,35,56,61,66,71,76)]
Exp3_MusicNomination_long <- Exp3_MusicNomination %>% gather(Condition,Year, -Participant, -Time)
Exp3_MusicNomination_long$Time <- factor(Exp3_MusicNomination_long$Time, levels = c("Age511", "Age1215", "Age1618","Current"))
## Nomination 1 song
Exp3_MusicNomination_Song1 <- Exp3_MusicNomination_long %>% filter( Condition == "YearSong1")
res = hist(Exp3_MusicNomination_Song1$Year, breaks=100)

plot.time.Year <- ggplot(Exp3_MusicNomination_Song1, aes(x = Year, y = Time)) +
    geom_density_ridges(aes(fill = Time), alpha= .5) +
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    # scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07","#999999")) +
    xlab("Year of release (nominated song)") +
    theme_bw()

ii3=1939:2020
Figure1_exp3 <- plot.time.Year + scale_x_continuous(breaks = ii3[c(F,T,F,F,F,F,F,F,F,F)]) + 
    theme(axis.text.x = element_text(size=12), axis.title=element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 12),
          axis.text.y = element_text(size=12))

ggsave("Figure1_exp3.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

mod.lm.Year = lm(Year ~ (as.factor(Time)), data = Exp3_MusicNomination_Song1)
anova(mod.lm.Year)
summary.lm(mod.lm.Year)
posthoc.exp3.mod.Year <- glht(mod.lm.Year, lsm(pairwise ~ Time))
summary(posthoc.exp3.mod.Year, test=adjusted("holm"))


# Analysis - qualitative: content analysis
Exp3_ContentAnalysis_Raw <- read_excel("data/exp3-content-analysis.xlsx")

Exp3_ContentAnalysis <- Exp3_ContentAnalysis_Raw[c(1,3:7)]

Exp3_ContentAnalysis_long <- Exp3_ContentAnalysis %>% gather(condition,count,Parents:Media)
Exp3_ContentAnalysis_long$condition <- as.factor(Exp3_ContentAnalysis_long$condition)

Exp3_ContentAnalysis_long$condition <- factor(Exp3_ContentAnalysis_long$condition, levels = c("Parents", "Grandparents","Peers","Media"))
Exp3_ContentAnalysis_long$Time <- factor(Exp3_ContentAnalysis_long$Time , levels = c("A511", "A1215","A1618","Now"))

model.who.exp3<- aov(count ~condition*Time, data=Exp3_ContentAnalysis_long)
Anova(model.who.exp3, Type="III")
summary.lm(model.who.exp3)

summary(glht(model.who.exp3, linfct = mcp(Half_decade = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(model.who.exp3, linfct = mcp(condition = "Tukey")), test = adjusted("holm")) #holm correction
TukeyHSD(model.who.exp3)

data_exp3_content_long_summary<- Exp3_ContentAnalysis_long %>%
    group_by(Time,condition) %>%
    dplyr::summarize(total= sum(as.numeric(count),na.rm=T),
              smean= mean(as.numeric(count),na.rm=T),
              ssd= sd(as.numeric(count),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))


Figure2_exp3 <- ggplot(data=data_exp3_content_long_summary, aes(x=Time, y=smean, fill= Time)) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=smean - se, ymax=smean+se), width=.2, position=position_dodge(.9)) +
    ylab("") +
    ylim(0, 0.75) +
    facet_wrap(~ condition, nrow=1) + 
    theme_bw()

Figure2_exp3_final <- Figure2_exp3 +
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    theme(axis.text.y=element_text(size=SIZE),axis.title.x = element_blank(),
                                  strip.text = element_text(size=SIZE),axis.text.x = element_text(size=10,angle = 90, hjust = 1),
                                  axis.title=element_text(size=SIZE), legend.position =  "none",
                                  axis.ticks.x = element_blank())


ggsave("Exp3_Figure8b_Final.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")


# add content plots from exp1,exp2 and exp3:
library(cowplot)
figure2b_exp123 = plot_grid(Figure2b_final, Figure5_year_final, Figure8b_Final, nrow=3)

ggsave("figure2b_exp123year.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# end