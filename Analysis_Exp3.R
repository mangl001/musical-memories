#Analysis paper: Exp3

# required packages
library(tidyverse)
library(plyr)
library(multcomp)
library(lsmeans)
library(car)
library(diptest)
library(readxl)

# data_exp3_long <- read_excel("data/exp3-long-all.xlsx") # load data
data_exp3_long$Time <- as.factor(data_exp3_long$Time)

#SONG nominations:
###  Year Release Song
Exp3_MusicNomination <- data_exp3_long[c(1,18,35,56,61,66,71,76)]
Exp3_MusicNomination_long <- Exp3_MusicNomination %>% gather(Condition,Year, -Participant, -Time)
Exp3_MusicNomination_long$Time <- factor(Exp3_MusicNomination_long$Time, levels = c("Age511", "Age1215", "Age1618","Current"))

### Nomination 1 song
Exp3_MusicNomination_Song1 <- Exp3_MusicNomination_long %>% filter( Condition == "YearSong1")
res = hist(Exp3_MusicNomination_Song1$Year, breaks=100)

qlibrary(ggridges)
p.time.Year.b <- ggplot(Exp3_MusicNomination_Song1, aes(x = Year, y = Time)) +
    geom_density_ridges(aes(fill = Time), alpha= .5) +
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    # scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07","#999999")) +
    xlab("Year of release (nominated song)") +
    theme_bw()

ii3=1939:2020
Figure7_exp3 <- p.time.Year.b + scale_x_continuous(breaks = ii3[c(F,T,F,F,F,F,F,F,F,F)]) + 
    theme(axis.text.x = element_text(size=12), axis.title=element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 12),
          axis.text.y = element_text(size=12),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))

ggsave("Figure7_exp3.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

mod.lm.Year = lm(Year ~ (as.factor(Time)), data = Exp3_MusicNomination_Song1)
anova(mod.lm.Year)
summary.lm(mod.lm.Year)
posthoc.exp3.mod.Year <- glht(mod.lm.Year, lsm(pairwise ~ Time))
summary(posthoc.exp3.mod.Year, test=adjusted("holm"))

#Looking for the bump
# 1 nomination
data_exp3 = Exp3_MusicNomination_Song1 %>%
    summarise(x = count(Year)) 

##############
# Analysis - qualitative: content analysis
#############
# Exp3_ContentAnalysis_Raw <- read_excel("data/exp3-content-analysis.xlsx") # load data

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


Figure8b <- ggplot(data=data_exp3_content_long_summary, aes(x=Time, y=smean, fill= Time)) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=smean - se, ymax=smean+se), width=.2, position=position_dodge(.9)) +
    ylab("") +
    ylim(0, 0.75) +
    facet_wrap(~ condition, nrow=1) + 
    theme_bw()

SIZE = 12

Figure8b_Final <- Figure8b +
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    theme(axis.text.y=element_text(size=SIZE),axis.title.x = element_blank(),
                                  strip.text = element_text(size=SIZE),axis.text.x = element_text(size=10,angle = 90, hjust = 1),
                                  axis.title=element_text(size=SIZE), legend.position =  "none",
                                  axis.ticks.x = element_blank())


ggsave("Exp3_Figure8b_Final.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")


# FIGURE 2: content analysis results in all experiemnts
library(cowplot)
figure2b_exp123 = plot_grid(Figure2b_final, Figure5_year_final, Figure8b_Final, nrow=3)

ggsave("figure2b_exp123year.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")




#######################################################
#### EXCLUDED in the paper
#######################################################

#Influences
DataExp3_Influences <- data_exp3_long[c(18,24:28)]
DataExp3_Influences_long <- DataExp3_Influences %>% gather(Condition,Influence, -Time)
DataExp3_Influences_long$Time <- factor(DataExp3_Influences_long$Time, levels = c("Age511", "Age1215", "Age1618","Current"))
d.E3_Influences_long2 <- transform(DataExp3_Influences_long, z_score=ave(Influence, Time, FUN=scale))

Plot.Ex3.Infuence <- summarySE(DataExp3_Influences_long, measurevar="Influence",groupvars=c("Time","Condition"))
P.Ex3.Infuence<- ggplot(Plot.Ex3.Infuence, aes(x=Time, y=Influence, group = Condition, shape=Condition, linetype=Condition))+ 
    # geom_errorbar(aes(ymin=z_score-ci, ymax=z_score+ci), width=.1, 
    #               position=position_dodge(0.005)) +
    geom_ribbon(aes(ymin=Influence-ci, ymax=Influence+ci, fill = Condition), alpha = 0.1) +
    geom_line(aes(color=Condition)) +
    geom_point()+
    ylim(1,5)+
    labs(x="Time", y = "Preference")+
    theme_bw()
P.Ex3.Infuence + scale_x_discrete(breaks=c("Age511","Age1215","Age1618","Current"),
                                  labels=c("5 to 11","12 to 15","16 to 18","Current"))

ggsave("Exp3.Influences.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")

d.E3_MP_long2.friends <- d.E3_Influences_long2 %>% filter( Condition == "friends")
mod.E3.friends <- aov(as.numeric(Influence) ~ as.factor(Time), data = d.E3_MP_long2.friends)
anova(mod.E3.friends)
summary.lm(mod.E3.friends)
posthoc.exp3.friends <- glht(mod.E3.friends, lsm(pairwise ~ Time))
summary(posthoc.exp3.friends, test=adjusted("holm"))

d.E3_MP_long2.media <- d.E3_Influences_long2 %>% filter( Condition == "media")
mod.E3.media <- aov(as.numeric(Influence) ~ as.factor(Time), data = d.E3_MP_long2.media)
anova(mod.E3.media)
summary.lm(mod.E3.media)
posthoc.exp3.media <- glht(mod.E3.media, lsm(pairwise ~ Time))
summary(posthoc.exp3.media, test=adjusted("holm"))

d.E3_MP_long2.parents <- d.E3_Influences_long2 %>% filter( Condition == "parents")
mod.E3.parents <- aov(as.numeric(Influence) ~ as.factor(Time), data = d.E3_MP_long2.parents)
anova(mod.E3.parents)
summary.lm(mod.E3.parents)
posthoc.exp3.oparents <- glht(mod.E3.parents, lsm(pairwise ~ Time))
summary(posthoc.exp3.oparents, test=adjusted("holm"))

d.E3_MP_long2.sibs <- d.E3_Influences_long2 %>% filter( Condition == "sibs")
mod.E3.sibs <- aov(as.numeric(Influence) ~ as.factor(Time), data = d.E3_MP_long2.sibs)
anova(mod.E3.sibs)
summary.lm(mod.E3.sibs)

d.E3_MP_long2.otherfam <- d.E3_Influences_long2 %>% filter( Condition == "otherfam")
mod.E3.otherfam <- aov(as.numeric(Influence) ~ as.factor(Time), data = d.E3_MP_long2.otherfam)
anova(mod.E3.otherfam)
summary.lm(mod.E3.otherfam)

### music preferneces
DataExp3_MusicalPrefences <- Exp3_LongData_ALL[c(18:23)]
DataExp3_MusicalPrefences_long <- DataExp3_MusicalPrefences %>% gather(Condition,Preferences, -Time)
DataExp3_MusicalPrefences_long$Time <- factor(DataExp3_MusicalPrefences_long$Time, levels = c("Age511", "Age1215", "Age1618","Current"))
d.E3_MP_long2 <- transform(DataExp3_MusicalPrefences_long, z_score=ave(Preferences, Time, FUN=scale))

Plot.Ex3.Preferences <- summarySE(DataExp3_MusicalPrefences_long, measurevar="Preferences",groupvars=c("Time","Condition"))
P.Ex3.Preferences<- ggplot(Plot.Ex3.Preferences, aes(x=Time, y=Preferences, group = Condition, shape=Condition, linetype=Condition))+ 
    # geom_errorbar(aes(ymin=z_score-ci, ymax=z_score+ci), width=.1, 
    #               position=position_dodge(0.005)) +
    geom_ribbon(aes(ymin=Preferences-ci, ymax=Preferences+ci, fill = Condition), alpha = 0.1) +
    geom_line(aes(color=Condition)) +
    geom_point()+
    ylim(1,7)+
    labs(x="Time", y = "Preference")+
    theme_bw()
P.Ex3.Preferences + scale_x_discrete(breaks=c("Age511","Age1215","Age1618","Current"),
                                     labels=c("5 to 11","12 to 15","16 to 18","Current"))

ggsave("Exp3.Preferences.time.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")

Plot.Ex3.Preferences.Z <- summarySE(d.E3_MP_long2, measurevar="z_score",groupvars=c("Time","Condition"))
P.Ex3.Preferences.Z<- ggplot(Plot.Ex3.Preferences.Z, aes(x=Time, y=z_score, group = Condition, shape=Condition, linetype=Condition))+ 
    # geom_errorbar(aes(ymin=z_score-ci, ymax=z_score+ci), width=.1, 
    #               position=position_dodge(0.005)) +
    geom_ribbon(aes(ymin=z_score-ci, ymax=z_score+ci, fill = Condition), alpha = 0.1) +
    geom_line(aes(color=Condition)) +
    geom_point()+
    ylim(-1,1)+
    labs(x="Time Period", y = "Preference")+
    theme_bw()
P.Ex3.Preferences.Z + scale_x_discrete(breaks=c("Age511","Age1215","Age1618","Current"),
                                       labels=c("5 to 11","12 to 15","16 to 18","Current"))

ggsave("Exp3.Preferences.time.zscores.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")

d.E3_MP_long2$Condition <- as.factor(d.E3_MP_long2$Condition)
d.E3_MP_long2$Time <- as.factor(d.E3_MP_long2$Time)

d.E3_MP_long2.Contemp <- d.E3_MP_long2 %>% filter( Condition == "Contemp")
mod.E3.Contemp <- aov(Preferences ~ Time, data = d.E3_MP_long2.Contemp)
anova(mod.E3.Contemp)
summary.lm(mod.E3.Contemp)
posthoc.exp3.Contemp <- glht(mod.E3.Contemp, lsm(pairwise ~ Time))
summary(posthoc.exp3.Contemp, test=adjusted("holm"))

d.E3_MP_long2.Intense<- d.E3_MP_long2 %>% filter( Condition == "Intense")
mod.E3.Intense <- aov(z_score ~ Time, data = d.E3_MP_long2.Intense)
anova(mod.E3.Intense)
summary.lm(mod.E3.Intense)

d.E3_MP_long2.Mellow<- d.E3_MP_long2 %>% filter( Condition == "Mellow")
mod.E3.Mellow <- aov(z_score ~ Time, data = d.E3_MP_long2.Mellow)
anova(mod.E3.Mellow)
summary.lm(mod.E3.Mellow)

d.E3_MP_long2.Soph<- d.E3_MP_long2 %>% filter( Condition == "Soph")
mod.E3.S <- aov(z_score ~ Time, data = d.E3_MP_long2.Soph)
anova(mod.E3.S)
summary.lm(mod.E3.S)

d.E3_MP_long2.Unpret<- d.E3_MP_long2 %>% filter( Condition == "Unpret")
mod.E3.U <- aov(z_score ~ Time, data = d.E3_MP_long2.Unpret)
anova(mod.E3.U)
summary.lm(mod.E3.U)

# ratings scales: 1 nomination
### 1.1 RatingScales
DataExp3_RatingScales <- Exp3_LongData_ALL[c(1,18,47:52)]
DataExp3_RatingScales_long <- DataExp3_RatingScales %>% gather(Condition,Scales,-Participant, -Time)
DataExp3_RatingScales_long$Time <- factor(DataExp3_RatingScales_long$Time, levels = c("Age511", "Age1215", "Age1618","Current"))

DataExp3_RatingScales_long.Zscores <- transform(DataExp3_RatingScales_long, z_score=ave(Scales, Time, FUN=scale))

Plot.Ex3.Scales <- summarySE(DataExp3_RatingScales_long, measurevar="Scales",groupvars=c("Time","Condition"))
P.Ex3.Scales<- ggplot(Plot.Ex3.Scales, aes(x=Time, y=Scales, group = Condition, shape=Condition, linetype=Condition))+ 
    # geom_errorbar(aes(ymin=z_score-ci, ymax=z_score+ci), width=.1, 
    #               position=position_dodge(0.005)) +
    geom_ribbon(aes(ymin=Scales-ci, ymax=Scales+ci, fill = Condition), alpha = 0.1) +
    geom_line(aes(color=Condition)) +
    geom_point()+
    ylim(1,5)+
    labs(x="Time", y = "Rating")+
    theme_bw()
P.Ex3.Scales
Plot.Ex3.Scales + scale_x_discrete(breaks=c("Age511","Age1215","Age1618","Current"),
                                   labels=c("5 to 11","12 to 15","16 to 18","Current"))

ggsave("Exp3.RatingScales.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")


Plot.Ex3.Scales.Z <- summarySE(DataExp3_RatingScales_long.Zscores, measurevar="z_score",groupvars=c("Time","Condition"))
P.Ex3.Scales.Z<- ggplot(Plot.Ex3.Scales.Z, aes(x=Time, y=z_score, group = Condition, shape=Condition, linetype=Condition))+ 
    # geom_errorbar(aes(ymin=z_score-ci, ymax=z_score+ci), width=.1, 
    #               position=position_dodge(0.005)) +
    geom_ribbon(aes(ymin=z_score-ci, ymax=z_score+ci, fill = Condition), alpha = 0.1) +
    geom_line(aes(color=Condition)) +
    geom_point()+
    ylim(-1,1)+
    labs(x="Time", y = "Rating")+
    theme_bw()
P.Ex3.Scales.Z + scale_x_discrete(breaks=c("Age511","Age1215","Age1618","Current"),
                                  labels=c("5 to 11","12 to 15","16 to 18","Current"))

ggsave("Exp3.RatingScales.Zscores.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")


d.E3_I_liking <- DataExp3_RatingScales_long.Zscores %>% filter( Condition == "liking")
mod.E3.like <- lm(z_score ~ as.factor(Time), data = d.E3_I_liking)
anova(mod.E3.like)
summary(mod.E3.like)


### Age of importance
DataExp3_AaR_long.AgeImportance <- DataExp3_AaR_long %>% filter( Condition == "AGEwhenMEMORY")
DataExp3_AaR_long.AgeImportance$Time <- factor(DataExp3_AaR_long.AgeImportance$Time, levels = c("Age511", "Age1215", "Age1618","Current"))
hist(DataExp3_AaR_long.AgeImportance$AaR, breaks=100)


mu.time.AoI <- ddply(DataExp3_AaR_long.AgeImportance, "Time", summarise, grp.mean=mean(AaR, na.rm=TRUE))
head(mu.time.AoI)

p.time.AoI <- ggplot(DataExp3_AaR_long.AgeImportance, aes(x=AaR, fill=Time)) +
    geom_density(alpha=0.4) + theme_bw()
p.time.AoI + geom_vline(data=mu.time.AoI, aes(xintercept=grp.mean, color=factor(Time)),
                        linetype="dashed") + theme(axis.text=element_text(size=14), 
                                                   axis.title=element_text(size=16),
                                                   axis.title.x = element_blank()) + labs(fill = "Time")

ggsave("p.time.AoI.A.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")


library(ggridges)
p.time.AoI.b <- ggplot(DataExp3_AaR_long.AgeImportance, aes(x = AaR, y = Time)) +
    geom_density_ridges(aes(fill = Time)) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07","#999999")) +
    theme_bw()

p.time.AoI.b + theme(axis.text=element_text(size=14), 
                     axis.title=element_text(size=16),
                     axis.title.x = element_blank())

ggsave("p.time.AoI.B.pdf", width=20, height=20, units = c("cm"),
       dpi=300, device = "pdf")

mod.AoI = lm(AaR ~ (as.factor(Time)), data = DataExp3_AaR_long.AgeImportance)
anova(mod.AoI)
summary.lm(mod.AoI)
posthoc.exp3.mod.AoI <- glht(mod.AoI, lsm(pairwise ~ Time))
summary(posthoc.exp3.mod.AoI, test=adjusted("holm"))

