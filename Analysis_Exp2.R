################################################################################
# File:     Analysis Script for Experiment 2
#
# Author:   Manuel Anglada-Tort
################################################################################
library(tidyverse)
library(party)
library(caret)
library(multcomp)
library(car)
library(mgcv)
library(mgcViz) #visuals for mgcv
library(lsmeans)
library(readxl)

set.seed(10)
SIZE = 12

# Functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
    lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
    upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

Make_figure_3a <- function(gam_output){
    plot_data <- getViz(gam_output)
    plot_data_gam <- plot(sm(plot_data,1)) +  
        l_ciPoly() + l_fitLine()  + 
        labs(x="Year", y = "Effect of time on music recognition")+
        theme_bw() 
    ii= c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
    figure <- plot_data_gam + scale_x_continuous(breaks=ii) + 
        theme(axis.text = element_text(size=12), axis.title=element_text(size=12),
              axis.title.x = element_blank())
    figure
    
}

Make_Figure3b <- function(gam_output, name_variable){
    plot_gam <- getViz(gam_output)
    figure <- plot(sm(plot_gam,1)) +  
        l_ciPoly() + l_fitLine()  + l_fitContour(colors="blue")+
        ylab(paste0("Effect of time on ", name_variable)) + 
        ylim(-2.5,3.5) +
        theme_bw() 
    ii= c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
    figure_final <- figure + scale_x_continuous(breaks=ii) + 
        theme(axis.text = element_text(size=12), axis.title=element_text(size=12),
              axis.title.x = element_blank())
    figure_final
}


# Analysis - quantitative
data_exp2_long <- read_excel("data/exp2-long-all.xlsx") 
data_exp2_long <- data_exp2_long[ ! (data_exp2_long$YearReleaseSong == "2015"), ]   # excluding 2015 to be consistent with exp1

## Recognition
Exp2_recognition <- data_exp2_long %>% filter( DV == "Recognition")
Exp2_recognition %>% group_by(ResponseId) %>% tally()
Exp2_recognition %>% group_by(Song) %>% tally()
Exp2_recognition$ResponseId = as.factor(Exp2_recognition$ResponseId)
GAM.Exp2b = gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)) + s(ResponseId, bs = "re"), data = Exp2_recognition,method="REML")
plot(GAM.Exp2b)
summary(GAM.Exp2b)
coef(GAM.Exp2)
gam.check(GAM.Exp2)
# GAM.Exp2 = gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)), data = Exp2_recognition,method="REML") # without re
# plot(GAM.Exp2)
# summary(GAM.Exp2)

Figure3a <- Make_figure_3a(GAM.Exp2b)
ggsave("Exp2_GAM_figure3a.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

### Compare linear vs nonlinear models
GAM.Exp2.linear <- gam(as.numeric(Response) ~ as.numeric(YearReleaseSong), data = Exp2_recognition)
GAM.Exp2.nointercept <- gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)), data = Exp2_recognition)
summary(GAM.Exp2.linear)
summary(GAM.Exp2.nointercept)

AIC(GAM.Exp2.linear)
AIC(GAM.Exp2.nointercept)

summary(GAM.Exp2.linear)$r.sq  # adjusted R squared
summary(GAM.Exp2.nointercept)$r.sq  # adjusted R squared

anova(GAM.Exp2,GAM.Exp2.linear,test="Chisq")

### Individual differences
GAM.Exp2_IDs = gam(
    as.numeric(Response)  ~ 
        s(as.numeric(AgeAtRelease)) + 
        s(as.numeric(MT)) +
        s(as.numeric(AE)) +
        s(ResponseId, bs = "re"), 
    data = Exp2_recognition, method="REML")
summary(GAM.Exp2_IDs)
plot(GAM.Exp2_IDs)

cor.AE= cor.test(as.numeric(Exp2_recognition$Response), as.numeric(Exp2_recognition$AE), method="pearson")
cor.AE
cor.MT= cor.test(as.numeric(Exp2_recognition$Response), as.numeric(Exp2_recognition$MT), method="pearson")
cor.MT


### Rating scales
data_exp2_long$ResponseId = as.factor(data_exp2_long$ResponseId)
Exp2_like <- data_exp2_long %>% filter( DV == "like")
Exp2_quality <- data_exp2_long %>% filter( DV == "quality")
Exp2_vivid <- data_exp2_long %>% filter( DV == "vivid")

GAM.exp2.like = gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)) + s(ResponseId, bs = "re"), data = Exp2_like,method="REML")
GAM.exp2.quality = gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)) + s(ResponseId, bs = "re"), data = Exp2_quality,method="REML")
GAM.exp2.vivid = gam(as.numeric(Response) ~ s(as.numeric(YearReleaseSong)) + s(ResponseId, bs = "re"), data = Exp2_vivid,method="REML")

plot(GAM.exp2.like)
plot(GAM.exp2.quality)
plot(GAM.exp2.vivid)
summary(GAM.exp2.like)
summary(GAM.exp2.quality)
summary(GAM.exp2.vivid)

Figure3b_like <- Make_Figure3b(GAM.exp2.like, "liking")
ggsave("Figure3b_like.pdf", width=12, height=10, units = c("cm"),
       dpi=300, device = "pdf")
Figure3c_quality <- Make_Figure3b(GAM.exp2.quality, "quality")
ggsave("Figure3c_quality.pdf", width=12, height=10, units = c("cm"),
       dpi=300, device = "pdf")
Figure3d_vivid <- Make_Figure3b(GAM.exp2.vivid, "vividness")
ggsave("Figure3d_vivid.pdf", width=12, height=10, units = c("cm"),
       dpi=300, device = "pdf")


# Exploratory analysis
mean_response <- Exp2_recognition %>%
    group_by(YearReleaseSong, Song) %>%
    summarize(total= sum(as.numeric(Response),na.rm=T),
              smean= mean(as.numeric(Response),na.rm=T),
              ssd= sd(as.numeric(Response),na.rm=T),
              scount=n()) 

library(caTools)
jpeg("Figure4a.jpeg", units="cm", width=12, height=10, res=300)

vari= ts(data = mean_response$smean, start = 1960 , end = 2014 , frequency = 3)
k2=10
ks <- ksmooth(time(vari), vari, "normal", bandwidth=5)
m2=runmean(mean_response$smean, k2)
y2=runsd(mean_response$smean, k2, center=ks$y)

plot(vari, plot.type="s", type="p", ylab = "Music Recognition", xlab= "Year of Release")
lines(ks, lwd=2,  col="red")
# best inflection points matthing the line are 195, 1992, and 2001
abline(v=c(1975,1992,2001), col=c("red", "red", "red"), lty=c(1,1,1), lwd=c(1, 1,1))

# insert ggplot code
dev.off()

## Analysis of groups
data_exp2_long_timegroup <- mutate(data_exp2_long, 
                                  TimeGroup = ifelse(YearReleaseSong %in% 1960:1975, "1960-1975",
                                                ifelse(YearReleaseSong %in% 1976:1992, "1976-1992",
                                                ifelse(YearReleaseSong %in% 1993:2001, "1993-2001","2002-2014"))))
data_exp2_long_timegroup <- subset(data_exp2_long_timegroup, !is.na(Response))


data_exp2_long_timegroup %>%
    group_by(DV,TimeGroup) %>%
    summarize(total= sum(as.numeric(Response),na.rm=T),
              smean= mean(as.numeric(Response),na.rm=T),
              ssd= sd(as.numeric(Response),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))


Exp2_Long_Final.timegroups.Recognition <- data_exp2_long_timegroup %>% filter( DV == "Recognition")
Exp2_Long_Final.timegroups.like <- data_exp2_long_timegroup %>% filter( DV == "like")
Exp2_Long_Final.timegroups.quality<- data_exp2_long_timegroup %>% filter( DV == "quality")
Exp2_Long_Final.timegroups.vivid <- data_exp2_long_timegroup %>% filter( DV == "vivid")

ANOVA.exp2.TimeGroups.reco<-aov(Response ~ TimeGroup, data = Exp2_Long_Final.timegroups.Recognition)
anova(ANOVA.exp2.TimeGroups.reco)
summary.lm(ANOVA.exp2.TimeGroups.reco)
posthoc.exp1 <- glht(ANOVA.exp2.TimeGroups.reco, lsm(pairwise ~ TimeGroup))
summary(posthoc.exp1, test=adjusted("holm"))

ANOVA.exp2.TimeGroups.like<-aov(Response ~ TimeGroup, data = Exp2_Long_Final.timegroups.like)
anova(ANOVA.exp2.TimeGroups.like)
summary.lm(ANOVA.exp2.TimeGroups.like)
posthoc.exp1 <- glht(ANOVA.exp2.TimeGroups.like, lsm(pairwise ~ TimeGroup))
summary(posthoc.exp1, test=adjusted("holm"))

ANOVA.exp2.TimeGroups.quality<-aov(Response ~ TimeGroup, data = Exp2_Long_Final.timegroups.quality)
anova(ANOVA.exp2.TimeGroups.quality)
summary.lm(ANOVA.exp2.TimeGroups.quality)
posthoc.exp1 <- glht(ANOVA.exp2.TimeGroups.quality, lsm(pairwise ~ TimeGroup))
summary(posthoc.exp1, test=adjusted("holm"))

ANOVA.exp2.TimeGroups.vivid<-aov(Response ~ TimeGroup, data = Exp2_Long_Final.timegroups.vivid)
anova(ANOVA.exp2.TimeGroups.vivid)
summary.lm(ANOVA.exp2.TimeGroups.vivid)
posthoc.exp1 <- glht(ANOVA.exp2.TimeGroups.vivid, lsm(pairwise ~ TimeGroup))
summary(posthoc.exp1, test=adjusted("holm"))

# Figure for time groups
Sum_TimeGroup <- Exp2_Long_Final.timegroups.Recognition %>%
    group_by(YearReleaseSong,Song,TimeGroup) %>%
    summarize(total= sum(as.numeric(Response),na.rm=T),
              smean= mean(as.numeric(Response),na.rm=T),
              ssd= sd(as.numeric(Response),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))

# Figure4b<- ggplot(Sum_TimeGroup, aes(x=YearReleaseSong,y=smean, color= TimeGroup, shape=TimeGroup)) + 

span <- 4
fit <- with(Sum_TimeGroup, 
            ksmooth(YearReleaseSong, smean, kernel = "normal", bandwidth = span))
fit_y = as.numeric(fit$y)
Sum_TimeGroup_withKernel = cbind(Sum_TimeGroup, fit_y)
Sum_TimeGroup_withKernel$KernelSmoother = Sum_TimeGroup_withKernel$...11

Figure4b<- ggplot(Sum_TimeGroup_withKernel, aes(x=YearReleaseSong,y=smean , color=TimeGroup)) + 
    geom_point(size = 2) + 
    geom_line(aes(YearReleaseSong, KernelSmoother), color="#999999") +
    # geom_smooth(span = 0.3,se = FALSE,color="red",size=0.5) +
    labs(x="Year", y = "Music recognition") +
    theme_bw()

ii= c(1960, 1970, 1980, 1990, 2000, 2010)
Figure4b_final <- Figure4b + 
    geom_vline(xintercept = c(1975,1992,2001), linetype="dashed", 
               color = "#999999", size=0.5) +
    scale_color_viridis(discrete= T,direction = -1) +
    scale_x_continuous(breaks=ii) + 
    theme(axis.text.x = element_text(size=12), axis.title=element_text(size=12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.position = "none")
ggsave("Figure4b_exp2.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# Fgiure4c (optional): run mean and run sd
data_figure4c <- Exp2_Long_Final.timegroups.Recognition %>%   
    group_by(YearReleaseSong, Song) %>%
    dplyr::summarise(mean= mean(Response, na.rm = TRUE))

data_figure4c <- mutate(data_figure4c, TimeGroup = ifelse(YearReleaseSong %in% 1960:1975, "1960",
                                        ifelse(YearReleaseSong %in% 1976:1992, "1976",
                                        ifelse(YearReleaseSong %in% 1993:2001, "1993","2002"))))


k2= 10
data_figure4c$runmean=runmean(data_figure4c$mean, k2)
data_figure4c$runsd=runsd(data_figure4c$mean, k2, center=m2)

runmean_response_group <- data_figure4c %>%
    group_by(TimeGroup) %>%
    dplyr::summarize(total= sum(as.numeric(runmean),na.rm=T),
              smean= mean(as.numeric(runmean),na.rm=T),
              ssd= sd(as.numeric(runmean),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))

sd_response_group <- data_figure4c %>%
    group_by(TimeGroup) %>%
    dplyr::summarize(total= sum(as.numeric(runsd),na.rm=T),
              smean= mean(as.numeric(runsd),na.rm=T),
              ssd= sd(as.numeric(runsd),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))

p1= ggplot(runmean_response_group, aes(x=TimeGroup, y=smean)) + 
    geom_pointrange(aes(ymin=lower_ci, ymax=upper_ci)) +
    labs(x="", y = "Moving average")+
    theme_bw() 
p1 <- p1 + theme(axis.text.x = element_text(size=10), axis.title=element_text(size=12),
                 axis.title.x = element_blank())

p2= ggplot(sd_response_group, aes(x=TimeGroup, y=smean)) + 
    geom_pointrange(aes(ymin=lower_ci, ymax=upper_ci)) +
    labs(x="", y = "Moving standard deviation")+
    theme_bw() 
p2 <- p2 + theme(axis.text.x = element_text(size=10), axis.title=element_text(size=12),
          axis.title.x = element_blank())

library(cowplot)
Figure4c_exp2 <- plot_grid(p1, p2, labels = c("",""))

ggsave("Figure4c_exp2.pdf", width=12, height=7, units = c("cm"),
       dpi=300, device = "pdf")


# Analysis - qualitative: content analysis
exp2_content_analysis <- read_excel("data/exp2-content-analysis.xlsx") 

exp2_content_analysis <- mutate(exp2_content_analysis, 
                                          TimeGroup = ifelse(YEAR %in% 1960:1975, "1960-75",
                                                ifelse(YEAR %in% 1976:1992, "1976-92",
                                                ifelse(YEAR %in% 1993:2001, "1993-01","2002-14"))))

exp2_content_analysis$TimeGroup = as.factor(exp2_content_analysis$TimeGroup)
exp2_content_analysis <- exp2_content_analysis[c(4:7,24)]
data_exp2_content_long <- exp2_content_analysis %>% gather(condition,count,Parents:Media)
data_exp2_content_long$condition <- as.factor(data_exp2_content_long$condition)

data_exp2_content_long$condition <- factor(data_exp2_content_long$condition, levels = c("Parents", "Grandparents","Peers","Media"))

model.who.exp2<- aov(count ~condition* TimeGroup, data=data_exp2_content_long)
Anova(model.who.exp2, Type="III")
summary.lm(model.who.exp2)

summary(glht(model.who.exp2, linfct = mcp( TimeGroup = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(model.who.exp2, linfct = mcp(condition = "Tukey")), test = adjusted("holm")) #holm correction
TukeyHSD(model.who.exp2)

data_exp2_content_long_summary<- data_exp2_content_long %>%
    group_by(TimeGroup,condition) %>%
    summarize(total= sum(as.numeric(count),na.rm=T),
              smean= mean(as.numeric(count),na.rm=T),
              ssd= sd(as.numeric(count),na.rm=T),
              scount=n()) %>%
    mutate(PotentialComments = ifelse(TimeGroup == "1960-75", 1125,
                                      ifelse(TimeGroup == "1976-92", 1200,
                                             ifelse(TimeGroup == "1993-01", 600,900)))) %>%
    mutate(FinalMean = total/PotentialComments) %>%
    mutate(sd_FinalMean = sd(FinalMean)) %>%
    mutate(se_FinalMean = sd_FinalMean/sqrt(PotentialComments))

# 1960-75 - 15 years * 75 participants = 1125
# 1976-92 - 16 years * 75 participants = 1200
# 1993-01 - 8 years * 75 participants = 600
# 2002-14 - 12 years * 75 participants = 900

Figure5b <- ggplot(data=data_exp2_content_long_summary, aes(x=TimeGroup, y=FinalMean, fill= TimeGroup)) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=FinalMean - se_FinalMean, ymax=FinalMean+se_FinalMean), width=.2, position=position_dodge(.9)) +
    ylab("Mean proportion comments") +
    facet_wrap(~ condition, nrow=1) + 
    theme_bw()

Figure5b_final <- Figure5b + 
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    theme(axis.text.y=element_text(size=SIZE),axis.title.x = element_blank(),
                                   strip.text = element_text(size=SIZE),axis.text.x = element_text(size=10,angle = 90, hjust = 1),
                                   axis.title=element_text(size=12), legend.position =  "none",
                                   axis.ticks.x = element_blank())

ggsave("Figure2b_content_exp2.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

#not grouping
exp2_content_analysis_year <- read_excel("data/exp2-content-analysis.xlsx") 

exp2_content_analysis_year <- exp2_content_analysis_year[c(2,4:7)]
data_exp2_content_long <- exp2_content_analysis_year %>% gather(condition,count,Parents:Media)
data_exp2_content_long$condition <- as.factor(data_exp2_content_long$condition)

data_exp2_content_long$condition <- factor(data_exp2_content_long$condition, levels = c("Parents", "Grandparents","Peers","Media"))

data_exp2_content_long_summary<- data_exp2_content_long %>%
    group_by(YEAR,condition) %>%
    summarize(total= sum(as.numeric(count),na.rm=T),
              smean= mean(as.numeric(count),na.rm=T),
              ssd= sd(as.numeric(count),na.rm=T),
              scount=n()) %>%
    mutate(PotentialComments = 75) %>%
    mutate(FinalMean = total/PotentialComments) %>%
    mutate(sd_FinalMean = sd(FinalMean)) %>%
    mutate(se_FinalMean = sd_FinalMean/sqrt(PotentialComments))

Figure5_year <- ggplot(data=data_exp2_content_long_summary, aes(x=as.factor(YEAR), y=FinalMean, fill= as.factor(YEAR))) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=FinalMean - se_FinalMean, ymax=FinalMean+se_FinalMean), width=.2, position=position_dodge(.9)) +
    ylab("Mean proportion comments") +
    scale_x_discrete(breaks = ii2[c(T,F,F,F,F)]) +
    facet_wrap(~ condition, nrow=1) + 
    theme_bw()

ii2=1960:2014

Figure5_year_final = Figure5_year + 
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    scale_color_discrete(breaks = ii2[c(T,F)]) + 
    theme(axis.text.y=element_text(size=SIZE),axis.title.x = element_blank(),
                                   strip.text = element_text(size=SIZE),axis.text.x = element_text(size=10,angle = 90, hjust = 1),
                                   axis.title=element_text(size=12), legend.position =  "none",
                                   axis.ticks.x = element_blank())

ggsave("Figure2year_content_exp2.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# end