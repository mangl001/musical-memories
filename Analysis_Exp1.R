################################################################################
# File:     Analysis Script for Experiment 1
#
# Author:   Manuel Anglada-Tort
################################################################################
library(tidyverse)
library(multcomp)
library(lsmeans)
library(car)
library(mgcv)
library(mgcViz) #visuals for mgcv
library(cowplot)
library(readxl)
library(viridis)

set.seed(10)
SIZE = 12

# Functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
    lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
    upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

Make_figure1a <- function(data){
    data_sum <-data %>%
        group_by(Half_Decade) %>%
        summarize(total= sum(as.numeric(DV),na.rm=T),
                  smean= mean(as.numeric(DV),na.rm=T),
                  ssd= sd(as.numeric(DV),na.rm=T),
                  scount=n()) %>%
        mutate(se = ssd / sqrt(scount),
               lower_ci = lower_ci(smean, se, scount),
               upper_ci = upper_ci(smean, se, scount))
    
    figure <- ggplot(data_sum, aes(x=as.factor(Half_Decade), y=smean, group=1))+ 
        geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1, 
                      position=position_dodge(0.005)) +
        geom_line() +
        geom_point()+
        ylim(0,1)+
        labs(x="Time", y = "Music recognition")+
        theme_bw()
    final_figure <- figure +  theme(axis.text=element_text(size=12), axis.title=element_text(size=12),
                                                                        axis.title.x = element_blank())
    final_figure
}

Make_figure1b <- function(data){
    plot_data <- getViz(data)
    plot_data_gam <- plot(sm(plot_data,1)) +  
        l_ciPoly() + l_fitLine()  + 
        labs(x="Year", y = "Effect of time on music recognition")+
        theme_bw() 
    figure <- plot_data_gam + scale_x_discrete(name ="Time", 
                                              limits=c("1960","1965","1970",
                                                       "1975","1980","1985",
                                                       "1990","1995","2000","2005","2010")) + 
        theme(axis.text=element_text(size=12), axis.title=element_text(size=12),
              axis.title.x = element_blank())
    
    figure
}

Make_figure1c <- function(data, name_variable){
    plot_data <- getViz(data)
    plot_data2 <- plot(sm(plot_data,1)) +  
        l_ciPoly() + l_fitLine()  + l_fitContour(colors="blue")+
        ylab(paste0("Effect of time on ", name_variable)) + 
        ylim(-3.5,3.5) +
        theme_bw() 
    figure <- plot_data2 + scale_x_discrete(name ="Time", 
                                              limits=c("1960","1965","1970",
                                                       "1975","1980","1985",
                                                       "1990","1995","2000","2005","2010")) + 
        theme(axis.text=element_text(size=12), axis.title=element_text(size=1),
              axis.title.x = element_blank())
    
    figure
}

prepare_data_exp1 <- function(data_exp1_wide){
    data_exp1_long <- data_exp1_wide %>% gather(Condition,Response, -Participantno, -ResponseId, -DOB, 
                                                  -ageatstudy, -ageatstudyfract, -Gender, -Ethnic, -Country, -ParentsAge,-M,
                                                  -F, -GMM, -GFM, -GMF, -GFF, -FamilyClose,
                                                  -MT, -AE, -Sophisticated, -Intense, -Unpretentious, -Mellow, -Contemporary, 
                                                  -Testrecogpc_1, -Testratings_1, -Testratings_2, -Testratings_3, -Testwordcount, -Testwords) 
    data_exp1_separate <- data_exp1_long %>% separate(Condition,c("Half_Decade","Rating"),sep="\\.")
    data_exp1_separate$Half_Decade <- as.factor(data_exp1_separate$Half_Decade)
    data_exp1_separate$ResponseId <- as.factor(data_exp1_separate$ResponseId)
    data_exp1_separate$Response <- as.numeric(data_exp1_separate$Response)
    data_exp1_separate
    levels(data_exp1_separate$Half_Decade) <- c("1960","1965","1970","1975","1980","1985",
         "1990","1995","2000","2005","2010","NA","NA")
    data_exp1_separate
}


# Analysis - quantitative
data_exp1_wide <- read_excel("data/exp1-wide-all.xlsx") 
data_exp1_long <- prepare_data_exp1(data_exp1_wide)

## Replication analsyis using multiple t-tests
Exp1_recognition <- data_exp1_long %>% filter( Rating == "recognised") 
Exp1_recognition$DV <- (Exp1_recognition$Response/100)

Figure1a <- Make_figure1a(Exp1_recognition)
ggsave("Exp1_recognition_figure1a.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

### multiple comparisons using t-test
Exp1_recognition_ttest_data <-Exp1_recognition %>%
    group_by(Participantno, Half_Decade) %>%
    summarize(total= sum(as.numeric(DV),na.rm=T),
              smean= mean(as.numeric(DV),na.rm=T),
              ssd= sd(as.numeric(DV),na.rm=T),
              scount=n()) 

pairwise.t.test(as.numeric(Exp1_recognition_ttest_data$smean), as.factor(Exp1_recognition_ttest_data$Half_Decade),
                p.adjust.method = "BH", paired=TRUE) # Krumanshal replication

## GAM
Exp1_recognition_NoNAs = Exp1_recognition %>% drop_na(DV)
GAM.Exp1 = gam(as.numeric(Response)  ~ s(as.numeric(Half_Decade)) + s(ResponseId, bs = "re"), 
               data = Exp1_recognition_NoNAs, method="REML")
plot(GAM.Exp1)
summary(GAM.Exp1)
coef(GAM.Exp1)
gam.check(GAM.Exp1)

Figure1b <- Make_figure1b(GAM.Exp1x)
ggsave("Exp1_GAM_figure1b.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

### Individual differences
#### Musical Training (MT) and Active Engagement (AE)
GAM.Exp1_IDs = gam(
    as.numeric(Response)  ~ 
        s(as.numeric(Half_Decade)) + 
        s(as.numeric(MT)) +
        s(as.numeric(AE)) +
        s(ResponseId, bs = "re"), 
    data = Exp1_recognition_NoNAs, method="REML")
summary(GAM.Exp1_IDs)
plot(GAM.Exp1_IDs)

cor.AE= cor.test(as.numeric(Exp1_recognition$Response), as.numeric(Exp1_recognition$AE), method="pearson")
cor.AE
cor.MT= cor.test(as.numeric(Exp1_recognition$Response), as.numeric(Exp1_recognition$MT), method="pearson")
cor.MT

### Comparing models
GAM.Exp1.linear <- gam(as.numeric(DV) ~ as.numeric(Half_Decade), data=Exp1_recognition)
GAM.Exp1.nointerept <- gam(as.numeric(DV) ~ s(as.numeric(Half_Decade)), data=Exp1_recognition)
summary(GAM.Exp1.linear)
summary(GAM.Exp1.nointerept)

AIC(GAM.Exp1.linear)
AIC(GAM.Exp1.nointerept)

summary(GAM.Exp1.linear)$r.sq  # adjusted R squared
summary(GAM.Exp1.nointerept)$r.sq  # adjusted R squared

anova(GAM.Exp1.linear,GAM.Exp1.nointerept,  test="Chisq")

### Rating scales
Exp1_like <- data_exp1_long %>% filter( Rating == "like")
Exp1_quality <- data_exp1_long %>% filter( Rating == "quality")
Exp1_vivid <- data_exp1_long %>% filter( Rating == "vivid")

GAM.like = gam(Response  ~ s(as.numeric(Half_Decade)) + s(ResponseId, bs = "re"), data = Exp1_like, method="REML")
GAM.quality = gam(Response  ~ s(as.numeric(Half_Decade)) + s(ResponseId, bs = "re"), data = Exp1_quality, method="REML")
GAM.vivid = gam(Response  ~ s(as.numeric(Half_Decade)) + s(ResponseId, bs = "re"), data = Exp1_vivid, method="REML")

plot(GAM.like)
plot(GAM.quality)
plot(GAM.vivid)
summary(GAM.like)
summary(GAM.quality)
summary(GAM.vivid)

Figure1c_like <- Make_figure1c(GAM.like, "liking")
ggsave("Exp1_Figure1c_like.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")
Figure1c_quality <- Make_figure1c(GAM.quality, "quality")
ggsave("Exp1_Figure1c_quality.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")
Figure1c_vivid <- Make_figure1c(GAM.vivid, "vividness")
ggsave("Exp1_Figure1c_vivid.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")


# Analysis - qualitative: content analysis
data_exp1_content_pre <- read_excel("data/exp1-content-analysis.xlsx") 
data_exp1_content_pre$Half_Decade = as.factor(data_exp1_content_pre$Half_Decade)
prepare_content_data <- function(data) {
    levels(data$Half_Decade) <- c("1960-64","1965-69","1970-74","1975-79","1980-84","1985-89",
                                               "1990-94","1995-99","2000-04","2005-09","2010-14")
    data_exp1_content2 <- data[c(1,3:6)]
    data_exp1_content2_long <- data_exp1_content2 %>% gather(condition,count,Parents:Media)
    data_exp1_content2_long$condition <- as.factor(data_exp1_content2_long$condition)
    data_exp1_content2_long$condition <- factor(data_exp1_content2_long$condition, levels = c("Parents", "Grandparents","Peers","Media"))
    data_exp1_content2_long
}

data_exp1_content = prepare_content_data(data_exp1_content_pre)

model.who.exp1<- aov(count ~condition*Half_Decade, data=data_exp1_content)
Anova(model.who.exp1, Type="III")
summary.lm(model.who.exp1)

summary(glht(model.who.exp1, linfct = mcp(Half_Decade = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(model.who.exp1, linfct = mcp(condition = "Tukey")), test = adjusted("holm")) #holm correction
TukeyHSD(model.who.exp1)

data_exp1_content_summar<- data_exp1_content %>%
    group_by(Half_Decade,condition) %>%
    summarize(total= sum(as.numeric(count),na.rm=T),
              smean= mean(as.numeric(count),na.rm=T),
              ssd= sd(as.numeric(count),na.rm=T),
              scount=n()) %>%
    mutate(se = ssd / sqrt(scount),
           lower_ci = lower_ci(smean, se, scount),
           upper_ci = upper_ci(smean, se, scount))

Figure2b <- ggplot(data=data_exp1_content_summar, aes(x=Half_Decade, y=smean, fill= Half_Decade)) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=smean - se, ymax=smean+se), width=.2, position=position_dodge(.9)) +
    ylab("") +
    ylim(0, 0.75) +
    facet_wrap(~ condition,nrow =  1) + 
    theme_bw()

Figure2b_final <- Figure2b + 
    scale_fill_viridis(discrete = TRUE, direction = -1) +
    theme(axis.text.y=element_text(size=SIZE),axis.title.x = element_blank(),
                                 strip.text = element_text(size=SIZE),axis.text.x = element_text(size=9,angle = 90, hjust = 1),
                                 axis.title=element_text(size=SIZE), legend.position =  "none",
                                 axis.ticks.x = element_blank())


ggsave("Figure2b_content_exp1.pdf", width=15, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# end