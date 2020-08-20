pacman::p_load(tidyverse, readxl, ggbeeswarm)
rawdata <- read_xlsx(
  "/home/marco/Dokumente/Programmierung/R/data/Uebungsdaten_anti-F8-IgG.xlsx",
  sheet =2, skip = 1) %>% 
  pivot_longer(cols=#everything(),
                 c(`HLP/hTrIa-V3 1st`, `hTr.Ia-F5-N6 1st`),
                 names_to = 'Vector Treatment',
                 values_to = 'Titer') %>% 
  filter(!is.na(Titer))
# Variable "Vector Treatment" mit Backtick

ggplot(rawdata,aes(x =`Vector Treatment`,y= Titer))+
  # alpha Null Ausreisser durchsichtig
  geom_boxplot(outlier.alpha = 0)+
  geom_beeswarm(alpha=.3, cex=2, size =2)+
  scale_y_log10()
# Alternativen:
rawdata$Responder <- 'no'
rawdata$Responder[which(rawdata$Titer>1000)] <- 'yes'
rawdata$Responder <- ifelse(test =rawdata$Titer>1000, 
                             yes = 'yes', no ='no')
# würde auch gehen: cut()
# würde auch gehen
# rawdata$RespTF <-  factor(rawdata$Titer>1000
rawdata$RespTF <-  factor(rawdata$Titer>1000,
                          levels = c(F,T),
                          labels = c('nonresponder', 'responder'))
table(rawdata$`Vector Treatment`, rawdata$Responder)
rawdata %>% count(`Vector treatment`, RespTF,.drop = F)
chi_out <- 
  chisq.test(rawdata$`Vector Treatment`, rawdata$Responder)
  min(chi_out$expected)<5
fisher_out <-   
  fisher.test(rawdata$`Vector Treatment`, rawdata$Responder)
ggplot(rawdata,aes(Titer,fill=`Vector Treatment`))+
  geom_density(alpha=.5)+
  scale_x_log10()
ggplot(rawdata,aes(Titer,fill=`Vector Treatment`))+
  geom_histogram()+
  scale_x_log10()+
  facet_grid(`Vector Treatment` ~ Responder)

# Normalverteilung mit Mittelwert = Mittelwert der Daten
# Normalverteilung mit SD = SD der Daten
# exact F da wrappedtools auch
ks.test(rawdata$Titer,pnorm,mean=mean(rawdata$Titer),
        sd=sd(rawdata$Titer),exact=F)

wrappedtools::ksnormal(
  (rawdata$Titer) %>% 
    filter(`Vector Treatment` == `hTr.Ia-F5-N6 1st`) %>% 
    pull(Titer))

# Std
vartest_out <- var.test(Titer~ `Vector Treatment`,
                    data = rawdata)

ttest_out <-  t.test(Titer~ `Vector Treatment`,
                     data = rawdata,
              var.eqal= vartest_out$p.Value> .05)

rawdata %>%  
  filter(`Vector Treatment` == `hTr.Ia-F5-N6 1st`) %>% 
  pull(Titer) %>%  mean()

by(rawdata$Titer, INDICES = rawdata$`Vector Treatment`,
   FFUN = mean)
by(rawdata$Titer, rawdata$`Vector Treatment`,median)

# tidy
rawdata %>% group_by(`Vector Treatment`) %>% 
  summarise(Mean=mean(Titer), Median=median(Titer))
















      