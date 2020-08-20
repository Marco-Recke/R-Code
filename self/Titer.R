pacman::p_load(tidyverse, readxl, ggbeeswarm)
rawdata <- read_xlsx(
  'c:/Users/abusj/Downloads/Uebungsdaten anti-F8-IgG.xlsx',
  sheet = 2,skip = 1) %>% 
  pivot_longer(cols = everything(),
               names_to='Vector Treatment',
               values_to = 'Titer') %>% 
  filter(!is.na(Titer))
ggplot(rawdata,aes(x = `Vector Treatment`,y = Titer))+
  geom_boxplot(outlier.alpha = 0)+
  geom_beeswarm(alpha=.3, cex=2, size=2)+
  scale_y_log10()
rawdata$Responder <- 'no'
rawdata$Responder[which(rawdata$Titer>1000)] <- 'yes'
rawdata$Responder <- ifelse(test = rawdata$Titer>1000,
                            yes = 'yes',no = 'no')
#cut()
# rawdata$RespTF <- rawdata$Titer>1000
rawdata$RespTF <- factor(rawdata$Titer>1000,
                         levels = c(F,T),
                         labels = c('nonresponder', 
                                    'responder'))
table(rawdata$`Vector Treatment`,rawdata$Responder)
rawdata %>% count(`Vector Treatment`, Responder)
chi_out <- 
  chisq.test(rawdata$`Vector Treatment`,rawdata$Responder)
min(chi_out$expected)<5
fisher_out <- 
  fisher.test(rawdata$`Vector Treatment`,rawdata$Responder)
ggplot(rawdata,aes(Titer, fill=`Vector Treatment`))+
  geom_density(alpha=.5)+
  scale_x_log10()
