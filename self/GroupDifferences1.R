rm(list=ls())
while (dev.cur()!=1) {dev.off()}
setwd(here::here())
pacman::p_load(plotrix,tidyverse, wrappedtools,
               coin,ggsignif, patchwork)
load('Data/testdata.RData')

##### Test for normal distribution -----
ks.test(rawdata$`Size (cm)`,
        'pnorm',
        mean(rawdata$`Size (cm)`,na.rm=T),
        sd(rawdata$`Size (cm)`,na.rm=T),
        exact=F)
ksnormal(rawdata$`Weight (kg)`)

shapiro.test(rawdata$`Size (cm)`)

ggplot(rawdata,aes(x = `Size (cm)`,fill=sex))+
  geom_density(alpha=.5)
ggplot(rawdata,aes(x = `Size (cm)`))+
  geom_line(stat='density')
# ksnormal<-function(ksdata)
# {
#    ksout<-ks.test(x=ksdata,'pnorm',mean(ksdata,na.rm=T),
#                   sd(ksdata,na.rm=T),exact=F)
#    return(ksout)
# }

(ksout<-ksnormal(ksdata = rawdata$`Size (cm)`))
ksout$p.value
# test, if log-normal (->then transform)
if (ksnormal(rawdata$`Size (cm)`)$p.value<=.05 & 
    ksnormal(log(rawdata$`Size (cm)`))$p.value>.05) {
  print('lognormal')
  # create new log-transformed variable or just transform...
} else {
  print('not lognormal')
}


##### quantitative measures with gaussian distribution -----
ggplot(rawdata,aes(x=sex,y=`Size (cm)`))+
  geom_point(size=3,position=position_jitter(width=.2))+
  stat_summary(color='red',size=1.2,alpha=.7, 
               fun.data='mean_se',fun.args=list(mult=2))+
  ylab('size (mean \u00B1 2*SEM)')
t.test(rawdata$`Size (cm)`[which(rawdata$sex=='f')],
       rawdata$`Size (cm)`[which(rawdata$sex=='m')])
(tOut<-t.test(rawdata$`Size (cm)`~rawdata$sex))
tOut$p.value

# equal variances assumption?
(vartestOut<-var.test(rawdata$`Size (cm)`~rawdata$sex))
(tOut<-t.test(rawdata$`Size (cm)`~rawdata$sex,
              var.equal = vartestOut$p.value>.05))
(tOut<-
    t.test(rawdata$`Size (cm)`~rawdata$sex,
           var.equal=var.test(
             rawdata$`Size (cm)`~rawdata$sex)$p.value>.05))

by(data=rawdata$`Size (cm)`,INDICES=rawdata$sex,
   FUN=mean,na.rm=T)
rawdata %>% group_by(sex) %>% 
  summarize(Mean=mean(`Size (cm)`,na.rm=T))

apply(X=rawdata[,quantvars$index],MARGIN=2,
      FUN=meansd,roundDig=2)

rawdata %>% select(quantvars$index) %>% 
  map_df(meansd) %>% 
  pivot_longer(cols = everything(),
               names_to = 'Variable',
               values_to = 'Desc')
print(c(mean(rawdata$sysBP_V0,na.rm=T),
        mean(rawdata$sysBP_V2,na.rm=T)))

t.test(rawdata$sysBP_V0,
       rawdata$sysBP_V2,
       alternative='greater', # x>y
       paired=T)  #pairwise t-test, within subject
t.test(rawdata$sysBP_V0,
       rawdata$sysBP_V2,
       # alternative='greater', # x>y
       paired=T)$p.value/2  #pairwise t-test, within subject

##### ordinal data -----
ordvars$names
ggplot(rawdata,aes(iron_lab))+
  geom_density()
by(data = rawdata[[ordvars$index[1]]], 
   INDICES = rawdata$sex,FUN = median_quart)

ggplot(rawdata,aes(sex,ptt_lab))+
  geom_boxplot()
(uOut<-wilcox.test(
  rawdata[[ordvars$index[1]]]~rawdata$sex,exact=F))
uOut$p.value
#  coin::wilcox_test
(uOut2<-wilcox_test(ptt_lab~as.factor(sex),
                    data=rawdata))
pvalue(uOut2) #no list-object, but methods to extract infos like p
wilcox.test(ptt_lab~sex,exact=F,correct=F,
            data=rawdata)
wilcox.test(x=rawdata$sysBP_V0,y=rawdata$sysBP_V2,
            exact=F,
            correct=T,paired=T)

##### categorial data -----
qualvars$names
(crosstab<-table(rawdata$sex,rawdata$testmedication))
chisq.test(crosstab,simulate.p.value=T,B=10^5)  #empirical p-value

chisq.test(table(rawdata$sex,rawdata$NYHA_V1))  #based on table
chisq.test(x=rawdata$sex,y=rawdata$NYHA_V1,
           simulate.p.value=T,B=10^5)  #based on rawdata

(crosstab1<-table(rawdata$sex,
                  rawdata$`Weight (kg)`<=
                    median(rawdata$`Weight (kg)`)))
prop.table(crosstab1)
round(prop.table(crosstab1)*100)
round(prop.table(crosstab1,margin=2)*100) #% per column
round(prop.table(crosstab1,margin=1)*100) #% per row

(tabletestOut<-chisq.test(crosstab1,simulate.p.value=T,
                          B=10^5))
tabletestOut$p.value
tabletestOut$expected
tabletestOut$observed
tabletestOut$statistic
# if minimum(expected<5) then Fishers exact test
if (min(tabletestOut$expected)<5) {
  tabletestOut<-fisher.test(crosstab1)
}
tabletestOut$p.value 





##### report -----
report_qv %<>% 
  mutate(
    Variable=factor(Variable, levels=quantvars$names),
    female='', male='',`p sexdiff`="") %>% 
  arrange(Variable)
for (var_i in 1:quantvars$count) {
  report_qv[var_i,c('female','male')] <- 
    meansd(rawdata[[quantvars$index[var_i]]],
           groupvar = rawdata$sex,
           .n = T,rangesep = '  ',
           range = T) %>% 
    as.list()
  evOut<-var.test((rawdata %>% pull(quantvars$index[var_i]))~
                    rawdata$sex)
  tOut<-t.test((rawdata %>% pull(quantvars$index[var_i]))~
                 rawdata$sex,
               var.equal=evOut$p.value>.05)
  report_qv$`p sexdiff`[var_i] <- formatP(tOut$p.value)
}

compare2numvars(data = rawdata,testvars = quantvars$names,
                groupvar = "testmedication",gaussian = T)

report_ov

report_ov %<>% 
  mutate(
    Variable=factor(Variable, levels=ordvars$names),
    female='', male='',`p sexdiff`="") %>% 
  arrange(Variable)
for (var_i in seq_len(ordvars$count)) {
  report_ov[var_i,c('female','male')] <- 
    # by(rawdata[[ordvars$names[var_i]]],
    #    rawdata$sex,quantile,probs=c(.25,.75),na.rm=T)
    median_quart(rawdata[[ordvars$index[var_i]]],
                 groupvar = rawdata$sex,
                 .n = T,rangesep = '  ',
                 range = T) %>% 
    as.list()
  wOut<-wilcox.test((rawdata %>% pull(ordvars$index[var_i]))~
                      rawdata$sex,exact=F)
  report_ov$`p sexdiff`[var_i] <- formatP(wOut$p.value)
  plottmp <- 
    rawdata %>% ggplot(aes_string('sex',ordvars$bticked[var_i]))+
    geom_boxplot()+
    geom_signif(comparisons = list(c(1,2)),
                annotations = paste0('p ',
                                     formatP(wOut$p.value,
                                             pretext = T)))+
    scale_y_continuous(expand = expansion(mult = c(.1,.15)))
  print(plottmp)
}

compare2numvars(data = rawdata,testvars = ordvars$names,
                groupvar = "sex",gaussian = F)

report_cat
groupvar <- 'sex'
report_cat %<>% 
  mutate(
    Variable=factor(Variable, levels=qualvars$names),
    female='', male='',`p sexdiff`="") %>% 
  arrange(Variable)

for(var_i in seq_along(qualvars$names)){
  if(qualvars$names[var_i]!=groupvar){
    report_cat[var_i,c('female','male')] <- 
      cat_desc_stats(rawdata[[qualvars$index[var_i]]],
                     groupvar = rawdata$sex,
                     singleline = T,trenner = ' / ',
                     return_level = F) %>%
      as.list()
    test_out <- fisher.test(rawdata[[qualvars$index[var_i]]],
                            rawdata[[groupvar]])
    report_cat$`p sexdiff`[var_i] <- formatP(test_out$p.value)
    
    p1 <- ggplot(rawdata,aes_string(groupvar,
                                    fill=qualvars$bticked[var_i]))+
      geom_bar()+
      geom_signif(
        aes(y=max(table(sex))+2),
        comparisons = list(c(1,2)),
        annotations = paste0('p ',
                             formatP(test_out$p.value,
                                     pretext = T)))+
      scale_y_continuous(expand = expansion(mult = c(.1,.15)))
    p2 <- ggplot(rawdata,aes_string(groupvar,
                                    fill=qualvars$bticked[var_i]))+
      geom_bar(position='fill')+
      geom_signif(
        aes(y=1.05),
        comparisons = list(c(1,2)),
        annotations = paste0('p ',
                             formatP(test_out$p.value,
                                     pretext = T)))+
      scale_y_continuous(expand = expansion(mult = c(.1,.15)),
                         labels = scales::percent)
    p3 <- ggplot(rawdata %>% 
                   filter(!is.na(!!sym(qualvars$names[var_i]))),
                 aes_string(groupvar,
                            fill=qualvars$bticked[var_i]))+
      geom_bar(position='fill')+
      geom_signif(
        aes(y=1.05),
        comparisons = list(c(1,2)),
        annotations = paste0('p ',
                             formatP(test_out$p.value,
                                     pretext = T)))+
      scale_y_continuous(expand = expansion(mult = c(.1,.15)),
                         labels = scales::percent)
    print(p1/(p2+p3))
    print(p1/(p2+p3)+plot_layout(guides = "collect") & 
            theme(legend.position = 'bottom'))
  }  
}
