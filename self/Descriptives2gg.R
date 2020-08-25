rm(list=ls())
#gc()
while (dev.cur()!=1) {dev.off()}
pacman::p_load(plotrix,tidyverse, wrappedtools,
               readxl,ggbeeswarm,#patchwork,
               here)
pacman::p_unload(readxl,here)
setwd(here::here())
rawdata <- #as.data.frame(
  readxl::read_excel('Data/Medtest_e.xlsx') %>% 
  rename('Weight (kg)'=weight,
         'Size (cm)'=size,
         'BMI (kg/m\u00B2)'=BMI)

##### Visualisation -----
# windows(12,8)
# x11(8,4)
# dev.off()
colnames(rawdata)
cn()
ggplot(rawdata)+
   geom_boxplot(aes(x='Größe',y=`Size (cm)`))+
  geom_boxplot(aes('Gewicht',`Weight (kg)`))+
   xlab(label='')+
  ylab('')
rawdata %>% pivot_longer(
  cols=c(`Size (cm)`,`Weight (kg)`),
  names_to = 'Kennwert',
  values_to = 'Messwert') %>% 
  ggplot(aes(x = Kennwert,y = Messwert))+
  geom_boxplot()

#das selbe für sysBP V0 und V2...


ggplot(rawdata,aes(x='',y=`Size (cm)`))+
  geom_boxplot()+
  facet_grid(~sex,margins = T)+
  xlab(NULL)
ggplot(rawdata,aes(y=`Weight (kg)`,x=`Size (cm)`))+
  geom_point(aes(color=sex,shape=sex),size=3)+
  geom_smooth(method='lm',se=T)+
   scale_color_manual(values = c('pink','blue'))+
  scale_shape_manual(values = c('\u2640','\u2642'))

ggplot(rawdata,aes(sex,`Weight (kg)`))+
  geom_beeswarm(cex = 2)


temp<-rawdata %>% group_by(sex) %>% 
  summarize(mean_height=mean(`Size (cm)`,na.rm = T))
ggplot(temp,aes(sex,mean_height))+
  geom_col(aes(fill=sex))+
  geom_label(aes(y=10,label=round(mean_height)))+
  scale_fill_viridis_d()
rawdata %>% group_by(sex) %>% 
  summarize(mean_height=mean(`Size (cm)`,na.rm = T),
            se=SEM(`Size (cm)`)) %>% 
  ggplot(aes(sex,mean_height))+
  geom_col(aes(fill=sex))+
  geom_label(aes(label=round(mean_height)),
             y=10)+
  scale_fill_viridis_d()+
  geom_errorbar(aes(ymax=mean_height+se,
                    ymin=mean_height-se),
                width=.3,size=1.5)

rawdata %>% 
  ggplot(aes(sex,`Size (cm)`))+
  stat_summary(fun.data=mean_cl_normal)



quantvars<-c(6:13,18:19) # for mean+-sd
quantvars <- FindVars(varnames = c('Si','We','BP','lv'))

ordvars <- FindVars('lab')
qualvars <- FindVars(c('test','sex$','NY'))
# for(var_i in qualvars$index){
#   rawdata[[var_i]] <- factor(rawdata[[var_i]])
# }
rawdata[qualvars$index] %<>% map(as.factor)

for (plotvar_i in quantvars$bticked) {
  print(ggplot(rawdata,
               aes_string('testmedication',
                          plotvar_i))+
          geom_boxplot())
}



##### descriptive stats -----
# count
table(rawdata$sex)
prop.table(table(rawdata$sex))
roundR(prop.table(table(rawdata$sex))*100)

paste0(table(rawdata$sex),' (',
       round(prop.table(table(rawdata$sex))*100),'%)')

cat_desc_stats(rawdata$sex)
cat_desc_stats(rawdata$sex,
               groupvar = rawdata$testmedication,
               ndigit = 2)


cat_desc_stats(rawdata$sex,trenner = ' / ',singleline = T,
               ndigit = 2,.german = T)

#mean
mean(rawdata$`Size (cm)`,na.rm=T)
ndig<-2  # define no of digits
round(mean(rawdata$`Size (cm)`,
           na.rm=T),digits=ndig)
roundR(mean(rawdata$`Size (cm)`,
           na.rm=T),level = ndig)

#sd
round(sd(rawdata$`Size (cm)`,na.rm=T),ndig)
roundR(sd(rawdata$`Size (cm)`,na.rm=T),ndig)

paste(roundR(mean(rawdata$`Size (cm)`,na.rm=T)),
      roundR(sd(rawdata$`Size (cm)`,na.rm=T)),
      sep=' \u00b1 ')
meansd(rawdata$`Size (cm)`)
meansd(rawdata$`Size (cm)`, groupvar = rawdata$sex)

#sem (library plotrix)
round(std.error(rawdata$`Size (cm)`,na.rm=T),ndig)

wrappedtools::meanse(rawdata$`Size (cm)`,roundDig = 3)
DescTools::MeanSE(rawdata$`Size (cm)`)
wrappedtools::SEM(rawdata$`Size (cm)`)

#median
round(median(rawdata$`Size (cm)`,na.rm=T),digits = ndig)

#quantiles
round(quantile(rawdata$`Size (cm)`,na.rm=T),digits = ndig)
round(quantile(rawdata$`Size (cm)`,probs=c(.05,.95),
               na.rm=T),ndig)
median_quart(rawdata$`Size (cm)`,qtype = 7)
DescTools::MedianCI(rawdata$`Size (cm)`)

### reporting descriptive stats
report_qv<-tibble(Variable=quantvars$names,
                   nCases=NA,'mean\u00B1sd'='&nbsp;')
ndig<-2
for (var_i in seq_len(quantvars$count)) {
  report_qv$`mean±sd`[var_i]<-#paste0(
         # round(mean(rawdata[,reportvars[var_i]],
         #            na.rm=T),ndig),
         #           '\u00B1',
         # round(sd(rawdata[,reportvars[var_i]],
         #          na.rm=T),ndig))
        meansd(rawdata[[quantvars$index[var_i]]],
               roundDig = ndig)
  report_qv$nCases[var_i]<-length(na.omit(
    rawdata[[quantvars$index[var_i]]]))
}

report_qv <- rawdata %>% 
  pivot_longer(cols = quantvars$index,
               names_to='Variable',values_to='Wert') %>% 
  filter(!is.na(Wert)) %>% group_by(Variable) %>% 
  summarize(n=n(),`mean±sd`=meansd(Wert))

rawdata %>% 
  summarise_at(.vars =quantvars$names,
               list(~meansd(.))) %>% 
  pivot_longer(everything())

rawdata %>% 
  summarize(across(quantvars$names,
                   list(n=~length(na.omit(.x)),
                        Desc=meansd),
                   .names = "{col}__{fn}")) %>% 
  mutate_if(.predicate = is.numeric,.funs = as.character) %>% 
  pivot_longer(everything(), names_to=c('Variable','what'),
               names_pattern='(.*)__(.+)') %>% 
  pivot_wider(names_from = what,values_from=value)

report_ov <- rawdata %>% 
  pivot_longer(cols = ordvars$index,
               names_to='Variable',values_to='Wert') %>% 
  filter(!is.na(Wert)) %>% group_by(Variable) %>% 
  summarize(n=n(),`Median [Quartile]`=median_quart(Wert))


report_cat<-tibble(Variable=qualvars$names,
                   Groups=NA,
                  nCases=NA,Frequency='&nbsp;')
for (var_i in seq_len(qualvars$count)) {
     cat_desc_stats_out <- cat_desc_stats(
       rawdata[[qualvars$index[var_i]]],
                    trenner = ' / ',singleline = T)
     report_cat$Groups[var_i] <- cat_desc_stats_out$level
     report_cat$Frequency[var_i] <- cat_desc_stats_out$freq$desc
     report_cat$nCases[var_i]<-length(na.omit(
       rawdata[[qualvars$index[var_i]]]))}



save(rawdata,quantvars,ordvars,qualvars,
     list = ls(pattern = 'report'),
     file='Data/testdata.RData')
