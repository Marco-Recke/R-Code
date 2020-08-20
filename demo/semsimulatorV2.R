### Prepare environment #####
pacman::p_load(tidyverse,ggridges,ggbeeswarm)

### Set constants #####
mu<-100
sd_pop<-15
nrepl<-10^3
ns<-seq(5,200,5)
### Create output structure #####
means <- matrix(nrow=nrepl,ncol=length(ns))
sds <- matrix(nrow=nrepl,ncol=length(ns))
sems<- matrix(nrow=nrepl,ncol=length(ns))
# simul_out<-data.frame(run=1:nrepl,
#                       Mean3=NA,SD3=NA,SEM3=NA,
#                       Mean200=NA,SD200=NA,SEM200=NA)
### loop for simulation runs #####
for (n_i in seq_along(ns)){
  for(repl_i in 1:nrepl) {
    rawdata<-rnorm(n = ns[n_i],mean = mu,sd = sd_pop)
    means[repl_i,n_i]<-mean(rawdata)
    sds[repl_i,n_i]<-sd(rawdata)
    sems[repl_i,n_i]<-sd(rawdata)/sqrt(ns[n_i])
  }
}
### Analyze simulation #####
simul_out <- tibble(
  rep_i=rep(1:nrepl,length(ns)),
  n = rep(ns,each=nrepl),
  Mean=as.vector(means),
  SD=as.vector(sds),
  SEM=as.vector(sems))
simul_out %>% filter(n==5) %>% 
  summarise(`mean of mean`=mean(Mean),
            `SD of mean`=sd(Mean),
            `mean of SEM`=mean(SEM),
            `median of SEM`=median(SEM))
simul_out %>% filter(n==200) %>% 
  summarise(`mean of mean`=mean(Mean),
            `SD of mean`=sd(Mean),
            `mean of SEM`=mean(SEM),
            `median of SEM`=median(SEM))
plotdata <- gather(data = simul_out,
                   key = measure,
                   value = Estimate,
                   Mean:SEM)
### Graphical output #####
plotdata_line <- plotdata%>% #mutate(n=factor(n))%>% 
  group_by(n,measure) %>% 
  summarize(
    Median=median(Estimate),
    max=quantile(Estimate,probs = .975),
    min=quantile(Estimate,probs = .025))
plotdata %>% ggplot(aes(x=n))+
  geom_ribbon(data=plotdata_line,
              aes(n,ymax=max,ymin=min),
              fill='yellow',alpha=.5)+
  geom_point(aes(y=Estimate),alpha=.1)+
  scale_x_continuous(breaks=seq(0,200,10))+
  facet_grid(measure~.,scales='free')
plotdata_line %>% #mutate(n=factor(n)) %>% 
  ggplot(aes(x=n))+
  # geom_line(data = plotdata_line,aes(n/5,y=max),group='all')+
  # geom_line(data = plotdata_line,aes(n/5, y=min),group='all')+
  # geom_violin(aes(x=factor(n),y = Estimate))+
  geom_ribbon(aes(ymax=max,ymin=min),
              fill='yellow',alpha=.5)+
  geom_violin(data=plotdata,aes(group=factor(n),y = Estimate))+
  geom_line(aes(y=Median),
            group='all',color='red')+
  scale_x_continuous(breaks=seq(0,200,10))+
  xlab('sample size')+
  labs(caption = 'Line shows median of estimates, area is 95% range')+
  facet_grid(measure~.,scales='free',switch = 'y')
plotdata %>% filter(n %in% c(seq(5,50,5),seq(60,100,10))) %>% 
  ggplot(aes(x=Estimate,y = factor(n)))+
  ylab('sample size')+
  scale_x_continuous(breaks=seq(0,200,10))+
  geom_density_ridges(alpha=.5)+
  facet_grid(.~measure,scales='free')

simul_out %>% filter(n==5) %>% pull(Mean) %>% sd()
simul_out %>% filter(n==50) %>% pull(Mean) %>% sd()
