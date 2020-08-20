setwd(here::here())
#source('F:/Aktenschrank/Analysen/R/teacheRtools.R')
getwd()

pacman::p_load(tidyverse,plotrix,grid,gridExtra,car,
               ggsci,ggsignif, ggthemes, ggridges,
               gganimate,ggforce,
               survival, survminer,
               ggdendro, ggbeeswarm,
               rpart,rpart.plot,
               gapminder)

#sample data
head(diamonds)
head(mtcars)

ggplot(data=diamonds,aes(x=clarity))+
   geom_bar()

#define aesthetics
ggplot(data=diamonds,aes(x=clarity,fill=cut))+
   geom_bar()

#aesthetics outside aes
ggplot(data=diamonds,aes(x=clarity))+
   geom_bar(fill='gold')

## gold !innerhalb aes (vorgaben außerhalb von aes)
ggplot(data=diamonds,aes(x=clarity))+
   geom_bar(aes(fill='gold')) #should be outside aes!

#fill/color unterschied 
# color = Rahmenfarbe
ggplot(data=diamonds,aes(x=clarity,color=cut))+
   geom_bar()
ggplot(data=mtcars,aes(factor(cyl),fill=factor(cyl)))+
   geom_bar(color='black')
# fehler
ggplot(data=mtcars,aes(factor(cyl),color=factor(cyl)))+
   geom_bar()

help(points)

#other color systems
(plottemp <- ggplot(data=diamonds,aes(x=clarity,fill=cut))+
      geom_bar())
plottemp + scale_fill_brewer(palette='Set2') #in-built
plottemp + scale_fill_grey(name = "cut of\ndiamonds")
plottemp + scale_fill_grey(name = "cut of\ndiamonds") +
   theme_bw()+
   theme(panel.grid.minor.x=element_blank(),
         panel.grid.major.x = element_blank())

# ggsci ####
plottemp+scale_fill_lancet()
plottemp+scale_fill_jama()
plottemp+scale_fill_startrek()

#save ggplots
ggsave('Graphs/ggtestplot.png',width=20,height=20,
       units='cm',dpi=150)
# alternative:
# png('ggtestplot.png',width = 20,height = 20,units = 'cm',res = 150)
# plottemp
# dev.off()
# scatterplot

#other geoms
ggplot(data=mtcars,aes(x = wt,y = mpg))+
   geom_point()

ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot() #default 1.5 IQR
ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot(coef=3)
ggplot(mtcars,aes(x = factor(gear),y = wt,fill=factor(cyl)))+
   geom_boxplot(coef=3)

ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot(coef=3)+
   geom_point()
ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot(coef=3)+
   geom_point(position = position_jitter(width = .1))
ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot(coef=3)+
   geom_dotplot(alpha=.7,
                binaxis = 'y',stackdir = 'center',
                stackratio = .9,dotsize = .6)
ggplot(mtcars,aes(x = factor(gear),y = wt))+
   geom_boxplot(coef=3)+
   ggbeeswarm::geom_beeswarm(cex = 2,size=3,alpha=.5)

#aesthetics again (finetuning)
ggplot(data=mtcars,aes(wt, mpg,color=qsec))+
   geom_point(size=4) #outside aes!

ggplot(data=mtcars,aes(wt, mpg,color=qsec, size=carb))+
   geom_point()
ggplot(data=mtcars,aes(wt, mpg,color=qsec, size=carb))+
   scale_color_gradient(low='darkred',high='blue')+
   geom_point()
ggplot(data=mtcars,aes(wt, mpg,color=qsec, size=carb))+
   scale_color_gradient2(low='red',high='yellow',
                         mid='blue',
                         limits=c(13,23),midpoint=18)+
   geom_point()

# use different aesthetic mappings
ggplot(data=mtcars,
       aes(wt, mpg,size=qsec, shape=factor(cyl)))+
   geom_point()

ggplot(data=mtcars,aes(carb, mpg,color=qsec, size=wt))+
   geom_point(shape=2)

#location, location,position
p<-ggplot(data=diamonds,aes(clarity,fill=cut))
p+geom_bar(position="stack")
p+geom_bar(position="dodge")
p+geom_bar(position="fill")
p+geom_bar(position="fill")+
   scale_y_continuous('Frequency (%)',
                      breaks=seq(0,1,.2),
                      labels=seq(0,100,20))
p+geom_bar(position="fill")+
   scale_y_continuous('Frequency',
                      breaks=seq(0,1,.1),
                      labels=scales::percent)
p+geom_bar(position="identity")
p+geom_bar(position="identity",alpha=.5)
ggplot(data=diamonds,aes(clarity,color=cut, group=cut))+
   geom_freqpoly(stat='count',position="identity",lwd=1.5)+
   geom_point(stat='count',size=5)+
   scale_color_manual(values = c('green','lightblue',
                        'darkblue','red',
                        'darkgreen'))

#layer/order / computed geoms
ggplot(data=mtcars,aes(wt, mpg))+
   geom_point(size=4)+
   geom_smooth(size=3)+
   geom_smooth(data=mtcars %>% filter(wt>3),
               method='lm',size=3, color='red')


ggplot(data=mtcars,aes(wt, mpg))+
   geom_smooth(size=3)+
   geom_point(size=4)
ggplot(data=mtcars,aes(wt, mpg))+
   geom_smooth(size=3,color='red')+
   geom_smooth(method='lm',size=3)+
   geom_point(size=4)
ggplot(data=mtcars,aes(wt, mpg,
                       color=factor(cyl)))+
   geom_point(size=4)+
   geom_smooth(method='lm',size=1)
ggplot(data=mtcars,aes(wt, mpg,
                       color=factor(cyl),
                       shape=factor(am)))+
   geom_point(size=2)+
   geom_smooth(method='lm',size=1,se=F)

#? lm for all?
ggplot(data=mtcars,aes(wt, mpg))+
   geom_smooth(size=1,color='black',fill='yellow')+
   geom_point(size=3,aes(color=factor(cyl),shape=factor(am))) #aes for geom only

# facet_wrap / facet_grid
(p.tmp <- ggplot(mtcars, aes(mpg, wt)) + geom_point())
p.tmp + facet_wrap(~cyl)
p.tmp + facet_wrap(~cyl, ncol=2)
p.tmp + facet_grid(gear~cyl,labeller=label_both,margins='gear')
p.tmp + geom_smooth()+
   facet_grid(gear~cyl, labeller=label_both,margins=T)
p.tmp + facet_wrap(~cyl+gear,labeller=label_both) # empty combination is dropped

# controlling scales in facets (default: scales="fixed")
p.tmp + facet_grid(gear~cyl, scales="fixed")
p.tmp + facet_grid(gear~cyl, scales="free")
p.tmp + facet_grid(gear~cyl, scales="free_x")
p.tmp + facet_grid(gear~cyl, scales="free_y")

# summaries ####
(plottemp <- ggplot(mtcars,aes(factor(gear),mpg))+
    geom_point())
plottemp+stat_summary(color='red')
plottemp+stat_summary(fun.data='mean_cl_boot',color='red')+
   ylab('mpg (mean \u00b1 95% CI)')
plottemp+stat_summary(fun.data='median_hilow',color='red')+
   ylab('mpg median \u00b1 95%')

# ggsign ####
p <- round(wilcox.test(mtcars$mpg~mtcars$am)$p.value,5)
(plottemp <- ggplot(mtcars,aes(as.factor(am),mpg))+
   geom_boxplot())
plottemp+geom_signif(
   comparisons=list(c(1,2)),
   # aes(y=0), 
   textsize = 3, vjust = .0, 
   y_position=max(mtcars$mpg+3),
   # annotations=paste0('p = ', p),
   annotations=wrappedtools::markSign(p),
   tip_length=.01)

plottemp + geom_signif(
   comparisons=list(1:2),vjust=-1)+
   scale_y_continuous(expand = expansion(mult=c(0.05,.2)))

old <- theme_set(theme_wsj())
theme_set(theme_wsj())
ggplot(data=diamonds,aes(x=clarity,fill=cut))+
   geom_bar()
theme_update(legend.position="bottom",
             axis.text=element_text(colour = "darkblue",
                                    size=12),
             axis.text.x=element_text(vjust=.4,angle=45),
             axis.title=element_text(size=15),
             plot.margin=unit(c(3,4,.5,3),'lines'),    #o,r,u,l
             axis.title.y=element_text(vjust=0.4,angle=45),
             legend.key.size=unit(2.5,'lines'),
             panel.background=element_rect(fill='darkgrey'),
             panel.grid.minor = element_line(colour='white'),
             panel.grid.major = element_line(color='red',size = 2),
             legend.text = element_text(size = 8))
ggplot(data=diamonds,aes(x=clarity,fill=cut))+
   geom_bar()
theme_set(theme_base())
#theme_set(old)
ggplot(data=diamonds,aes(x=clarity,fill=cut))+
   geom_bar()

# ggthemes ####
plottemp+theme_economist()
plottemp+theme_excel()+
   theme(axis.text = element_text(face='bold'))

#manipulate plots from packages
fit<- survfit(Surv(time, status) ~ sex, data = lung)
ggsurvplot(fit)
(ggsurv <- ggsurvplot(fit,pval=F,risk.table='absolute',
           fun='pct',cumevents=F, censor.shape="|",
           tables.height=.25,palette=pal_jama("default")(2),
           ylim=c(05,100),xlim=c(0,1000),break.time.by=250,
           axes.offset=T))
ggsurv$plot<-ggsurv$plot+
   annotate('text',x=700,y=92,size=rel(5),
            label=surv_pvalue(fit,method='n')$pval.txt)+
   geom_vline(xintercept=0)+
   theme(axis.line.y=element_blank(),
         panel.grid.major.y=element_line(color='grey'),
         axis.text.y=element_text(hjust=1,
                                  margin=margin(r=0)),
         axis.ticks.y=element_blank())
ggsurv
ggsurv$table <- ggsurv$table + 
   # scale_x_continuous(breaks=seq(0,180,30),expand=c(0,0))+
   theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y=element_blank(),
      plot.title=element_text(size=rel(1.1)))
ggsurv

#Regression trees
gg_rtree<-function(rpartdata=rpart_out,miny=NULL,
                          title='')
{
   t<-ggdendro::dendro_data(rpartdata,uniform=T,compress=T)
   if(is.numeric(t$leaf_labels$label))
   {
      t$leaf_labels$label<-prettyNum(as.numeric(t$leaf_labels$label))
   }
   t$leaf_labels<-arrange(t$leaf_labels,x)
   if(is.null(miny)) {miny<-c(min(t$leaf_labels$y),min(t$leaf_labels$y)/2)}
   t$labels$label<-enc2utf8(as.character(t$labels$label))
   t$labels$label<-stringr::str_replace(pattern='>=',replacement=' \u2265 ',
                                        string=gsub('<',' <',t$labels$label))
   t$segments$leaf<-'no'
   for (i in 1:nrow(t$segments))
   {
      if(t$segments$y[i]>t$segments$yend[i])
      {
         temp<-t$segments$y[i]
         t$segments$y[i]<-t$segments$yend[i]
         t$segments$yend[i]<-temp
      }
      
   }
   for(i in 1:nrow(t$leaf_labels))
   {
      t$segments$y[which(t$segments$x==t$leaf_labels$x[i] &
                            t$segments$y==t$leaf_labels$y[i])]<-
         ifelse(i%%2==0,miny[1],miny[2])
      t$leaf_labels$y[i]<-
         ifelse(i%%2==0,miny[1],miny[2])
      t$segments$leaf[which(t$segments$x==t$leaf_labels$x[i] &
                               t$segments$y==t$leaf_labels$y[i])]<-'yes'
   }
   #yes/no edges
   t$yes<-t$segments[which(t$segments$y==t$segments$yend),1:3]
   t$yes$label<-'yes'
   t$no<-t$segments[which(t$segments$y==t$segments$yend),c(1,3:4)]
   colnames(t$no)[3]<-'x'
   t$no$label<-'no'
   
   # t$leaf_labels$y<-c(min(t$leaf_labels$y),0.05)
   tree<-ggplot(t$segments)+
      ggtitle(title)+
      geom_segment(aes(x,y,xend=xend,yend=yend,
                       linetype=leaf,size=leaf))+
      scale_size_manual(values=c(.5,1.25),guide=F)+
      scale_linetype_manual(values=c(1,2),guide=F)+
      geom_label(data=t$labels,aes(x,y,label=gsub('>=',' \u2265 ',label)),
                 vjust=-.25,hjust=.5,fontface='bold')+
      geom_label(data=t$yes,aes(x,y,label=label),vjust=1,size=3,
                 label.r=unit(.5,units='lines'),color='white',fill='black',
                 fontface='bold')+
      geom_label(data=t$no,aes(x,y,label=label),vjust=1,size=3,
                 label.r=unit(.5,units='lines'),color='white',fill='black',
                 fontface='bold')+
      geom_label(data=t$leaf_labels,aes(x,y,label=label),vjust=1,
                 label.r=unit(0,'lines'),fontface='bold',fill='lightgrey')+
      scale_y_continuous(limits=c(0,max(t$labels$y)*1.1))+
      ggdendro::theme_dendro()+
      theme(plot.title = element_text(hjust = 0.5,size=rel(2)))
   return(tree)
}

rpart_out <- rpart(mpg~factor(am)+wt+hp,data=mtcars,minsplit=5)
prp(rpart_out)
gg_rtree(rpart_out,title = 'mpg prediction')

# https://github.com/thomasp85/patchwork

# https://www.data-imaginist.com/2019/a-flurry-of-facets/

# https://github.com/thomasp85/gganimate


ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
   geom_point(alpha = 0.7, show.legend = FALSE) +
   scale_colour_manual(values = country_colors) +
   scale_size(range = c(2, 12)) +
   scale_x_log10() +
   facet_wrap(~continent) +
   # Here comes the gganimate specific bits
   labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
   transition_time(year) +
   ease_aes('linear')

# https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html

