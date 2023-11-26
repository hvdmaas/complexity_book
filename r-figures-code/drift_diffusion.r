model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX = I
    return(list(c(dX)))
  })
}
p <- c(I=.01); s <- c(X=0)
bound <- 1
set.seed(1)
r1=run(table=T,timeplot=T,method='euler', tstep=.1,
    tmax=500,after="state<-state+rnorm(1,mean=0,sd=0.1)*sqrt(tstep);
    if(abs(state)>bound) break",ymin=-bound,ymax=bound)
set.seed(8)
r2=run(table=T,timeplot=T,method='euler', tstep=.1,
       tmax=500,after="state<-state+rnorm(1,mean=0,sd=0.1)*sqrt(tstep);
    if(abs(state)>bound) break",ymin=-bound,ymax=bound)
names(r1)=c('time','X1')
names(r2)=c('time','X2')
r=merge(r1,r2,all=T)


library(rtdists)
h=rdiffusion(29000,a=2,v=.01,t0=0,s=.1)
h1=h[h$response=='upper',]
h2=h[h$response=='lower',]

library(ggplot2)
library(patchwork)
g1 = ggplot(r, aes(x = time)) +
  geom_line(aes(y = X1)) +
  geom_line(aes(y = X2)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = -1) +
 # geom_hline(yintercept = 0) +
  ylim(-1,1) +
  xlim(0,150)+
  ylab('X') +  
  theme_minimal()+
  theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())

g2=ggplot(h1,aes(x=rt)) +
  geom_histogram(binwidth = 5,fill = "grey",color = "black",  alpha = 0.7)  +  labs(title = "", x = "", y = "")+
  theme_minimal()+
  xlim(0,150)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

g3=ggplot(h2,aes(x=rt)) +
  geom_histogram(binwidth = 5,fill = "grey",color = "black",  alpha = 0.7) +
  theme_minimal() +  
  xlim(0,150)+
  labs(title = "", x = "", y = "")+ scale_y_reverse()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
g2/g1/g3 + plot_layout(heights = c(1, 5,1))
