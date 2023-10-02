# -------------------------------------------------------------------------
# chapter 5 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
library(Grind)

# fig 5.1 -----------------------------------------------------------------

# required objects
LV <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dN <- a*N - b*P*N
    dP <- c*P*N - d*P
    return(list(c(dN, dP)))
  })
}
##
set.seed(1)
layout(matrix(1:4,2,2,byrow=T))
p <- c(a=1.1,b=.4,c=.1,d=0.4) # p is a named vector of parameters
s <- c(N=10,P=10)             # s is the state
n <- 30
data_deterministic <- run(odes=LV,n,table=T,timeplot =F) # deterministic data
data_stochastic <- run(odes=LV,n,table=T,after="state<-state+rnorm(2,0,.1)",
                       timeplot =F) # add stochasticity
data_error <- run(odes=LV,n,table=T,timeplot =F)
data_error[,2:3] <- data_error[,2:3]+
  matrix(rnorm(2*n,0,2),,2) # measurement error 
#fit & plot
r <- .5 # growth rate
s<- s*abs(rnorm(2,1,0.1));s; p<- p*abs(rnorm(4,1,0.1));p    # start values
f_deter<-  fit(odes=LV,data_deterministic,main='deterministic')
f_stoch<- fit(odes=LV,data_stochastic,main='stochastic', timeplot =T)
f_error<- fit(odes=LV,data_error,main='error')
#pars <- matrix(c(f_deter$par[3:6],f_stoch$par[3:6],f_error$par[3:6]),,3)
#pars <- rbind(pars,c(summary(f_deter)$sigma,summary(f_stoch)$sigma,summary(f_error)$sigma))
#barplot(t(pars),beside=T,names=c('a','b','c','d','Residuals'),
#        legend.text=c('deterministic','stochastic','error'),
#        args.legend=c(x=13))

plotfun <- function(datas){
  strng <- deparse(substitute(datas))
  title <- str_remove(strng, 'data_')
  datas %>% 
    pivot_longer(c('N','P'), names_to = 'par', values_to = 'val') %>% 
    ggplot()+
    geom_line(aes(time, val, color = par), linewidth = .5)+
    geom_point(aes(time, val, shape = par, color = par), size = 1)+
    scale_color_manual(values = c(colors[1],colors[2]))+
    labs(y = 'Density', color = '', shape = '', 
         title = title)+
    theme_minimal() + theme1+
    theme(legend.position = 'right')
}
#Grind::timePlot(data_deterministic)
p1 <- plotfun(data_deterministic)
#Grind::timePlot(data_stochastic)
p2 <- plotfun(data_stochastic)
#Grind::timePlot(data_error)
p3 <- plotfun(data_error)

# bar plot
bardata <- bind_rows(f_deter$par[3:6], f_stoch$par[3:6], f_error$par[3:6]) %>% 
  bind_cols(.,'Residuals' = c(summary(f_deter)$sigma,summary(f_stoch)$sigma,summary(f_error)$sigma)) %>% 
  add_column(set = c('deterministic','stochastic','error'))

p4 <- bardata %>% 
  pivot_longer(c('a','b','c','d','Residuals'), names_to = 'param', values_to = 'val') %>% 
  ggplot()+
  geom_col(aes(set, val, fill = set))+
  labs(y = '', x = '', fill = '')+
  scale_fill_manual(values = c('#E4D5B6', '#A6761D', '#634611'))+
  facet_wrap(~param, nrow = 1)+
  theme_minimal() + theme1+
  theme(axis.text.x = element_blank(), 
        strip.text = element_text(colour = "grey10", size = 15))

p <- (p1 + p2) / (p3 + p4 )
p

ggsave('media/ch5/fig-ch5-img1-old-49.jpg', width = 6, height = 4.5, units = 'in', dpi = 300)

# fig 5.2 -----------------------------------------------------------------
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    dX <-  a + b*X - X^3        # cusp
    return(list(dX))
  })
}
layout(t(c(1,1,1,2)))
data <- run(table=T,tmax=1000,method='euler',tstep=.1,after="state<-state+
            rnorm(1,mean=0,sd=0.4)*sqrt(tstep)",ymax=2,ymin=-2,timeplot=F)
p1 <- data %>% 
  ggplot(aes(time, X))+
  geom_line(linewidth = .1, color = '#634611') +
  scale_y_continuous(breaks = seq(-1.5, 1.5, .5))+
  theme_minimal() + theme1
#plot(data,type='l',bty='n')
#barplot(hist(data[,2],30,plot=F)$counts,xlab="X",hor=T)
p2 <- data %>% select(X) %>% ggplot() + 
  geom_histogram(aes(X), color = '#634611', fill = '#E4D5B6', linewidth = .2) +
  coord_flip() + labs(x = '', y='')+
  scale_y_continuous(breaks = c(1,1000))+
  theme_minimal() + theme1
p2
library(cowplot)
combined_plot <- plot_grid(
  p1, p2, ncol = 2, rel_widths = c(3/4, 1/4))
combined_plot

ggsave('media/ch5/fig-ch5-img2-old-50.jpg', width = 5, height = 3, units = 'in', dpi = 300)

# -------------------------------------------------------------------------

# fig 5.3 -----------------------------------------------------------------

p <- c(a=0,b=1)
low <- newton(s=c(X=-1)) # finds a minimum starting from X = -1
continue(low,x="a",y="X",xmin=-2,xmax=2,ymax=2) # Continue this steady state varying a
high <- newton(s=c(X=1)) # again starting from X = 1
continue(high,x="a",y="X",xmin=-2,xmax=2,ymax=2,add=T)

# fig 5.4 -----------------------------------------------------------------
library(deBif)
phaseplane(model,s,p)

# fig 5.5 -----------------------------------------------------------------

h1=function(N,a,b) b*N
h2=function(N,a,b) b*N/(a+N)
h3=function(N,a,b) b*N^2/(a+N^2)
h4=function(N,a,b) b*N/(a+N^2)
#layout(1)
#curve(h1(x,0,.2),0,5,bty='n',ylab='Predation',xlab='N',lwd=2,cex.main=2,main='Hollings functional responses')
#curve(h2(x,.1,1),0,5,add=T,col='2',lty=2,lwd=2)
#curve(h3(x,.5,1),0,5,add=T,col='3',lty=3,lwd=2)
#curve(h4(x,.5,1),0,5,add=T,col='4',lty=4,lwd=2)
#legend('bottomright',lty=1:4,col=1:4,cex=1.5,legend=paste('type ',1:4,sep='',col=''))
text(3.6,.58,expression(paste('Type I: ', P==b*N)),cex=1.5)
text(.8, .97,expression(paste('Type II: ',P==b*N/(a+N))),cex=1.5)
text(2.7,.85,expression(paste('Type III: ',P==b*N^2/(a+N^2))),cex=1.5)
text(4.2,.35,expression(paste('Type IV: ',P==b*N/(a+N^2))),cex=1.5)

p <- ggplot(data.frame(x = seq(0, 5, by = 0.01)), aes(x = x)) +
  stat_function(fun = h1, args = list(0,.2), geom = "line",
                linetype = "solid", linewidth = .4)+ 
  stat_function(fun = h2, args = list(.1,1), geom = "line",
                linetype = "dashed",linewidth = .4, color = colors[1])+ 
  stat_function(fun = h3, args = list(.5,1), geom = "line",
                linetype = "dotdash", linewidth = .4, color = colors[2])+ 
  stat_function(fun = h4, args = list(.5,1), geom = "line",
                linetype = "dotted",linewidth = .4, color = colors[3])+
  labs(y='Predation',x='N', title = 'Hollings functional responses') +
  theme_minimal() + theme1
p + annotate('text',x=3.6,y=.58,label=expression(paste('Type I: ', Rho==Beta*Nu)), size = 6)+
    annotate('text',x=.3 ,y=.97,label=expression(paste('Type II: ',Rho==frac(Beta*Nu,Alpha+Nu))),
             color = colors[1], size = 6)+
    annotate('text',x=2.7,y=.85,label=expression(paste('Type III: ',Rho==frac(Beta*Nu^2,Alpha+Nu^2))),
             color = colors[2], size = 6)+
    annotate('text',x=4.2,y=.35,label=expression(paste('Type IV: ',Rho==frac(Beta*Nu,Alpha+Nu^2))),
             color = colors[3], size = 6)
  
ggsave('media/ch5/fig-ch5-img5-old-53.jpg', width = 5, height = 3, units = 'in', dpi = 300)

# fig 5.6 -----------------------------------------------------------------
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    x <- state[1:n]
    v <- state[(n+1):(2*n)]
    dx <-  v # change in distance = speed
    delta_v <- v- m %*% v # difference in speed to next car
    s_alpha <- m %*% x - x -l #  distance to next car
    s_alpha[n] <- 100 # front car has no car in front
    s_star <- s0 + v * T + v * delta_v / (2*sqrt(a*b))
    dv <- a * (1 - (v/v0)^delta - (s_star/s_alpha)^2) # change in speed
    return(list(c(dx,dv)))
  })
}

n=50
p <- c(l=5,v0=30,T=1.5,a=.73,b=1.67,delta=4,s0=2)
x_init <- (0:(n-1))*(p['s0']+p['l'])
v_init <- rep(0,n)
s <- c(x_init,v_init)

m <- diag(1, n, n); m= rbind(m[-1,],0) # order cars
# simulation with front car suddenly breaking at t = 150
data <- run(tmax=300,timeplot = F,table=T,after = 'if (t==150) state[2*n] = 0')

#matplot(data[,2:(n+1)],type='l',bty='n',xlab='time',ylab = 'x')
library(ggmatplot)
ggmatplot(data[, 2:(n+1)], plot_type = "line", color = colors[3],
          linewidth = .15, linetype = 1, xlab = "time",ylab = 'x') + 
  theme_minimal() + theme1 +
  theme(legend.position = 'none')

ggsave('media/ch5/fig-ch5-img6-old-54.png', width = 4, height = 2.5, units = 'in', dpi = 300)

