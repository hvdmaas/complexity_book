# -------------------------------------------------------------------------
# chapter 5 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
source("r-figures-code/plane2T.R")
library(Grind)

# antiRun function --------------------------------------------------------

antiRun <- function(data, point = FALSE, psize = .4, lsize = .25, grp = NULL,  ...){
  dots <- list(...)
  p <- data %>% 
    pivot_longer(colnames(data)[-1], names_to = 'par', values_to = 'val') %>% 
    ggplot() +
    geom_line(aes(time, val, linetype = par, color = par), linewidth = lsize, ...) +
    scale_color_manual(values = c(ncolors[1],ncolors[2]))+
    labs(y = 'Density', x = 'Time', color = '', linetype = '', shape = '', ...) +
    theme_minimal() + theme1+
    theme(legend.position = 'right')
  if(point) pl <- p + geom_point(aes(time, val, shape = par, color = par), size = psize, ...)
  ifelse(point, return(pl), return(p))
}

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
f_deter<-  fit(odes=LV,data_deterministic,main='deterministic', timeplot = FALSE)
f_stoch<- fit(odes=LV,data_stochastic,main='stochastic', timeplot = FALSE)
f_error<- fit(odes=LV,data_error,main='error', timeplot = FALSE)
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
    geom_line(aes(time, val, color = par, linetype = par), linewidth = .4)+
    geom_point(aes(time, val, shape = par, color = par), size = 1)+
    scale_color_manual(values = c(ncolors[1],ncolors[2]))+
    labs(y = 'Density', color = '', shape = '', linetype = '', x = 'Time', 
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
        strip.text = element_text(colour = "grey10", size = 15),
        legend.key.size = unit(0.1, 'in'),
        legend.text=element_text(size=12),
        legend.position = 'right')

plot1 <- (p1 + p2) / (p3 + p4 )
plot1

ggsave('media/ch5/fig-ch5-img1-old-49.jpg', width = 6, height = 4.5, units = 'in', dpi = 300)

# fig 5.2 -----------------------------------------------------------------
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    dX <-  a + b*X - X^3        # cusp
    return(list(dX))
  })
}
p <- c(a=0,b=1); s <- c(X=.1)
#run(ymin=-1)
s[1] <- -.1
#run(add=T)
#layout(t(c(1,1,1,1)))
dat <- run(table=T,tmax=1000,method='euler',tstep=.1,after="state<-state+
            rnorm(1,mean=0,sd=0.4)*sqrt(tstep)",ymax=2,ymin=-2,timeplot=F)
p1 <- dat %>% 
  ggplot(aes(time, X))+
  geom_line(linewidth = .1, color = ncolors[4]) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, .5))+
  labs(x='Time')+
  theme_minimal() + theme1
#plot(data,type='l',bty='n')
#barplot(hist(data[,2],30,plot=F)$counts,xlab="X",hor=T)
p2 <- dat %>% dplyr::select(X) %>% ggplot() + 
  geom_histogram(aes(X), fill = ncolors[4], color = 'white', linewidth = .2) +
  coord_flip() + labs(x = '', y='')+
  scale_y_continuous(breaks = c(1,1000))+
  labs(y = 'Frequency')+
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
bifurcation(model,s,p)

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
                linetype = "dashed",linewidth = .4, color = ncolors[1])+ 
  stat_function(fun = h3, args = list(.5,1), geom = "line",
                linetype = "dotdash", linewidth = .4, color = ncolors[2])+ 
  stat_function(fun = h4, args = list(.5,1), geom = "line",
                linetype = "dotted",linewidth = .4, color = ncolors[3])+
  labs(y='Predation',x='N', title = 'Hollings functional responses') +
  theme_minimal() + theme1
p + annotate('text',x=3.6,y=.58,label=expression(paste('Type I: ', Rho==Beta*Nu)), size = 6)+
    annotate('text',x=.3 ,y=.97,label=expression(paste('Type II: ',Rho==frac(Beta*Nu,Alpha+Nu))),
             color = ncolors[1], size = 6)+
    annotate('text',x=2.7,y=.85,label=expression(paste('Type III: ',Rho==frac(Beta*Nu^2,Alpha+Nu^2))),
             color = ncolors[2], size = 6)+
    annotate('text',x=4.2,y=.35,label=expression(paste('Type IV: ',Rho==frac(Beta*Nu,Alpha+Nu^2))),
             color = ncolors[3], size = 6)
  
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
dat <- run(tmax=300,timeplot = F,table=T,after = 'if (t==150) state[2*n] = 0')

#matplot(data[,2:(n+1)],type='l',bty='n',xlab='time',ylab = 'x')
library(ggmatplot)
ggmatplot(dat[, 2:(n+1)], plot_type = "line", color = ncolors[3],
          linewidth = .15, linetype = 1, xlab = "time",ylab = 'x') + 
  theme_minimal() + theme1 +
  theme(legend.position = 'none')

ggsave('media/ch5/fig-ch5-img6-old-54.png', width = 4, height = 2.5, units = 'in', dpi = 300)


# fig 5.9 -----------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR <- a*R+b*J
    dJ <- c*R+d*J
    return(list(c(dR, dJ)))
  })
}
layout(1)
p <- c(a=-1,b=1,c=.5,d=-1) # parameters
s <- c(R=0.1,J=.1) 
dat1 <- run(table=TRUE, timeplot = FALSE)
#Grind::timePlot(data_deterministic)
p1 <- antiRun(dat1)
#plane(portrait=T,ymin=-1,xmin=-1,grid=3,vector=T,legend=F)

p <- c(a=-.2,b=-1,c=1,d=0) # parameters
dat2 <- run(ymin=-.2,legend=F, table=TRUE, timeplot = FALSE)
p2 <- antiRun(dat2)
#plane(portrait=T,ymin=-1,xmin=-1,grid=2,tstep=.001,legend=F)

p <- c(a=-.1,b=-1,c=1,d=0.1) # parameters
dat3 <- run(ymin=-.2,legend=F, table=TRUE, timeplot = FALSE)
p3 <- antiRun(dat3)
#plane(portrait=T,ymin=-1,xmin=-1,grid=3,tstep=.001,legend=F)

p <- p1 / p2 / p3
p

ggsave('media/ch5/fig-ch5-img9-old-57_1of2.png', width = 3, height = 5, units = 'in', dpi = 300)

# plane 2 -----------------------------------------------------------------
png('media/ch5/fig-ch5-img9-old-57_2of2.png', width = 2.5, height = 5, units = 'in', res = 300)
layout(matrix(1:3,3,1,byrow=T))
par(mar=c(5,4,1,2))
p <- c(a=-1,b=1,c=.5,d=-1) # parameters
plane2(portrait=T,ymin=-1,xmin=-1,grid=3,vector=T,legend=F)

p <- c(a=-.2,b=-1,c=1,d=0) # parameters
plane2(portrait=T,ymin=-1,xmin=-1,grid=2,tstep=.001,legend=F)

p <- c(a=-.1,b=-1,c=1,d=0.1)
plane2(portrait=T,ymin=-1,xmin=-1,grid=3,tstep=.001,legend=F)
dev.off()

# fig 5.10 ----------------------------------------------------------------
influence <- function(x,a=-8,b=1) sign(x)/(1+exp(a*(abs(x)-b)))
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW <- influence(H,a,b)-rw*W+We
    dH <- influence(W,a,b)-rh*H+He
    return(list(c(dW,dH)))
  })
}
## COLUMN 1 - INFLUENCE FUNCTION
a=-8
infPlot <- function(b) {
  ggplot() +
  stat_function(fun = function(x) influence(x, a, b), geom = "line", linewidth = .5,
                color = ncolors[4]) +
  labs(x = 'W', y = 'H') +
  scale_x_continuous(limits = c(-3,3), n.breaks = 6) + theme_minimal()+
  theme1 + theme(legend.position = 'none')
}
p1 <- infPlot(b = Inf)
p2 <- infPlot(b = 0)
p3 <- infPlot(b = 1)
plot1 <- p1 / p2 / p3
plot1
ggsave('media/ch5/fig-ch5-img10-old-58_1of3.png', width = 2.5, height = 5, units = 'in', dpi = 300)
dev.off()
## COLUMN 2 - nullclines
png('media/ch5/fig-ch5-img10-old-58_2of3.png', width = 2.5, height = 5, units = 'in', res = 300)
layout(matrix(1:3,3,1,byrow=T))
par(mar=c(4,4,1,2))
for(b in c(Inf,0,1)){
  p <- c(rw=.6,rh=.6,We=.18,He=-.18,a=-8,b=b)
  s <- c(W=0,H=0)
  plane2(xmin=-2.5,xmax=2.5,ymin=-2,ymax=2,legend=F)
  for(i in seq(-2,2,by=.25)) newton(s=c(W=i,H=i),plot=T)
}
dev.off()

## COLUMN 3 - PHASE
#layout(matrix(1:9,3,3,byrow=T))
#par(mar=c(4,4,1,2))
p <- c(rw=.6,rh=.6,We=.18,He=-.18,a=-8,b=Inf)
#
dat <- list()
for(b in c(Inf,0,1)){
  p['b']<- b
  if(is.infinite(b)) n = 1; if(b==0) n = 2; if(b==1) n = 3
  dat[[n]] <- list()
  for (i in 1:100)
    dat[[n]][[i]] <- run(state=c(W=rnorm(1,0,.5),H=rnorm(1,0,1)), tmax=50,ymin=-2,ymax=2,add=(i>1),legend=F,
        table=TRUE, timeplot = FALSE)
}
datt <- sapply(dat, bind_rows, simplify = FALSE)

# make antiRun2 because of Time groups
antiRun2 <- function(data, lsize = .25, ...){
  data %>% 
    mutate(grp = rep(1:100, each = 51)) %>% 
    #filter(grp ==5) %>% 
    pivot_longer(colnames(data)[-1], names_to = 'par', values_to = 'val', ...) %>% 
    ggplot() +
    geom_line(aes(time, val, color = par, group = interaction(par, grp), linetype = par), linewidth = lsize, ...) +
    scale_color_manual(values = c(ncolors[1],ncolors[2]))+
    labs(y = 'Density', x = 'Time', color = '', linetype = '', shape = '', ...) +
    theme_minimal() + theme1+
    theme(legend.position = 'none')
}

p1 <- antiRun2(datt[[1]])
p2 <- antiRun2(datt[[2]])
p3 <- antiRun2(datt[[3]])
plot3 <- p1/p2/p3
plot3
ggsave('media/ch5/fig-ch5-img10-old-58_3of3.png', width = 2.5, height = 5, units = 'in', dpi = 300)


# fig 5.11 ----------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX <- (a + b * Y) * X - a * X^2 / K
    dY <- (c + d * X) * Y - c * Y^2 / K
    return(list(c(dX, dY)))
  }) 
}
p <- c(K = 1, a = 0.4, b = -0.05, c=.4, d = -0.15)
s <- c(X = 0.01, Y = 0.01)
dat <- run(method = "euler", tstep = 1,
           table=TRUE, timeplot = FALSE)
p1 <- antiRun(dat)
#layout(matrix(1:4,2,2))


p <- c(K = 1, a = 0.05, b = -0.1,  c = 0.05, d = -0.09)
s <- c(X = 0.0126, Y = 0.01)
dat2 <- run(tmax = 1000, method = "euler", tstep = 1,
    table=TRUE, timeplot = FALSE)
p2 <- antiRun(dat2)
halfp <- p1+p2
halfp
ggsave('media/ch5/fig-ch5-img11-old-59_1of2.png', width = 6, height = 3, units = 'in', dpi = 300)
dev.off()
## plane2 row
png('media/ch5/fig-ch5-img11-old-59_2of2.png', width = 10, height = 5, units = 'in', res = 300)
layout(matrix(1:2,1,2))
p <- c(K = 1, a = 0.4, b = -0.05, c=.4, d = -0.15)
s <- c(X = 0.01, Y = 0.01)
plane2(portrait = TRUE,grid=4)
p <- c(K = 1, a = 0.05, b = -0.1,  c = 0.05, d = -0.09)
s <- c(X = 0.0126, Y = 0.01)
plane2(portrait = TRUE,grid=4)

dev.off()

# -------------------------------------------------------------------------

# fig 5.13 ----------------------------------------------------------------

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dA <- -A + b*T  
    dT <- -T + 1/(1+exp(-alpha*(A+beta)))
    return(list(c(dA, dT)))
  })
}
p <- c(b=1, alpha=12, beta=-.7) 
s <- c(A=0, T=0) 
#1
png('media/ch5/fig-ch5-img13-old-61_1of2.png', width = 8, height = 4, units = 'in', res = 300)
plane2(vector=T,xmin=0,ymin=0,xmax=1,ymax=1.1,legend=F) 
newton(s=c(A=0,T=0),plot=T)
newton(s=c(A=0.8,T=.8),plot=T)
newton(s=c(A=1,T=1),plot=T);
dev.off()
#2
dat <- run(after="if(t>20&t<30)state[1]<-1;state<-state+rnorm(2,mean=0,sd=0.1)",
    table=TRUE, timeplot = FALSE)
antiRun(dat, line = TRUE, psize = .4)
ggsave('media/ch5/fig-ch5-img13-old-61_2of2.png', width = 5, height = 2.5, units = 'in', dpi = 300)

# 5.14 --------------------------------------------------------------------
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    dX <-  a + b*X - X^3        # cusp
    da <- -e*X
    return(list(c(dX,da)))
  })
}

s <- c(X=.1,a=0); # initial state and parameter values
#1
p <- c(e=.05,b=-.5)
dat1 <- run(ymin=-.1,main='b = -.5',legend=F,
            table=TRUE, timeplot = FALSE)
p1 <- antiRun(dat1, title = 'b = -.5')
p <- c(e=.05,b=1)
dat2 <- run(ymin=-1.5,main='b = 1',legend=F,
            table=TRUE, timeplot = FALSE)
p2 <- antiRun(dat2, title = 'b = 1')
p1/p2
ggsave('media/ch5/fig-ch5-img14-old-62_1of2.png', width = 3.5, height = 5, units = 'in', dpi = 300)

#2
png('media/ch5/fig-ch5-img14-old-62_2of2.png', width = 5.5, height = 8, units = 'in', res = 300)
layout(matrix(1:2,2,1))
p <- c(e=.05,b=-.5)
plane2(xmax=2,ymin=-1,ymax=2,xmin=-2,portrait=T,grid=2,main='b = -.5')
p <- c(e=.05,b=1)
plane2(xmax=2,ymin=-1,ymax=2,xmin=-2,portrait=T,grid=2,main='b = 1')
dev.off()

# 5. 16 -------------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX <- a*Y + b*X - X^3 
    dY <- c*X + d*Y - Y^3
    return(list(c(dX, dY))) 
  })
}

s <- c(X=0,Y=0) 
png('media/ch5/fig-ch5-img16-old-64.png', width = 8, height = 6, units = 'in', res = 300)
layout(matrix(1:4,2,2))
for(i in c('a','b','c','d'))
{
  if (i == 'a') p <- c(a=.3,b=1,c=.3,d=1)
  if (i == 'b') p <- c(a=.6,b=1,c=.6,d=1)
  if (i == 'c') p <- c(a=1,b=1,c=1,d=1)
  if (i == 'd') p <- c(a=1,b=1,c=-1,d=1)
  plane2(tstep=0.5,portrait=(i=='d'),xmin=-2,ymin=-2,xmax=2,ymax=2,
        legend=F,grid=2,main = paste("Case ",i)) # make a phase portrait (Fig 1c)
  if (i != 'd') for(i in 1:200) newton(c(X=runif(1,-2,2),Y=runif(1,-2,2)),plot=T) else
    newton(c(X=0,Y=0),plot=T)
}
dev.off()
# fig 5.17 --------------------------------------------------------------------

s <- c(X=0.1,Y=.1) 
p <- c(a=1,b=1,c=-1,d=1)
dat <- run(tmax=20,tstep=0.1,ymin=-2,ymax=2,
    table=TRUE, timeplot = FALSE)
antiRun(dat)
ggsave('media/ch5/fig-ch5-img17-old-65.png', width = 5, height = 2.5, units = 'in', dpi = 300)

# fig 5.19 ----------------------------------------------------------------
set.seed(1)
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    X <- state[1:N]
    b0_i <- parms[1:N]
    dX <- -X^3 + a0_i + a_ij %*% X + b0_i*X + (X * b_ij %*%  abs(X))  # note abs(X)
    return(list(dX))
  })
}
N=10 # 10 necker cubes
X <- runif(N,-0.1,0.1) # initial state of X
a0_i <- rep(0,N) # no  bias in percepts
a_ij <- matrix(.02,N,N) # small couplings (normal)
diag(a_ij) <-  0 # set diagonal of a to 0
b0_i <- rep(-.3,N) # attention initially low
b_ij <- matrix(.2,N,N) # some spread of attention (splitting)
diag(b_ij) <- 0 # set diagonal of b to 0

s <- X;p <- c(b0_i) # required for grind

dat <- run(after="if(t==33)parms<-c(1,rep(-.3,N-1));
           if(t==66)parms<-rep(-.3,N);
           state<-state+rnorm(N,mean=0,sd=0.05)",
           table=TRUE, timeplot = FALSE)

antiRuntmp <- function(data, line = TRUE, psize = .4, lsize = .25, ...){
  dots <- list(...)
  p <- data %>% 
    pivot_longer(colnames(data)[-1], names_to = 'par', values_to = 'val') %>% 
    ggplot() +
    #geom_point(aes(time, val, shape = par), color = ncolors[3], size = psize, ...)+
    #scale_color_manual(values = c(ncolors[3]))+
    labs(y = 'Density', x = 'Time', color = '', shape = '', ...) +
    theme_minimal() + theme1+
    theme(legend.position = 'none', ...)
  if(line) pl <- p + geom_line(aes(time, val, group = par),color = ncolors[4], linewidth = lsize, ...)
  ifelse(line, return(pl), return(p))
}
pl <- antiRuntmp(dat)
b0_i <- rep(-.3,100); b0_i[34:66]=1 # for plotting attention
pl1 <- pl + geom_line(data = data.frame(
  time = 1:100,
  val = b0_i,
  par = "b0_i"
), aes(x = time, y = val), color = "black", size = .4, linetype = "dotted")

pl2 <- pl1 + annotate("text", x = 80, y = 1.4, label = 'Percepts', size = 9, family = cfont) +
  annotate("text", x = 80, y = -0.5, label = 'Attention', size = 9, family = cfont)
pl2
ggsave('media/ch5/fig-ch5-img19-old-67.png', width = 6, height = 3, units = 'in', dpi = 300)

