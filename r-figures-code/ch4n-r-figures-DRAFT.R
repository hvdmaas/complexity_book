# -------------------------------------------------------------------------
# chapter 5 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
library(Grind)

source("r-figures-code/plane2T.R")
source("r-figures-code/run2_and_timePlot2.R")
source("r-figures-code/curve2.R")
source("r-figures-code/fit2.R")


# antiRun function --------------------------------------------------------

#antiRun <- function(data, point = FALSE, psize = .4, lsize = .25, grp = NULL,  ...){
#  dots <- list(...)
#  p <- data %>% 
#    pivot_longer(colnames(data)[-1], names_to = 'par', values_to = 'val') %>% 
#    ggplot() +
#    geom_line(aes(time, val, linetype = par, color = par), linewidth = lsize, ...) +
#    scale_color_manual(values = c(ncolors[1],ncolors[2]))+
#    labs(y = 'Density', x = 'Time', color = '', linetype = '', shape = '', ...) +
#    theme_minimal() + theme1+
#    theme(legend.position = 'right')
#  if(point) pl <- p + geom_point(aes(time, val, shape = par, color = par), size = psize, ...)
#  ifelse(point, return(pl), return(p))
#}

# fig 4n.1 -----------------------------------------------------------------

# required objects
LV <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dN <- a*N - b*P*N
    dP <- c*P*N - d*P
    return(list(c(dN, dP)))
  })
}

png('media/ch4n/fig-ch4n-img1-old-49.png', width = 8, height = 4.82, units = 'in', res = 300)

## base 2
set.seed(1)
layout(matrix(1:4,2,2,byrow=T))
par(mar=c(4.5,5,1,2)) 
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
s<- s*abs(rnorm(2,1,0.1));s; p<- p*abs(rnorm(4,1,0.1));p    # start values
f_deter<- fit2(odes=LV,data_deterministic,main='deterministic', legend = FALSE)
f_stoch<- fit2(odes=LV,data_stochastic,main='stochastic', legend = FALSE)
f_error<- fit2(odes=LV,data_error,main='error', legend = FALSE)

pars <- matrix(c(f_deter$par[3:6],f_stoch$par[3:6],f_error$par[3:6]),,3)
pars <- rbind(pars,c(summary(f_deter)$sigma,summary(f_stoch)$sigma,summary(f_error)$sigma))

barplot(t(pars),beside=T,names=c('a','b','c','d','Residuals'), 
        bty = 'n', family = "CMU-bright", axes = FALSE,
        legend = c('deterministic','stochastic','error'), 
        args.legend=c( bty = 'n', x = 'top'))
axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1) 
dev.off()

# fig 4n.2 -----------------------------------------------------------------
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
  geom_line(linewidth = .5, color = ncolors[4]) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, .5))+
  labs(x='Time')+
  theme_minimal() + theme1
#plot(data,type='l',bty='n')
#barplot(hist(data[,2],30,plot=F)$counts,xlab="X",hor=T)
p2 <- dat %>% dplyr::select(X) %>% ggplot() + 
  geom_histogram(aes(X), fill = ncolors[4], color = 'white', linewidth = .5) +
  coord_flip() + labs(x = '', y='')+
  scale_y_continuous(breaks = c(1,1000))+
  labs(y = 'Frequency')+
  theme_minimal() + theme1
p2
library(cowplot)
combined_plot <- plot_grid(
  p1, p2, ncol = 2, rel_widths = c(3/4, 1/4))
combined_plot

ggsave('media/ch4n/fig-ch4n-img2-old-50.png', width = 10, height = 7, units = 'in', dpi = 300)

# fig 4n.5 -----------------------------------------------------------------

h1=function(N,a,b) b*N
h2=function(N,a,b) b*N/(a+N)
h3=function(N,a,b) b*N^2/(a+N^2)
h4=function(N,a,b) b*N/(a+N^2)
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
  
ggsave('media/ch4n/fig-ch4n-img5-old-53.jpg', width = 5, height = 3, units = 'in', dpi = 300)

# fig 4n.6 -----------------------------------------------------------------
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

n <- 50
p <- c(l=5,v0=30,T=1.5,a=.73,b=1.67,delta=4,s0=2)
x_init <- (0:(n-1))*(p['s0']+p['l'])
v_init <- rep(0,n)
s <- c(x_init,v_init)

m <- diag(1, n, n); m= rbind(m[-1,],0) # order cars
# simulation with front car suddenly breaking at t = 150
dat <- run(tmax=300,timeplot = F,table=T,after = 'if (t==150) state[2*n] = 0')

#matplot(data[,2:(n+1)],type='l',bty='n',xlab='time',ylab = 'x')
library(ggmatplot)
ggmatplot(dat[, 2:(n+1)], plot_type = "line", color = ncolors[4],
          linewidth = .4, linetype = 1, xlab = "Time",ylab = 'x') + 
  theme_minimal() + theme1 +
  theme(legend.position = 'none')

ggsave('media/ch4n/fig-ch4n-img6-old-54.png', width = 10, height = 6, units = 'in', dpi = 300)

# fig 4n.7 -----------------------------------------------------------------

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX = I
    return(list(c(dX)))
  })
}
p <- c(I=.01); s <- c(X=0)
bound <- 1
set.seed(1)
r1 <- run2(table=T,timeplot=T,method='euler', tstep=.1,
           tmax=500,after="state<-state+rnorm(1,mean=0,sd=0.1)*sqrt(tstep);
    if(abs(state)>bound) break",ymin=-bound,ymax=bound)
set.seed(8)
r2 <- run2(table=T,timeplot=T,method='euler', tstep=.1,
           tmax=500,after="state<-state+rnorm(1,mean=0,sd=0.1)*sqrt(tstep);
    if(abs(state)>bound) break",ymin=-bound,ymax=bound)
names(r1) <- c('time','X1')
names(r2) <- c('time','X2')
r <- merge(r1,r2,all=T)

library(rtdists)
h <- rdiffusion(29000,a=2,v=.01,t0=0,s=.1)
h1 <- h[h$response=='upper',]
h2 <- h[h$response=='lower',]

g1  <- ggplot(r, aes(x = time)) +
  geom_line(aes(y = X1), color = ncolors[4]) +
  geom_line(aes(y = X2), color = ncolors[4]) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = -1) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_vline(xintercept = 0) +
  #ylim(-1,1) +
  scale_y_continuous(breaks = c(0))+
  xlim(0,130)+
  labs(x = 'Time', y = 'X')+
  #coord_flip()+
  theme_minimal()+ theme1+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.x = unit(0, 'pt'))
g1plus <- g1 + 
  annotate("segment", x = 0, xend = 42, y = 0, yend = 1,
           arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  annotate('text', x = 20, y = 0.8, label = "Drift Rate: I", size = 20)+
  
  annotate("segment", x = 80, xend = 80, y = 0, yend = 1,
           arrow = arrow(ends='both',type = "closed", length = unit(0.1, "inches")))+
  annotate('text', x = 90, y = 0.5, label = "Bound: b", size = 20)+
  
  annotate("segment", x = 110, xend = 125, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")))+
  annotate('text', x = 115, y = -0.1, label = "Decision Time", size = 20) 
g1plus

g2 <- ggplot(h1,aes(x=rt)) +
  geom_histogram(binwidth = 5,fill = ncolors[4],color = "white", alpha = 0.7)+
  xlim(0,180)+
  theme_minimal()+
  theme(axis.line = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0,'pt'),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, -15, 0, "pt"))

g3 <-  ggplot(h2,aes(x=rt)) +
  geom_histogram(binwidth = 5,fill = ncolors[4],color = "white", alpha = 0.7)+
  xlim(0,180)+
  theme_minimal()+
  scale_y_reverse()+
  theme(axis.line = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0,'pt'),
        panel.grid = element_blank(),
        plot.margin = margin(-15, 0, 0, 0, "pt"))

gfinal <- g2/g1plus/g3 + plot_layout(heights = c(1, 5,1))
gfinal

ggsave('media/ch4n/fig-ch4n-img7-old-55.png', width = 10, height = 6, units = 'in', dpi = 300)

# fig 4n.9 -----------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR <- a*R+b*J
    dJ <- c*R+d*J
    return(list(c(dR, dJ)))
  })
}

# Using base2 -------------------------------------------------------------
png('media/ch4n/fig-ch4n-img9-old-57.png', width = 6, height = 5.45, units = 'in', res = 300)
layout(matrix(1:6,3,2,byrow=T))
par(mar=c(4.5,5,1,2)) #bott left top right
p <- c(a=-1,b=1,c=.5,d=-1) # parameters
s <- c(R=0.1,J=.1) 
#debug(run2)
run2(legend=F)
plane2(portrait=T,ymin=-1,xmin=-1,grid=3,vector=T,legend=F)
p <- c(a=-.2,b=-1,c=1,d=0) # parameters
run2(ymin=-.2,legend=F)
plane2(portrait=T,ymin=-1,xmin=-1,grid=2,tstep=.001,legend=F)
p <- c(a=-.1,b=-1,c=1,d=0.1) # parameters
run2(ymin=-.2,legend=F)
plane2(portrait=T,ymin=-1,xmin=-1,grid=3,tstep=.001,legend=F)
dev.off()

# fig 4n.10 ----------------------------------------------------------------
influence <- function(x,a=-8,b=1) sign(x)/(1+exp(a*(abs(x)-b)))
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dW <- influence(H,a,b)-rw*W+We
    dH <- influence(W,a,b)-rh*H+He
    return(list(c(dW,dH)))
  })
}

# using base2
png('media/ch4n/fig-ch4n-img10-old-58.png', width = 7, height = 6.36, units = 'in', res = 300)
layout(matrix(1:9,3,3,byrow=T))
par(mar=c(4,4,1,2))
p <- c(rw=.6,rh=.6,We=.18,He=-.18,a=-8,b=Inf)
s <- c(W=0,H=0)
for(b in c(Inf,0,1)){
  p['b']<- b
  curve2(influence(x,-8,b),-3,3,xlab='W',ylab='H',lwd=1)
  plane2(xmin=-2.5,xmax=2.5,ymin=-2,ymax=2,legend=F)
  for(i in seq(-2,2,by=.25)) newton(s=c(W=i,H=i),plot=T)
  for (i in 1:100)
    run2(state=c(W=rnorm(1,0,.5),H=rnorm(1,0,1)), tmax=50,ymin=-2,ymax=2,add=(i>1),
         legend=F)
}
dev.off()

# fig 4n.11 ----------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX <- (a + b * Y) * X - a * X^2 / K
    dY <- (c + d * X) * Y - c * Y^2 / K
    return(list(c(dX, dY)))
  }) 
}

## Using base2
png('media/ch4n/fig-ch4n-img11-old-59.png', width = 8, height = 4.82, units = 'in', res = 300)
layout(matrix(1:4,2,2))
par(mfrow=c(2,2))
# Set parameter values and run the model:
p <- c(K = 1, a = 0.4, b = -0.05, c=.4, d = -0.15)
s <- c(X = 0.01, Y = 0.01)
run2(method = "euler", tstep = 1, legend = FALSE)
plane2(portrait = TRUE,grid=4, legend = FALSE)
p <- c(K = 1, a = 0.05, b = -0.1,  c = 0.05, d = -0.09)
s <- c(X = 0.0126, Y = 0.01)
run2(tmax = 1500, method = "euler", tstep = 1, legend = FALSE)
plane2(portrait = TRUE,grid=4, legend = FALSE)
dev.off()

# fig 4n.12 ----------------------------------------------------------------
#polya urn model
library(ggmatplot)
set.seed(1)
urn=c(0,1,1)
urn_s=length(urn)
n=1000;m=50
urn[n]=NA
urns=matrix(mean(urn,na.rm=T),n,m)

for(j in 1:m)
{
  for(i in (urn_s+1):n)
  {
    s=sample(urn[1:(i-1)],size=1)
    urn[i]=s
    urns[i,j]=mean(urn[1:i])
  }
}
layout(matrix(1:2,1,2))

#matplot(urns[,1:20],type='l',ylim=c(0,1),ylab='p(blue)',xlab='turns',bty='n')
p1 <- ggmatplot(urns[,1:20], color = 'black',linetype = 'solid', linewidth = 0.2,
          plot_type = "line",xlab = 'Turns',ylab='p(blue)')+
  theme_minimal() + theme1 + theme(legend.position = 'none')
p1

p2 <- ggplot()+
  geom_histogram(aes(urns[n,]), binwidth = 0.1,
                 fill = ncolors[4], color = 'white') +
  xlab('p(blue)')+ylab('Frequency')+
  theme_minimal()+theme1
p2
#hist(urns[n,],main='',xlab='p(blue)',col='grey')

library(png) 
urns_png <- readPNG('media/ch4n/urns_new.png')
library(cowplot)
urnsgg <- ggdraw() + draw_image(urns_png)
# Plot + image
combined_p1_p2 <- plot_grid(p1, p2, ncol = 2)

# Arrange urnsgg above the combined p1 and p2
plot_grid(
  urnsgg,
  combined_p1_p2,
  ncol = 1,
  rel_heights = c(1.2, 0.8))

ggsave('media/ch4n/fig-ch4n-img12-old-60_above.png', width = 10, height = 20, units = 'in', dpi = 300)

# fig 4n.13 ----------------------------------------------------------------
png('media/ch4n/fig-ch4n-img13-old-61.png', width = 7, height = 4.22, units = 'in', res = 300)
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dA <- -A + b*T  
    dT <- -T + 1/(1+exp(-alpha*(A+beta)))
    return(list(c(dA, dT)))
  })
}
p <- c(b=1, alpha=12, beta=-.7) 
s <- c(A=0, T=0) 
# arousal increase for time t in 20:30, leads to panic, which after some time ('30 min') disappears
layout(1:2)
par(mar=c(4,4,1,2))
plane2(vector=T,xmin=0,ymin=0,xmax=1,ymax=1.1,legend=F) 
newton(s=c(A=0,T=0),plot=T)
newton(s=c(A=0.8,T=.8),plot=T)
newton(s=c(A=1,T=1),plot=T);
run2(after="if(t>20&t<30)state[1]<-1;state<-state+rnorm(2,mean=0,sd=0.1)")
dev.off()

# 4.14 --------------------------------------------------------------------
model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    dX <-  a + b*X - X^3        # cusp
    da <- -e*X
    return(list(c(dX,da)))
  })
}
png('media/ch4n/fig-ch4n-img14-old-62.png', width = 7, height = 4.5, units = 'in', res = 300)
s <- c(X=.1,a=0); # initial state and parameter values
layout(matrix(1:4,2,2,byrow=T))
par(mar=c(4,4,3,2))
p <- c(e=.05,b=-.5)
run2(ymin=-.1,main='b = -.5',legend=TRUE, c.legend.pos = 'bottomright')
plane2(xmax=2,ymin=-1,ymax=2,xmin=-2,portrait=T,grid=2,main='b = -.5', legend = FALSE)
p <- c(e=.05,b=1)
run2(ymin=-1.5,main='b = 1',legend=TRUE, c.legend.pos = 'bottomright')
plane2(xmax=2,ymin=-1,ymax=2,xmin=-2,portrait=T,grid=2,main='b = 1', legend = FALSE)
dev.off()

# 4. 16 -------------------------------------------------------------------
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dX <- a*Y + b*X - X^3 
    dY <- c*X + d*Y - Y^3
    return(list(c(dX, dY))) 
  })
}

s <- c(X=0,Y=0) 
png('media/ch4n/fig-ch4n-img16-old-64b.png', width = 8, height = 6.4, units = 'in', res = 300)
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

library(png) 
four_cases <- readPNG('media/ch4n/fig-ch4n-img16-old-64b.png')
bif_diagram <- readPNG('media/ch4n/cases_new.png')
library(cowplot)
four_cases_gg <- ggdraw() + draw_image(four_cases)
bif_diagram_gg <- ggdraw() + draw_image(bif_diagram)
# Plot + image
plot_grid( bif_diagram_gg, four_cases_gg,ncol = 1, rel_heights = c(2/5, 3/5))

ggsave('media/ch4n/fig-ch4n-img16-old-64_above.png', width = 8, height = 10, units = 'in', dpi = 300)

# fig 4n.17 --------------------------------------------------------------------
png('media/ch4n/fig-ch4n-img17-old-65.png', width = 6, height = 3.62, units = 'in', res = 300)
layout(1)
s <- c(X=0.1,Y=.1) 
p <- c(a=1,b=1,c=-1,d=1)
run2(tmax=20,tstep=0.1,ymin=-2,ymax=2)
dev.off()

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

antiRuntmp <- function(data, line = TRUE, psize = .4, lsize = .50, ...){
  dots <- list(...)
  p <- data %>% 
    pivot_longer(colnames(data)[-1], names_to = 'par', values_to = 'val') %>% 
    ggplot() +
    #geom_point(aes(time, val, shape = par), color = ncolors[3], size = psize, ...)+
    #scale_color_manual(values = c(ncolors[3]))+
    labs(y = 'Density', x = 'Time', color = '', shape = '', ...) +
    theme_minimal() + theme1+
    theme(legend.position = 'none', ...)
  if(line) pl <- p + geom_line(aes(time, val, group = par),color = ncolors[1], linewidth = lsize, ...)
  ifelse(line, return(pl), return(p))
}
pl <- antiRuntmp(dat)
b0_i <- rep(-.3,100); b0_i[34:66]=1 # for plotting attention
pl1 <- pl + geom_line(data = data.frame(
  time = 1:100,
  val = b0_i,
  par = "b0_i"
), aes(x = time, y = val), color = ncolors[2], size = 1, linetype = "dotted")+
  theme(
    )

pl2 <- pl1 + annotate("text", x = 80, y = 1.4, label = 'Percepts', size = 24, family = cfont) +
  annotate("text", x = 80, y = -0.5, label = 'Attention', size = 24, family = cfont)
pl2
ggsave('media/ch4n/fig-ch4n-img19-old-67.png', width = 10, height = 5.5, units = 'in', dpi = 300)
