# -------------------------------------------------------------------------
# chapter 6 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
library(Grind)
library(IsingSampler)
library(igraph);library(qgraph)
# fig 6.1 -----------------------------------------------------------------

# GOOGLE
library(readxl)
scholar_statistics <- read_excel("r-figures-code/scholar statistics.xlsx")
scholar_statistics %>% ggplot()+
  geom_line(aes(Year, Citations), linewidth = .5,color = 'grey15')+
  geom_point(aes(Year, Citations), size = 2, shape = 21, stroke = 1, 
             color = 'white', fill = 'grey15')+
  scale_x_continuous(breaks = scholar_statistics$Year)+
  theme_minimal()+theme1
ggsave('media/ch6/fig-ch6-img1-old-70.jpg', width = 5, height = 3, units = 'in', dpi = 300)

# fig 6.2 -----------------------------------------------------------------


#g1 <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) 
#plot(g1) # an undirected network with 3 nodes
#g2 <- graph( edges=c(1,2, 2,3, 3,1, 1,3, 3,3), n=3, directed=T ) 
#plot(g2) # an directed network with self-excitation on node 3
#get.adjacency(g2) # weight matrix
#fcn <- make_full_graph(10) # a fully connected network
#plot(fcn, vertex.size=10, vertex.label=NA)
layout(1)
set.seed(1)
adj <- matrix(rnorm(100,0,.2),10,10) # a weighted adjacency matrix
adj <- adj*sample(0:1,100,replace=T,prob=c(.8,.2)) # set 80% to 0

png('media/ch6/fig-ch6-img2-old-71_1of2.png', width = 4, height = 3, units = 'in', res = 300)
qgraph(adj, edge.color = ifelse(adj > 0, ncolors[5], ncolors[6])
       #,negDashed = TRUE #remove dashed line or not?
       )
dev.off()

library(png)
library(grid)
library(gridExtra)
p1 <- readPNG("media/ch6/fig-ch6-img2-old-71_1of2.png")
p1g <- rasterGrob(p1)

p2 <- centralityPlot(qgraph(adj)) + theme_minimal() + theme1 +
  theme(axis.text.x = element_text(size = 15))
p2
png('media/ch6/fig-ch6-img2-old-71.png', width = 14, height = 7, units = 'in', res = 300)
grid.arrange(arrangeGrob(p1g, p2, ncol = 2, widths = c(3/5, 2/5)))
dev.off()

# -------------------------------------------------------------------------
# fig 6.3 -----------------------------------------------------------------
png('media/ch6/fig-ch6-img3-old-72.png', width = 8, height = 11, units = 'in', res = 300)

layout(matrix(1:6,3,2))
par(mar=c(1,1,1,1))
par(family = cfont, cex = 1.1)
plot(make_star(10, mode = "out"), main="make_star", 
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA
     )

plot(g <- sample_tree(20,method = "lerw"),main="sample tree",
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA
     )
plot(sample_smallworld(1, 50,  4,.012),main="sample_smallworld", 
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA)
pm=matrix(.01,8,8);diag(pm)=1
plot(sample_sbm(40, pref.matrix=pm, block.sizes=rep(5,8)),main='sample_sbm (stochastic block)', 
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA)
plot(make_lattice(c(10, 10, 1),nei=1),main="make_lattice", 
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA)
plot(sample_pa(20),main="sample_pa (preferential attachment)", 
     vertex.color = 'white', 
     edge.color = ncolors[4],
     vertex.frame.color = 'black',
     vertex.label = NA)
dev.off()

# fig 6.6 -----------------------------------------------------------------
png('media/ch6/fig-ch6-img6-old-75.png', width = 6, height = 5, units = 'in', res = 300)
set.seed(1)
layout(1)
par(mar=c(4,4,4,4))
M <- matrix(rnorm(8^2,0.1,0.0),8,8)
M <- M*matrix(sample(0:1,8^2,replace=T,prob=c(.4,.6)),8,8)
M[diag(8)==1] <-  -.1
qgraph(M,diag=T,layout='circle',labels=paste('x',1:8,sep='',col=''), 
       edge.color = ifelse(M > 0, ncolors[5], ncolors[6]), 
       edge.width = .75,
       mar = c(5,5,5,5))
text(-.3,.3,'M',cex=3, family = cfont)
text(-.7,1.1,expression(a*X*(1-X/K)),cex=1.5)
dev.off()

# fig 6.7 -----------------------------------------------------------------

mutualism <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    X <- state[1:nr_var]
    dX <- a*X*(1-X/k) + a*(X * M %*% X)/k # using matrix multiplication
    return(list(dX))
  })
}

layout(matrix(1:2,1,2))
nr_var <- 12 # number of tests, abilities (W)
nr_of_pp <- 500
data <- matrix(0,nr_of_pp,nr_var) # to collect the data in the simulation
M <- matrix(.05,nr_var,nr_var)
M[diag(nr_var)==1] <- 0 # set diagonal of M to 0

pplist <- list()
for(i in 1:nr_of_pp)
{
  # sample a,K, starting values X from normal distributions for each person separately
  # note M is constant over persons.
  a <- rnorm(nr_var,.2,.05) 
  k <- rnorm(nr_var,10,2)
  x0 <- rnorm(nr_var,2,0.1) # initial state of X
  s  <- x0;p <- c() # required for grind
  tmp <- run(odes=mutualism ,tmax=60,table = TRUE,  timeplot = FALSE, legend=T) # collect data (end points)
  #plot person 1 only
  pplist[[i]] <- tmp
}

pplist[[1]] # One person all Time points (61) all abilities (12)

dat1 <- pplist[[1]] %>% 
  pivot_longer(colnames(.)[-1], names_to = 'ab', values_to = 'val') %>% 
  ggplot() +
  geom_line(aes(time, val, group = ab), color = 'grey5',linewidth = .25) +
  labs(y = 'Density', x = 'Time', color = '', linetype = '', shape = '') +
  ylim(c(NA,30))+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
dat1
tmpdata <- lapply(pplist, function(x) x[61,-1])
dataT <- do.call(rbind,tmpdata)
histplot <- tibble('x' = cor(dataT)[cor(dataT)<1]) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[4])+
  labs(title = 'Positive Manifold' ,x = 'Between test correlations', y = 'Frequency')+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
histplot
dat1+histplot
ggsave('media/ch6/fig-ch6-img7-old-76.jpg', width = 6, height = 3.5, units = 'in', dpi = 300)

# 6.12 --------------------------------------------------------------------

n <- 10 # nodes
W <- matrix(.1,n,n); diag(W)=0
tau <- 0
N <- 1000 # replications
thresholds <- rep(tau, n)

dat1 <- IsingSampler(N, W, nIter=100, thresholds, beta = .1, responses = c(-1, 1))
hist1 <- tibble(x = apply(dat1,1,sum)) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[4])+
  labs(title = 'beta = .1' ,x = 'sum of x', y = 'Frequency')+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
hist1
dat2 <- IsingSampler(N, W, nIter=100, thresholds, beta = 2, responses = c(-1, 1))
hist2 <- tibble(x = apply(dat2,1,sum)) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[4])+
  labs(title = 'beta = 2' ,x = 'sum of x', y = 'Frequency')+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
hist2
hist1 + hist2
ggsave('media/ch6/fig-ch6-img12-old-81.jpg', width = 6, height = 3, units = 'in', dpi = 300)

# fig 6.13 ----------------------------------------------------------------
layout(1)
N <- 400
n <- 10
W <- matrix(.1,n,n); diag(W) <- 0
thresholds <- sample(c(-.2,.2),n,replace=T) # a random pattern of thresholds
dat <- numeric(0)
beta.range <- seq(0,3,by=.05)
for(beta in beta.range)
{
  data <- IsingSampler(N, W, nIter = 100, thresholds, beta = beta, responses = c(-1, 1))
  dat <- c(dat,sum(thresholds*apply(data,2,sum))) # a simple measure of alignment
}

tibble(x = beta.range, y = dat) %>% 
  ggplot() +
  geom_point(aes(x = x, y = y), shape = 1, size = 1)+
  labs(x = 'Beta', 
       y = 'Alignment with thresholds')+
  theme_minimal() + theme1
ggsave('media/ch6/fig-ch6-img13-old-82.jpg', width = 5, height = 3, units = 'in', dpi = 300)

# fig 6.14 ------------------------------------------------------------------
hamiltonian <- function(x,n,t,w) -sum(t*x)-sum(w*x%*%t(x)/2)
glauber_step <- function(x,n,t,w,beta)
{
  i = sample(1:n,size=1) # take a random node
  x_new=x;x_new[i]=x_new[i]*-1 # construct new state with flipped node
  p=1/(1+exp(beta*(hamiltonian(x_new,n,t,w)-hamiltonian(x,n,t,w))))  # update probability
  if(runif(1)<p) x=x_new # update state
  return(x)
}

png('media/ch6/fig-ch6-img14-old-83.png', width = 7, height = 4, units = 'in', res = 300)
layout(t(1:2))
epsilon <- .002;lambda <- .002 # low values = slow time scale
n <- 10
W <- matrix(rnorm(n^2,0,.1),n,n); W <- pmax(W,t(W)) # to make W symmetric
diag(W) <- 0
qgraph(W, edge.color = ifelse(W > 0, ncolors[5], ncolors[6]),
       mar = c(3,3,5,3))
title('Before learning', family = cfont)
thresholds <- rep(.2, n)
x <- sample(c(-1,1),n,replace=T)
for(i in 1:500)
{
  x <- glauber_step(x,n,thresholds,W,beta=2)
  W <- W+epsilon*(1-abs(W))*outer(x,x,"*")-lambda*W # Hebbian learning
  diag(W) <- 0
}
round(W,2)
qgraph(W, edge.color = ifelse(W > 0, ncolors[5], ncolors[6]),
       mar = c(3,3,5,3))
title('After learning', family = cfont)
dev.off()

# fig 6.15 ----------------------------------------------------------------

f <- function(x,a,b,c,d) (1/6)*x^6-.25*d*x^4-(1/3)*c*x^3-.5*b*x^2-a*x
layout(matrix(1:9,1,9))
par(mar=c(1,1,1,1))
a=0;c=0;d=5
for(b in 1:-7)
  curve(f(x,a,b,c,d),-2.8,2.8,axes=F,bty='n',ylab='',xlab='')

# fig 6.16 ----------------------------------------------------------------
library(png)
library(grid)
library(gridExtra)

mutualism <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    X <- state[1:nr_var]
    dX <- a*X*(1-X/k) + a*(X * M %*% X)/k # using matrix multiplication
    return(list(dX))
  })
}
layout(1)
## Cross-Sectional (A)

## The mutualism simulated data
nr_var <- 12 # number of tests, abilities (W)
nr_of_pp <- 500
data <- matrix(0,nr_of_pp,nr_var) # to collect the data in the simulation
M <- matrix(.05,nr_var,nr_var)
##
M[,1] <- .2 # strong influence of X1 on all others
M[2,] <- .2 # strong influence on X2 by all others
M[diag(nr_var)==1] <- 0 # set diagonal of m to 0

for(i in 1:nr_of_pp)
{
  # sample a,K, starting values X from normal distributions for each person separately
  # note M is constant over persons.
  a <- rnorm(nr_var,.2,.05) 
  k <- rnorm(nr_var,10,2)
  x0 <- rnorm(nr_var,2,0.1) # initial state of X
  s  <- x0;p <- c() # required for grind
  data[i,] <- run(odes=mutualism ,tmax=60, timeplot = (i==1),legend=F) # collect data (end points)
  #plot person 1 only
}

cormat <- cor_auto(data) #cor matrix

nw <- EBICglasso(cormat, nrow(data),gamma = 0.5) #EBIC

png('media/ch6/fig-ch6-img16-old-85A1.png', width = 4, height = 4, units = 'in', res = 300)
qnw <- qgraph(nw, layout = 'spring',
       edge.color = ifelse(nw > 0, ncolors[5], ncolors[6]))
dev.off()

A1 <- readPNG("media/ch6/fig-ch6-img16-old-85A1.png")
A1g <- rasterGrob(A1)
A2 <- centralityPlot(qnw,include = c( "Betweenness","Closeness","Strength", "ExpectedInfluence"),
                     scale = "z-scores") + scale_x_continuous(breaks = c(0,1,2,3))+
  theme_minimal() + theme1 + theme(axis.text = element_text(size = 14),
                                   strip.text = element_text(size = 11), 
                                   plot.margin = unit(c(0.1,0.1,0.1,0), "cm"))
A2
A<- grid.arrange(arrangeGrob(A1g, A2, ncol = 2, widths = c(2/5, 3/5)))
## Time series
library(graphicalVAR)
# make time series for one persons with some stochastic effects
data <- run(odes=mutualism, table = T, tmax=1000,timeplot = (i==1),legend=F, after="state<-state+rnorm(nr_var,mean=0,sd=1);state[state<0]=.1")
data <- data[,-1]
colnames(data) <- vars <- paste('X',1:nr_var,sep='',col='')
fit <- graphicalVAR(data[50:1000,], vars = vars, gamma=0, nLambda = 5)

png('media/ch6/fig-ch6-img16-old-85B1.png', width = 4, height = 4, units = 'in', res = 300)
par(mar=c(3,3,3,3))
plot(fit,"PDC", titles = FALSE,
     edge.color = ncolors[5],
     mar = c(6,6,8,6))
title("Partial Directed Correlations", family = cfont)
dev.off()

B1 <- readPNG("media/ch6/fig-ch6-img16-old-85B1.png")
B1g <- rasterGrob(B1)

B2 <- centralityPlot(fit$PDC) +
  theme_minimal() + theme1 + theme(axis.text = element_text(size = 14),
                                   strip.text = element_text(size = 11), 
                                   plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
B <- grid.arrange(arrangeGrob(B1g, B2, ncol = 2, widths = c(2/5, 3/5)))

png('media/ch6/fig-ch6-img16-old-85.png', width = 10, height = 8, units = 'in', res = 300)
grid.arrange(A, B)
dev.off()
# fig 6.17 --------------------------------------------------------------------

# MISSING 14/1

# fig 6.18 ----------------------------------------------------------------

library(IsingSampler)
library(IsingFit)
set.seed(1)
n <- 8
W <- matrix(runif(n^2,0.5,2),n,n); # random positive matrix
W <- W * matrix(sample(0:1,n^2,prob=c(.8,.2),replace=T),n,n) # delete 90% of nodes
W <- pmax(W,t(W)) # make symmetric 
diag(W) <- 0
ndata <- 1000
thresholds <- rnorm(n,0,1) 
data <- IsingSampler(ndata, W, thresholds, beta = .5, responses = c(-1, 1))
fit <- IsingFit(data,family='binomial', plot=FALSE)
png('media/ch6/fig-ch6-img18-old-87.png', width = 8, height = 4, units = 'in', res = 300)
layout(t(1:3))
qgraph(W,fade = FALSE,
       edge.color = ifelse(W > 0, ncolors[5], ncolors[6]),
       negDashed = TRUE,
       mar = c(3,3,5,3))
title("Original network",cex.main=2, family = cfont)
qgraph(fit$weiadj,fade = FALSE,
       edge.color = ifelse(W > 0, ncolors[5], ncolors[6]),
       negDashed = FALSE, mar = c(3,3,5,3))
title("Estimated network",cex.main=2, family = cfont)
plot(thresholds,
     type='p',bty='n',
     xlab='node',ylab='Threshold',
     cex=2,cex.lab=1.5, axes = FALSE, family = cfont)
axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)  # custom axis
axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont) 
lines(fit[[2]],lwd=1.5)
dev.off()

# fig 6.19 ----------------------------------------------------------------
layout(1)
Obama <- read.table("data/Obama.txt",header=T) # see book data folder
ObamaFit <- IsingFit(Obama,plot=F)
ObamaiGraph<- graph_from_adjacency_matrix(abs (ObamaFit$weiadj), 'undirected', weighted = TRUE,     add.colnames = FALSE)
ObamaCom <- cluster_walktrap(ObamaiGraph)
png('media/ch6/fig-ch6-img19-old-88.png', width = 6, height = 4, units = 'in', res = 300)

qgraph(ObamaFit$weiadj, 
       layout = 'spring', 
       cut = .8, groups = communities(ObamaCom), 
       legend = FALSE,
       edge.color = ifelse(W > 0, ncolors[5], ncolors[6]),
       color = c('white',
                 ncolors[7],
                 ncolors[3],
                 ncolors[8]),
       negDashed = FALSE)
dev.off()

# extra network -----------------------------------------------------------
library(igraph)
library(colorspace)
library(qgraph)

hsbm <- function (cluster_sizes,strengths=NULL,plot=T)
{
  k=cluster_sizes # cluster sizes
  levels=length(k) # hierarchic levels
  n=prod(k) # nodes
  if(n > 2000) print('Warning: ', n, 'nodes', 'might be too much for qgraph')
  if(length(k)!=length(strengths)) { print ('cluster_sizes and strengths dont match');stop()}
  group=rep(0,n) # node group
  m=matrix(levels,n,n) # connection matrix
  for(l in (levels-1):1)
    for(i in 1:n)
    {for(j in 1:n)
      if((i-1)%/%prod(k[1:l])==(j-1)%/%prod(k[1:l])) m[i,j]=l
    group[i]=(i-1)%/%prod(k[1:l])
    }
  
  if(plot) image(1/(m+.5))
  
  if(length(strengths)<1) m <- 1/m^3 else
  {
    for(i in 1:levels)
      m[m==i]=strengths[i]
  }
  return(list(m,group))
}

clusters=c(4,4,4,4,4);strengths = c(1,.3,.1,.02,.005)
clusters=c(4,4,4,4);strengths = c(.7,.1,.04,.008)


h=hsbm(clusters,strengths)
m <- h[[1]]
group=h[[2]]

#name2=paste('network',paste0(clusters,collapse=''),'.jpg',sep='',col='')
png('media/ch6/fig-ch6-extra.png',width = 5, height = 3, units = 'in', res = 300)
layout(t(1))
qgraph(m,groups=group, layout="spring",labels=F,
       vsize=1.5,edge.width=.4, bg=adjustcolor("white", alpha.f=0),
       mar = c(1,1,1,1)) 
dev.off()

