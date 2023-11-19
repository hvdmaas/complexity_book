# -------------------------------------------------------------------------
# chapter 6 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
source("r-figures-code/plane2T.R")
library(Grind)
# fig 6.1 -----------------------------------------------------------------

# GOOGLE

# 

# fig 6.2 -----------------------------------------------------------------

library(igraph);library(qgraph)
g1 <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) 
plot(g1) # an undirected network with 3 nodes
g2 <- graph( edges=c(1,2, 2,3, 3,1, 1,3, 3,3), n=3, directed=T ) 
plot(g2) # an directed network with self-excitation on node 3
get.adjacency(g2) # weight matrix
fcn <- make_full_graph(10) # a fully connected network
plot(fcn, vertex.size=10, vertex.label=NA)
layout(1)
set.seed(1)
adj <- matrix(rnorm(100,0,.2),10,10) # a weighted adjacency matrix
adj <- adj*sample(0:1,100,replace=T,prob=c(.8,.2)) # set 80% to 0

png('media/ch6/fig-ch6-img2-old-71_1of2.png', width = 4, height = 3, units = 'in', res = 300)
qgraph(adj, edge.color = ifelse(adj > 0, ncolors[5], ncolors[6]))
dev.off()

edge_density(fcn) # indeed 1
edge_density(graph_from_adjacency_matrix(adj,weighted=TRUE)) # indeed .2
centralityPlot(qgraph(adj)) + theme_minimal() + theme1 # note centrality() gives more indices
ggsave('media/ch6/fig-ch6-img2-old-71_2of2.jpg', width = 4, height = 4, units = 'in', dpi = 300)

# -------------------------------------------------------------------------
# fig 6.3 -----------------------------------------------------------------
library(igraph);library(qgraph)
png('media/ch6/fig-ch6-img3-old-72.png', width = 10, height = 7, units = 'in', res = 300)

layout(matrix(1:6,2,3))
par(mar=c(1,1,1,1))
plot(make_star(10, mode = "out"),main="make_star", 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white'
     )

plot(g <- sample_tree(20,method = "lerw"),main="sample tree",, 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(sample_smallworld(1, 50,  4,.012),main="sample_smallworld", 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
pm=matrix(.01,8,8);diag(pm)=1
plot(sample_sbm(40, pref.matrix=pm, block.sizes=rep(5,8)),main='sample_sbm (stochastic block)', 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(make_lattice(c(10, 10, 1),nei=1),main="make_lattice", 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(sample_pa(20),main="sample_pa (preferential attachment)", 
     vertex.color = ncolors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
dev.off()

# fig 6.6 -----------------------------------------------------------------
png('media/ch6/fig-ch6-img6-old-75.png', width = 6, height = 5, units = 'in', res = 300)
set.seed(1)
M <- matrix(rnorm(8^2,0.1,0.0),8,8)
M <- M*matrix(sample(0:1,8^2,replace=T,prob=c(.4,.6)),8,8)
M[diag(8)==1] <-  -.1
qgraph(M,diag=T,layout='circle',labels=paste('x',1:8,sep='',col=''), 
       edge.color = ifelse(M > 0, ncolors[5], ncolors[6]))
text(-.3,.3,'M',cex=3)
text(-.7,1,expression(a*X*(1-X/K)),cex=1.5)
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
  geom_line(aes(time, val, color = ab), linewidth = .25) +
  labs(y = 'Density', x = 'Time', color = '', linetype = '', shape = '') +
  ylim(c(NA,30))+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
dat1
tmpdata <- lapply(pplist, function(x) x[61,-1])
dataT <- do.call(rbind,tmpdata)
histplot <- tibble('x' = cor(dataT)[cor(dataT)<1]) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[3])+
  labs(title = 'Positive Manifold' ,x = 'Between test correlations')+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
histplot
dat1+histplot
ggsave('media/ch6/fig-ch6-img7-old-76.jpg', width = 6, height = 3.5, units = 'in', dpi = 300)


# 6.12 --------------------------------------------------------------------
library("IsingSampler")
n <- 10 # nodes
W <- matrix(.1,n,n); diag(W)=0
tau <- 0
N <- 1000 # replications
thresholds <- rep(tau, n)

dat1 <- IsingSampler(N, W, nIter=100, thresholds, beta = .1, responses = c(-1, 1))
hist1 <- tibble(x = apply(dat1,1,sum)) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[3])+
  labs(title = 'beta = .1' ,x = 'sum of x')+
  theme_minimal() + theme1+
  theme(legend.position = 'none')
hist1
dat2 <- IsingSampler(N, W, nIter=100, thresholds, beta = 2, responses = c(-1, 1))
hist2 <- tibble(x = apply(dat2,1,sum)) %>% 
  ggplot() +
  geom_histogram(aes(x), bins = 10, col = 'white', fill = ncolors[3])+
  labs(title = 'beta = 2' ,x = 'sum of x')+
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
plot(beta.range,dat,xlab='beta',ylab='alignment with thresholds',bty='n')

