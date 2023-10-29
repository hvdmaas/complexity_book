# -------------------------------------------------------------------------
# chapter 6 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")

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
qgraph(adj, edge.color = ifelse(adj > 0, colors[1], colors[2]))
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
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white'
     )

plot(g <- sample_tree(20,method = "lerw"),main="sample tree",, 
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(sample_smallworld(1, 50,  4,.012),main="sample_smallworld", 
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
pm=matrix(.01,8,8);diag(pm)=1
plot(sample_sbm(40, pref.matrix=pm, block.sizes=rep(5,8)),main='sample_sbm (stochastic block)', 
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(make_lattice(c(10, 10, 1),nei=1),main="make_lattice", 
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
plot(sample_pa(20),main="sample_pa (preferential attachment)", 
     vertex.color = colors[1], 
     edge.color = 'grey50',
     vertex.frame.color = 'grey50',
     vertex.label.family = cfont,   # Change font family (e.g., "sans" for sans-serif)
     vertex.label.font = 1, #1:normal 2: bold
     vertex.label.color = 'white')
dev.off()

