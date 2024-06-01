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

name2=paste('network',paste0(clusters,collapse=''),'.jpeg',sep='',col='')
jpeg(name2,width = 5, height = 3, units = 'in', res = 300)
layout(t(1))
#qgraph(m,groups=group, layout="spring",labels=F,
#       vsize=1,edge.width=.4, bg=adjustcolor("white", alpha.f=0)) 
grayscale_colors <- colorRampPalette(c("black", "white"))(63)
qgraph(m,groups = group,layout = "spring",labels = FALSE,vsize = 1,edge.width = 0.4,bg = "white",color = grayscale_colors,border.color = "black",edge.color = "darkgray",label.color = "black") 

dev.off()
