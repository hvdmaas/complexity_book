
# -------------------------------------------------------------------------
# chapter 3 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")

# fig 3. ------------------------------------------------------------------



# -------------------------------------------------------------------------

# fig 3.4 -----------------------------------------------------------------

#layout(t(1:3))
v=function(x,a) -a*x + x^3
#curve(v(x,a=-2),-3,3,bty='n')
#points(-1,-1.5,pch=16,cex=3,col =1)
#arrows(-1,-1.5,-1.5,-5,length=.1)
#curve(v(x,a=0),-3,3,bty='n')
#points(0,1.1,pch=16,cex=3,col =1)
#curve(v(x,a=2),-3,3,bty='n')
#points(.8,-.1,pch=16,cex=3,col =1)

ptmp <- function(a, point.x , point.y){
  v <- function(x,a) -a*x + x^3
  ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
    stat_function(fun = v, args = list(a = a), geom = "line", linewidth = .5) + 
    geom_point(data = data.frame(x = point.x, y = point.y), aes(x, y), shape = 16, size = 2.5, color = "black")+
    scale_x_continuous(limits = c(-3,3))+
    labs(y = expression(paste('V(X) = -', alpha, 'x + ',x^{3})),
         title = bquote(alpha == .(a)))+
    theme_minimal()+
    theme1 + theme(legend.position = 'none')
}
p1 <- ptmp(a = -2, point.x = -0.8, point.y = -.1)
p1 <- p1 + geom_segment(aes(x = -0.8, y = 0, xend = -1.8, yend = -5),
                  arrow = arrow(length = unit(0.15, "cm")))
p2 <- ptmp(a = 0, point.x = 0, point.y = 1.1)
p3 <- ptmp(a = 2, point.x = 0.8, point.y = -.2)
p <- p1 + p2 + p3
p
ggsave('media/ch3/fig-ch3-img4.jpg', width = 5, height = 3, units = 'in', dpi = 300)


# fig 3.5 -----------------------------------------------------------------
# old code
fh <- function(a) sign*sqrt(a/3)
sign <- 1;curve(fh,0,2,ylim=c(-1,1),xlim=c(-1,2),bty='n',xlab='a',ylab='X*',cex.lab=1)
sign <- -1;curve(fh,0,2,add=T,lty=3)

# new code
f <- function(a, sign) sign*sqrt(pmax(0, a/3))
p1 <- data.frame(a = seq(0, 2, by = 0.01)) %>% 
  filter(a >1) %>% 
  ggplot(aes(x = a)) +
  stat_function(fun = f, args = list(sign = 1), linewidth = .5) +
  ylim(-1, 1) +
  xlim(0, 2) +
  labs( x=expression(alpha),y='X*')+
  theme_minimal()+
  theme1 + theme(legend.position = 'none')
p1
p1 + stat_function(fun = f, args = list(sign = -1), linetype = 'dotted')
ggsave('media/ch3/fig-ch3-img5-old-17.jpg', width = 5, height = 3, units = 'in', dpi = 300)


# fig 3.17 -----------------------------------------------------------------

#layout(t(1)); par(mar=c(4,4,1,1))
x <- read.table('data/PNAS_patient_data.txt',header=T)
library(ecp) # if error: install.packages('ecp')
e1 <- e.divisive(matrix(x$dep,,1),sig=.01,min.size=10)
#plot(x$week,x$dep,type='b',pch=(e1$cluster-1)*16+1,xlab='Week',ylab='SLC-90',bty='n',main='Jump to depression')

if(length(e1$cluster) == nrow(x)){
  x <- cbind(x, 'cluster' = e1$cluster)
}
x %>% 
  ggplot(aes(x = week, y = dep, color = cluster))+
  geom_line(color = 'black', linewidth = .4) +
  geom_point(aes( fill = cluster), shape = 21, stroke = .5, color = 'black')+
  scale_fill_gradient(low = 'white', high = colors[1])+
  labs(x = 'Week', y = 'SLC-90')+
  theme_minimal()+
  theme1 + theme(legend.position = 'none')
ggsave('media/ch3/fig-ch3-img19-old-31.jpg', width = 5, height = 3, units = 'in', dpi = 300)


# fig 3.20 ----------------------------------------------------------------


x <- unlist(read.table('data/conservation_anticipation_item3.txt'))
library(mixtools) # if error: install.packages('mixtools')
result <- normalmixEM(x)
plot(result,whichplot=2,breaks=30)
p <- result$x %>% data.frame(x) %>% 
     ggplot(aes(x))+
      geom_histogram(aes(y=..density..), 
                 color = 'grey20', fill = 'grey80', linewidth = .3)+
    geom_density(color = colors[2], adjust = 2/3, linewidth = .3)+
  coord_flip()+
    theme1+theme_void()
p
## add custom photo
library(png) 
glass_png <- readPNG('media/ch3/glasses.png')
library(cowplot)
glasses <- ggdraw() +
  draw_image(glass_png,  x = 0, y = 0, scale = 0.9)
combined_plot <- plot_grid(
  glasses, p, ncol = 2, rel_widths = c(2/3, 1/3))
combined_plot

ggsave('media/ch3/fig-ch3-img20-old-32.jpg', width = 6, height = 3, units = 'in', dpi = 300)
