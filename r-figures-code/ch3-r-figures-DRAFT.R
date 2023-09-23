


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
ggsave('fig-ch3-img4.jpg', width = 5, height = 3, units = 'in', dpi = 300)


# fig 3.5 -----------------------------------------------------------------

f <- function(a) sign*sqrt(a/3)
sign <- 1;curve(f,0,2,ylim=c(-1,1),xlim=c(-1,2),bty='n',xlab='a',ylab='X*',cex.lab=1)
sign <- -1;curve(f,0,2,add=T,lty=3)


f <- function(a, sign) sign*sqrt(a/3)
p1 <- ggplot(expand.grid(a = seq(0, 2, by = 0.01), aes(x = a))) +
    stat_function(fun = f, args = list(sign = 1), linewidth = .5) +
    ylim(-1, 1) +
    xlim(-1, 2) +
    theme_minimal()+
    theme1 + theme(legend.position = 'none')
p1 + stat_function(fun = f, args = list(sign = -1), linetype = 'dotted')

