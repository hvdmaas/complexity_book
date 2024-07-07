
# -------------------------------------------------------------------------
# chapter 3 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")

# fig 3.4 -----------------------------------------------------------------

v=function(x,a) -a*x + x^3

ptmp <- function(a, point.x , point.y){
  v <- function(x,a) -a*x + x^3
  ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
    stat_function(fun = v, args = list(a = a), geom = "line", linewidth = .5) + 
    geom_point(data = data.frame(x = point.x, y = point.y), aes(x, y), shape = 16, size = 5, color = "black")+
    
    labs(
      #y = expression(paste('V(X) = -', alpha, 'x + ',x^{3})),
      y = expression(paste('V(X)')),
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
ggsave('media/ch3/fig-ch3-img4-old-16.png', width = 10, height = 5, units = 'in', dpi = 300)


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
  stat_function(fun = f, args = list(sign = 1), linewidth = .75) +
  ylim(-1, 1) +
  xlim(0, 2) +
  labs( x=expression(alpha),y='X*')+
  theme_minimal()+
  theme1 + theme(legend.position = 'none')
p1
p1 + stat_function(fun = f, args = list(sign = -1), linetype = 'dotted', linewidth = .75)
ggsave('media/ch3/fig-ch3-img5-old-17.png', width = 10, height = 6, units = 'in', dpi = 300)


# fig 3.19 -----------------------------------------------------------------

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
  geom_point(aes( fill = cluster), size = 3, shape = 21, stroke = .5, color = 'black')+
  scale_fill_gradient(low = 'white', high = colors[4])+
  labs(x = 'Week', y = 'SLC-90')+
  theme_minimal()+
  theme1 + theme(
    legend.position = 'none',
    #axis.title.x = element_text(size = 60),
    #axis.title.y = element_text(size = 60),
    #axis.text.x  = element_text(size = 60),
    #axis.text.y  = element_text(size = 60)
    )
ggsave('media/ch3/fig-ch3-img19-old-31.png', width = 10, height = 6, units = 'in', dpi = 300)


# fig 3.20 ----------------------------------------------------------------


x <- unlist(read.table('data/conservation_anticipation_item3.txt'))
library(mixtools) # if error: install.packages('mixtools')
result <- normalmixEM(x)
plot(result,whichplot=2,breaks=30)
p <- result$x %>% data.frame(x) %>% 
     ggplot(aes(x))+
      geom_histogram(aes(y=..density..), 
                 color = 'grey20', fill = 'grey80', linewidth = .3)+
    geom_density(color = ncolors[2], adjust = 2/3, linewidth = .3)+
  coord_flip()+
    theme1+theme_void()
p
## add custom image
library(png) 
glass_png <- readPNG('media/ch3/glasses2.png')
library(cowplot)
glasses <- ggdraw() +
  draw_image(glass_png,  x = 0, y = 0, scale = 0.9)+
  draw_text('Up/down', x = 0.61, y = 0.7, size = 35, color = "black", family = cfont)
# Plot + image
combined_plot <- plot_grid(
  glasses, p, ncol = 2, rel_widths = c(2/3, 1/3))
combined_plot

ggsave('media/ch3/fig-ch3-img20-old-32.jpg', width = 6, height = 3, units = 'in', dpi = 300)


# fig 3.23 ----------------------------------------------------------------
library(cusp)
set.seed(10)
n <- 500
X1 <- seq(-1,1,le=n) # rnorm(n) #runif(1000) # independent variable 1
a0 <- 0; a1 <- 2; b0 <- 2 # to be estimated parameters
b0s <- seq(-1,2,by=.25)
i <- 0
dat <- matrix(0,length(b0s),7)
for (b0 in b0s){
  i <- i + 1
  Y1 <- Vectorize(rcusp)(1, a1 * X1, b0)
  data <- data.frame(X1, Y1) # collect ‘measured’ variables in data
  fit <- cusp(y ~ Y1, alpha ~ X1, beta ~ 1, data)
  sf <- summary(fit)
  dat[i, ] <- c(b0, sf$r2lin.r.squared[1], sf$r2cusp.r.squared[1],sf$r2lin.bic[1], sf$r2cusp.bic[1],sf$r2lin.aic[1], sf$r2cusp.aic[1])
}

plotdat <- bind_cols(b = dat[,1], linear = dat[,4], cusp = dat[,5]) 
p <- plotdat %>% 
  pivot_longer(2:3,names_to = 'mod', values_to = 'bic') %>% 
  ggplot(aes(x = b, y = bic))+
  geom_line(aes(color = mod, linetype = mod), linewidth = .6)+
  geom_point(aes(color = mod, shape = mod), size = 3)+
  geom_vline(xintercept = 0, linetype = 'dotted')+
  ylim(600, 1400) +
  scale_x_continuous(n.breaks = 6) +
  scale_color_manual(values = c(ncolors[1:2]))+
  labs( x=expression(beta),
        y='BIC', 
        color = '', 
        shape = '', linetype = '')+
  theme_minimal()+
  theme1 + theme1 + theme(
    legend.position = 'right'
    )
p + annotate("text", x=-.5, y=800, label="no hysteresis",
            color="grey20", size = 20) +
  annotate("text", x=.5, y=800, label="hysteresis",
           color="grey20", size = 20) 

ggsave('media/ch3/fig-ch3-img23-old-35.png', width = 10, height = 6, units = 'in', dpi = 300)

# fig 3.24 ----------------------------------------------------------------

#x <- read.table('data/stoufer.txt')
#colnames(x) <- c('IntensityofFeeling','Attitude')
#fit <- cusp(y ~ Attitude, alpha ~ IntensityofFeeling, beta ~ IntensityofFeeling, x)
#summary(fit)
#plot(fit, 'bifurcation')


# fig 3.25 ----------------------------------------------------------------
x <- read.table('data/bentler.txt',header=TRUE)
#layout(t(1:8))
age <- c('age 4 to 4.5','age 4.5 to 5','age 5 to 5.5','age 5.5 to 6','age 6 to 6.5','age 6.5 to 7','age 7 to 7.5','age 7.5 to 8')
x %>% 
  tibble() %>% 
  mutate(age_range = factor(age_range, labels = age)) %>% 
  ggplot()+
  geom_bar(aes(score), fill = colors[4])+ coord_flip()+
  facet_wrap(~age_range, nrow = 1)+
  scale_x_continuous(breaks=seq(0,12,1))+
  labs(x = 'sumscore', y = '')+
  theme_minimal()+
  theme1 + theme(axis.text.x = element_blank(),
                 strip.text = element_text(colour = "grey10", size = 38))
ggsave('media/ch3/fig-ch3-img25-old-37.png', width = 11, height = 6, units = 'in', dpi = 300)

