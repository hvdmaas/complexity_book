
# -------------------------------------------------------------------------
# chapter 2 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("final-theme.R")

# fig 2.1 -----------------------------------------------------------------

n <- 15
r <- 2
x <- rep(0,n)
x[1] <- 2 # initial state X0 = 1 and thus X1 = 2
for (i in 1:(n - 1))
  x[i + 1] <- r * x[i]
#plot(x, type = 'b', xlab = 'time', bty = 'n')

ggplot(tibble(x), aes(x = row_number(x), y = x))+
  geom_line()+
  geom_point(shape = 21, fill = colors[4], size = 2, stroke = 1, color = 'white')+
  labs(x= 'time') + scale_x_continuous(breaks=seq(1,15,1))+
  theme_minimal() + theme1 

ggsave('fig-ch2-img1.jpg', width = 4, height = 2.5, units = 'in', dpi = 300)
dev.off()
# Fig 2.2 -----------------------------------------------------------------

n <- 15; r <- 2; x <- rep(0,n)
x[1] <- .01 # initial state
for (i in 1:(n - 1)){
  x[i + 1] = r * x[i] * (1 - x[i])
}
#plot(x, type = 'b', xlab = 'time', bty = 'n')
ggplot(tibble(time = rep(1:n), x = x), aes(x = time, y = x))+
  geom_line()+
  geom_point(shape = 21, fill = colors[4], size = 2, stroke = 1, color = 'white')+
  labs(x= 'time') + scale_x_continuous(breaks=seq(1,15,1))+
  theme_minimal() + theme1 
ggsave('fig-ch2-img2.jpg', width = 4, height = 2.5, units = 'in', dpi = 300)
dev.off()
# -------------------------------------------------------------------------
# Fig 2.3 -----------------------------------------------------------------

n <- 30
r <- 2
x <- matrix(0, n, length(seq(0, 0.7, by = 0.01)))
init <- seq(0, 0.7, by = 0.01)
for (i in 1:length(init)) {
  x[1, i] <- init[i]
  for (j in 2:n) {
    x[j, i] <- r * x[j - 1, i] * (1 - x[j - 1, i])
  }
}


ggmatplot(x,plot_type = "line", color = 'black',
          linewidth = .2, linetype = 1, xlab = "time",ylab = 'x')+ theme_minimal()+
  theme1 + theme(legend.position = 'none')

ggsave('fig-ch2-img3.jpg', width = 4, height = 2.5, units = 'in', dpi = 300)
dev.off()


# fig 2.4 -----------------------------------------------------------------

#plot(x, type = 'b', xlab = 'time', bty = 'n')
tmpFun <- function(r, n){
  x <- rep(0,n);
  x[1] <- .01 # initial state
  for (i in 1:(n - 1)){
    x[i + 1] = r * x[i] * (1 - x[i])
  }
  ggplot(tibble(time = rep(1:n), x = x), aes(x = time, y = x))+
    geom_line()+
    geom_point(shape = 21, fill = colors[4], size = 1.5, stroke = 0.75, color = 'white')+
    labs(title = paste0('r = ',r), x= 'time') + scale_x_continuous(breaks=seq(0,n,5))+
    theme_minimal() + theme1 
}
r2_9 <- tmpFun(r = 2.9, n = 30); r3_1 <- tmpFun(r = 3.1, n = 30)
r3_3 <- tmpFun(r = 3.3, n = 30); r3_5 <- tmpFun(r = 3.5, n = 30)

p <- (r2_9 + r3_1) / (r3_3 + r3_5)
ggsave(plot = p, 'fig-ch2-img4.jpg', width = 7, height = 5, units = 'in', dpi = 300)
 dev.off()


# fig 2.5 -----------------------------------------------------------------
 tmpFun(r = 4, n = 100)
 ggsave('fig-ch2-img5.jpg', width = 4, height = 2.5, units = 'in', dpi = 300)
 dev.off()

# fig 2.6 -----------------------------------------------------------------
 n <- 50; r <- 4; x <- rep(0,n)
 x[1] <- .001 # initial state
 for (i in 1:(n - 1)){
   x[i + 1] = r * x[i] * (1 - x[i])
 }
 y <- rep(0,n); y[1] <- .0010001
 # restart with sightly different initial state
 for (i in 1:(n - 1))
   y[i + 1] <- r * y[i] * (1 - y[i])
 ggplot(tibble(time = rep(1:n), x = x, y=y))+
   geom_line(aes(x = time, y = x), color = 'grey10')+
   geom_line(aes(x = time, y = y), color = colors[4], linetype = 'dashed')+
   #geom_point(shape = 21, fill = colors[4], size = 1.5, stroke = 0.75, color = 'white')+
   labs(title = paste0('r = ',r), x= 'time') + scale_x_continuous(breaks=seq(0,n,5))+
   theme_minimal() + theme1 
 ggsave('fig-ch2-img6.jpg', width = 4, height = 2.5, units = 'in', dpi = 300)
 dev.off()


# fig 2.7 -----------------------------------------------------------------
 ## Time plots
r <- 3.3; n <- 200; x1 <- rep(0,n)
x1[1] <- .001
for(i in 1:(n-1)) x1[i+1] = r*x1[i]*(1-x1[i])
##
r <- 4 ; x2 <- rep(0,n)
x2[1] <- .001
for(i in 1:(n-1)) x2[i+1] = r*x2[i]*(1-x2[i])
##
x3 <- runif(200,0,1)
##
x <- tibble(time = rep(1:n), 'r = 3.3' = x1, 'r = 4' = x2, 'random noise' = x3) 

Time_plots <- ggplot(x %>% 
         pivot_longer(-time, names_to= 'name', values_to = 'x') %>%  
         filter(time >= 100))+
  geom_line(aes(x = time, y = x, color = name), linewidth = .3)+
  labs(title = 'Time plots', x= 'time', y = 'x') + 
  scale_x_continuous(breaks=seq(100,n,n/10))+
  scale_y_continuous(limits = (0:1))+
  scale_color_manual(values = colors)+
  facet_wrap(~name)+
  theme_minimal() + theme1 + theme(legend.position = 'none')

  ## Phase plots
  
# Create the data frame
x1 <- x1[-1:-100]; x2 <- x2[-1:-100]; x3 <- x3[-1:-100]
phase_data_X <- tibble(
  'r = 3.3' = x1[-length(x1)], 'r = 4' = x2[-length(x2)], 'random noise' = x3[-length(x3)]) %>% 
  pivot_longer(everything(.),values_to = 'Xt', names_to = 'name')
phase_data_Xt <- tibble(
  'r = 3.3' = x1[-1], 'r = 4' = x2[-1], 'random noise' = x3[-1]) %>% 
  pivot_longer(everything(.),values_to = 'Xt+1', names_to = 'name') %>% 
  dplyr::select(-name)
phase_data_joined <- cbind(phase_data_X, phase_data_Xt)
  

# Create the scatterplot using ggplot2
Phase_plots <- ggplot(phase_data_joined, aes(x = Xt, y = `Xt+1`, color = name)) +
  geom_point(alpha = .3, shape = 1) +
  xlim(0, 1) + ylim(0, 1) +
  scale_color_manual(values = colors)+
  labs(title = 'Phase plots', x = "Xt", y = "Xt+1") +
  facet_wrap(~name)+
  theme_minimal() + theme1 + theme(legend.position = 'none')

Time_n_Phase <- Time_plots / Phase_plots
ggsave('fig-ch2-img7.jpg', width = 6, height = 4, units = 'in', dpi = 300)
dev.off()


# fig 2.8 -----------------------------------------------------------------
f <- function(r, x, n, m){
  x <- rep(x,n)
  for(i in 1:(n-1)) x[i+1] <- r*x[i]*(1-x[i])
  x[c((n-m):n)] # only return last m iterations
}
r.range <- seq(0, 2.5, by=0.01) 
r.range <- c(r.range,seq(2.5, 4, by=0.001)) 
n <- 200; m <-100 
equilibria <- as.vector(sapply(r.range, f,  x=0.1, n=n, m=m-1))
r <- sort(rep(r.range, m))
#plot(equilibria ~ r, pch=19,cex=.01,bty='n')
ggplot(tibble(equilibria, r), aes( x= r, y = equilibria)) +
  geom_point( alpha = .1, shape = 20, size = .01)+
  theme_minimal() + theme1 
ggsave('fig-ch2-img8.jpg', width = 8, height = 4, units = 'in', dpi = 300)
dev.off()