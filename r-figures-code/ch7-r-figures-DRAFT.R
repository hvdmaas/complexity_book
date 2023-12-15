# -------------------------------------------------------------------------
# chapter 7 R- Figures ----------------------------------------------------
# -------------------------------------------------------------------------
source("r-figures-code/final-theme.R")
source("r-figures-code/run2_and_timePlot2.R")

# -------------------------------------------------------------------------

resample <- function(x, ...) x[sample.int(length(x), ...)]
n <- 1000
x <- vector("list", n)
iter <- 100000
n_possible_words <- 1000000 # should be very high
total_words <- total_unique_words  <-  numeric(iter)
for(i in 1:iter)
{
  j <- sample(1:n,1) # speaker
  k <- sample((1:n)[-i],1) # hearer
  # if speaker has no words, make one up (actually just number)
  if(length(x[[j]])==0) x[[j]][1] <- sample(1:n_possible_words,1) else 
  {
    spoken_word <- resample(x[[j]],1) # choose a word (actually just number)
    if(any(x[[k]]%in%spoken_word)) # hearer knows the word
    {
      x[[j]] <- spoken_word # erase list except spoken_word
      x[[k]] <- spoken_word # erase list except spoken_word
    } else # hearer does not know the word
      x[[k]] <- c(x[[k]],spoken_word)  # add word to list
  } 
  total_words[i] <- length(unlist(x))
  total_unique_words[i] <- length(unique(unlist(x)))
}
layout(1)
#plot(total_words,type='l',xlab='time',bty='n')
png('media/ch7/fig-ch7-img3-old-91.png', width = 6, height = 3, units = 'in', res = 300)
plot(total_unique_words,type='l',xlab='Time',ylab = 'total unique words',bty='n',
      axes = FALSE, family = cfont)
axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)  # custom axis
axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)
dev.off()

# fig 7.4 -----------------------------------------------------------------
layout(t(1:2))
resample <- function(x, ...) x[sample.int(length(x), ...)] # as sample(5,1) acts odd
n <- 100
iter <- 50000
plot_fun <- function(F, Q){
  uniques <- numeric(iter)
  x <- matrix(sample(1:Q,replace=T,n*F),n,F)
  for(i in 1:iter)
  {
    j <- sample(1:n,1)       # agent 1
    k <- sample((1:n)[-j],1) # agent 2
    w <- sum(x[j,]==x[k,])/F # agreement
    if(w<1 & runif(1)<w) {
      f <- resample(which(x[j,]!=x[k,]),1) # which (unequal) feature to update
      x[j,f] <- x[k,f] # update
    }
    uniques[i] <- nrow(unique(x))
  }
  
  plot(uniques[1:i],type='l',lwd=2,xlab='Time',ylab='# unique cultures',
       bty='n',ylim=c(0,80),main=paste('Axelrod model with F = ',F, ', Q = ', Q),
       axes = FALSE, family = cfont)
  axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)  # custom axis
  axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)
}
png('media/ch7/fig-ch7-img4-old-92.png', width = 5, height = 7, units = 'in', res = 300)
layout(1:2)
plot_fun(F = 4, Q = 4)
plot_fun(F = 2, Q = 10)
dev.off()

# -------------------------------------------------------------------------


# fig 7.7  ----------------------------------------------------------------
png('media/ch7/fig-ch7-img7-old-95.png', width = 8, height = 5, units = 'in', res = 300)

FJ <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    X <- state[1:n]
    M <- M/apply(M,1,sum) # weights sum to 1
    dX <- (1-g) * M %*% X  + g * X - X
    return(list(dX))
  })
}
n <- 100
M <- matrix(runif(n^2,0,1),n,n)
g <- 0.95 # if g  = 0 => DeGroot model
x0 <- runif(n,0,1)
s <- x0;p  <- c() 
layout(1)
run2(odes=FJ,method='euler',tmax=100, legend = FALSE)
dev.off()
# fig 7.8 -----------------------------------------------------------------
png('media/ch7/fig-ch7-img8-old-96.png', width = 8, height = 6, units = 'in', res = 300)

set.seed(20)
layout(matrix(1:4, 2, 2))
iter <- 50; mu <- .5; n <- 200
for (bound in c(.1, .2, .3, .5))
{
  x <- runif(n, 0, 1)
  dat <- matrix(0, iter, n)
  for (i in 1:iter)
  {
    y <- sample(x, n, replace = T) # find an partner for every agent
    x <- ifelse(abs(x - y) < bound, x + mu * (y - x), x)
    dat[i, ] = x
  }
  matplot(dat,type='l',col=1,lty=1,bty='n',xlab='',
          ylab='opinion',main=paste('bound = ',bound),
          axes = FALSE, family = cfont)
  axis(1, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)  # custom axis
  axis(2, at = NULL, labels = TRUE, tcl = 0, cex.axis = 1, family = cfont)
}
dev.off()

# -------------------------------------------------------------------------

# fig 7.9 -----------------------------------------------------------------

HK <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    X = state[1:n]
    accepted <- abs(outer(X,X,'-')) < bound # acceptable neighbors
    M <- accepted * M
    M <- M/apply(M,1,sum) # normalize
    dX <- M  %*% X  - X
    return(list(dX))
  })
}
layout(1)
n=100
M=matrix(runif(n^2,0,1),n,n) * matrix(sample(0:1,n^2,replace=T),n,n)
bound <- .1
x0 <- runif(n,0,1)
s <- x0;p <- c() 
png('media/ch7/fig-ch7-img9-old-97.png', width = 8, height = 6, units = 'in', res = 300)
run2(odes=HK,tstep=1,method='euler',tmax=20,legend=FALSE)
dev.off()
