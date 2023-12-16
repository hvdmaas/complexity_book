x=read.csv('Social impact theory experiment-table.csv',skip=6 )
library(ggmatplot)
x=x[order(x$X.step),]
ggmatplot(x=x$X.step,y=x$magnetization,group = x$X.run.number,
          plot_type = "line",xlab = 'time',ylab='proportion')
