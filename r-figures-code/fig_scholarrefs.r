library(openxlsx)
library(ggplot2)
dat=read.xlsx('scholar statistics.xlsx',sheet=1)
head(dat)
ggplot(dat, aes(x = Year)) +
  geom_line(aes(y = Citations)) +
  labs(color = "Year") +
  ylab('Citations') + theme_minimal()

