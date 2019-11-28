library(ggplot2)
library(reshape2)
a=read.csv(file="G:\\Required\\6th Sem\\DA\\Lab\\Lab1\\all.csv", head = TRUE, sep = ",", nrows=30)
a

sp = melt(a)
sp
ggplot(sp, aes(x=District, y=value)) +
  geom_bar(aes(fill=variable), stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=75, hjust = 1))
