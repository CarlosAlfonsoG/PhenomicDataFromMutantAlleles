library(ggplot2)
wt$id <- as.numeric(wt$id)
totals <-rbind(wt, prg)
p <- c("4", "5", "6", "7", "8", "9") 
complot <- ggplot(subset(totals,polygonNo == p), aes(x=id, y=freq, colour=list2, group=list2)) +
  geom_point() +
  geom_line() + facet_wrap(~ polygonNo) 
complot + theme_minimal()




ggplot(subset(df2,polygonNo == p), aes(x=id, y=freq, group=polygonNo)) +
  geom_line(aes(color=polygonNo))+
  geom_point(aes(color=polygonNo))
scale_x_continuous(breaks = seq(0, 20, by = 5))
theme(legend.position="top")
