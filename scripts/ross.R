library(ggplot2)

ross_data<-read.csv("https://raw.githubusercontent.com/Neilblund/GW_course/master/data/ross.csv", row.names = "X")
rossplot<-qplot(data=subset(ross_data, logoil_gas_valuePOP_2000>0), logoil_gas_valuePOP_2000,polity_2000) + 
  theme_bw() + 
  ggtitle("Oil wealth and democracy") + 
  xlab("log oil/gas revenue") +
  ylab("polity score")

rossplot #show plot


rossplot<-rossplot+geom_smooth(method="lm",size=1.5) #add a best fit line

rossplot 

rossplot + facet_grid(~me_nafr)  #subset and plot by region
