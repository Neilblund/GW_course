rossplot<-qplot(data=subset(ross_data, logoil_gas_valuePOP_2000>0), logoil_gas_valuePOP_2000,polity_2000) + 
  theme_bw() + 
  ggtitle("Oil wealth and democracy") + 
  xlab("log oil/gas revenue") +
  ylab("polity score")



rossplot<-rossplot+geom_smooth(method="lm",size=1.5)

rossplot

rossplot + facet_grid(~me_nafr)  
