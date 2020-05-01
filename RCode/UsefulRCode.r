##### Filename:  /Users/x644435/Desktop/RCode/UsefulRCode.r
# source("/nfs/science/users/xiaojiez/Rcode/UsefulRCode.r")
# source("/Users/x644435/Desktop/RCode/UsefulRCode.r")

#####Example 1:  Multiple plots on one page#####
library(ggplot2)
library(grid)
#--- individual plots
  g1= ggplot(data=mtcars,aes(x=mpg, y=qsec)) + geom_line(colour="brown") + ylab("qsec") + xlab(" ") 
  g2= ggplot(data=mtcars,aes(x=mpg, y=qsec, colour=qsec)) +  geom_line(colour="brown") +
       geom_point() + ylab("qsec") + xlab(" ") + scale_colour_gradientn(colours=rainbow(20))

  g3= ggplot(data=mtcars,aes(x=mpg, y=qsec, colour=qsec)) +  geom_line(colour="brown") +
            geom_point() + ylab("qsec") + xlab(" ") + scale_fill_distiller( )

  #---put all together
  # png("plots.png", width=10, height=5, units="in", res=150)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.5, 5, 5, 5), "null"))))   

  grid.text("MAIN TITLE", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(g1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(g2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(g3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))

  #dev.off()

  ###end: multiple plot in 1 page

#### Example 2: ggplot - scatter plot joined by line #######
ggplot() +  my.theme() +
  ggtitle(paste("HSHD #", sHSHD_ID)) +
  scale_x_continuous(name="", breaks=c(1,5,9, 13), labels=c("2012","2013", "2014", "2015")) + 
  scale_y_continuous(name="12 Week Spending ($)\n", 
                     limits=c(min(tmp$M_HH_Spend.p12)-10,max(tmp$M_HH_Spend.p12)+10), 
                     breaks=c(0:100)*100) +
  geom_point(data=tmp, aes(x=p12, y=M_HH_Spend.p12)) + 
     geom_line(data=tmp, aes(x=p12, y=M_HH_Spend.p12)) + 
  geom_point(data=tmp, aes(x=p12, y=M_HH_Spend.p12+5), colour="darkblue") + 
     geom_line(data=tmp, aes(x=p12, y=M_HH_Spend.p12+5), colour="darkblue", linetype = "dashed") 
  
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}     

##### Example 3: Multple plots using gridExtra #####
#http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization#arranging-multiple-graphs-using-cowplot
library(cowplot)
library(gridExtra)
#--- individual plots
g1= ggplot() +  
  geom_point(data=tmp0b, aes(x=relsum_grp, y=redm_mn, color=LCM_ID)  ) + 
  scale_y_continuous(name="% HHs Redeemed at Least 1")+ background_grid(major = "xy", minor = "none")+
  theme(legend.position = "none", axis.text.x=element_blank(),
        axis.title.x=element_blank() , axis.ticks.x=element_blank())

legend <- get_legend(g1)

g2= ggplot() + geom_density(data=tmp0a, aes(x=relsum, color=LCM_ID)) + 
  scale_y_continuous(name="Density") + 
  theme(legend.position = "none")

#draw_plot(plot, x = 0, y = 0, width = 1, height = 1)
ggdraw() +
  draw_plot(g1, x=0, y=.375, width=1, .6) +
  draw_plot(g2, x=0.01, y=0, width=1, .4) +
  draw_plot(legend, x=0.01, y=0.3, width=1, .4) 


# grid.arrange(g1, g2, legend, nrow=3, heights=c(1, 0.5, 0.1))
# draw_plot(plot, x = 0, y = 0, width = 1, height = 1)


#### Example 4: ggplot - pass var names as arguments using ae_string #####
x_var= 1:10
y_var=rnorm(10)
indat=data.frame(x_var,y_var)
plot_func = function(dat=indat, x='x_var', y='y_vay' ){
  p0 = ggplot(dat,  aes_string(x=x, y=y)) + geom_point()+ geom_line()    
  print(p0)
 }
plot_func(dat=indat, x='x_var', y='y_var')

##### Example 5:  ggplot - histogram with % in y axis  ######
library(scales)
ggplot(data=cnt_ttl, aes(x=cnt_ttl)) +  
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 5) +
  scale_y_continuous(name = '', labels = percent_format()) +
  scale_x_continuous(name = 'Months Visited') + ggtitle("Over 64% of households had 5 months of visited or less")

##### Example 6:  Creates dummary ######
#--- via data.table
library(data.table)
setDT(a3)[, c(levels(a3$CHURNERS), "CHURNERS") := 
            c(lapply(levels(CHURNERS), function(x) as.integer(x == CHURNERS)), .(NULL))]
#--- via dplyr
junk = a3 %>%
  mutate(row_id=row_number(), value = 1, CHURNERS = paste0("CHURNERS_", CHURNERS)) %>%
  spread(CHURNERS, value,  fill = 0) %>% arrange(row_id) 


##### Example 7:  Data manupulations ######
#----rename a set of columns
setnames(dat, old=paste0("COUPON_RELEVANCY_SCORE", 1:ncoupons), new=paste0("CPRE_RELSCORE", 1:ncoupons))
