##### Filename:  /nfs/science/users/xiaojiez/Rcode/FreqUsedRFunc.r
# source("/nfs/science/users/xiaojiez/Rcode/FreqUsedRFunc.r")
# source("/Users/x644435/Desktop/RCode/FreqUsedRFunc.r")

library(grid)
library(tidyverse)
library(reshape2)
library(feather)

################ General section: ######################
###### Begin: mode(x):  Calculate mode ######
#--- example: dat %>% group_by(week_id %>% summarise(most_freq=mode(store_id))
mode <- function(codes)  which.max(tabulate(codes))
###end: mode():  Calculate mode  



###### Begin: as.numeric.factor(x) ######
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

###end: as.numeric.factor  



###### Begin: tbl2pdf  write data frame to pds ########
# To Use it:  OUT1=data.frame(V1=1:10, V2=1:10) ;   
#             fname = "junk.pdf"
#             tbl2pdf(tbl=OUT1, RowPerPage=30, filename=fname);

tbl2pdf = function(tbl, RowPerPage=45, filename){
  library(gridExtra); 
  maxrow = RowPerPage; 
  npages = ceiling(nrow(tbl)/maxrow); 
  pdf(filename, height=11, width=8.5); 
  for (i in 1:npages) {idx = seq(1+((i-1)*maxrow), i*maxrow); 
  grid.newpage(); 
  grid.table(tbl[idx, ], theme = ttheme_default(base_size = 6) )  }; 
  dev.off()
}

###### Begin: ptable1(x), ptable2.rowpct(x,y), ptable2.colpct(x,y)   #######
ptable1 = function(x){x = as.factor(x); round(100*prop.table(table(x)),1)}
ptable2.rowpct = function(x,y){round(100*prop.table(table(x,y),1),1) }
ptable2.colpct = function(x,y){round(100*prop.table(table(x,y),2),1) }

###### Begin: break.factor - Create a dataset with factor variables with limited number of levels #######
# Example:   junk = break.factor(as.factor(1:30), max.flevel=10)

break.factor = function(fvar, max.flevel=52){
  fvar=as.factor(fvar)
  flevel  = levels(fvar)
  nlevels = length(flevel)
  nvar=ceiling(nlevels / max.flevel)
  
  var_out=NULL
  
  for(i in 1:nvar){
    #  i=0; i=i+1; i
    slevel =  flevel[(1+(i-1)*max.flevel): (i*max.flevel) ]
    var_out = cbind(var_out, ifelse(fvar %in% slevel, fvar, 0) )
  }
  dim(var_out)
  #  colnames(var_out)=paste0("fvar", 1:nvar)
  
  return( as.data.frame(var_out) %>% mutate_all(funs(as.factor)))
}

###### End: break.factor - Create a dataset with factor variables with limited number of levels---



###### Begin: my.na.locf(x) ##### 
# x: a series or a column in a data frame
my.na.locf <- function(x) {  v <- !is.na(x);   c(NA, x[v])[cumsum(v)+1] }
###### Begin: Inverse Box Cox function ######
#Usage:  y_org = inv_box_cox(y_cb, lambda)
inv_box_cox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 
}


###### Begin: bivariate screening using t-tstat#####

# dat = as.data.frame(matrix(runif(100*10), 100, 10) )
# 
# tstat <- function(x, y) tryCatch(summary(lm(y ~ x))$coefficients[2,3],  error=function(e) 0)
# mn = summarise_each(a1, funs(tstat=tstat(., CHURNERS_N) ), -c(CHURNERS_N)  ) 


################ Measurement section: ######################
###### Begin: BalClassRate - calculate binary classification error, returns  #####
#----- evnt.rate, F.max, ROC.AUC, PR.AUC, balanced error rate

###### Begin: BiClassPerf - calculate binary classification performance, returns  #####
#----- evnt.rate, F.max, ROC.AUC, PR.AUC
BiClassPerf = function(prob.pred, event.obs){
  require(PRROC)
  require(ROCR)
  
  pred <- prediction(prob.pred, event.obs); 
  
  evnt.rate = mean(event.obs)
  F.max = max(performance(pred,"f")@y.values[[1]], na.rm=TRUE)  # F-score
  ROC.AUC =   max(performance(pred, "auc")@y.values[[1]], na.rm=TRUE)  # AUC
  pr <- pr.curve(scores.class0 = prob.pred[event.obs == 1], scores.class1 = prob.pred[event.obs == 0], curve = T)
  PR.AUC = pr$auc.integral
  
  # c(evnt.rate, F.max, ROC.AUC, PR.AUC)
  c(evnt.rate=evnt.rate, F.max=F.max, ROC.AUC=ROC.AUC, PR.AUC=PR.AUC)
}
###### End: BiClassPerf

###### Begin: BiClass_OptCut: select opmtial cutoff #######
#----- cutoff and balance classification rate
BiClass_OptCut = function(prob.pred, event.obs, tr.p=0.5){
  #- tr.p = % of data to identify the optimal cutoff
  require(ROCR)
  
  pred <- prediction(prob.pred, event.obs); 
  
  opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
  }
  
  #--calculate cutoff value minimizes the sum of squared tpr and (1-fpr)--
  ttr = rbinom(length(prob.pred), 1, tr.p)  # create a subset for selecting cut-off
  tr.pred=prob.pred[ttr==1]; tr.obs= event.obs[ttr==1]
  #--at least 2 events in the training data
  if((sum(tr.obs)>=2)  & (sum(1-tr.obs)>=2)){
    pred.ttr <- prediction(tr.pred, tr.obs); 
    roc.perf.ttr = performance(pred.ttr, measure = "tpr", x.measure = "fpr")
    cutoff = opt.cut(roc.perf.ttr, pred.ttr)["cutoff",]; cutoff
    
    #--calculate balanced error rate on test data
    tst.pred  = prob.pred[ttr==0]; tst.obs=event.obs[ttr==0]
    tp = (tst.pred > cutoff) & (tst.obs==1)
    tn = (tst.pred <= cutoff) & (tst.obs==0)
    err.col= mean(c(sum(tp)/sum(tst.obs==1),   sum(tn)/sum(tst.obs==0)))
  }else  err.col=NA
  
  c(cutoff, err.col)
}
###### End: BiClass_OptCut


################ Plot section: ######################
###### Begin: plot2Axis() - Plot with secondard axis ##### 
# To use it:  
#
plot2Axis = function(){
  library(scales)
  scale_factor=1.1
  a0 = data.frame(total_cost=1:10, dollars=1:10, ROI_dollars=10:1)
  a0 %>% ggplot(aes(x=total_cost)) + my.theme1() +
    geom_point(aes(y=dollars, colour = "Sales")) + 
    geom_line(aes(y=dollars, colour = "Sales")) + 
    footnote(title="Sales & S2C Ratio trade-off, A3619, Pepsi 24 pack", subtitle="021") + 
    scale_x_continuous(labels = comma_format(), name="Redmption Cost")+
    geom_point(aes(y=ROI_dollars*scale_factor, colour = "S2C Ratio"),  shape=2) + 
    geom_smooth(aes(y=ROI_dollars*scale_factor, colour = "S2C Ratio"), se=FALSE, linetype=2) +
    scale_y_continuous(labels = comma_format(), name="Sales", limits=c(0, 11),
                       sec.axis = sec_axis(~./scale_factor, name = "S2C Ratio"))+
    scale_colour_manual(values = c("blue", "black"))+
    labs(colour = "Parameter") +
    theme( axis.text.y.right = element_text(color = "blue"),
           axis.title.y.right = element_text(color = "blue"))
  print("This is not a function, but an example code")
}
#plot2Axis()
###### End: Plot with secondard axis ### 
###### Begin: Multiple plots on one page##### 
# To use it:  
# layout = matrix(c(1:6), nrow=2, byrow=TRUE)
# multiplot(g1, g2, g3, g4, g5, g6, layout=layout) 
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#### End: Multiple plots on one page ### 

###### Begin: My.theme ####
# To use it:
# ggplot() + my.theme()

my.theme =   function (base_size = 14, base_family = "") 
{
  require(grid)
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(plot.title = element_text(size=base_size*1.2, vjust=1,face="bold"),
          axis.title.x=element_text(vjust=0),
          axis.title=element_text(face="bold"),
          axis.text=element_text(face="bold", size=rel(0.8)) 
    )
}

#### end: My.theme ###

###### Begin: my.theme1 ####
# To use it:
# ggplot() + my.theme1()

my.theme1 =   function (base_size = 14, base_family = "") {
  theme_bw() +
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
          text=element_text(family="Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(colour="black", size = 11),
          axis.text.y=element_text(colour="black", size = 9),
          axis.line = element_line(size=0.5, colour = "black"))
}

###### End: my.theme1

###### Begin: my.ggplot.examples: my_hist_qntile, my_hist, my_hist_log #####
my.ggplot=function(){
  a3$UNIT_PE1 = sign(a3$UNIT_PE)*abs(a3$UNIT_PE)^(1/3)
  a3$TTE_PE1 = sign(a3$TTE_PE)*abs(a3$TTE_PE)^(1/3)
  xvar='UNIT_PE1'
  yvar='TTE_PE1'
  
  
  ggplot(data=a3, aes_string(x= xvar, y=yvar)) + my.theme1() + 
    geom_bin2d(bins=100) + geom_density_2d()+ geom_jitter() +
    scale_x_continuous(name=xvar, limit=c(-0.3, -0.05)) +
    scale_y_continuous(name=yvar, limit=c(-1, -0.05)) +
    labs(title = "Title here", 
         subtitle = 'Subtitle here', 
         caption = paste0('\n',getwd(), '::  ', prgname, '::  ', format(Sys.Date(), "%d/%m/%Y"))
    )
  
}

###### End: my.ggplot.examples: my_hist_qntile, my_hist, my_hist_log

###### Begin: my.histogram.examples: my_hist_qntile, my_hist, my_hist_log #####
my_hist_qntile = function(dat=indat, qntlvars=dat$y){
  #--- histogram
  library(scales)
  qntls=quantile(qntlvars,c(0.01, 0.99), na.rm=TRUE)
  geom_histogram(aes(y = (..count..)/sum(..count..), binwidth = (qntls[2]-qntls[1])/20)) +   
    scale_y_continuous(name = '%', labels = percent_format()) 
}

my_hist  = function(indata=a2, hist_var='nL10', hist_var_label = NULL){
  ggplot(data=indata, aes_string(x= hist_var)) + my.theme1() +
    geom_histogram(aes(y = 100*(..count..)/sum(..count..)), bins = 15, color="black", fill='blue') + 
    scale_x_continuous(name=paste0("\n ", hist_var_label, '\n')) + 
    scale_y_continuous(name="%") 
}

my_hist_log  = function(indata=a2, hist_var='nL10', hist_var_label = NULL){
  ggplot(data=indata, aes_string(x= hist_var)) + my.theme1() +
    geom_histogram(aes(y = 100*(..count..)/sum(..count..)), bins = 15, color="black", fill='blue') + 
    scale_x_log10(name=paste0("\n ", hist_var_label, '\n')) + 
    scale_y_continuous(name="%") 
}
###### End: my.histogram.examples: my_hist_qntile, my_hist, my_hist_log

###### Begin: Add footnote to a ggplot ########
footnote = function(title=NULL, subtitle=NULL,    legendlabel=NULL,
                    caption=paste0('\n',getwd(), '::  ', prgname, '::  ', format(Sys.Date(), "%d/%m/%Y"))
){
  labs(title = title, 
       subtitle = subtitle, 
       color=legendlabel,group=legendlabel, fill=legendlabel,
       caption =  caption       
  )
}
###### End: Add footnote to a ggplot
