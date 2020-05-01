<a id="table-of-contents"></a>
## R Functions:  Table of Contents
* [retain() -- fill next cell with previous one](#retain)
* [c2n.pvalue() -- remove < and dots and convert p-values to numeric](#pvalue)

<div id='retain'/>
###--- Fill the next cell with value from previous row if it is blank ###############
    retains<-function(in.col)
    {
      temp<-in.col
      for (i in 1:length(temp)){
        if (temp[i]=="")
          temp[i]=temp[i-1]
      }
    return(temp)
    }
    #  indata$PrimaryAnal<-retains(indata$PrimaryAnal)
   
[(back to top)](#table-of-contents)

<div id='pvalue'/>
###--- remove dots and < and convert it to numberic p-value  ---

    c2n.pvalue<-function(cpvalue)
    {
      temp<-sub("<", "0", cpvalue)
      
      for (i in 1:length(temp)){
        if (temp[i]=='.' | is.na(temp[i]))
          temp[i]="NA"
      }  
      return(as.numeric(temp))
    }
    #  c2n.pvalue(cpvalue)
    
[(back to top)](#table-of-contents)
