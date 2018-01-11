# Javier Cabrera's email, 10/03/2014
# 
# This is the function for the matching using the propensity withing exact group.
# Please try it very carefully and see if it works.(I am sure there will be buggs)
# Best
# We just tried it with one dataset and it works but there are no guarantees.
# 
# Javier

match3w = function(y,xpp,xe,xpt,kpt=1,del=1) {
  # y is the two groups coded as 0 treatment 1 control
  # xpp is the data frame with the variables used for calculating the propensity scores
  # xe is the data frame with the variables used for the exact matching
  # xpt is the data frame with the variables used for partial matching
  # kpt=1 vector (or scalar) of maximum differences allowed for partial 
  #       matching for each of the partial matching variables 
  # del=1 delta for the acceptable propensity change
  # returns a two column array with the matches
  n = nrow(xe)
  ish <- sample(n)
  y = y[ish];  xpp=xpp[ish,];xe=xe[ish,];xpt=xpt[ish,]
  gr = as.character(xe[,1])
  if (ncol(xe)>1) for(i in 2:ncol(xe))
      gr =factor( paste(gr,as.character(xe[,i])))
    gru = unique(gr); grn = length(gru) 
  tt =  predict(glm(y~.*gr, data=cbind(xpp,xpt),family = binomial))
  pp =  exp(tt)/(1+exp(tt))
  j = (y==1)
  pp0=pp[j];pp1=pp[!j]
  gr0=gr[j];gr1=gr[!j]
  ish0=ish[j];ish1=ish[!j]
  xpt0=t(as.matrix(xpt[j,]))
  xpt1=t(as.matrix(xpt[!j,]))
  n0 = length(pp0);n1 = length(pp1);
  q1 = 1:n0; res=NULL
  for(i in 1:n1) {
    tt =abs( xpt0[,q1] - xpt1[,i])<=kpt
    t1 = apply(tt,2,all) & gr0[q1]==gr1[i]
    i2=which.min(1-t1+abs(pp0[q1]-pp1[i]))
    if(length(i2)==1 & abs(pp0[q1[i2]]-pp1[i])<=del)
    {res[i]=q1[i2]; q1 = q1[-i2]}
  }
  cbind(ish1,ish0[res])
}