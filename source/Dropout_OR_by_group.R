rm(list=ls());
setwd("C:/EDU/UMDNJ/JBK");

#================================================
# End point = HF
#================================================
dat <- read.csv("21studies.csv")
name <- as.character(dat$Name);

# counts
hf.control <- dat$HF_Control;
hf.active <- dat$HF_Active;

hf.control[hf.control==0] <- 0.5;
hf.active[hf.active==0] <- 0.5;

total.control <- dat$N_Control;
total.active <- dat$N_Active;

dropout.control <- dat$Dropout_Placebo;
dropout.active <- dat$Dropout_Active;

library(meta)
# ALL
meta.rd <- metabin(hf.active, total.active, hf.control, total.control, sm="RD", method="I", studlab=name)
rd <- summary(meta.rd)
rd.mean <- rd$random$TE;
rd.lower <- rd$random$lower;
rd.upper <- rd$random$upper;

meta.or <- metabin(hf.active, total.active, hf.control, total.control, sm="OR", method="I", studlab=name)
or <- summary(meta.or)
or.mean <- exp(or$random$TE);
or.lower <- exp(or$random$lower);
or.upper <- exp(or$random$upper);

meta.do <- metabin(dropout.active, total.active, dropout.control, total.control, sm="OR", method="I", studlab=name)
tmp <- summary(meta.do)
do.mean <- exp(tmp$random$TE);
do.lower <- exp(tmp$random$lower);
do.upper <- exp(tmp$random$upper);

meta.do2 <- metabin(dropout.active, total.active, dropout.control, total.control, sm="RD", method="I", studlab=name)
tmp <- summary(meta.do2)
do.mean2 <- exp(tmp$random$TE);
do.lower2 <- exp(tmp$random$lower);
do.upper2 <- exp(tmp$random$upper);


dClass <- dat$ActiveClass;
nl <- nlevels(dClass);
gRD.mean <- numeric(nl);
gRD.lower <- numeric(nl);
gRD.upper <- numeric(nl);
gOR.mean <- numeric(nl);
gOR.lower <- numeric(nl);
gOR.upper <- numeric(nl);
gDO.mean <- numeric(nl);
gDO.lower <- numeric(nl);
gDO.upper <- numeric(nl);

# risk difference in dropout
gDO.mean2 <- numeric(nl);
gDO.lower2 <- numeric(nl);
gDO.upper2 <- numeric(nl);
for (i in 1:nl) {
  q <- dClass==levels(dClass)[i];    
  
  gmeta.rd <- metabin(hf.active[q], total.active[q], hf.control[q], total.control[q], sm="RD", method="I", studlab=name[q])
  rd <- summary(gmeta.rd)
  gRD.mean[i] <- rd$random$TE;
  gRD.lower[i] <- rd$random$lower;
  gRD.upper[i] <- rd$random$upper;
  
  gmeta.or <- metabin(hf.active[q], total.active[q], hf.control[q], total.control[q], sm="OR", method="I", studlab=name[q])
  or <- summary(gmeta.or)
  gOR.mean[i] <- exp(or$random$TE);
  gOR.lower[i] <- exp(or$random$lower);
  gOR.upper[i] <- exp(or$random$upper);
  
  meta.do <- metabin(dropout.active[q], total.active[q], dropout.control[q], total.control[q], sm="OR", method="I", studlab=name[q])
  tmp <- summary(meta.do)
  gDO.mean[i] <- exp(tmp$random$TE);
  gDO.lower[i] <- exp(tmp$random$lower);
  gDO.upper[i] <- exp(tmp$random$upper);
  
  meta.do2 <- metabin(dropout.active[q], total.active[q], dropout.control[q], total.control[q], sm="RD", method="I", studlab=name[q])
  tmp <- summary(meta.do2)
  gDO.mean2[i] <- exp(tmp$random$TE);
  gDO.lower2[i] <- exp(tmp$random$lower);
  gDO.upper2[i] <- exp(tmp$random$upper);
}

## Plot 1
##
title <- "HF OR versus Dropout OR By Drug Class";
figname <- "fig/HF_or_vs_DO_or.pdf"

pdf(figname, height=7, width=10);
#x.min = min(c(gRD.lower, rd.lower))*1.4
x.min = min(c(gDO.lower, do.lower))
x.max = max(c(gDO.upper, do.upper)) 
#x.max = max(c(gRD.upper, rd.upper))*1.4 
xrange = x.max - x.min;

y.min = min(c(gOR.lower, or.lower))
y.max = max(c(gOR.upper, or.upper)) 
yrange = y.max - y.min;


plot(1, type="n", main=title, xlim=c(x.min, x.max), ylim=c(y.min, y.max), #axes=F, 
     xlab="Dropout OR", 
     ylab="HF OR", cex.lab=1, cex.main=1.2);

for (i in 1:nl) {
  if (levels(dClass)[i]=="ARB") {
    f1 <- 0.005
    f2 <- 0.015
    #f1 <- 0; f2 <- 0;
    lines(c(gDO.lower[i]+f1*xrange, gDO.upper[i])+f1*xrange, rep(gOR.mean[i]+f2*yrange, 2), lwd=3, type="l", col=i);
    lines(rep(gDO.mean[i]+f1*xrange, 2), c(gOR.lower[i]+f2*yrange, gOR.upper[i]+f2*yrange), lwd=3, type="l", col=i);
  } else {
    color=i
    if (i==7) color="orange";
    lines(c(gDO.lower[i], gDO.upper[i]), rep(gOR.mean[i], 2), lwd=3, type="l", col=color);
    lines(rep(gDO.mean[i], 2), c(gOR.lower[i], gOR.upper[i]), lwd=3, type="l", col=color);
  }
  xcoord <- gDO.mean[i]-0.06*xrange;
  ycoord <- gOR.mean[i]+0.04*yrange;
  
  if (levels(dClass)[i]=="DHP" | levels(dClass)[i]=="Intensive") {
    xcoord <- gDO.mean[i];
    ycoord <- gOR.lower[i] - 0.03*yrange;
  }
  if (levels(dClass)[i]=="ARB") {
    xcoord <- gDO.mean[i]+0.04*xrange;
    ycoord <- gOR.upper[i] + 0.001*yrange;
  }
  if (levels(dClass)[i]=="ACEI") {
    xcoord <- gDO.mean[i]+0.04*xrange;
    ycoord <- gOR.lower[i] + 0.004*yrange;
  }
  
  color=ifelse(i==7, "orange", i);
  text(xcoord, ycoord, levels(dClass)[i], col=color, cex=1);
  cat(paste(levels(dClass)[i], "\t", gOR.mean[i], "\t", gOR.lower[i], "\t", gOR.upper[i], "\t", 
            gDO.mean[i], "\t", gDO.lower[i], "\t", gDO.upper[i], "\n"));
}
# ALL
lines(c(do.lower, do.upper), rep(or.mean, 2), lwd=7, type="l", col="brown");
lines(rep(do.mean, 2), c(or.lower, or.upper), lwd=7, type="l", col="brown");
text(do.mean+0.01*xrange, or.lower-0.03*yrange, "All", col="brown", cex=1);
abline(1, 0)
abline(v=0)
axis(1);
#axis(1, at=c(-0.03, -0.025, -0.02, -0.015, -0.01, -0.005, 0.00, 0.005) , cex.axis=1.1, font=2, tcl=-0.5, labels=c("3.0", "2.5", "2.0", "1.5", "1.0","0.5", "0", "-0.5"));
axis(2)
box();

dev.off();

## Plot 2
##
title <- "HF OR versus Dropout Risk Difference By Drug Classes";
figname <- "fig/HF_or_vs_DO_rd.pdf"

pdf(figname, height=7, width=10);
#x.min = min(c(gRD.lower, rd.lower))*1.4
x.min = min(c(gDO.lower2, do.lower2))
x.max = max(c(gDO.upper2, do.upper2)) 
#x.max = max(c(gRD.upper, rd.upper))*1.4 
xrange = x.max - x.min;

y.min = min(c(gOR.lower, or.lower))
y.max = max(c(gOR.upper, or.upper)) 
yrange = y.max - y.min;


plot(1, type="n", main=title, xlim=c(x.min, x.max), ylim=c(y.min, y.max), axes=F, xlab="Dropout Risk Difference", 
     ylab="HF OR", cex.lab=1, cex.main=1.2);

for (i in 1:nl) {
  if (levels(dClass)[i]=="ARB") {
    f1 <- 0.005
    f2 <- 0.015
    #f1 <- 0; f2 <- 0;
    lines(c(gDO.lower2[i]+f1*xrange, gDO.upper2[i])+f1*xrange, rep(gOR.mean[i]+f2*yrange, 2), lwd=3, type="l", col=i);
    lines(rep(gDO.mean2[i]+f1*xrange, 2), c(gOR.lower[i]+f2*yrange, gOR.upper[i]+f2*yrange), lwd=3, type="l", col=i);
  } 
  else {
    color=i
    if (i==7) color="orange";
    lines(c(gDO.lower2[i], gDO.upper2[i]), rep(gOR.mean[i], 2), lwd=3, type="l", col=color);
    lines(rep(gDO.mean2[i], 2), c(gOR.lower[i], gOR.upper[i]), lwd=3, type="l", col=color);
  }
  xcoord <- gDO.mean2[i]-0.06*xrange;
  ycoord <- gOR.mean[i]+0.04*yrange;
  
  if (levels(dClass)[i]=="Verapamil" | levels(dClass)[i]=="DHP" | levels(dClass)[i]=="Intensive" | levels(dClass)[i]=="BB" ) {
    xcoord <- gDO.mean2[i];
    ycoord <- gOR.lower[i] - 0.03*yrange;
  }
  if (levels(dClass)[i]=="ARB") {
    xcoord <- gDO.mean2[i]+0.01*xrange;
    ycoord <- gOR.upper[i] + 0.03*yrange;
  }
  color=ifelse(i==7, "orange", i);
  text(xcoord, ycoord, levels(dClass)[i], col=color, cex=1);
  cat(paste(levels(dClass)[i], "\t", gOR.mean[i], "\t", gOR.lower[i], "\t", gOR.upper[i], "\t", 
            gDO.mean2[i], "\t", gDO.lower2[i], "\t", gDO.upper2[i], "\n"));
}

# ALL
lines(c(do.lower2, do.upper2), rep(or.mean, 2), lwd=7, type="l", col="brown");
lines(rep(do.mean2, 2), c(or.lower, or.upper), lwd=7, type="l", col="brown");
text(do.mean2+0.01*xrange, or.lower-0.03*yrange, "All", col="brown", cex=1);
abline(1, 0)
abline(v=0)
axis(1);
#axis(1, at=c(-0.03, -0.025, -0.02, -0.015, -0.01, -0.005, 0.00, 0.005) , cex.axis=1.1, font=2, tcl=-0.5, labels=c("3.0", "2.5", "2.0", "1.5", "1.0","0.5", "0", "-0.5"));
axis(2)
box();

dev.off();

# DS 10/22/2016
plot(1 ~ 1,
     type = "n",
     xlim = range(c(gDO.upper2,
                    gDO.lower2)),
     ylim = range(c(gOR.upper,
                    gOR.lower)),
     xlab = "Dropout Risk Difference",
     ylab = "HF OR")
segments(x0 = gDO.lower2,
         x1 = gDO.upper2,
         y0 = gOR.mean,
         y1 = gOR.mean,
         col = 1:5)
segments(x0 = gDO.mean2,
         x1 = gDO.mean2,
         y0 = gOR.upper,
         y1 = gOR.lower,
         col = 1:5)
text(levels(dClass),
     x = gDO.mean2,
     y = gOR.mean,
     col = 1:5)

## Plot 3
##
title <- "HF Risk Difference versus Dropout OR By Drug Classes";
figname <- "fig/HF_rd_vs_DO_or.pdf"

pdf(figname, height=7, width=10);
#x.min = min(c(gRD.lower, rd.lower))*1.4
x.min = min(c(gDO.lower, do.lower))
x.max = max(c(gDO.upper, do.upper)) 
#x.max = max(c(gRD.upper, rd.upper))*1.4 
xrange = x.max - x.min;

y.min = min(c(gRD.lower, rd.lower))
y.max = max(c(gRD.upper, rd.upper)) 
yrange = y.max - y.min;


plot(1, type="n", main=title, xlim=c(x.min, x.max), ylim=c(y.min, y.max), axes=F, xlab="Dropout OR", 
     ylab="HF Risk Difference", cex.lab=1, cex.main=1.2);

for (i in 1:nl) {
  if (levels(dClass)[i]=="ARB") {
    f1 <- 0.005
    f2 <- 0.015
    #f1 <- 0; f2 <- 0;
    lines(c(gDO.lower[i]+f1*xrange, gDO.upper[i])+f1*xrange, rep(gRD.mean[i]+f2*yrange, 2), lwd=3, type="l", col=i);
    lines(rep(gDO.mean[i]+f1*xrange, 2), c(gRD.lower[i]+f2*yrange, gRD.upper[i]+f2*yrange), lwd=3, type="l", col=i);
  } 
  else {
    color=i
    if (i==7) color="orange";
    lines(c(gDO.lower[i], gDO.upper[i]), rep(gRD.mean[i], 2), lwd=3, type="l", col=color);
    lines(rep(gDO.mean[i], 2), c(gRD.lower[i], gRD.upper[i]), lwd=3, type="l", col=color);
  }
  xcoord <- gDO.mean[i]-0.06*xrange;
  ycoord <- gRD.mean[i]+0.04*yrange;
  
  if (levels(dClass)[i]=="Verapamil" | levels(dClass)[i]=="DHP" | levels(dClass)[i]=="Intensive" | levels(dClass)[i]=="BB" ) {
    xcoord <- gDO.mean[i];
    ycoord <- gRD.lower[i] - 0.03*yrange;
  }
  if (levels(dClass)[i]=="ARB") {
    xcoord <- gDO.mean[i]+0.01*xrange;
    ycoord <- gRD.upper[i] + 0.03*yrange;
  }
  color=ifelse(i==7, "orange", i);
  text(xcoord, ycoord, levels(dClass)[i], col=color, cex=1);
  cat(paste(levels(dClass)[i], "\t", gRD.mean[i], "\t", gRD.lower[i], "\t", gRD.upper[i], "\t", 
            gDO.mean[i], "\t", gDO.lower[i], "\t", gDO.upper[i], "\n"));
}

# ALL
lines(c(do.lower, do.upper), rep(rd.mean, 2), lwd=7, type="l", col="brown");
lines(rep(do.mean, 2), c(rd.lower, rd.upper), lwd=7, type="l", col="brown");
text(do.mean+0.01*xrange, rd.lower-0.03*yrange, "All", col="brown", cex=1);
abline(1, 0)
abline(v=0)
axis(1);
#axis(1, at=c(-0.03, -0.025, -0.02, -0.015, -0.01, -0.005, 0.00, 0.005) , cex.axis=1.1, font=2, tcl=-0.5, labels=c("3.0", "2.5", "2.0", "1.5", "1.0","0.5", "0", "-0.5"));
axis(2)
box();

dev.off();

## Plot 4
##
title <- "HF Risk Difference versus Dropout Risk Difference By Drug Classes";
figname <- "fig/HF_rd_vs_DO_rd.pdf"

pdf(figname, height=7, width=10);
#x.min = min(c(gRD.lower, rd.lower))*1.4
x.min = min(c(gDO.lower2, do.lower2))
x.max = max(c(gDO.upper2, do.upper2)) 
#x.max = max(c(gRD.upper, rd.upper))*1.4 
xrange = x.max - x.min;

y.min = min(c(gRD.lower2, rd.lower2))
y.max = max(c(gRD.upper2, rd.upper2)) 
yrange = y.max - y.min;

plot(1, type="n", main=title, xlim=c(x.min, x.max), ylim=c(y.min, y.max), axes=F, xlab="Dropout Risk Difference", 
     ylab="HF Risk Difference", cex.lab=1, cex.main=1.2);

for (i in 1:nl) {
  if (levels(dClass)[i]=="ARB") {
    f1 <- 0.005
    f2 <- 0.015
    #f1 <- 0; f2 <- 0;
    lines(c(gDO.lower2[i]+f1*xrange, gDO.upper2[i])+f1*xrange, rep(gRD.mean[i]+f2*yrange, 2), lwd=3, type="l", col=i);
    lines(rep(gDO.mean2[i]+f1*xrange, 2), c(gRD.lower[i]+f2*yrange, gRD.upper[i]+f2*yrange), lwd=3, type="l", col=i);
  } 
  else {
    color=i
    if (i==7) color="orange";
    lines(c(gDO.lower2[i], gDO.upper2[i]), rep(gRD.mean[i], 2), lwd=3, type="l", col=color);
    lines(rep(gDO.mean2[i], 2), c(gRD.lower[i], gRD.upper[i]), lwd=3, type="l", col=color);
  }
  xcoord <- gDO.mean2[i]-0.06*xrange;
  ycoord <- gRD.mean[i]+0.04*yrange;
  
  if (levels(dClass)[i]=="Verapamil" | levels(dClass)[i]=="DHP" | levels(dClass)[i]=="Intensive" | levels(dClass)[i]=="BB" ) {
    xcoord <- gDO.mean2[i];
    ycoord <- gRD.lower[i] - 0.03*yrange;
  }
  if (levels(dClass)[i]=="ARB") {
    xcoord <- gDO.mean2[i]+0.01*xrange;
    ycoord <- gRD.upper[i] + 0.03*yrange;
  }
  color=ifelse(i==7, "orange", i);
  text(xcoord, ycoord, levels(dClass)[i], col=color, cex=1);
  cat(paste(levels(dClass)[i], "\t", gRD.mean[i], "\t", gRD.lower[i], "\t", gRD.upper[i], "\t", 
            gDO.mean2[i], "\t", gDO.lower2[i], "\t", gDO.upper2[i], "\n"));
}

# ALL
lines(c(do.lower2, do.upper2), rep(rd.mean, 2), lwd=7, type="l", col="brown");
lines(rep(do.mean2, 2), c(rd.lower, rd.upper), lwd=7, type="l", col="brown");
text(do.mean2+0.01*xrange, rd.lower-0.03*yrange, "All", col="brown", cex=1);
abline(1, 0)
abline(v=0)
axis(1);
#axis(1, at=c(-0.03, -0.025, -0.02, -0.015, -0.01, -0.005, 0.00, 0.005) , cex.axis=1.1, font=2, tcl=-0.5, labels=c("3.0", "2.5", "2.0", "1.5", "1.0","0.5", "0", "-0.5"));
axis(2)
box();

dev.off();





