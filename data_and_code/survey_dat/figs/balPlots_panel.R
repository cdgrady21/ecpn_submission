library(RItools)
rm(list=ls())
load("../d_analysis/diss_appendix/panel_balance1.rda")
load("../d_analysis/diss_appendix/panel_balance2.rda")
load("../d_analysis/diss_appendix/panel_balance3.rda")

xbLabs <- bal1
varlabs  <- c("Trust","Other","Clash",
              "Contact","Benefit","Threat",
              "Insecurity","Cohesion","Empathy",
              "Bargaining", "Violence","Violence Experience",
              "Age", "Female")

rbind(dimnames(xbLabs$results)[["vars"]],varlabs) ## check
dimnames(xbLabs$results)[["vars"]] <- varlabs
#xbLabs$results <- xbLabs$results[order(abs(xbLabs$results[,"std.diff", "unstrat"])),,]

pdf(file="balanceplot1.pdf",height=6,width=8)
par(oma=rep(0,4),mar=c(3,5,0,0),mgp=c(1.5,.5,0))
plot(xbLabs,absolute=TRUE,cex.axis=.7, strata.labels=c(unstrat="Raw Differences"),
     groups=rep(NA,dim(xbLabs$results)[1]))
text(.9,dim(xbLabs$results)[1]+1,paste("Omnibus Test of Balance: p=",round(xbLabs$overall[,"p.value"],4),sep=""))
dev.off()

# Bal2

xbLabs <- bal2
varlabs  <- c("Trust","Other","Clash",
              "Contact","Benefit","Threat",
              "Insecurity","Cohesion","Empathy",
              "Bargaining", "Violence","Violence Experience",
              "Age", "Female")

rbind(dimnames(xbLabs$results)[["vars"]],varlabs) ## check
dimnames(xbLabs$results)[["vars"]] <- varlabs
#xbLabs$results <- xbLabs$results[order(abs(xbLabs$results[,"std.diff", "unstrat"])),,]

pdf(file="balanceplot2.pdf",height=6,width=8)
par(oma=rep(0,4),mar=c(3,5,0,0),mgp=c(1.5,.5,0))
plot(xbLabs,absolute=TRUE,cex.axis=.7, strata.labels=c(unstrat="Raw Differences"),
     groups=rep(NA,dim(xbLabs$results)[1]), xlim=c(0,0.4))
text(.2,dim(xbLabs$results)[1]+1,paste("Omnibus Test of Balance: p=",round(xbLabs$overall[,"p.value"],4),sep=""))
dev.off()


# Bal 3

xbLabs <- bal3
varlabs  <- c("Trust","Other","Clash",
              "Contact","Benefit","Threat",
              "Insecurity","Cohesion","Empathy",
              "Bargaining", "Violence","Violence Experience",
              "Age", "Female")

rbind(dimnames(xbLabs$results)[["vars"]],varlabs) ## check
dimnames(xbLabs$results)[["vars"]] <- varlabs
#xbLabs$results <- xbLabs$results[order(abs(xbLabs$results[,"std.diff", "unstrat"])),,]

pdf(file="balanceplot3.pdf",height=6,width=8)
par(oma=rep(0,4),mar=c(3,5,0,0),mgp=c(1.5,.5,0))
plot(xbLabs,absolute=TRUE,cex.axis=.7, strata.labels=c(unstrat="Raw Differences"),
     groups=rep(NA,dim(xbLabs$results)[1]), xlim=c(0,0.4))
text(.2,dim(xbLabs$results)[1]+1,paste("Omnibus Test of Balance: p=",round(xbLabs$overall[,"p.value"],4),sep=""))
dev.off()

