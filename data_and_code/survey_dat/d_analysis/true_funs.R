#Chris: notes
##Want to replace all lm() with lm_lin() but lm_lin() does not allow me to vary treatment in a function.-->
rm(list=ls())

#True p-value function.  Don't need covariate adjustment to absorb error.  True p's Especially necessary with community level analysis, because we have a small number of clusters.  Treatment assigned at PSU level.

# Differencing
true.fun1 <- function(var, tr, nsims=10000, dat=ag.df)
{
  if(grepl("resid", var)){
    thelm <- lm(dat[,var]~dat[,tr], data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.nas <- sample(unique(dat$psu[dat$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(dat$psu[dat$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- dat
      rand.samp$treatment <- ifelse(rand.samp$psu %in% rand, 1, 0)
      tr.lm <- lm(tr_fmla,rand.samp)
      lm.tr_resid<-resid(tr.lm)
      rand.samp[names(lm.tr_resid),"tr_resid"]<-lm.tr_resid
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr], data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment"){
    thelm <- lm(dat[,var]~dat[,tr]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.nas <- sample(unique(dat$psu[dat$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(dat$psu[dat$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- dat
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else{
    thelm <- lm(dat[,var]~dat[,tr]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.samp <- dat %>% dplyr::group_by(state) %>% # for indices predicting indices, just shuffle within state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,'newtr']+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  thep <- mean(rand.coef>thecoef)
  thedf <- data.frame(coef=thecoef,truep=thep)
  rownames(thedf) <- paste0(var, "~",tr)
  return(thedf)
}

######## Controlling For
true.fun <- function(var, tr, nsims=10000, dat=ag.df)
{
  if(grepl("resid", var)){
    thelm <- lm(dat[,var]~dat[,tr], data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.nas <- sample(unique(dat$psu[dat$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(dat$psu[dat$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- dat
      rand.samp$treatment <- ifelse(rand.samp$psu %in% rand, 1, 0)
      tr.lm <- lm(tr_fmla,rand.samp)
      lm.tr_resid<-resid(tr.lm)
      rand.samp[names(lm.tr_resid),"tr_resid"]<-lm.tr_resid
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr], data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment" & (grepl("end$", var) | grepl("y1", var))){
    thelm <- lm(dat[,var]~dat[,tr]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.nas <- sample(unique(dat$psu[dat$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(dat$psu[dat$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- dat
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment" & !(grepl("end$", var) | grepl("y1", var))){
    thelm <- lm(dat[,paste0(var,"_end")]~dat[,tr]+dat[,paste0(var,"_base")]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.nas <- sample(unique(dat$psu[dat$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(dat$psu[dat$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- dat
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,paste0(var,"_end")]~rand.samp[,tr]+dat[,paste0(var,"_base")]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(grepl("end$", var) | grepl("y1", var)){
    thelm <- lm(dat[,var]~dat[,tr]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.samp <- dat %>% dplyr::group_by(state) %>% # for indices predicting indices, just shuffle within state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,'newtr']+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(!grepl("end$", var) | grepl("y1", var)){
    thelm <- lm(dat[,paste0(var,"_end")]~dat[,tr]+dat[,paste0(var,"_base")]+state, data=dat)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,nsims)
    for(i in 1:nsims){
      rand.samp <- dat %>% dplyr::group_by(state) %>% # for indices predicting indices, just shuffle within state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,paste0(var,"_end")]~rand.samp[,'newtr']+dat[,paste0(var,"_base")]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  
  thep <- mean(rand.coef>thecoef)
  thedf <- data.frame(coef=thecoef,truep=thep)
  rownames(thedf) <- paste0(var, "~",tr)
  return(thedf)
}

# Which function to use?
strat.fun <- function(var){
  thesd <- sd(ag.df[[var]])*.2
  thediff <- abs(mosaic::mean(ag.df[[var]]~ag.df$treatment)[1]-mosaic::mean(ag.df[[var]]~ag.df$treatment)[2])
  thebal <- thediff-thesd
  if(thebal<0){
    return("Controlling-for: true.fun")
  }
  if(thebal>=0){
    return("Difference: true.fun1")
  }
  else(return("Fail"))
}


#save
save.image(file="true_fun.rda")



# I prefer the SD method, but could also use lin hypothesis test or an equivalence test.
## issue with these: we are super low powered, so no baseline differences will be stat sig.  And equivalence test will always say "not same, not diff"

# Hypothesis test
#(x1_test <- lm_lin(x_cw_base~treatment, covariates= ~state, data=ag.df, clusters=psu))

# equivalence
#TOSTtwo(m1=mean(ag.df$x_cw_base[ag.df$treatment %in% 0]), m2=mean(ag.df$x_cw_base[ag.df$treatment %in% 1]), 
#        sd1=sd(ag.df$x_cw_base[ag.df$treatment %in% 0]), sd2=sd(ag.df$x_cw_base[ag.df$treatment %in% 1]), 
#        n1=length(unique(ag.df$comm[ag.df$treatment %in% 0])), n2=length(unique(ag.df$comm[ag.df$treatment %in% 1])),
#        low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha = 0.05)

#TOSTtwo(m1=mean(ag.df$contactOnly_cw_base[ag.df$treatment %in% 0]), m2=mean(ag.df$contactOnly_cw_base[ag.df$treatment %in% 1]), 
#        sd1=sd(ag.df$contactOnly_cw_base[ag.df$treatment %in% 0]), sd2=sd(ag.df$contactOnly_cw_base[ag.df$treatment %in% 1]), 
#        n1=length(unique(ag.df$comm[ag.df$treatment %in% 0])), n2=length(unique(ag.df$comm[ag.df$treatment %in% 1])),
#        low_eqbound_d=-0.01, high_eqbound_d=0.01, alpha = 0.05)
