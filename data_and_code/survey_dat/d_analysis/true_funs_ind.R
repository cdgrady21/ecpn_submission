#Chris: notes
##Want to replace all lm() with lm_lin() but lm_lin() does not allow me to vary treatment in a function.-->
rm(list=ls())

# true pval function.  Might not be necessary for panel since we have a decent number of obs, but still few clusters.
## for panel, might need the covar adjust version because possible confounders.
## chris: should block here should be community? No, then treatment perfectly correlates with the block, washing out effect.
true.fun1 <- function(var, tr)
{
  if(grepl("resid", var)){
    thelm <- lm(panel.df[,var]~panel.df[,tr], data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,1500)
    for(i in 1:1500){
      rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- panel.df
      rand.samp$treatment <- ifelse(rand.samp$psu %in% rand, 1, 0)
      tr.lm <- lm(tr_fmla,rand.samp)
      lm.tr_resid<-resid(tr.lm)
      rand.samp[names(lm.tr_resid),"tr_resid"]<-lm.tr_resid
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr], data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment"){
    thelm <- lm(panel.df[,var]~panel.df[,tr]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,2000)
    for(i in 1:2000){
      rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- panel.df
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else{
    thelm <- lm(panel.df[,var]~panel.df[,tr]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,1000)
    for(i in 1:1000){
      rand.samp <- panel.df %>% dplyr::group_by(state) %>% # for ind~ind, null dist can assign value of anyone in same state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,'newtr']+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  thep <- mean(rand.coef>=thecoef)
  thedf <- data.frame(coef=thecoef,truep=thep)
  rownames(thedf) <- paste0(var, "~",tr)
  return(thedf)
}
#true.fun1(var='cohes_cw', tr='treatment')
#true.fun1(var='cohes_cw_resid', tr='tr_resid') # Could be useful if confounders
#true.fun1(var='cohes_cw', tr='bene_cw')


########## Samii Suggestion
true.fun <- function(var, tr)
{
  if(grepl("resid", var)){
    thelm <- lm(panel.df[,var]~panel.df[,tr], data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,1500)
    for(i in 1:1500){
      rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- panel.df
      rand.samp$treatment <- ifelse(rand.samp$psu %in% rand, 1, 0)
      tr.lm <- lm(tr_fmla,rand.samp)
      lm.tr_resid<-resid(tr.lm)
      rand.samp[names(lm.tr_resid),"tr_resid"]<-lm.tr_resid
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr], data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment" & (grepl("end$", var) | grepl("y1", var) | grepl("rMean", var))){
    thelm <- lm(panel.df[,var]~panel.df[,tr]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,2000)
    for(i in 1:2000){
      rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- panel.df
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,tr]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(tr=="treatment" & !(grepl("end$", var) | grepl("y1", var) | grepl("rMean", var))){
    thelm <- lm(panel.df[,paste0(var,"_y1")]~panel.df[,tr]+panel.df[,paste0(var,"_y0")]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,2000)
    for(i in 1:2000){
      rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
      rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
      rand <- c(as.character(rand.nas), as.character(rand.ben))
      rand.samp <- panel.df
      rand.samp[,tr] <- ifelse(rand.samp$psu %in% rand, 1, 0)
      
      lm.null <- lm(rand.samp[,paste0(var,"_y1")]~rand.samp[,tr]+rand.samp[,paste0(var,"_y0")]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(!(grepl("end$", var) | grepl("y1", var) | grepl("rMean", var))){
    thelm <- lm(panel.df[,paste0(var,"_y1")]~panel.df[,tr]+panel.df[,paste0(var,"_y0")]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,1000)
    for(i in 1:1000){
      rand.samp <- panel.df %>% dplyr::group_by(state) %>% # for ind~ind, null dist can assign value of anyone in same state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,paste0(var,"_y1")]~rand.samp[,'newtr']+rand.samp[,paste0(var,"_y0")]+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  else if(grepl("end$", var) | grepl("y1", var) | grepl("rMean", var)){
    thelm <- lm(panel.df[,var]~panel.df[,tr]+state, data=panel.df)
    thecoef <-coef(thelm)[2]
    
    rand.coef = rep(NA,1000)
    for(i in 1:1000){
      rand.samp <- panel.df %>% dplyr::group_by(state) %>% # for ind~ind, null dist can assign value of anyone in same state.
        mutate(newtr = shuffle(.data[[tr]])) %>%
        as.data.frame(.)
      
      lm.null <- lm(rand.samp[,var]~rand.samp[,'newtr']+state, data=rand.samp)
      rand.coef[i] <- summary(lm.null)$coefficients[2,1]
    }
  }
  thep <- mean(rand.coef>=thecoef)
  thedf <- data.frame(coef=thecoef,truep=thep)
  rownames(thedf) <- paste0(var, "~",tr)
  return(thedf)
}

#################
#Committee vs Non-committee vs. Control
# com.fun1 is with differencing. com.fun is controlling-for. strat.fun decides which to use.

# differencing
com.fun1 <- function(var, nsims)
{
  lm1 <- lm(panel.df[,var] ~ tr_n+state, panel.df)
  #lm1 <- lm_robust(panel.df[,var] ~ tr_n+state, clusters=community, panel.df)
  #lm_robust not needed because only using coef
  obs.coef <- coef(lm1)[2]
  
  rand.coefs <- matrix(data=NA, nrow=nsims, ncol=1) # I think correct thing is to: (1) for treatment shuffle PSU-level treatment within state, (2) for committee shuffle committee within Community for the comm/non comparison.  To avoid committee people in non-treated areas, need to make "committee" column for "newtr" the same as committee for "treatment" before shuffling.
  for(i in 1:nsims){
    # randomly select for treatment PSUs within state
    rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
    rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
    rand <- c(as.character(rand.nas), as.character(rand.ben))
    rand.samp <- panel.df
    
    # 10p had no preselected, need to add it as 0% committee so have a committee proportion for each tr site pair
    rand.samp[nrow(rand.samp)+1,] <- NA
    rand.samp$community <- as.character(rand.samp$community)
    rand.samp[nrow(rand.samp), c("treatment", "community", "psu", "committee")] <- c(1,"10.pastoralists", 10, 0)
    rand.samp[,"newtr"] <- ifelse(rand.samp$psu %in% rand, 1, 0)
    
    # randomly select respondents to be committee members within newtr communities
    committees <- droplevels(rand.samp[rand.samp$treatment %in% 1, c("committee", "community")])
    toadd <- as.vector(prop.table(table(committees$community, committees$committee),1)[,"1"]) # prop of commit ppl in each tr community
    tr.samp <- droplevels(rand.samp[rand.samp$newtr %in% 1,]) #simulated treatment people
    tr.samp$newcomm <- randomizr::block_ra(blocks = tr.samp$community, block_prob = shuffle(as.numeric(toadd)))
    rand.samp$newcomm <- tr.samp$newcomm[match(rand.samp$id_num, tr.samp$id_num)]
    rand.samp$newcomm[is.na(rand.samp$newcomm)] <- 0 # make control ppl 0
    rand.samp["new_tr_f"] <- droplevels(interaction(rand.samp[,"newtr"], rand.samp[,"newcomm"]))
    rand.samp["new_tr_n"] <- as.numeric(rand.samp[,"new_tr_f"])
    
    lm.null <- lm(rand.samp[,var] ~ new_tr_n+state, rand.samp)
    rand.coefs[i,1] <- coef(lm.null)[2]
    
  }
  
  # now it's: "how many times are the non coef AND the comm coef greater than or equal to what we observed?"
  #thep <- mean(rand.coefs[,1]>=non & rand.coefs[,2]>=comm) # reject unless BOTH bigger
  #thep <- 1 - mean(non>rand.coefs[,1] & both>rand.coefs[,3] | comm>rand.coefs[,2] & both>rand.coefs[,3])
  thep <- 1 - mean(obs.coef>=rand.coefs[,1]) #immediate above is equivalent to this
  thedf <- data.frame(thecoef=obs.coef, truep=thep)
  rownames(thedf) <- paste0(var, "-", "trueP")
  return(thedf)
}
#com.fun1("x_cw",nsims=1000)

# Controlling-for
com.fun <- function(var, nsims)
{
  lm1 <- lm(panel.df[,paste0(var,"_y1")] ~ tr_n+state+panel.df[,paste0(var,"_y0")], panel.df)
  #lm1 <- lm_robust(panel.df[,paste0(var,"_y1")] ~ tr_n+state+panel.df[,paste0(var,"_y0")], clusters=community, panel.df)
  #lm_robust not needed because only using coef
  obs.coef <- coef(lm1)[2]
  
  rand.coefs <- matrix(data=NA, nrow=nsims, ncol=1) # I think correct thing is to: (1) for treatment shuffle PSU-level treatment within state, (2) for committee shuffle committee within Community for the comm/non comparison.  To avoid committee people in non-treated areas, need to make "committee" column for "newtr" the same as committee for "treatment" before shuffling.
  for(i in 1:nsims){
    # randomly select for treatment PSUs within state
    rand.nas <- sample(unique(panel.df$psu[panel.df$state %in% "nas"]), size=6)
    rand.ben <- sample(unique(panel.df$psu[panel.df$state %in% "ben"]), size=4)
    rand <- c(as.character(rand.nas), as.character(rand.ben))
    rand.samp <- panel.df
    
    # 10p had no preselected, need to add it as 0% committee so have a committee proportion for each tr site pair
    rand.samp[nrow(rand.samp)+1,] <- NA
    rand.samp$community <- as.character(rand.samp$community)
    rand.samp[nrow(rand.samp), c("treatment", "community", "psu", "committee")] <- c(1,"10.pastoralists", 10, 0)
    rand.samp[,"newtr"] <- ifelse(rand.samp$psu %in% rand, 1, 0)
    
    # randomly select respondents to be committee members within newtr communities
    committees <- droplevels(rand.samp[rand.samp$treatment %in% 1, c("committee", "community")])
    toadd <- as.vector(prop.table(table(committees$community, committees$committee),1)[,"1"]) # prop of commit ppl in each tr community
    tr.samp <- droplevels(rand.samp[rand.samp$newtr %in% 1,]) #simulated treatment people
    tr.samp$newcomm <- randomizr::block_ra(blocks = tr.samp$community, block_prob = shuffle(as.numeric(toadd)))
    rand.samp$newcomm <- tr.samp$newcomm[match(rand.samp$id_num, tr.samp$id_num)]
    rand.samp$newcomm[is.na(rand.samp$newcomm)] <- 0 # make control ppl 0
    rand.samp["new_tr_f"] <- droplevels(interaction(rand.samp[,"newtr"], rand.samp[,"newcomm"]))
    rand.samp["new_tr_n"] <- as.numeric(rand.samp[,"new_tr_f"])
    
    lm.null <- lm(rand.samp[,paste0(var,"_y1")] ~ new_tr_n + state+rand.samp[,paste0(var,"_y0")], rand.samp)
    rand.coefs[i,1] <- coef(lm.null)[2]
    
  }
  
  thep <- 1 - mean(obs.coef>=rand.coefs[,1])
  thedf <- data.frame(thecoef=obs.coef, truep=thep)
  rownames(thedf) <- paste0(var, "-", "trueP")
  return(thedf)
}

###################

# controlling for or differencing?
strat.fun <- function(var, dat=panel.df){
  thesd <- sd(dat[[var]], na.rm=T)*.2
  thediff <- abs(sort(mosaic::mean(dat[[var]]~dat$tr_f, na.rm=T))[[1]]-sort(mosaic::mean(dat[[var]]~dat$tr_f, na.rm=T))[[3]])
  thebal <- thediff-thesd
  if(thebal<0){
    return("Controlling-for, com.fun")
  }
  if(thebal>=0){
    return("Difference, com.fun1")
  }
  else(return("Fail"))
}


#save
save.image(file="true_fun_ind.rda")

