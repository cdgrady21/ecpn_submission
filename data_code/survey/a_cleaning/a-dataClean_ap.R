########################
#Setup: set working directory, set libraries, scan in data, make variables, etc...
###############################
# need to set a working directory to this file location.
rm(list=ls())
library(mosaic)
library(plyr)
library(dplyr)
library(splitstackshape)
library(car)
svy=read.csv("../raw/ecpn_end.csv")
svy<-svy[svy$form.consent_survey %in% "yes",]
pgp <- read.csv("../raw/pgp.csv")
pgp[pgp$state %in% "nas",'day'] <- as.Date(strptime(pgp$date[pgp$state %in% "nas"], format="%m/%d/%Y"))
pgp[pgp$state %in% "ben",'day'] <- as.Date(strptime(pgp$date[pgp$state %in% "ben"], format="%d/%m/%Y"))
pgp <- droplevels(pgp[!is.na(pgp$day),])


# housekeeping
svy$phone<-as.numeric(as.character(svy$form.name_phone_grp.phone))
names(svy) <- sub("form.", "", names(svy)) # fix the weird "form." at the beginning of each name,

# prelim date and time stuff
svy$start=as.Date(strptime(svy$started_time, format="%Y-%m-%d %H:%M:%S"))
svy$end=as.Date(strptime(svy$completed_time, format="%Y-%m-%d %H:%M:%S"))
svy<-svy[svy$end > "2018-01-28",]
svy<-svy[!svy$username %in% "test",] # remove my own if there are any
svy$end[svy$username %in% "a.bako" & svy$end < "2018-02-11"] <- svy$end[svy$username %in% "a.bako" & svy$end < "2018-02-11"] + 1 # Anna's tab is one day behind somehow
svy$start[svy$username %in% "a.bako" & svy$start < "2018-02-11"] <- svy$start[svy$username %in% "a.bako" & svy$start < "2018-02-11"] + 1 # Anna's tab is one day behind somehow
svy$end[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] <- svy$end[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] + 1 # Tahiru is now using Anna's old tab that is one day behind somehow
svy$start[svy$username %in% "t.ahmadu" & svy$start > "2018-04-17" & svy$start < "2018-04-21"] <- svy$start[svy$username %in% "t.ahmadu" & svy$start > "2018-04-17" & svy$start < "2018-04-21"] + 1 # Tahiru is now using Anna's old tab that is one day behind somehow
svy <- svy[!(svy$state %in% "ben" & svy$start < "2018-04-10"),] # martins orga submitted a survey during retraining

# time stuff
svy$done_calc=strptime(svy$completed_time, format="%Y-%m-%d %H:%M:%S")
svy$start_calc=strptime(svy$started_time, format="%Y-%m-%d %H:%M:%S")
svy$done_calc[svy$username %in% "i.hassan" & svy$done_calc == "2018-02-18 00:05:08 CST"] <- "2018-02-18 12:05:08 CST" # oddly, Ibrahim's tab (used by Jonah) didn't switch from AM to PM after noon one time.
svy$duration=difftime(svy$done_calc,svy$start_calc)
svy$duration<-ifelse(svy$duration>120,120,svy$duration)
#svy$duration2=as.numeric(svy$duration)/60 #if not in min
#svy$duration2<-ifelse(svy$duration2>120,120,svy$duration2)
svy$short=ifelse(svy$duration<15,1,0)
svy$vshort=ifelse(svy$duration<10,1,0)

# time in between variable
#svy=svy[with(svy, rev(order(info.username,done))),] #order by date new-old
svy=svy[with(svy, order(username,done_calc)),] #order by date old-new
svy=mutate(svy, svy_last=lag(completed_time,default=NA))
svy$svy_last=strptime(svy$svy_last, format="%Y-%m-%d %H:%M:%S")
svy$between=difftime(svy$start_calc,svy$svy_last)/60 #is in seconds 
svy$between2=ifelse(svy$between>10,10,svy$between) #make really long time between == 10 (occurs when next row is next day).
svy$between2=ifelse(svy$between2<0,NA,svy$between2) #make negative time 0 (occurs when next row is a new user)
svy$between2=as.numeric(svy$between2)
svy$short_between=ifelse(svy$between2<3,1,0)
svy$vshort_between=ifelse(svy$between2<2,1,0)
svy$vvshort_between=ifelse(svy$between2<1,1,0)

# the days when anna's and tahir's tablets were a day behind
svy$start_calc[svy$username %in% "a.bako" & svy$end < "2018-02-11"] <- svy$start_calc[svy$username %in% "a.bako" & svy$end < "2018-02-11"] + 60*60*24 #have to add 1 days worth of sections
svy$done_calc[svy$username %in% "a.bako" & svy$end < "2018-02-11"] <- svy$done_calc[svy$username %in% "a.bako" & svy$end < "2018-02-11"] + 60*60*24 #have to add 1 days worth of sections
stopifnot(svy$end[svy$username %in% "a.bako"]==sub(" .*", "", svy$done_calc[svy$username %in% "a.bako"]))
stopifnot(svy$start[svy$username %in% "a.bako"]==sub(" .*", "", svy$start_calc[svy$username %in% "a.bako"]))
svy$start_calc[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] <- svy$start_calc[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] + 60*60*24
svy$done_calc[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] <- svy$done_calc[svy$username %in% "t.ahmadu" & svy$end > "2018-04-17" & svy$end < "2018-04-21"] + 60*60*24
stopifnot(svy$end[svy$username %in% "t.ahmadu"]==sub(" .*", "", svy$done_calc[svy$username %in% "t.ahmadu"]))


# Randomization checks as numbers
svy$house_num <- as.numeric(as.character(svy$house_num))
svy$house_size<-as.numeric(as.character(svy$select_grp.house_size))
svy$respondent_num<-as.numeric(as.character(svy$select_grp.respondent_num))
svy$fake_rando<-ifelse(svy$respondent_num>svy$house_size,1,0)
svy$house_size[svy$house_size>200]=200


# fix mistake username
svy$username <- as.character(svy$username)
svy$username[svy$username %in% 's.ibrahim' & grepl("Idris",svy$int_name) %in% 1] <- "b.idris"
svy$int_name[svy$int_name %in% "Bullish Idris"] <- "Bilkisu Idris"
svy$username[svy$username %in% "m.orume" & svy$int_name %in% "Nafisa"] <- "n.yunusa"
svy$username[svy$username %in% "b.idris" & svy$comm_leader %in% "yes"] <- "t.ahmadu"
svy$username[svy$username %in% "e.jonah" & svy$int_name %in% "Tahiru"] <- "t.ahmadu"
svy$username[svy$username %in% "i.hassan" & svy$start > "2018-02-16" & svy$start < "2018-04-01"] <- "e.jonah" # jonah used ibrahim's tablet on 2-17-2018 - end of survey 3-1-2018
svy$username[svy$username %in% "i.hassan" & svy$start > "2018-04-09" & svy$start < "2018-04-11"] <- "f.onoja" # francis used ibrahim's username for a day....
svy$username[svy$username %in% "b.idris" & svy$start > "2018-04-09" & svy$start < "2018-04-11"] <- "i.iorkegh" # timothy as bilkisu
svy$username[svy$username %in% "i.hassan" & grepl("patience", svy$int_name, ignore.case=T)] <- "p.thomas" # Patience used Ibrahim's tablet starting 4-24-2018
svy$username[svy$username %in% "t.ahmadu" & svy$int_name %in% "ibrahim"] <- "i.hassan" # sometimes ibrahim uses tahir's tablet
svy$username[!svy$username %in% "f.onoja" & grepl("onoja", svy$int_name, ignore.case=T)] <- "f.onoja" # Onoja used other tabs because his could not connect to network.

# fix when enumerator checks "yes" to comm leader (must come after fixing usernames)
svy$id_num <- as.character(svy$id_num)
svy$pre_selected[svy$username %in% "l.aule" & svy$comm_leader %in% "yes"] <- "pre"
svy$id_num[svy$username %in% "l.aule" & svy$comm_leader %in% "yes"] <- '2001'       # lucy used comm_leader for a preselected.
svy$comm_leader[!svy$username %in% 't.ahmadu' & !svy$username %in% 'm.orume' & 
                  !svy$username %in% "f.gbenyi" & !svy$username %in% "i.hassan"] <- "no" # so far just didam, anna, lucy on 4-10-2018, simon on 4-12.
svy[svy$username %in% "f.gbenyi" & svy$comm_leader %in% "yes" & svy$site_ben %in% "sabon",c("pre_selected","id_num")] <- c("pre",2004) # Fanan gave comm_leader of sabon-gida agan the 2004 envelope because leader was accidentally listed as a preselected
svy$comm_leader[svy$username %in% "t.ahmadu" & svy$start == "2018-04-19"] <- "no" # tahiru accidentally checked "community leader" for one.
svy$comm_leader[svy$site_ben %in% "zong" & svy$farm_pastor %in% "pastoralists" & svy$gender %in% "female"] <- "no" # tahiru accidentally said this lady was the comm leader.
svy$pre_selected[!svy$pre_selected %in% "rand" & !svy$pre_selected %in% "pre" & !svy$comm_leader %in% "yes"] <- "rand"

# fix other bs
##chris: Anna accidentally submitted two a day late, and they submitted out of order.  Need to estimate their real end_time for pgp.
## and need to change their "end" date
##svy[!svy$start==svy$end & svy$resp_available %in% "yes", c("username", "start_calc", "done_calc")]
svy$done_calc[svy$username %in% "a.bako" & !svy$start==svy$end & svy$resp_available %in% "yes"] <- 
  svy$start_calc[svy$username %in% "a.bako" & !svy$start==svy$end & svy$resp_available %in% "yes"] + 60*median(svy$duration[svy$username %in% "a.bako" & svy$start == "2018-02-03"], na.rm=T) + 60*min(svy$between2[svy$username %in% "a.bako" & svy$start == "2018-02-03"], na.rm=T)
svy$end[svy$username %in% "a.bako" & !svy$start==svy$end & svy$resp_available %in% "yes"] <- svy$start[svy$username %in% "a.bako" & !svy$start==svy$end & svy$resp_available %in% "yes"]
svy$site_nas[svy$username %in% "d.daniel" & svy$end > "2018-02-01" & svy$end < "2018-02-05"] <- "usha" # didam said he was surveying loko pastoralists the whole first week.  NOTE: his farmer survey is indeed a farmer (helping suleiman).
svy$int_name[svy$username %in% "e.jonah" & svy$int_name %in% "Nafisa yunusa"] <- "Jonah Egga" # Nafis had to use Jonah's tab 4-22-2018

# fix Hausa autocorrect to english
svy$name <- as.character(svy$name_phone_grp.name)
svy$name[svy$username %in% "s.ibrahim" & svy$name %in% "Dani Niagara loko"] <- "Sanni Hassan Loko"

# fix mistaken psu & other things
svy$house_num[svy$house_num %in% 4 & svy$username %in% "b.idris" & svy$start == "2018-01-31"] <- 44
#svy[svy$username %in% "i.hassan" & svy$start == "2018-02-08", c("house_size","house_num", "respondent_num")] # Ibrahim accidentally mixed up house size, house num, and respondent num on his first day...
svy$house_num[svy$house_size %in% c(76,40)] <- c(76,40)
svy$respondent_num[svy$house_size %in% c(76,40)] <- c(1,2)
svy$house_size[svy$house_size %in% c(76,40)] <- c(3,2)
svy$house_num[svy$username %in% "b.idris" & svy$site_nas %in% 'gidan' & svy$house_num %in% 79] <- 94
svy$house_num[svy$username %in% 'b.idris' & svy$site_nas %in% "ruk" & svy$house_num %in% 53 & svy$respondent_num %in% 2] <- 62
svy[svy$username %in% "d.ruth" & svy$fake_rando %in% 1,c("house_size","respondent_num","fake_rando")] <- c(2,1,0) # dooshima mixed up house size and respondent number
svy$id_num[svy$username %in% "a.simon" & svy$house_num %in% 0] <- 2003 # simon using old fucking version, need preselected number.  Used 0 for house num of preselected.
svy$pre_selected[svy$username %in% "a.simon" & svy$house_num %in% 0] <- "pre"
svy$pre_selected[svy$username %in% "a.simon" & svy$pre_selected %in% "---"] <- "rand" # now that his pre is marked, mark simon's rand as rand.
svy$end_exp1a[svy$username %in% "d.ruth" & svy$farm_pastor %in% "pastoralists"] <- NA # because dooshima accidentally clicked "pastoralist", a farmer got asked about support for the policy endorsed by farmers...removing that
svy$farm_pastor[svy$username %in% "d.ruth"] <- "farmers" # dooshima accidentally clicked "pastoralist" once
svy[svy$username %in% "l.aule" & svy$start == "2018-04-17" & svy$pre_selected %in% "pre",
    c("pre_selected", "id_num", "house_num")] <- c("rand", NA, "547")

# remove actual "tactical" head_of_house surveys
svy$select_grp.force[svy$username %in% "f.gbenyi" & svy$start == "2018-04-16" & svy$select_grp.force %in% "head"] <- "rand"
svy <- svy[!svy$select_grp.force %in% "head",]

# Fixing preselect numbers and stuff
## benue team was re-using the preselected unique IDs, so Adaka #s repeated sabon-gida agan numberes
svy[svy$id_num %in% '7', c("id_num","house_num","pre_selected")] <- c("---",7,"rand")  # and Lucy accidentally clicked "id_num" =7 when it was house_num=7
svy$id_num[svy$site_ben %in% "ada" & !svy$id_num %in% "---" & svy$farm_pastor %in% "farmers"] <- as.numeric(svy$id_num[svy$site_ben %in% "ada" & !svy$id_num %in% "---" & svy$farm_pastor %in% "farmers"]) + 10
## Mbaku farmers were 1000 higher than they should be.
svy$id_num[svy$site_ben %in% "mbak" & svy$farm_pastor %in% "farmers" & svy$pre_selected %in% "pre"] <- 
  as.numeric(as.character(svy$id_num[svy$site_ben %in% "mbak" & svy$farm_pastor %in% "farmers" & svy$pre_selected %in% "pre"])) - 1000
svy$id_num[svy$id_num == "30003" & svy$username %in% "e.jonah"] <- "3003"  # jonah added an extra 0
svy$id_num[svy$username %in% "h.muhammad" & svy$id_num == '2020'] <- "3020" # Hamisu accidentally put 2020 instead of 3020
svy[svy$username %in% "t.ikyaave" & svy$house_num %in% 2059,c("pre_selected","id_num", "house_num")] <- c("pre", 2059, NA) # Tyrese accidentally checked "random" for a preselected
svy$id_num[svy$farm_pastor %in% "farmers" & svy$site_ben %in% "any" & svy$pre_selected %in% "pre"] <- 
  as.numeric(svy$id_num[svy$farm_pastor %in% "farmers" & svy$site_ben %in% "any" & svy$pre_selected %in% "pre"]) +10 # team used numbers 10 too low in Anyiin, since they had skipped these numbs in Mbaku and didn't know that I had fixed their mistake
svy$id_num[svy$id_num %in% 1077 & svy$name_phone_grp.name %in% "Margret Benjamin"] <- 1079
svy$id_num[svy$id_num %in% 1249 & svy$username %in% "d.alahirah"] <- 1349
svy[svy$house_num %in% 1337, c("id_num", "pre_selected", "house_num")] <- c(1337, "pre", NA) # Bilkisu accidentally clicked "random" for a preselected
svy[svy$username %in% "m.rachel" & svy$end=="2018-02-03" & svy$id_num %in% 124, 
    c("pre_selected", "id_num", "house_num")] <- c("rand", NA, 124) # moses accidntally used 124 as id_num for pre, not house_num for rand
svy$farm_pastor[svy$id_num %in% 1249] <- "farmers" # didam accidentally selected "pastoralist" for a farmer preselected...

# fix things in main pgp file
pgp$enumerator <- trimws(tolower(pgp$enumerator))
pgp$enumerator <- sub(" ", "", pgp$enumerator) # remove white space in middle also
pgp$enumerator[pgp$enumerator %in% "h.muhammed"] <- "h.muhammad"
pgp$number[pgp$number > 3050 & pgp$day=="2018-04-20" | pgp$number > 3050 & pgp$day == "2018-04-21"] <-
  pgp$number[pgp$number > 3050 & pgp$day=="2018-04-20" | pgp$number > 3050 & pgp$day == "2018-04-21"] -1000 # the mbaku mistake
pgp$number[pgp$number > 2000 & pgp$number < 3000 & pgp$day=="2018-04-24" | pgp$number > 2000 & pgp$number < 3000 & pgp$day == "2018-04-25"] <- 
  pgp$number[pgp$number > 2000 & pgp$number < 3000 & pgp$day=="2018-04-24" | pgp$number > 2000 & pgp$number < 3000 & pgp$day == "2018-04-25"]+10 # the anyii mistake
pgp$number[pgp$number %in% 1379] <- 1349
pgp$number[pgp$enumerator %in% "e.jonah" & pgp$day=="2018-02-28"] <- 1358 # Jonah's survey on 02-28 was preselected 1358

# remove from pgp the pgp envelopes with no survey match (1)
pgp <- pgp[!pgp$number %in% 1241,] # cannot find preselected 1241 in hunki from jonah
pgp_lead <- pgp[pgp$number %in% 2004,]
pgp <- pgp[!pgp$number %in% 2004,] # when fanan accidentally gave a comm_leader a pgp because comm leader was listed as preselected 2004.
pgp <- pgp[!(pgp$enumerator %in% "m.orume" & pgp$day=="2018-02-16" & pgp$number %in% 5),] # mariam gave comm leader an envelope in gidan buba
svy$end[svy$username %in% "n.yunusa" & !(svy$end==svy$start)] <- svy$start[svy$username %in% "n.yunusa" & !(svy$end==svy$start)] # nafisa had a survey stuck on her tab because she forget to hit "submit"
annaMiss <- c("AB1",1,NA,"2/28/2018", "a.bako", "nas", "", "2018-02-28") # Anna accidentally checked "comm_leader" for someone on the last day and so skipped the pgp
pgp <- rbind(pgp,annaMiss)

# match PGG money with respondent ag.df$state <- df$state[match(ag.df$site_num,df$site_num)] # chris: this is still not done! argh!
svy <- droplevels(svy)
stopifnot(table(pgp$enumerator) == table(svy$username[!svy$comm_leader %in% "yes"]))
stopifnot(all(with(svy[!svy$comm_leader %in% "yes",], table(end, username))==
                table(pgp$day, pgp$enumerator)))

# add pgp amount to svy.
## first do preselected.  then do randomly selected.  Then rbind.
pgp$pgp_amount <- as.numeric(pgp$pgp_amount)
svy$pgp_amount <- NA
svy$id_num <- as.numeric(svy$id_num)
pre <- svy[svy$pre_selected %in% "pre" & !svy$comm_leader %in% "yes",]
rand <- svy[svy$pre_selected %in% "rand",]
lead <- svy[svy$comm_leader %in% "yes",]
stopifnot(nrow(pre)+nrow(rand)+nrow(lead)==nrow(svy))
pgp$number <- as.numeric(pgp$number)
pgp_pre <- pgp[pgp$number>10,]
pgp_rand <- pgp[pgp$number<10,]
stopifnot(nrow(pgp_pre)+nrow(pgp_rand)==nrow(pgp))
stopifnot(all(table(pre$username, pre$end)==
                table(pgp_pre$enumerator, pgp_pre$day)))
stopifnot(setdiff(pgp_pre$number, pre$id_num)>0 | setdiff(pre$id_num,pgp_pre$number)>0)

##preselected
pre$pgp_amount <- pgp_pre$pgp_amount[match(pre$id_num, pgp_pre$number)]
stopifnot(table(pgp_pre$pgp_amount)==table(pre$pgp_amount))
stopifnot(tapply(pre$pgp_amount, pre$username, mean,na.rm=T)==tapply(pgp_pre$pgp_amount, pgp_pre$enumerator, mean, na.rm=T))
stopifnot(tapply(pgp_pre$number, pgp_pre$enumerator, mean, na.rm=T)==
            tapply(pre$id_num, pre$username, mean, na.rm=T))
stopifnot(tapply(pgp_pre$number, pgp_pre$day, mean, na.rm=T)==
            tapply(pre$id_num, pre$end, mean, na.rm=T))

## rand selected now - make unique id for each user-day-svy_num combo.
rand$enumDay <- interaction(rand$username, rand$end)
rand <- rand[order(rand$done_calc),]
rand$start_calc <-  as.POSIXct(rand$start_calc) # dplyr doesn't like POSIXlt class, but is okay with POSIXct
rand$done_calc <- as.POSIXct(rand$done_calc)
rand$svy_last <- as.POSIXct(rand$svy_last)

rand <- rand %>% dplyr::group_by(enumDay) %>%
  dplyr::mutate(survey_num = 1:length(state)) # can be any variable
stopifnot(table(rand$survey_num)==table(pgp_rand$number))
stopifnot(table(rand$survey_num, rand$end)==table(pgp_rand$number, pgp_rand$day))
stopifnot(all(table(rand$username, rand$end)==
                table(pgp_rand$enumerator, pgp_rand$day)))

rand$pgp_add <- interaction(rand$enumDay, rand$survey_num)
pgp_rand$pgp_add <- interaction(pgp_rand$enumerator, pgp_rand$day, pgp_rand$number)
stopifnot(table(rand$pgp_add)==table(pgp_rand$pgp_add))
stopifnot(length(rand$pgp_add)==length(unique(rand$pgp_add)))
stopifnot(length(pgp_rand$pgp_add)==length(unique(pgp_rand$pgp_add)))

pgp_rand$pgp_amount <- as.numeric(pgp_rand$pgp_amount)
rand$pgp_amount <- pgp_rand$pgp_amount[match(rand$pgp_add, pgp_rand$pgp_add)]
stopifnot(summary(rand$pgp_amount)==summary(pgp_rand$pgp_amount))
stopifnot(tapply(rand$pgp_amount, rand$username, mean,na.rm=T)==tapply(pgp_rand$pgp_amount, pgp_rand$enumerator, mean, na.rm=T))

## the one leader fanan accidentally gave an envelope to!
lead$pgp_amount <- pgp_lead$pgp_amount[match(lead$id_num, pgp_lead$number)]
#table(lead$pgp_amount, lead$id_num)

# rbind
rand <- as.data.frame(rand)
lead$survey_num <- NA
pre$survey_num <- NA
svy <- rbind(lead, pre, rand[,c(1:252,254)])

# verify right survey got right amount
stopifnot(with(svy[!svy$id_num %in% 2004,], tapply(pgp_amount, username, mean,na.rm=T))==tapply(pgp$pgp_amount, pgp$enumerator, mean, na.rm=T)) # all same without Fanan's leader survey, which I removed earlier from pgp


###############################
# New Variables
###############################
# one both state "comm/psu" variable...separate by farmer/pastor
svy$num<-1:nrow(svy)
psus<-svy[,c('num',"site_nas", "site_ben")]
psus[psus=="---"]<-NA
psu.df=cbind(psus[1], unlist(psus[-1],use.names = F))
names(psu.df)<-c("num","psu")
psu.df<-na.omit(psu.df)
psu.df<-droplevels(psu.df)
svy<-merge(svy,psu.df,by="num",all=T)
svy$community <- interaction(svy$psu, svy$farm_pastor) # after making inclusive psu var, interact with farm-pastor so the communities/psus differentiate farmer/pastoralist.

# fix things that require PSU
svy$house_num[svy$house_num %in% 23 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 71 # Dugalpi put wrong house numbers in Hunki...
svy$house_num[svy$house_num %in% 30 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 78
svy$house_num[svy$house_num %in% 24 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 72
svy$house_num[svy$house_num %in% 28 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 76
svy$house_num[svy$house_num %in% 25 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 73
svy$house_num[svy$house_num %in% 16 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 64
svy$house_num[svy$house_num %in% 21 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 69
svy$house_num[svy$house_num %in% 15 & svy$username %in% "d.alahirah" & svy$psu == "hun"] <- 63

# GPS checks
svy$gps<-ifelse(svy$location!="",1,0)

# remove surveys that are just too short (if any, find out who)
#summary(svy$duration)
svy<-svy[svy$duration>5,]

#########################################

# Chris: to use the social networks "friend_grp" data, need to fuzzy match list of friend's names to "names" before anonymizing.
## need to split into communities and do fuzzy matching within community.
## many people only gave first names, so think bout how to deal with that.

#just saving a df here to use in another file 
frd <- svy[svy$consent_survey %in% "yes",c("psu", 'name', 'farm_pastor',
                                            names(svy)[grep(".friend[1-3]",names(svy))])]
frd <- droplevels(frd)

save(frd, file="ecpn_endline_namesFriends.Rdata")




  



############################################
# Look at the comments people made

com <- svy[,c("psu", "comments")]
thanks <- c("nagode", "na gode", "thank")
com$thanks <- ifelse(grepl(paste(thanks, collapse="|"), com$comments), 1, 0)
com$tr <- ifelse(com$psu %in% 'ruk' | com$psu %in% 'tudun' | com$psu %in% 'loko'
                 | com$psu %in% 'assak' | com$psu %in% 'tork' | com$psu %in% 'zong'
                 | com$psu %in% 'any' | com$psu %in% 'mbak' | com$psu %in% 'gidan'
                 | com$psu %in% 'ashi', 1, 0)
prop.table(table(com$thanks, com$tr),2)

water <- c("borehole", "water", "ruwa")
com$water <- ifelse(grepl(paste(water, collapse="|"), com$comments), 1, 0)
prop.table(table(com$water, com$tr),2)

none <- c("no", "none", "no comment", "nothing")
com$none <- ifelse(grepl(paste(none, collapse="|"), com$comments), 1, 0)
prop.table(table(com$none, com$tr),2)

rm(com)
#











#########################################################
#Baseline
########################################################

# Final base data (cleaned in excel during collection...bad practice!  I was young!)
base <- read.csv("../raw/ecpn_base.csv")
base<- subset(base,base$consent_survey=="yes") #Remove no consent Surveys
base <- base[base$rct %in% 1,]
base <- droplevels(base)

# add names that enumerators wrote down on paper instead of putting into survey on day 1
base$num <- rownames(base)
base$num <- as.numeric(as.character(base$num))
base$name.1 <- as.character(base$name.1)
base$name.1[base$num %in% 2] <- "Suleiman"
base$name.1[base$num %in% 4] <- "Danjuma Okuwa"
base$name.1[base$num %in% 11] <- "Salisu"
base$name.1[base$num %in% 26] <- "Rukaya and Alwal"

# fix state, since some enumerators said NAS when they were surveying pastoralists in a BEN site (surveys were done just across the border)
base$state[base$site_num %in% c(21,32)] <- "ben"
#















#########################################
# Anonymize and Save
#########################################

# Anonymize the endline data
svy[,c("site_nas", "site_ben", "location", "name_phone_grp.name", "name_phone_grp.phone", 
       "name", "fake_rando", "phone", "ï..number", "d", "meta_instanceID", "X.version", "device",
       "svy_last", "comments",
       names(svy)[grepl("friend_grp",names(svy))])] <- NULL
levels(svy$psu) <- c(37, 51, 20, 50, 5, 10, 1, 4, 6, 34, 32, 35, 33, 21, 22)
svy$community <- interaction(svy$psu, svy$farm_pastor) #remake anonymous
outNames <- names(svy)[grepl("qip_aware", names(svy))] 
outNames2 <- setdiff(names(svy)[grepl("qip", names(svy))], outNames)[-c(1:2)]
names(svy)[grepl("qip_aware", names(svy))] <- paste0(sub("e_.*","", outNames),"e_",1:length(outNames))
names(svy)[names(svy) %in% outNames2] <- paste0(sub("n_.*","", outNames2),"n_",1:length(outNames2))


# final end data
svy<-droplevels(svy)
end<-svy[svy$consent_survey %in% "yes",]
end <-droplevels(end)

save(end, file="a_ecpn_endline.Rdata")



# Anonymize the baseline data in a way that I can still merge with preselected later. Chris: NOTE THAT name.1 IS NOT REMOVED
outNames <- c("info.location..text", "phone", "device", "farm_pastor",
              "rct", "num", "comments", "number",
              names(base)[grepl("LGA", names(base))], 
              names(base)[grepl("village", names(base))])

base[,outNames] <- NULL

save(base, file="a_ecpn_baseline.Rdata")

