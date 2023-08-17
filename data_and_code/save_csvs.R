rm(list=ls())

# behavioral data
## events
load("obs_dat/a_clean/a_obsDat_clean.rda")
eventOutcomes <- eventOutcomes[grepl("index", eventOutcomes)]
eventOutcomes <- c(eventOutcomes, "State", "event_type", 
                   "treatment", "time", "month_num", "site")
events <- events[,eventOutcomes]
write.csv(events, "events.csv")

## markets
outcomes <- outcomes[grepl("index", outcomes)]
marketOutcomes <- c(outcomes, "State", 
                   "treatment", "time", "month_num", "site")
markets <- markets[,marketOutcomes]
write.csv(markets, "markets.csv")

# Survey data
## aggregate
load("survey_dat/c_creating_dataframes/f-aggregateComms_ap.Rdata")
outcomes_ag.df <- c("attitude", "in_", "contactOnly",
                    "rMean", "end_exp", "pgp",
                    "vio_")
ag_vars <- names(ag.df)[grepl("cw|index", names(ag.df))]
ag_vars <- ag_vars[grepl(paste(outcomes_ag.df, collapse="|"), ag_vars)]
ag_vars <- c(ag_vars, names(ag.df)[grepl("violence_group", names(ag.df))])

agOutcomes <- c(ag_vars, "comm", "psu", "state",
                    "treatment", "radio_base", "age_base",
                "ethnic2fulani_base", "female_base",
                "income_month_base", "outgroup_atts_group.trust_rel")

ag.df <- ag.df[,agOutcomes]
write.csv(ag.df, "ag_df.csv")

## rand.df
load("survey_dat/c_creating_dataframes/rand_df.Rdata")
rand_outcomes <- gsub("_base|_end", "", agOutcomes)
rand_outcomes <- gsub("comm", "community", rand_outcomes)
rand_outcomes <- c(rand_outcomes, "survey", "religion",
                   "occupation")
rand.df <- rand.df[,rand_outcomes]
write.csv(rand.df, "rand_df.csv")

## panel
load("survey_dat/b_creating_outcomes/f2-panelData.Rdata")
pan_outcomes <- gsub("_base", "_y0", agOutcomes)
pan_outcomes <- gsub("_end", "_y1", pan_outcomes)
pan_outcomes <- gsub("comm", "community", pan_outcomes)
pan_outcomes <- c(pan_outcomes, "survey")
pan_outcomes <- gsub("female", "gender", pan_outcomes)
pan_outcomes <- gsub("ethnic2fulani", "ethnic2", pan_outcomes)
panel.df <- panel.df[,pan_outcomes]
write.csv(panel.df, "panel_df.csv")





