# Important files

obsDat_coefplot_coefs.Rmd
- coefs and bootstrap CIs for observational data
- produces marketCoefs1.rda and eventCoefs1.rda
- in figs folder
- used in obsDat_figs.Rmd to make "obsDat_fig_dat.rda" ("outcome2")

obsDat_figs.Rmd
- saves "obsDat_fig_dat.rda" ("outcome2") which is used in ecpn_coefplot_final.Rmd

c_obsDat_randInference.Rmd
- true pvalues for observational data
- creates "obsDat_truePs.rda", which contains true p-values for:
	- "mark_pasts": pastoralists in the market
	- "mark_farms": farmers in the market
	- "out_ind": percentage of outgroup members at social events