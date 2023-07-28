# Important files

obsDat_coefplot_coefs.Rmd
- coefs and bootstrap CIs for observational data
- produces marketCoefs1.rda and eventCoefs1.rda
- in figs folder
- used in obsDat_figs.Rmd to make "obsDat_fig_dat.rda" ("outcome2")

obsDat_figs.Rmd
- saves "obsDat_fig_dat.rda" ("outcome2") which is used in ecpn_coefplot_final.Rmd
- chris: need to make sure outcomes used in coefplot is same as outcomes used in true pvalues.

c_obsDat_randInference.Rmd
- true pvalues for observational data
- creates "obsDat_truePs.rda", which contains true p-values for:
	- "mark_pasts": pastoralists in the market
	- "mark_farms": farmers in the market
	- "out_ind": percentage of outgroup members at social events
- chris: use "obsDat_truePs.rda" in review_appendix.Rmd to do control family-wise error rate.
- chris: need to make sure outcomes used in coefplot is same as outcomes used in true pvalues.
	- same outcomes, coefs slightly different because c_obsDat_randInference.Rmd uses fixed effects for site and state, but obsDat_coefplot_coefs.Rmd does not.