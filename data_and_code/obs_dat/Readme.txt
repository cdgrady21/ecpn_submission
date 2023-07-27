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
- creates mark_pasts, mark_farms, and out_ind.
- chris: need to save those for use in adjusted pvalues.
- chris: need to make sure outcomes used in coefplot is same as outcomes used in true pvalues.