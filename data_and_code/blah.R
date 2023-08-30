# Load the grDevices package
library(grDevices)

# List of PNG files to convert
png_files <- c("image1.png", "image2.png", "image3.png")

# Output PDF file name
pdf_output <- "output.pdf"

# Open a PDF device
pdf(pdf_output)

# Loop through the list of PNG files and convert them to PDF
for (png_file in png_files) {
  img <- readPNG(png_file)  # Read PNG image
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  rasterImage(img, 0, 0, 1, 1)  # Plot the image
  dev.off()  # Close the PDF device
}

# Close the PDF device
dev.off()



# look at coefs
ind.coefs[ind.coefs$var %in% "contactOnly_cw",c("coef_non", "coef_part")]
newList_ind[[3]]
# in_cw different
# attitude/alltrust a bit different
# contactOnly_cw is identical


# panel.df is my current panel.df
# panel.df_test is my old panel.df
setdiff(names(panel.df), names(panel.df_test))
setdiff(names(panel.df_test), names(panel.df))




# regression check
panel.df_test$tr_f <- droplevels(interaction(panel.df_test$treatment, panel.df_test$committee))

##contact
lm1 <- lm(panel.df$contactOnly_cw ~ tr_f+state, panel.df)
coef(lm1)['tr_f1.0']
coef(lm1)['tr_f1.1']

lm1 <- lm(panel.df$contactOnly_cw ~ tr_f+state, panel.df_test)
coef(lm1)['tr_f1.0']
coef(lm1)['tr_f1.1']

## attitude/allTrust
lm2 <- lm(panel.df$attitude_cw ~ tr_f+state, panel.df)
coef(lm2)['tr_f1.0']
coef(lm2)['tr_f1.1']

lm2 <- lm(panel.df_test$attitude_cw ~ tr_f+state, panel.df_test)
coef(lm2)['tr_f1.0']
coef(lm2)['tr_f1.1']
ind.coefs[ind.coefs$var %in% "allTrust_cw",c("coef_non", "coef_part")]

## insecurity
lm3 <- lm(panel.df$in_cw ~ tr_f+state, panel.df)
coef(lm3)['tr_f1.0']
coef(lm3)['tr_f1.1']
newList_ind[[2]]

lm3 <- lm(panel.df_test$in_cw ~ tr_f+state, panel.df_test)
coef(lm3)['tr_f1.0']
coef(lm3)['tr_f1.1']
ind.coefs[ind.coefs$var %in% "in_cw",c("coef_non", "coef_part")]

lm(panel.df$in_index ~ tr_f+state, panel.df)
lm(panel.df_test$in_index ~ tr_f+state, panel.df_test)

tapply(panel.df$in_cw, 
       panel.df$community, 
       mean, na.rm=T)
tapply(panel.df_test$in_cw, 
       panel.df_test$community, 
       mean, na.rm=T)


tapply(panel.df$in_index, 
       panel.df$community, 
       mean, na.rm=T)
tapply(panel.df_test$in_index, 
       panel.df_test$community, 
       mean, na.rm=T)


# the datasets are different, but only for the insecurity index/cw.
# the new one is correct.  Idk what's up with the old one, I must have changed how I calculated the insecurity index and never reran the panel coefs.



# LOOK AT ag.df
# ag.df_current = main
# ag.df_priv is priv, definitely correct
# ag.df_test is old.

##contact
lm(ag.df$contactOnly_cw ~ treatment+state, ag.df)
lm(ag.df$contactOnly_cw ~ treatment+state, ag.df_test)
lm(ag.df$contactOnly_cw ~ treatment+state, ag.df_priv)

## attitude/allTrust
lm(ag.df$attitude_cw ~ treatment+state, ag.df)
lm(ag.df_test$attitude_cw ~ treatment+state, ag.df)
lm(ag.df_priv$attitude_cw ~ treatment+state, ag.df)


## insecurity
lm(ag.df$in_cw ~ treatment+state, ag.df)
lm(ag.df_test$in_cw ~ treatment+state, ag.df_test)
coef(lm3)['treatment1.0']
coef(lm3)['treatment1.1']
ind.coefs[ind.coefs$var %in% "in_cw",c("coef_non", "coef_part")]

lm(ag.df$in_index ~ treatment+state, ag.df)
lm(ag.df_test$in_index ~ treatment+state, ag.df_test)

tapply(ag.df$in_cw, 
       ag.df$community, 
       mean, na.rm=T)
tapply(ag.df_test$in_cw, 
       ag.df_test$community, 
       mean, na.rm=T)


tapply(ag.df$in_index, 
       ag.df$community, 
       mean, na.rm=T)
tapply(ag.df_test$in_index, 
       ag.df_test$community, 
       mean, na.rm=T)

