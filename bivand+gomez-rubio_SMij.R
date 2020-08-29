### R code from vignette source 'bivand+gomez-rubio_SMij.Rnw'

zz <- file("sunk_output.txt")
sink(zz)
sink(zz, type="message")

###################################################
### code chunk number 13: bivand+gomez-rubio_SMij.Rnw:242-254
###################################################
library(spatialprobit)
data(Katrina)
library(sf)
sf_katrina <- st_as_sf(Katrina.raw, coords=c("long", "lat"))
st_crs(sf_katrina) <- "+proj=longlat +datum=WGS84"
is.na(sf_katrina$days) <- sf_katrina$days == 99999
sf_katrina$Days <- as.Date("2005-08-29") + sf_katrina$days
t <- table(sf_katrina$Days)
diff_days <- diff(as.Date(names(t)))
names(diff_days) <- names(t)[-length(t)]
yrs <- format(as.Date(names(diff_days)), "%Y")
# o


###################################################
### code chunk number 14: bivand+gomez-rubio_SMij.Rnw:265-268
###################################################
suppressMessages(library(xtable))
mat <- table(yrs, diff_days)
#print(xtable(mat, align=c("l","r","r","r","r","r","r","r"), digits=c(NA, 0, 0, 0, 0, 0, 0, 0), display=c("s", "d", "d", "d", "d", "d", "d", "d")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 15: bivand+gomez-rubio_SMij.Rnw:283-292
###################################################
sf_katrina$days_mod <- sf_katrina$days
sf_katrina$days[is.na(sf_katrina$days)] <- 99999
sf_katrina$y0_90 <- as.numeric(!sf_katrina$days < 90)
sf_katrina$y0_180 <- as.numeric(!sf_katrina$days < 180)
sf_katrina$y0_360 <- as.numeric(!sf_katrina$days < 360)
sf_katrina$sliced <- (sf_katrina$y0_90 == 0) + (sf_katrina$y0_180 == 0) + (sf_katrina$y0_360 == 0) + as.numeric(sf_katrina$days < 99999)
sf_katrina$f_sliced <- factor(sf_katrina$sliced, levels=4:0, labels=c("day 0-90", "day 90-180", "day 180-360", "day 360 +", "never"))
# table(sf_katrina$y0_90)

library(mapview)
if (sf:::CPL_gdal_version() >= "3.1.0") mapviewOptions(fgb = FALSE)
m1 <- mapview(sf_katrina, zcol="f_sliced")
mapshot(m1, file="webmap.png")

###################################################
### code chunk number 16: bivand+gomez-rubio_SMij.Rnw:295-301
###################################################
suppressPackageStartupMessages(library(car))
sf_katrina$f_owntype <- recode(sf_katrina$owntype, "1='sole'; 2='local'; 3='national'", as.factor=TRUE)
sf_katrina$f_sesstatus <- recode(sf_katrina$sesstatus, "c(1,2)='lower'; c(4,5)='upper'; else='av'", as.factor=TRUE)
sf_katrina$f_sizeemp <- recode(sf_katrina$sizeemp, "1='small'; 2='av'; 3='large'", as.factor=TRUE)
sf_katrina$f_street <- recode(sf_katrina$street, "1='Magazine Street'; 2='Carrollton Avenue'; 3='St. Claude Avenue'", as.factor=TRUE, levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue"))
sf_katrina$elevation1 <- sf_katrina$elevation - min(sf_katrina$elevation)


###################################################
### code chunk number 17: bivand+gomez-rubio_SMij.Rnw:313-317
###################################################
mat <- table(sf_katrina$f_sliced, sf_katrina$f_street)
mat <- rbind(mat, apply(mat, 2, sum))
rownames(mat)[6] <- "sum"
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 0, 0, 0), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,5,6))


###################################################
### code chunk number 18: bivand+gomez-rubio_SMij.Rnw:333-341
###################################################
sf_katrina$days_mod[is.na(sf_katrina$days_mod)] <- 450
sf_katrina_41 <- sf_katrina[sf_katrina$days_mod > 41,]
sf_katrina_41$days_mod <- sf_katrina_41$days_mod - 41
sf_katrina_41$status <- !(sf_katrina_41$days_mod == 409)
sf_katrina_41$days_mod1 <- sf_katrina_41$days_mod/30 # approximate months
sf_katrina_41$days_mod2 <- sf_katrina_41$days_mod/7 # approximate weeks
#sf_katrina_41$days_mod1 <- sf_katrina_41$days_mod/max(sf_katrina_41$days_mod) # Extreme re-scaling to avoid problems with INLA (?)
# print(prop.table(table(sf_katrina_41$status, sf_katrina_41$f_street), margin=2)*100, digits=3)


###################################################
### code chunk number 19: bivand+gomez-rubio_SMij.Rnw:349-352
###################################################
mat <- prop.table(table(sf_katrina_41$status, sf_katrina_41$f_street), margin=2)*100
rownames(mat) <- c("still closed", "re-opened")
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 20: bivand+gomez-rubio_SMij.Rnw:370-377
###################################################
sf_katrina_41$f_owntype1 <- recode(sf_katrina_41$owntype, "1='sole'; c(2,3)='chain'", as.factor=TRUE)
sf_katrina_41$f_sesstatus1 <- recode(sf_katrina_41$sesstatus, "c(1,2)='lower'; c(3,4,5)='av'", as.factor=TRUE)
sf_katrina_41$f_sizeemp1 <- recode(sf_katrina_41$sizeemp, "1='small'; c(2,3)='av'", as.factor=TRUE)
sf_katrina_41$f_flood <- factor(as.integer(sf_katrina_41$flood < -4))
sf_katrina_41$flood1 <- abs(sf_katrina_41$flood)
sf_katrina_41$all_id <- 1:nrow(sf_katrina_41)
streets_41 <- split(sf_katrina_41, sf_katrina_41$f_street)


###################################################
### code chunk number 21: bivand+gomez-rubio_SMij.Rnw:479-482
###################################################
library(survival)
km_all <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = sf_katrina_41)
km_streets <- survfit(Surv(time = days_mod1, event = status) ~ f_street, data = sf_katrina_41)


###################################################
### code chunk number 22: bivand+gomez-rubio_SMij.Rnw:486-488
###################################################
km_allw <- survfit(Surv(time = days_mod2, event = status) ~ 1, data = sf_katrina_41)
km_streetsw <- survfit(Surv(time = days_mod2, event = status) ~ f_street, data = sf_katrina_41)


###################################################
### code chunk number 23: bivand+gomez-rubio_SMij.Rnw:492-512
###################################################
# VGR: PLots of observations per time
obs.time <- as.data.frame(table(round(sf_katrina_41$days_mod2, 3)))
obs.time$Var1 <- as.numeric(as.character(obs.time$Var1))

# Required to draw axis
slices_d <- (c(41, 90, 180, 360)-41)/7
library(zoo)
lbs <- as.yearmon(seq(as.Date("2005-08-29")+41, as.Date("2006-08-29")+41, 90))

suppressPackageStartupMessages(library(ggplot2))
#cairo_ps(file="fig_data.eps", width = 6, height = 4, pointsize = 10,
# onefile = TRUE)
pdf(file = "fig1.pdf")
obs.time1 <- obs.time[-nrow(obs.time),]

ggplot(obs.time1) + # Remove censored shops
  geom_line(aes(x = Var1, y = Freq)) + 
  geom_vline(xintercept=slices_d, lty=3, lwd=1) + scale_x_continuous(breaks=seq(0, 400/7, 90/7), labels=as.character(lbs)) +
  xlab("Date") + ylab("Number of businesses that re-opened")
dev.off()


###################################################
### code chunk number 24: bivand+gomez-rubio_SMij.Rnw:517-525
###################################################
#slices_d <- (c(41, 90, 180, 360)-41)/7
#library(zoo)
#lbs <- as.yearmon(seq(as.Date("2005-08-29")+41, as.Date("2006-08-29")+41, 90))
pdf(file = "fig2.pdf")
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
autoplot(km_streetsw) + geom_vline(xintercept=slices_d, lty=3, lwd=1) + scale_x_continuous(breaks=seq(0, 400/7, 90/7), labels=as.character(lbs))
dev.off()

###################################################
### code chunk number 25: bivand+gomez-rubio_SMij.Rnw:533-535
###################################################
wb_modelb_41w <- survreg(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1, data = sf_katrina_41, dist="weibull")


###################################################
### code chunk number 26: bivand+gomez-rubio_SMij.Rnw:538-547
###################################################
mag <- sf_katrina_41[sf_katrina_41$f_street == "Magazine Street",]
wb_modela_41_mag <- survreg(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1, data = mag, dist="weibull")
car <- sf_katrina_41[sf_katrina_41$f_street == "Carrollton Avenue",]
wb_modela_41_car <- survreg(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1, data = car, dist="weibull")
stc <- sf_katrina_41[sf_katrina_41$f_street == "St. Claude Avenue",]
wb_modela_41_stc <- survreg(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1, data = stc, dist="weibull")


###################################################
### code chunk number 27: bivand+gomez-rubio_SMij.Rnw:550-551
###################################################
suppressPackageStartupMessages(library(INLA))


###################################################
### code chunk number 28: bivand+gomez-rubio_SMij.Rnw:554-566
###################################################
sf_katrina_41$st_id <- as.numeric(sf_katrina_41$f_street)
#all_surv_41 <- Surv(time = sf_katrina_41$days_mod2, event = sf_katrina_41$status)
#all_surv_inla_41 <- inla.surv(all_surv_41[,1], all_surv_41[,2])

# to match later models: inla.surv(time = days_mod2 / 60, event = status)

wb_inla_fita_41 <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 +
    f_sesstatus1 + log(medinc) + f_sizeemp1 +
    f(st_id, model="iid", hyper = list(prec = list(param = c(0.001, 0.001)))),
  data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10))
wb_inla_fita_41 <- inla.rerun(wb_inla_fita_41)
saveRDS(wb_inla_fita_41, file="wb_inla_fita_41.rds")


###################################################
### code chunk number 29: bivand+gomez-rubio_SMij.Rnw:569-571
###################################################
wb_inla_fitb_41 <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street, data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))), control.fixed = list(prec = 1e-10))
# saveRDS(wb_inla_fitb_41, file="wb_inla_all.rds")


###################################################
### code chunk number 30: bivand+gomez-rubio_SMij.Rnw:574-577
###################################################
wb_inla_fitb_41_nostreet <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))), control.fixed = list(prec = 1e-10))
wb_inla_fitb_41_nostreet <- inla.rerun(wb_inla_fitb_41_nostreet)
saveRDS(wb_inla_fitb_41_nostreet, file="wb_inla_all_nostreet.rds")


###################################################
### code chunk number 31: bivand+gomez-rubio_SMij.Rnw:580-585
###################################################
#mag_surv_41 <- Surv(time = mag$days_mod2, event = mag$status)
#mag_surv_inla_41 <- inla.surv(mag_surv_41[,1], mag_surv_41[,2])
wb_inla_fitb_41_mag <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = mag, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))), control.fixed = list(prec = 1e-10))
wb_inla_fitb_41_mag <- inla.rerun(wb_inla_fitb_41_mag)
saveRDS(wb_inla_fitb_41_mag, file="wb_inla_mag.rds")


###################################################
### code chunk number 32: bivand+gomez-rubio_SMij.Rnw:588-593
###################################################
#stc_surv_41 <- Surv(time = stc$days_mod2, event = stc$status)
#stc_surv_inla_41 <- inla.surv(stc_surv_41[,1], stc_surv_41[,2])
wb_inla_fitb_41_stc <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = stc, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))), control.fixed = list(prec = 1e-10))
wb_inla_fitb_41_stc <- inla.rerun(wb_inla_fitb_41_stc)
saveRDS(wb_inla_fitb_41_stc, file="wb_inla_stc.rds")


###################################################
### code chunk number 33: bivand+gomez-rubio_SMij.Rnw:596-601
###################################################
#car_surv_41 <- Surv(time = car$days_mod2, event = car$status)
#car_surv_inla_41 <- inla.surv(car_surv_41[,1], car_surv_41[,2])
wb_inla_fitb_41_car <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = car, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))), control.fixed = list(prec = 1e-10))
wb_inla_fitb_41_car <- inla.rerun(wb_inla_fitb_41_car)
saveRDS(wb_inla_fitb_41_car, file="wb_inla_car.rds")


###################################################
### code chunk number 37: bivand+gomez-rubio_SMij.Rnw:734-737
###################################################
suppressPackageStartupMessages(library(spBayesSurv))
mcmc <- list(nburn = 50000, nsave = 150000, nskip = 5, ndisplay= 20000)
prior <- list(maxL = 15)


###################################################
### code chunk number 38: bivand+gomez-rubio_SMij.Rnw:742-745 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_mag, "wb_mag.rds")


###################################################
### code chunk number 39: bivand+gomez-rubio_SMij.Rnw:749-752 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_car, "wb_car.rds")


###################################################
### code chunk number 40: bivand+gomez-rubio_SMij.Rnw:756-759 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_stc, "wb_stc.rds")


###################################################
### code chunk number 41: bivand+gomez-rubio_SMij.Rnw:762-765 (eval = FALSE)
###################################################
## set.seed(1)
## suppressMessages(wb_all <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street, data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_all, "wb_all.rds")


###################################################
### code chunk number 42: bivand+gomez-rubio_SMij.Rnw:769-772 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all_nostreet <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_all_nostreet, "wb_all_nostreet_plain.rds")


###################################################
### code chunk number 43: bivand+gomez-rubio_SMij.Rnw:775-778 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all_nostreet_iidst <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("iid", st_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_all_nostreet_iidst, "wb_all_nostreet_iidst.rds")


###################################################
### code chunk number 45: bivand+gomez-rubio_SMij.Rnw:807-861 (eval = FALSE)
###################################################
# Function to extract 95% credible intervals
#x: Matrix of MCMC samples; variables by row
#sign: Wehther to multiply values by -1 to flip signs.
# Returns as column-wise matrix
# IMPORTANT: Performs sign shift and swaps columns as a result
CI95 <- function(x, signflip = TRUE) {
  tab <- t(apply(x, 1, function(X) {
    quantile(X, c(0.025, 0.975))
  }))

  if(signflip) {
    tab <- -tab[, 2:1]
    colnames(tab) <- colnames(tab)[2:1]
  }

  return(tab)
}

# Load results and create table for plotting
wb_mag <- readRDS("wb_mag.rds")
wb_car <- readRDS("wb_car.rds")
wb_stc <- readRDS("wb_stc.rds")
wb_all_fit1 <- readRDS("wb_all_nostreet_plain.rds") #readRDS("wb_all.rds")
c1 <- summary(wb_mag)$coeff[, c(1, 3)]
c1[, 1] <- -1*c1[, 1]
c1 <- cbind(c1, CI95(wb_mag$beta))
c2 <- summary(wb_car)$coeff[, c(1, 3)]
c2[, 1] <- -1*c2[, 1]
c2 <- cbind(c2, CI95(wb_car$beta))
c3 <- summary(wb_stc)$coeff[, c(1, 3)]
c3[, 1] <- -1*c3[, 1]
c3 <- cbind(c3, CI95(wb_stc$beta))
c4 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
c4[, 1] <- -1*c4[, 1]
c4 <- cbind(c4, CI95(wb_all_fit1$beta[1:6,]))
wb_all_stiid <- readRDS("wb_all_nostreet_iidst.rds")
c5 <- summary(wb_all_stiid)$coeff[, c(1, 3)]
c5[, 1] <- -1*c5[, 1]
c5 <- cbind(c5, CI95(wb_all_stiid$beta))

res <- rbind(c1, c2, c3, c4, c5)
saveRDS(res, "figure_mat_mcmc_plain.rds")
dens <- apply(-wb_all_stiid$v, 1, density)
saveRDS(dens, file="mcmc_st_iid_densities.rds")
## library(coda)
## rd_pld <- raftery.diag(as.mcmc(t(wb_all_fit1$beta)))
## rd_stc <- raftery.diag(as.mcmc(t(wb_stc$beta)))
## rd_car <- raftery.diag(as.mcmc(t(wb_car$beta)))
## rd_mag <- raftery.diag(as.mcmc(t(wb_mag$beta)))
## # save(rd_mag, rd_stc, rd_car, rd_pld, file="rd_simple_mcmc.rda")
## gd_pld <- geweke.diag(as.mcmc(t(wb_all_fit1$beta)))
## gd_stc <- geweke.diag(as.mcmc(t(wb_stc$beta)))
## gd_car <- geweke.diag(as.mcmc(t(wb_car$beta)))
## gd_mag <- geweke.diag(as.mcmc(t(wb_mag$beta)))
## gd_names <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "Carrollton Avenue", "St. Claude Avenue")
## # save(gd_mag, gd_stc, gd_car, gd_pld, gd_names, file="gd_simple_mcmc.rda")

###################################################
### code chunk number 77: bivand+gomez-rubio_SMij.Rnw:1698-1735 (eval = FALSE)
###################################################
cs_wb_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_car, "cs_wb_car.rds")
cs_wb_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_stc, "cs_wb_stc.rds")
cs_wb_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_mag, "cs_wb_mag.rds")
cs_wb_all <- cox.snell.survregbayes(wb_all_fit1, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_all, file="cs_wb_all_nostreet.rds")
cs_wb_stiid <- cox.snell.survregbayes(wb_all_stiid, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_stiid, file="cs_wb_stiid.rds")


###################################################
### code chunk number 34: bivand+gomez-rubio_SMij.Rnw:634-664 (eval = FALSE)
###################################################
#### AFT models with INLA + pooled with street dummies
wb_inla_mag <- readRDS("wb_inla_mag.rds")
wb_inla_car <- readRDS("wb_inla_car.rds")
wb_inla_stc <- readRDS("wb_inla_stc.rds")
wb_inla_all <- readRDS("wb_inla_all_nostreet.rds") # Pooled
wb_inla_fita_41 <- readRDS("wb_inla_fita_41.rds") # Pooled + street IID
## 
## # res <- readRDS("figure_mat_mcmc_plain.rds")
res <- rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)],
  wb_inla_car$summary.fixed[-1, c(1:3, 5)],
  wb_inla_stc$summary.fixed[-1, c(1:3, 5)],
  wb_inla_all$summary.fixed[-1, c(1:3, 5)], 
  wb_inla_fita_41$summary.fixed[2:7, c(1:3, 5)]
)
dres <- as.data.frame(res)

# Change signs
dres[, c(1, 3:4)] <- - dres[, c(1, 3:4)]

dres$covariate <- factor(rep(rownames(res)[1:6], 5), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled", "Pooled IID"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled", "Pooled IID"))
## #dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
## #dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## #dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## # Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]
dres_inla <- dres
dres_inla$Estimator <- "INLA"

res <- readRDS("figure_mat_mcmc_plain.rds")
dres <- as.data.frame(res)
dres[, c(1, 3:4)] <- - dres[, c(1, 3:4)]

dres$covariate <- factor(rep(rownames(res)[1:6], 5), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled", "Pooled IID"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled", "Pooled IID"))
## #dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
## #dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## #dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## # Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]
dres_mcmc <- dres
dres_mcmc$Estimator <- "MCMC"

names(dres_inla)[3:4] <- names(dres_mcmc)[3:4]
dres <- rbind(dres_inla, dres_mcmc)
saveRDS(dres, file="plain_dres_mcmc_inla.rds")


###################################################
### code chunk number 35: bivand+gomez-rubio_SMij.Rnw:668-673
###################################################
pdf(file = "fig3.pdf")
dres <- readRDS("plain_dres_mcmc_inla.rds")
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, colour=Data, shape=Estimator)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()

###################################################
### code chunk number 36: bivand+gomez-rubio_SMij.Rnw:705-724
###################################################
pdf(file = "fig4.pdf")
mcmc_re <- readRDS("mcmc_st_iid_densities.rds")
# wb_inla_fita_41 <- readRDS("wb_inla_fita_41.rds") # Pooled + street IID
inla_re <- wb_inla_fita_41$marginals.random$st_id
saveRDS(inla_re, file="inla_st_iid_margs.rds")
inla_re <- readRDS("inla_st_iid_margs.rds")
oopar <- par(mfrow=c(1,3))
plot(inla_re[[1]] %*% diag(c(-1, 1)), type="l", xlim=c(-3.5, 2.5), xlab="", ylab="", main="Magazine Street")
lines(mcmc_re[[1]], lty=2)
abline(v=0, lty=3)
plot(inla_re[[2]] %*% diag(c(-1, 1)), type="l", xlim=c(-3.5, 2.5), xlab="", ylab="", main="Carrollton Avenue")
lines(mcmc_re[[2]], lty=2)
abline(v=0, lty=3)
plot(inla_re[[3]] %*% diag(c(-1, 1)), type="l", xlim=c(-1.5, 3.5), xlab="", ylab="", main="St. Claude Avenue")
lines(mcmc_re[[3]], lty=2)
abline(v=0, lty=3)
legend("topright", legend=c("INLA", "MCMC"), lty=1:2, bty="n", cex=0.8)
par(oopar)
dev.off()


###################################################
### code chunk number 46: bivand+gomez-rubio_SMij.Rnw:870-875 (eval = FALSE)
###################################################
## load("gd_simple_mcmc.rda")
## mat <- cbind(c(gd_mag$z, NA, NA), c(gd_stc$z, NA, NA), c(gd_car$z, NA, NA), gd_pld$z)
## rownames(mat) <- gd_names
## colnames(mat) <- c("Magazine St", "St Claude Ave", "Carrollton Ave", "Pooled")
## print(xtable(mat, align=c("l","r","r","r", "r"), digits=c(NA, 5, 5, 5, 5), display=c("s", "f", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,8))

###################################################
### code chunk number 77: bivand+gomez-rubio_SMij.Rnw:1698-1735 (eval = FALSE)
###################################################
set.seed(1)
cs_wb_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_car, "cs_wb_car.rds")
set.seed(1)
cs_wb_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_stc, "cs_wb_stc.rds")
set.seed(1)
cs_wb_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_mag, "cs_wb_mag.rds")


###################################################
### code chunk number 48: bivand+gomez-rubio_SMij.Rnw:919-1003
###################################################
cs_wb_mag <- readRDS("cs_wb_mag.rds")
cs_wb_car <- readRDS("cs_wb_car.rds")
cs_wb_stc <- readRDS("cs_wb_stc.rds")
wb_all_fit1 <- readRDS("wb_all_nostreet_plain.rds")
set.seed(1)
cs_wb_all <- cox.snell.survregbayes(wb_all_fit1, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_all, file="cs_wb_all_nostreet.rds")
cs_wb_all <- readRDS("cs_wb_all_nostreet.rds")
wb_all_stiid <- readRDS("wb_all_nostreet_iidst.rds")
set.seed(1)
cs_wb_stiid <- cox.snell.survregbayes(wb_all_stiid, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_stiid, file="cs_wb_stiid.rds")
cs_wb_stiid <- readRDS("cs_wb_stiid.rds")
#saveRDS(cs_wb_grf_mag, "cs_wb_grf_mag.rds")

#(load(file="summary_stuff.RData"))
#(load(file="alls_summaries.RData"))
pdf(file = "fig5.pdf")
opar <- par(mfrow=c(1,5), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Magazine St")
fit <- survival::survfit(cs_wb_mag$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_mag[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Carrollton Ave")
fit <- survival::survfit(cs_wb_car$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_car[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="St Claude Ave")
fit <- survival::survfit(cs_wb_stc$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_stc[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Pooled")
fit <- survival::survfit(cs_wb_all$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_all[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Street IID")
fit <- survival::survfit(cs_wb_stiid$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_stiid[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
par(opar)
dev.off()
rm(wb_all_fit1, wb_all_stiid, wb_inla_mag, wb_inla_car, wb_inla_stc, wb_inla_all, wb_all_fit1, wb_car, wb_mag, wb_stc)
gc()


###################################################
### code chunk number 95: bivand+gomez-rubio_SMij.Rnw:2296-2317 (eval = FALSE)
###################################################
## ## Probit models with INLA
## 
suppressMessages(probit_inla_mag <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = mag, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
suppressMessages(probit_inla_mag <- inla.rerun(probit_inla_mag))
saveRDS(probit_inla_mag, "probit_inla_mag.rds")
## 
suppressMessages(probit_inla_car <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = car, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
suppressMessages(probit_inla_car <- inla.rerun(probit_inla_car))
saveRDS(probit_inla_car, "probit_inla_car.rds")
## 
## 
suppressMessages(probit_inla_stc <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = stc, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
suppressMessages(probit_inla_stc <- inla.rerun(probit_inla_stc))
saveRDS(probit_inla_stc, "probit_inla_stc.rds")
## 
suppressMessages(probit_inla_all <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = sf_katrina_41, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
suppressMessages(probit_inla_all <- inla.rerun(probit_inla_all))
saveRDS(probit_inla_all, "probit_inla_all_nostreet.rds")
## 
## 
## 



###################################################
### code chunk number 49: bivand+gomez-rubio_SMij.Rnw:1042-1075
###################################################
### probit models with INLA
wb_inla_mag <- readRDS("probit_inla_mag.rds")
wb_inla_car <- readRDS("probit_inla_car.rds")
wb_inla_stc <- readRDS("probit_inla_stc.rds")
wb_inla_all <- readRDS("probit_inla_all_nostreet.rds")

# res <- readRDS("figure_mat_mcmc_plain.rds")
res <- rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)],
  wb_inla_car$summary.fixed[-1, c(1:3, 5)],
  wb_inla_stc$summary.fixed[-1, c(1:3, 5)],
  wb_inla_all$summary.fixed[2:7, c(1:3, 5)] # Includes f_street
)
dres <- as.data.frame(res)

# Change signs
#dres[, c(1, 3:4)] <- - dres[, c(1, 3:4)]

dres$covariate <- factor(rep(rownames(res)[1:6], 4), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]


pdf(file = "fig6.pdf")
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, colour=Data)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()

###################################################
### code chunk number 50: bivand+gomez-rubio_SMij.Rnw:1098-1101 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_fit1_nostreet <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("iid", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_iid_fit1_nostreet, "wb_iid_fit1_nostreet.rds")


###################################################
### code chunk number 51: bivand+gomez-rubio_SMij.Rnw:1105-1108 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_iid_mag, "wb_iid_mag.rds")


###################################################
### code chunk number 52: bivand+gomez-rubio_SMij.Rnw:1112-1115 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_iid_car, "wb_iid_car.rds")


###################################################
### code chunk number 53: bivand+gomez-rubio_SMij.Rnw:1119-1122 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
saveRDS(wb_iid_stc, "wb_iid_stc.rds")


###################################################
### code chunk number 54: bivand+gomez-rubio_SMij.Rnw:1125-1148 (eval = FALSE)
###################################################
## ## Models fit with INLA
## 
## # IMPORTANT!!! TIme = time / 60 (max. time = 58.42)
## 
## # Models for MAG, CAR and ALL cannot be fit without re-scaling time
## 
suppressMessages(wb_inla_iid_mag <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "iid", hyper = list(prec = list(param = c(0.001, 0.001)))), data = mag, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_iid_mag <- inla.rerun(wb_inla_iid_mag)
saveRDS(wb_inla_iid_mag, "wb_inla_iid_mag.rds")
## 
## 
suppressMessages(wb_inla_iid_car <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "iid", hyper = list(prec = list(param = c(0.001, 0.001)))), data = car, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_iid_car <- inla.rerun(wb_inla_iid_car)
saveRDS(wb_inla_iid_car, "wb_inla_iid_car.rds")
## 
suppressMessages(wb_inla_iid_stc <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "iid", hyper = list(prec = list(param = c(0.001, 0.001)))), data = stc, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_iid_stc <- inla.rerun(wb_inla_iid_stc)
saveRDS(wb_inla_iid_stc, "wb_inla_iid_stc.rds")
## 
## 
suppressMessages(wb_inla_iid_all_nostreet <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "iid", hyper = list(prec = list(param = c(0.001, 0.001)))), data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
wb_inla_iid_all_nostreet <- inla.rerun(wb_inla_iid_all_nostreet)
saveRDS(wb_inla_iid_all_nostreet, "wb_inla_iid_all_nostreet.rds")


###################################################
### code chunk number 55: bivand+gomez-rubio_SMij.Rnw:1151-1183 (eval = FALSE)
###################################################
wb_mag <- readRDS("wb_iid_mag.rds")
wb_car <- readRDS("wb_iid_car.rds")
wb_stc <- readRDS("wb_iid_stc.rds")
wb_all_fit1 <- readRDS("wb_iid_fit1_nostreet.rds")
c1 <- summary(wb_mag)$coeff[, c(1, 3)]
c1[, 1] <- -1*c1[, 1]
c1 <- cbind(c1, CI95(wb_mag$beta))
c2 <- summary(wb_car)$coeff[, c(1, 3)]
c2[, 1] <- -1*c2[, 1]
c2 <- cbind(c2, CI95(wb_car$beta))
c3 <- summary(wb_stc)$coeff[, c(1, 3)]
c3[, 1] <- -1*c3[, 1]
c3 <- cbind(c3, CI95(wb_stc$beta))
c4 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
c4[, 1] <- -1*c4[, 1]
c4 <- cbind(c4, CI95(wb_all_fit1$beta[1:6, ]))
res <- rbind(c1, c(summary(wb_mag)$frailvar[1, c(1,3:5)]), c2, c(summary(wb_car)$frailvar[1, c(1,3:5)]), c3, c(summary(wb_stc)$frailvar[1, c(1,3:5)]), c4, c(summary(wb_all_fit1)$frailvar[1, c(1,3:5)]))
rownames(res)[c(7, 14, 21, 28)] <- "frailtyvar"
saveRDS(res, file="wb_iid_res.rds")

set.seed(1)
cs_wb_iid_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_iid_car, "cs_wb_iid_car.rds")
set.seed(1)
cs_wb_iid_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_iid_stc, "cs_wb_iid_stc.rds")
set.seed(1)
cs_wb_iid_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_iid_mag, "cs_wb_iid_mag.rds")


## 
## library(coda)
## rd_pld_iid <- raftery.diag(as.mcmc(cbind(t(wb_all_fit1$beta), wb_all_fit1$tau2)))
## rd_stc_iid <- raftery.diag(as.mcmc(cbind(t(wb_stc$beta), wb_stc$tau2)))
## rd_car_iid <- raftery.diag(as.mcmc(cbind(t(wb_car$beta), wb_car$tau2)))
## rd_mag_iid <- raftery.diag(as.mcmc(cbind(t(wb_mag$beta), wb_mag$tau2)))
## # save(rd_mag_iid, rd_stc_iid, rd_car_iid, rd_pld_iid, file="rd_iid_mcmc.rda")
## gd_pld_iid <- geweke.diag(as.mcmc(cbind(t(wb_all_fit1$beta), wb_all_fit1$tau2)))
## gd_stc_iid <- geweke.diag(as.mcmc(cbind(t(wb_stc$beta), wb_stc$tau2)))
## gd_car_iid <- geweke.diag(as.mcmc(cbind(t(wb_car$beta), wb_car$tau2)))
## gd_mag_iid <- geweke.diag(as.mcmc(cbind(t(wb_mag$beta), wb_mag$tau2)))
## # save(gd_mag_iid, gd_stc_iid, gd_car_iid, gd_pld_iid, file="gd_iid_mcmc.rda")
## 


###################################################
### code chunk number 56: bivand+gomez-rubio_SMij.Rnw:1187-1201 (eval = FALSE)
###################################################
res <- readRDS("wb_iid_res.rds")
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "IID frailty")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# USe 95% credible intervals
dres$ymax <- dres[, 4]
dres$ymin <- dres[, 3]
dres$Estimator <- "MCMC"
saveRDS(dres, file="wb_iid_mcmc_dres.rds")



###################################################
### code chunk number 89: bivand+gomez-rubio_SMij.Rnw:2078-2125 (eval = FALSE)
###################################################
### IID models with INLA
wb_inla_mag <- readRDS("wb_inla_iid_mag.rds")
wb_inla_car <- readRDS("wb_inla_iid_car.rds")
wb_inla_stc <- readRDS("wb_inla_iid_stc.rds")
wb_inla_all <- readRDS("wb_inla_iid_all_nostreet.rds")
## 
#Summary of variance
#obj: INLA object
summary.var <- function(obj) {
  res <- unlist(
    inla.zmarginal(inla.tmarginal(function(x) exp(-x),
      obj$internal.marginals.hyperpar[[2]]), silent = TRUE)[c(1:3, 7)])
  return(res)
}
## 
## # res <- readRDS("figure_mat_mcmc_plain.rds")
res <- rbind(
  rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_mag)),
  rbind(wb_inla_car$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_car)),
  rbind(wb_inla_stc$summary.fixed[-1, c(1:3, 5)], variance= summary.var(wb_inla_stc)),
  rbind(wb_inla_all$summary.fixed[2:7, c(1:3, 5)], variance = summary.var(wb_inla_all)) # Includes f_street
)
dres <- as.data.frame(res)

dres$covariate <- factor(rep(rownames(res)[1:7], 4), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "variance"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "IID frailty")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd

# Change signs
idx <- dres$covariate != "IID frailty"
dres[idx, c(1, 3:4)] <- - dres[idx, c(1, 3:4)]


# Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]
dres$Estimator <- "INLA"
saveRDS(dres, "wb_iid_inla_dres.rds")


###################################################
### code chunk number 57: bivand+gomez-rubio_SMij.Rnw:1205-1214
###################################################
dres_mcmc <- readRDS("wb_iid_mcmc_dres.rds")
dres_inla <- readRDS("wb_iid_inla_dres.rds")
names(dres_inla) <- names(dres_mcmc)
dres <- rbind(dres_inla, dres_mcmc)

pdf(file = "fig7.pdf")
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, colour=Data, shape=Estimator)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()


###################################################
### code chunk number 58: bivand+gomez-rubio_SMij.Rnw:1222-1285
###################################################
cs_wb_mag <- readRDS("cs_wb_iid_mag.rds")
cs_wb_car <- readRDS("cs_wb_iid_car.rds")
cs_wb_stc <- readRDS("cs_wb_iid_stc.rds")
wb_iid_fit1_nostreet <- readRDS("wb_iid_fit1_nostreet.rds")
set.seed(1)
cs_wb_all  <- cox.snell.survregbayes(wb_iid_fit1_nostreet, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_all, file="cs_wb_iid_nostreet.rds")
cs_wb_all <- readRDS("cs_wb_iid_nostreet.rds")
pdf(file = "fig8.pdf")
opar <- par(mfrow=c(1,4), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Magazine Street")
fit <- survival::survfit(cs_wb_mag$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_mag[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Carrollton Avenue")
fit <- survival::survfit(cs_wb_car$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_car[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="St Claude Avenue")
fit <- survival::survfit(cs_wb_stc$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_stc[[i + 1]] ~ 1)
 try(lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey"), silent=TRUE)
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Pooled")
fit <- survival::survfit(cs_wb_all$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_all[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
par(opar)
dev.off()

rm(wb_iid_fit1_nostreet, wb_inla_mag, wb_inla_car, wb_inla_stc, wb_inla_all, wb_all_fit1, wb_car, wb_mag, wb_stc)
gc()



###################################################
### code chunk number 59: bivand+gomez-rubio_SMij.Rnw:1308-1313 (eval = FALSE)
###################################################
## load("gd_iid_mcmc.rda")
## mat <- cbind(c(gd_mag_iid$z[1:6], NA, NA, gd_mag_iid$z[7]), c(gd_stc_iid$z[1:6], NA, NA, gd_stc_iid$z[7]), c(gd_car_iid$z[1:6], NA, NA, gd_car_iid$z[7]), gd_pld_iid$z)
## rownames(mat) <- c(gd_names, "Tau2")
## colnames(mat) <- c("Magazine St", "St Claude Ave", "Carrollton Ave", "Pooled")
## print(xtable(mat, align=c("l","r","r","r", "r"), digits=c(NA, 5, 5, 5, 5), display=c("s", "f", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,9))


###################################################
### code chunk number 60: bivand+gomez-rubio_SMij.Rnw:1363-1373
###################################################
library(spdep)
nb <- knn2nb(knearneigh(sf_katrina_41, k=11), sym=TRUE)
#nb
#n.comp.nb(nb)$nc
E_all <- nb2mat(nb, style="B")
#tf <- tempfile()
#nb2INLA(nb, file=tf)
E_mag <- nb2mat(subset(nb, sf_katrina_41$f_street == "Magazine Street"), style="B")
E_car <- nb2mat(subset(nb, sf_katrina_41$f_street == "Carrollton Avenue"), style="B")
E_stc <- nb2mat(subset(nb, sf_katrina_41$f_street == "St. Claude Avenue"), style="B")


###################################################
### code chunk number 61: bivand+gomez-rubio_SMij.Rnw:1377-1380 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_mag_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_mag))
saveRDS(wb_mag_fit, "wb_mag_fit.rds")


###################################################
### code chunk number 62: bivand+gomez-rubio_SMij.Rnw:1384-1387 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_car_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_car))
saveRDS(wb_car_fit, "wb_car_fit.rds")


###################################################
### code chunk number 63: bivand+gomez-rubio_SMij.Rnw:1390-1393 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_stc_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_stc))
saveRDS(wb_stc_fit, "wb_stc_fit.rds")


###################################################
### code chunk number 64: bivand+gomez-rubio_SMij.Rnw:1396-1399 (eval = FALSE)
###################################################
## set.seed(1)
## suppressMessages(wb_all_fit1 <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street + frailtyprior("car", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_all))
## # saveRDS(wb_all_fit1, "wb_all_fit1.rds")


###################################################
### code chunk number 65: bivand+gomez-rubio_SMij.Rnw:1403-1415 (eval = FALSE)
###################################################
set.seed(1)
# Pooled model without street dummies
suppressMessages(wb_all_fit1_nostreet <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_all))
saveRDS(wb_all_fit1_nostreet, "wb_all_fit1_nostreet.rds")
## 
## geweke.diag(as.mcmc(cbind(t(wb_all_fit1_nostreet$beta), wb_all_fit1_nostreet$tau2)))
## #
## #Fraction in 1st window = 0.1
## #Fraction in 2nd window = 0.5 
## #
## #   var1    var2    var3    var4    var5    var6    var7 
## # 0.2159  0.5129 -2.2049  0.9636  1.1399  2.3834  0.1311 


###################################################
### code chunk number 66: bivand+gomez-rubio_SMij.Rnw:1418-1457 (eval = FALSE)
###################################################
## ## Models fit with INLA
## 
## # IMPORTANT!!! TIme = time / 60 (max. time = 58.42)
## 
## # Models for MAG, CAR and ALL cannot be fit without re-scaling time
## 
suppressMessages(wb_inla_car_mag <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "besag", graph = E_mag, hyper = list(prec = list(param = c(0.001, 0.001)))), data = mag, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_car_mag <- inla.rerun(wb_inla_car_mag)
saveRDS(wb_inla_car_mag, "wb_inla_car_mag.rds")
## 
## # Re-define index for 'beasg' effect
car$all_id2 <- car$all_id - min(car$all_id) + 1
suppressMessages(wb_inla_car_car <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id2, model = "besag", graph = E_car, hyper = list(prec = list(param = c(0.001, 0.001)))), data = car, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_car_car <- inla.rerun(wb_inla_car_car)
saveRDS(wb_inla_car_car, "wb_inla_car_car.rds")
## 
# Re-define index for 'beasg' effect
stc$all_id2 <- stc$all_id - min(stc$all_id) + 1
suppressMessages(wb_inla_car_stc <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id2, model = "besag", graph = E_stc, hyper = list(prec = list(param = c(0.001, 0.001)))), data = stc, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )), control.fixed = list(prec = 1e-10)))
wb_inla_car_stc <- inla.rerun(wb_inla_car_stc)
saveRDS(wb_inla_car_stc, "wb_inla_car_stc.rds")
## 
## 
## suppressMessages(wb_inla_car_all <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street + f(all_id, model = "besag", graph = E_all, hyper = list(prec = list(param = c(0.001, 0.001)))), data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE) , control.fixed = list(prec = 1e-10)
## ))
## wb_inla_car_all <- inla.rerun(wb_inla_car_all)
## # saveRDS(wb_inla_car_all, "wb_inla_car_all.rds")
## 
suppressMessages(wb_inla_car_all_nostreet <- inla(inla.surv(time = days_mod2 / 60, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(all_id, model = "besag", graph = E_all, hyper = list(prec = list(param = c(0.001, 0.001)))), data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE) , control.fixed = list(prec = 1e-10)
))
wb_inla_car_all_nostreet <- inla.rerun(wb_inla_car_all_nostreet)
saveRDS(wb_inla_car_all_nostreet, "wb_inla_car_all_nostreet.rds")
## 


###################################################
### code chunk number 67: bivand+gomez-rubio_SMij.Rnw:1462-1494 (eval = FALSE)
###################################################
wb_mag <- readRDS("wb_mag_fit.rds")
wb_car <- readRDS("wb_car_fit.rds")
wb_stc <- readRDS("wb_stc_fit.rds")
#wb_all_fit1 <- readRDS("wb_all_fit1.rds")
wb_all_fit1 <- readRDS("wb_all_fit1_nostreet.rds")
c1 <- summary(wb_mag)$coeff[, c(1, 3)]
c1[, 1] <- -1*c1[, 1]
c1 <- cbind(c1, CI95(wb_mag$beta))
c2 <- summary(wb_car)$coeff[, c(1, 3)]
c2[, 1] <- -1*c2[, 1]
c2 <- cbind(c2, CI95(wb_car$beta))
c3 <- summary(wb_stc)$coeff[, c(1, 3)]
c3[, 1] <- -1*c3[, 1]
c3 <- cbind(c3, CI95(wb_stc$beta))
c4 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
c4[, 1] <- -1*c4[, 1]
c4 <- cbind(c4, CI95(wb_all_fit1$beta[1:6, ]))
res <- rbind(c1, c(summary(wb_mag)$frailvar[1, c(1,3:5)]), c2, c(summary(wb_car)$frailvar[1, c(1,3:5)]), c3, c(summary(wb_stc)$frailvar[1, c(1,3:5)]), c4, c(summary(wb_all_fit1)$frailvar[1, c(1,3:5)]))
rownames(res)[c(7, 14, 21, 28)] <- "frailtyvar"
saveRDS(res, file="wb_car_res.rds")
## library(coda)
## rd_pld_car <- raftery.diag(as.mcmc(cbind(t(wb_all_fit1$beta), wb_all_fit1$tau2)))
## rd_stc_car <- raftery.diag(as.mcmc(cbind(t(wb_stc$beta), wb_stc$tau2)))
## rd_car_car <- raftery.diag(as.mcmc(cbind(t(wb_car$beta), wb_car$tau2)))
## rd_mag_car <- raftery.diag(as.mcmc(cbind(t(wb_mag$beta), wb_mag$tau2)))
## # save(rd_mag_car, rd_stc_car, rd_car_car, rd_pld_car, file="rd_car_mcmc.rda")
## gd_pld_car <- geweke.diag(as.mcmc(cbind(t(wb_all_fit1$beta), wb_all_fit1$tau2)))
## gd_stc_car <- geweke.diag(as.mcmc(cbind(t(wb_stc$beta), wb_stc$tau2)))
## gd_car_car <- geweke.diag(as.mcmc(cbind(t(wb_car$beta), wb_car$tau2)))
## gd_mag_car <- geweke.diag(as.mcmc(cbind(t(wb_mag$beta), wb_mag$tau2)))
## # save(gd_mag_car, gd_stc_car, gd_car_car, gd_pld_car, file="gd_car_mcmc.rda")
## 

set.seed(1)
cs_wb_car_fit <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_car_fit, "cs_wb_car_fit.rds")
set.seed(1)
cs_wb_stc_fit <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_stc_fit, "cs_wb_stc_fit.rds")
set.seed(1)
cs_wb_mag_fit <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_mag_fit, "cs_wb_mag_fit.rds")


###################################################
### code chunk number 68: bivand+gomez-rubio_SMij.Rnw:1498-1513 (eval = FALSE)
###################################################
res <- readRDS("wb_car_res.rds")
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "CAR frailty")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# Use 95% credible intervals
dres$ymax <- dres[, 4]
dres$ymin <- dres[, 3]
dres$Estimator <- "MCMC"
saveRDS(dres, file="dres_mcmc_car.rds")

###################################################
### code chunk number 90: bivand+gomez-rubio_SMij.Rnw:2133-2178 (eval = FALSE)
###################################################
## ### CAR models with INLA
wb_inla_mag <- readRDS("wb_inla_car_mag.rds")
wb_inla_car <- readRDS("wb_inla_car_car.rds")
wb_inla_stc <- readRDS("wb_inla_car_stc.rds")
## #wb_inla_all <- readRDS("wb_inla_car_all.rds")
wb_inla_all <- readRDS("wb_inla_car_all_nostreet.rds")
## 
## #wb_inla_mag <- inla.rerun(wb_inla_mag)
## #wb_inla_car <- inla.rerun(wb_inla_car)
## #wb_inla_stc <- inla.rerun(wb_inla_stc)
## #wb_inla_all <- inla.rerun(wb_inla_all)
## 
## # res <- readRDS("figure_mat_mcmc_plain.rds")
res <- rbind(
  rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_mag)),
  rbind(wb_inla_car$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_car)),
  rbind(wb_inla_stc$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_stc)),
  rbind(wb_inla_all$summary.fixed[2:7, c(1:3, 5)], variance = summary.var(wb_inla_all)) # Includes f_street
)
dres <- as.data.frame(res)
## 
dres$covariate <- factor(rep(rownames(res)[1:7], 4), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "variance"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "CAR frailty")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd

# Change signs
idx <- dres$covariate != "CAR frailty"
dres[idx, c(1, 3:4)] <- - dres[idx, c(1, 3:4)]



# Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]
dres$Estimator <- "INLA"
saveRDS(dres, "dres_inla_car.rds")
## 

###################################################
### code chunk number 69: bivand+gomez-rubio_SMij.Rnw:1523-1531
###################################################
pdf(file = "fig9.pdf")
dres_mcmc <- readRDS("dres_mcmc_car.rds")
dres_inla <- readRDS("dres_inla_car.rds")
names(dres_inla) <- names(dres_mcmc)
dres <- rbind(dres_mcmc, dres_inla)
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, colour=Data, shape=Estimator)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()

###################################################
### code chunk number 70: bivand+gomez-rubio_SMij.Rnw:1549-1612
###################################################
cs_wb_mag <- readRDS("cs_wb_mag_fit.rds")
cs_wb_car <- readRDS("cs_wb_car_fit.rds")
cs_wb_stc <- readRDS("cs_wb_stc_fit.rds")
wb_all_fit1_nostreet <- readRDS("wb_all_fit1_nostreet.rds")
set.seed(1)
cs_wb_all_nostreet  <- cox.snell.survregbayes(wb_all_fit1_nostreet, ncurves=100, PLOT=FALSE)
saveRDS(cs_wb_all_nostreet, file="cs_wb_all_car_nostreet.rds")
cs_wb_all <- readRDS("cs_wb_all_car_nostreet.rds")
pdf(file = "fig10.pdf")
opar <- par(mfrow=c(1,4), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Magazine Street")
fit <- survival::survfit(cs_wb_mag$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_mag[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Carrollton Avenue")
fit <- survival::survfit(cs_wb_car$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_car[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="St Claude Avenue")
fit <- survival::survfit(cs_wb_stc$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_stc[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
r.max <- 4
xlim <- c(0, r.max)
ylim <- c(0, r.max)
xx <- seq(0, r.max, 0.01)
plot(1, type="n", xlim = xlim, ylim = ylim, main="Pooled")
fit <- survival::survfit(cs_wb_all$resid1 ~ 1)
lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
ncurves <- 100
for (i in 2:ncurves) {
 fit <- survival::survfit(cs_wb_all[[i + 1]] ~ 1)
 lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
}
lines(xx, xx, lty = 1, lwd = 2)
par(opar)
dev.off()
rm(wb_all_fit1_nostreet, wb_inla_mag, wb_inla_car, wb_inla_stc, wb_inla_all, wb_all_fit1, wb_car, wb_mag, wb_stc)
gc()

###################################################
### code chunk number 71: bivand+gomez-rubio_SMij.Rnw:1624-1629 (eval = FALSE)
###################################################
## load("gd_car_mcmc.rda")
## mat <- cbind(c(gd_mag_car$z[1:6], NA, NA, gd_mag_car$z[7]), c(gd_stc_car$z[1:6], NA, NA, gd_stc_car$z[7]), c(gd_car_car$z[1:6], NA, NA, gd_car_car$z[7]), gd_pld_car$z)
## rownames(mat) <- c(gd_names, "Tau2")
## colnames(mat) <- c("Magazine St", "St Claude Ave", "Carrollton Ave", "Pooled")
## print(xtable(mat, align=c("l","r","r","r", "r"), digits=c(NA, 5, 5, 5, 5), display=c("s", "f", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,9))


###################################################
### code chunk number 72: bivand+gomez-rubio_SMij.Rnw:1638-1643 (eval = FALSE)
###################################################
## crds_mag <- st_coordinates(mag)
## crds_mag[which(duplicated(crds_mag)),] <- crds_mag[which(duplicated(crds_mag)),] + c(0.00005, 0.000005)
## set.seed(1)
## suppressMessages(wb_grf_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_mag, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_mag, "wb_grf_mag.rds")


###################################################
### code chunk number 73: bivand+gomez-rubio_SMij.Rnw:1648-1653 (eval = FALSE)
###################################################
## crds_car <- st_coordinates(car)
## crds_car[which(duplicated(crds_car)),] <- crds_car[which(duplicated(crds_car)),] + c(0.00005, 0.000005)
## set.seed(1)
## suppressMessages(wb_grf_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_car, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_car, "wb_grf_car.rds")


###################################################
### code chunk number 74: bivand+gomez-rubio_SMij.Rnw:1657-1661 (eval = FALSE)
###################################################
## crds_stc <- st_coordinates(stc)
## set.seed(1)
## suppressMessages(wb_grf_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_stc, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_stc, "wb_grf_stc.rds")


###################################################
### code chunk number 75: bivand+gomez-rubio_SMij.Rnw:1665-1670 (eval = FALSE)
###################################################
## crds <- st_coordinates(st_transform(sf_katrina_41, "EPSG:3452"))
## crds[which(duplicated(crds)),] <- crds[which(duplicated(crds)),] + c(0.5, 0.5)
## set.seed(1)
## suppressMessages(wb_grf_all <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds, DIST=fields::rdist))
## # saveRDS(wb_grf_all, "wb_grf_all.rds")


###################################################
### code chunk number 76: bivand+gomez-rubio_SMij.Rnw:1674-1693 (eval = FALSE)
###################################################
## wb_mag <- readRDS("wb_grf_mag.rds")
## wb_car <- readRDS("wb_grf_car.rds")
## wb_stc <- readRDS("wb_grf_stc.rds")
## #wb_all_fit1 <- readRDS("wb_grf_all.rds")
## c1 <- summary(wb_mag)$coeff[, c(1, 3)]
## c1[, 1] <- -1*c1[, 1]
## c1 <- cbind(c1, CI95(wb_mag$beta))
## c2 <- summary(wb_car)$coeff[, c(1, 3)]
## c2[, 1] <- -1*c2[, 1]
## c2 <- cbind(c2, CI95(wb_car$beta))
## c3 <- summary(wb_stc)$coeff[, c(1, 3)]
## c3[, 1] <- -1*c3[, 1]
## c3 <- cbind(c3, CI95(wb_stc$beta))
## #c4 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
## #c4[, 1] <- -1*c4[, 1]
## res <- rbind(c1, c(summary(wb_mag)$frailvar[1, c(1,3:5)]), c2, c(summary(wb_car)$frailvar[1, c(1,3:5)]), c3, c(summary(wb_stc)$frailvar[1, c(1,3:5)]))#, c4, c(summary(wb_all_fit1)$frailvar[1, c(1,3:5)]))
## rownames(res)[c(7, 14, 21)] <- "frailtyvar"
## #rownames(res)[c(7, 14, 21, 28)] <- "frailtyvar"
## # saveRDS(res, file="wb_grf_res.rds")


###################################################
### code chunk number 77: bivand+gomez-rubio_SMij.Rnw:1698-1735 (eval = FALSE)
###################################################
## cs_wb_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_car, "cs_wb_car.rds")
## cs_wb_car_fit <- cox.snell.survregbayes(wb_car_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_car_fit, "cs_wb_car_fit.rds")
## cs_wb_iid_car <- cox.snell.survregbayes(wb_iid_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_car, "cs_wb_iid_car.rds")
## cs_wb_grf_car <- cox.snell.survregbayes(wb_grf_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_car, "cs_wb_grf_car.rds")
## cs_wb_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_stc, "cs_wb_stc.rds")
## cs_wb_stc_fit <- cox.snell.survregbayes(wb_stc_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_stc_fit, "cs_wb_stc_fit.rds")
## cs_wb_iid_stc <- cox.snell.survregbayes(wb_iid_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_stc, "cs_wb_iid_stc.rds")
## cs_wb_grf_stc <- cox.snell.survregbayes(wb_grf_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_stc, "cs_wb_grf_stc.rds")
## cs_wb_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_mag, "cs_wb_mag.rds")
## cs_wb_mag_fit <- cox.snell.survregbayes(wb_mag_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_mag_fit, "cs_wb_mag_fit.rds")
## cs_wb_iid_mag <- cox.snell.survregbayes(wb_iid_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_mag, "cs_wb_iid_mag.rds")
##  cs_wb_grf_mag <- cox.snell.survregbayes(wb_grf_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_mag, "cs_wb_grf_mag.rds")
## models <- c("wb_mag", "wb_iid_mag", "wb_grf_mag", "wb_mag_fit", "wb_car", "wb_iid_car", "wb_grf_car", "wb_car_fit", "wb_stc", "wb_iid_stc", "wb_grf_stc", "wb_stc_fit")
## DICs <- numeric(length(models))
## for (i in seq(along=models)) DICs[i] <- get(models[i])$DIC
## names(DICs) <- models
## pDs <- numeric(length(models))
## for (i in seq(along=models)) pDs[i] <- get(models[i])$pD
## names(pDs) <- models
## cs_r.max <- numeric(length(models))
## for (i in seq(along=models)) cs_r.max[i] <- ceiling(quantile(get(models[i])$Surv.cox.snell[, 1], 0.99)) + 1
## names(cs_r.max) <- models
## cs_r.maxs <- apply(matrix(cs_r.max, nrow=3, byrow=TRUE), 1, max)
## names(cs_r.maxs) <- c("mag", "car", "stc")
## #save(cs_r.maxs, DICs, pDs, file="summary_stuff.RData")


###################################################
### code chunk number 78: bivand+gomez-rubio_SMij.Rnw:1745-1809 (eval = FALSE)
###################################################
## #mat <- cbind(matrix(DICs, ncol=3)[c(1,2,4,3),], c(alls_DIC, NA))
mat <- matrix(NA, nrow=3, ncol=4)
obj <- readRDS("wb_mag.rds")
mat[1, 1] <- obj$DIC
obj <- readRDS("wb_car.rds")
mat[1, 2] <- obj$DIC
obj <- readRDS("wb_stc.rds")
mat[1, 3] <- obj$DIC
obj <- readRDS("wb_all_nostreet_plain.rds")
mat[1, 4] <- obj$DIC
obj <- readRDS("wb_iid_mag.rds")
mat[2, 1] <- obj$DIC
obj <- readRDS("wb_iid_car.rds")
mat[2, 2] <- obj$DIC
obj <- readRDS("wb_iid_stc.rds")
mat[2, 3] <- obj$DIC
obj <- readRDS("wb_iid_fit1_nostreet.rds")
mat[2, 4] <- obj$DIC
obj <- readRDS("wb_mag_fit.rds")
mat[3, 1] <- obj$DIC
obj <- readRDS("wb_car_fit.rds")
mat[3, 2] <- obj$DIC
obj <- readRDS("wb_stc_fit.rds")
mat[3, 3] <- obj$DIC
obj <- readRDS("wb_all_fit1_nostreet.rds")
mat[3, 4] <- obj$DIC
colnames(mat) <- c(names(streets_41), "Pooled")
rownames(mat) <- paste("MCMC", c("No frailty", "IID frailty", "CAR frailty"))
saveRDS(mat, "mcmc_DICs.rds")
## 
mat1 <- matrix(NA, nrow=3, ncol=4)
obj <- readRDS("wb_inla_mag.rds")
mat1[1, 1] <- obj$dic$dic
obj <- readRDS("wb_inla_car.rds")
mat1[1, 2] <- obj$dic$dic
obj <- readRDS("wb_inla_stc.rds")
mat1[1, 3] <- obj$dic$dic
obj <- readRDS("wb_inla_all_nostreet.rds") # Pooled
mat1[1, 4] <- obj$dic$dic
obj <- readRDS("wb_inla_iid_mag.rds")
mat1[2, 1] <- obj$dic$dic
obj <- readRDS("wb_inla_iid_car.rds")
mat1[2, 2] <- obj$dic$dic
obj <- readRDS("wb_inla_iid_stc.rds")
mat1[2, 3] <- obj$dic$dic
obj <- readRDS("wb_inla_iid_all_nostreet.rds") # Pooled
mat1[2, 4] <- obj$dic$dic
obj <- readRDS("wb_inla_car_mag.rds")
mat1[3, 1] <- obj$dic$dic
obj <- readRDS("wb_inla_car_car.rds")
mat1[3, 2] <- obj$dic$dic
obj <- readRDS("wb_inla_car_stc.rds")
mat1[3, 3] <- obj$dic$dic
obj <- readRDS("wb_inla_car_all_nostreet.rds") # Pooled
mat1[3, 4] <- obj$dic$dic
## 
colnames(mat1) <- c(names(streets_41), "Pooled")
rownames(mat1) <- paste("INLA", c("No frailty", "IID frailty", "CAR frailty"))
saveRDS(mat1, "inla_DICs.rds")
## 
## # Remove GRF
## #mat <- mat[-4, ]
## 
## #print(xtable(mat, align=c("l","r","r","r", "r")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,4))


###################################################
### code chunk number 79: bivand+gomez-rubio_SMij.Rnw:1817-1821
###################################################
mat <- readRDS("mcmc_DICs.rds")
mat1 <- readRDS("inla_DICs.rds")
mat <- rbind(mat, mat1)
print(xtable(mat, align=c("l","r","r","r", "r")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3,6))


###################################################
### code chunk number 80: bivand+gomez-rubio_SMij.Rnw:1848-1849
###################################################
forma <- ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1


###################################################
### code chunk number 81: bivand+gomez-rubio_SMij.Rnw:1853-1865
###################################################
suppressPackageStartupMessages(library(spatialreg))
W <- as(nb2listw(nb, style="W"), "CsparseMatrix")
suppressPackageStartupMessages(library(ProbitSpatial))
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_360c <- SpatialProbitFit(update(forma, y0_360 ~ .), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
sp_res360b <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_mag <- SpatialProbitFit(update(forma, y0_360 ~ .), data=mag, W=as(E_mag/rowSums(E_mag), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_mag <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_car <- SpatialProbitFit(update(forma, y0_360 ~ .), data=car, W=as(E_car/rowSums(E_car), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_car <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_stc <- SpatialProbitFit(update(forma, y0_360 ~ .), data=stc, W=as(E_stc/rowSums(E_stc), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_stc <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]


###################################################
### code chunk number 82: bivand+gomez-rubio_SMij.Rnw:1908-1918
###################################################
sp_res_360 <- rbind(sp_res360a_mag, sp_res360a_car, sp_res360a_stc, sp_res360b)
sp_dres_360 <- as.data.frame(sp_res_360)
sp_dres_360$covariate <- factor(as.character(sp_dres_360$V1), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(sp_dres_360$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
sp_dres_360$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
names(sp_dres_360)[2:3] <- c("mean", "sd")
pdf(file = "fig11.pdf")
limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
ggplot(sp_dres_360, aes(y=mean, x=covariate, colour=Data)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()

###################################################
### code chunk number 83: bivand+gomez-rubio_SMij.Rnw:1956-1959
###################################################
mat <- prop.table(table(sf_katrina$f_sizeemp, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "large", "small")
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 84: bivand+gomez-rubio_SMij.Rnw:1971-1973
###################################################
mat <- prop.table(table(sf_katrina$f_owntype, sf_katrina$f_street), margin=2)*100
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 85: bivand+gomez-rubio_SMij.Rnw:1985-1988
###################################################
mat <- prop.table(table(sf_katrina$f_sesstatus, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "lower", "upper")
print(xtable(mat[c(2,1,3),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 86: bivand+gomez-rubio_SMij.Rnw:1995-2000
###################################################
pdf(file = "fig12.pdf")
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(flood ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="flood")
par(oopar)
dev.off()

###################################################
### code chunk number 87: bivand+gomez-rubio_SMij.Rnw:2007-2012
###################################################
pdf(file = "fig13.pdf")
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(elevation1 ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="elevation")
par(oopar)
dev.off()

###################################################
### code chunk number 88: bivand+gomez-rubio_SMij.Rnw:2037-2053
###################################################
#res <- rbind(summary(wb_modela_41)$table[-c(1,8),1:2], summary(wb_modelb_41)$table[-c(1,8:10),1:2], inla_fxd)
res <- rbind(summary(wb_modela_41_mag)$table[-c(1,8),1:2], summary(wb_modela_41_car)$table[-c(1,8:10),1:2],  summary(wb_modela_41_stc)$table[-c(1,8:10),1:2], summary(wb_modelb_41w)$table[-c(1,8:10),1:2])
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 6), rep("yes", 6), rep("iid", 6))
names(dres)[1:2] <- c("mean", "sd")
dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
dres$ymin <- dres$mean + qnorm(0.025)*dres$sd

library(ggplot2)
pdf(file = "fig14.pdf")
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, colour=Data)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
dev.off()

###################################################
### code chunk number 89: bivand+gomez-rubio_SMij.Rnw:2078-2125 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 3.5, pointsize = 10,
##  onefile = TRUE)
## ### IID models with INLA
## wb_inla_mag <- readRDS("wb_inla_iid_mag.rds")
## wb_inla_car <- readRDS("wb_inla_iid_car.rds")
## wb_inla_stc <- readRDS("wb_inla_iid_stc.rds")
## wb_inla_all <- readRDS("wb_inla_iid_all_nostreet.rds")
## 
## #Summary of variance
## #obj: INLA object
## summary.var <- function(obj) {
##   res <- unlist(
##     inla.zmarginal(inla.tmarginal(function(x) exp(-x),
##       obj$internal.marginals.hyperpar[[2]]), silent = TRUE)[c(1:3, 7)])
##   return(res)
## }
## 
## # res <- readRDS("figure_mat_mcmc_plain.rds")
## res <- rbind(
##   rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_mag)),
##   rbind(wb_inla_car$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_car)),
##   rbind(wb_inla_stc$summary.fixed[-1, c(1:3, 5)], variance= summary.var(wb_inla_stc)),
##   rbind(wb_inla_all$summary.fixed[2:7, c(1:3, 5)], variance = summary.var(wb_inla_all)) # Includes f_street
## )
## dres <- as.data.frame(res)
## 
## dres$covariate <- factor(rep(rownames(res)[1:7], 4), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "variance"))
## levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "IID frailty")
## dres$Data <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
## #dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
## names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
## #dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## #dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## 
## # Change signs
## idx <- dres$covariate != "IID frailty"
## dres[idx, c(1, 3:4)] <- - dres[idx, c(1, 3:4)]
## 
## 
## # Use 95% credible intervals
## dres$ymax <- dres[, 3]
## dres$ymin <- dres[, 4]
## dres$Estimator <- "INLA"
## saveRDS(dres, "wb_iid_inla_dres.rds")
## 
## limits <- aes(ymax = ymax, ymin=ymin)
## ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 90: bivand+gomez-rubio_SMij.Rnw:2133-2178 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 3.5, pointsize = 10,
##  onefile = TRUE)
## ### CAR models with INLA
## wb_inla_mag <- readRDS("wb_inla_car_mag.rds")
## wb_inla_car <- readRDS("wb_inla_car_car.rds")
## wb_inla_stc <- readRDS("wb_inla_car_stc.rds")
## #wb_inla_all <- readRDS("wb_inla_car_all.rds")
## wb_inla_all <- readRDS("wb_inla_car_all_nostreet.rds")
## 
## #wb_inla_mag <- inla.rerun(wb_inla_mag)
## #wb_inla_car <- inla.rerun(wb_inla_car)
## #wb_inla_stc <- inla.rerun(wb_inla_stc)
## #wb_inla_all <- inla.rerun(wb_inla_all)
## 
## # res <- readRDS("figure_mat_mcmc_plain.rds")
## res <- rbind(
##   rbind(wb_inla_mag$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_mag)),
##   rbind(wb_inla_car$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_car)),
##   rbind(wb_inla_stc$summary.fixed[-1, c(1:3, 5)], variance = summary.var(wb_inla_stc)),
##   rbind(wb_inla_all$summary.fixed[2:7, c(1:3, 5)], variance = summary.var(wb_inla_all)) # Includes f_street
## )
## dres <- as.data.frame(res)
## 
## dres$covariate <- factor(rep(rownames(res)[1:7], 4), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "variance"))
## levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "CAR frailty")
## dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
## #dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
## names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
## #dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## #dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## 
## # Change signs
## idx <- dres$covariate != "CAR frailty"
## dres[idx, c(1, 3:4)] <- - dres[idx, c(1, 3:4)]
## 
## 
## 
## # Use 95% credible intervals
## dres$ymax <- dres[, 3]
## dres$ymin <- dres[, 4]
## dres$Estimator <- "INLA"
## saveRDS(dres, "dres_inla_car.rds")
## 
## limits <- aes(ymax = ymax, ymin=ymin)
## ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 91: bivand+gomez-rubio_SMij.Rnw:2204-2221 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 3.5, pointsize = 10,
##  onefile = TRUE)
## res <- readRDS("wb_grf_res.rds")
## dres <- as.data.frame(res)
## dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
## levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "GRF frailty")
## #dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
## #dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
## dres$Street <- factor(rep(c(levels(sf_katrina$f_street)), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue"))
## names(dres)[1:2] <- c("mean", "sd")
## #dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## #dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## # Use 95% credible intervals
## dres$ymax <- dres[, 4]
## dres$ymin <- dres[, 3]
## limits <- aes(ymax = ymax, ymin=ymin)
## ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 92: bivand+gomez-rubio_SMij.Rnw:2230-2276 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 2.5, pointsize = 10,
##  onefile = TRUE)
## opar <- par(no.readonly = TRUE)
## par(mar=c(4,4,2,1)+0.1)
## cs_wb_mag <- readRDS("cs_wb_grf_mag.rds")
## cs_wb_car <- readRDS("cs_wb_grf_car.rds")
## cs_wb_stc <- readRDS("cs_wb_grf_stc.rds")
## opar <- par(mfrow=c(1,3), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
## r.max <- cs_r.maxs[1]
## xlim <- c(0, r.max)
## ylim <- c(0, r.max)
## xx <- seq(0, r.max, 0.01)
## plot(1, type="n", xlim = xlim, ylim = ylim, main="Magazine Street")
## fit <- survival::survfit(cs_wb_mag$resid1 ~ 1)
## lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## ncurves <- 100
## for (i in 2:ncurves) {
##  fit <- survival::survfit(cs_wb_mag[[i + 1]] ~ 1)
##  lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## }
## lines(xx, xx, lty = 1, lwd = 2)
## r.max <- cs_r.maxs[2]
## xlim <- c(0, r.max)
## ylim <- c(0, r.max)
## xx <- seq(0, r.max, 0.01)
## plot(1, type="n", xlim = xlim, ylim = ylim, main="Carrollton Avenue")
## fit <- survival::survfit(cs_wb_car$resid1 ~ 1)
## lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## ncurves <- 100
## for (i in 2:ncurves) {
##  fit <- survival::survfit(cs_wb_car[[i + 1]] ~ 1)
##  lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## }
## lines(xx, xx, lty = 1, lwd = 2)
## r.max <- cs_r.maxs[3]
## xlim <- c(0, r.max)
## ylim <- c(0, r.max)
## xx <- seq(0, r.max, 0.01)
## plot(1, type="n", xlim = xlim, ylim = ylim, main="St Claude Avenue")
## fit <- survival::survfit(cs_wb_stc$resid1 ~ 1)
## lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## ncurves <- 100
## for (i in 2:ncurves) {
##  fit <- survival::survfit(cs_wb_stc[[i + 1]] ~ 1)
##  lines(fit, fun = "cumhaz", conf.int = F, mark.time = FALSE, col="grey")
## }
## lines(xx, xx, lty = 1, lwd = 2)
## par(opar)
## par(opar)
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 93: bivand+gomez-rubio_SMij.Rnw:2284-2287 (eval = FALSE)
###################################################
## prob_fit0_360a_mag <- glm(update(forma, y0_360 ~ .), data=mag, family = binomial(link = "probit"), x=TRUE)
## prob_fit0_360a_car <- glm(update(forma, y0_360 ~ .), data=car, family = binomial(link = "probit"), x=TRUE)
## prob_fit0_360a_stc <- glm(update(forma, y0_360 ~ .), data=stc, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 94: bivand+gomez-rubio_SMij.Rnw:2291-2292 (eval = FALSE)
###################################################
## prob_fit0_360b <- glm(update(forma, y0_360 ~ . + f_street), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 95: bivand+gomez-rubio_SMij.Rnw:2296-2317 (eval = FALSE)
###################################################
## ## Probit models with INLA
## 
## suppressMessages(probit_inla_mag <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = mag, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
## suppressMessages(probit_inla_mag <- inla.rerun(probit_inla_mag))
## # saveRDS(probit_inla_mag, "probit_inla_mag.rds")
## 
## suppressMessages(probit_inla_car <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = car, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
## suppressMessages(probit_inla_car <- inla.rerun(probit_inla_car))
## # saveRDS(probit_inla_car, "probit_inla_car.rds")
## 
## 
## suppressMessages(probit_inla_stc <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = stc, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
## suppressMessages(probit_inla_stc <- inla.rerun(probit_inla_stc))
## # saveRDS(probit_inla_stc, "probit_inla_stc.rds")
## 
## suppressMessages(probit_inla_all <- inla(y0_360 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = sf_katrina_41, family = "binomial", control.compute = list(dic = TRUE), control.fixed = list(prec = 1e-10)))
## suppressMessages(probit_inla_all <- inla.rerun(probit_inla_all))
## # saveRDS(probit_inla_all, "probit_inla_all_nostreet.rds")
## 
## 
## 


###################################################
### code chunk number 96: bivand+gomez-rubio_SMij.Rnw:2322-2332 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 3.5, pointsize = 10,
##  onefile = TRUE)
## res_360 <- rbind(summary(prob_fit0_360a_mag)$coefficients[-1,1:2], summary(prob_fit0_360a_car)$coefficients[-1,1:2], summary(prob_fit0_360a_stc)$coefficients[-1,1:2], summary(prob_fit0_360b)$coefficients[-c(1, 8, 9),1:2])
## dres_360 <- as.data.frame(res_360)
## dres_360$covariate <- factor(rownames(res_360), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
## levels(dres_360$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
## dres_360$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
## names(dres_360)[1:2] <- c("mean", "sd")
## limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
## ggplot(dres_360, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 97: bivand+gomez-rubio_SMij.Rnw:2357-2383 (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1
## file <- paste("bivand+gomez-rubio-fig", .PngNo, ".eps", sep="")
## cairo_ps(file=file, width = 7, height = 3.5, pointsize = 10,
##  onefile = TRUE)
## wb_inla_fita_41 <- readRDS("wb_inla_fita_41.rds")
## inla_fxd <- summary(wb_inla_fita_41)$fixed[-1, 1:2]
## inla_fxd[,1] <- -1*inla_fxd[,1]
## #res <- rbind(summary(wb_modela_41)$table[-c(1,8),1:2], summary(wb_modelb_41)$table[-c(1,8:10),1:2], inla_fxd)
## res <- rbind(summary(wb_modela_41_mag)$table[-c(1,8),1:2], summary(wb_modela_41_car)$table[-c(1,8:10),1:2],  summary(wb_modela_41_stc)$table[-c(1,8:10),1:2], summary(wb_modelb_41w)$table[-c(1,8:10),1:2], inla_fxd)
## dres <- as.data.frame(res)
## dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
## levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
## dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled", "IID RE"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled", "IID RE"))
## #dres$street_dummy <- c(rep("no", 6), rep("yes", 6), rep("iid", 6))
## names(dres)[1:2] <- c("mean", "sd")
## dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
## dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
## 
## # FIX INLA estimates to use 95% credible intervals
## dres$ymax[dres$Street == "IID RE"] <-  -wb_inla_fita_41$summary.fixed[-1, 3]
## dres$ymin[dres$Street == "IID RE"] <- -wb_inla_fita_41$summary.fixed[-1, 5]
## 
## # Remove IID RE
## dres <- subset(dres, Street != "IID RE")
## 
## library(ggplot2)
## limits <- aes(ymax = ymax, ymin=ymin)
## ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")

sessionInfo()


sink(type = "message")
sink()


