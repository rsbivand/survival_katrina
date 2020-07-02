### R code from vignette source 'bivand+gomez-rubio_SMij.Rnw'

###################################################
### code chunk number 13: bivand+gomez-rubio_SMij.Rnw:239-251
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
### code chunk number 14: bivand+gomez-rubio_SMij.Rnw:262-265
###################################################
suppressMessages(library(xtable))
mat <- table(yrs, diff_days)
#print(xtable(mat, align=c("l","r","r","r","r","r","r","r"), digits=c(NA, 0, 0, 0, 0, 0, 0, 0), display=c("s", "d", "d", "d", "d", "d", "d", "d")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 15: bivand+gomez-rubio_SMij.Rnw:280-289
###################################################
sf_katrina$days_mod <- sf_katrina$days
sf_katrina$days[is.na(sf_katrina$days)] <- 99999
sf_katrina$y0_90 <- as.numeric(!sf_katrina$days < 90)
sf_katrina$y0_180 <- as.numeric(!sf_katrina$days < 180)
sf_katrina$y0_360 <- as.numeric(!sf_katrina$days < 360)
sf_katrina$sliced <- (sf_katrina$y0_90 == 0) + (sf_katrina$y0_180 == 0) + (sf_katrina$y0_360 == 0) + as.numeric(sf_katrina$days < 99999)
sf_katrina$f_sliced <- factor(sf_katrina$sliced, levels=4:0, labels=c("day 0-90",
    "day 90-180", "day 180-360", "day 360 +", "never"))
# table(sf_katrina$y0_90)


###################################################
### code chunk number 16: bivand+gomez-rubio_SMij.Rnw:292-298
###################################################
suppressPackageStartupMessages(library(car))
sf_katrina$f_owntype <- recode(sf_katrina$owntype, "1='sole'; 2='local'; 3='national'", as.factor=TRUE)
sf_katrina$f_sesstatus <- recode(sf_katrina$sesstatus, "c(1,2)='lower'; c(4,5)='upper'; else='av'", as.factor=TRUE)
sf_katrina$f_sizeemp <- recode(sf_katrina$sizeemp, "1='small'; 2='av'; 3='large'", as.factor=TRUE)
sf_katrina$f_street <- recode(sf_katrina$street, "1='Magazine Street'; 2='Carrollton Avenue'; 3='St. Claude Avenue'", as.factor=TRUE, levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue"))
sf_katrina$elevation1 <- sf_katrina$elevation - min(sf_katrina$elevation)


###################################################
### code chunk number 17: bivand+gomez-rubio_SMij.Rnw:310-314
###################################################
mat <- table(sf_katrina$f_sliced, sf_katrina$f_street)
mat <- rbind(mat, apply(mat, 2, sum))
rownames(mat)[6] <- "sum"
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 0, 0, 0), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,5,6))


###################################################
### code chunk number 18: bivand+gomez-rubio_SMij.Rnw:330-338
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
### code chunk number 19: bivand+gomez-rubio_SMij.Rnw:346-349
###################################################
mat <- prop.table(table(sf_katrina_41$status, sf_katrina_41$f_street), margin=2)*100
rownames(mat) <- c("still closed", "re-opened")
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 20: bivand+gomez-rubio_SMij.Rnw:359-366
###################################################
sf_katrina_41$f_owntype1 <- recode(sf_katrina_41$owntype, "1='sole'; c(2,3)='chain'", as.factor=TRUE)
sf_katrina_41$f_sesstatus1 <- recode(sf_katrina_41$sesstatus, "c(1,2)='lower'; c(3,4,5)='av'", as.factor=TRUE)
sf_katrina_41$f_sizeemp1 <- recode(sf_katrina_41$sizeemp, "1='small'; c(2,3)='av'", as.factor=TRUE)
sf_katrina_41$f_flood <- factor(as.integer(sf_katrina_41$flood < -4))
sf_katrina_41$flood1 <- abs(sf_katrina_41$flood)
sf_katrina_41$all_id <- 1:nrow(sf_katrina_41)
streets_41 <- split(sf_katrina_41, sf_katrina_41$f_street)


###################################################
### code chunk number 21: bivand+gomez-rubio_SMij.Rnw:464-467
###################################################
library(survival)
km_all <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = sf_katrina_41)
km_streets <- survfit(Surv(time = days_mod1, event = status) ~ f_street, data = sf_katrina_41)


###################################################
### code chunk number 22: bivand+gomez-rubio_SMij.Rnw:471-473
###################################################
km_allw <- survfit(Surv(time = days_mod2, event = status) ~ 1, data = sf_katrina_41)
km_streetsw <- survfit(Surv(time = days_mod2, event = status) ~ f_street, data = sf_katrina_41)


###################################################
### code chunk number 23: bivand+gomez-rubio_SMij.Rnw:477-497
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
#pdf(file = "fig_data.pdf", width = 6, height = 4)
obs.time1 <- obs.time[-nrow(obs.time),]

ggplot(obs.time1) + # Remove censored shops
  geom_line(aes(x = Var1, y = Freq)) + 
  geom_vline(xintercept=slices_d, lty=3, lwd=1) + scale_x_continuous(breaks=seq(0, 400/7, 90/7), labels=as.character(lbs)) +
  xlab("Date") + ylab("Number of businesses that re-opened")
#dev.off()


###################################################
### code chunk number 24: bivand+gomez-rubio_SMij.Rnw:502-510
###################################################
#slices_d <- (c(41, 90, 180, 360)-41)/7
#library(zoo)
#lbs <- as.yearmon(seq(as.Date("2005-08-29")+41, as.Date("2006-08-29")+41, 90))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
autoplot(km_streetsw) + geom_vline(xintercept=slices_d, lty=3, lwd=1) + scale_x_continuous(breaks=seq(0, 400/7, 90/7), labels=as.character(lbs))


###################################################
### code chunk number 25: bivand+gomez-rubio_SMij.Rnw:518-520
###################################################
wb_modelb_41w <- survreg(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1 + f_street, data = sf_katrina_41, dist="weibull")


###################################################
### code chunk number 26: bivand+gomez-rubio_SMij.Rnw:523-532
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
### code chunk number 27: bivand+gomez-rubio_SMij.Rnw:535-536
###################################################
suppressPackageStartupMessages(library(INLA))


###################################################
### code chunk number 28: bivand+gomez-rubio_SMij.Rnw:539-546
###################################################
sf_katrina_41$st_id <- as.numeric(sf_katrina_41$f_street)
all_surv_41 <- Surv(time = sf_katrina_41$days_mod2, event = sf_katrina_41$status)
all_surv_inla_41 <- inla.surv(all_surv_41[,1], all_surv_41[,2])
wb_inla_fita_41 <- inla(all_surv_inla_41 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(st_id, model="iid"), data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2) )))
# saveRDS(wb_inla_fita_41, file="wb_inla_fita_41.rds")
#wb_inla_fitb_41 <- inla(all_surv_inla_41 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = sf_katrina_41, family = "weibullsurv", control.compute = list(dic = TRUE), control.family = list(hyper = list(alpha = list(initial = -2))))
# saveRDS(wb_inla_fitb_41, file="wb_inla_fitb_41.rds")


###################################################
### code chunk number 29: bivand+gomez-rubio_SMij.Rnw:557-580
###################################################
#wb_inla_fita_41 <- readRDS("wb_inla_fita_41.rds")
inla_fxd <- summary(wb_inla_fita_41)$fixed[-1, 1:2]
inla_fxd[,1] <- -1*inla_fxd[,1]
#res <- rbind(summary(wb_modela_41)$table[-c(1,8),1:2], summary(wb_modelb_41)$table[-c(1,8:10),1:2], inla_fxd)
res <- rbind(summary(wb_modela_41_mag)$table[-c(1,8),1:2], summary(wb_modela_41_car)$table[-c(1,8:10),1:2],  summary(wb_modela_41_stc)$table[-c(1,8:10),1:2], summary(wb_modelb_41w)$table[-c(1,8:10),1:2], inla_fxd)
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled", "IID RE"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled", "IID RE"))
#dres$street_dummy <- c(rep("no", 6), rep("yes", 6), rep("iid", 6))
names(dres)[1:2] <- c("mean", "sd")
dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
dres$ymin <- dres$mean + qnorm(0.025)*dres$sd

# FIX INLA estimates to use 95% credible intervals
dres$ymax[dres$Street == "IID RE"] <-  -wb_inla_fita_41$summary.fixed[-1, 3]
dres$ymin[dres$Street == "IID RE"] <- -wb_inla_fita_41$summary.fixed[-1, 5]

library(ggplot2)
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 30: bivand+gomez-rubio_SMij.Rnw:615-621
###################################################
mat <- -1*wb_inla_fita_41$summary.random$st_id[, 4:6]
cn <- colnames(mat)
mat <- mat[, 3:1]
colnames(mat) <- cn
rownames(mat) <- names(streets_41)
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 5, 5, 5), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 31: bivand+gomez-rubio_SMij.Rnw:631-634
###################################################
suppressPackageStartupMessages(library(spBayesSurv))
mcmc <- list(nburn = 50000, nsave = 150000, nskip = 5, ndisplay= 20000)
prior <- list(maxL = 15)


###################################################
### code chunk number 32: bivand+gomez-rubio_SMij.Rnw:639-642 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
# saveRDS(wb_mag, "wb_mag.rds")


###################################################
### code chunk number 33: bivand+gomez-rubio_SMij.Rnw:646-649 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_car, "wb_car.rds")


###################################################
### code chunk number 34: bivand+gomez-rubio_SMij.Rnw:653-656 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1, data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_stc, "wb_stc.rds")


###################################################
### code chunk number 35: bivand+gomez-rubio_SMij.Rnw:659-662 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street, data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_all, "wb_all.rds")


###################################################
### code chunk number 36: bivand+gomez-rubio_SMij.Rnw:667-704 (eval = FALSE)
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
## 
## # Load results and create table for plotting
## wb_mag <- readRDS("wb_mag.rds")
## wb_car <- readRDS("wb_car.rds")
## wb_stc <- readRDS("wb_stc.rds")
## wb_all_fit1 <- readRDS("wb_all.rds")
wb_all_fit1 <- wb_all
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
res <- rbind(c1, c2, c3, c4)
## # saveRDS(res, "figure_mat_mcmc_plain.rds")


###################################################
### code chunk number 37: bivand+gomez-rubio_SMij.Rnw:708-724
###################################################
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd") #, "q0.025", "q0.975")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# Use 95% credible intervals
dres$ymax <- dres[, 3]
dres$ymin <- dres[, 4]
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 38: bivand+gomez-rubio_SMij.Rnw:737-799
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
set.seed(1)
cs_wb_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
#cs_wb_mag <- readRDS("cs_wb_mag.rds")
set.seed(1)
cs_wb_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
#cs_wb_car <- readRDS("cs_wb_car.rds")
set.seed(1)
cs_wb_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
#cs_wb_stc <- readRDS("cs_wb_stc.rds")
set.seed(1)
cs_wb_all <- cox.snell.survregbayes(wb_all_fit1, ncurves=100, PLOT=FALSE)
#cs_wb_all <- readRDS("cs_wb_all.rds")
# read summary files aggregated in earlier run providing among other things 
# fixed sizes for Cox-Snell plots
load(file="summary_stuff.RData")
load(file="alls_summaries.RData")
oopar <- par(mfrow=c(1,4), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- cs_r.maxs[1]
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
r.max <- cs_r.maxs[2]
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
r.max <- cs_r.maxs[3]
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
r.max <- cs_r.max[1]
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
par(oopar)
par(opar)


###################################################
### code chunk number 39: bivand+gomez-rubio_SMij.Rnw:826-827
###################################################
forma <- ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1


###################################################
### code chunk number 40: bivand+gomez-rubio_SMij.Rnw:830-833
###################################################
prob_fit0_360a_mag <- glm(update(forma, y0_360 ~ .), data=mag, family = binomial(link = "probit"), x=TRUE)
prob_fit0_360a_car <- glm(update(forma, y0_360 ~ .), data=car, family = binomial(link = "probit"), x=TRUE)
prob_fit0_360a_stc <- glm(update(forma, y0_360 ~ .), data=stc, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 41: bivand+gomez-rubio_SMij.Rnw:836-837
###################################################
prob_fit0_360b <- glm(update(forma, y0_360 ~ . + f_street), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 42: bivand+gomez-rubio_SMij.Rnw:841-851
###################################################
res_360 <- rbind(summary(prob_fit0_360a_mag)$coefficients[-1,1:2], summary(prob_fit0_360a_car)$coefficients[-1,1:2], summary(prob_fit0_360a_stc)$coefficients[-1,1:2], summary(prob_fit0_360b)$coefficients[-c(1, 8, 9),1:2])
dres_360 <- as.data.frame(res_360)
dres_360$covariate <- factor(rownames(res_360), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(dres_360$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
dres_360$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
names(dres_360)[1:2] <- c("mean", "sd")
limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
ggplot(dres_360, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 43: bivand+gomez-rubio_SMij.Rnw:873-876 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_fit1 <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  f_street + frailtyprior("iid", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_iid_fit1, "wb_iid_fit1.rds")


###################################################
### code chunk number 44: bivand+gomez-rubio_SMij.Rnw:880-883 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_iid_mag, "wb_iid_mag.rds")


###################################################
### code chunk number 45: bivand+gomez-rubio_SMij.Rnw:887-890 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_iid_car, "wb_iid_car.rds")


###################################################
### code chunk number 46: bivand+gomez-rubio_SMij.Rnw:894-897 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
## # saveRDS(wb_iid_stc, "wb_iid_stc.rds")


###################################################
### code chunk number 47: bivand+gomez-rubio_SMij.Rnw:901-920 (eval = FALSE)
###################################################
## wb_mag <- readRDS("wb_iid_mag.rds")
## wb_car <- readRDS("wb_iid_car.rds")
## wb_stc <- readRDS("wb_iid_stc.rds")
## wb_all_fit1 <- readRDS("wb_iid_fit1.rds")
wb_mag <- wb_iid_mag
wb_car <- wb_iid_car
wb_stc <- wb_iid_stc
wb_all_fit1 <- wb_iid_fit1
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
# saveRDS(res, file="wb_iid_res.rds")


###################################################
### code chunk number 48: bivand+gomez-rubio_SMij.Rnw:924-941
###################################################
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "IID frailty")
dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# USe 95% credible intervals
dres$ymax <- dres[, 4]
dres$ymin <- dres[, 3]

limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 49: bivand+gomez-rubio_SMij.Rnw:949-1009
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
set.seed(1)
cs_wb_mag <- cox.snell.survregbayes(wb_iid_mag, ncurves=100, PLOT=FALSE)
#cs_wb_mag <- readRDS("cs_wb_iid_mag.rds")
set.seed(1)
cs_wb_car <- cox.snell.survregbayes(wb_iid_car, ncurves=100, PLOT=FALSE)
#cs_wb_car <- readRDS("cs_wb_iid_car.rds")
set.seed(1)
cs_wb_stc <- cox.snell.survregbayes(wb_iid_stc, ncurves=100, PLOT=FALSE)
#cs_wb_stc <- readRDS("cs_wb_iid_stc.rds")
set.seed(1)
cs_wb_all <- cox.snell.survregbayes(wb_iid_fit1, ncurves=100, PLOT=FALSE)
#cs_wb_all <- readRDS("cs_wb_all_iid.rds")
oopar <- par(mfrow=c(1,4), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- cs_r.maxs[1]
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
r.max <- cs_r.maxs[2]
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
r.max <- cs_r.maxs[3]
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
r.max <- cs_r.max[2]
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
par(oopar)
par(opar)


###################################################
### code chunk number 50: bivand+gomez-rubio_SMij.Rnw:1062-1072
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
### code chunk number 51: bivand+gomez-rubio_SMij.Rnw:1076-1079 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_mag_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_mag))
## # saveRDS(wb_mag_fit, "wb_mag_fit.rds")


###################################################
### code chunk number 52: bivand+gomez-rubio_SMij.Rnw:1083-1086 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_car_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_car))
## # saveRDS(wb_car_fit, "wb_car_fit.rds")


###################################################
### code chunk number 53: bivand+gomez-rubio_SMij.Rnw:1089-1092 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_stc_fit <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_stc))
## # saveRDS(wb_stc_fit, "wb_stc_fit.rds")


###################################################
### code chunk number 54: bivand+gomez-rubio_SMij.Rnw:1095-1098 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all_fit1 <- survregbayes(Surv(time = days_mod2, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street + frailtyprior("car", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_all))
## # saveRDS(wb_all_fit1, "wb_all_fit1.rds")


###################################################
### code chunk number 55: bivand+gomez-rubio_SMij.Rnw:1102-1121 (eval = FALSE)
###################################################
## wb_mag <- readRDS("wb_mag_fit.rds")
## wb_car <- readRDS("wb_car_fit.rds")
## wb_stc <- readRDS("wb_stc_fit.rds")
## wb_all_fit1 <- readRDS("wb_all_fit1.rds")
wb_mag <- wb_mag_fit
wb_car <- wb_car_fit
wb_stc <- wb_stc_fit
wb_all_fit1 <- wb_all_fit1
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
## # saveRDS(res, file="wb_car_res.rds")


###################################################
### code chunk number 56: bivand+gomez-rubio_SMij.Rnw:1125-1141
###################################################
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "CAR frailty")
dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# Use 95% credible intervals
dres$ymax <- dres[, 4]
dres$ymin <- dres[, 3]
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 57: bivand+gomez-rubio_SMij.Rnw:1149-1209
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
set.seed(1)
cs_wb_mag <- cox.snell.survregbayes(wb_mag_fit, ncurves=100, PLOT=FALSE)
#cs_wb_mag <- readRDS("cs_wb_mag_fit.rds")
set.seed(1)
cs_wb_car <- cox.snell.survregbayes(wb_car_fit, ncurves=100, PLOT=FALSE)
#cs_wb_car <- readRDS("cs_wb_car_fit.rds")
set.seed(1)
cs_wb_stc <- cox.snell.survregbayes(wb_stc_fit, ncurves=100, PLOT=FALSE)
#cs_wb_stc <- readRDS("cs_wb_stc_fit.rds")
set.seed(1)
cs_wb_all <- cox.snell.survregbayes(wb_all_fit1, ncurves=100, PLOT=FALSE)
#cs_wb_all <- readRDS("cs_wb_all_car.rds")
oopar <- par(mfrow=c(1,4), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- cs_r.maxs[1]
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
r.max <- cs_r.maxs[2]
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
r.max <- cs_r.maxs[3]
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
r.max <- cs_r.max[3]
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
par(oopar)
par(opar)


###################################################
### code chunk number 58: bivand+gomez-rubio_SMij.Rnw:1217-1222 (eval = FALSE)
###################################################
crds_mag <- st_coordinates(mag)
crds_mag[which(duplicated(crds_mag)),] <- crds_mag[which(duplicated(crds_mag)),] + c(0.00005, 0.000005)
set.seed(1)
suppressMessages(wb_grf_mag <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = mag, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_mag, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_mag, "wb_grf_mag.rds")


###################################################
### code chunk number 59: bivand+gomez-rubio_SMij.Rnw:1227-1232 (eval = FALSE)
###################################################
crds_car <- st_coordinates(car)
crds_car[which(duplicated(crds_car)),] <- crds_car[which(duplicated(crds_car)),] + c(0.00005, 0.000005)
set.seed(1)
suppressMessages(wb_grf_car <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = car, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_car, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_car, "wb_grf_car.rds")


###################################################
### code chunk number 60: bivand+gomez-rubio_SMij.Rnw:1236-1240 (eval = FALSE)
###################################################
crds_stc <- st_coordinates(stc)
set.seed(1)
suppressMessages(wb_grf_stc <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = stc, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds_stc, DIST=fields::rdist.earth))
## # saveRDS(wb_grf_stc, "wb_grf_stc.rds")


###################################################
### code chunk number 61: bivand+gomez-rubio_SMij.Rnw:1244-1249 (eval = FALSE)
###################################################
#crds <- st_coordinates(st_transform(sf_katrina_41, "EPSG:3452"))
#crds[which(duplicated(crds)),] <- crds[which(duplicated(crds)),] + c(0.5, 0.5)
#set.seed(1)
#suppressMessages(wb_grf_all <- survregbayes(Surv(time = days_mod2, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("grf", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Coordinates=crds, DIST=fields::rdist))
## # saveRDS(wb_grf_all, "wb_grf_all.rds")


###################################################
### code chunk number 62: bivand+gomez-rubio_SMij.Rnw:1253-1272 (eval = FALSE)
###################################################
## wb_mag <- readRDS("wb_grf_mag.rds")
## wb_car <- readRDS("wb_grf_car.rds")
## wb_stc <- readRDS("wb_grf_stc.rds")
## #wb_all_fit1 <- readRDS("wb_grf_all.rds")
wb_mag <- wb_grf_mag
wb_car <- wb_grf_car
wb_stc <- wb_grf_stc
#wb_all_fit1 <- readRDS("wb_grf_all.rds")
c1 <- summary(wb_mag)$coeff[, c(1, 3)]
c1[, 1] <- -1*c1[, 1]
c1 <- cbind(c1, CI95(wb_mag$beta))
c2 <- summary(wb_car)$coeff[, c(1, 3)]
c2[, 1] <- -1*c2[, 1]
c2 <- cbind(c2, CI95(wb_car$beta))
c3 <- summary(wb_stc)$coeff[, c(1, 3)]
c3[, 1] <- -1*c3[, 1]
c3 <- cbind(c3, CI95(wb_stc$beta))
## #c4 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
## #c4[, 1] <- -1*c4[, 1]
res <- rbind(c1, c(summary(wb_mag)$frailvar[1, c(1,3:5)]), c2, c(summary(wb_car)$frailvar[1, c(1,3:5)]), c3, c(summary(wb_stc)$frailvar[1, c(1,3:5)]))#, c4, c(summary(wb_all_fit1)$frailvar[1, c(1,3:5)]))
rownames(res)[c(7, 14, 21)] <- "frailtyvar"
## #rownames(res)[c(7, 14, 21, 28)] <- "frailtyvar"
## # saveRDS(res, file="wb_grf_res.rds")


###################################################
### code chunk number 63: bivand+gomez-rubio_SMij.Rnw:1276-1293
###################################################
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
levels(dres$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm", "GRF frailty")
#dres$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
#dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
dres$Street <- factor(rep(c(levels(sf_katrina$f_street)), each=7), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue"))
names(dres)[1:2] <- c("mean", "sd")
#dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
#dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
# Use 95% credible intervals
dres$ymax <- dres[, 4]
dres$ymin <- dres[, 3]
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 64: bivand+gomez-rubio_SMij.Rnw:1302-1339 (eval = FALSE)
###################################################
#cs_wb_car <- cox.snell.survregbayes(wb_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_car, "cs_wb_car.rds")
#cs_wb_car_fit <- cox.snell.survregbayes(wb_car_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_car_fit, "cs_wb_car_fit.rds")
#cs_wb_iid_car <- cox.snell.survregbayes(wb_iid_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_car, "cs_wb_iid_car.rds")
set.seed(1)
cs_wb_grf_car <- cox.snell.survregbayes(wb_grf_car, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_car, "cs_wb_grf_car.rds")
#cs_wb_stc <- cox.snell.survregbayes(wb_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_stc, "cs_wb_stc.rds")
#cs_wb_stc_fit <- cox.snell.survregbayes(wb_stc_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_stc_fit, "cs_wb_stc_fit.rds")
#cs_wb_iid_stc <- cox.snell.survregbayes(wb_iid_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_stc, "cs_wb_iid_stc.rds")
set.seed(1)
cs_wb_grf_stc <- cox.snell.survregbayes(wb_grf_stc, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_stc, "cs_wb_grf_stc.rds")
#cs_wb_mag <- cox.snell.survregbayes(wb_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_mag, "cs_wb_mag.rds")
#cs_wb_mag_fit <- cox.snell.survregbayes(wb_mag_fit, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_mag_fit, "cs_wb_mag_fit.rds")
#cs_wb_iid_mag <- cox.snell.survregbayes(wb_iid_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_iid_mag, "cs_wb_iid_mag.rds")
set.seed(1)
cs_wb_grf_mag <- cox.snell.survregbayes(wb_grf_mag, ncurves=100, PLOT=FALSE)
## #saveRDS(cs_wb_grf_mag, "cs_wb_grf_mag.rds")
#models <- c("wb_mag", "wb_iid_mag", "wb_grf_mag", "wb_mag_fit", "wb_car", "wb_iid_car", "wb_grf_car", "wb_car_fit", "wb_stc", "wb_iid_stc", "wb_grf_stc", "wb_stc_fit")
#DICs <- numeric(length(models))
#for (i in seq(along=models)) DICs[i] <- get(models[i])$DIC
#names(DICs) <- models
#pDs <- numeric(length(models))
#for (i in seq(along=models)) pDs[i] <- get(models[i])$pD
#names(pDs) <- models
#cs_r.max <- numeric(length(models))
#for (i in seq(along=models)) cs_r.max[i] <- ceiling(quantile(get(models[i])$Surv.cox.snell[, 1], 0.99)) + 1
#names(cs_r.max) <- models
#cs_r.maxs <- apply(matrix(cs_r.max, nrow=3, byrow=TRUE), 1, max)
#names(cs_r.maxs) <- c("mag", "car", "stc")
## #save(cs_r.maxs, DICs, pDs, file="summary_stuff.RData")


###################################################
### code chunk number 65: bivand+gomez-rubio_SMij.Rnw:1344-1390
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
cs_wb_mag <- cs_wb_grf_mag
cs_wb_car <- cs_wb_grf_car
cs_wb_stc <- cs_wb_grf_stc
oopar <- par(mfrow=c(1,3), cex = 1, mar = c(2.1, 2.1, 2.1, 1), cex.lab = 1, cex.axis = 1)
r.max <- cs_r.maxs[1]
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
r.max <- cs_r.maxs[2]
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
r.max <- cs_r.maxs[3]
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
par(oopar)
par(opar)


###################################################
### code chunk number 66: bivand+gomez-rubio_SMij.Rnw:1418-1422
###################################################
mat <- cbind(matrix(DICs, ncol=3)[c(1,2,4,3),], c(alls_DIC, NA))
colnames(mat) <- c(names(streets_41), "Pooled")
rownames(mat) <- c("None", "IID", "CAR", "GRF")
print(xtable(mat, align=c("l","r","r","r", "r")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,4))


###################################################
### code chunk number 67: bivand+gomez-rubio_SMij.Rnw:1436-1448
###################################################
suppressPackageStartupMessages(library(spatialreg))
W <- as(nb2listw(nb, style="W"), "CsparseMatrix")
suppressPackageStartupMessages(library(ProbitSpatial))
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_360c <- SpatialProbitFit(update(forma, y0_360 ~ . + f_street), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
sp_res360b <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_mag <- SpatialProbitFit(update(forma, y0_360 ~ .), data=mag, W=as(E_mag/rowSums(E_mag), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_mag <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_car <- SpatialProbitFit(update(forma, y0_360 ~ .), data=car, W=as(E_car/rowSums(E_car), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_car <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
suppressWarnings(capture.output(SemProb_fit0_360a_stc <- SpatialProbitFit(update(forma, y0_360 ~ .), data=stc, W=as(E_stc/rowSums(E_stc), "CsparseMatrix"), DGP="SEM", method="full-lik", varcov="varcov"), file=tf))
sp_res360a_stc <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]


###################################################
### code chunk number 68: bivand+gomez-rubio_SMij.Rnw:1452-1462
###################################################
sp_res_360 <- rbind(sp_res360a_mag, sp_res360a_car, sp_res360a_stc, sp_res360b)
sp_dres_360 <- as.data.frame(sp_res_360)
sp_dres_360$covariate <- factor(as.character(sp_dres_360$V1), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
levels(sp_dres_360$covariate) <- c("Elevation", "Squared elevation", "Sole owner", "Lower SE status", "Log med. income", "Small firm")
sp_dres_360$Street <- factor(rep(c(levels(sf_katrina$f_street), "Pooled"), each=6), levels=c("Magazine Street", "Carrollton Avenue", "St. Claude Avenue", "Pooled"))
names(sp_dres_360)[2:3] <- c("mean", "sd")
limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
ggplot(sp_dres_360, aes(y=mean, x=covariate, shape=Street)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 69: bivand+gomez-rubio_SMij.Rnw:1530-1533
###################################################
mat <- prop.table(table(sf_katrina$f_sizeemp, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "large", "small")
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 70: bivand+gomez-rubio_SMij.Rnw:1545-1547
###################################################
mat <- prop.table(table(sf_katrina$f_owntype, sf_katrina$f_street), margin=2)*100
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 71: bivand+gomez-rubio_SMij.Rnw:1559-1562
###################################################
mat <- prop.table(table(sf_katrina$f_sesstatus, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "lower", "upper")
print(xtable(mat[c(2,1,3),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 72: bivand+gomez-rubio_SMij.Rnw:1569-1574
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(flood ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="flood")
par(oopar)
par(opar)


###################################################
### code chunk number 73: bivand+gomez-rubio_SMij.Rnw:1581-1586
###################################################
opar <- par(no.readonly = TRUE)
par(mar=c(4,4,2,1)+0.1)
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(elevation1 ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="elevation")
par(oopar)
par(opar)


