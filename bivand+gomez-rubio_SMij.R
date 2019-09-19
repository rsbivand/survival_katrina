### R code from vignette source 'bivand+gomez-rubio_SMij.Rnw'

###################################################
### code chunk number 12: bivand+gomez-rubio_SMij.Rnw:224-236
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


###################################################
### code chunk number 13: bivand+gomez-rubio_SMij.Rnw:245-248
###################################################
suppressMessages(library(xtable))
mat <- table(yrs, diff_days)
print(xtable(mat, align=c("l","r","r","r","r","r","r","r"), digits=c(NA, 0, 0, 0, 0, 0, 0, 0), display=c("s", "d", "d", "d", "d", "d", "d", "d")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 14: bivand+gomez-rubio_SMij.Rnw:256-265
###################################################
sf_katrina$days_mod <- sf_katrina$days
sf_katrina$days[is.na(sf_katrina$days)] <- 99999
sf_katrina$y0_90 <- as.numeric(!sf_katrina$days < 90)
sf_katrina$y0_180 <- as.numeric(!sf_katrina$days < 180)
sf_katrina$y0_360 <- as.numeric(!sf_katrina$days < 360)
sf_katrina$sliced <- (sf_katrina$y0_90 == 0) + (sf_katrina$y0_180 == 0) + (sf_katrina$y0_360 == 0) + as.numeric(sf_katrina$days < 99999)
sf_katrina$f_sliced <- factor(sf_katrina$sliced, levels=4:0, labels=c("day 0-90",
    "day 90-180", "day 180-360", "day 360 +", "never"))


###################################################
### code chunk number 15: bivand+gomez-rubio_SMij.Rnw:268-274
###################################################
suppressPackageStartupMessages(library(car))
sf_katrina$f_owntype <- recode(sf_katrina$owntype, "1='sole'; 2='local'; 3='national'", as.factor=TRUE)
sf_katrina$f_sesstatus <- recode(sf_katrina$sesstatus, "c(1,2)='lower'; c(4,5)='upper'; else='av'", as.factor=TRUE)
sf_katrina$f_sizeemp <- recode(sf_katrina$sizeemp, "1='small'; 2='av'; 3='large'", as.factor=TRUE)
sf_katrina$f_street <- recode(sf_katrina$street, "1='Magazine Street'; 2='Carrollton Avenue'; 3='St. Claude Avenue'", as.factor=TRUE)
sf_katrina$elevation1 <- sf_katrina$elevation - min(sf_katrina$elevation)


###################################################
### code chunk number 16: bivand+gomez-rubio_SMij.Rnw:284-288
###################################################
mat <- table(sf_katrina$f_sliced, sf_katrina$f_street)
mat <- rbind(mat, apply(mat, 2, sum))
rownames(mat)[6] <- "sum"
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 0, 0, 0), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,5,6))


###################################################
### code chunk number 17: bivand+gomez-rubio_SMij.Rnw:309-312
###################################################
mat <- prop.table(table(sf_katrina$f_sizeemp, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "large", "small")
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 18: bivand+gomez-rubio_SMij.Rnw:324-326
###################################################
mat <- prop.table(table(sf_katrina$f_owntype, sf_katrina$f_street), margin=2)*100
print(xtable(mat[c(3,1,2),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 19: bivand+gomez-rubio_SMij.Rnw:338-341
###################################################
mat <- prop.table(table(sf_katrina$f_sesstatus, sf_katrina$f_street), margin=2)*100
rownames(mat) <- c("average", "lower", "upper")
print(xtable(mat[c(2,1,3),], align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 20: bivand+gomez-rubio_SMij.Rnw:348-353
###################################################
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(flood ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="flood")
par(oopar)


###################################################
### code chunk number 21: bivand+gomez-rubio_SMij.Rnw:360-365
###################################################
oopar <- par(las=1, mar=c(4, 10, 4, 2))
boxplot(elevation1 ~ f_street, sf_katrina, varwidth=TRUE, horizontal=TRUE, ylab="", xlab="elevation")
par(oopar)


###################################################
### code chunk number 22: bivand+gomez-rubio_SMij.Rnw:382-388
###################################################
sf_katrina$days_mod[is.na(sf_katrina$days_mod)] <- 450
sf_katrina_41 <- sf_katrina[sf_katrina$days_mod > 41,]
sf_katrina_41$days_mod <- sf_katrina_41$days_mod - 41
sf_katrina_41$status <- !(sf_katrina_41$days_mod == 409)
sf_katrina_41$days_mod1 <- sf_katrina_41$days_mod/30 # approximate months

###################################################
### code chunk number 23: bivand+gomez-rubio_SMij.Rnw:397-400
###################################################
mat <- prop.table(table(sf_katrina_41$status, sf_katrina_41$f_street), margin=2)*100
rownames(mat) <- c("still closed", "re-opened")
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 2, 2, 2), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,2))


###################################################
### code chunk number 24: bivand+gomez-rubio_SMij.Rnw:408-415
###################################################
sf_katrina_41$f_owntype1 <- recode(sf_katrina_41$owntype, "1='sole'; c(2,3)='chain'", as.factor=TRUE)
sf_katrina_41$f_sesstatus1 <- recode(sf_katrina_41$sesstatus, "c(1,2)='lower'; c(3,4,5)='av'", as.factor=TRUE)
sf_katrina_41$f_sizeemp1 <- recode(sf_katrina_41$sizeemp, "1='small'; c(2,3)='av'", as.factor=TRUE)
sf_katrina_41$f_flood <- factor(as.integer(sf_katrina_41$flood < -4))
sf_katrina_41$flood1 <- abs(sf_katrina_41$flood)
sf_katrina_41$all_id <- 1:nrow(sf_katrina_41)
streets_41 <- split(sf_katrina_41, sf_katrina_41$f_street)


###################################################
### code chunk number 25: bivand+gomez-rubio_SMij.Rnw:426-432
###################################################
library(survival)
km_all <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = sf_katrina_41)
km_car <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = streets_41[["Carrollton Avenue"]])
km_mag <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = streets_41[["Magazine Street"]])
km_stc <- survfit(Surv(time = days_mod1, event = status) ~ 1, data = streets_41[["St. Claude Avenue"]])
km_streets <- survfit(Surv(time = days_mod1, event = status) ~ f_street, data = sf_katrina_41)


###################################################
### code chunk number 26: bivand+gomez-rubio_SMij.Rnw:437-464
###################################################
slices_d <- (c(41, 90, 180, 360)-41)/30
suppressPackageStartupMessages(library(zoo))
lbs <- as.yearmon(seq(as.Date("2005-08-29")+41, as.Date("2006-08-29")+41, 60))
oopar <- par(mfrow=c(2,2), mar=c(3, 3, 3, 1))
plot(km_all, col="blue", main="All", xaxs="r", axes=FALSE)
abline(v=slices_d, lty=3)
axis(1, at=seq(0,12,2), labels=lbs)
box()
axis(2)
plot(km_car, main="Carrollton Avenue", xaxs="r", axes=FALSE)
abline(v=slices_d, lty=3)
axis(1, at=seq(0,12,2), labels=lbs)
box()
axis(2)
plot(km_mag, col="red", main="Magazine Street", xaxs="r", axes=FALSE)
abline(v=slices_d, lty=3)
axis(1, at=seq(0,12,2), labels=lbs)
box()
axis(2)
plot(km_stc, col="green", main="St. Claude Avenue", xaxs="r", axes=FALSE)
abline(v=slices_d, lty=3)
axis(1, at=seq(0,12,2), labels=lbs)
box()
axis(2)
par(oopar)


###################################################
### code chunk number 27: bivand+gomez-rubio_SMij.Rnw:473-481
###################################################
plot(km_streets, main="", conf.int=TRUE, col=1:3, xaxs="r", axes=FALSE)
legend("topright", legend=names(streets_41), col=1:3, pch=19, bty="n", cex=0.7)
abline(v=slices_d, lty=3)
axis(1, at=seq(0,12,2), labels=lbs)
box()
axis(2)


###################################################
### code chunk number 28: bivand+gomez-rubio_SMij.Rnw:487-491
###################################################
wb_modela_41 <- survreg(Surv(time = days_mod1, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1, data = sf_katrina_41, dist="weibull")
wb_modelb_41 <- survreg(Surv(time = days_mod1, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + 
                          log(medinc) + f_sizeemp1 + f_street, data = sf_katrina_41, dist="weibull")


###################################################
### code chunk number 29: bivand+gomez-rubio_SMij.Rnw:495-496
###################################################
suppressPackageStartupMessages(library(INLA))


###################################################
### code chunk number 30: bivand+gomez-rubio_SMij.Rnw:499-504 (eval = FALSE)
###################################################
sf_katrina_41$st_id <- as.numeric(sf_katrina_41$f_street)
all_surv_41 <- Surv(time = sf_katrina_41$days_mod1, event = sf_katrina_41$status)
all_surv_inla_41 <- inla.surv(all_surv_41[,1], all_surv_41[,2])
wb_inla_fita_41 <- inla(all_surv_inla_41 ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f(st_id, model="iid", param=c(1, 0.01)), data = sf_katrina_41, family = "weibullsurv")
# saveRDS(wb_inla_fita_41, file="wb_inla_fita_41.rds")


###################################################
### code chunk number 31: bivand+gomez-rubio_SMij.Rnw:512-527
###################################################
# wb_inla_fita_41 <- readRDS("wb_inla_fita_41.rds")
inla_fxd <- summary(wb_inla_fita_41)$fixed[-1, 1:2]
inla_fxd[,1] <- -1*inla_fxd[,1]
res <- rbind(summary(wb_modela_41)$table[-c(1,8),1:2], summary(wb_modelb_41)$table[-c(1,8:10),1:2], inla_fxd)
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
dres$street_dummy <- c(rep("no", 6), rep("yes", 6), rep("iid", 6))
names(dres)[1:2] <- c("mean", "sd")
dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
library(ggplot2)
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=street_dummy)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 32: bivand+gomez-rubio_SMij.Rnw:540-543
###################################################
mat <- -1*wb_inla_fita_41$summary.random$st_id[, 4:6]
rownames(mat) <- names(streets_41)
print(xtable(mat, align=c("l","r","r","r"), digits=c(NA, 5, 5, 5), display=c("s", "f", "f", "f")), floating=FALSE, comment=FALSE, sanitize.text.function=function(x){x}, hline.after=c(-1,0,3))


###################################################
### code chunk number 33: bivand+gomez-rubio_SMij.Rnw:558-565
###################################################
library(spdep)
nb <- knn2nb(knearneigh(sf_katrina_41, k=11), sym=TRUE)
E_all <- nb2mat(nb, style="B")


###################################################
### code chunk number 34: bivand+gomez-rubio_SMij.Rnw:568-571
###################################################
suppressPackageStartupMessages(library(spBayesSurv))
mcmc <- list(nburn = 10000, nsave = 50000, nskip = 1, ndisplay= 10000)
prior <- list(maxL = 15)


###################################################
### code chunk number 35: bivand+gomez-rubio_SMij.Rnw:575-578 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all_fit <- survregbayes(Surv(time = days_mod1, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + frailtyprior("car", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_all))
# saveRDS(wb_all_fit, "wb_all_fit.rds")


###################################################
### code chunk number 36: bivand+gomez-rubio_SMij.Rnw:582-585 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_all_fit1 <- survregbayes(Surv(time = days_mod1, event = status) ~  elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 + f_street + frailtyprior("car", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior, Proximity= E_all))
# saveRDS(wb_all_fit1, "wb_all_fit1.rds")


###################################################
### code chunk number 37: bivand+gomez-rubio_SMij.Rnw:589-592 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_fit <- survregbayes(Surv(time = days_mod, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  frailtyprior("iid", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
# saveRDS(wb_iid_fit, "wb_iid_fit.rds")


###################################################
### code chunk number 38: bivand+gomez-rubio_SMij.Rnw:596-599 (eval = FALSE)
###################################################
set.seed(1)
suppressMessages(wb_iid_fit1 <- survregbayes(Surv(time = days_mod, event = status) ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1 +  f_street + frailtyprior("iid", all_id), data = sf_katrina_41, survmodel = "AFT", dist = "weibull", mcmc = mcmc, prior = prior))
# saveRDS(wb_iid_fit1, "wb_iid_fit1.rds")


###################################################
### code chunk number 39: bivand+gomez-rubio_SMij.Rnw:603-612
###################################################
# wb_all_fit <- readRDS("wb_all_fit.rds")
# wb_all_fit1 <- readRDS("wb_all_fit1.rds")
wb_fit_all_draws_long <- c(t(-1*wb_all_fit$beta), wb_all_fit$tau2)
vars <- rep(c(colnames(wb_all_fit$X), "frailtyvar"), each=ncol(wb_all_fit$beta))
wb_fit1_all_draws_long <- c(t(-1*wb_all_fit1$beta[1:6,]), wb_all_fit1$tau2)
df <- data.frame(value=c(wb_fit_all_draws_long, wb_fit1_all_draws_long), covariate=factor(c(vars, vars), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar")), street_dummy=c(rep("no", length(vars)), rep("yes", length(vars))))
ggplot(df, aes(covariate, y=value, fill=street_dummy)) + geom_boxplot() + facet_wrap(~covariate, scale="free") + geom_hline(yintercept=0, linetype=2)  + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 40: bivand+gomez-rubio_SMij.Rnw:624-640
###################################################
c1 <- summary(wb_all_fit)$coeff[, c(1, 3)]
c1[, 1] <- -1*c1[, 1]
c2 <- summary(wb_all_fit1)$coeff[1:6, c(1, 3)]
c2[, 1] <- -1*c2[, 1]
res <- rbind(c1, c(summary(wb_all_fit)$frailvar[1, c(1,3)]), c2, c(summary(wb_all_fit1)$frailvar[1, c(1,3)]))
rownames(res)[c(7, 14)] <- "frailtyvar"
dres <- as.data.frame(res)
dres$covariate <- factor(rownames(res), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small", "frailtyvar"))
dres$street_dummy <- c(rep("no", 7), rep("yes", 7))
names(dres)[1:2] <- c("mean", "sd")
dres$ymax <- dres$mean + qnorm(0.975)*dres$sd
dres$ymin <- dres$mean + qnorm(0.025)*dres$sd
limits <- aes(ymax = ymax, ymin=ymin)
ggplot(dres, aes(y=mean, x=covariate, shape=street_dummy)) + geom_point(position=position_dodge(.9), cex=2) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 41: bivand+gomez-rubio_SMij.Rnw:657-661
###################################################
mat <- cbind(t(-1*wb_all_fit$beta), wb_all_fit$tau2)
colnames(mat) <- c(colnames(wb_all_fit$X), "frailtyvar")
library(coda)
fit_coda <- mcmc(mat)


###################################################
### code chunk number 42: bivand+gomez-rubio_SMij.Rnw:665-670
###################################################
oopar <- par(mfrow=c(3, 3))
plot(fit_coda, density=FALSE, auto.layout=FALSE)
par(oopar)


###################################################
### code chunk number 43: bivand+gomez-rubio_SMij.Rnw:676-680
###################################################
mat <- cbind(t(-1*wb_all_fit1$beta), wb_all_fit1$tau2)
colnames(mat) <- c(colnames(wb_all_fit1$X), "frailtyvar")
library(coda)
fit1_coda <- mcmc(mat)


###################################################
### code chunk number 44: bivand+gomez-rubio_SMij.Rnw:684-689
###################################################
oopar <- par(mfrow=c(3, 3))
plot(fit1_coda, density=FALSE, auto.layout=FALSE)
par(oopar)


###################################################
### code chunk number 45: bivand+gomez-rubio_SMij.Rnw:700-703
###################################################
forma <- ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1
prob_fit0_90a <- glm(update(forma, y0_90 ~ .), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)
prob_fit0_90b <- glm(update(forma, y0_90 ~ . + f_street), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 46: bivand+gomez-rubio_SMij.Rnw:708-711
###################################################
forma <- ~ elevation1 + I(elevation1^2) + f_owntype1 + f_sesstatus1 + log(medinc) + f_sizeemp1
prob_fit0_360a <- glm(update(forma, y0_360 ~ .), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)
prob_fit0_360b <- glm(update(forma, y0_360 ~ . + f_street), data=sf_katrina_41, family = binomial(link = "probit"), x=TRUE)


###################################################
### code chunk number 47: bivand+gomez-rubio_SMij.Rnw:715-732
###################################################
res_90 <- rbind(summary(prob_fit0_90a)$coefficients[-1,1:2], summary(prob_fit0_90b)$coefficients[-c(1, 8, 9),1:2])
dres_90 <- as.data.frame(res_90)
dres_90$covariate <- factor(rownames(res_90), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
dres_90$street_dummy <- c(rep("no", 6), rep("yes", 6))
dres_90$street_by_days <- paste(dres_90$street_dummy, "90", sep=":")
names(dres_90)[1:2] <- c("mean", "sd")
res_360 <- rbind(summary(prob_fit0_360a)$coefficients[-1,1:2], summary(prob_fit0_360b)$coefficients[-c(1, 8, 9),1:2])
dres_360 <- as.data.frame(res_360)
dres_360$covariate <- factor(rownames(res_360), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
dres_360$street_dummy <- c(rep("no", 6), rep("yes", 6))
dres_360$street_by_days <- paste(dres_360$street_dummy, "360", sep=":")
names(dres_360)[1:2] <- c("mean", "sd")
dres <- rbind(dres_90, dres_360)
limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
ggplot(dres, aes(y=mean, x=covariate, shape=street_by_days)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


###################################################
### code chunk number 48: bivand+gomez-rubio_SMij.Rnw:745-760
###################################################
suppressPackageStartupMessages(library(spatialreg))
W <- as(nb2listw(nb, style="W"), "CsparseMatrix")
suppressPackageStartupMessages(library(ProbitSpatial))
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_90a <- SpatialProbitFit(update(forma, y0_90 ~ .), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
res90a <- read.table(tf, skip=3, header=FALSE)[1:6,1:3]
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_90b <- SpatialProbitFit(update(forma, y0_90 ~ . + f_street), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
res90b <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_360a <- SpatialProbitFit(update(forma, y0_360 ~ .), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
res360a <- read.table(tf, skip=3, header=FALSE)[1:6,1:3]
tf <- tempfile()
suppressWarnings(capture.output(SemProb_fit0_360c <- SpatialProbitFit(update(forma, y0_360 ~ . + f_street), data=sf_katrina_41, W=W, DGP="SEM", method="full-lik"), file=tf))
res360b <- read.table(tf, skip=3, header=FALSE, fill=TRUE, nrows=6)[,1:3]


###################################################
### code chunk number 49: bivand+gomez-rubio_SMij.Rnw:764-780
###################################################
res_90 <- rbind(res90a, res90b)
dres_90 <- as.data.frame(res_90)
dres_90$covariate <- factor(as.character(dres_90$V1), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
dres_90$street_dummy <- c(rep("no", 6), rep("yes", 6))
names(dres_90)[2:3] <- c("mean", "sd")
dres_90$street_by_days <- paste(dres_90$street_dummy, "90", sep=":")
res_360 <- rbind(res360a, res360b)
dres_360 <- as.data.frame(res_360)
dres_360$covariate <- factor(as.character(dres_360$V1), levels=c("elevation1", "I(elevation1^2)", "f_owntype1sole", "f_sesstatus1lower", "log(medinc)", "f_sizeemp1small"))
dres_360$street_dummy <- c(rep("no", 6), rep("yes", 6))
names(dres_360)[2:3] <- c("mean", "sd")
dres_360$street_by_days <- paste(dres_360$street_dummy, "360", sep=":")
limits <- aes(ymax = mean + qnorm(0.975)*sd, ymin=mean + qnorm(0.025)*sd)
ggplot(dres, aes(y=mean, x=covariate, shape=street_by_days)) + geom_point(position=position_dodge(.9), cex=3) + geom_errorbar(limits, position=position_dodge(.9)) + geom_hline(yintercept = 0, linetype=2) + facet_wrap(~covariate, scale="free") + theme(plot.background = element_rect(fill = "transparent",colour = NA), legend.background = element_rect(colour = NA, fill = "transparent")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylab("")


