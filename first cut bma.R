setwd("C:/Users/stefa/Dropbox/Dissertation/Data/Budget & Appropriations/Appropriations")

library(foreign)
library(MASS)
library(MCMCpack)
library(MCMCglmm)
library(arm)
library(BMS)
library(BayesFactor)

set.seed(1859)

dat <- read.csv("test.csv")

# sub in/out diff DVs, IVs. For House winning model
data.sub <- dat[,c("hconf_winner", "delay",  "house_margin", "conference", "majority_house", "hearing_days", "hearing_count",
                   "h_polar", "unified", "grh", "bea", "chamber_diff", "outlays_approp", "cong_approval", 
                   "unemployment", "deficit", "inflation_rate", "hac_cc_d", "happ_house_diff")]
data.sub <- na.omit((data.sub))

#"cr_duration", "cr", "omnibus", "supplemental", 

# for Senate winning model
data.sub1 <- dat[,c("sconf_winner", "delay",  "senate_margin", "conference", "majority_senate",
                   "s_polar", "unified", "grh", "bea", "chamber_diff", "outlays_approp", "cong_approval", 
                   "unemployment", "deficit", "inflation_rate", "omnibus", "supplemental", "cr_duration", "cr", 
                   "sac_cc_d", "sapp_senate_diff")]
data.sub1 <- na.omit((data.sub1))

# for Executive/President winning model
data.sub2 <- dat[,c("econf_winner", "delay", "conference", "veto",
                    "s_polar", "h_polar", "unified", "grh", "bea", "chamber_diff", "outlays_approp", "cong_approval", 
                    "pres_approval",  "unemployment", "deficit", "inflation_rate", "omnibus", "supplemental", "cr_duration", "cr", 
                    "pres_chamb_diff", "hac_ex_d", "sac_ex_d", "hc_ex_d", "sc_ex_d")]
data.sub2 <- na.omit((data.sub2))


grid <- bms(data.sub, mprior = "uniform") 
grid # Print out results
image(grid) # Blue = positive coefficient; red = negative coefficient

install.packages("effects")
install.packages("clusterSEs")
require(effects)
library(clusterSEs)

m <- glm(hconf_winner ~ delay + happ_house_diff + hac_cc_d + outlays_approp + majority_house
           + unified + bea + hearing_count, dat = dat, family = binomial(link="logit"))
summary(m)

# model w/ cluster SEs on annual approps bill
clust.bs.p <- cluster.bs.glm(m, dat, ~ uniqueid, report = T)

