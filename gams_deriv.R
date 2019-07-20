# ============================================================================================================
# Test Gams Derivatives
# How to compute derivatives and periods of change for interactions in GAMS?
# ============================================================================================================

# clear workspace
rm(list=ls(all=TRUE))

# load libraries
sapply(c("readr", "mgcv", "gratia"), require, character.only = TRUE, quietly = TRUE)

# import data
dta.gam  <- read_csv('data.csv')

# housekeeping
dta.gam$region <- as.factor(dta.gam$region)


# Gam model
mod  <- bam(tweets ~ ti(time, k=210, bs="cr")
            + ti(temp, bs='cc', k=53)
            + ti(time, temp)
            + s(time, region, bs="fs", m=1)
            + s(temp, region, bs="fs", m=1),
            data=dta.gam,
            method="fREML",
            family=tw(),
            discrete=TRUE,
            nthreads=4)

summary(mod)

# check for overdispersion
resid.ssq <- sum(residuals(mod,type="pearson")^2) ## sum of squares of Pearson resids
resid.df <- nrow(dta.gam)-length(coef(mod)) ## estimated resid df (N-p)
resid.ssq/resid.df

# check model
gam.check(mod)

# plot(gam)
draw(mod, select=2)

# compute derivative for temp with derivatives() from gratia package
derivatives(mod, term="temp", type = "central")

# try with fderiv()
fderiv(mod, term="temp")

