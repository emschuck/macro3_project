rm(list = ls())

library(foreign)
library(Synth)
library(xtable)

d <- read.dta("repgermany.dta")


# =====================================================
# Baseline synthetic control
# =====================================================

dataprep.out <- dataprep(
  foo            = d,
  predictors     = c("gdp", "trade", "infrate", "industry"),
  dependent      = "gdp",
  unit.variable  = 1,
  time.variable  = 3,
  special.predictors = list(
    list("schooling", c(1980, 1985), c("mean")),
    list("invest80",  1980,          c("mean"))
  ),
  treatment.identifier = 7,
  controls.identifier  = unique(d$index)[-7],
  time.predictors.prior = 1981:1990,
  time.optimize.ssr     = 1960:1990,
  unit.names.variable   = 2,
  time.plot             = 1960:2003
)

synth.out <- synth(
  data.prep.obj = dataprep.out
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# Baseline with adjusted optimizer options
# =====================================================

synth.out <- synth(
  data.prep.obj = dataprep.out,
  Margin.ipop   = 0.0005,
  Sigf.ipop     = 10,
  Bound.ipop    = 10
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# Equal predictor weights
# =====================================================

synth.out <- synth(
  data.prep.obj = dataprep.out,
  custom.v      = rep(1, 6) / 6
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# Regression weights
# =====================================================

X0 <- cbind(1, t(dataprep.out$X0))
X1 <- as.matrix(c(1, dataprep.out$X1))
W  <- X0 %*% solve(t(X0) %*% X0) %*% X1

synth.out.reg            <- synth.out
synth.out.reg$solution.w <- W

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out.reg
)

synth.tables

path.plot(
  synth.res       = synth.out.reg,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# GDP-only predictor
# =====================================================

dataprep.out <- dataprep(
  foo            = d,
  predictors     = "gdp",
  dependent      = "gdp",
  unit.variable  = 1,
  time.variable  = 3,
  treatment.identifier = 7,
  controls.identifier  = unique(d$index)[-7],
  time.predictors.prior = 1981:1990,
  time.optimize.ssr     = 1960:1990,
  unit.names.variable   = 2,
  time.plot             = 1960:2003
)

synth.out <- synth(
  data.prep.obj = dataprep.out,
  custom.v      = 1
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# GDP-only predictor plus lagged GDP
# =====================================================

dataprep.out <- dataprep(
  foo            = d,
  predictors     = "gdp",
  dependent      = "gdp",
  unit.variable  = 1,
  time.variable  = 3,
  special.predictors = list(
    list("gdp", 1971:1980, c("mean"))
  ),
  treatment.identifier = 7,
  controls.identifier  = unique(d$index)[-7],
  time.predictors.prior = 1981:1990,
  time.optimize.ssr     = 1960:1990,
  unit.names.variable   = 2,
  time.plot             = 1960:2003
)

synth.out <- synth(
  data.prep.obj = dataprep.out,
  custom.v      = c(0.5, 0.5)
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1990,
  Legend.position = "topleft"
)


# =====================================================
# Placebo treatment year: 1980
# =====================================================

dataprep.out <- dataprep(
  foo            = d,
  predictors     = c("gdp", "trade", "infrate", "industry"),
  dependent      = "gdp",
  unit.variable  = 1,
  time.variable  = 3,
  special.predictors = list(
    list("schooling", c(1970, 1975), c("mean")),
    list("invest80",  1980,          c("mean"))
  ),
  treatment.identifier = 7,
  controls.identifier  = unique(d$index)[-7],
  time.predictors.prior = 1971:1980,
  time.optimize.ssr     = 1960:1980,
  unit.names.variable   = 2,
  time.plot             = 1960:2003
)

synth.out <- synth(
  data.prep.obj = dataprep.out
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1980,
  Legend.position = "topleft"
)


# =====================================================
# Placebo treatment year: 1980, equal predictor weights
# =====================================================

synth.out <- synth(
  data.prep.obj = dataprep.out,
  custom.v      = rep(1, 6) / 6
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

synth.tables

path.plot(
  synth.res       = synth.out,
  dataprep.res    = dataprep.out,
  Ylim            = c(0, 34000),
  tr.intake       = 1980,
  Legend.position = "topleft"
)