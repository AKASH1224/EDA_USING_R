# EDA Graphical Method Distributions

# loading the required packages
library("ggplot2")
library(aqp)
library(soilDB)

# Load from the loafercreek dataset
data("loafercreek")
loafercreek
# Construct generalized horizon designations
n <- c("A", "BAt", "Bt1", "Bt2", "Cr", "R")

# REGEX rules
p <- c("A", "BA|AB", "Bt|Bw", "Bt3|Bt4|2B|C",
       "Cr", "R")

# Compute genhz labels and add
# to loafercreek dataset
loafercreek$genhz <- generalize.hz(
  loafercreek$hzname, n, p)

# Extract the horizon table
h <- horizons(loafercreek)

# Examine the matching of pairing
# of the genhz label to the hznames
table(h$genhz, h$hzname)

vars <- c("genhz", "clay", "total_frags_pct",
          "phfield", "effclass")
summary(h[, vars])

sort(unique(h$hzname))
h$hzname <- ifelse(h$hzname == "BT",
                   "Bt", h$hzname)

# graphs
# bar plot
ggplot(h, aes(x = texcl)) +geom_bar()

# histogram
ggplot(h, aes(x = clay)) +
  geom_histogram(bins = nclass.Sturges(h$clay))

# density curve
ggplot(h, aes(x = clay)) + geom_density()

# box plot
ggplot(h, (aes(x = genhz, y = clay))) +
  geom_boxplot()

# QQ Plot for Clay
ggplot(h, aes(sample = clay)) +
  geom_qq() +
  geom_qq_line()
