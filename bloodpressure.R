
library(mosaic)
library("ggpubr")
library(PairedData)
library(tidyverse)
library(rstatix)

setwd("OneDrive - Downstate Medical Center/projects/sickle cell disease and blood pressure/")
bp=read.csv("BLOOD PRESSURES_COMPARISON_25june2020.csv")

wilcox.test(bp$sickle5th, bp$nonsickle2004_5, paired=T, subset=(gender=="male" & type=="diastolic"))

wilcox.test(bp$sickle5th, bp$nonsickle2004_5, paired=T, subset=(gender=="male" & type=="systolic"))

wilcox.test(bp$sickle5th, bp$nonsickle2004_5, paired=T, subset=(gender=="female" & type=="diastolic"))

wilcox.test(bp$sickle5th, bp$nonsickle2004_5, paired=T, subset=(gender=="female" & type=="systolic"))

sickle5=subset(bp,gender=="female" & type=="systolic")


# 5th percentile sickle vs non sickle 2004

bp.long <- bp %>%
  gather(key = "group", value = "bloodpressure", sickle5th, nonsickle2004_5)
head(bp.long, 3)

# Effect size measure.  Can do this for each pair of groups.
bp.long  %>%
	filter(type=="systolic") %>%
  wilcox_effsize(bloodpressure ~ group, paired = TRUE)

# Note that there's a large difference in diastolic blood pressure for the non-sickle groups in 2004 and 2017, and moderate difference in systolic blood pressure. This is paired within gender and height percentiles, but not stratified by these.
bp.long  %>%
	filter(type=="diastolic") %>%
  wilcox_effsize(bloodpressure ~ group, paired = TRUE)


# 		label="Age",

bxp5 <- ggpaired(bp.long, x = "group", y = "bloodpressure", facet.by=c("type", "gender"),
		color="Height_perc",
         order = c("sickle5th", "nonsickle2004_5"),
         ylab = "Blood pressure", xlab = "Groups")
bxp5

bxp <- ggboxplot(
  bp.long, x="group", y="bloodpressure", width = 0.5, facet.by=c("type", "gender"),
  add = c("mean", "jitter"), color="Height_perc",
  ylab = "Blood pressure", xlab = "Groups"
  )
bxp


# 5th percentile sickle vs non sickle 2004 and 2017

bp.long <- bp %>%
  gather(key = "group", value = "bloodpressure", sickle5th, nonsickle2004_5, nonsickle2017_5)
head(bp.long, 3)


# 		label="Age",

bxp5 <- ggpaired(bp.long, x = "group", y = "bloodpressure", facet.by=c("type", "gender"),
		color="Height_perc",
         order = c("sickle5th", "nonsickle2004_5", "nonsickle2017_5"),
         ylab = "Blood pressure", xlab = "Groups")
bxp5



stat.test <- stat.test %>% add_xy_position(x = "group")
	bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))