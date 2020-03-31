require('pacman')
suppressWarnings(pacman::p_load(tidyverse, survival, survminer, lubridate, scales, knitr, kableExtra, readxl, patchwork, writexl, broom, ggrepel, ggpmisc))

#Figure 1A data import and survival objects
insec.NP <- read.csv("C:/OneDrive/OneDrive - National Institutes of Health/InsectaryExperiments/22-Jan-18-R.formatted.masterlist.csv") %>%
  filter(Experiment != "nonbloodfed.survival" & Experiment != "pupae.primed" & Experiment != "SEasL1.half.exp.with.just.18C") %>%
  filter(Primed.as=="Not.primed" & Experiment != "multi.feed" & Experiment != "single.feed.rep")
fit.NP <- survfit(Surv(Date.of.death, Censor) ~ Temp, data=insec.NP) #weird piping bug workaround

#Figure 1A stats table with maximum age 
fit.NP #overall stats
max.1A <- insec.NP %>% group_by(Temp) %>% summarise(max=max(Date.of.death)) #max survival

NP.sum <- summary(fit.NP)$table %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(CI= paste(`0.95LCL`, `0.95UCL`, sep='-')) %>%
  select(n=records, mean=`*rmean`, se=`*se(rmean)`, median, CI) %>% 
  data.frame(Temp = c("18", "27"), .) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  left_join(max.1A) #full table
writexl::write_xlsx(NP.sum, "13-Feb-20-1A.summary.table.xlsx")

res.NP <- insec.NP %>%
  pairwise_survdiff(Surv(Date.of.death, Censor) ~ Temp, data=., p.adjust.method = 'BH') 

res.NP$p.value


#Figure 1A
cairo_pdf("30-Mar-20.figure1A.pdf", fallback_resolution = 600, height=8, width=10)
palette18.27 <- c('#00bfc4', '#f4a582')
bf <- ggsurvplot(fit.NP,insec.NP, break.time.by = 5, break.y.by=.1, 
                 risk.table=TRUE, title="Unprimed", surv.median.line = "hv", ggtheme=theme_bw(), palette = palette18.27, size=1.5, xlim=c(0,80), pval=TRUE, pval.coord = c(3, 0.03), font.title=14, font.x=12, font.y=12, panel.labs.font.y=12, font.legend=12, font.tickslab=12, legend="bottom") 
bf$plot <- bf$plot + theme(legend.text=element_text(size=14), plot.title = element_text(size = 14), axis.title=element_text(size=14)) + geom_vline(xintercept=60, alpha=0.7, color="red", linetype=2) + guides(color=guide_legend(nrow=2,byrow=TRUE))
bf$table <- bf$table + theme(text = element_text(size=14)) +ggtitle("Number alive")
bf
dev.off()


#Figure 1B data import and survival objects
insec.old.prime.L1 <- read.csv("22-Jan-18-R.formatted.masterlist.csv") %>%
  filter(Primed.as == "L1", Temp != "18.male") 
fit.old.prime.L1 <- survfit(Surv(Date.of.death, Censor) ~ Temp, data=insec.old.prime.L1)

#Figure 1B stats table with maximum age 
fit.old.prime.L1 #overall stats
max.1B <- insec.old.prime.L1 %>% group_by(Temp) %>% summarise(max=max(Date.of.death)) #max survival

sum.1B <- summary(fit.old.prime.L1)$table %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(CI= paste(`0.95LCL`, `0.95UCL`, sep='-')) %>%
  select(n=records, mean=`*rmean`, se=`*se(rmean)`, median, CI) %>% 
  data.frame(Temp = c("18", "22", "27", "SE"), .) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  left_join(max.1B) #full table
writexl::write_xlsx(sum.1B, "13-Feb-20-1B.summary.table.xlsx")

#Figure 1B
se.18.22.27 <- c('#00bfc4', "#7aa84c", '#f4a582',"#ca0020")
cairo_pdf("30-Mar-20.figure1B.pdf", fallback_resolution = 600, height=8, width=10)
p.l1 <- ggsurvplot(fit.old.prime.L1,insec.old.prime.L1, break.time.by = 10, break.y.by=.1, 
                   risk.table=TRUE, title="Primed as L1", surv.median.line = "hv", ggtheme=theme_bw(), palette = se.18.22.27, size=1.5, xlim=c(0,110), font.title=12, font.x=12, font.y=12, font.legend=12, font.tickslab=12, legend="bottom")
p.l1$plot <- p.l1$plot + theme(legend.text=element_text(size=14), plot.title = element_text(size = 14), axis.title=element_text(size=14)) + geom_vline(xintercept=60, alpha=0.7, color="red", linetype=2) + guides(color=guide_legend(nrow=2,byrow=TRUE))
p.l1$table <- p.l1$table + theme(text = element_text(size=14)) +ggtitle("Number alive")
p.l1
dev.off()

#Stats 1B
res.old.prime.L1 <- insec.old.prime.L1 %>%
  pairwise_survdiff(Surv(Date.of.death, Censor) ~ Temp, data=., p.adjust.method = 'BH') 

res.old.prime.L1$p.value


#Figure 2 - data input and survival objects
insec.merge2 <- bind_rows(read_xlsx("14-Sept-18-merged.photoperiod.Rformat.xlsx"), read_xlsx("1-Oct-18-photoperiod.round3.Rformat.xlsx")) %>%
  mutate(Temp = factor(Temp, levels=c("27", "20")),
         Light.Hours = factor(Light.Hours, levels=c("12","8")),
         Species = factor(Species, levels=c("S", "M")))
insec.merge2$Censor[is.na(insec.merge2$Censor)] <- 0
fit.merge2 <- survfit(Surv(Death.Age, Censor) ~ Temp + Species + Light.Hours + Primed, data=insec.merge2)

#Figure 2 stats
res_merge <- insec.merge2 %>%
  pairwise_survdiff(Surv(Death.Age, Censor) ~ Temp + Species + Light.Hours + Primed, data=., p.adjust.method = 'BH') 

res_merge$p.value


fig2.pvalues <- res_merge$p.value %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  select(comp.1=rowname, everything()) %>% 
  pivot_longer(cols=2:16, names_to='comp.2', values_to = 'pval') %>% 
  filter(!is.na(pval)) %>% 
  distinct() %>%
  mutate(sig = case_when(pval <= 0.05 & pval >= 0.01 ~ "*",
                         pval < 0.01 & pval >= 0.001 ~ "**",
                         pval < 0.001 ~ "***",
                         pval > 0.05 ~ "NS"),
         pval=prettyNum(pval))

max.im2 <- insec.merge2 %>% group_by(Species, Temp, Primed, Light.Hours) %>% summarise(max=max(Death.Age)) %>%
  mutate(Light.Hours = as.numeric(as.character(Light.Hours))) #max value
full.sum <- summary(fit.merge2)$table %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(CI= paste(`0.95LCL`, `0.95UCL`, sep='-')) %>%
  select(n=records, mean=`*rmean`, se=`*se(rmean)`, median, CI) %>% 
  data.frame(Temp=c(rep("27", 8), rep("20", 8)),
             Species=c(rep("M", 4), rep("S", 4),rep("M", 4), rep("S", 4)), 
             Light.Hours=c(12, 12, 8, 8, 12, 12, 8, 8, 12, 12, 8, 8, 12, 12, 8, 8),
             Primed=c(strrep(c("N", "Y"), 1)), .) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  left_join(max.im2)
write_xlsx(full.sum, "28-Jan-20-summary.table.fig.2.with.max.xlsx")

#Figure 2 - plot
bkpalette <- c('#f4a582', '#ca0020','#92c5de','#0571b0')
plot_merge_w_rnd3 <- ggsurvplot(fit.merge2,insec.merge2, pval=FALSE, conf.int=FALSE, risk.table=FALSE, 
                                xlim=c(5,110), pval.coord=c(25, 0.8), break.time.by = 5, break.y.by=.1, 
                                title="Effects of Photoperiod with Priming - All merge", surv.median.line = "hv", 
                                facet.by=c("Light.Hours", "Temp"), ggtheme=theme_bw(), ylab="Survival", xlab="Time (Days)", font.title=12, font.x=12, font.y=12, font.legend=12, palette = bkpalette, size=1.5) + theme(panel.grid.major = element_line(size=0.8))


cairo_pdf("30-Mar-20.figure2.pdf", fallback_resolution = 600, height=8, width=12)
plot_merge_w_rnd3 + geom_vline(xintercept=90, alpha=0.7) + 
  theme(strip.text = element_text(size = 12), axis.text.y=element_text(size=12), axis.text.x=element_text(size=12, angle=45, hjust=1))
dev.off()



#Second Experiment (Figure 2 data) -- Fitting Parametric and Non-parametric survival models
#initial look at variable importance --- using cox proportional hazards
cph <- coxph(Surv(Death.Age, Censor) ~ Temp+Species+Light.Hours+Primed, data=insec.merge2) 
cph 

ggforest(cph)
#however it violates proportional hazards assumption for light hours variable
cph.zph <- cox.zph(cph)
cph.zph


#moving to other models after CPH violation
#finding best fit of available survreg distributions
dists2 <- c("extreme", "logistic", "gaussian", "weibull", "exponential", "rayleigh", "loggaussian", "lognormal", "loglogistic", "t")

fsmap <- function(dist.input){
  model <- survreg(Surv(insec.merge2$Death.Age,insec.merge2$Censor)~1, data=insec.merge2, dist=dist.input)
  df <- data.frame(distribution = dist.input, glance(model)) %>%
    select(distribution, chi.sq=statistic, df, AIC)
  #df2 <- data.frame(distribution = dist.input, t(data.frame(exp(coef(model)))))
  return(df)
}

allvarmap <- function(dist.input){
  model <- survreg(Surv(insec.merge2$Death.Age,insec.merge2$Censor)~Temp + Species + Light.Hours + Primed, data=insec.merge2, dist=dist.input)
  df <- data.frame(distribution = dist.input,  glance(model)) %>%
    select(distribution, chi.sq=statistic, df, AIC)
  #df2 <- data.frame(distribution = dist.input, t(data.frame(exp(coef(model)))))
  return(df)
}

#calculate AIC for model selection without/with variables
baseAIC <- map_dfr(dists2, fsmap) %>% arrange(AIC) %>% mutate(dAIC=AIC-min(AIC))
allvarAIC <- map_dfr(dists2, allvarmap) %>% arrange(AIC) %>% mutate(dAIC=AIC-min(AIC))

#and with interactions
allvarmap.inter <- function(dist.input){
  model <- survreg(Surv(insec.merge2$Death.Age,insec.merge2$Censor)~Temp + Light.Hours + Primed + (Temp*Species) + (Light.Hours*Species) + (Primed*Species), data=insec.merge2, dist=dist.input)
  df <- data.frame(distribution = dist.input,  glance(model)) %>%
    select(distribution, chi.sq=statistic, df, AIC)
  return(df)
}

allvarAIC.inter <- map_dfr(dists2, allvarmap.inter) %>% arrange(AIC) %>% mutate(dAIC=AIC-min(AIC))

#weibull AFT model seems overall the most promising, moving forward with that
weibull.model <- survreg(Surv(insec.merge2$Death.Age,insec.merge2$Censor)~Temp + Species + Light.Hours + Primed, data=insec.merge2, dist='weibull')
weibull.summary <- summary(weibull.model)$table %>% as.data.frame() %>% rownames_to_column() %>% mutate(ETR=exp(Value)) %>%
  select(variable=rowname, value= Value, SE = `Std. Error`, z, p, ETR)

#and weibull with interactions (included in paper)
weibull.model.inter <- survreg(Surv(insec.merge2$Death.Age,insec.merge2$Censor)~Temp + Light.Hours + Primed + Species + (Temp*Species) + (Light.Hours*Species) + (Primed*Species), data=insec.merge2, dist='weibull')
weibull.summary.inter <- summary(weibull.model.inter)$table %>% as.data.frame() %>% rownames_to_column() %>% mutate(ETR=exp(Value)) %>%
  select(variable=rowname, value= Value, SE = `Std. Error`, z, p, ETR)

#Supplemental Table - AIC -- Additional File 3
write_xlsx(allvarAIC, "29-feb-20-aic.summary.table.xlsx")
write_xlsx(allvarAIC.inter, "26-Mar-20-aic.summary.table.inter.xlsx")

#Table - WeibullSummary
write_xlsx(weibull.summary.inter, "26-Mar-20-weibull.summary.table.inter.xlsx")


#Additional File 1:
#See 27-Mar-20-incubator.plotting.R document for this figure

#Additional File 2:
insec.merge.rain <- insec.merge2 %>%
  mutate(Primed.Spec = paste(Primed, Species, sep='.'),
         Primed.Spec = case_when(Primed.Spec == "N.M" ~ "M form - Unprimed", 
                                 Primed.Spec =="Y.M" ~ "M form - Primed",
                                 Primed.Spec == "N.S" ~ "S form - Unprimed", 
                                 Primed.Spec == "Y.S" ~ "S form - Primed"),
         Temp2 = case_when(Temp == "27" ~ "27 °C",
                           Temp == "20" ~ "20 °C"),
         Temp2 = factor(Temp2, levels=c("27 °C", "20 °C")),
         Primed.Spec = factor(Primed.Spec, levels=c("M form - Unprimed", "M form - Primed", "S form - Unprimed", "S form - Primed")),
         Light.Hours2=case_when(Light.Hours == "8" ~ "8:16 L:D",
                                Light.Hours == "12" ~ "12:12 L:D"))

#stuff for raincloud plotting
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#from https://github.com/RainCloudPlots/RainCloudPlots
raincloud_theme <- theme( 
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  # panel.border = element_blank(),
  # panel.grid.minor = element_blank(),
  # panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))

rainpalette <- c("#E0E0E0", "#FF8A65", "#03A9F4", "#FF9800")
ggplot(data = insec.merge.rain, 
       aes(x = Primed.Spec, y = Death.Age, fill = Primed.Spec)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 0.3) +
  geom_point(aes(y = Death.Age, fill = Primed.Spec), colour="black",pch=21,
             position = position_jitter(width = .1), size = 1.5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = rainpalette) +
  scale_fill_manual(values = rainpalette) +
  #coord_flip() + # flip or not
  theme_bw() +
  raincloud_theme + labs(y="Age at Death", x="Group") + facet_grid(Light.Hours2~Temp2) + theme(strip.text = element_text(size=16)) + scale_y_continuous(breaks=pretty_breaks(n=10))
ggsave("22-Jul-19-additional.file.2.pdf", device=cairo_pdf, dpi=600, height=10, width=14)

##Additional File 4
length.output <- readRDS("length.output.RDS")
length.output.rnd2 <- readRDS("length.output.rnd2.RDS")

length.summary.df <- bind_rows(length.output, 
                               length.output.rnd2 %>% 
                                 filter(is.na(point.missing) & Dist >0 & ID != "P-336") %>%
                                 mutate(Cage.letter = as.character(Cage.letter))) %>%
  mutate(Primed.Spec = paste(Primed, Species, sep='.'),
         Primed.Spec = case_when(Primed.Spec == "N.M" ~ "M form - Unprimed", 
                                 Primed.Spec =="Y.M" ~ "M form - Primed",
                                 Primed.Spec == "N.S" ~ "S form - Unprimed", 
                                 Primed.Spec == "Y.S" ~ "S form - Primed"),
         Primed.Spec = factor(Primed.Spec, levels=c("M form - Unprimed", "M form - Primed", "S form - Unprimed", "S form - Primed")),
         age.class= case_when(Death.Age < 30 ~ "Young (< 30 days)",                                                                                Death.Age > 75 ~ "Old (75+)", TRUE ~ "Middle (30-75)"),
         age.class=factor(age.class, levels=c("Young (< 30 days)", "Middle (30-75)", "Old (75+)")),
         alt.ps = paste(Primed, Species, sep='.'))




lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumleng <- length.summary.df %>% 
  select(Species, Primed, Dist) %>% 
  group_by(Species, Primed) %>% 
  summarise_all(funs(mean, median, lower = lb, upper = ub))

sumleng


g <- 
  ggplot(data = length.summary.df, 
         aes(x = Primed.Spec, y = Dist, fill = Primed.Spec)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .5) +
  geom_point(aes(y = Dist, color = Primed.Spec, shape = Experiment, alpha = Experiment), 
             position = position_jitter(width = .15), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  #coord_flip() + # flip or not
  theme_bw() +
  raincloud_theme + labs(y="Wing Length (mm)") + 
  scale_alpha_manual(values=c(0.4, 0.8)) +
  theme(axis.title.x=element_blank())

g

ggsave('15-Oct-19-Additional.File.4.pdf', device=cairo_pdf, dpi=300, height=4, width=8, units='in')

#Additional File 5 --- wing correlations
hulls.rnd2 <- readRDS("31-Mar-20-hulls.round2.RDS")

ggplot(hulls.rnd2 %>% 
         filter(is.na(point.missing) & ID != "P-336") %>%
         mutate(Cage.letter = as.character(Cage.letter))) + 
  geom_point(aes(x=Death.Age, y=area/265, fill=Species, alpha=Primed), colour="black",pch=21, size=4) + 
  scale_alpha_manual(values=c(0.1, 1)) + 
  facet_grid(Species~Primed, scales='free') + theme_bw() + ylab("Wing Area (mm2)") + ggtitle("Wing Area Round 2") + 
  geom_text_repel(data = hulls.rnd2 %>% filter(area/265 < 200), aes(x=Death.Age, y=area/265, label=ID)) + geom_smooth(aes(x=Death.Age, y=area/265), method='lm') +
  stat_poly_eq(formula = y ~ x, 
               aes(x=Death.Age, y=area/265, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = y ~ x),
                  label.x = "right",
                  label.y = "bottom",
                  aes(x=Death.Age, y=area/265, label = paste("italic(P)*\"-value = \"*", 
                                                             signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE)
ggsave("31-Mar-20-Additional.file.5.A.pdf", device = cairo_pdf, dpi=300, height=8, width=10)

hulls.rnd3 <- readRDS("31-Mar-20-hulls.round3.RDS")
missing.point.samples.round3 <- c("P3-014", "P3-015", "P3-019", "P3-022", "P3-028", "P3-040", 
                                  "P3-047", "P3-049", "P3-053", "P3-058", "P3-062", "P3-068", 
                                  "P3-071", "P3-082", "P3-083", "P3-088", "P3-089", "P3-090", 
                                  "P3-295", "P3-299", "P3-301", "P3-303", "P3-304", "P3-307", 
                                  "P3-311", "P3-313", "P3-314", "P3-319", "P3-325", "P3-326", 
                                  "P3-336", "P3-345")

ggplot(hulls.rnd3 %>% filter(!ID %in% missing.point.samples.round3)) + 
  geom_point(aes(x=Death.Age, y=area/265, fill=Species, alpha=Primed), 
             colour="black",pch=21, size=4) + 
  scale_alpha_manual(values=c(0.1, 1)) + 
  facet_grid(Species~Primed, scales='free') + theme_bw() + ylab("Wing Area (mm2)") + ggtitle("Wing Area Round 3") + geom_smooth(aes(x=Death.Age, y=area/265), method='lm') +
  stat_poly_eq(formula = y ~ x, 
               aes(x=Death.Age, y=area/265, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = y ~ x),
                  label.x = "right",
                  label.y = "bottom",
                  aes(x=Death.Age, y=area/265, label = paste("italic(P)*\"-value = \"*", 
                                                             signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE)
ggsave("31-Mar-20-Additional.file.5.B.pdf", device = cairo_pdf, dpi=300, height=8, width=10)


