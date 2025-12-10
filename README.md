**Developmental plasticity of thermal limits across populations of a tropical reef fish**

**Abstract** 

The rapid rate of climate change is shifting environmental conditions towards species thermal limits, at a pace that many species cannot match via genetic adaptation. Phenotypic plasticity can provide species with a rapid response to buffer the negative impacts of warming temperatures; however, not all populations have the same potential for plasticity. Here, we investigate intraspecific variation in phenotypic plasticity in a tropical reef fish, the spiny chromis damselfish (Acanthochromis polyacanthus), to understand how low and high latitude populations differ in their response to ocean warming. To test plastic potential in low and high latitude populations, juveniles were exposed to developmental temperatures of 28.5°C, 30°C, and 31.5°C, and investigated for differences in morphology, critical thermal maximum, and oxygen uptake. At warmer temperatures fish grew to smaller sizes and displayed only marginal increases in critical thermal maximums (+0.16°C). No differences were observed in critical temperatures (representing a transition where cellular damage starts to outpace cellular repair) between developmental treatments or latitudes. Critical temperature thresholds (~33.92°C) were identified as being ~3°C below critical thermal maximums, and only ~2.19°C above recent heatwave events on the Great Barrier Reef. Thermal death time models found a >17-day survival advantage at 33°C, and ~1.5-day advantage at 34°C, for fish that developed at 31.5°C from both latitudes, revealing potential beneficial plasticity. These results demonstrate that low and high latitude populations are expected to have similar capacities to respond to future warming, that are limited in their ability to match the pace of climate change through phenotypic plasticity.  

**Experimental design** 

Within this study adult spiny chromis damselfish (_Acanthochromis polyacanthus_) were held at 28.5°C during the breeding season. Upon hatching, juveniles (from each clutch) were divided into 6 replicates of 20 individuals; 2 replicates were placed at each developmental temperature treatment, 28.5°C, 30.0°C (+1.5°C), and 31.5°C (+3.0°C).Morphology (length and mass), CTmax, and CTmax/oxygen uptake (i.e., CTmax/resp) were measured to determine differences in thermal tolerance between different experimental groups. 

**Statistical analysis**

In depth statistical analysis can be viewed by downloading either .html (recommended for viewing) or .Rmd (recommended for replication of results), additionally, if you wish to load models that were used for results these can be found in the associated trait *_files folder. Below data analysis is presented to run the analysis in Rscript format, however, only essential code is included. The full data analysis that includes steps such as exploratory data anaysis, outlier investigation, descriptive statistics, model selection and validation, and summary figures, can be found in .html or .Rmd files. 

# Growth analysis (Length) 

```{r}
library(modelsummary) # descriptive statistics 
library(tidyverse) # data manipulation
library(ggpubr) # figure arrangement 
library(brms) # Bayesian models
library(StanHeaders)# needed to run Bayesian models
library(rstan) # needed to run Bayesian models
library(standist) # needs to be installed 
library(bayesplot) # needed for MCMC diagnostics 
library(DHARMa) # model validation 
library(ggdist) # partial plots 
library(tidybayes) # partial plots 
library(broom.mixed) # model investigation
library(emmeans) # pairwise comparisons
library(rstanarm) # pairwise comparisons - need for emmeans  

growth <- read_delim("import_files/growth_data.txt", 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(NOTES = col_skip(), 
        ...16 = col_skip(), ...17 = col_skip()), 
    trim_ws = TRUE) 

clutch_data <- read_delim("import_files/clutch_data_2022_2023.txt", 
    delim = "\t", escape_double = FALSE, 
    trim_ws = TRUE) %>% 
  mutate(CLUTCH_NUMBER = as.factor(CLUTCH_NUMBER))

  density <- count(growth, CLUTCH_NUMBER, TANK) |> 
  rename(DENSITY = n) |> 
  mutate(CLUTCH_NUMBER = as.factor(CLUTCH_NUMBER), 
         TANK = as.factor(TANK)) 
growth2 <- growth |> 
  mutate(CLUTCH_NUMBER = as.factor(CLUTCH_NUMBER), 
         MALE = as.factor(MALE), 
         FEMALE = as.factor(FEMALE), 
         REGION = as.factor(REGION), 
         POPULATION = as.factor(POPULATION), 
         DATE_OF_HATCH = as.Date(DATE_OF_HATCH, format = "%d/%m/%Y"), 
         DATE_SAMPLED = as.Date(DATE_SAMPLED, format = "%d/%m/%Y"), 
         DEV_TEMP = as.factor(DEV_TEMP), 
         TANK = as.factor(TANK), 
         REP = as.factor(REP), 
         LENGTH = as.numeric(LENGTH), 
         MASS = as.numeric(MASS), 
         FULTONK = (1000*MASS)/(LENGTH^3), # Adding FULTON'S K metric
         EXPERIMENT = as.factor(EXPERIMENT)) |> 
  full_join(select(clutch_data, c("CLUTCH_NUMBER",
                                   "MALE_STANDARD_LENGTH", 
                                   "MALE_MASS", 
                                   "MALE_LAT", 
                                   "MALE_LONG", 
                                   "FEMALE_STANDARD_LENGTH", 
                                   "FEMALE_MASS", 
                                   "FEMALE_LAT", 
                                   "FEMALE_LONG", 
                                   "CLUTCH_ORDER", 
                                   "DAYS_IN_TREATMENT", 
                                   "EGG_COUNT", 
                                   "HATCHING_SUCCESS")), by = "CLUTCH_NUMBER") |> 
  select(c(1:6,16:27,7:13,15,14)) |> 
  drop_na(EXPERIMENT) |>
  mutate(CLUTCH_ORDER = as.factor(CLUTCH_ORDER), 
         EXP_GROUP = as.factor(paste0(REGION,"_",DEV_TEMP))) |> 
  inner_join(density, by=c("CLUTCH_NUMBER","TANK")) |> 
  mutate(TANK = as.numeric(as.character(TANK)),
         LEVEL = as.factor(case_when(TANK >= 1 & TANK <= 199 ~ 1,
                           TANK >= 200 & TANK <= 299 ~ 2,
                           TANK >= 300 & TANK <= 399 ~ 3,
                           TRUE ~ NA_real_))) |> 
  mutate(TANK = as.factor(TANK)) 

growth3 <- growth2 |> 
  filter(MASS < 4, na.rm=TRUE, 
         FULTONK < 0.079, 
         FULTONK > 0.01, 
         DENSITY > 3) 

model1.formula <- bf(LENGTH ~ DEV_TEMP*REGION + 
                      scale(FEMALE_STANDARD_LENGTH, center=TRUE, scale=TRUE) + 
                      scale(DENSITY, center=TRUE, scale=TRUE) + 
                      scale(AGE_DAYS, center=TRUE, scale=TRUE)+ 
                      (1|LEVEL/TANK) + (1|POPULATION) + (1|FEMALE) + (1|CLUTCH_ORDER),
              family=gaussian()) 
              
model1 <- brm(model1.formula,
              data = growth3, 
              prior = growth.priors,
              warmup = 500, 
              iter = 5000,
              seed=123, 
              cores=2, 
              save_pars = save_pars(all=TRUE),  
              sample_prior = "yes",
              chains = 2, 
              thin = 5, 
              control = list(adapt_delta=0.95)) 

summary(model1) 
model1 |> ref_grid() |> joint_tests()
model1 |> bayes_R2(summary = FALSE) |> median_hdci()
tidyMCMC(model1, estimate.method='median', conf.int=TRUE, conf.method='HPDinterval')
model1 |> gather_draws(`b_.*|sigma`, regex =TRUE) |> 
  median_hdci()
model1 |> mcmc_plot(type='intervals')

out <- model1 |> emmeans(pairwise ~ REGION*DEV_TEMP, type="response") 
out$emmeans |>
  as.data.frame() |>
  dplyr::mutate(across(where(is.numeric), ~ formatC(.x, digits = 2, format = "f")))

model1 |> emmeans(pairwise ~ REGION*DEV_TEMP, type="response") |> pairs(by="DEV_TEMP") |> summary()
model1 |> emmeans(pairwise ~ DEV_TEMP, type="response") |> summary()
model1 |> emmeans(pairwise ~ REGION*DEV_TEMP, type="response") |> pairs(by="REGION") |> summary()

mtsqst <- model1 |> emmeans(pairwise ~ REGION*DEV_TEMP)
mtsqrt2 <- mtsqst$contrasts |> gather_emmeans_draws()
mtsqrt2 %>% group_by(contrast) %>% dplyr::summarise(Prob = sum(.value>0)/n())
```

# Mass
