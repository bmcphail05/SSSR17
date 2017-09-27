
rm(list = ls(all = TRUE))

knitr::opts_chunk$set(echo = TRUE)
library(plyr)
library(tidyverse)
library(Gmisc)
library(stargazer)
library(texreg)
library(tables)
library(captioner)
library(scales)
library(waffle)

setwd("~/Google Drive/Purdue Coursework/SOC581_Stats/581Final")

# Load data into R
dat <- read.table("~/Dropbox/Data/SLS2007/SLS2007.csv", header = TRUE, sep = ",")

# Dummy Variables for each response item of BELIEVE1
dat$BELIEVE1<- as.factor(dat$BELIEVE1)
dat$BELIEVE1 <- revalue(dat$BELIEVE1, c('74'=NA, '98'=NA, '99'=NA))     
dat <- within(dat, BELIEVE1 <- relevel(BELIEVE1, ref = '97'))

# Variable for Religious Affiliation Yes/No
dat$BELIEVE <- as.factor(dat$BELIEVE1)
dat$BELIEVE <- revalue(dat$BELIEVE, c("1"=1, "2"=1, "3"=1, "4"=1, "5"=1, "6"=1,
                                       "74"=1, "97"=0, "98"=NA, "99"=NA))
dat$BELIEVE <- as.character(dat$BELIEVE)
dat$BELIEVE <- as.numeric(dat$BELIEVE)


dat$RELIGBLF <- as.factor(dat$RELIGBLF)
dat$RELIGBLF<- revalue(dat$RELIGBLF, c('1'=1, '2'=0, '8'=NA, '9'=NA))
dat$RELIGBLF <- as.character(dat$RELIGBLF)
dat$RELIGBLF <- as.numeric(dat$RELIGBLF)

# Create Pure Atheist Variable
dat$ATHEISTS <- 1
dat$ATHEISTS[dat$RELIGBLF != "0"] <- 0
dat$ATHEISTS[dat$WORSHIP1 != 97] <- 0
dat$ATHEISTS[dat$ACTIVTY1 != 97] <- 0
dat$ATHEISTS[dat$ITEMS1 != 97] <- 0
dat$ATHEISTS[dat$WRKITMS1 != 97] <- 0
dat$ATHEISTS[dat$WEARACC1 != 97] <- 0
dat$ATHEISTS[dat$PAST12A != 97] <- 0
dat$ATHEISTS[dat$RDONE1 != 97] <- 0
dat$ATHEISTS[dat$RDONEL01 != 97] <- 0
dat$ATHEISTS[dat$EVERPRAY != 2] <- 0
dat$ATHEISTS[dat$DONATED != 2] <- 0
dat$ATHEISTS[dat$EXIST1 != 2] <- 0
dat$ATHEISTS[dat$EXIST2 != 2] <- 0
dat$ATHEISTS[dat$EXIST3 != 2] <- 0
dat$ATHEISTS[dat$EXIST4 != 2] <- 0
dat$ATHEISTS[dat$EXIST5 != 2] <- 0
dat$ATHEISTS[dat$EXIST6 != 2] <- 0
dat$ATHEISTS[dat$EXIST7 != 2] <- 0
dat$ATHEISTS[dat$EXIST8 != 2] <- 0
dat$ATHEISTS[dat$EXIST9 != 2] <- 0
dat$ATHEISTS[dat$EXIST10 != 2] <- 0
dat$ATHEISTS[dat$EXIST11 != 2] <- 0
dat$ATHEISTS[dat$EXIST12 != 2] <- 0
dat$ATHEISTS[dat$EXIST13 != 2] <- 0
dat$ATHEISTS[dat$EXIST14 != 2] <- 0
dat$ATHEISTS[dat$EXIST15 != 2] <- 0
dat$ATHEISTS[dat$EXIST16 != 2] <- 0
dat$ATHEISTS[dat$EXIST17 != 2] <- 0
dat$ATHEISTS[dat$EXIST18 != 2] <- 0
dat$ATHEISTS[dat$VIEWGODS != 3] <- 0
dat$ATHEISTS[dat$SOULEXST != 3] <- 0

dat$ATHEISTS <- as.factor(dat$ATHEISTS)
levels(dat$ATHEISTS) <- c("0", "1")
dat$ATHEISTS <- as.character(dat$ATHEISTS)
dat$ATHEISTS <- as.numeric(dat$ATHEISTS)

#Create PARTY variable
dat$POLITAFF <- as.factor(dat$POLITAFF)
dat$PARTY <-revalue(dat$POLITAFF, c("1"=1, "2"=0, "3"=1, "4"=0,
                                 "51"=1, "98"=NA, "99"=NA))
dat$PARTY <- as.character(dat$PARTY)
dat$PARTY <- as.numeric(dat$PARTY)

# Recode Education
dat$EDUC <- as.factor(dat$EDUC)
dat$EDUC1 <- revalue(dat$EDUC, c("1"=1, "2"=1, "3"=1, "4"=2, "5"=3, "6"=4,
                                        "7"=4, "8"=NA, "9"=NA))
levels(dat$EDUC1) <- c("LessHigh", "HighSchool",
                       "Vocational", "MoreCollege")

# Create college degree variable
dat$COLDEGR <- 0
dat$COLDEGR[dat$EDUC == '6'] <- 1
dat$COLDEGR[dat$EDUC == '7'] <- 1
dat$COLDEGR[dat$EDUC == '8'] <- NA
dat$COLDEGR[dat$EDUC == '9'] <- NA
dat$COLDEGR <- as.numeric(dat$COLDEGR)

# Recode Gender
dat$MALE <- as.factor(dat$GENDER)
dat$MALE <- revalue(dat$MALE, c("1"=1, "2"=0))
dat$MALE <- as.character(dat$MALE)
dat$MALE <- as.numeric(dat$MALE)

# Recode Income with mid-point of response categories (NOT USED IN THIS ANALYSIS)
dat$INCCITY <- as.factor(dat$INCCITY)
dat$INCCITY<- revalue(dat$INCCITY, c("1"= 250, "2"=750, "3"=1250, "4"= 1750, "5"=2250,
                          "6"=2750, "7"=3250, "8"=3750, "9"=4250, "10"=4750,
                          "11"=5250, "12"=5750, "13"=6250, "14"=6750, "15"=7500,
                          "16"=8500, "17"=9500, "18"=10001, "19"=0, "20"=NA, "98"=NA,
                          "99"=NA))
dat$INCCITY <- as.factor(dat$INCCITY)
dat$INCCITY <- as.character(dat$INCCITY)
dat$INCCITY <- as.numeric(dat$INCCITY)

# Create City income annual variable
dat <- mutate(dat, INCCITYann = INCCITY *12)


dat$INCRURL <- as.factor(dat$INCRURL)
dat$INCRURL <- revalue(dat$INCRURL, c("1"= 250, "2"=750, "3"=1250, "4"= 1750,"5"=2250,
                                      "6"=2750, "7"=3250, "8"=3750, "9"=4250, "10"=4750,
                                      "11"=5250, "12"=5750, "13"=6250, "14"=6750,
                                      "15"=7500, "16"=8500, "17"=9500, "18"=10001,
                                      "19"=0, "20"=NA, "98"=NA, "99"=NA))
dat$INCRURL <- as.factor(dat$INCRURL)
dat$INCRURL <- as.character(dat$INCRURL)
dat$INCRURL <- as.numeric(dat$INCRURL)

dat$pANNINCOME <- ifelse(!is.na(dat$INCRURL), dat$INCRURL, dat$INCCITYann)


# Recode household income (city income was multiplied by 12 for annual)
dat$INCHSCIT <- as.factor(dat$INCHSCIT)
dat$INCHSCITYR<- revalue(dat$INCHSCIT, c("1"=3000, "2"=9000, "3"=18000, "4"= 30000,
                                      "5"=42000, "6"=54000, "7"=66000, "8"=78000,
                                      "9"=90000, "10"=102000, "11"=114000, "12"=132000,
                                      "13"=162000, "14"=210000, "15"=240000,"16"=0,
                                      "17"=NA, "98"=NA, "99"=NA))

dat$INCHSCITYR <- as.character(dat$INCHSCITYR)
dat$INCHSCITYR <- as.numeric(dat$INCHSCITYR)

dat$INCHSRUR <- as.factor(dat$INCHSRUR)
dat$INCHSRURYR <- revalue(dat$INCHSRUR, c("1"=250, "2"=750, "3"=1500, "4"= 2500,
                                      "5"=3500, "6"=4500, "7"=5500, "8"=6500,
                                      "9"=7500, "10"=8500, "11"=9500, "12"=11000,
                                      "13"=13500, "14"=17500, "15"=22500,"16"=27500,
                                      "17"=40000, "18"=65000, "19"=90000, "20"= 100000,
                                      "21"=0, "22"=NA, "98"=NA, "99"=NA))
dat$INCHSRURYR <- as.factor(dat$INCHSRURYR)
dat$INCHSRURYR <- as.character(dat$INCHSRURYR)
dat$INCHSRURYR <- as.numeric(dat$INCHSRURYR)

# Combine City and Rural income into a single variable
dat$ANNINCOME <- ifelse(!is.na(dat$INCHSRURYR), dat$INCHSRURYR, dat$INCHSCITYR)

# Create Urban Variable (based on Income variable information)
dat$URBAN <- 0
dat$URBAN[is.na(dat$INCHSRUR)] <- 1
dat$URBAN <- as.numeric(dat$URBAN)

# Exclude non-working individuals from data set
dat$OCCCITY1 <- as.factor(dat$OCCCITY1)
dat$RURALNOW <- as.factor(dat$RURALNOW)

exclude.cityocc <- c(14, 16, 17, 18, 19, 98, 99)
exclude.rurocc <- c(11, 31, 33, 41, 68, 96, 98, 99)

dat1 <- subset(dat, !(OCCCITY1 %in% exclude.cityocc) | is.na(dat$OCCCITY1))
dat1 <- subset(dat1, !(RURALNOW %in% exclude.rurocc) | is.na(dat1$RURALNOW))

#Subset data to only include variables of interest and complete observations
vars <- c("ANNINCOME", "RELIGBLF", "URBAN", "MALE", "pANNINCOME",
          "PARTY", "COLDEGR", "BELIEVE", "ATHEISTS", "BELIEVE1", "WEIGHT")
dat1 <- dat1[vars]
dat1 <- na.omit(dat1)



# Estimate linear models of personal annual income by select variables
lm1 <- lm(pANNINCOME ~ RELIGBLF, dat1)
summary(lm1)

lm2 <- lm(pANNINCOME ~ RELIGBLF + URBAN + MALE + PARTY + COLDEGR, dat1)
summary(lm2)

lma <- lm(pANNINCOME ~ BELIEVE, dat1)
summary(lma)

lmb <- lm(pANNINCOME ~ BELIEVE + URBAN, dat1)
summary(lmb)

lmc <- lm(pANNINCOME ~ BELIEVE + URBAN + MALE, dat1)
summary(lmc)

lmd <- lm(pANNINCOME ~ BELIEVE + URBAN + MALE + PARTY, dat1)
summary(lmd)

lme <- lm(pANNINCOME ~ BELIEVE + URBAN + MALE + PARTY + COLDEGR, dat1)
summary(lme)

lm_ATHEISTS <- lm(pANNINCOME ~ ATHEISTS + URBAN + MALE + PARTY + COLDEGR, dat1)
summary(lm_ATHEISTS)

lm_ATHEISTS1 <- lm(pANNINCOME ~ ATHEISTS, dat1)
summary(lm_ATHEISTS1)

lm_BELIEVE1 <- lm(pANNINCOME ~ BELIEVE1, dat1)
summary(lm_BELIEVE1)

lm_BELIEVE1a <- lm(pANNINCOME ~ BELIEVE1 + URBAN + MALE + PARTY + COLDEGR, dat1)
summary(lm_BELIEVE1a)


# Weighted analysis
# dat1w <- svydesign(ids = ~0, weights = ~ WEIGHT, data = dat1)
# summary(dat1w)
# 
# svytable(~URBAN, design=dat1w)


#tables and figures
figs <- captioner(prefix="Figure")
tbls <- captioner(prefix="Table")

tbls(name="Desc1","Mean Personal Annual Income by Independent Variables")
tbls(name="OLS1","OLS Regression Coefficients Predicting Annual Personal Income.")


# Table 1 Descriptives

# Add labels to variable categories for table output
dat1$BELIEVE1 <- factor(dat1$BELIEVE1,
                        levels = c(97, 1, 2, 3, 4, 5, 6),
                        labels = c("None", "Buddhism", "Daoism",
                                  "Confucianism", "Protestantism", "Catholicism",
                                  "Islam"))
dat1$RELIGBLF <- factor(dat1$RELIGBLF,
                        levels = c(1, 0),
                        labels = c("Yes", "No"))

dat1$ATHEISTS <- factor(dat1$ATHEISTS,
                        levels = c(1, 0),
                        labels = c("Pure Atheism", "Other"))

dat1$URBAN <- factor(dat1$URBAN,
                     levels = c(1,0),
                     labels = c("Urban", "Rural"))

dat1$MALE <- factor(dat1$MALE,
                     levels = c(1,0),
                     labels = c("Male", "Female"))

dat1$PARTY<- factor(dat1$PARTY,
                     levels = c(1,0),
                     labels = c("Member", "Non-member"))

dat1$COLDEGR <- factor(dat1$COLDEGR,
                     levels = c(1,0),
                     labels = c("College Degree", "No College Degree"))

# Create Descriptives table
latex(tabular(Heading("Religious Belief")*(RELIGBLF) + 
                Heading("Religious Self-ID ")*(BELIEVE1) + 
                Heading("Pure Atheism")*(ATHEISTS) + 
                Heading("Location")*(URBAN) + 
                Heading("Gender")*(MALE) + 
                Heading("Communist Party")*(PARTY) + 
                Heading("Education")*(COLDEGR) + 
                1 ~ Heading("Annual Personal Income in RMB")*(pANNINCOME)*
                (Heading("N")*(n=1)+ Format(digits=1)*Percent() + 
                   Heading("Mean")*mean + Heading("Std Dev")*sd), data = dat1))


#Table 2 Coefficients
stargazer(lm1, lm_BELIEVE1, lm_ATHEISTS1, lm2, lm_BELIEVE1a, lm_ATHEISTS,
          style = 'asr',
          header=FALSE,
          single.row = FALSE,
          digits = 2,
          df = FALSE,
          omit.stat=c("f", "ser"),
          column.sep.width="2pt",
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = 
            "Table 2: OLS Regression Coefficients Predicting Annual Personal Income",
          covariate.labels = c("Religious Belief", "Buddhism", "Daoism",
                               "Confucianism", "Protestantism", "Catholicism",
                               "Islam", "Pure Atheism", "Urban", "Male",
                               "Party Member", "College Degree"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "Annual Personal Income in RMB")
          #float.env = "sidewaystable")

# Descriptive Chart


## Religious Belief Waffle
belief <- dat1 %>%
  filter(!is.na(RELIGBLF)) %>% 
  group_by(RELIGBLF) %>%
  summarise(n = n()) %>% 
  mutate(percent = round(100 * n/sum(n), 0)) %>% 
  na.omit()
                          

belief.w <- waffle(c(`No (85%)                `= 85, `Yes (15%)`= 15), rows = 5, size = 0.5,
                   title = "Religious Belief", 
                   colors=c("#969696", "#1879bf")) +
  theme(legend.text = element_text(size = 14), 
        plot.title = element_text(size = 18, face = "bold"), 
        plot.margin = margin(c(0, 0, 25, 0)))

# Religious Group Waffle

rel.group <- dat1 %>% 
  filter(!is.na(BELIEVE1)) %>% 
  group_by(BELIEVE1) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(100 * n/sum(n), 2)) %>% 
  dplyr::select(BELIEVE1, percent)

rel.group.w <- waffle(c(`None (80%)` = 80, `Buddhism (16%)` = 16, 
                        `Protestantism (2%)` = 2, `Other (1%)` = 1),
          rows = 5, size = 0.5, title = "Religious Affiliation", 
       colors=c("#969696", "yellow3", "red3", "purple")) +
  theme(legend.text = element_text(size = 14), 
        plot.title = element_text(size = 18, face = "bold"), 
        plot.margin = margin(c(0, 0, 25, 0)))

 # `Daoism (<1%)` = 0, `Confucianism (<1%)` = 0, `Islam (<1%)` = 0, 
#  `Catholicism (<1%)` = 0),
# "dodgerblue", "gold4", "green4", "purple4")
  
  
# Atheism Waffle 
atheism.w <- waffle(c(`Strict Atheism (5%)`= 5, `Other (95%)`= 95), rows = 5, size = 0.5,
                    title = "Strict Atheism", 
                    colors=c("#969696", "#1879bf")) +
  theme(
    legend.text = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold"), 
    plot.margin = margin(c(0, 0, 25, 0)))

# Combine waffles

iron(belief.w, rel.group.w, atheism.w)



# Plot Coefficients (dot whisker)
library(arm)
library(dotwhisker)
library(broom)
library(scales)

lm2_df <- tidy(lm2)
lm2_df$model <- "Model 4"
lm_BELIEVE1a_df <- tidy(lm_BELIEVE1a)
lm_BELIEVE1a_df$model <- "Model 5"
lm_ATHEISTS_df <- tidy(lm_ATHEISTS)
lm_ATHEISTS_df$model <- "Model 6"

lm_combined_df <- rbind(lm2_df, lm_BELIEVE1a_df, lm_ATHEISTS_df) 

lm_combined_df$term <- factor(lm_combined_df$term, levels = c("RELIGBLF", 
                                          "BELIEVE11","BELIEVE12","BELIEVE13",
                                          "BELIEVE14", "BELIEVE15", "BELIEVE16",
                                          "ATHEISTS", "URBAN", "MALE", "PARTY",
                                          "COLDEGR", "(Intercept)"))  

lm_combined_df <- relabel_predictors(lm_combined_df, c(RELIGBLF = "Religious Belief",                       
                     BELIEVE11 = "Buddhist", 
                     BELIEVE12 = "Daoist", 
                     BELIEVE13 = "Confucianist", 
                     BELIEVE14 = "Protestant",
                     BELIEVE15 = "Catholic", 
                     BELIEVE16 = "Muslim", 
                     ATHEISTS = "Strict Atheist", 
                     URBAN = "Urban", 
                     MALE = "Male", 
                     PARTY = "CCP Member", 
                     COLDEGR = "College Degree"))

scale_colour_manual(values=c("#124E78", "#F2BB05", "#D74E09"))

dwplot(lm_combined_df, dot_args = list(size = .8), alpha = .05) +
  theme_bw() + 
  xlab("Coefficient Estimate") + 
  ylab("") +
  scale_colour_manual(values=c("#124E78", "#F2BB05", "#D74E09")) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  coord_cartesian(xlim=range(lm_combined_df$estimate) + c(-4000, 7000)) +
  scale_x_continuous(label = dollar_format(prefix="¥"), 
                     breaks = c(-5000, -2500, 0, 2500, 5000, 
                                7500, 10000, 12500, 15000, 17500)) +
  ggtitle("Predicting Annual Personal Income") +
     theme(plot.title = element_text(size = 18, face="bold"),
           legend.justification=c(1, 0), 
           legend.position=c(1, 0),
           legend.background = element_rect(colour="black", size = .2),
           legend.title = element_blank(),
           legend.text = element_text(size = 12),
           panel.grid.minor.x = element_blank(), 
           panel.border = element_blank(),
           axis.title.x = element_text(size = 14, margin = unit(c(5, 0, 0, 0), "mm")), 
           axis.text = element_text(size = 12)) 




