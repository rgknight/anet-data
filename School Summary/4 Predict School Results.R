# Use the summarized school results to calculate MCAS predictions
options(stringsAsFactors=F)
require(tidyr)
require(ggplot2)

df <- read.csv("C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data/ANet/Tables/ANet and MCAS with Totals.csv")

mcas <- df %>% filter(test =="MCAS") %>% 
  select(State.ID, school, subject, grade, year, 
         mcas_ptile = ptile, ap = perc)

ela <- df %>% filter(subject == "ELA")
math <- df %>% filter(subject == "Math")


thisone <- ela %>% filter(grade == 6, !iterim %in% c("All", "A1", "A2")) %>%
  unite(year_test, year, interim)

p <- ggplot(thisone, aes(x=year_test, y=ptile, group=school))
p + geom_line(aes(colour = school))


yearlong <- ela %>% 
  unite(id, school, grade, subject, year) %>%
  mutate(interim = ifelse(test == "MCAS", "MCAS", interim)) %>%
  select(id, interim, perc, ptile) %>%
  gather(outcome, value, -id, -interim) %>%
  unite(interim_outcome, interim , outcome, remove = T) %>%
  spread(interim_outcome, value) %>%
  separate(id, c("school", "grade", "subject", "year"), "_")

mcas_ptil_ap <- ggplot(yearlong, aes(x=MCAS_ptile, y=MCAS_perc))
mcas_ptil_ap + geom_point(aes(colour = grade))


anet_mcas <- left_join(math %>% filter(test != "MCAS"), 
                       mcas %>% filter(subject == "Math"))

a3ptile_mcasptile <- ggplot(anet_mcas %>% filter(interim == "A3"), aes(x=ptile, y=ap))
a3ptile_mcasptile + geom_point(aes(colour = grade)) + geom_smooth(method = "lm")

# ways to do this

# Develop a prediction model 
# Outcome - 2014 MCAS
# Covariates
  # 2013 MCAS
  # 2014 ANet, A1-A3, also A1-A2 for ELA
  # Grade
  # Grade x ANet
  # UP x All

# What to do about the fact that results are likely correlated within a school?
  # Ignore it... there's probably some lost information there, but I don't 
  # know what to do about it

library(rpart)
library(randomForest)
library(dplyr)
library(nnet)

up.schools <- c(1490049, 1490090, 4800405, 35050405, 350167, 99999)

df.anal <- df %>% 
  group_by(grade, interim, subject) %>%
  filter(interim != "All", !(grade %in% c("All", "2") %>%
  select(-diff_to_network, -n, -rank) %>%
  mutate(perc_scale = scale(perc)) %>%
  gather(measure, value, perc, perc_scale) %>%
  unite(interim_measure, interim, measure) %>%
  spread(interim_measure, value) %>%
  mutate(over75 = ifelse("MCAS 2014" >= 75, 1, 0),
         up = ifelse(State.ID %in% up.schools, 1, 0))

train <- sample()

# Calculate MCAS predictions
#mcas.src <- "C:/Dropbox (UP)/UP-Data Evaluation/UP Data Sources/Sacred Data/MCAS/FY13/MCAS 2013 Merged.csv"

#mcas <- read.csv( mcas.src )

#mcas$comp <- mcas$Org.Code %in% comp$State.ID

# Plot the performance


# Elementary ELA

elem.ela <- by.s[ by.s$gspan=="Elem" & by.s$subject == "ELA", ]

model <- lm(m.pa.perc ~ perc, data = elem.ela)
summary(model)

elem.ela$yhat <- predict(model, elem.ela)

model2 <- lm(m.pa.perc ~ rank, data = elem.ela)
summary(model)
elem.ela$yhatrank <- predict(model2, elem.ela)


elem.ela[elem.ela$State.ID == 35050405, ]

qplot(perc, m.pa.perc , data=elem.ela, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "ANet Average % Correct", yscale=c(0,80),
      ylab = "2013 MCAS % Advanced-Proficient", main = "Elementary ELA ANet versus MCAS") + 
  geom_segment(aes(x = 43.9, y = 2, xend = 43.9, yend = 29.3)) + 
  geom_segment(aes(x = 30, y = 29.3, xend = 43.9, yend = 29.3)) +
  annotate("text", x=30, y = 32, label = "Predicted MCAS = 29%", hjust=0) +
  annotate("text", x=43.9, y = 0, label = "UAD ANet = 44%") + 
  scale_y_continuous(breaks = seq(0,80,20))


# Elementary Math
df.p <- by.s[ by.s$gspan=="Elem" & by.s$subject == "Math", ]

model <- lm(m.pa.perc ~ perc, data = df.p)
summary(model)

df.p$yhat <- predict(model, df.p)

model2 <- lm(m.pa.perc ~ rank, data = df.p)
summary(model)
df.p$yhatrank <- predict(model2, df.p)


df.p[df.p$State.ID == 35050405, ]

qplot(perc, m.pa.perc , data=df.p, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "ANet Average % Correct",
      ylab = "2013 MCAS % Advanced-Proficient", main = "Elementary Math ANet versus MCAS") + 
  geom_segment(aes(x = 46.3, y = 2, xend = 46.3, yend = 39.1)) + 
  geom_segment(aes(x = 35, y = 39.1, xend = 46.3, yend = 39.1)) +
  annotate("text", x=35, y = 42, label = "Predicted MCAS = 39%", hjust=0) +
  annotate("text", x=46.3, y = 0, label = "UAD ANet = 46%") + 
  scale_y_continuous(breaks = seq(0,80,20))

# Middle School Math
df.p <- by.s[ by.s$gspan=="Middle" & by.s$subject == "Math", ]

model <- lm(m.pa.perc ~ perc, data = df.p)
summary(model)

df.p$yhat <- predict(model, df.p)

model2 <- lm(m.pa.perc ~ rank, data = df.p)
summary(model)
df.p$yhatrank <- predict(model2, df.p)


df.p[df.p$State.ID %in% c(1490049, 1490090, 4800405), ]

xscore <- 52.1
yscore <- 43.6

qplot(perc, m.pa.perc , data=df.p, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "ANet Average % Correct",
      ylab = "2013 MCAS % Advanced-Proficient", main = "Middle ELA ANet versus MCAS") + 
  geom_segment(aes(x = xscore, y = 2, xend = xscore, yend = yscore)) + 
  geom_segment(aes(x = 35, y = yscore, xend = xscore, yend = yscore)) +
  annotate("text", x=35, y = yscore + 2, label = paste("Predicted MCAS = ", yscore, "%", sep=""), hjust=0) +
  annotate("text", x=xscore, y = 0, label = paste("ANet = ", xscore, "%", sep="")) + 
  scale_y_continuous(breaks = seq(0,80,20))

# Middle School ELA
df.p <- by.s[ by.s$gspan=="Middle" & by.s$subject == "ELA", ]

model <- lm(m.pa.perc ~ perc, data = df.p)
summary(model)

df.p$yhat <- predict(model, df.p)

model2 <- lm(m.pa.perc ~ rank, data = df.p)
summary(model)
df.p$yhatrank <- predict(model2, df.p)


df.p[df.p$State.ID %in% c(1490049, 1490090, 4800405), ]

xscore <- 43.4
yscore <- 31.4

qplot(perc, m.pa.perc , data=df.p, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "ANet Average % Correct",
      ylab = "2013 MCAS % Advanced-Proficient", main = "Middle Math ANet versus MCAS") + 
  geom_segment(aes(x = xscore, y = 2, xend = xscore, yend = yscore)) + 
  geom_segment(aes(x = 35, y = yscore, xend = xscore, yend = yscore)) +
  annotate("text", x=35, y = yscore + 2, label = paste("Predicted MCAS = ", yscore, "%", sep=""), hjust=0) +
  annotate("text", x=xscore, y = 0, label = paste("ANet = ", xscore, "%", sep="")) + 
  scale_y_continuous(breaks = seq(0,80,20))

# Level 4 schools
# Average by interim and overall
# Average MCAS
# Number in group by interim
# Median rank by interim and overall

