# Use the summarized school results to calculate MCAS predictions


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

