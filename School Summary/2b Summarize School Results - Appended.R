require(dplyr)
options(stringsAsFactors=F)

# Purpose: Calculate weighted averages & comp group averages.

modules <- "C:/Dropbox (UP)/UP-Data Evaluation/Modules/ANet School Level/Tables/"

setwd(modules)

df <- read.csv("ANet and MCAS appended.csv")

anet <- read.csv("ANet and MCAS merged.csv")


# Filter out lower grades from ANet and schools that don't have ANet
df <- df %>% filter(grade > 2, State.ID %in% anet$State.ID) %>% select(-gspan)

df$grade <- as.character(df$grade)

# Note: codes are abbreviated as below
# s - schoool, i - interim, o - overall


# Calculate hypothetical comparable school
comp.i.g <- df %>%
  filter( comp == TRUE ) %>%
  group_by(subject, grade, test, interim) %>%
  summarize(tot.stu = sum(stu, na.rm = T) ,
            tot.perc = sum( perc*stu / tot.stu, na.rm = T ) ,
            tot.diff = sum(diff_to_network*stu / tot.stu, na.rm = T) )


comp.i.g$Anet.School.Id <- 99999
comp.i.g$State.ID <- 99999
comp.i.g$school <- "Comparable Group Ave"
comp.i.g$comp <- NA
comp.i.g$CPI <- NA
comp.i.g$SGP <- NA
comp.i.g$SGP.N <- NA
comp.i.g <- comp.i.g %>% rename(perc = tot.perc, stu = tot.stu, diff_to_network = tot.diff)

df <- rbind(df, comp.i.g)

# Calculate weighted averages by school

# By school by grade by interim
by.s.i.g <- df %>% 
  filter(test == "ANet") %>%
  group_by(subject, grade, interim) %>%
  mutate(
      rank.inv = rank(perc, na.last=NA, ties.method="max"),
      n = max(rank.inv),
      rank = n - rank.inv + 1,
      ptile = cume_dist(perc))


# By school by interim
by.s.i <- df %>%
  filter(test == "ANet") %>%
  group_by(State.ID, subject, interim) %>%
  summarize(tot.stu = sum(stu, na.rm = T) ,
            tot.perc = sum( perc*stu / tot.stu, na.rm = T ),
            tot.diff = sum( diff_to_network*stu / tot.stu, na.rm = T ))

by.s.i <- by.s.i %>% 
  group_by(subject, interim) %>%
  mutate(rank.inv = rank(tot.perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1,
         ptile = cume_dist(tot.perc))


# By school by grade
by.s.g <- df %>%
  group_by(State.ID, subject, test, grade) %>%
  summarize(tot.stu = sum(stu, na.rm = T) ,
            tot.perc = sum( perc*stu / tot.stu, na.rm = T ),
            tot.diff = sum( diff_to_network*stu / tot.stu, na.rm = T ))

by.s.g <- by.s.g %>% 
  group_by(subject, test, grade) %>%
  mutate(rank.inv = rank(tot.perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1,
         ptile = cume_dist(tot.perc))


# By school
by.s <- df %>%
  group_by(State.ID, subject, test) %>%
  summarize(tot.stu = sum(stu, na.rm = T) ,
            tot.perc = sum( perc*stu / tot.stu, na.rm = T ),
            tot.diff = sum( diff_to_network*stu / tot.stu, na.rm = T ))

by.s <- by.s %>% 
  group_by(subject, test) %>%
  mutate(rank.inv = rank(tot.perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1,
         ptile = cume_dist(tot.perc))



# Add all the empty columns we need to re-append
by.s.i.g <- by.s.i.g %>% select(-Anet.School.Id, -school, -comp, -CPI,
                                -SGP, -SGP.N)

by.s.i$test <- "ANet"
by.s.i$grade <- "All"
by.s.i <- rename(by.s.i, stu = tot.stu, perc = tot.perc, diff_to_network = tot.diff)

by.s.g$interim = ifelse(by.s.g$test == "MCAS", "MCAS 2014", "All")
by.s.g <- rename(by.s.g, stu = tot.stu, perc = tot.perc, diff_to_network = tot.diff)

by.s$grade = "All"
by.s$interim = ifelse(by.s$test == "MCAS", "MCAS 2014", "All")
by.s <- rename(by.s, stu = tot.stu, perc = tot.perc, diff_to_network = tot.diff)

by.s.i.g$grade <- as.character(by.s.i.g$grade)
by.s.g$grade <- as.character(by.s.g$grade)

# Append and Export Results
out <- rbind(by.s.i.g, by.s.i, by.s.g, by.s)

anet.t <- anet %>% select(State.ID, school) %>% filter( !duplicated(State.ID))

out <- left_join(out, anet.t, by = "State.ID")

out[out$State.ID == 99999, "school"] <- "Comparable Group"

out[out$test == "MCAS", "diff_to_network"] <- NA

write.csv(out, file = "MCAS and ANet with Totals.csv", row.names = F, na = "")
   


unpo <- c(1490049, 1490090, 4800405, 35050405, 350167, 99999)
# 1490049 - UP Academy Oliver
# 1490090 - UP Academy Leonard
# 4800405 - UP Academy Boston
# 35050405 - UP Academy Dorchester
# 350167 - UP Academy Holland
# 99999 - Comparable Group

out <- out %>% filter( State.ID %in% unpo )

write.csv(out, "UP and Comp Group with Averages.csv", row.names = F, na = "")

