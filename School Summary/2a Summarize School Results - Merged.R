require(dplyr)
options(stringsAsFactors=F)

# Purpose: Calculate weighted averages & comp group averages.

sacred <- "C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data"
modules <- "C:/Dropbox (UP)/UP-Data Evaluation/Modules"

df <- read.csv(paste(modules, "ANet School Level/Tables/ANet and MCAS merged.csv", sep = "/"))

comp.group <- read.csv(paste(sacred, "Crosswalks and Groups/ANet Comp Group.csv", sep="/"))
comp.group <- comp.group[!is.na(comp.group$Long.Code), ]

names  <- read.csv(paste(sacred,"Crosswalks and Groups/ANet Name Matching.csv", sep="/"))
shortnames <- read.csv(paste(sacred,"Crosswalks and Groups/Short Common School Names.csv", sep="/"))

# df <- df[df$grade > 2, ]

unpo <- c(1490049, 1490090, 4800405, 35050405, 35016)
# 1490049 - UP Academy Oliver
# 1490090 - UP Academy Leonard
# 4800405 - UP Academy
# 35050405 - UP Academy Charter School of Dorchester

# s - schoool, i - interim, o - overall

# Un-comment the next line for UAO ONLY
# df <- df[df$grade == 6 , ]

# Calculate weighted averages by school

# By school by grade by interim
by.s.i.g <- df %>% 
  select(State.ID, subject, grade, perc, stu, interim, gspan, AP.perc, Stu.N) %>%
  rename( m.AP.perc = AP.perc, m.stu = Stu.N) %>%
  group_by(subject, grade, interim) %>%
  mutate(
      rank.inv = rank(perc, na.last=NA, ties.method="max"),
      n = max(rank.inv),
      rank = n - rank.inv + 1,
      m.rank.inv = rank(m.AP.perc, na.last="keep", ties.method="max"),
      m.n = max(m.rank.inv, na.rm = T),
      m.rank = m.n - m.rank.inv + 1)

by.s.i.g$tot.interim <- NA


# By school by interim
by.s.i <- df %>%
  group_by(State.ID, subject, gspan, interim) %>%
  mutate(a.tot.stu = sum(stu, na.rm = T),
         a.perc.stu = stu / a.tot.stu,
         a.perc.w = perc*a.perc.stu,
         m.tot.stu = sum(Stu.N, na.rm = T),
         m.perc.stu = Stu.N / m.tot.stu,
         m.perc.w = AP.perc*m.perc.stu) %>%
  summarize(perc = sum(a.perc.w, na.rm = T),
            stu  = max(a.tot.stu),
            m.AP.perc = sum(m.perc.w, na.rm = T),
            m.stu = max(m.tot.stu) )

by.s.i$m.AP.perc <- ifelse(by.s.i$m.AP.perc == 0, NA, by.s.i$m.AP.perc)
  
by.s.i <- by.s.i %>% 
  group_by(subject, gspan, interim) %>%
  mutate(rank.inv = rank(perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1)

# By school by grade
by.s.g <- df %>%
  group_by(State.ID, subject, grade) %>%
  mutate(a.tot.stu = sum(stu, na.rm = T),
         a.perc.stu = stu / a.tot.stu,
         a.perc.w = perc*a.perc.stu) %>%
  summarize(perc = sum(a.perc.w, na.rm = T),
            stu  = max(a.tot.stu),
            m.AP.perc = max(AP.perc),
            m.stu = max(Stu.N),
            tot.interim = length(interim))

by.s.g$m.AP.perc <- ifelse(by.s.g$m.AP.perc == 0, NA, by.s.g$m.AP.perc)

by.s.g <- by.s.g %>%
  group_by(subject, grade) %>%
  mutate(rank.inv = rank(perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1,
         m.rank.inv = rank(m.AP.perc, na.last="keep", ties.method="max"),
         m.n = max(m.rank.inv, na.rm = T),
         m.rank = m.n - m.rank.inv + 1)

by.s.g$gspan <- ifelse(by.s.g$grade<6, "Elem", "Middle")
by.s.g$interim = "All"

# By school
by.s <- by.s.i %>%
  group_by(State.ID, subject, gspan) %>%
  mutate(a.tot.stu  = sum(stu, na.rm = T),
         a.perc.stu = stu / a.tot.stu,
         a.perc.w   = perc*a.perc.stu,
         m.tot.stu  = sum(m.stu),
         m.perc.stu = m.stu / m.tot.stu,
         m.perc.w   = m.AP.perc*m.perc.stu) %>%
  summarize(perc = sum(a.perc.w, na.rm = T),
            stu  = max(a.tot.stu),
            m.AP.perc = sum(m.perc.w, na.rm = T),
            m.stu = max(m.tot.stu),
            tot.interim = length(interim) )

by.s$m.AP.perc <- ifelse(by.s$m.AP.perc == 0, NA, by.s$m.AP.perc)

by.s <- by.s %>%
  group_by(subject, gspan) %>%
  mutate(rank.inv = rank(perc, na.last=NA, ties.method="max"),
         n = max(rank.inv),
         rank = n - rank.inv + 1,
         m.rank.inv = rank(m.AP.perc, na.last="keep", ties.method="max"),
         m.n = max(m.rank.inv, na.rm = T),
         m.rank = m.n - m.rank.inv + 1)
 

  # Comp schools
    # Average by interim and overall

comp.i.g <- by.s.i.g %>%
  filter( State.ID %in% comp.group$Long.Code ) %>%
  group_by(subject, grade, interim) %>%
  summarize(perc = mean(perc, na.rm=T),
            rank = median(rank, na.rm=T),
            m.AP.perc = mean(m.AP.perc, na.rm=T),
            n = length(State.ID) )

comp.i.g$m.n <- NA
comp.i.g$m.rank <- NA
comp.i.g$gspan <- ifelse(comp.i.g$grade<6, "Elem", "Middle")


comp.i <-  by.s.i %>%
  filter( State.ID %in% comp.group$Long.Code ) %>%
  group_by(subject, gspan, interim) %>%
  summarize(perc = mean(perc, na.rm=T),
            rank = median(rank, na.rm=T),
            m.AP.perc = mean(m.AP.perc, na.rm=T),
            n = length(State.ID) )

comp.i$grade <- "All"
comp.i$m.n <- NA
comp.i$m.rank <- NA


comp.g <- by.s.g %>%
  filter(State.ID %in% comp.group$Long.Code ) %>%
  group_by(subject, grade) %>% 
  summarize(perc = mean(perc, na.rm=T),
            rank = median(rank, na.rm=T),
            m.n = sum(!is.na(m.AP.perc)),
            m.AP.perc = mean(m.AP.perc, na.rm=T),
            m.rank = median(m.rank, na.rm=T),
            n = length(State.ID) )

comp.g$gspan <- ifelse(comp.g$grade<6, "Elem", "Middle")
comp.g$interim <- "All"


comp.o <- by.s %>%
  filter(State.ID %in% comp.group$Long.Code ) %>%  
  group_by(subject, gspan) %>%
  summarize(perc = mean(perc),
            rank = median(rank),
            m.n = sum(!is.na(m.AP.perc)),
            m.AP.perc = mean(m.AP.perc, na.rm=T),
            m.rank = median(m.rank, na.rm=T),
            n = length(State.ID) )

comp.o$grade <- "All"
comp.o$interim <- "All"

comp.all <- rbind(comp.i.g, comp.g, comp.i, comp.o)
comp.all$State.ID <- 999999
comp.all$rank.inv <- NA
comp.all$stu <- NA
comp.all$m.stu <- NA
comp.all$tot.interim <- NA
comp.all$m.rank.inv <- NA


# Our schools

upbysgi <- by.s.i.g[ by.s.i.g$State.ID %in% unpo, ]
upbyi <- by.s.i[ by.s.i$State.ID %in% unpo, ]
upbyg <- by.s.g[ by.s.g$State.ID %in% unpo, ]
upbys <- by.s[ by.s$State.ID %in% unpo, ]

upbyi$grade <- "All"
upbyi$m.n <- NA
upbyi$m.rank <- NA
upbyi$m.rank.inv <- NA
upbyi$tot.interim <- NA

upbys$interim <- "All"
upbys$grade <- "All"

up.all <- rbind(upbysgi, upbyg, upbyi, upbys)

up.to.comp <- rbind(up.all, comp.all)
up.to.comp <- merge(up.to.comp, shortnames, by=c("State.ID"), all.x=T)
up.to.comp <- up.to.comp[with(up.to.comp,order(State.ID, subject, grade, interim)), ]

write.csv(up.to.comp, file = "UP and Comp Group with Averages wide.csv", row.names = F, na = "")

by.s <- merge(by.s, names, by = c("State.ID"), all.x=T)
by.s.g <- merge(by.s.g, names, by = c("State.ID"), all.x=T)
by.s.i <- merge(by.s.i, names, by = c("State.ID"), all.x=T)

write.csv(by.s, file = "MCAS and ANet by School.csv", row.names = F)
write.csv(by.s.g, file = "MCAS and ANet by School by Grade.csv", row.names = F)
write.csv(by.s.i, file = "MCAS and ANet by School by Interim.csv", row.names = F)
