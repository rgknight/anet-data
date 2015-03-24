# Take the collected ANet data and merge in State accountability data
# intended to be run after 1a

require(dplyr)
require(tidyr)
options(stringsAsFactors=F)

setwd("C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data")

sacred.source <- "C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data/"
mcas.src <- "C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data/"

fy14 <- read.csv("ANet/FY14/School Results/Anet All Schools All Grades.csv")
fy15 <- read.csv("ANet/FY15/School Results/Anet All Schools All Grades.csv")

comp <- read.csv("Crosswalks and Groups/ANet Comp Group.csv")
anet.names <- read.csv("Crosswalks and Groups/ANet Name Matching.csv")

m <- read.csv("MCAS/Merged/All School All Stu By Grade.csv")


# Merge in codes from anet
fy15 <- left_join(fy15,  anet.names %>%
                  filter(!is.na(Anet.Name) & !duplicated(Anet.School.Id) ) %>%
                  select(-Anet.Name, -Notes), by = c("Anet.School.Id" = "Anet.School.Id" ))

fy14 <- left_join(fy14, anet.names %>% 
                    select(-Notes, school = Anet.Name) %>%
                    filter(!is.na(school)))

fy15$year <- "FY15"
fy14$year <- "FY14"

f <- rbind(fy15, fy14)

f$gspan <- ifelse(f$grade<5, "Elem", "Middle")

# Add comp group indicator
comp <- comp[!is.na(comp$Long.Code),]
f$comp <- f$State.ID %in% comp$Long.Code

# Add incorrect codes ( Dever McCormack is 2 schools in MCAS & 1 in ANet)
f$State.ID <- with(f, ifelse( school %in% c("Dever McCormack School - BPS", "Dever McCormack School"),
                      ifelse( gspan == "Elem", 350268, 350179), State.ID))
table(f[is.na(f$State.ID), "school"])

f <- f %>% filter(!is.na(State.ID))

# Prepare  MCAS Data
m <- m[m$Year %in% c(2014, 2013), ]
m$year <-with(m, ifelse( Year == 2014, "FY14", "FY13"))

# Replace the Marshall as being UAD, and the Oliver as UAO (no longer necessary)
m[m$Org.Code==350178 , "Org.Code"] <- 35050405
m[m$Org.Code==1490050, "Org.Code"] <- 1490049
m[m$Org.Code==1490045, "Org.Code"] <- 1490090

m <- rename(m ,  State.ID = Org.Code, subject = Subject, grade = Grade)

m$subject <- ifelse(m$subject=="MTH", "Math", m$subject)


# Append MCAS 
m <- m %>% select(State.ID, subject, grade, 
                  school = Org.Name,
                  perc = AP.perc, 
                  stu = Stu.N,
                  CPI, SGP, SGP.N, year)

m$Anet.School.Id <- NA
m$diff_to_network <- NA
m$comp <- NA
m$gspan <- ifelse(m$grade<5, "Elem", "Middle")
m$interim <- ifelse(m$year == "FY14", "MCAS 2014","MCAS 2013")
m$test <- "MCAS"
  
f$CPI <- NA
f$SGP <- NA
f$SGP.N <- NA
f$test <- "ANet"

anet.append <- rbind(f, m)

anet.append$comp <- anet.append$State.ID %in% comp$Long.Code

write.csv(anet.append, "ANet/Tables/ANet and MCAS.csv", row.names=F, na="")


# Schools who don't have codes:
head(f[is.na(f$State.ID), ])

