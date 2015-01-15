# Reshape ANet student summary data into long form
options(stringsAsFactors = F)
require(dplyr)
require(tidyr)
require(stringr)

sacred.loc <- "C:/Dropbox (UP Education Network)/UP-Data Evaluation/Sacred Data"

setwd(sacred.loc)

# Expects data to come in two files for ela and two files for math,
# one for for K-5 and one for 6-8.
# This may need editing if ANet changes their export format.

ela

# Note -- change this for A3. We might want to append from ANet website data
ela  <- read.csv("Anet/FY15/Student Summary/InterimScores_ELA_A1A2_AllGrades.csv")
math <- read.csv("Anet/FY15/Student Summary/InterimScores_Math_A1A2_AllGrades.csv")

ela$test <- "ELA"
math$test <- "Math"

# Remove the "ELA" and "Math" prefixes from the variable names
remove_pre <- function(db, remover) {
  db.extra <- select(db, starts_with(remover), Student.ANET.ID)
  db.extra <- gather(db.extra, measure, value, starts_with(remover))
  db.extra$measure <- str_replace(db.extra$measure, remover, "")
  
  db.wide <- spread(db.extra, measure, value)
  db.small <- select(db, -starts_with(remover))
  left_join(db.small, db.wide)
}

ela2 <- remove_pre(ela, "ELA")

math2 <- remove_pre(math, "Math")

all <- rbind(ela2, math2)

# Reshape to long. First, gather to super-long, then spread the points-possible points

a1.colnum <- grep("A1 Raw Score", names(all))

all.long <- gather(all, measure, value, a1.colnum:ncols(all))

all.long$interim <- str_sub(all.long$measure, 1, 2)
all.long$measure <- str_sub(all.long$measure, start = 3)

correct <- spread(all.long, measure, value)
