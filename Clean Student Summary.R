# Reshape ANet student summary data into long form
options(stringsAsFactors = F)
require(dplyr)
require(tidyr)
require(stringr)

sacred.loc <- "C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data"
anet.loc <- "ANet/FY15/Student Summary/"

setwd(sacred.loc)
sasid.link <- read.csv("Crosswalks and Groups/Student ANet ID to SASID Link.csv")

setwd(paste(sacred.loc, anet.loc, sep = "/"))

# Expects data to come in two files for ela and two files for math,
# one for for K-5 and one for 6-8.
# This may need editing if ANet changes their export format.

ela.elem <- read.csv("InterimScores-2014-State20-ELA-Elem.csv")
ela.middle <- read.csv("InterimScores-2014-State20-ELA-Middle.csv")

math.elem <- read.csv("InterimScores-2014-State20-Math-Elem.csv")
math.middle <- read.csv("InterimScores-2014-State20-Math-Middle.csv")

ela <- rbind(ela.elem, ela.middle)
math <- rbind(math.elem, math.middle)

ela$subject <- "ELA"
math$subject <- "Math"

# There should be one row per student
stopifnot(sum(duplicated(math$Student.ANET.ID))==0)
stopifnot(sum(duplicated(ela$Student.ANET.ID))==0 )

# Remove the "ELA" and "Math" prefixes from the variable names
# Note that I didn't know about NSE when making this. 
# There is a better way using SE. See the NSE vignette.

remove_pre <- function(db, remover) {
  db.extra <- select(db, starts_with(remover), Student.ANET.ID)
  db.extra <- gather(db.extra, measure, value, starts_with(remover))
  db.extra$measure <- str_replace(db.extra$measure, remover, "")
  
  db.wide <- spread(db.extra, measure, value)
  db.small <- select(db, -starts_with(remover))
  left_join(db.small, db.wide)
}

ela2 <- remove_pre(ela, "ELA.")

math2 <- remove_pre(math, "Math.")

all <- rbind(ela2, math2)

# Reshape to long. First, gather to super-long, then spread the points-possible points

a1.colnum <- grep("A1.Raw.Score", colnames(all))
select.cols <- grep("A[0-9]", colnames(all))

all.long <- gather(all, measure, value, select.cols)

all.long$interim <- str_sub(all.long$measure, 1, 2)
all.long$measure <- str_sub(all.long$measure, start = 4)

data <- spread(all.long, measure, value)

data <- data %>%
  mutate( Raw.Score = extract_numeric(Raw.Score),
          Points.Possible = extract_numeric(Points.Possible),
          perc = round(Raw.Score / Points.Possible, 3)*100 )

hist(data$perc)

# Check student IDs
need.sasid <- all[!(all$SASID %in% sasid.link$SASID), ]

write.csv(need.sasid, "Need SASID.csv", row.names = F, na = "")

write.csv(data, "ANet Student Summary.csv", row.names = F, na = "")
