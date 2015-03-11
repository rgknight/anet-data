require(tidyr)
require(dplyr)
require(stringr)
options(stringsAsFactors=F)

# Loop through all Anet files in a directory
# relying on a standard naming convention to assign meta-data
# and clean up a few oddities

loadClean <- function(thispath, startFields){

	# Get list of files & loops through loadin and cleaning
	files <- list.files(path= paste(thispath, "Data", sep = "/"), pattern=".csv$")
	f <- NULL
	for(i in files) {

	  # read
	  X <- read.csv(paste(thispath, "Data",i, sep="/"), header=TRUE)
	  
	  # remove blank rows (no school)
	  X <- X[X[ , 1] != "", ]

	  print(i)
	  
	  # Reshape to long
	  X <- X %>% select(-State) 
	  long <- X %>% gather(measure, value, startFields:ncol(X))

	  # Get meta-data from the file name  
	  parts <- strsplit(i, " ")
	  subj <- parts[[1]][1]
	  inter <- parts[[1]][3]
	  inter <-substr(inter, 1, nchar(inter)-4)#remove the last 4 char (.csv)
	  
	  long$interim <- inter
	  long$subject <- subj
	  
	  f <- rbind(long,f)
	}


	# clean some of the output
	splitpoint <- 15

	f <- f %>% 
	  filter(value != "-" & !duplicated(f) ) %>%
	  separate(measure, c("grade.level", "item"), sep = splitpoint)
  
  if ("School.Id" %in% names(f)) {
	  f <- rename(f, Anet.School.Id = School.Id)
  }
	
	f$value <- gsub("%", "", f$value)
	f$value <- as.numeric(f$value)

	f$grade <- str_sub(f$grade.level, -1)
	f$grade <- as.numeric(f$grade)

	# Note: I should make this more flexible
	f[f$item == ".School", "item"] <- "perc"
	f[f$item == ".Network", "item"] <- "diff_to_network"
	f[f$item == "...STUDENTS.School", "item"] <- "stu"

	# make wide by item
	f <- f %>% select(-grade.level) %>%
	  rename(school = School) %>%
	  spread(item, value)


	write.csv(f, file =paste(thispath, "Anet All Schools All Grades.csv", sep = "/"), row.names = F)
}

loadClean("C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data/ANet/FY15/School Results", 3)
loadClean("C:/Dropbox (UP)/UP-Data Evaluation/Sacred Data/ANet/FY14/School Results", 2)
