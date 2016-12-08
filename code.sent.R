library(arules)
library(arulesViz)
library(readxl)
library(dplyr)
library(stringr)
#########read in data
dat <- read_excel("CommonCourseApp.xlsx")
#remove NA
dat <- dat[complete.cases(dat),] 
#rename course
course.name <- paste(dat$`Course Name`,".",dat$`Section Name`,sep='')
course.name <- gsub("PH.","",course.name) 
#rename term
course.term <- dat$AcadPeriodSctn
course.term <- gsub("PH 2015-16 ","", course.term)
#final data
mydat <- data.frame(id = dat$RandomID, program = dat$AcademicProgramName,term = course.term,course = course.name)

########naive recommander
CourseRank <- function(user.program,user.term){
        mydat.summary <- mydat %>%  group_by(program,term,course) %>% summarise(count=n()) %>% arrange(desc(count))
        course.all <- mydat.summary %>% filter(program==user.program & term == user.term)
        return(list(term = user.term,recommand.courses = course.all$course))
}
#example of CourseRank
user.program <- "PHD: Biostatistics"
user.term <- "1"
CourseRank(user.program,user.term)


#######rule mining
###covert to binary matrix
CovertBinary <- function(items,purchase){
        id <- unique(purchase$id)
        binary.matrix <- matrix(0,nrow = length(id),ncol = length(items))
        rownames(binary.matrix) <- id
        colnames(binary.matrix) <- items
        for(i in 1:length(id)){
                current.purchase <- purchase[which(purchase$id==id[i]),]$course
                pois <- match(current.purchase,items)
                binary.matrix[i,pois] <- 1
        }
        binary.matrix
}
#example of CovertBinary
purchase <- mydat %>% filter(program==user.program & term== user.term) 
items <- CourseRank(user.program,user.term)$recommand.courses
mybinary <- CovertBinary(items,purchase)


###create association rule
AssociationRule <- function(mybinary){
        rules <- apriori(mybinary, parameter=list(support=0.1, confidence=0.8)) #specified parameters
        capture.output(inspect(sort(rules, by ="lift")))
}

#example of AssociationRule
mybinary <- CovertBinary(items,purchase)
AssociationRule(mybinary)

###prediction based on association rule
#given the association rule
rules <- AssociationRule(mybinary)

#corrently find the first lhs that contains all the input.courses
Recommander <- function(input.courses){
        lhs <- as.vector(rules$lhs)
        rhs <- as.vector(rules$rhs)
        #covert lhs to list of vectors
        lhs <- gsub("\\{|\\}","",lhs) %>% str_split(",")
        rhs <- gsub("\\{|\\}","",rhs) %>% str_split(",")
        for (i in 1:length(lhs)){
                if (input.courses %in% lhs[[i]]){
                        recommand.courses <- rhs[[i]]
                        break;
                }
        }
        recommand.courses
}

#example of Recommander
#input method, output probability
input.courses <- "140.751.01"
Recommander(input.courses)
