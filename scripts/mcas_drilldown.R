library(openxlsx)
library(plyr)

cat("\nWhat is the url base where the index XML will be public when published? \nIf you followed the file structure from 2014, it should be \nhttp://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/2015/ \nType 1 and enter if the file structure is identical to 2014. \nOtherwise, type out the base URL where the live XMLs can be found (don't forget the last '/')")
url_choice <- readline("\n  ")

if (url_choice==1) {
  index_base <- paste("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/", yr, sep="")
  url_base <- paste(index_base, "/districts/xml/", sep="")
} else { 
  url_base <- url_choice
  index_base <- paste("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/", yr, sep="")
}

cat("\nWhere are you uploading the .jsonp and .js files? \n(ie. 'http://cache.boston.com/multimedia/graphics/andrew/mcas/2015/') \n")
cache_url <- readline("\nType out the root address. DON'T FORGET THE '/' AT THE END ")

dir.create("../output", showWarnings=FALSE)
dir.create("../output/_list", showWarnings=FALSE)


cat("\nPlease select the spreadsheet with the raw schools data \n")
schools_file <- file.choose()
schools_from_xl <- read.xlsx(schools_file,sheet=1,colNames=TRUE)
cat("\nPlease wait...  \n")

schools_from_xl$short <- gsub("\\s", "", schools_from_xl$district_name)
schools_from_xl$short <- gsub("-", "", schools_from_xl$short)
schools_from_xl$short <- gsub("[.]", "", schools_from_xl$short)
schools_from_xl$short <- sub("[(].*", "", schools_from_xl$short)
schools_from_xl$short <- gsub("[']", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Public", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Technical", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Vocational", "", schools_from_xl$short)
schools_from_xl$short <- tolower(schools_from_xl$short)

schools_from_xl$district_name_url <- paste(url_base, schools_from_xl$short, sep="")
schools_from_xl$district_name_url <- paste("<a href='", schools_from_xl$district_name_url, sep="")
schools_from_xl$district_name_url <- paste(schools_from_xl$district_name_url, ".xml'>", sep="")
schools_from_xl$district_name_url <- paste(schools_from_xl$district_name_url, schools_from_xl$district_name, sep="")
schools_from_xl$district_name_url <- paste(schools_from_xl$district_name_url, "</a>", sep="")

schools_from_xl$school_name_url <- paste(url_base, schools_from_xl$short, sep="")
schools_from_xl$school_name_url <- paste("<a href='", schools_from_xl$school_name_url, sep="")
schools_from_xl$school_name_url <- paste(schools_from_xl$school_name_url, ".xml#schools'>", sep="")
schools_from_xl$school_name_url <- paste(schools_from_xl$school_name_url, schools_from_xl$school_name, sep="")
schools_from_xl$school_name_url <- paste(schools_from_xl$school_name_url, "</a>", sep="")

if (yr < 2000) {yr <- yr+2000}
yr_abbrv <- yr-2000

grade <- paste("grade_", yr_abbrv, sep="")
etotal <- paste("etotal_perf_", yr_abbrv, sep="")
mtotal <- paste("mtotal_perf_", yr_abbrv, sep="")
stotal <- paste("stotal_perf_", yr_abbrv, sep="")
e_AandP <- paste("e_AandP_", yr_abbrv, sep="")
m_AandP <- paste("m_AandP_", yr_abbrv, sep="")
s_AandP <- paste("s_AandP_", yr_abbrv, sep="")

school_summary <- schools_from_xl[c("short", "concatenated", "school_name", grade,
                                    etotal, mtotal, stotal, e_AandP, m_AandP, s_AandP, "echange_type", 
                                    "mchange_type", "schange_type", "district_name_url", "school_name_url")]

colnames(school_summary) <- c("short","concatenated", "school_name", "grade", "etotal", 
                              "mtotal", "stotal", "e_AandP", "m_AandP", "s_AandP", 
                              "echange_type", "mchange_type", "schange_type",
                              "district_name_url", "school_name_url")

if (length(grep("stateresults", school_summary$short)) != 0) {
  school_summary <- school_summary[- grep("stateresults", school_summary$short),]
}

school_summary$etotal <- as.numeric(as.character(school_summary$etotal))
school_summary$mtotal <- as.numeric(as.character(school_summary$mtotal))
school_summary$stotal <- as.numeric(as.character(school_summary$stotal))

school_summary$e_AandP <- as.numeric(as.character(school_summary$e_AandP))
school_summary$m_AandP <- as.numeric(as.character(school_summary$m_AandP))
school_summary$s_AandP <- as.numeric(as.character(school_summary$s_AandP))

school3rd <- subset(school_summary, grade=="03")
#write.csv(school3rd, "school3rd.csv")
school4th <- subset(school_summary, grade=="04")
#write.csv(school4th, "school4th.csv")
school5th <- subset(school_summary, grade=="05")
#write.csv(school5th, "school5th.csv")
school6th <- subset(school_summary, grade=="06")
#write.csv(school6th, "school6th.csv")
school7th <- subset(school_summary, grade=="07")
#write.csv(school7th, "school7th.csv")
school8th <- subset(school_summary, grade=="08")
#write.csv(school8th, "school8th.csv")
school10th <- subset(school_summary, grade=="10")
#write.csv(school10th, "school10th.csv")
schoolAL <- subset(school_summary, grade=="AL")



#totals in ranking here
school3rd$e_total <- sum(!is.na(school3rd$e_AandP))
school3rd$m_total <- sum(!is.na(school3rd$m_AandP))
school4th$e_total <- sum(!is.na(school4th$e_AandP))
school4th$m_total <- sum(!is.na(school4th$m_AandP))
school5th$e_total <- sum(!is.na(school5th$e_AandP))
school5th$m_total <- sum(!is.na(school5th$m_AandP))
school5th$s_total <- sum(!is.na(school5th$s_AandP))
school6th$e_total <- sum(!is.na(school6th$e_AandP))
school6th$m_total <- sum(!is.na(school6th$m_AandP))
school7th$e_total <- sum(!is.na(school7th$e_AandP))
school7th$m_total <- sum(!is.na(school7th$m_AandP))
school8th$e_total <- sum(!is.na(school8th$e_AandP))
school8th$m_total <- sum(!is.na(school8th$m_AandP))
school8th$s_total <- sum(!is.na(school8th$s_AandP))
school10th$e_total <- sum(!is.na(school10th$e_AandP))
school10th$m_total <- sum(!is.na(school10th$m_AandP))
school10th$s_total <- sum(!is.na(school10th$s_AandP))
schoolAL$e_total <- sum(!is.na(schoolAL$e_AandP))
schoolAL$m_total <- sum(!is.na(schoolAL$m_AandP))
schoolAL$s_total <- sum(!is.na(schoolAL$s_AandP))


#ok, actual ranking here

school3rd$e_rank <- rank(-school3rd$e_AandP, na.last="keep", ties.method="min")
school3rd$m_rank <- rank(-school3rd$m_AandP, na.last="keep", ties.method="min")
school4th$e_rank <- rank(-school4th$e_AandP, na.last="keep", ties.method="min")
school4th$m_rank <- rank(-school4th$m_AandP, na.last="keep", ties.method="min")
school5th$e_rank <- rank(-school5th$e_AandP, na.last="keep", ties.method="min")
school5th$m_rank <- rank(-school5th$m_AandP, na.last="keep", ties.method="min")
school5th$s_rank <- rank(-school5th$s_AandP, na.last="keep", ties.method="min")
school6th$e_rank <- rank(-school6th$e_AandP, na.last="keep", ties.method="min")
school6th$m_rank <- rank(-school6th$m_AandP, na.last="keep", ties.method="min")
school7th$e_rank <- rank(-school7th$e_AandP, na.last="keep", ties.method="min")
school7th$m_rank <- rank(-school7th$m_AandP, na.last="keep", ties.method="min")
school8th$e_rank <- rank(-school8th$e_AandP, na.last="keep", ties.method="min")
school8th$m_rank <- rank(-school8th$m_AandP, na.last="keep", ties.method="min")
school8th$s_rank <- rank(-school8th$s_AandP, na.last="keep", ties.method="min")
school10th$e_rank <- rank(-school10th$e_AandP, na.last="keep", ties.method="min")
school10th$m_rank <- rank(-school10th$m_AandP, na.last="keep", ties.method="min")
school10th$s_rank <- rank(-school10th$s_AandP, na.last="keep", ties.method="min")
schoolAL$e_rank <- rank(-schoolAL$e_AandP, na.last="keep", ties.method="min")
schoolAL$m_rank <- rank(-schoolAL$m_AandP, na.last="keep", ties.method="min")
schoolAL$s_rank <- rank(-schoolAL$s_AandP, na.last="keep", ties.method="min")


icons1 <-read.csv("../raw_data/ze_icons1.csv")
icons2 <-read.csv("../raw_data/ze_icons2.csv")
icons3 <-read.csv("../raw_data/ze_icons3.csv")

school3rd <- join(school3rd, icons1, by="echange_type")
school3rd <- join(school3rd, icons2, by="mchange_type")

school4th <- join(school4th, icons1, by="echange_type")
school4th <- join(school4th, icons2, by="mchange_type")

school5th <- join(school5th, icons1, by="echange_type")
school5th <- join(school5th, icons2, by="mchange_type")
school5th <- join(school5th, icons3, by="schange_type")

school6th <- join(school6th, icons1, by="echange_type")
school6th <- join(school6th, icons2, by="mchange_type")

school7th <- join(school7th, icons1, by="echange_type")
school7th <- join(school7th, icons2, by="mchange_type")

school8th <- join(school8th, icons1, by="echange_type")
school8th <- join(school8th, icons2, by="mchange_type")
school8th <- join(school8th, icons3, by="schange_type")

school10th <- join(school10th, icons1, by="echange_type")
school10th <- join(school10th, icons2, by="mchange_type")
school10th <- join(school10th, icons3, by="schange_type")

schoolAL <- join(schoolAL, icons1, by="echange_type")
schoolAL <- join(schoolAL, icons2, by="mchange_type")
schoolAL <- join(schoolAL, icons3, by="schange_type")



#school3rd <- read.csv("school3rd.csv")
#school4th <- read.csv("school4th.csv")
#school5th <- read.csv("school5th.csv")
#school6th <- read.csv("school6th.csv")
#school7th <- read.csv("school7th.csv")
#school8th <- read.csv("school8th.csv")
#school10th <- read.csv("school10th.csv")


school3rd$school_short <- paste(school3rd$school_name, school3rd$short, sep=", ")
school4th$school_short <- paste(school4th$school_name, school4th$short, sep=", ")
school5th$school_short <- paste(school5th$school_name, school5th$short, sep=", ")
school6th$school_short <- paste(school6th$school_name, school6th$short, sep=", ")
school7th$school_short <- paste(school7th$school_name, school7th$short, sep=", ")
school8th$school_short <- paste(school8th$school_name, school8th$short, sep=", ")
school10th$school_short <- paste(school10th$school_name, school10th$short, sep=", ")


ranks_list3 <- school3rd[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total")]
ranks_list4 <- school4th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total")]
ranks_list5 <- school5th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total", "s_rank", "s_total")]
ranks_list6 <- school6th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total")]
ranks_list7 <- school7th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total")]
ranks_list8 <- school8th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total", "s_rank", "s_total")]
ranks_list10 <- school10th[c("short", "school_name", "school_short", "m_rank", "m_total", "e_rank", "e_total", "s_rank", "s_total")]

ranks_list3$m_placementoverall <- paste(ranks_list3$m_rank, "of")
ranks_list3$m_placementoverall <- paste(ranks_list3$m_placementoverall, ranks_list3$m_total)
ranks_list3$e_placementoverall <- paste(ranks_list3$e_rank, "of")
ranks_list3$e_placementoverall <- paste(ranks_list3$e_placementoverall, ranks_list3$e_total)

ranks_list4$m_placementoverall <- paste(ranks_list4$m_rank, "of")
ranks_list4$m_placementoverall <- paste(ranks_list4$m_placementoverall, ranks_list4$m_total)
ranks_list4$e_placementoverall <- paste(ranks_list4$e_rank, "of")
ranks_list4$e_placementoverall <- paste(ranks_list4$e_placementoverall, ranks_list4$e_total)

ranks_list5$m_placementoverall <- paste(ranks_list5$m_rank, "of")
ranks_list5$m_placementoverall <- paste(ranks_list5$m_placementoverall, ranks_list5$m_total)
ranks_list5$e_placementoverall <- paste(ranks_list5$e_rank, "of")
ranks_list5$e_placementoverall <- paste(ranks_list5$e_placementoverall, ranks_list5$e_total)
ranks_list5$s_placementoverall <- paste(ranks_list5$s_rank, "of")
ranks_list5$s_placementoverall <- paste(ranks_list5$s_placementoverall, ranks_list5$s_total)

ranks_list6$m_placementoverall <- paste(ranks_list6$m_rank, "of")
ranks_list6$m_placementoverall <- paste(ranks_list6$m_placementoverall, ranks_list6$m_total)
ranks_list6$e_placementoverall <- paste(ranks_list6$e_rank, "of")
ranks_list6$e_placementoverall <- paste(ranks_list6$e_placementoverall, ranks_list6$e_total)

ranks_list7$m_placementoverall <- paste(ranks_list7$m_rank, "of")
ranks_list7$m_placementoverall <- paste(ranks_list7$m_placementoverall, ranks_list7$m_total)
ranks_list7$e_placementoverall <- paste(ranks_list7$e_rank, "of")
ranks_list7$e_placementoverall <- paste(ranks_list7$e_placementoverall, ranks_list7$e_total)

ranks_list8$m_placementoverall <- paste(ranks_list8$m_rank, "of")
ranks_list8$m_placementoverall <- paste(ranks_list8$m_placementoverall, ranks_list8$m_total)
ranks_list8$e_placementoverall <- paste(ranks_list8$e_rank, "of")
ranks_list8$e_placementoverall <- paste(ranks_list8$e_placementoverall, ranks_list8$e_total)
ranks_list8$s_placementoverall <- paste(ranks_list8$s_rank, "of")
ranks_list8$s_placementoverall <- paste(ranks_list8$s_placementoverall, ranks_list8$s_total)

ranks_list10$m_placementoverall <- paste(ranks_list10$m_rank, "of")
ranks_list10$m_placementoverall <- paste(ranks_list10$m_placementoverall, ranks_list10$m_total)
ranks_list10$e_placementoverall <- paste(ranks_list10$e_rank, "of")
ranks_list10$e_placementoverall <- paste(ranks_list10$e_placementoverall, ranks_list10$e_total)
ranks_list10$s_placementoverall <- paste(ranks_list10$s_rank, "of")
ranks_list10$s_placementoverall <- paste(ranks_list10$s_placementoverall, ranks_list10$s_total)

ranks_list3 <- ranks_list3 [c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall")]
ranks_list4 <- ranks_list4[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall")]
ranks_list5 <- ranks_list5[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall", "s_placementoverall")]
ranks_list6 <- ranks_list6[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall")]
ranks_list7 <- ranks_list7[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall")]
ranks_list8 <- ranks_list8[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall", "s_placementoverall")]
ranks_list10 <- ranks_list10[c("short", "school_name", "school_short", "e_placementoverall", "m_placementoverall", "s_placementoverall")]

ranks_list3$m_placementoverall <- gsub("NA of NA", "--",ranks_list3$m_placementoverall)
ranks_list3$e_placementoverall <- gsub("NA of NA", "--",ranks_list3$e_placementoverall)
ranks_list4$m_placementoverall <- gsub("NA of NA", "--",ranks_list4$m_placementoverall)
ranks_list4$e_placementoverall <- gsub("NA of NA", "--",ranks_list4$e_placementoverall)
ranks_list5$m_placementoverall <- gsub("NA of NA", "--",ranks_list5$m_placementoverall)
ranks_list5$e_placementoverall <- gsub("NA of NA", "--",ranks_list5$e_placementoverall)
ranks_list5$s_placementoverall <- gsub("NA of NA", "--",ranks_list5$s_placementoverall)
ranks_list6$m_placementoverall <- gsub("NA of NA", "--",ranks_list6$m_placementoverall)
ranks_list6$e_placementoverall <- gsub("NA of NA", "--",ranks_list6$e_placementoverall)
ranks_list7$m_placementoverall <- gsub("NA of NA", "--",ranks_list7$m_placementoverall)
ranks_list7$e_placementoverall <- gsub("NA of NA", "--",ranks_list7$e_placementoverall)
ranks_list8$m_placementoverall <- gsub("NA of NA", "--",ranks_list8$m_placementoverall)
ranks_list8$e_placementoverall <- gsub("NA of NA", "--",ranks_list8$e_placementoverall)
ranks_list8$s_placementoverall <- gsub("NA of NA", "--",ranks_list8$s_placementoverall)
ranks_list10$m_placementoverall <- gsub("NA of NA", "--",ranks_list10$m_placementoverall)
ranks_list10$e_placementoverall <- gsub("NA of NA", "--",ranks_list10$e_placementoverall)
ranks_list10$s_placementoverall <- gsub("NA of NA", "--",ranks_list10$s_placementoverall)

#district stuff


cat("\nPlease select the spreadsheet with the raw districts data \n")
districts_file <- file.choose()
districts_from_xl <- read.xlsx(districts_file,sheet=1,colNames=TRUE)
cat("\nPlease wait...  \n")


districts_from_xl$short <- gsub("\\s", "", districts_from_xl$District_Name)
districts_from_xl$short <- gsub("-", "", districts_from_xl$short)
districts_from_xl$short <- gsub("[.]", "", districts_from_xl$short)
districts_from_xl$short <- sub("[(].*", "", districts_from_xl$short)
districts_from_xl$short <- gsub("[']", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Public", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Technical", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Vocational", "", districts_from_xl$short)
districts_from_xl$short <- tolower(districts_from_xl$short)

districts_from_xl$district_name_url <- paste(url_base, districts_from_xl$short, sep="")
districts_from_xl$district_name_url <- paste("<a href='", districts_from_xl$district_name_url, sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, ".xml'>", sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, districts_from_xl$district_name, sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, "</a>", sep="")


district_list <- districts_from_xl

schools <- schools_from_xl
  
schools$school_short <-  paste(schools$school_name, schools$short, sep=", ")

if (length(grep("stateresults", schools$short)) != 0) {
  schools <- schools[- grep("stateresults", schools$short),]
}

if (length(grep("stateresults", district_list$short)) != 0) {
  district_list <- district_list[- grep("stateresults", district_list$short),]
}

district_list <- district_list[!duplicated(district_list$District_Name),]

schools <- as.character(schools)
district_list <- as.character(district_list)
file_dir <- ("../output/_list/")


#schools <- read.csv("schools_complete.csv", stringsAsFactors=FALSE, na.strings = "")

#district_list <- read.csv("definitive_district_dataset.csv")

# alt file destination stuff

# setwd("~/Documents/r_scripts/mcas/files/district_rankings/alt_lists")

# schools <- read.csv("../../schools_complete.csv", stringsAsFactors=FALSE, na.strings = "")

# district_list <- read.csv("../../definitive_district_dataset.csv")

#in the mcas_analysis folder

# schools <- read.csv("schools_complete.csv", stringsAsFactors=FALSE, na.strings = "")
# district_list <- read.csv("definitive_district_dataset.csv")

# setwd("~/Desktop/mcas_analysis/lists")

# here's the loop
list_length <- 1:length(district_list$short)

for (i in list_length) {
  
  school_short_list <- district_list$short[i]
  
  just_schools <- subset(schools, short==school_short_list)
  
  if (nrow(just_schools) != 0) {
  
  sub_list <- 1:length(just_schools$school_short)
  
  the_list <- NULL
  
  for (i in sub_list) {
    
    if (just_schools$grade_14[i] == "03") {
      grade_subject <- "3rd English"
      school <- just_schools$school_name[i]
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list3, school_short==schoolz)
      if(nrow(placed) == 0 ) {placementoverall <- ""  } else {  placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) { the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      grade_subject <- "3rd Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list3, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade_14[i] == "04") {
      grade_subject <- "4th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list4, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "4th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list4, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade_14[i] == "05") {
      grade_subject <- "5th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "5th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "5th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    if (just_schools$grade_14[i] == "06") {
      grade_subject <- "6th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list6, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      grade_subject <- "6th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list6, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade_14[i] == "07") {
      grade_subject <- "7th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list7, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "7th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list7, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade_14[i] == "08") {
      grade_subject <- "8th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "8th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "8th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    
    if (just_schools$grade_14[i] == "10") {
      grade_subject <- "10th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall)
      
      grade_subject <- "10th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "10th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    if (just_schools$grade_14[i] == "AL") {
      grade_subject <- "All grades English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per_14[i]
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placementoverall <- ""
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall)
      
      grade_subject <- "All grades Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per_14[i]
      if ( is.null(advanced) | advanced=="NA" ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per_14[i]
      if ( is.null(proficient) | proficient=="NA" ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per_14[i]
      if ( is.null(improvement) | improvement =="NA" ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per_14[i]      
      if ( is.null(warningz) | warningz =="NA" ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      
      
      tested <- just_schools$mtotal_perf_14[i]
      if ( is.null(tested) | tested =="NA" ) { tested <- ""}
      placementoverall <- ""      
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
  }
  
  
  the_list$warning <- the_list$warningz
  the_list$warningz <- NULL
  
  
  json_filename <- paste(school_short_list,"_list.jsonp")
  json_filename <- paste(file_dir, json_filename, sep="")
  json_filename <-gsub(" ","", json_filename)
  
  schools_json <- toJSONArray(the_list)
  schools_json <- paste(schools_json, "})")
  schools_json <- paste('school_summary({ "school": ', schools_json)
  write(schools_json, json_filename)
  } else {
    cat("WARNING: Could not find match for", school_short_list, " ")
  }
}


wd <- getwd()
file_directory <- setwd("..")
file_directory <- getwd()

file_name <- paste(file_directory,"/output/_list/", sep="")
cat("\nThe files can be found in ", file_name)


