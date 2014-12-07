library(plyr)
library(openxlsx)


cat("\nPlease select the spreadsheet with the raw districts data \n")
districts_file <- file.choose()
districts_from_xl <- read.xlsx(districts_file,sheet=1,colNames=TRUE)

cat("\nPlease select the spreadsheet with the raw schools data \n")
schools_file <- file.choose()
schools_from_xl <- read.xlsx(schools_file,sheet=1,colNames=TRUE)

cat("\nPlease wait... This will take several minutes... \n")

if (yr < 2000) {yr <- yr+2000}
yr_abbrv <- yr-2000


districts_from_xl$short <- gsub("\\s", "", districts_from_xl$District_Name)
districts_from_xl$short <- gsub("-", "", districts_from_xl$short)
districts_from_xl$short <- gsub("[.]", "", districts_from_xl$short)
districts_from_xl$short <- sub("[(].*", "", districts_from_xl$short)
districts_from_xl$short <- gsub("[']", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Public", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Technical", "", districts_from_xl$short)
districts_from_xl$short <- gsub("Vocational", "", districts_from_xl$short)
districts_from_xl$short <- tolower(districts_from_xl$short)

grade <- paste("grade_", yr_abbrv, sep="")
etotal <- paste("etotal_perf_", yr_abbrv, sep="")
mtotal <- paste("mtotal_perf_", yr_abbrv, sep="")
stotal <- paste("stotal_perf_", yr_abbrv, sep="")
e_AandP <- paste("e_AandP_", yr_abbrv, sep="")
m_AandP <- paste("m_AandP_", yr_abbrv, sep="")
s_AandP <- paste("s_AandP_", yr_abbrv, sep="")
eadv_per <- paste("eadv_per_", yr_abbrv, sep="")
epro_per <- paste("epro_per_", yr_abbrv, sep="")
eni_per <- paste("eni_per_", yr_abbrv, sep="")
ewf_per <- paste("ewf_per_", yr_abbrv, sep="")
madv_per <- paste("madv_per_", yr_abbrv, sep="")
mpro_per <- paste("mpro_per_", yr_abbrv, sep="")
mni_per <- paste("mni_per_", yr_abbrv, sep="")
mwf_per <- paste("mwf_per_", yr_abbrv, sep="")
sadv_per <- paste("sadv_per_", yr_abbrv, sep="")
spro_per <- paste("spro_per_", yr_abbrv, sep="")
sni_per <- paste("sni_per_", yr_abbrv, sep="")
swf_per <- paste("swf_per_", yr_abbrv, sep="")
ela_part_rate <- paste("ela_part_rate_", yr_abbrv, sep="")
math_part_rate <- paste("math_part_rate_", yr_abbrv, sep="")
sci_part_rate <- paste("sci_part_rate_", yr_abbrv, sep="")


schools_from_xl$short <- gsub("\\s", "", schools_from_xl$district_name)
schools_from_xl$short <- gsub("-", "", schools_from_xl$short)
schools_from_xl$short <- gsub("[.]", "", schools_from_xl$short)
schools_from_xl$short <- sub("[(].*", "", schools_from_xl$short)
schools_from_xl$short <- gsub("[']", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Public", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Technical", "", schools_from_xl$short)
schools_from_xl$short <- gsub("Vocational", "", schools_from_xl$short)
schools_from_xl$short <- tolower(schools_from_xl$short)


schools_from_xl$school_short <- paste(schools_from_xl$school_name, schools_from_xl$short, sep=", ")


school_summary <- schools_from_xl[c(
  "short",
  "school_short",
  "school_name",
  grade, 
  etotal, 
  mtotal, 
  stotal, 
  e_AandP, 
  m_AandP, 
  s_AandP, 
  eadv_per, 
  epro_per, 
  eni_per, 
  ewf_per, 
  madv_per, 
  mpro_per, 
  mni_per, 
  mwf_per, 
  sadv_per, 
  spro_per, 
  sni_per, 
  swf_per, 
  ela_part_rate, 
  math_part_rate, 
  sci_part_rate,
  "echange_type", 
  "mchange_type", "schange_type")]

colnames(school_summary) <- c("short","school_short", "school_name", "grade", "etotal", 
                                "mtotal", "stotal", "e_AandP", "m_AandP", "s_AandP","eadv_per", 
                                "epro_per", 
                                "eni_per", 
                                "ewf_per", 
                                "madv_per", 
                                "mpro_per", 
                                "mni_per", 
                                "mwf_per", 
                                "sadv_per", 
                                "spro_per", 
                                "sni_per", 
                                "swf_per", 
                                "ela_part_rate", 
                                "math_part_rate", 
                                "sci_part_rate", "echange_type", 
                              "mchange_type", "schange_type")

school_summary <- subset(school_summary, short!="stateresults")

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




# here's the loop
districts_list <- subset(districts_from_xl, short!="stateresults")

list_length <- 1:length(districts_list$short)

for (i in list_length) {
  
  school_short_list <- districts_list$short[i]
  
  just_schools <- subset(school_summary, short==school_short_list)
  
  sub_list <- 1:length(just_schools$school_short)
  
  the_list <- NULL
  
  for (i in sub_list) {
    
    if (just_schools$grade[i] == "03") {
      grade_subject <- "3rd English"
      school <- just_schools$school_name[i]
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list3, school_short==schoolz)
      if(nrow(placed) == 0 ) {placementoverall <- ""  } else {  placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) { the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "3rd Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list3, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade[i] == "04") {
      grade_subject <- "4th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list4, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "4th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list4, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade[i] == "05") {
      grade_subject <- "5th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "5th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "5th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list5, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    if (just_schools$grade[i] == "06") {
      grade_subject <- "6th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list6, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      grade_subject <- "6th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list6, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade[i] == "07") {
      grade_subject <- "7th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list7, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "7th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list7, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
    if (just_schools$grade[i] == "08") {
      grade_subject <- "8th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else { the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall) }
      
      grade_subject <- "8th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "8th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list8, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    
    if (just_schools$grade[i] == "10") {
      grade_subject <- "10th English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$e_placement}
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall)
      
      grade_subject <- "10th Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$m_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
      grade_subject <- "10th Science"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$sadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$spro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$sni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$swf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$stotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placed <- subset(ranks_list10, school_short==schoolz)         
      if(nrow(placed) == 0 ) {placementoverall <- "" } else { placementoverall <- placed$s_placement}
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
      
    }
    
    if (just_schools$grade[i] == "AL") {
      grade_subject <- "All grades English"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$eadv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$epro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$eni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$ewf_per[i]
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$etotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placementoverall <- ""
      if(is.data.frame(the_list)) {
        the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))  
      } else the_list <- data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall)
      
      grade_subject <- "All grades Math"
      school <- just_schools$school_name[i]       
      schoolz <- just_schools$school_short[i]
      advanced <- just_schools$madv_per[i]
      if ( is.null(advanced) | is.na(advanced) ) { advanced <- ""} else { advanced <- paste(advanced,"%") }  
      proficient <- just_schools$mpro_per[i]
      if ( is.null(proficient) | is.na(proficient) ) { proficient <- ""} else { proficient <- paste(proficient, "%")}       
      improvement <- just_schools$mni_per[i]
      if ( is.null(improvement) | is.na(improvement) ) { improvement <- ""} else { improvement <- paste(improvement, "%")}
      warningz <- just_schools$mwf_per[i]      
      if ( is.null(warningz) | is.na(warningz) ) { warningz <- ""} else { warningz <- paste(warningz,"%")}
      tested <- just_schools$mtotal[i]
      if ( is.null(tested) | is.na(tested) ) { tested <- ""}
      placementoverall <- ""      
      the_list <- rbind(the_list, data.frame(grade_subject, school, schoolz, advanced, proficient, improvement, warningz, tested, placementoverall))
    }
    
  }
  
  
  
  the_list$warning <- the_list$warningz
  the_list$warningz <- NULL
  
  json_filename <- paste("../output/summaries/", school_short_list, sep="")
  
  json_filename <- paste(json_filename,"_list.jsonp", sep="")
  
  schools_json <- toJSONArray(the_list)
  schools_json <- paste(schools_json, "})")
  schools_json <- paste('school_summary({ "school": ', schools_json)
  write(schools_json, json_filename)
  
}

# whoa did it work?








district_summary <- districts_from_xl[c(
  "short", 
  "District_Name", 
  grade, 
  etotal, 
  mtotal, 
  stotal, 
  e_AandP, 
  m_AandP, 
  s_AandP, 
  eadv_per, 
  epro_per, 
  eni_per, 
  ewf_per, 
  madv_per, 
  mpro_per, 
  mni_per, 
  mwf_per, 
  sadv_per, 
  spro_per, 
  sni_per, 
  swf_per, 
  ela_part_rate, 
  math_part_rate, 
  sci_part_rate)]

colnames(district_summary) <- c("short","District_Name", "grade", "etotal", 
                                "mtotal", "stotal", "e_AandP", "m_AandP", "s_AandP","eadv_per", 
                                "epro_per", 
                                "eni_per", 
                                "ewf_per", 
                                "madv_per", 
                                "mpro_per", 
                                "mni_per", 
                                "mwf_per", 
                                "sadv_per", 
                                "spro_per", 
                                "sni_per", 
                                "swf_per", 
                                "ela_part_rate", 
                                "math_part_rate", 
                                "sci_part_rate")

district_summary$etotal <- as.numeric(as.character(district_summary$etotal))
district_summary$mtotal <- as.numeric(as.character(district_summary$mtotal))
district_summary$stotal <- as.numeric(as.character(district_summary$stotal))

district_summary$e_AandP <- as.numeric(as.character(district_summary$e_AandP))
district_summary$m_AandP <- as.numeric(as.character(district_summary$m_AandP))
district_summary$s_AandP <- as.numeric(as.character(district_summary$s_AandP))

district_summary$eadv_per <- as.numeric(as.character(district_summary$eadv_per))
district_summary$epro_per <- as.numeric(as.character(district_summary$epro_per))
district_summary$eni_per <- as.numeric(as.character(district_summary$eni_per))
district_summary$ewf_per <- as.numeric(as.character(district_summary$ewf_per))
district_summary$madv_per <- as.numeric(as.character(district_summary$madv_per))
district_summary$mpro_per <- as.numeric(as.character(district_summary$mpro_per))
district_summary$mni_per <- as.numeric(as.character(district_summary$mni_per))
district_summary$mwf_per <- as.numeric(as.character(district_summary$mwf_per))
district_summary$sadv_per <- as.numeric(as.character(district_summary$sadv_per))
district_summary$spro_per <- as.numeric(as.character(district_summary$spro_per))
district_summary$sni_per <- as.numeric(as.character(district_summary$sni_per))
district_summary$swf_per <- as.numeric(as.character(district_summary$swf_per))
district_summary$ela_part_rate <- as.numeric(as.character(district_summary$ela_part_rate))
district_summary$math_part_rate <- as.numeric(as.character(district_summary$math_part_rate))
district_summary$sci_part_rate <- as.numeric(as.character(district_summary$sci_part_rate))

dir.create("../output", showWarnings=FALSE)
dir.create("../output/summaries", showWarnings=FALSE)

list_length <- 1:length(district_summary$short)

for (i in list_length) {

school_short <- district_summary$short[i]

district_total <- NULL
district_whoa <- NULL

district_alone <- subset(district_summary, short==school_short | District_Name=="State Results")

grader <- subset(district_alone, grade=="03" & short==school_short)
subject <- "English"
gradez <- "Grade 3"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal

if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)


grader_state <- subset(district_alone, grade=="03" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 3"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_state <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_total, district_state)
}

grader <- subset(district_alone, grade=="03" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 3"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)


grader_state <- subset(district_alone, grade=="03" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 3"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}

grader <- subset(district_alone, grade=="04" & short==school_short)
subject <- "English"
gradez <- "Grade 4"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="04" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 4"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="04" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 4"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="04" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 4"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="05" & short==school_short)
subject <- "English"
gradez <- "Grade 5"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="05" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 5"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}

grader <- subset(district_alone, grade=="05" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 5"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="05" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 5"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="06" & short==school_short)
subject <- "English"
gradez <- "Grade 6"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="06" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 6"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="06" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 6"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="06" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 6"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="07" & short==school_short)
subject <- "English"
gradez <- "Grade 7"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="07" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 7"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="07" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 7"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="07" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 7"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="08" & short==school_short)
subject <- "English"
gradez <- "Grade 8"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="08" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 8"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="08" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 8"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="08" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 8"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="08" & short==school_short)
subject <- "Science & Tech"
gradez <- "Grade 8"
average <- "District"
advanced <- grader$sadv_per
proficient <- grader$spro_per
improvement <- grader$sni_per
warnings <- grader$swf_per
tested <- grader$stotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="08" & District_Name=="State Results")
subject <- "Science & Tech"
gradez <- "Grade 8"
average <- "State"
advanced <- grader_state$sadv_per
proficient <- grader_state$spro_per
improvement <- grader_state$sni_per
warnings <- grader_state$swf_per
tested <- grader_state$stotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="10" & short==school_short)
subject <- "English"
gradez <- "Grade 10"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="10" & District_Name=="State Results")
subject <- "English"
gradez <- "Grade 10"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="10" & short==school_short)
subject <- "Mathematics"
gradez <- "Grade 10"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="10" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "Grade 10"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="10" & short==school_short)
subject <- "Science & Tech"
gradez <- "Grade 10"
average <- "District"
advanced <- grader$sadv_per
proficient <- grader$spro_per
improvement <- grader$sni_per
warnings <- grader$swf_per
tested <- grader$stotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="10" & District_Name=="State Results")
subject <- "Science & Tech"
gradez <- "Grade 10"
average <- "State"
advanced <- grader_state$sadv_per
proficient <- grader_state$spro_per
improvement <- grader_state$sni_per
warnings <- grader_state$swf_per
tested <- grader_state$stotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="AL" & short==school_short)
subject <- "English"
gradez <- "All Grades"
average <- "District"
advanced <- grader$eadv_per
proficient <- grader$epro_per
improvement <- grader$eni_per
warnings <- grader$ewf_per
tested <- grader$etotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="AL" & District_Name=="State Results")
subject <- "English"
gradez <- "All Grades"
average <- "State"
advanced <- grader_state$eadv_per
proficient <- grader_state$epro_per
improvement <- grader_state$eni_per
warnings <- grader_state$ewf_per
tested <- grader_state$etotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="AL" & short==school_short)
subject <- "Mathematics"
gradez <- "All Grades"
average <- "District"
advanced <- grader$madv_per
proficient <- grader$mpro_per
improvement <- grader$mni_per
warnings <- grader$mwf_per
tested <- grader$mtotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="AL" & District_Name=="State Results")
subject <- "Mathematics"
gradez <- "All Grades"
average <- "State"
advanced <- grader_state$madv_per
proficient <- grader_state$mpro_per
improvement <- grader_state$mni_per
warnings <- grader_state$mwf_per
tested <- grader_state$mtotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade=="AL" & short==school_short)
subject <- "Science & Tech"
gradez <- "All Grades"
average <- "District"
advanced <- grader$sadv_per
proficient <- grader$spro_per
improvement <- grader$sni_per
warnings <- grader$swf_per
tested <- grader$stotal
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade=="AL" & District_Name=="State Results")
subject <- "Science & Tech"
gradez <- "All Grades"
average <- "State"
advanced <- grader_state$sadv_per
proficient <- grader_state$spro_per
improvement <- grader_state$sni_per
warnings <- grader_state$swf_per
tested <- grader_state$stotal

district_total <- data.frame(subject, gradez, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}

district_whoa$advanced <- paste(district_whoa$advanced,"%")
district_whoa$proficient <- paste(district_whoa$proficient,"%")
district_whoa$improvement <- paste(district_whoa$improvement,"%")
district_whoa$warnings <- paste(district_whoa$warnings,"%")

colnames(district_whoa) <- c("Subject", "Grade", "Average", "Advanced", "Proficient", "Improvement", "Warning", "Tested")

json_filename <- paste("../output/summaries/", school_short, sep="")
json_filename <- paste(json_filename,".jsonp", sep="")

district_json <- toJSONArray(district_whoa)
district_json <- paste(district_json, "})")
district_json <- paste('summary({ "district": ', district_json)
write(district_json, json_filename)
}

  