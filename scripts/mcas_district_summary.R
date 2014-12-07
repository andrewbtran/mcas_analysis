
setwd("~/Documents/r_scripts/mcas/files/jsonp")


urls <- read.csv("new_urls.csv")
district_list <- read.csv("updated_district_list_to_merge_with_schools.csv")
district1 <- read.csv("district_step1.csv")
district_urls <- urls[c("short", "district_name_url")]

matched_districts_urls <- merge(district_urls, district_list, by="short", all.y=TRUE)
matched_districts_urls <- join(district_list, district_urls, by="short", type="left", match="first")
write.csv(matched_districts_urls, "districts_all_url.csv")
library(plyr)

new_district <- matched_districts_urls[c("short", "District_Name", "District_Name_via_District", "district_name_full", "list", "district_name_url")]
write.csv(new_district,"definitive_district_dataset.csv")
new_district <- read.csv("definitive_district_dataset.csv")
district_set <- merge(district1, new_district, by="District_Name")
write.csv(district_set, "district_step2.csv")

district_set <- read.csv("district_step2.csv")

district_summary <- district_set[c("short", "District_Name", "grade_14", "etotal_perf_14", "mtotal_perf_14", "stotal_perf_14", "e_AandP_14", "m_AandP_14", "s_AandP_14", "eadv_per_14", "epro_per_14", "eni_per_14", "ewf_per_14", "madv_per_14", "mpro_per_14", "mni_per_14", "mwf_per_14", "sadv_per_14", "spro_per_14", "sni_per_14", "swf_per_14", "ela_part_rate_14", "math_part_rate_14", "sci_part_rate_14")]

write.csv(district_summary, "fix_this.csv")

///
  
  
dist_sum <- read.csv("fix_this.csv")
district_list <- read.csv("sosotired.csv")
district_list$short <- as.character(district_list$short)

list_length <- 1:length(district_list$short)

for (i in list_length) {

school_short <- district_list$short[i]

district_total <- NULL
district_whoa <- NULL
district_alone <- subset(dist_sum, short==school_short | District_Name=="State Results")

grader <- subset(district_alone, grade_14==3 & short==school_short)
subject <- "English"
grade <- "Grade 3"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14

if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)


grader_state <- subset(district_alone, grade_14=="3" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 3"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_state <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_total, district_state)
}

grader <- subset(district_alone, grade_14=="3" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 3"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)


grader_state <- subset(district_alone, grade_14=="3" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 3"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}

grader <- subset(district_alone, grade_14=="4" & short==school_short)
subject <- "English"
grade <- "Grade 4"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="4" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 4"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="4" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 4"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="4" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 4"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="5" & short==school_short)
subject <- "English"
grade <- "Grade 5"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="5" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 5"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}

grader <- subset(district_alone, grade_14=="5" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 5"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="3" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 5"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="6" & short==school_short)
subject <- "English"
grade <- "Grade 6"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="6" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 6"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="6" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 6"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="6" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 6"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="7" & short==school_short)
subject <- "English"
grade <- "Grade 7"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="7" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 7"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="7" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 7"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="7" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 7"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="8" & short==school_short)
subject <- "English"
grade <- "Grade 8"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="8" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 8"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="8" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 8"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="8" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 8"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="8" & short==school_short)
subject <- "Science & Tech"
grade <- "Grade 8"
average <- "District"
advanced <- grader$sadv_per_14
proficient <- grader$spro_per_14
improvement <- grader$sni_per_14
warnings <- grader$swf_per_14
tested <- grader$stotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="8" & District_Name=="State Results")
subject <- "Science & Tech"
grade <- "Grade 8"
average <- "State"
advanced <- grader_state$sadv_per_14
proficient <- grader_state$spro_per_14
improvement <- grader_state$sni_per_14
warnings <- grader_state$swf_per_14
tested <- grader_state$stotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="10" & short==school_short)
subject <- "English"
grade <- "Grade 10"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="10" & District_Name=="State Results")
subject <- "English"
grade <- "Grade 10"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="10" & short==school_short)
subject <- "Mathematics"
grade <- "Grade 10"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="10" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "Grade 10"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="10" & short==school_short)
subject <- "Science & Tech"
grade <- "Grade 10"
average <- "District"
advanced <- grader$sadv_per_14
proficient <- grader$spro_per_14
improvement <- grader$sni_per_14
warnings <- grader$swf_per_14
tested <- grader$stotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="10" & District_Name=="State Results")
subject <- "Science & Tech"
grade <- "Grade 10"
average <- "State"
advanced <- grader_state$sadv_per_14
proficient <- grader_state$spro_per_14
improvement <- grader_state$sni_per_14
warnings <- grader_state$swf_per_14
tested <- grader_state$stotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="99" & short==school_short)
subject <- "English"
grade <- "All Grades"
average <- "District"
advanced <- grader$eadv_per_14
proficient <- grader$epro_per_14
improvement <- grader$eni_per_14
warnings <- grader$ewf_per_14
tested <- grader$etotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="99" & District_Name=="State Results")
subject <- "English"
grade <- "All Grades"
average <- "State"
advanced <- grader_state$eadv_per_14
proficient <- grader_state$epro_per_14
improvement <- grader_state$eni_per_14
warnings <- grader_state$ewf_per_14
tested <- grader_state$etotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="99" & short==school_short)
subject <- "Mathematics"
grade <- "All Grades"
average <- "District"
advanced <- grader$madv_per_14
proficient <- grader$mpro_per_14
improvement <- grader$mni_per_14
warnings <- grader$mwf_per_14
tested <- grader$mtotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="99" & District_Name=="State Results")
subject <- "Mathematics"
grade <- "All Grades"
average <- "State"
advanced <- grader_state$madv_per_14
proficient <- grader_state$mpro_per_14
improvement <- grader_state$mni_per_14
warnings <- grader_state$mwf_per_14
tested <- grader_state$mtotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}
grader <- subset(district_alone, grade_14=="99" & short==school_short)
subject <- "Science & Tech"
grade <- "All Grades"
average <- "District"
advanced <- grader$sadv_per_14
proficient <- grader$spro_per_14
improvement <- grader$sni_per_14
warnings <- grader$swf_per_14
tested <- grader$stotal_perf_14
if(length(tested) != 0 | !!is.null(tested)) {
district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)

grader_state <- subset(district_alone, grade_14=="99" & District_Name=="State Results")
subject <- "Science & Tech"
grade <- "All Grades"
average <- "State"
advanced <- grader_state$sadv_per_14
proficient <- grader_state$spro_per_14
improvement <- grader_state$sni_per_14
warnings <- grader_state$swf_per_14
tested <- grader_state$stotal_perf_14

district_total <- data.frame(subject, grade, average, advanced, proficient, improvement, warnings, tested)
district_whoa <- rbind(district_whoa, district_total)
}


district_whoa$advanced <- paste(district_whoa$advanced,"%")
district_whoa$proficient <- paste(district_whoa$proficient,"%")
district_whoa$improvement <- paste(district_whoa$improvement,"%")
district_whoa$warnings <- paste(district_whoa$warnings,"%")


colnames(district_whoa) <- c("Subject", "Grade", "Average", "Advanced", "Proficient", "Improvement", "Warning", "Tested")

json_filename <- paste(school_short,".jsonp")
json_filename <-gsub(" ","", json_filename)

district_json <- toJSONArray(district_whoa)
district_json <- paste(district_json, "})")
district_json <- paste('summary({ "district": ', district_json)
write(district_json, json_filename)

}

///
  
  