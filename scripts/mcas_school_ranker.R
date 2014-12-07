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

school_3 <- school3rd[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(school_3) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
school_4 <- school4th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(school_4) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
school_5 <- school5th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon",
                            "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(school_5) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image", 
                          "science_rank", "science_tested", "science_score", "science_image")
school_6 <- school6th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(school_6) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
school_7 <- school7th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]

colnames(school_7) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")

school_8 <- school8th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon",
                            "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(school_8) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image", 
                          "science_rank", "science_tested", "science_score", "science_image")

school_10 <- school10th[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                              "m_rank", "mtotal", "m_AandP", "m_icon",
                              "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(school_10) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                           "math_rank", "math_tested", "math_score", "math_image", 
                           "science_rank", "science_tested", "science_score", "science_image")

school_AL <- schoolAL[c("school_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                          "m_rank", "mtotal", "m_AandP", "m_icon",
                          "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(school_AL) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                         "math_rank", "math_tested", "math_score", "math_image", 
                         "science_rank", "science_tested", "science_score", "science_image")

dir.create("../output", showWarnings=FALSE)
dir.create("../output/school_rankings", showWarnings=FALSE)

dummy_2 <- readLines("../raw_data/school_rankings_2.txt", encoding="UTF-8")
dummy_3 <- readLines("../raw_data/school_rankings_3.txt", encoding="UTF-8")

dummy_2 <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas", cache_url, dummy_2)
dummy_3 <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas", cache_url, dummy_3)
dummy_2 <- sub("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas", index_base, dummy_2)
dummy_3 <- sub("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas", index_base, dummy_3)

dummy_2 <- gsub("WHATYEAR", yr, dummy_2)
dummy_3 <- gsub("WHATYEAR", yr, dummy_3)

schooltojson <- toJSONArray(school_3)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school3rd.jsonp")
school_text <- gsub("WHATGRADE", "3rd", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "school3rd", school_text)
write(school_text, "../output/school_rankings/school3rd.txt")

schooltojson <- toJSONArray(school_4)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school4th.jsonp")
school_text <- gsub("WHATGRADE", "4th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "school4th", school_text)
write(school_text, "../output/school_rankings/school4th.txt")

schooltojson <- toJSONArray(school_5)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school5th.jsonp")
school_text <- gsub("WHATGRADE", "5th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "school5th", school_text)
write(school_text, "../output/school_rankings/school5th.txt")

schooltojson <- toJSONArray(school_6)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school6th.jsonp")
school_text <- gsub("WHATGRADE", "6th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "school6th", school_text)
write(school_text, "../output/school_rankings/school6th.txt")

schooltojson <- toJSONArray(school_7)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school7th.jsonp")
school_text <- gsub("WHATGRADE", "7th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "school7th", school_text)
write(school_text, "../output/school_rankings/school7th.txt")

schooltojson <- toJSONArray(school_8)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school8th.jsonp")
school_text <- gsub("WHATGRADE", "8th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "school8th", school_text)
write(school_text, "../output/school_rankings/school8th.txt")

schooltojson <- toJSONArray(school_10)
schooltojson <- paste(schooltojson, "})")
schooltojson <- paste('scores({ "mcas": ', schooltojson)
write(schooltojson, "../output/school_rankings/school10th.jsonp")
school_text <- gsub("WHATGRADE", "10th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "school10th", school_text)
write(school_text, "../output/school_rankings/school10th.txt")

print("Done! You can find the jpts and jsonp files at '../output/school_rankings/' ")