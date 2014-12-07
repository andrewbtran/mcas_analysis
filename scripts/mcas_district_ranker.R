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

cat("\nWhere are you uploading the .jsonp and .js files? \n(ie. 'http://cache.boston.com/multimedia/graphics/andrew/mcas/2015/rankings/') \nIf structured like last year, type 1 and enter.")
cache_url <- readline("\n Otherwise, type out the root address. DON'T FORGET THE '/' AT THE END ")

if (cache_url==1) {
  cache_url <- "http://cache.boston.com/multimedia/graphics/andrew/mcas/2015/rankings/"
} else cache_url <- cache_url

cat("\nPlease select the spreadsheet with the raw districts data \n")
districts_file <- file.choose()
districts_from_xl <- read.xlsx(districts_file,sheet=1,colNames=TRUE)
cat("\nPlease wait...  \n")

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

districts_from_xl$district_name_url <- paste(url_base, districts_from_xl$short, sep="")
districts_from_xl$district_name_url <- paste("<a href='", districts_from_xl$district_name_url, sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, ".xml'>", sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, districts_from_xl$district_name, sep="")
districts_from_xl$district_name_url <- paste(districts_from_xl$district_name_url, "</a>", sep="")


grade <- paste("grade_", yr_abbrv, sep="")
etotal <- paste("etotal_perf_", yr_abbrv, sep="")
mtotal <- paste("mtotal_perf_", yr_abbrv, sep="")
stotal <- paste("stotal_perf_", yr_abbrv, sep="")
e_AandP <- paste("e_AandP_", yr_abbrv, sep="")
m_AandP <- paste("m_AandP_", yr_abbrv, sep="")
s_AandP <- paste("s_AandP_", yr_abbrv, sep="")

district_summary <- districts_from_xl[c("short","District_Name", grade,
                                    etotal, mtotal, stotal, e_AandP, m_AandP, s_AandP, "echange_type", 
                                    "mchange_type", "schange_type", "district_name_url")]

colnames(district_summary) <- c("short","district_name", "grade", "etotal", 
                              "mtotal", "stotal", "e_AandP", "m_AandP", "s_AandP", 
                              "echange_type", "mchange_type", "schange_type",
                              "district_name_url")

district_summary <- district_summary[- grep("stateresults", district_summary$short),]


district_summary$etotal <- as.numeric(as.character(district_summary$etotal))
district_summary$mtotal <- as.numeric(as.character(district_summary$mtotal))
district_summary$stotal <- as.numeric(as.character(district_summary$stotal))

district_summary$e_AandP <- as.numeric(as.character(district_summary$e_AandP))
district_summary$m_AandP <- as.numeric(as.character(district_summary$m_AandP))
district_summary$s_AandP <- as.numeric(as.character(district_summary$s_AandP))

district3rd <- subset(district_summary, grade=="03")
#write.csv(district3rd, "district3rd.csv")
district4th <- subset(district_summary, grade=="04")
#write.csv(district4th, "district4th.csv")
district5th <- subset(district_summary, grade=="05")
#write.csv(district5th, "district5th.csv")
district6th <- subset(district_summary, grade=="06")
#write.csv(district6th, "district6th.csv")
district7th <- subset(district_summary, grade=="07")
#write.csv(district7th, "district7th.csv")
district8th <- subset(district_summary, grade=="08")
#write.csv(district8th, "district8th.csv")
district10th <- subset(district_summary, grade=="10")
#write.csv(district10th, "district10th.csv")
districtAL <- subset(district_summary, grade=="AL")



#totals in ranking here
district3rd$e_total <- sum(!is.na(district3rd$e_AandP))
district3rd$m_total <- sum(!is.na(district3rd$m_AandP))
district4th$e_total <- sum(!is.na(district4th$e_AandP))
district4th$m_total <- sum(!is.na(district4th$m_AandP))
district5th$e_total <- sum(!is.na(district5th$e_AandP))
district5th$m_total <- sum(!is.na(district5th$m_AandP))
district5th$s_total <- sum(!is.na(district5th$s_AandP))
district6th$e_total <- sum(!is.na(district6th$e_AandP))
district6th$m_total <- sum(!is.na(district6th$m_AandP))
district7th$e_total <- sum(!is.na(district7th$e_AandP))
district7th$m_total <- sum(!is.na(district7th$m_AandP))
district8th$e_total <- sum(!is.na(district8th$e_AandP))
district8th$m_total <- sum(!is.na(district8th$m_AandP))
district8th$s_total <- sum(!is.na(district8th$s_AandP))
district10th$e_total <- sum(!is.na(district10th$e_AandP))
district10th$m_total <- sum(!is.na(district10th$m_AandP))
district10th$s_total <- sum(!is.na(district10th$s_AandP))
districtAL$e_total <- sum(!is.na(districtAL$e_AandP))
districtAL$m_total <- sum(!is.na(districtAL$m_AandP))
districtAL$s_total <- sum(!is.na(districtAL$s_AandP))


#ok, actual ranking here

district3rd$e_rank <- rank(-district3rd$e_AandP, na.last="keep", ties.method="min")
district3rd$m_rank <- rank(-district3rd$m_AandP, na.last="keep", ties.method="min")
district4th$e_rank <- rank(-district4th$e_AandP, na.last="keep", ties.method="min")
district4th$m_rank <- rank(-district4th$m_AandP, na.last="keep", ties.method="min")
district5th$e_rank <- rank(-district5th$e_AandP, na.last="keep", ties.method="min")
district5th$m_rank <- rank(-district5th$m_AandP, na.last="keep", ties.method="min")
district5th$s_rank <- rank(-district5th$s_AandP, na.last="keep", ties.method="min")
district6th$e_rank <- rank(-district6th$e_AandP, na.last="keep", ties.method="min")
district6th$m_rank <- rank(-district6th$m_AandP, na.last="keep", ties.method="min")
district7th$e_rank <- rank(-district7th$e_AandP, na.last="keep", ties.method="min")
district7th$m_rank <- rank(-district7th$m_AandP, na.last="keep", ties.method="min")
district8th$e_rank <- rank(-district8th$e_AandP, na.last="keep", ties.method="min")
district8th$m_rank <- rank(-district8th$m_AandP, na.last="keep", ties.method="min")
district8th$s_rank <- rank(-district8th$s_AandP, na.last="keep", ties.method="min")
district10th$e_rank <- rank(-district10th$e_AandP, na.last="keep", ties.method="min")
district10th$m_rank <- rank(-district10th$m_AandP, na.last="keep", ties.method="min")
district10th$s_rank <- rank(-district10th$s_AandP, na.last="keep", ties.method="min")
districtAL$e_rank <- rank(-districtAL$e_AandP, na.last="keep", ties.method="min")
districtAL$m_rank <- rank(-districtAL$m_AandP, na.last="keep", ties.method="min")
districtAL$s_rank <- rank(-districtAL$s_AandP, na.last="keep", ties.method="min")



#district3rd <- read.csv("district3rd.csv")
#district4th <- read.csv("district4th.csv")
#district5th <- read.csv("district5th.csv")
#district6th <- read.csv("district6th.csv")
#district7th <- read.csv("district7th.csv")
#district8th <- read.csv("district8th.csv")
#district10th <- read.csv("district10th.csv")

icons1 <-read.csv ("../raw_data/ze_icons1.csv")
icons2 <-read.csv ("../raw_data/ze_icons2.csv")
icons3 <-read.csv ("../raw_data/ze_icons3.csv")

district3rd <- join(district3rd, icons1, by="echange_type")
district3rd <- join(district3rd, icons2, by="mchange_type")

district4th <- join(district4th, icons1, by="echange_type")
district4th <- join(district4th, icons2, by="mchange_type")

district5th <- join(district5th, icons1, by="echange_type")
district5th <- join(district5th, icons2, by="mchange_type")
district5th <- join(district5th, icons3, by="schange_type")

district6th <- join(district6th, icons1, by="echange_type")
district6th <- join(district6th, icons2, by="mchange_type")

district7th <- join(district7th, icons1, by="echange_type")
district7th <- join(district7th, icons2, by="mchange_type")

district8th <- join(district8th, icons1, by="echange_type")
district8th <- join(district8th, icons2, by="mchange_type")
district8th <- join(district8th, icons3, by="schange_type")

district10th <- join(district10th, icons1, by="echange_type")
district10th <- join(district10th, icons2, by="mchange_type")
district10th <- join(district10th, icons3, by="schange_type")

districtAL <- join(districtAL, icons1, by="echange_type")
districtAL <- join(districtAL, icons2, by="mchange_type")
districtAL <- join(districtAL, icons3, by="schange_type")

district_3 <- district3rd[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(district_3) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
district_4 <- district4th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(district_4) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
district_5 <- district5th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon",
                            "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(district_5) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image", 
                          "science_rank", "science_tested", "science_score", "science_image")
district_6 <- district6th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]
colnames(district_6) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")
district_7 <- district7th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon")]

colnames(district_7) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image")

district_8 <- district8th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon",
                            "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(district_8) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image", 
                          "science_rank", "science_tested", "science_score", "science_image")

district_10 <- district10th[c("district_name_url", "e_rank", "etotal", "e_AandP", "e_icon", 
                            "m_rank", "mtotal", "m_AandP", "m_icon",
                            "s_rank", "stotal", "s_AandP", "s_icon")]
colnames(district_10) <- c("school_name", "english_rank", "english_tested", "english_score", "english_image", 
                          "math_rank", "math_tested", "math_score", "math_image", 
                          "science_rank", "science_tested", "science_score", "science_image")


dir.create("../output", showWarnings=FALSE)
dir.create("../output/district_rankings", showWarnings=FALSE)

dummy_2 <- readLines("../raw_data/district_rankings_2.txt", encoding="UTF-8")
dummy_3 <- readLines("../raw_data/district_rankings_3.txt", encoding="UTF-8")

dummy_2 <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas/rankings/", cache_url, dummy_2)
dummy_3 <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas/rankings/", cache_url, dummy_3)
dummy_2 <- sub("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas", index_base, dummy_2)
dummy_3 <- sub("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas", index_base, dummy_3)

dummy_2 <- gsub("WHATYEAR", yr, dummy_2)
dummy_3 <- gsub("WHATYEAR", yr, dummy_3)


districttojson <- toJSONArray(district_3)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district3rd.jsonp")
school_text <- gsub("WHATGRADE", "3rd", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "district3rd", school_text)
write(school_text, "../output/district_rankings/district3rd.txt")

districttojson <- toJSONArray(district_4)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district4th.jsonp")
school_text <- gsub("WHATGRADE", "4th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "district4th", school_text)
write(school_text, "../output/district_rankings/district4th.txt")

districttojson <- toJSONArray(district_5)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district5th.jsonp")
school_text <- gsub("WHATGRADE", "5th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "district5th", school_text)
write(school_text, "../output/district_rankings/district5th.txt")

districttojson <- toJSONArray(district_6)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district6th.jsonp")
school_text <- gsub("WHATGRADE", "6th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "district6th", school_text)
write(school_text, "../output/district_rankings/district6th.txt")

districttojson <- toJSONArray(district_7)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district7th.jsonp")
school_text <- gsub("WHATGRADE", "7th", dummy_2)
school_text <- gsub("SCHOOLRANKINGJSONP", "district7th", school_text)
write(school_text, "../output/district_rankings/district7th.txt")

districttojson <- toJSONArray(district_8)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district8th.jsonp")
school_text <- gsub("WHATGRADE", "8th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "district8th", school_text)
write(school_text, "../output/district_rankings/district8th.txt")

districttojson <- toJSONArray(district_10)
districttojson <- paste(districttojson, "})")
districttojson <- paste('scores({ "mcas": ', districttojson)
write(districttojson, "../output/district_rankings/district10th.jsonp")
school_text <- gsub("WHATGRADE", "10th", dummy_3)
school_text <- gsub("SCHOOLRANKINGJSONP", "district10th", school_text)
write(school_text, "../output/district_rankings/district10th.txt")

print("Done! You can find the jpts and jsonp files at '../output/district_rankings/' ")