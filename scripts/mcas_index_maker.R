
library(openxlsx)
library(plyr)

cat("\n Have you already prebuilt the JPT and XML shells based on last year's district list? \n1. Check to see what new JPTs and XMLs you might need to make due to new districts \n2. Do you just want the index page code? \nType 1 to check what new school districts popped up between this year and last or \nType 2 to generate the code for the index only. Press enter. ")
step_index <- readline("\n1 or 2?  ")

if (step_index==1) {
  dummy_text <- readLines("../raw_data/dummy_schools.txt", encoding="UTF-8")
  
  cat("\nWhere are you uploading the .jsonp and .js files? (ie. 'http://cache.boston.com/multimedia/graphics/andrew/mcas/2015') \n")
  cache_url <- readline("\nType out the root address. Don't end with '/'  ")
  old_list <- read.csv('../raw_data/district_names_list_2014.csv')
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
    
  no_dupes <- subset(districts_from_xl, !duplicated(districts_from_xl$short))
  
  how_many_new <- join(no_dupes, old_list, by="short")
  
  the_new <- subset(how_many_new, is.na(how_many_new$list))
  
  the_new$index <- NULL
  the_new$district_name <- the_new$District_Name
  the_new$district_name_full <- the_new$District_Name
  the_new$list <- paste(the_new$short, "_list", sep="")
  
  dir.create("../output", showWarnings=FALSE)
  dir.create("../output/new_list", showWarnings=FALSE)
  
  write.csv(the_new, "../output/new_list/the_inconsistencies_list.csv")
  mcas_list <- the_new
  
  dir.create("../output", showWarnings=FALSE)
  dir.create("../output/jpt_code", showWarnings=FALSE)
  dir.create("../output/jpt_code/inconsistencies", showWarnings=FALSE)
  
  list_length <- 1:length(mcas_list$district_name)
  print("Please wait... (If you want to alter the original JPT being cloned, look up dummy_text.txt in the raw_data folder)")
  for (i in list_length) {
    
    full_name <- mcas_list$district_name_full[i]
    just_name <- mcas_list$district_name[i]
    mcas_summary <- mcas_list$short[i]
    school_list <- mcas_list$list[i]
    text_one <- sub("FULL_SCHOOL_NAME_PLZ", full_name, dummy_text)
    text_two <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas", cache_url, text_one)
    text_three <- sub("MCAS_SUMMARY", mcas_summary, text_two)
    text_four <- sub("SCHOOLS_LIST", school_list, text_three)
    text_five <- sub("YEARZ", yr, text_four)
    
    file_dir <- ("../output/jpt_code/inconsistencies/")
    mcas_summary <- paste(mcas_summary,".txt", sep="")
    file_name <- paste(file_dir, mcas_summary, sep="")
    
    write(text_five, file_name) }
    cat("\nThere are ", nrow(the_new)," inconsistencies with the new list and the old one. \nThat means some districts have closed or opened. \nGet the list at ../output/new_list/the_inconsistencies_list.csv \n the names of the jpts\\xmls to create or can be found in the 'short' column. \nThe new jpt code can be found in ../output/jpt_code/inconsistencies")
  
  } else if (step_index==2) {
    cat("\nWhat is the url base where all the district page XMLs? \nIf you followed the file structure from 2013, it should be \nhttp://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/2015/districts/xml/ \nType 1 and enter if the file structure is identical to 2014. \nOtherwise, type out the base URL where the live XMLs can be found (don't forget the last '/')")
    url_choice <- readline("\n  ")
    
    cat("\nWhere are you uploading the .jsonp and .js files? \n(ie. 'http://cache.boston.com/multimedia/graphics/andrew/mcas/2015/') \n")
    cache_url <- readline("\nType out the root address. DON'T FORGET THE '/' AT THE END ")
    
    
    if (url_choice==1) {
      index_base <- paste("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/", yr, sep="")
      url_base <- paste(index_base, "/districts/xml/", sep="")
    } else { 
      url_base <- url_choice
      index_base <- paste("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/", yr, sep="")
    }
    
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
   
    index <- schools_from_xl[c("school_name_url", "district_name_url", "Lowest.Grade", "Highest.Grade", "OrgType.Description", "short")]
    colnames(index) <- c("school_names", "district_name_full", "lowest_grade", "highest_grade", "org_type","short")
  
    index_json <- toJSONArray(index)
    index_json <- paste(index_json, "})")
    index_json <- paste('index({ "mcas": ', index_json)
    dir.create("../output", showWarnings=FALSE)
    dir.create("../output/index", showWarnings=FALSE)
    
    write(index_json,"../output/index/index.jsonp")
    index_text <- readLines("../raw_data/index_jpt.txt", encoding="UTF-8")
    index_text <- sub("YEARZ", yr, index_text)
    index_text <- gsub("http://cache.boston.com/multimedia/graphics/andrew/mcas/", cache_url, index_text)
    index_text <- sub("http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas", index_base, index_text)
    
    write(index_text, "../output/index/index.txt", sep="")
    
    print("Done! The jsonp and txt files are in the /output/index/ folder")
    
    } else print("Sorry, that input wasn't recognized.")


#exported_list <- schools_from_xl[c("org_code", "school_name", "district_name", "concatenated")]

#write.csv(index, "index.csv")
