mcas_list <- read.csv("../raw_data/district_names_list_2014.csv", stringsAsFactors=FALSE)
dummy_text <- readLines("../raw_data/dummy_schools.txt", encoding="UTF-8")

cat("\nWhere are you uploading the .jsonp and .js files? (ie. 'http://cache.boston.com/multimedia/graphics/andrew/mcas/2015') \n")
cache_url <- readline("\nType out the root address. Don't end with '/'  ")

dir.create("../output", showWarnings=FALSE)
dir.create("../output/jpt_code", showWarnings=FALSE)

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

file_dir <- ("../output/jpt_code/")
mcas_summary <- paste(mcas_summary,".txt", sep="")
file_name <- paste(file_dir, mcas_summary, sep="")

write(text_five, file_name)

}

wd <- getwd()
file_directory <- setwd("..")
file_directory <- getwd()

file_name <- paste(file_directory,"/output/jpt_code/", sep="")
cat("\nThe files can be found in ", file_name, "\nCopy and paste the contents of each text file into the matching jpt. \n(abington.txt -> abington.jpt) \nI recommend you place them in a file structure similar to 2014: http://www.bostonglobe.com/Page/Boston/2011-2020/WebGraphics/Metro/BostonGlobe.com/mcas/2015/districts/jpt/ ")
