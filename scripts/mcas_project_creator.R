
  
mcas_generator <- function(yr) {
  
  if (yr < 2000) {yr <- yr+2000}
  yr_abbrv <- yr-2000
  yr <<- yr
  
  setwd("/Users/tran/Desktop/mcas_analysis/scripts")
  
  source('jsoner.r')
  
  cat("\nWhat stage are you at in MCAS presentation? \nPre-planning? Or do you have the official set of new scores?\n\n1. Early. I need to start building the pages in Methode\n2. I have the spreadsheet of the latest scores.\n")
  step <- readline("\nType 1 or 2 and press enter  ")
  
  if (step == 1) {
    cat("\n1. Do you need a list of last year's districts to start building empty jpts and xmls?\n2. Do you want the code that goes in the JPTs?\n")
    step_1 <- readline("\nType 1 for list or 2 to produce the code  ")
    if (step_1 ==1) {
      old_list <- read.csv('../raw_data/district_names_list_2014.csv')
      dir.create("../output", showWarnings=FALSE)
      dir.create("../output/new_list", showWarnings=FALSE)
      write.csv(old_list, "../output/new_list/the_list.csv")
      file_directory <- setwd("..")
      file_name <- paste(file_directory,"/output/new_list/the_list.csv", sep="")
      cat("The list can be find at:", file_name, "\nCreate the files using the values in the 'short' column. Like, lawrence.jpt and lawrence.xml")
    } else if (step_1 == 2) {
      source('mcas_jpt_maker.r')
    } else print("Sorry, I don't recognize what you've input.")
    
  } else if (step == 2) {
    cat("\n1. Do you need to make the index page? \n2. Do you need to make the district rankings pages? \n3. Do you need to make the school rankings pages? \n4. Do you need to make the district and schools summary pages? \n5. Do you need the files to create the MCAS search widgets? \n")
    step_2 <- readline("\nType a number between 1 and 4 and press enter  ")
    if (step_2 == 1){
      source('mcas_index_maker.r')
    } else if (step_2 ==2){
      source('mcas_district_ranker.r')
    } else if (step_2 ==3) {
      source('mcas_school_ranker.r')
    } else if (step_2 ==4) {
      source('mcas_summary_maker.r')
    } else if (step_2 ==5) {
      print("5")
    } else print("Sorry, I don't recognize what you've input.")
    
  } else print("Sorry, I don't recognize what you've input.")
  
}


#The list of names can be found at 'new_list.csv'")
#or print ("You're all set. You don't need to add new jpts/xmls because the districts have remained the same. That never happens')

#district_all <- merge(districts, district_id, by="district_name")

#mcas <- read.csv("mcas_test_ranked.csv")
#mcas_json <- toJSONArray(mcas)
#mcas_test <- paste(mcas_json, "})")

  
#  library(rjson)

#index <- read.csv("index_needs_urls.csv")

#index_json <- toJSONArray(index)
#index_json <- paste(index_json, "})")
#index_json <- paste('index({ "mcas": ', index_json)
#write(index_json,"index3.jsonp")

# fun <- function() {
#  ANSWER <- readline("Are you a satisfied R user? ")
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
#  if (substr(ANSWER, 1, 1) == "n")
#    cat("This is impossible.  YOU LIED!")
#  else
#    cat("I knew it.")
#}

#if(interactive()) fun()
