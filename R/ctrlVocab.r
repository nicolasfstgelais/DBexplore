rm(list=ls(all=TRUE))

setwd("C:/Users/nicol/Documents/GitHub/dbExplore")


raw=as.data.frame(readxl::read_excel("controlled vocabulary_23may2017_jfl.xlsx",sheet = "rawNames"))
categories=as.data.frame(readxl::read_excel("controlled vocabulary_23may2017_jfl.xlsx",sheet = "categories"))

j=1

for (j in 1:nrow(categories)) {

  # for each category and the associated patterns to look for
  categTemp = rownames(categories)[j]
  pattTemp = categories[j, "Keywords"]
  #pattTemp = paste(unlist(pattTemp), collapse = "|")
  #rem = grep(pattern = pattRem, contr[,1], ignore.case = TRUE)

    colsTemp = grep(pattern = pattTemp, raw[,"Raw variable names"],ignore.case = TRUE)

  if (length(colsTemp) == 0) {next}

  raw[colsTemp, "Category Lvl1"] = categories[j,"Category Lvl1"]
    raw[colsTemp, "Category Lvl2"] = categories[j,"Category Lvl2"]

}
