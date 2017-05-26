rm(list=ls(all=TRUE))

setwd("C:/Users/nicol/Documents/GitHub/dbExplore")


#raw=as.data.frame(readxl::read_excel("controlled vocabulary_23may2017_jfl.xlsx",sheet = "rawNames"))
raw=read.csv("variables_cont.csv")
raw=unique(raw)
#categories=as.data.frame(readxl::read_excel("controlled vocabulary_23may2017_jfl.xlsx",sheet = "categories"))
categories = as.data.frame(readxl::read_excel("dbInput_cont.xlsx", sheet = "categories"))
j="alkalinity"
lvl="Lvl2"
raw$Lvl1=NA
raw$Lvl2=NA
for (j in unique(categories[,lvl])) {

  # for each category and the associated patterns to look for
  pattTemp = paste(categories[categories[,lvl]==j, "Keywords"], collapse = "|")
  #pattTemp = paste(unlist(pattTemp), collapse = "|")
  #rem = grep(pattern = pattRem, contr[,1], ignore.case = TRUE)
  colsTemp = grep(pattern = pattTemp, raw[,1],ignore.case = TRUE)

  if (length(colsTemp) == 0) {next}

  raw[colsTemp,"Lvl1"] = categories[categories[,lvl]==j,"Lvl1"]
  raw[colsTemp,"Lvl2"] = categories[categories[,lvl]==j,"Lvl2"]

}
write.csv(raw,"ctrlVocab.csv")
