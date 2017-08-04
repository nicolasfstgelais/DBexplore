#' dbExplore main function
#'
#' This function is designed to explore databases and summarize
#' the spatial and temporal coverage of pre-selected varaibles (need to fill the input.xls file)
#' @param inputFile inputFile filename, needs to be in the working directory
#' @param dirPath   the path to the databases (see input file)
#' @param startAt at which line to start in input
#' @param append = F,
#' @param lineSkip
#' @param lvl
#' @keywords cats
#' @export
#' @examples
#' dbSurvey ()

DBexplore<- function(inputFile = "dbInput.xlsx",dirPath=NA, startAt = 1,append = F,lineSkip=0,lvl="Lvl2")
  {

    oriDir=getwd()

    # input a excel, but should eventually be a csv
    input = readxl::read_excel(inputFile, sheet = "dbInput")

    #input categories to identified should also be a csv
    categories = as.data.frame(readxl::read_excel(inputFile, sheet = "categories"))
    #categories=firstAsRowNames(categories)


    exclu = readxl::read_excel(inputFile, sheet = "exclu", col_names = F)

    # decide if you append to an existing input csv or you create a new one

    if (append) {
        outp = read.csv("output.csv", row.names = 1)
        nskip = length(unique(outp[, 2]))
    } else {
        nskip = 0
    }

    # create the final output table
    output <- data.frame(path=character(),
                         state=character(),
                         category=character(),
                         varNames=character(),
                         varDepth=character(),
                         nbObs=numeric(),
                         nbLakes=numeric(),
                         nbDepths=numeric(),
                         nbYears=numeric(),
                         startYear=numeric(),
                         endYear=numeric())

    output$category=LtoC(output$category)
    output$path=LtoC(output$path)
    output$varNames=LtoC(output$varNames)
    output$varDepth=LtoC(output$varDepth)
    output$state=LtoC(output$state)


    i = 2
    # j=1

    # if you want to run the loop for a limited number of db starting at x
    if (append) {startAt = nskip + 1}
    # if(!append)startAt=93
i=1

    for (i in startAt:nrow(input)) {
      count = 1
      # create the final output table
      output <- output [0,]
      input=as.data.frame(input)


      #change working directory
      if(!is.na(dirPath)){setwd(dirPath)}

        sheetTemp = do.call(rbind, strsplit(LtoC(input[i, "sheet"]), ";"))
        if(!is.na(sheetTemp)){if(sheetTemp=="NA"){sheetTemp=NA}}
        lineSkip=input[i, "lineSkip"]

        # For xlsx if multiple sheets need to be rbind, sep = ';' and the
        # columns of the first sheet are used in the rbind
        if (input[i, "type"] == "xls") {
            first = T
            for (w in sheetTemp) {
                if (!is.na(w)) {
                  sheet = w
                } else {
                  sheet = 1
                }
                if (first)
                  {db = readxl::read_excel(paste(".\\",LtoC(input[i, "path"]),sep=""), sheet = sheet,skip = lineSkip)}
                if (!first)
                  {db = rbind(db, readxl::read_excel(paste("..\\", LtoC(input[i,
                    "path"]), sep = ""), sheet = sheet,skip = lineSkip)[, colnames(db)])}
                first = F
            }
        }

        if (input[i, "type"] == "csv")
            {db = read.csv(paste(".\\", LtoC(input[i, "path"]), sep = ""),
                1 ,skip = lineSkip,na.strings = c("", "NA"))}

        db=as.data.frame(db)

        # long db are trandformed to wide db
        if (!is.na(input[i, "wideVar"])) {
            if (!is.na(input[i, "Zsample"])) {
                db = db[, c(LtoC(input[i, "stationID"]), LtoC(input[i,
                  "dateID"]), LtoC(input[i, "wideVar"]), LtoC(input[i,
                  "Zsample"]), LtoC(input[i, "wideResults"]))]
            } else {
                db = db[, c(LtoC(input[i, "stationID"]), LtoC(input[i,
                  "dateID"]), LtoC(input[i, "wideVar"]), LtoC(input[i,
                  "wideResults"]))]
            }
            db= LtoW(db,input,i)
        }


        # if(!is.na(input[i,'wideVar'])){
        # db=db[,c(LtoC(input[i,'stationID']),LtoC(input[i,'dateID']),LtoC(input[i,'wideVar']),LtoC(input[i,'wideResults']))]
        # db <- reshape(db, timevar = LtoC(input[i,'wideVar']), idvar =
        # c(LtoC(input[i,'stationID']),LtoC(input[i,'dateID'])),direction =
        # 'wide') }

        if (!is.na(input[i, "NA"]))
            {db[db == input[i, "NA"][[1]]] = NA}

        Zsample = LtoC(input[i, "Zsample"])
        if (is.na(Zsample)) {
            Zsample = colnames(db)[grep("^(?=.*depth)(?!.*secchi)(?!.*max)(?!.*min)",
                colnames(db), ignore.case = TRUE, perl = T)][1]
        } else {
            Zsample = NA
        }

        # this is for db with only one
        if (is.na(input[i, "stationID"]) | input[i, "stationID"] == "NA") {
            db$stationId = "A"
            stationId = "stationId"
        } else {
            stationId = LtoC(input[i, "stationID"])
        }

        dateId = LtoC(input[i, "dateID"])


        db = db[rowSums(is.na(db)) != ncol(db), ]  #remove columns with only NAs
        db = db[, colSums(is.na(db)) != nrow(db)]  #remove rows with only NAs
        db = db[!is.na(db[, dateId]), ]  #remove rows with only NAs


        if (input[i, "dateFormat"] == "B")
            {db[, dateId] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[, dateId]),
                orders = "mdy")))}

        if (input[i, "dateFormat"] == "C") {
            db$date2 = NA
            db$date2[grep("/", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                dateId][grep("/", db[, dateId])]), orders = "mdy H:M")))
            db$date2[grep("-", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                dateId][grep("-", db[, dateId])]), orders = "ymd H:M")))
            db[, dateId] = db$date2
            db$date2 = NULL
        }

        if (input[i, "dateFormat"] == "D") {
            db$date2 = NA
            db$date2[grep("/", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                dateId][grep("/", db[, dateId])]), orders = "mdy")))
            db$date2[grep("-", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                dateId][grep("-", db[, dateId])]), orders = "ymd")))
            db[, dateId] = db$date2
            db$date2 = NULL
        }
        if (input[i, "dateFormat"] == "E")
            {db[, dateId] = LtoC(lubridate::ymd(LtoC(db[, dateId])))}

        if (input[i, "dateFormat"] == "F") {
            y = "YEAR"
            d = "DAY"
            m = "MONTH"
            db[, dateId] = lubridate::ymd((paste(db[, c(y)], db[, c(m)], db[, c(d)],
                sep = "-")))
        }

        if (input[i, "dateFormat"] == "G") {
            db[, dateId] = lubridate::ymd(lubridate::parse_date_time(LtoC(db[, dateId]), orders = "y"))
        }

        j = "Alkalinity"
        selCat=unique(categories[,lvl])
        j=selCat[2]
        for (j in selCat) {

            # for each category and the associated patterns to look for
            #categTemp = categories[categories[,lvl]==j,lvl]
            pattTemp = paste(categories[categories[,lvl]==j, "Keywords"], collapse = "|")
            #pattTemp = paste(unlist(pattTemp), collapse = "|
            pattRem = paste(unlist(exclu), collapse = "|")
            rem = grep(pattern = pattRem, colnames(db), ignore.case = TRUE)
            if (length(rem) > 0){
                colsTemp = grep(pattern = pattTemp, colnames(db)[-rem],
                  ignore.case = TRUE)} else {
                colsTemp = grep(pattern = pattTemp, colnames(db), ignore.case = TRUE)
            }
            if (length(colsTemp) == 0) {next}

            output[count, "path"] = LtoC(input[i, "path"])
            output[count, "category"] = j
            varOut = c("nbObs", "nbLakes", "nbYears", "startYear", "endYear")
            mat = matrix(NA, length(colsTemp), 5, dimnames = list(colnames(db)[colsTemp],
                varOut))

            tempZ = NA
            c = 1
            for (k in colnames(db)[colsTemp]) {

                # LtoC(db[!is.na(db[,k]),input[i,'Zsample']])
                #db[!is.na(db[, k]), ]
              # problem with tibble
                db=as.data.frame(db)

                mat[k, "nbObs"] = nrow(db[!is.na(db[, k]), ])
                mat[k, "nbLakes"] = length(unique(LtoC(db[!is.na(db[, k]),
                  stationId])))
                mat[k, "nbYears"] = length(unique(lubridate::year(((db[!is.na(db[,
                  k]), dateId])))))
                mat[k, "startYear"] = min(lubridate::year((db[!is.na(db[, k]),
                  dateId])), na.rm = T)
                mat[k, "endYear"] = max(lubridate::year((db[!is.na(db[, k]), dateId])),
                  na.rm = T)
                if (mat[k, "nbLakes"] == 0)
                 { mat[k, "nbLakes"] = 1}

                tempMat = db[!is.na(db[, k]), ]
                tempMat$uniM = paste(tempMat[, stationId], tempMat[, dateId],
                  sep = ":")

                # calcule the number of unique depth for one lake at one depth then
                # average for the database limit the estimate of depths if not will
                # take forever
                if (!is.na(Zsample)) {
                  for (h in tempMat$uniM) {
                    tempZ[c] = length(unique(tempMat[tempMat$uniM == h,
                      Zsample]))
                    c = c + 1
                    if (c > 100)
                      {break}
                  }
                }
            }

            if (nrow(mat) > 1)
                {mat = mat[order(mat[, "nbObs"], decreasing = T), ]}
            output[count, "nbObs"] = mat[1, "nbObs"]
            output[count, "nbLakes"] = mat[1, "nbLakes"]
            output[count, "nbYears"] = mat[1, "nbYears"]
            output[count, "startYear"] = mat[1, "startYear"]
            output[count, "endYear"] = mat[1, "endYear"]
            output[count, "endYear"] = mat[1, "endYear"]
            output[count, "state"] = input[i, "state"]
            if (!is.na(Zsample)) {
               { output[count, "nbDepths"] = mean(tempZ)}
            } else {
                output[count, "nbDepths"] = 1
            }

            output[count, "varNames"] = paste(unlist(rownames(mat)), collapse = "; ")
            output[count, "varDepth"] = Zsample
            output[count, "ID"]=i

            count = count + 1
        }

        print(i)
        setwd(oriDir)
        cnames=gsub(paste(input[i, "wideResults"],".",sep=""), "",colnames(db))
        # need to fix the append component
        if(i==1){
          write.table(output, "output_cont.csv",sep = ",",row.names = F)
          write.table(as.matrix(cnames), "variables_cont.csv",sep = ",",row.names = F)}
        if(i!=1){
        write.table(output, "output_cont.csv", sep = ",", col.names = F, append = T,row.names = F)}
        write.table(as.matrix(cnames), "variables_cont.csv", sep = ",", col.names = F, append = T,row.names = F)
        }

    #if (append) {output = rbind(outp, output)}
    return(output)
}


# need to work on this function far from optimal -> need input and i
LtoW <- function(db, input, i,size = 10000)
{
  count = 1
  while (count < nrow(db)) {
    if (any(colnames(db) %in% input[i, "Zsample"])) {
      idvar = c(LtoC(input[i, "stationID"]), LtoC(input[i, "dateID"]),
                LtoC(input[i, "Zsample"]))
    } else {
      idvar = c(LtoC(input[i, "stationID"]), LtoC(input[i, "dateID"]))
    }
    timevar = LtoC(input[i, "wideVar"])
    if (count == 1)
    {dbtemp = reshape(db[count:(count + size - 1), ], timevar = timevar,
                      idvar = idvar, direction = "wide")}
    if (count > 1)
    {dbtemp = plyr::rbind.fill(dbtemp, reshape(db[count:(count + size -
                                                           1), ], timevar = timevar, idvar = idvar, direction = "wide"))}
    print(paste(count, ":", nrow(dbtemp)))
    count = count + size
  }
  return(dbtemp)
}

firstAsRowNames <- function(mat)
{
  mat=as.data.frame(mat)
  rownames(mat) = mat[, 1]
  mat[, 1] = NULL
  return(mat)
}

# function to convert levels to numeric or characters
LtoN <- function(x) {as.numeric(as.character(x))}
LtoC <- function(x) {as.character(x)}

