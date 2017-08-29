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
i=8
DBexplore<- function(inputFile = "dbInput_cont.xlsx",dirPath=NA, startAt = 1,append = F,lineSkip=0,lvl="Lvl2")
  {

    # inputs----

    #-oriDir=getwd()

    # input a excel, but should eventually be a csv
    input = LtoC(read.csv("inputs/dbInput.csv",na.strings = ""))

    #input categories to identified should also be a csv
    categories = LtoC(read.csv("inputs/categories.csv"))

    exclu =  read.csv("inputs/exclu.csv",header=F)


    # create the final output file and table----

    # decide if you append to an existing input csv or you create a new one
    if (append) {
      outp = read.csv("output.csv", row.names = 1)
      nskip = length(unique(outp[, 2]))
    } else {
      nskip = 0
    }


    #-output <- data.frame(path=character(),
      #-                   state=character(),
        #-                 category=character(),
          #-               year=numeric(),
            #-             lvl1=character(),
              #-           varNames=character(),
                #-         varDepth=character(),
                  #-       nbObs=numeric(),
                    #-     nbLakes=numeric(),
                      #-   nbDepths=numeric(),
                        #- nbYears=numeric(),
                         #-startYear=numeric(),
                         #-endYear=numeric())

   #- output$category=LtoC(output$category)
    #-output$lvl1=LtoC(output$lvl1)
    #-output$path=LtoC(output$path)
    #-output$varNames=LtoC(output$varNames)
    #-output$varDepth=LtoC(output$varDepth)
    #-output$state=LtoC(output$state)


    # if you want to run the loop for a limited number of db starting at x
    if (append) {startAt = nskip + 1}


i=1
    for (i in startAt:nrow(input)) {
      count = 1

      # create the final output table
      #-output <- output [0,]
      #input=as.data.frame(input)


      #change working directory
      #if(!is.na(dirPath)){setwd(dirPath)}



        sheetTemp = do.call(rbind, strsplit(LtoC(input[i, "sheet"]), ";"))
        if(!is.na(sheetTemp)){if(sheetTemp=="NA"){sheetTemp=NA}}
        lineSkip=input[i, "lineSkip"]

        if(length(grep("csv",LtoC(input[i, "path"])))!=0){fileType="csv"}
        if(length(grep("xl",LtoC(input[i, "path"])))!=0){fileType="xls"}

        # For xlsx if multiple sheets need to be rbind, sep = ';' and the
        # columns of the first sheet are used in the rbind
        # time the loop

        #log input
        fileName=paste0("logs/",as.character(Sys.Date()),".log")
        cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")


        time=gsub(" EDT","",gsub(" ","_",Sys.time()))

        if (fileType == "xls") {
            first = T
            for (w in sheetTemp) {
                if (!is.na(w)) {
                  sheet = w
                } else {
                  sheet = 1
                }
                if (first)
                {

                db = readxl::read_excel(paste(dirPath,"/",LtoC(input[i, "path"]),sep=""), sheet = sheet,skip = lineSkip)}
                if (!first)
                  {db = rbind(db, readxl::read_excel(paste(dirPath,"/", LtoC(input[i,
                    "path"]), sep = ""), sheet = sheet,skip = lineSkip)[, colnames(db)])}
                first = F
            }
        }

        if (fileType == "csv")
            {db = read.csv(paste(dirPath,"/", LtoC(input[i, "path"]), sep = ""),
                1 ,skip = lineSkip,na.strings = c("", "NA"))}

        db=as.data.frame(db)



        #if (!is.na(input[i, "wideVar"])) {
         #   if (!is.na(input[i, "Zsample"])) {

          #      db = db[rowSel, c(LtoC(input[i, "stationID"]), LtoC(input[i,
           #       "dateID"]), LtoC(input[i, "wideVar"]), LtoC(input[i,
            #      "Zsample"]), LtoC(input[i, "wideResults"]))]
            #} else {
             #   db = db[rowSel, c(LtoC(input[i, "stationID"]), LtoC(input[i,
              #    "dateID"]), LtoC(input[i, "wideVar"]), LtoC(input[i,
               #   "wideResults"]))]
            #}
            #db= LtoW(db,input,i)
        #}


        # if(!is.na(input[i,'wideVar'])){
        # db=db[,c(LtoC(input[i,'stationID']),LtoC(input[i,'dateID']),LtoC(input[i,'wideVar']),LtoC(input[i,'wideResults']))]
        # db <- reshape(db, timevar = LtoC(input[i,'wideVar']), idvar =
        # c(LtoC(input[i,'stationID']),LtoC(input[i,'dateID'])),direction =
        # 'wide') }

        if (!is.na(input[i, "NAvalue"]))
            {db[db == input[i, "NAvalue"][[1]]] = NA}

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

        dateId=input$dateID[i]

        db = db[rowSums(is.na(db)) != ncol(db), ]  #remove columns with only NAs
        db = db[, colSums(is.na(db)) != nrow(db)]  #remove rows with only NAs
        db = db[!is.na(db[, dateId]), ]  #remove rows with only NAs

        db[, dateId]=unlist(strsplit(LtoC(db[, dateId]), " "))

        tryYMD <- tryCatch(lubridate::ymd(db[, dateId]),error=function(e) e, warning=function(w) w)
        tryMDY <- tryCatch(lubridate::mdy(db[, dateId]),error=function(e) e, warning=function(w) w)
        tryDMY <- tryCatch(lubridate::dmy(db[, dateId]),error=function(e) e, warning=function(w) w)


        if(!is( tryYMD ,"warning")){db[, dateId]= tryYMD }
        if(!is( tryMDY ,"warning")){db[, dateId]= tryMDY }
        if(!is( tryDMY ,"warning")){db[, dateId]= tryDMY }


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
        lvl="Lvl2"
        cats=LtoC(unique(categories[,lvl]))
        lvl1=(categories[,"Lvl1"])

      #-  j=selCat[1]
        c2=1
        parameters=data.frame(param=NA,ctrl=NA,KeyW=NA)
        #params=unique(db[,input$wideVar[i]])
        c3=1

        if(!is.na(input[i, "wideVar"])){
          searchVec=LtoC(unique(db[,LtoC(input$wideVar[i])]))
        }
        if(is.na(input[i, "wideVar"])){
          searchVec=colnames(db)
        }


      for (j in cats) {
            # loop to search for the kerwords in order, maybe switch from | to  ; between keywords

            # create a list of pattern to look for
            pattTemp = paste(categories[categories[,lvl]==j, "Keywords"], collapse = "|")
            pattTemp = gsub("\\|",";",pattTemp)
            pattList=unlist(strsplit(pattTemp,";"))

            # loop to look for patterns, first before
            colsTemp=NULL
            for(r in 1:length(pattList))
            {
              colsTemp = grep(pattern = pattList[r], searchVec , ignore.case = TRUE)
              if(length(colsTemp)>0){break}
            }

            if (length(colsTemp) == 0) {c2=c2+1;next}

            # store params name, category and keyword in params
            parameters[c3,"param"]=searchVec[colsTemp[1]]
            parameters[c3,"KeyW"]=pattList[r]
            parameters[c3,"ctrl"]=j
            c3=c3+1

            #log names
            cat(paste("\t",j,":",pattList[r]), file=fileName, append=T, sep = "\n")
            cat(paste("\t\t",searchVec[colsTemp]), file=fileName, append=T, sep = "\n")
        }


            #-output[count, "path"] = LtoC(input[i, "path"])
            #-output[count, "category"] = as.character(j)
            #-output[count, "lvl1"] =  as.character(lvl1[c2])

            #-c2=c2+1



            #-kw.colap=paste(parameters$KeyW,collapse = "|")
            #-rowSel=grep(kw.colap,LtoC(db[,LtoC(input[i, "wideVar"])]),ignore.case = TRUE,perl = T)

            #add a dummy row



        if(!is.na(input[i, "wideVar"])){
          pattern=gsub("\\(","\\\\(",paste(parameters$param,collapse="|"))
          pattern=gsub("\\)","\\\\)",pattern)
          dbSub=db[grep(pattern,db[,LtoC(input$wideVar[i])],perl=T),]
        }

        if(is.na(input[i, "wideVar"])){
          pattern=gsub("\\(","\\\\(",paste(parameters$param,collapse="|"))
          pattern=gsub("\\)","\\\\)",pattern)
          dbSub=db[,c(LtoC(input$stationID[i]),LtoC(input$dateID[i]),grep(pattern,colnames(db),perl=T,value=T)),]
          input$wideVar[i]="parameter"
          input$wideResults[i]="value"
          dbSub=tidyr::gather(dbSub,parameter,value,grep(pattern,colnames(dbSub),perl=T),na.rm=T)
        }

                dbSub$dum=1

                #-inputSub=db[rowSel,]

                #-stat=input$stationID[i]
                #-wide=i

               colnames(dbSub)[colnames(dbSub)%in%LtoC(input$stationID[i])]="stationID"
               dbSub$year =lubridate::year(dbSub[,LtoC(input$dateID[i])])


               output= as.data.frame(dplyr::summarise(dplyr::group_by_at(dbSub,dplyr::vars(LtoC(input$wideVar[i]))),
                                                    nObs=sum(dum),
                                                    nbLakes=length(unique(stationID)),
                                                    nbYears=length(unique(year)),
                                                    startYear=min(year),
                                                    endYear=max(year)))

               output=merge(parameters,output,by.y = LtoC(input$wideVar[i]), by.x="param")

               #-output$state = input[i, "state"]
               #-output$path = input[i, "path"]

               output=data.frame(path=LtoC(input[i, "path"]),state=LtoC(input[i, "state"]),output)


                #if (mat[k, "nbLakes"] == 0)
                # { mat[k, "nbLakes"] = 1}

                #-tempMat$uniM = paste(tempMat[, stationId], tempMat[, dateId],
                  #-sep = ":")

                # calcule the number of unique depth for one lake at one depth then
                # average for the database limit the estimate of depths if not will
                # take forever
                #if (!is.na(Zsample)) {
                 #for (h in tempMat$uniM) {
                    #tempZ[c] = length(unique(tempMat[tempMat$uniM == h,
                    #Zsample]))
                    #c = c + 1
                   # if (c > 100)
                  #    {break}
                 # }
                #}
            #-}


           # if (!is.na(Zsample)) {
            #   { output[count, "nbDepths"] = mean(tempZ)}
            #} else {
             #   output[count, "nbDepths"] = 1
            #}

            #output[count, "varNames"] = paste(unlist(rownames(mat)), collapse = "; ")
            #output[count, "varDepth"] = Zsample
            #output[count, "ID"]=i

            #-count = count + 1
        #-}

        #-print(i)
        #-setwd(oriDir)
      cnames=gsub(paste(LtoC(input[i, "wideResults"]),".",sep=""), "",colnames(db))
        # need to fix the append component
        if(i==startAt){
          write.table(output, "output/output.csv",sep = ",",row.names = F)
         #- write.table(as.matrix(cnames), "variables_cont.csv",sep = ",",row.names = F)
          }
        if(i!=startAt){
        write.table(output, "output/output.csv", sep = ",", col.names = F, append = T,row.names = F)}
#-        write.table(as.matrix(cnames), "variables_cont.csv", sep = ",", col.names = F, append = T,row.names = F)
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
LtoC <- function(x) {
  if(!is.null(dim(x))){
    for (i in 1:ncol(x)){
      if(class(x[,i])=="factor")x[,i]=as.character(x[,i])}
    }
  if(is.null(dim(x))){x=as.character(x)}
  return(x)
  }

