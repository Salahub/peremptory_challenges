########################################

## THESIS DATA PROCESSING SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################

## PACKAGES ############################
library(readxl)
library(tm)
library(stringr)


## CONSTANTS ###########################

## start by defining file locations
ThesisDir <- "c:/Users/Chris/Documents/ETH Zurich/Thesis/Data"
SunshineFile <- paste0(ThesisDir, "/JurySunshineExcel.xlsx")
SunshineSheets <- excel_sheets(SunshineFile)

NorthCarFile <- paste0(ThesisDir,
                       "/Jury Study Data and Materials/NC Jury Selection Study Database6 Dec 2011.csv")

PhillyFile <- paste0(ThesisDir,
                     "/Voir Dire Data & Codebook/capital_venires.csv")

## next the factor level codes as given in the codebook and regularized here
## regularization: - political affiliation "N" replaced with "I" for all entries
LevRace <- sort(c("A","B","H","N","O","U","W"))
LevGen <- sort(c("F","M","U"))
LevPol <- sort(c("D","L","R","I","U"))

## create a charge tree with regex nodes to identify and clean charge text
chargeTree <- list("rape" = list("statutory", "first|1", "second|2"), "sex(?=.*offense)" = list("first|1", "second|2"),
                   "sex(?=.*offend)" = list("regis", "addr"), "murder" = list("first|1" = list("att"), "second|2" = list("att")),
                   "arson", "firearm" = list("pos", "disch"), "stole" = list("pos"),
                   "mari" = list("pos", "sell|sale", "man", "pwimsd"), "coca" = list("pos", "sell|sale", "man", "pwimsd"),
                   "cs" = list("pos", "sell|sale", "man", "pwimsd"), "hero" = list("pos", "sell|sale", "man", "pwimsd"),
                   "meth" = list("pos", "sell|sale", "man", "pwimsd"),
                   "oxycod" = list("pos", "sell|sale", "man", "pwimsd"), "mass" = list("pos"), "break" = list("enter"),
                   "assa" = list("serious bodily", "female", "strangul", "deadly", "official"),
                   "larceny" = list("motor", "felon", "merchant"), "false" = list("pretense"),
                   "driving" = list("impaired"), "kidnap" = list("first|1", "second|2"),
                   "robb" = list("dang"), "burg" = list("first|1", "second|2"), "indec" = list("liber"),
                   "embez", "manslaughter" = list("inv"), "flee" = list("arrest"),
                   "abuse|cruelty" = list("child", "anim"), "identity" = list("theft"))

## create a list of variables which can sensibly be summarized by trial
TrialVars <- c("TrialNumberID", "DateOutcome", "JudgeID", "DefAttyType", "VictimName",
               "VictimRace", "VictimGender", "CrimeLocation", "PropertyType",
               "ZipCode.Trials", "StateTotalRemoved", "DefenseTotalRemoved",
               "CourtTotalRemoved", "JDistrict", "JName", "JRace", "JGender",
               "JPoliticalAff", "JVoterRegYr", "JYrApptd", "JResCity", "JResZip",
               "ChargeTxt", "Outcome", "Sentence.FullSunshine", "DefendantID.FullSunshine",
               "DefendantID.DefendantToTrial", "DefRace", "DefGender", "DefDOB", "DefAttyID",
               "DefAttyName", "DCRace", "DCGender", "DCPoliticalAff", "DCYrRegVote",
               "DCYrLicensed", "DCResideCity", "DCResideZip", "ProsecutorID", "ProsName",
               "ProsRace", "ProsGender", "ProsPoliticalAff", "PYrRegVote", "PYrLicensed",
               "PResideCity", "PResideZip", "Guilty", "CrimeType")


## FUNCTIONS ###########################

## Loading and cleaning ################
## create a descriptive merge function for cleaning (essentially a 'merge' wrapper)
CleaningMerge <- function(x, y, ...) {
    ## start by creating the merge
    ## first match arguments
    MatchCall <- match.call(merge)
    MatchCall[[1]] <- quote(merge)
    ## get input names and ensure proper name structure
    xname <- MatchCall$x
    if (!is.symbol(xname)) xname <- as.symbol(paste0(xname[[2]],xname[[3]]))
    yname <- MatchCall$y
    if (!is.symbol(yname)) yname <- as.symbol(paste0(yname[[2]],yname[[3]]))
    ## use this to extract suffixes and fix MatchCall
    MatchCall$suffixes <- paste0(".", c(xname, yname))
    MatchCall$x <- xname
    MatchCall$y <- yname
    ## specify that the match should be an outer join
    MatchCall$all <- TRUE
    ## and use this to make a clean local assignment to modify
    assign(as.character(xname), cbind(x, Diag.x = 1), envir = environment())
    assign(as.character(yname), cbind(y, Diag.y = 1), envir = environment())
    ## now evaluate the call
    Merged <- eval(MatchCall, envir = environment())
    ## next perform some checks
    xExpInds <- is.na(Merged$Diag.x)
    yExpInds <- is.na(Merged$Diag.y)
    ## remove the diagnostic columns
    Merged$Diag.x <- NULL; Merged$Diag.y <- NULL
    ## summarize the diagnostic checks
    X_nexp <- sum(xExpInds)
    Y_nexp <- sum(yExpInds)
    X_missing <- Merged[xExpInds,]
    Y_missing <- Merged[yExpInds,]
    ## print the diagnostics
    cat("Joined ", paste(xname, yname, sep = " and "), " with ",
        X_nexp, " and ", Y_nexp, " failed matches respectively \n", sep = "")
    ## return the results, preferentially keeping the data which is present in x but missing from y
    if (X_nexp == 0 & Y_nexp == 0) {
        Merged
    } else list(Merge = Merged[!xExpInds,], Xfails = X_missing, Yfails = Y_missing)
}

## a function to identify and perform swaps with user input
SimpleSwapper <- function(data, CorrectLevs, auto = FALSE) {
    ## first match the data to the columns of interest
    colInds <- match(names(CorrectLevs), names(data))
    ## extract the levels of the columns of interest to check if there are any potential swaps
    swapCheck <- all(sapply(1:length(colInds),
                            function(ind) identical(sort(levels(as.factor(data[,colInds[ind]]))),
                                                    sort(CorrectLevs[[ind]]))))
    ## if no swaps are present end this check
    if (swapCheck) {
        cat("No errors found, exiting.")
        return(data)
    }
    ## if errors are found, further investigate them
    ## identify potential rows
    ## first those which have elements out of place
    SwapPoss <- sapply(1:length(colInds),
                       function(ind) !(data[,colInds[ind]] %in% CorrectLevs[[ind]]))
    ## now rows containing unknown entries
    Unknown <- sapply(1:length(colInds),
                      function(ind) data[,colInds[ind]] == "U")
    ## identify potential swaps by row
    Swaps <- apply(SwapPoss, 1, function(row) sum(row) > 1)
    ## identify the potential errors
    PotErr <- apply(SwapPoss, 1, function(row) sum(row) == 1)
    ## use the unknowns to account for some errors
    UnkInd <- apply(Unknown, 1, any)
    FalErr <- PotErr & UnkInd
    ## identify the indices to investigate
    SwapInds <- which(Swaps|FalErr)
    ErrInds <- which(PotErr & !UnkInd)
    ## communicate to the user and ask for input
    cat("There are ", sum(Swaps|FalErr), " swaps to check\n", sep = "")
    cat("Additionally, it seems there are ", sum(PotErr & !UnkInd), " errors in entries\n", sep = "")
    ## unless automated
    if (auto) ErrorReturn <- TRUE else ErrorReturn <- as.logical(readline("Return the errors? (T/F): "))
    ## now, if there are possible swaps investigate them
    if (sum(Swaps|FalErr) != 0) {
        ## create a temporary storage structure
        tempRows <- data[SwapInds, colInds]
        tempRows <- as.data.frame(lapply(tempRows, function(var) levels(var)[as.numeric(var)]),
                                  stringsAsFactors = FALSE)
        ## loop through and populate this
        for (ii in 1:nrow(tempRows)) {
            ## inspect the row
            print(tempRows[ii,])
            ## suggest corrections, first generate matches
            candComb <- lapply(tempRows[ii,],
                               function(el) which(sapply(CorrectLevs,
                                                         function(levs) el %in% levs)))
            reps <- unlist(lapply(candComb, length))
            ## now generate all swap combinations
            candComb[[1]] <- rep(candComb[[1]], each = max(reps[-1]))
            candComb <- as.data.frame(candComb, row.names = NULL)
            ## identify rows which contain all indices, in other words those valid as swaps
            compRows <- apply(candComb, 1, function(row) all(1:length(CorrectLevs) %in% row))
            goodComb <- candComb[compRows,]
            ## clean them up and print them
            colnames(goodComb) <- NULL
            rownames(goodComb) <- NULL
            cat("Potential combinations:\n")
            print(t(apply(goodComb,1,order)))
            ## take user input or automatically determine value
            if (auto) {
                if (!any(compRows)) acceptedComb <- 0 else acceptedComb <- 1
            } else acceptedComb <- as.numeric(readline("Enter a combination choice (0 for error, <enter> to accept first): "))
            ## handle special cases, 0 if a true error has been identified
            if (identical(acceptedComb,0)) { ## 0 if a true error has been identified
                ErrInds <- c(ErrInds, SwapInds[ii])
                cat("True error identified, adding ", SwapInds[ii], " to error list\n", sep = "")
            } else { ## the case where a swap has been correctly identified and selected, or enter has been pressed
                ## if enter has been pressed accept the first row
                if (is.na(acceptedComb)) acceptedComb <- 1
                ## print recombined row
                newRows <- tempRows[ii,order(as.matrix(goodComb[acceptedComb,]))]
                colnames(newRows) <- NULL
                rownames(newRows) <- NULL
                cat("Corrected row:")
                print(newRows)
                cat("------------------\n")
                ## correct entry
                tempRows[ii,] <- newRows
            }
        }
        ## fill the data
        ## first prevent factor level errors
        data[,colInds] <- lapply(colInds, function(ind) levels(data[,ind])[as.numeric(data[,ind])])
        ## now swap the data
        data[SwapInds,colInds] <- lapply(1:length(colInds), function(ind) tempRows[,ind])
        ## reconvert back to factors
        data[,colInds] <- lapply(colInds, function(ind) as.factor(data[,ind]))
    }
    ## in either case return the data and errors as specified
    if (ErrorReturn) {
        return(list(Data = data, Errors = ErrInds))
    } else {
        return(data)
    }
}

## now create a function to address the errors possibly identified in the above function automatically
SwapErrorFix <- function(errorData, CorrectLevs) {
    ## check if we are in the case without errors
    if (!identical(names(errorData), c("Data", "Errors"))) {
        cat("No errors\n")
        return(errorData)
    } else {
        ## extract the data and data in error
        fulldata <- errorData$Data
        ## get the relevant columns
        colInds <- match(names(CorrectLevs), names(fulldata))
        ## go through the specified variables and remove errors
        fixed <- lapply(1:length(colInds),
                        function(ind) {
                            var <- fulldata[,colInds[ind]]
                            var <- levels(var)[as.numeric(var)]
                            inds <- !(var %in% CorrectLevs[[ind]])
                            cat(names(CorrectLevs)[ind], ": ", sum(inds),
                                " errors\n", sep = "")
                            var[inds] <- "U"
                            as.factor(var)
                        })
        ## insert these fixed values
        fulldata[, colInds] <- fixed
        ## return this
        fulldata
    }
}

## write a wrapper to perform this swapping and error correction in one call
SwapandError <- function(data, CorrectLevs) {
    swapped <- SimpleSwapper(data = data, CorrectLevs = CorrectLevs, auto = TRUE)
    fixed <- SwapErrorFix(errorData = swapped, CorrectLevs = CorrectLevs)
    fixed
}

## Variable Synthesis ##################
## Kullback-Leibler divergence function
kldiv <- function(samp, dist) {
    ## convert to matrices
    mat1 <- as.matrix(samp)
    mat2 <- as.matrix(dist)
    ## make into proper distributions
    mat1 <- mat1/rowSums(mat1)
    mat2 <- mat2/rowSums(mat2)
    ## take the log ratio
    logratio <- log(mat1/mat2)
    ## multiply by correct matrix
    vals <- mat1*logratio
    ## take the row sums
    rowSums(vals, na.rm = TRUE)
}

## make a text-mining regularization function
StringReg <- function(strs) {
    ## first set everything to lowercase
    strs <- tolower(strs)
    ## replace specific patterns (noticed during early tests)
    strs <- str_replace_all(strs, "b/e|break/enter|b&e|break or enter|b or e|b &/or e|b & e", "breaking and entering")
    strs <- str_replace_all(strs, "controlled substance", "cs")
    strs <- str_replace_all(strs, "dwi", "driving while impaired")
    strs <- str_replace_all(strs, "rwdw", "robbery with a deadly weapon")
    strs <- str_replace_all(strs, "pwisd|pwmsd|pwmsd|pwitd|pwid|pwmisd|pwosd", "pwimsd")
    strs <- str_replace_all(strs, "robery|rob ", "robbery")
    strs <- str_replace_all(strs, "bulgary", "burglary")
    strs <- str_replace_all(strs, "awdw", "assault with a deadly weapon")
    strs <- str_replace_all(strs, "(?<=[\\sa-z])[0-9]{2,}", "")
    strs <- str_replace_all(strs, "att ", "attempted ")
    strs <- str_replace_all(strs, "assult", "assault")
    strs <- str_replace_all(strs, "marj", "marijuana")
    ## replace punctuation
    strs <- gsub("[^[:alnum:][:space:]']", "", strs)
    ## return these
    strs
}

## create a function to process such a tree structure given a list of strings
stringTree <- function(strs, regexTree, inds = 1:length(strs), includeOther = TRUE) {
    ## identify the sublists, and divide the data
    sublists <- sapply(regexTree, is.list)
    ## iterate over unnamed items (leaf nodes)
    listdiv <- lapply(regexTree[!sublists], function(el) inds[grepl(el, strs, perl = TRUE)])
    names(listdiv) <- unlist(regexTree[!sublists])
    ## check if there are any sublists
    if (!any(sublists)) {
        if (includeOther) listdiv <- c(listdiv, other = list(inds[!(inds %in% unlist(listdiv))]))
        ## in the case of none, treat the object as a list to iterate through
        listdiv
    } else {
        ## otherwise recurse over the branches
        finlist <- c(listdiv, lapply(names(regexTree)[sublists],
                                     function(name) stringTree(strs[grepl(name, strs, perl = TRUE)],
                                                               regexTree[[name]],
                                                               inds[grepl(name, strs, perl = TRUE)],
                                                               includeOther)))
        names(finlist)[(length(listdiv) + 1):length(finlist)] <- names(regexTree)[sublists]
        c(finlist, other = list(inds[!(inds %in% unlist(finlist))]))
    }
}

## create a tree depth helper function
maxdepth <- function(tree, counter = 1) {
    max(sapply(tree, function(br) if (!is.list(br)) counter else maxdepth(br, counter + 1)))
}

## create a function to aggregate a tree as specified above at the desired depth
treeAgg <- function(tree, level = 1) {
    ## first check the max depth of the tree
    treedepth <- maxdepth(tree)
    ## compare this to requested aggregation level
    stopifnot(level <= treedepth)
    ## aggregate at desired level with a helper function
    agg <- function(tr, depth = 1) {
        if (depth == level) lapply(tr, function(el) setNames(unlist(el),NULL)) else lapply(tr, function(br) agg(dr, depth + 1))
    }
    agg(tree)
}

## create a crime class aggregation function
CrimeClassify <- function(tree, regChar) {
    crimes <- list()
    crimes$Sex <- unique(c(unlist(tree[c("rape", "sex(?=.*offense)", "sex(?=.*offend)", "indec")]),
                           tree$other[grepl("sex", regChar[tree$other])]))
    crimes$Theft <- unique(unlist(tree[c("stole", "embez", "break", "larceny", "robb", "burg", "identity")]))
    crimes$Murder <- unique(unlist(tree[c("murder", "manslaughter")]))
    crimes$Drug <- unique(c(unlist(tree[c("mari", "coca", "cs", "hero", "meth", "oxycod")]),
                            tree$other[grepl("para|drug|substance|pwimsd", regChar[tree$other])]))
    crimes$Violent <- unique(unlist(tree[c("arson", "assa", "abuse|cruelty")]))
    crimes$Driving <- unique(c(unlist(tree[c("driving")]),
                               tree$other[grepl("hit(?=.*run)|speeding", regChar[tree$other], perl = TRUE)]))
    crimes
}

## in order to make the process of pre-processing the data and adding desired columns, place the pre-processing into a
## flexible function and add operations as desired
SynCols <- function(data) {
    ## too busy, synthesize some variables to clearly indicate the results of defense and prosecution selection
    data$VisibleMinor <- data$Race != "White"
    data$PerempStruck <- grepl("S_rem|D_rem", data$Disposition)
    data$DefStruck <- data$Disposition == "D_rem"
    data$ProStruck <- data$Disposition == "S_rem"
    data$CauseRemoved <- data$Disposition == "C_rem"
    ## lets look at which race struck each juror
    data$StruckBy <- as.factor(sapply(1:nrow(data),
                                               function(ind) {
                                                   dis <- as.character(data$Disposition[ind])
                                                   if (dis == "S_rem") {
                                                       as.character(data$ProsRace[ind])
                                                   } else if (dis == "D_rem") {
                                                       as.character(data$DCRace[ind])
                                                   } else "Not Struck"
                                               }))
    ## create a white black other indicator
    data$WhiteBlack <- FactorReduce(data$Race, tokeep = c("Black", "White"))
    data$DefWhiteBlack <- FactorReduce(data$DefRace, tokeep = c("Black", "White"))
    data$VicWhiteBlack <- FactorReduce(data$VictimRace, tokeep = c("Black", "White"))
    ## return the data with synthesized columns
    data
}


## Summary Functions ###################
## make a function to summarize trial jury data
JurySummarize <- function(Varnames = c("Disposition", "Race", "Gender", "PoliticalAffiliation")) {
    ## check if a juror summary object exists already
    if (!("sun.juror" %in% ls(.GlobalEnv))) {
        ## first group the data for easy access
        Juries <- aggregate(sun.swap[, Varnames],
                            by = list(TrialNumberID = sun.swap$TrialNumberID, JurorNumer = sun.swap$JurorNumber),
                            unique)
    } else Juries <- sun.juror
    ## in either case, perform aggregation by trial instance
    Juries <- aggregate(Juries[, Varnames],
                        by = list(TrialNumberID = Juries$TrialNumberID),
                        function(var) var)
    ## clean up the names
    names(Juries)[grepl("Polit", names(Juries))] <- "PolAff"
    Varnames[4] <- "PolAff"
    ## now summarize relevant features
    Summary <- apply(Juries[, Varnames], 1,
                     function(row) {
                         ## get final jury indices
                         disps <- unlist(row$Disposition)
                         foreman <- grepl("Foreman", disps)
                         finJur <- grepl("Foreman|Kept", disps)
                         defStruck <- grepl("D_rem", disps)
                         proStruck <- grepl("S_rem", disps)
                         ## process all variables
                         newrow <- sapply(row,
                                          function(el) {
                                              c(Jury = table(unlist(el)[finJur]),
                                                Venire = table(unlist(el)),
                                                DefRem = table(unlist(el)[defStruck]),
                                                ProRem = table(unlist(el)[proStruck]))
                                          })
                         newrow$Disposition <- NULL
                         newrow <- c(unlist(newrow), ForeRace = row$Race[foreman],
                                     ForeGender = row$Gender[foreman], ForePol = row$PolAff[foreman])
                         if (sum(foreman) > 1) {
                             names(newrow)[names(newrow) == "ForeRace1"] <- "ForeRace"
                             names(newrow)[names(newrow) == "ForeGender1"] <- "ForeGender"
                             names(newrow)[names(newrow) == "ForePol1"] <- "ForePol"
                         }
                         newrow
                    })
    ## perform some clean up
    longest <- sapply(Summary, length)
    longest <- which(longest == max(longest))[1]
    longNames <- names(Summary[[longest]])
    Summary <- lapply(names(Summary[[longest]]),
                      function(name) unname(sapply(Summary,
                                                   function(el) el[name])))
    names(Summary) <- longNames
    Summary <- lapply(longNames,
                      function(nm) {
                          if (grepl("ForeGender", nm)) {
                              Summary[[nm]] <- factor(Summary[[nm]], levels = 1:3, labels = LevGen)
                          } else if (grepl("ForePol", nm)) {
                              Summary[[nm]] <- factor(Summary[[nm]], levels = 1:5, labels = LevPol)
                          } else if (grepl("ForeRace", nm)) {
                              Summary[[nm]] <- factor(Summary[[nm]], levels = 1:7, labels = LevRace)
                          } else Summary[[nm]]
                      })
    names(Summary) <- longNames
    ## return these
    list(Juries = Juries, Summaries = as.data.frame(Summary))
}

## a generic simplification method to summarize a vector
Simplifier <- function(col, ...) {
    UseMethod("Simplifier")
}

## code up methods for the types to be seen
Simplifier.default <- function(col, collapse = "") paste0(col, collapse = collapse)
Simplifier.numeric <- function(col, na.rm = TRUE, trim = 0, ...) mean.default(col, trim = trim, na.rm = na.rm)
Simplifier.factor <- function(col, collapse = "", ...) paste0(sort(as.character(levels(col)[as.numeric(col)])),
                                                              collapse = collapse)
Simplifier.character <- function(col, collapse = "", ...) paste0(sort(col), collapse = collapse)

## create a grouping wrapper which does unique aggregation of a data set
UniqueAgg <- function(data, by, ...) {
    ## convert data to a data frame for regularity
    if (!is.data.frame(data)) data <- as.data.frame(data)
    ## identify the grouping column by in the data
    by.groups <- names(data) == by
    ## provide nice error handling
    stopifnot(sum(by.groups) > 0)
    ## first identify which rows are already unique
    groups <- as.numeric(as.factor(unlist(data[by.groups])))
    unqRows <- sapply(groups, function(el) sum(groups == el) == 1)
    ## consider grouping only the other rows using the unique function
    endata <- data[unqRows,]
    unqdata <- aggregate(data[!unqRows, !by.groups], by = list(data[!unqRows, by.groups]), unique)
    ## reorder to make sure everything is compatible
    names(unqdata)[1] <- by
    unqdata <- unqdata[,match(names(endata), names(unqdata))]
    ## now use the Simplifier helper defined above to process these results
    procdata <- lapply(unqdata, function(col) sapply(col, Simplifier, ...))
    ## append everything together
    endata <- lapply(1:length(endata),
                     function(n) c(if (is.factor(endata[[n]])) as.character(endata[[n]]) else endata[[n]],
                                   procdata[[n]]))
    names(endata) <- names(data)
    ## convert to a data frame
    as.data.frame(endata)
}

## a simple helper to convert multiple factor levels into a single 'other' level
FactorReduce <- function(vals, tokeep) {
    chars <- as.character(vals)
    ## simply replace elements
    chars[!grepl(paste0(tokeep, collapse = "|"), chars)] <- "Other"
    chars
}

## write a function to re-level factor variables to make mosaic plots cleaner
MatRelevel <- function(data) {
    temp <- lapply(data, function(el) if (is.factor(el)) as.factor(levels(el)[as.numeric(el)]) else el)
    temp <- as.data.frame(temp)
    names(temp) <- names(data)
    temp
}

## another simple processing function to correct NA's given some other identifier and data set
FillNAs <- function(dataNAs, filldata, identifier) {
    ## extract the relevant column indices in a flexible way
    if (is.null(colnames(filldata))) {
        relcol <- grepl(identifier, names(filldata))
    } else relcol <- grepl(identifier, colnames(filldata))
    ## first identify the relevant rows in the data NAs
    relRows <- is.na(dataNAs)
    ## take the relevant rows of the filldata
    filldata <- matrix(unlist(filldata[relcol]), ncol = sum(relcol))
    rowfiller <- rowSums(filldata[relRows,])
    ## return the filled data
    dataNAs[relRows] <- rowfiller
    dataNAs
}

## write a wrapper to estimate the values of total removed jurors
RemovedJurorEstimates <- function(tofill, data, ident, plot = TRUE) {
    temp <- FillNAs(tofill, filldata = data, identifier = ident)
    temp2 <- rowSums(data[,grepl(ident, names(data))])
    ## let's see how accurate this is if plotting is desired
    if (plot) {
        plot(temp, temp2, xlab = "Observed and Filled", ylab = "Juror Sums")
        abline(0,1)
    }
    cat("= : ", sum(temp == temp2)/length(temp2), "\n", "< : ", sum(temp2 < temp)/length(temp2), "\n", sep = "")
    ## replace the filled values less than the estimated, for consistency
    temp[temp < temp2] <- temp2[temp < temp2]
    temp
}


## LOADING AND PROCESSING DATA #########

## load the data
SunshineData <- lapply(SunshineSheets, function(nm) as.data.frame(read_excel(SunshineFile, sheet = nm)))
names(SunshineData) <- SunshineSheets
NorthCarData <- read.csv(NorthCarFile)
PhillyData <- read.csv(PhillyFile)

## clean non-informative columns
CleanSunshine <- lapply(SunshineData, function(dat) dat[, !apply(dat,2,function(col) all(is.na(col)))])

## the Sunshine data needs to be restructured into one table, rather than a relational database structure
## see the IDMatch function, this was created specifically to perform ID-based table joins
## the most appropriate global target is the juror table, start by matching this to the trial
FullSunshine <- with(CleanSunshine, CleaningMerge(Jurors, Trials, by = "TrialNumberID"))
## remove extra ID column, fix a misleading name
FullSunshine$CountyName <- FullSunshine$CountyID
FullSunshine$CountyID <- NULL
## clean up two additional columns which had inconsistencies
FullSunshine$Disposition <- toupper(FullSunshine$Disposition)
FullSunshine$Race[FullSunshine$Race == "?"] <- "U"
## before appending everything to this table, perform some other joins
TrialsToCharge <- with(CleanSunshine, CleaningMerge(Charges, Junction, by = "ACISID", all = TRUE))
DefendantToTrial <- with(CleanSunshine, CleaningMerge(Defendants, DefendantTrial, by = "DefendantID", all = TRUE))
AttorneyToTrial <- with(CleanSunshine, CleaningMerge(Attorney, AttorneyTrial, by = "DefAttyID", all = TRUE))
ProsecutorToTrial <- with(CleanSunshine, CleaningMerge(Prosecutor, ProsecutorTrial, by = "ProsecutorID", all = TRUE))
## merge issues:
##    - trials to charge: one charge is missing a trial ID, hopefully not important
##    - prosecutors to trials: 26 prosecutors without trials, however all entries were entirely uninformative
## given the above outputs, rename the failed clean merges to make the next section cleaner
TrialsToCharge <- TrialsToCharge$Merge
ProsecutorToTrial <- ProsecutorToTrial$Merge

## now perform some additional merges to create one sheet/data.frame
## add the judge descriptions (no issues)
FullSunshine <- CleaningMerge(FullSunshine, CleanSunshine$Judges, by = "JudgeID", all = TRUE)
## the charges
FullSunshine <- CleaningMerge(FullSunshine, TrialsToCharge, by = "TrialNumberID", all = TRUE)
## this leads to 22 jurors in trials without charges and 29 charges without trials, inspecting these:
##     - the jurors without charges are all related to a trial with ID number "710-01", thankfully the other data
##       for this case is complete, and so it may still be useful for viewing jury behaviour
##     - the charges without trials are all of the form "710-0xx", suggesting the omission of entire trials of some
##       relation, hopefully these were not too similar, or this exclusion can be explained later
FullSunshine <- FullSunshine$Merge
## the defendants
FullSunshine <- CleaningMerge(FullSunshine, DefendantToTrial, by = "TrialNumberID", all = TRUE)
## the attorneys
FullSunshine <- CleaningMerge(FullSunshine, AttorneyToTrial, by = "TrialNumberID", all = TRUE)
## the prosecutors
FullSunshine <- CleaningMerge(FullSunshine, ProsecutorToTrial, by = "TrialNumberID", all = TRUE)
## 26 jurors appear to be lacking a prosecutor, these appear to be the uninformative prosecutors from earlier, included
## due to the preferential inclusion of the missing values in the first of the merged matrices
FullSunshine <- FullSunshine$Merge

## perform some cleanup
## start with some specific factor replacements
## replace the "N" with "I", as these factor levels are interchangeable in the codebook and prevent confusion with race
FullSunshine[,grepl("Pol", names(FullSunshine))] <- lapply(FullSunshine[,grepl("Pol", names(FullSunshine))],
                                                           function(var) {
                                                               var <- toupper(var)
                                                               var[var == "N"] <- "I"
                                                               var
                                                           })
## next save most variables as factors
FullSunshine <- lapply(FullSunshine,
                       function(el) if (is.character(el)) as.factor(el) else el)
## correct some overzealous assignment from above
FullSunshine[grepl("Notes", names(FullSunshine))] <- lapply(FullSunshine[grepl("Notes", names(FullSunshine))],
                                                            as.character)
## perform factor regularization according to the factor levels provided in the codebook
FullSunshine <- sapply(FullSunshine,
                       function(el) {
                           if (!is.factor(el)) {
                               el[el == 999] <- NA
                               el
                           } else {
                               el <- as.character(el)
                               el <- toupper(el)
                               el[is.na(el)] <- "U"
                               as.factor(el)
                           }
                       }, simplify = FALSE)
FullSunshine <- as.data.frame(FullSunshine)
## remove some unnecessary columns
FullSunshine$ID <- NULL
FullSunshine$TrialIDAuto <- NULL
## combine the name columns to produce more useful columns
FullSunshine$JName <- paste(FullSunshine$JFirstName, FullSunshine$JLastName)
FullSunshine$JName[FullSunshine$JName == "U U"] <- "U"
FullSunshine$DefAttyName <- paste(FullSunshine$DCFirstName, FullSunshine$DCLastName)
FullSunshine$DefAttyName[FullSunshine$DefAttyName == "U U"] <- "U"
FullSunshine$ProsName <- paste(FullSunshine$ProsecutorFirstName, FullSunshine$ProsecutorLastName)
FullSunshine$ProsName[FullSunshine$ProsName == "U U"] <- "U"

## Checkpoint 1: the clean data has been processed, none of the swaps, synthesis, or expansion has taken place
## save this
if (!("FullSunshine.csv" %in% list.files())) write.csv(FullSunshine, "FullSunshine.csv", row.names = FALSE)
## load if the desire is to start at checkpoint 1
if (!("FullSunshine" %in% ls())) FullSunshine <- read.csv("FullSunshine.csv")

## Note: the below swap functions have been set to auto as the function's performance in these cases has already
## been assessed, and so the swaps have already been inspected, it is critical for new data that "auto" be switched
## off to take full advantage of this functionality, and so the wrapper "SwapandError" should not be used
## in the juror data
sun.swapJuror <- SwapandError(FullSunshine, CorrectLevs = list(Race = LevRace,
                                                               Gender = LevGen,
                                                               PoliticalAffiliation = LevPol))
## in the judge data
sun.swap <- SimpleSwapper(sun.swapJuror, CorrectLevs = list(JRace = LevRace,
                                                                    JGender = LevGen,
                                                                    JPoliticalAff = LevPol))
## viewing the error report of these data, they are all related to one judge, Arnold O Jones II, who is verified
## as a male after a quick Google search
unique(sun.swap$Data[sun.swap$Errors, c("JFirstName", "JLastName")])
sun.swapJudge <- sun.swap$Data
sun.swapJudge$JGender[sun.swap$Errors] <- "M"
sun.swapJudge$JGender <- as.factor(levels(sun.swapJudge$JGender)[as.numeric(sun.swapJudge$JGender)])
## in the prosecutor data
sun.swap <- SimpleSwapper(sun.swapJudge, CorrectLevs = list(ProsRace = LevRace,
                                                                    ProsGender = LevGen,
                                                                    ProsPoliticalAff = LevPol))
## that found no errors
## a quick check of the levels of the defendant data finds only one error
levels(sun.swap$DefGender)
levels(sun.swap$DefRace)
sun.swap <- SwapandError(sun.swap, CorrectLevs = list(DefRace = LevRace,
                                                               DefGender = LevGen))
## next the attorney data
sun.swap <- SwapandError(sun.swap, CorrectLevs = list(DCRace = LevRace,
                                                               DCGender = LevGen,
                                                               DCPoliticalAff = LevPol))
## finally the victim data
sun.swap <- SwapandError(sun.swap, CorrectLevs = list(VictimRace = LevRace,
                                                               VictimGender = LevGen))
## this leaves the data error-free (in at least the race/gender/politics columns)

## fix the outcome data, which had some improper levels
sun.swap$Outcome[sun.swap$Outcome == "HC"] <- "U"
sun.swap$Outcome[sun.swap$Outcome == "G"] <- "GC"
sun.swap$Outcome <- as.factor(levels(sun.swap$Outcome)[as.numeric(sun.swap$Outcome)])

## lets make the levels more clear for some of the data (race, politics, disposition)
## start with the disposition
levels(sun.swap$Disposition) <- c("C_rem", "D_rem", "Foreman", "Kept", "U_rem",
                                      "S_rem", "Unknown")
## next the political affiliation
sun.swap <- lapply(sun.swap, function(el) {
    if (is.factor(el) & identical(levels(el), LevPol)) {
        levels(el) <- c("Dem", "Ind", "Lib", "Rep", "U")
        el
    } else el})
levels(sun.swap$JPoliticalAff) <- c("Dem", "Ind", "Rep", "U")
## now the race
sun.swap <- lapply(sun.swap, function(el) {
    if (is.factor(el) & identical(levels(el), LevRace)) {
        levels(el) <- c("Asian", "Black", "Hisp", "NatAm", "Other",
                        "U", "White")
        el
    } else el})
levels(sun.swap$VictimRace) <- c("Asian", "Black", "Hisp", "NatAm",
                                     "U", "White")
levels(sun.swap$JRace) <- c("Black", "Hisp", "NatAm", "U", "White")
levels(sun.swap$DCRace) <- c("Asian", "Black", "NatAm", "Other",
                                 "U", "White")
## now the outcome/verdict
levels(sun.swap$Outcome) <- c("Acquittal", "Guilty as Charged",
                                  "Guilty of Lesser", "Incomplete", "Mistrial",
                                  "U")
## the defense attorney type
levels(sun.swap$DefAttyType) <- c("App Priv", "Public", "Private",
                                  "Ret Priv", "U", "Waived")

## add a guilt indicator
sun.swap$Guilty <- grepl("Guilty", sun.swap$Outcome)

## now perform tree classification of crimes
## first cast sun.swap as a data frame
sun.swap <- as.data.frame(sun.swap)
## regularize the charges
chargFact <- as.factor(sun.swap$ChargeTxt)
regCharg <- StringReg(levels(chargFact))[as.numeric(chargFact)]
## classify these into a charge tree and aggregate this at the coarsest level
aggCharg <- treeAgg(stringTree(regCharg, chargeTree))
## these can be further classified into crime classes
crimes.trial <- CrimeClassify(aggCharg, regCharg)
## convert these classes into a factor for the data, start with a generic "other" vector
sun.swap$CrimeType <- rep("Other", nrow(sun.swap))
## now populate it
for (nm in sort(names(crimes.trial))) sun.swap$CrimeType[crimes.trial[[nm]]] <- nm
sun.swap$CrimeType <- as.factor(sun.swap$CrimeType)

## synthesize additional columns
sun.swap <- SynCols(sun.swap)

## now organize this on the juror scale
sun.juror <- UniqueAgg(sun.swap, by = "JurorNumber", collapse = ",")

## Checkpoint 2: the swapped data has been processed and summarized to be on the scale of individual jurors
## save the swapped data
write.csv(sun.swap, "FullSunshine_Swapped.csv", row.names = FALSE)
## and the juror summarized data
saveRDS(sun.juror, "JurorAggregated.Rds")

## summarize by trial, get the unique trials
Trials <- unique(sun.swap$TrialNumberID)
## extract information about these trials, note that grouping occurs on the trial ID, defendant ID, and charge ID levels,
## as the trials frequency involve multiple charges and defendants, which makes them less clean
sun.trial <- aggregate(sun.swap[,TrialVars],
                           by = list(sun.swap$TrialNumberID, sun.swap$DefendantID.DefendantToTrial,
                                     sun.swap$ID.Charges),
                           unique)
sun.trial$Group.1 <- NULL
sun.trial$Group.2 <- NULL
sun.trial$Group.3 <- NULL

## summarize the juries by trial as well
sun.jursum <- JurySummarize()

## merge the summaries to the trial sunshine data
sun.trialsum <- merge(cbind(TrialNumberID = sun.jursum$Juries$TrialNumberID, sun.jursum$Summaries),
                      sun.trial, all = TRUE)

## notice that the total removed variables are incomplete, try to correct this where possible using the jury
## summarized data above
sun.trialsum$DefRemEst <- RemovedJurorEstimates(sun.trialsum$DefenseTotalRemoved, data = sun.trialsum,
                                                ident = "Gender.DefRem", plot = FALSE)
## perform this same procedure for the prosecution removals
sun.trialsum$ProRemEst <- RemovedJurorEstimates(sun.trialsum$StateTotalRemoved, data = sun.trialsum,
                                                ident = "Gender.ProRem", plot = FALSE)
## synthesize some other variables, simple race indicators
sun.trialsum$DefWhiteBlack <- as.factor(FactorReduce(sun.trialsum$DefRace, tokeep = c("Black", "White")))
sun.trialsum$DefWhiteOther <- as.factor(FactorReduce(sun.trialsum$DefWhiteBlack, tokeep = "White"))
## the Kullback-Leibler divergence
sun.trialsum$KLdiv <- kldiv(sun.trialsum[,grepl("Jury", names(sun.trialsum))],
                            sun.trialsum[,grepl("Venire", names(sun.trialsum))])

## Checkpoint 3: the data has been set to the trial level and summarized
## save this
saveRDS(sun.trialsum, "TrialAggregated.Rds")
saveRDS(sun.jursum, "AllJuries.Rds")

