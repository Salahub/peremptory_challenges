########################################

## THESIS DATA PROCESSING SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################

## PACKAGES ############################
library(readxl)
library(vcd)


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
LevRace <-  c("A","B","H","N","O","U","W")
LevGen <-  c("F","M","U")
LevPol <-  c("D","L","R","I","U")



## FUNCTIONS ###########################

## a simple row extension helper generic function
extendRows <- function(object, nrows) {
    ## now use the appropriate method
    UseMethod("extendRows", object)
}

## create a matrix method
extendRows.matrix <- function(mat, nrows) {
    ## first define the extension data
    NArows <- matrix(NA, nrow = nrows, ncol = ncol(mat))
    ## simply add the specified number of NA rows
    rbind(mat, NArows)
}

## create a data.frame method
extendRows.data.frame <- function(dataframe, nrows) {
    ## first define the extension data
    NArows <- matrix(NA, nrow = nrows, ncol = ncol(dataframe))
    ## similar to before, but change the names first
    colnames(NArows) <- colnames(dataframe)
    rbind(dataframe, NArows)
}

## a function to, given an ID, populate the relevant columns of a data frame using a second data set
IDMatch <- function(target, data, IDcolumn = NULL, safeMode = FALSE, sufx = "_1") {
    ## extract names (useful later)
    dataNames <- colnames(data)
    targetNames <- colnames(target)
    ## perform some simple checks of provided arguments
    if (is.null(IDcolumn)) {
        IDcolumn <- dataNames[1]
    }
    stopifnot(IDcolumn %in% targetNames, IDcolumn %in% dataNames)
    ## sort the target to make the join simpler
    target <- target[order(target[,IDcolumn]),]
    ## compare the ID values between the two data sets
    dataFact <- data[,IDcolumn]
    targFact <- target[,IDcolumn]
    dataCommon <- dataFact %in% targFact
    ## extend the target if necessary
    if (!all(dataCommon)) {
        target <- extendRows(target, sum(!dataCommon))
        target[, IDcolumn] <- c(targFact, dataFact[!dataCommon])
        targFact <- target[, IDcolumn]
    }
    ## now add the necessary columns to the data
    ## start by matching the data to the target
    dataMatch <- match(dataFact, targFact)
    ## now extract the correct ordering of the values
    dataOrd <- order(dataMatch)
    ## generate the correct number of entries
    dataInds <- rep(dataOrd, times = diff(c(dataMatch[dataOrd],nrow(target)+1)))
    ## apply this ordering an repetition to the data, removing spurious repetition
    dataNew <- data[dataInds,]
    dataNew[dataNew[,IDcolumn] != targFact,] <- NA
    ## combine this with the other data
    amalgam <- cbind(target, dataNew)
    ## rename the merged data to make duplicates clearer
    tempNewNames <- paste(dataNames, c("", sufx)[dataNames %in% targetNames + 1], sep = "")
    names(amalgam) <- c(targetNames, tempNewNames)
    ## perform error check for "safe" merging if desired, and to make cleaning easier
    if (safeMode) {
        ## start by finding the unique column names
        uniqueCols <- unique(colnames(amalgam))
        ## find duplicates
        dupCols <- sapply(uniqueCols, function(nm) sum(colnames(amalgam) == nm) > 1)
        ## iterate through duplicates to check status
        matchingDups <- sapply(uniqueCols[dupCols],
                               function(nm) is.logical(all.equal(amalgam[,nm],
                                                                 amalgam[,colnames(amalgam) == nm][,2])))
        ## provide warning if this fails
        if (any(!matchingDups)) warning(c("Duplicate columns ",
                                          paste(uniqueCols[dupCols][!matchingDups],collapse=", "),
                                          " did not match"))

    }
    ## return the merged data
    amalgam
}

## create a descriptive merge function (somewhat replaces the above) for cleaning
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

## write a helper for IdentifySwap to made the code cleaner
RowSwap <- function(row, CorrectLevels, interactive = FALSE) {
    ## begin by identifying the swap candidate combinations which are complete, i.e. include all factors
    candComb <- as.data.frame(lapply(row,
                                     function(el) which(sapply(CorrectLevels,
                                                               function(levs) el %in% levs))))
    compRows <- apply(candComb, 1, function(row) all(1:length(CorrectLevels) %in% row))
    ## first consider the interactive case to select from these
    if (interactive) {
        ## in this case the row and correct levels should be printed
        print(CorrectLevels)
        print(row)
        ## communicate the swap options available to the user
        cat("Candidate swaps:\n")
        print(candComb[compRows,])
        ## ask for input
        comb <- as.numeric(readline("Choose a recombination row: "))
    ## otherwise simply take the first
    } else {
        comb <- 1
    }
    ## return the specified input
    return(row[order(as.matrix(candComb[compRows,][comb,]))])
}

## to address the possible data entry errors for adjacent columns, introduce a cleaner function
IdentifySwap <- function(data, CorrectLevels = NULL, autoswap = FALSE) {
    ## match the data names to the correct level specifications
    factorMatch <- match(names(CorrectLevels), names(data))
    ## now adjust the factors being checked to avoid issues
    data[,factorMatch] <- lapply(factorMatch, function(fact) as.character(data[,fact]))
    ## perform an inclusion check
    missingFact <- is.na(factorMatch)
    if (any(missingFact)) warning(paste(names(CorrectLevels)[missingFact], collapse = ", "),
                                  " not included in the data")
    ## for each, check the possibilty of swaps
    SwapPoss <- sapply(which(!is.na(missingFact)),
                       function(ind) !(data[,factorMatch[ind]] %in% CorrectLevels[[ind]]))
    Swaps <- apply(SwapPoss, 1, function(row) sum(row) > 1)
    ## and record the columns which seem to have errors
    Errors <- apply(SwapPoss, 1, function(row) sum(row) == 1)
    ## finally, produce a correct levels object without names, as they are no long necessary and produce warnings
    CorrectLevels.unnamed <- CorrectLevels
    names(CorrectLevels.unnamed) <- NULL
    ## now look through the data, with interactive display in the case that autoswapping is not desired
    if (!autoswap) {
        ## iterate through the selected rows
        for (ii in which(Swaps)) {
            ## for each row call the interactive RowSwap function
            newRow <- RowSwap(data[ii, factorMatch[!missingFact]],
                              CorrectLevels.unnamed[!missingFact], interactive = TRUE)
            ## replace the row elements
            data[ii, factorMatch] <- newRow
        }
    } else {
        ## in this case there is no desire for interactive selection, so simply perform the swap
        data[which(Swaps), factorMatch] <- t(apply(data[which(Swaps), factorMatch[!missingFact]], 1,
                                                 function(row) RowSwap(row, CorrectLevels.unnamed[!missingFact])))
        ## print a small summary of number of swaps performed
        cat("Swapped ", sum(Swaps), " columns \n", sep = "")
    }
    ## return the resulting data, with factors reinstated
    data[, factorNames] <- lapply(factorNames, function(name) as.factor(data[,name]))
    return(data)
}

## this construct seemed too complicated, make a simpler one which has fewer complexities to account for efficiency and
## modularity, this function does not need to be modular or efficient, just helpful
SimpleSwapper <- function(data, CorrectLevs) {
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
    ErrorReturn <- as.logical(readline("Return the errors? (T/F): "))
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
            ## take user input
            acceptedComb <- as.numeric(readline("Enter a combination choice (0 for error, <enter> to accept first): "))
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

## check for column swaps
## in the juror data
SwapSunshine <- SimpleSwapper(FullSunshine, CorrectLevs = list(Race = LevRace,
                                                               Gender = LevGen,
                                                               PoliticalAffiliation = LevPol))
JurorSwapSunshine <- SwapErrorFix(SwapSunshine, CorrectLevs = list(Race = LevRace,
                                                               Gender = LevGen,
                                                               PoliticalAffiliation = LevPol))
## in the judge data
SwapSunshine <- SimpleSwapper(JurorSwapSunshine, CorrectLevs = list(JRace = LevRace,
                                                                    JGender = LevGen,
                                                                    JPoliticalAff = LevPol))
## viewing the error report of these data, they are all related to one judge, Arnold O Jones II, who is verified
## as a male after a quick Google search
unique(SwapSunshine$Data[SwapSunshine$Errors, c("JFirstName", "JLastName")])
JudgeSwapSunshine <- SwapSunshine$Data
JudgeSwapSunshine$JGender[SwapSunshine$Errors] <- "M"
JudgeSwapSunshine$JGender <- as.factor(levels(JudgeSwapSunshine$JGender)[as.numeric(JudgeSwapSunshine$JGender)])
## in the prosecutor data
SwapSunshine <- SimpleSwapper(JudgeSwapSunshine, CorrectLevs = list(ProsRace = LevRace,
                                                                    ProsGender = LevGen,
                                                                    ProsPoliticalAff = LevPol))
## that found no errors
## a quick check of the levels of the defendant data finds only one error
levels(SwapSunshine$DefGender)
levels(SwapSunshine$DefRace)
SwapSunshine <- SimpleSwapper(SwapSunshine, CorrectLevs = list(DefRace = LevRace,
                                                               DefGender = LevGen))
SwapSunshine <- SwapErrorFix(SwapSunshine, CorrectLevs = list(DefRace = LevRace,
                                                              DefGender = LevGen))
## this leaves the data error-free (in at least the race/gender/politics columns)

## save the full sunshine data
if (!("FullSunshine_Swapped.csv" %in% list.files())) write.csv(SwapSunshine, "FullSunshine_Swapped.csv", row.names = FALSE)


## DATA INSPECTION #####################

## load the data if it is not loaded
if (!("FullSunshine" %in% ls())) FullSunshine <- read.csv("FullSunshine.csv")
if (!("SwapSunshine" %in% ls())) SwapSunshine <- read.csv("FullSunshine_Swapped.csv")

## display information about juror rejection tendencies
mosaicplot(Race ~ Disposition, data = SRaceKnown)
## too busy, synthesize some variables to clearly indicate the results of defense and prosecution selection
SwapSunshine$VisibleMinor <- SwapSunshine$Race != "W"
SwapSunshine$PerempStruck <- SwapSunshine$Disposition == "S" | SwapSunshine$Disposition == "D"
SwapSunshine$DefStruck <- SwapSunshine$Disposition == "D"
SwapSunshine$ProStruck <- SwapSunshine$Disposition == "S"
## create a race filtered data set
SRaceKnown <- SwapSunshine[SwapSunshine$Race != "U",]
SRaceKnown$Race <- as.factor(levels(SRaceKnown$Race)[as.numeric(SRaceKnown$Race)])
## try plotting these
mosaicplot(VisibleMinor ~ PerempStruck, data = SRaceKnown, shade = TRUE)
mosaicplot(VisibleMinor ~ DefStruck, data = SRaceKnown, shade = TRUE)
mosaicplot(VisibleMinor ~ ProStruck, data = SRaceKnown, shade = TRUE)
## it seems that there are significantly different strike habits between the defense and prosecution, but that
## generally the system does not strike at different rates on average
## look at rejection with more detail
mosaicplot(Race ~ PerempStruck, data = SRaceKnown)
mosaicplot(Race ~ DefStruck, data = SRaceKnown)
mosaicplot(Race ~ ProStruck, data = SRaceKnown)
## try another approach
mosaicplot(Race ~ Disposition, data = SRaceKnown)


## however, this suggests another question: is this strategy actually successful? That is, does there appear to
## be a relation between the number of peremptory challenges and the court case outcome?
## this may be difficult, there are a lot of factors to consider:
##                  - the lawyer and their track record
##                  - how to judge the success/failure of the case
## start by making a simple indicator of guilty/not guilty ignoring the complexities of such a verdict
SwapSunshine$Guilty <- SwapSunshine$Outcome %in% c("GC", "GL", "G")
## see if the presence of challenges is related to this verdict
mosaicplot(PerempStruck ~ Guilty, data = SwapSunshine, shade = TRUE)
## on the level of jurors, this is certainly not the case, but this is not the correct scale for the question being
## asked, this question will be addressed again in the case-summarized data

## identify the unique trials
Trials <- unique(SwapSunshine$TrialNumberID)
## and the variables which can be sensibly summarized for each trial
TrialVars <- c("TrialNumberID", "JudgeID", "DefAttyType", "VictimName",
               "VictimRace", "VictimGender", "CrimeLocation", "PropertyType",
               "ZipCode.Trials", "StateTotalRemoved", "DefenseTotalRemoved",
               "CourtTotalRemoved", "JDistrict", "JFirstName", "JLastName",
               "JRace", "JGender", "JPoliticalAff", "JVoterRegYr", "JYrApptd",
               "JResCity", "JResZip", "ChargeTxt", "Outcome", "Sentence.FullSunshine",
               "DefendantID.FullSunshine", "DefendantID.DefendantToTrial", "DefRace",
               "DefGender", "DefDOB", "ProsecutorID", "ProsecutorFirstName",
               "ProsecutorLastName", "ProsRace", "ProsGender", "ProsPoliticalAff",
               "PYrRegVote", "PYrLicensed", "PResideCity", "PResideZip")
## extract information about these trials
UniqueTrial <- aggregate(SwapSunshine[,TrialVars],
                         by = list(SwapSunshine$TrialNumberID),
                         unique)
UniqueTrial$Group.1 <- NULL
UniqueTrial <- lapply(UniqueTrial, function(var) if (is.character(var)) as.factor(var) else var)
## interestingly, the outcomes do not seem to be unique to the trials in the data, which is surprising
## let's investigate these outcomes
OutcomeTabs <- lapply(Trials, function(trial) table(SwapSunshine$Outcome[SwapSunshine$TrialNumberID == trial]))
OutcomeTabs <- lapply(OutcomeTabs, function(tab) tab[tab != 0])
DubOut <- OutcomeTabs[sapply(OutcomeTabs, function(tab) length(tab) > 1)]
length(DubOut)
## this does not seem to make sense until the charges are viewed by unique trial
sum(sapply(UniqueTrial$ChargeTxt, function(el) length(el > 1)))
## this and multiple defendants on a trial seem to explain almost all of the duplicates, aggregate by all three of these
## variables and create new, clearer IDs
UniqueTrial <- aggregate(SwapSunshine[,TrialVars],
                         by = list(SwapSunshine$TrialNumberID, SwapSunshine$DefendantID.DefendantToTrial,
                                   SwapSunshine$ID.Charges),
                         unique)
UniqueTrial$Group.1 <- NULL
UniqueTrial$Group.2 <- NULL
UniqueTrial$Group.3 <- NULL
## this has solved the issue
## synthesize a minority defense indicator
UniqueTrial$MinorDef <- sapply(UniqueTrial$DefRace, function(el) !("W" %in% el), simplify = TRUE)
## add a guilty indicator
UniqueTrial$Guilty <- UniqueTrial$Outcome %in% c("GC", "GL", "G")
## try to address the same question of "effectiveness" as before
plot(DefenseTotalRemoved ~ as.factor(Guilty), data = UniqueTrial)
plot(StateTotalRemoved ~ as.factor(Guilty), data = UniqueTrial)
plot(jitter(as.numeric(Guilty)) ~ StateTotalRemoved, data = UniqueTrial)
## see if there is anything to an advantage given by a usage differential between prosecution and defense
UniqueTrial$PerempDiff <- UniqueTrial$StateTotalRemoved - UniqueTrial$DefenseTotalRemoved
plot(PerempDiff ~ as.factor(Guilty), data = UniqueTrial)
## try fitting a logistic regression model
diffMod <- glm(Guilty ~ MinorDef + DefenseTotalRemoved + StateTotalRemoved, data = UniqueTrial,
               family = binomial)
## do a quick race investigation
mosaicplot(DefRace ~ as.factor(Guilty), data = UniqueTrial, shade = TRUE)
plot(DefenseTotalRemoved ~ as.factor(Guilty) + DefRace, data = UniqueTrial)
plot(StateTotalRemoved ~ as.factor(Guilty) + as.factor(MinorDef), data = UniqueTrial)
## there does not seem to be any advantage given by peremptory challenge usage, maybe try controlling for crime severity

## to control for crime severity, try performing some text analysis on the sentencing data and charge text

## try this for the other data sets
mosaicplot(RACEQUES ~ FINLJURY, data = PhillyData)
## not many rejections, LOTS of missing data
c(table(PhillyData$FINLJURY), "NAs" = sum(is.na(PhillyData$FINLJURY)))
## is this data even useful? next data set:
mosaic(table(NorthCarData[,c("RaceLabel", "Struck")]), shade = TRUE)



## do some plotting
with(SingleTrialData, plot(y = jitter(DefenseTotalRemoved), x = jitter(as.numeric(DefAttyType)),
                           xaxt = "n", xlab = "Defense Attorney Type", ylab = "Number of Rejected Jurors",
                           col = adjustcolor(c("firebrick","steelblue")[MinorDef+1], alpha.f = 0.2), pch = 19))
axis(side = 1, at = 1:6, labels = c("P-Appointed", "PublicDefender", "P-Unknown",
                                    "P-Retained", "Unknown", "Self-Representation"),
     cex.axis = 0.75)
for (ii in 0:17) abline(h = ii, lty = 2, col = adjustcolor("gray50", alpha.f = 0.3))
with(SingleTrialData, points(x = 1:6, y = sapply(levels(DefAttyType),
                                               function(type) mean(DefenseTotalRemoved[DefAttyType == type],
                                                                   na.rm = TRUE)),
                             col = "red", pch = 19))

## comparison plot of prosecutor rejection
plot(x = sort(SingleTrialData$StateTotalRemoved), y = ppoints(length(sort(SingleTrialData$StateTotalRemoved))),
     ylab = "Quantile", xlab = "Number of Juror Rejections", col = "steelblue")
points(x = sort(SingleTrialData$DefenseTotalRemoved), y = ppoints(length(sort(SingleTrialData$DefenseTotalRemoved))),
       col = "firebrick")
legend(x = "bottomright", legend = c("Prosecution", "Defense"), col = c("steelblue","firebrick"),
       pch = 1)

## plot by race combination of defendant

## plot of rejection by minority defendant
par(mfrow = c(1,2))
boxplot(DefenseTotalRemoved ~ MinorDef, data = SingleTrialData, ylim = c(-1,17), xlab = "Minority Defendant",
        ylab = "Defense Peremptory Challenges")
text(x = c(1,2), y = c(-1,-1),
     labels = paste0("n = ", c(sum(!SingleTrialData$MinorDef), sum(SingleTrialData$MinorDef))))
boxplot(StateTotalRemoved ~ MinorDef, data = SingleTrialData, ylim = c(-1,17), xlab = "Minority Defendant",
        ylab = "Prosecution Peremptory Challenges")
text(x = c(1,2), y = c(-1,-1),
     labels = paste0("n = ", c(sum(!SingleTrialData$MinorDef), sum(SingleTrialData$MinorDef))))

## plot the unconditional challenge distribution of both sides
