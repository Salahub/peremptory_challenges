########################################

## THESIS DATA PROCESSING SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################

## PACKAGES ############################
library(readxl)


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

## save this
if (!("FullSunshine.csv" %in% list.files())) write.csv(FullSunshine, "FullSunshine.csv", row.names = FALSE)

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
## next the attorney data
SwapSunshine <- SimpleSwapper(SwapSunshine, CorrectLevs = list(DCRace = LevRace,
                                                               DCGender = LevGen,
                                                               DCPoliticalAff = LevPol))
SwapSunshine <- SwapErrorFix(SwapSunshine, CorrectLevs =  list(DCRace = LevRace,
                                                               DCGender = LevGen,
                                                               DCPoliticalAff = LevPol))
## finally the victim data
SwapSunshine <- SimpleSwapper(SwapSunshine, CorrectLevs = list(VictimRace = LevRace,
                                                               VictimGender = LevGen))
SwapSunshine <- SwapErrorFix(SwapSunshine, CorrectLevs = list(VictimRace = LevRace,
                                                              VictimGender = LevGen))
## this leaves the data error-free (in at least the race/gender/politics columns)

## fix the outcome data, which had some improper levels
SwapSunshine$Outcome[SwapSunshine$Outcome == "HC"] <- "U"
SwapSunshine$Outcome[SwapSunshine$Outcome == "G"] <- "U"
SwapSunshine$Outcome <- as.factor(levels(SwapSunshine$Outcome)[as.numeric(SwapSunshine$Outcome)])

## lets make the levels more clear for some of the data (race, politics, disposition)
## start with the disposition
levels(SwapSunshine$Disposition) <- c("C_rem", "D_rem", "Foreman", "Kept", "U_rem",
                                      "S_rem", "Unknown")
## next the political affiliation
SwapSunshine <- lapply(SwapSunshine, function(el) {
    if (is.factor(el) & identical(levels(el), LevPol)) {
        levels(el) <- c("Dem", "Ind", "Lib", "Rep", "U")
        el
    } else el})
levels(SwapSunshine$JPoliticalAff) <- c("Dem", "Ind", "Rep", "U")
## now the race
SwapSunshine <- lapply(SwapSunshine, function(el) {
    if (is.factor(el) & identical(levels(el), LevRace)) {
        levels(el) <- c("Asian", "Black", "Hisp", "NatAm", "Other",
                        "U", "White")
        el
    } else el})
levels(SwapSunshine$VictimRace) <- c("Asian", "Black", "Hisp", "NatAm",
                                     "U", "White")
levels(SwapSunshine$JRace) <- c("Black", "Hisp", "NatAm", "U", "White")
levels(SwapSunshine$DCRace) <- c("Asian", "Black", "NatAm", "Other",
                                 "U", "White")
## now the outcome/verdict
levels(SwapSunshine$Outcome) <- c("Acquittal", "Guilty as Charged",
                                  "Guilty of Lesser", "Incomplete", "Mistrial",
                                  "U")
## the defense attorney type
levels(SwapSunshine$DefAttyType) <- c("App Priv", "Public", "Private",
                                      "Ret Priv", "U", "Waived")

## save the full sunshine data
if (!("FullSunshine_Swapped.csv" %in% list.files())) write.csv(SwapSunshine, "FullSunshine_Swapped.csv", row.names = FALSE)
