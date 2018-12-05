########################################

## THESIS OLD FUNCTIONS
## Christopher Salahub
## December 4, 2018

########################################

## a place for functions which were not used in the actual processing (first attempts which proved ineffective or
## non-functioning) but which were just interesting or useful enough to warrant preservation

## IDMatch: a custom take on "merge" that is much slower ########################################################
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

## IdentifySwap: a swap identifying function which was buggy and replaced by "SimpleSwapper" ####################
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

## ColProc: a function to convert lists of multiple entries to single entries, replaced by a generic ############
## write a scale converting summarize function which accepts arguments on how to handle different data types
ColProc <- function(col, typelist = list(character = paste0, numeric = mean, factor = paste0),
                           opts = list(character = list(collapse = ""), numeric = list(na.rm = TRUE),
                                       factor = list(collapse = ""))) {
    ## identify the class being handled
    cl <- class(col)
    ## address lists by unlisting, unless otherwise specified
    if (cl == "list" && !("list" %in% typelist)) {
        col <- unlist(col)
        cl <- class(col)
    }
    ## identify the function being used
    func <- do.call(switch, c(cl, typelist))
    ## and the optional arguments
    optarg <- do.call(switch, c(cl, opts))
    ## evaluate
    do.call(func, c(list(col),optarg))
}

