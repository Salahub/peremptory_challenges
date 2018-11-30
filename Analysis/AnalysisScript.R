########################################

## THESIS ANALYSIS SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################

## PACKAGES ############################
library(readxl)
library(MASS)
library(eikosograms)
library(RColorBrewer)
library(stringr)
library(tm)
library(lme4)
library(lmerTest)

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
LevRace <- sort(c("Asian","Black","Hisp","NatAm","Other","U","White"))
LevGen <-  sort(c("F","M","U"))
LevPol <-  sort(c("Dem","Lib","Rep","Ind","U"))

## the relevant variables for the trial summaries
TrialVars <- c("TrialNumberID", "DateOutcome", "JudgeID", "DefAttyType", "VictimName",
               "VictimRace", "VictimGender", "CrimeLocation", "PropertyType",
               "ZipCode.Trials", "StateTotalRemoved", "DefenseTotalRemoved",
               "CourtTotalRemoved", "JDistrict", "JFirstName", "JLastName",
               "JRace", "JGender", "JPoliticalAff", "JVoterRegYr", "JYrApptd",
               "JResCity", "JResZip", "ChargeTxt", "Outcome", "Sentence.FullSunshine",
               "DefendantID.FullSunshine", "DefendantID.DefendantToTrial", "DefRace",
               "DefGender", "DefDOB", "DefAttyID", "DCFirstName", "DCLastName", "DCRace",
               "DCGender", "DCPoliticalAff", "DCYrRegVote", "DCYrLicensed",
               "DCResideCity", "DCResideZip", "ProsecutorID", "ProsecutorFirstName",
               "ProsecutorLastName", "ProsRace", "ProsGender", "ProsPoliticalAff",
               "PYrRegVote", "PYrLicensed", "PResideCity", "PResideZip")

## color constants
racePal <- brewer.pal(3, "RdYlBu") # c("steelblue","grey50","firebrick")
whitePal <- c("steelblue","firebrick")
crimePal <- brewer.pal(7, "Set1")
dispPal <- brewer.pal(3, "Set2")

## define a cleanup tree for charge text clean up later
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


## FUNCTIONS ###########################

## make a Kullback-Leibler divergence function
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

## make a function to summarize trial jury data
JurySummarize <- function(Varnames = c("Disposition", "Race", "Gender", "PoliticalAffiliation")) {
    ## check if a juror summary object exists already
    if (!("JurorSunshine" %in% ls(.GlobalEnv))) {
        ## first group the data for easy access
        Juries <- aggregate(SwapSunshine[, Varnames],
                            by = list(TrialNumberID = SwapSunshine$TrialNumberID, JurorNumer = SwapSunshine$JurorNumber),
                            unique)
    } else Juries <- JurorSunshine
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

## the previous function is designed poorly, instead use generics
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

## a simple helper to convert multiple factor levels into a reduced number
FactorReduce <- function(vals, tokeep) {
    chars <- as.character(vals)
    ## simply replace elements
    chars[!grepl(paste0(tokeep, collapse = "|"), chars)] <- "Other"
    chars
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

## write a function to re-level factor variables to make mosaic plots cleaner
MatRelevel <- function(data) {
    temp <- lapply(data, function(el) if (is.factor(el)) as.factor(levels(el)[as.numeric(el)]) else el)
    temp <- as.data.frame(temp)
    names(temp) <- names(data)
    temp
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

## make a text-mining regularization function
StringReg <- function(strs) {
    ## first set everything to lowercase
    strs <- tolower(strs)
    ## replace specific patterns noticed
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

## create a plot which visualizes positional data patterns by a categorical variable
## could encode density as either box sizes or through alpha levels of colour
## the areal representation breaks the "dimensionality" rule of data in Edward Tufte's "The Visual Display of Information",
## to limit the dimensionality of a representation to at most the dimensionality of the data itself
## place the legend labels in the largest box instead of off to the side (didn't really work...)
posboxplot <- function(x, y, cats, boxcolours = NULL, boxwids = 0.8, alphaencoding = TRUE, alphamin = 0.1,
                       areaencoding = FALSE, xlim = range(x) + boxwids*c(-1/1.05,1/1.05), inc.leg = TRUE,
                       ylim = range(y) + boxwids*c(-1/1.05,1/1.05), ...) {
    ## get an important scale variable
    ncats <- length(unique(cats))
    ## provide box colours if none are given
    if (is.null(boxcolours)) {
        boxcols <- rainbow(ncats)
        boxcolours <- boxcols
    } else boxcols <- boxcolours
    ## first identify the unique positions
    unqPos <- unique(cbind(x,y))
    ## iterate through these, create tables of the categories
    cattabs <- t(apply(unqPos, 1, function(pos) {
        ## generate a count a table
        table(cats[x == pos[1] & y == pos[2]])
    }))
    ## calculate the counts for each location for scaling
    rowcounts <- rowSums(cattabs)
    ## convert the count table to proportions
    catprops <- t(apply(cbind(0,cattabs/rowcounts), 1, cumsum))
    ## use the row counts to determine the colours and box widths
    if (alphaencoding) {
        ## start by converting the box colours to rgb
        boxcols <- col2rgb(rep(boxcols, each = nrow(catprops)), alpha = FALSE)/255
        ## convert back to hex
        boxcols <- rgb(t(boxcols),
                          alpha = rep(round((1-alphamin)*rowcounts/max(rowcounts) + alphamin, digits = 4), times = ncats))
    } else boxcols <- rep(boxcols, each = nrow(catprops))
    ## create an empty plot before determining the box widths to allow default x and y limits to work
    plot(x, y, col = NA, xlim = xlim, ylim = ylim, ...)
    ## determine box widths
    if (areaencoding) boxwids <- boxwids*sqrt(rowcounts/max(rowcounts))
    ## determine some rectangle parameters
    bottomx <- unqPos[,1] - boxwids/2
    bottomy <- unqPos[,2] - boxwids/2
    ## calculate rectangle corner positions
    rectx <- bottomx + catprops*boxwids
    recty <- cbind(rep(bottomy, times = ncats), rep(bottomy + boxwids, times = ncats))
    ## convert the x coordinates into a pair of vectors specifying all positions
    xvec <- lapply(1:(ncats+1), function(n) rectx[,n])
    ## place the rectangles
    rect(xleft = unlist(xvec[1:ncats]), ybottom = recty[,1], xright = unlist(xvec[2:(ncats+1)]),
         ytop = recty[,2], col = boxcols, border = boxcols)
    ## include a legend if desired
    if (inc.leg) legend(x = "top", legend = colnames(rectx)[-1],fill = boxcolours, bty = "n",
                        xpd = NA, horiz = TRUE)
}

## write a back to back histogram function
backtobackhist <- function(data1, data2, colpal = c("steelblue","firebrick"), ...) {
    ## start by generating the two histogram bin sizes
    bins1 <- hist(data1, plot = FALSE)
    histbreaks <- bins1$breaks
    bins2 <- hist(data2, breaks = histbreaks, plot = FALSE)
    maxcount <- max(c(bins2$counts, bins1$counts))
    ## create a plot area
    plot(NA, xlim = c(-maxcount, maxcount), ylim = extendrange(c(data1,data2), f = 0.1), xaxt = "n", ...)
    ## add a line
    abline(v = 0)
    ## add the histograms
    with(bins1, rect(xleft = -counts, ybottom = breaks[1:length(counts)], xright = rep(0, length(counts)),
                     ytop = breaks[2:length(breaks)], col = colpal[1]))
    with(bins2, rect(xleft = rep(0, length(counts)), ybottom = breaks[1:length(counts)], xright = counts,
                     ytop = breaks[2:length(breaks)], col = colpal[2]))
}

## create a function for proportional line plots
## incorporate possibility to display in a non-proportional absolute way
propparcoord <- function(fact, cats, levs = NULL, proportional = TRUE, includerel = proportional, ylim = NULL,
                         colpal = NULL, ...) {
    ## check if the factor is indeed a factor
    if (!is.factor(fact)) fact <- as.factor(fact)
    ## check if levs have been supplied
    if (is.null(levs)) levs <- unique(cats)
    ## extract the levels and indices of interest
    levinds <- cats %in% levs
    ## get the length of the categories provided and a table of frequencies
    ctab <- table(as.character(cats[levinds]))
    len <- length(fact)
    lineLen <- length(levels(fact))
    ## set the ylim and other values based on whether a proportional plot is desired
    factab <- table(fact)
    if (proportional) factab <- factab/len else if (is.null(ylim)) ylim <- c(0, max(factab))
    ## generate a palette if one is not given
    if (is.null(colpal)) colpal <- rainbow(length(ctab))
    ## check for missing ylim value
    if (is.null(ylim)) yrng <- c(0,1) else yrng <- ylim
    ## now plot everything
    if (proportional) ynm <- "Proportion" else ynm <- "Count"
    plot(NA, xlim = c(1,lineLen), ylim = yrng, xlab = "Race", ylab = ynm, xaxt = 'n', ...)
    lines(1:lineLen, factab)
    ## create positions for relative proportions bar chart if this is desired
    if (includerel) {
        ## specify bar chart rectangle bounds
        rectbounds <- seq(0.75, 0.75 - 0.03*(length(ctab)+3), by = -0.03)*yrng[2]
        ## add the reference "total population" bar
        rect(xleft = 1, xright = 1+lineLen/4, ybottom = rectbounds[1], ytop = rectbounds[2], col = "black")
    }
    ## plot lines and relative size rectangles, depending on options
    for (ii in 1:length(ctab)) {
        ## get the counts for the subset selected by ii
        subsetab <- table(fact[cats == names(ctab)[ii]])
        ## set these proportional if desired
        if (proportional) subsetab <- subsetab/ctab[names(ctab)[ii]]
        ## place these in a line
        lines(1:lineLen, subsetab, col = colpal[ii])
        ## add the appropriate bar if desired
        if (includerel) {
            rect(xleft = 1, xright = 1+(lineLen/4)*ctab[ii]/len, ybottom = rectbounds[ii+1], ytop = rectbounds[ii+2],
                 col = colpal[ii])
        }
    }
    ## add a legend and axis
    legend(x = "topleft", legend = c("All", names(ctab)), lty = 1, col = c("Black", colpal))
    if (includerel) text("Relative Totals", x = 1, y = 0.77*yrng[2], pos = 4)
    axis(1, 1:lineLen, levels(fact))
}


## DATA INSPECTION #####################

## load the data if it is not loaded
if ("FullSunshine_Swapped.csv" %in% list.files(ThesisDir)) {
    SwapSunshine <- read.csv(paste0(ThesisDir, "/FullSunshine_Swapped.csv"))
} else source(paste0(ThesisDir, "/DataProcess.R"))
FullSunshine <- read.csv(paste0(ThesisDir, "/FullSunshine.csv"))

## summarize onto the correct scale, the jurors
if ("JurorAggregated.Rds" %in% list.files()) {
    JurorSunshine <- readRDS("JurorAggregated.Rds")
} else JurorSunshine <- UniqueAgg(SwapSunshine, by = "JurorNumber", collapse = ",")

## display information about juror rejection tendencies
mosaicplot(Race ~ Disposition, data = JurorSunshine, las = 2, shade = TRUE)

## synthesize some variables
SwapSunshine <- SynCols(SwapSunshine)
JurorSunshine <- SynCols(JurorSunshine)

## create a race filtered data set
SRaceKnown <- JurorSunshine[JurorSunshine$Race != "U",]
SRaceKnown <- MatRelevel(SRaceKnown)

## try plotting these
mosaicplot(Race ~ PerempStruck, data = SRaceKnown, main = "Race vs. Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ Disposition, data = SRaceKnown, main = "Race by Trial Status", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = SRaceKnown, main = "Race by Defense Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = SRaceKnown, main = "Race by Prosecution Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ CauseRemoved, data = SRaceKnown, main = "Race by Removal with Cause", shade = TRUE, las = 2)
## it seems that there are significantly different strike habits between the defense and prosecution, but that
## generally the system does not strike at different rates on average
## recall the paper "Ideological Imbalance and the Peremptory Challenge"
par(mfrow = c(1,2))
mosaicplot(Race ~ PoliticalAffiliation, data = SRaceKnown[SRaceKnown$Gender == "M",],
           main = "Affiliation and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ PoliticalAffiliation, data = SRaceKnown[SRaceKnown$Gender == "F",],
           main = "Affiliation and Race (Women)", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = SRaceKnown[SRaceKnown$Gender == "M",],
           main = "Defense Removals and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = SRaceKnown[SRaceKnown$Gender == "F",],
           main = "Defense Removals and Race (Women)", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = SRaceKnown[SRaceKnown$Gender == "M",],
           main = "Prosecution Removals and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = SRaceKnown[SRaceKnown$Gender == "F",],
           main = "Prosecution Removals and Race (Women)", shade = TRUE, las = 2)
par(mfrow = c(1,1))
## maybe the same forces are at play here, compare to simulation?

## these mosaic plots can be confusing, and seemed ineffective upon first presentation, try parallel axis plots
## instead

## first compare defense strikes, prosecution strikes, venire, and jurors
with(JurorSunshine, propparcoord(Race, Disposition, levs = c("Kept","S_rem","D_rem"),
                                 colpal = dispPal, ylim = c(0,0.7)))
with(JurorSunshine, propparcoord(WhiteBlack, Disposition, levs = c("Kept","S_rem","D_rem"),
                                 colpal = dispPal, ylim = c(0,0.7)))
with(JurorSunshine, propparcoord(WhiteBlack, Disposition, levs = c("Kept","S_rem","D_rem"),
                                 colpal = dispPal, proportional = FALSE))
## now view the defense in detail
with(JurorSunshine[JurorSunshine$Disposition == "D_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  proportional = TRUE, ylim = c(0,0.8)))
## and the prosecution
with(JurorSunshine[JurorSunshine$Disposition == "S_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  proportional = TRUE, ylim = c(0,0.7)))

## however, this suggests another question: is this strategy actually successful? That is, does there appear to
## be a relation between the number of peremptory challenges and the court case outcome?
## this may be difficult, there are a lot of factors to consider:
##                  - the lawyer and their track record
##                  - how to judge the success/failure of the case
## start by making a simple indicator of guilty/not guilty ignoring the complexities of such a verdict
SwapSunshine$Guilty <- grepl("Guilty", SwapSunshine$Outcome)
## see if the presence of challenges is related to this verdict
mosaicplot(PerempStruck ~ Guilty, data = SwapSunshine, main = "Strikes by Guilt", shade = TRUE)
## on the level of jurors, this is certainly not the case, but this is not the correct scale for the question being
## asked, this question will be addressed again in the case-summarized data

## a third obvious question is a comparison of which races strike or keep which others, used the synthesized variable
## above to try and identify this
mosaicplot(Race ~ StruckBy, data = SRaceKnown, shade = TRUE, main = "Race of Juror to Race Removing Juror",
           las = 2)
mosaicplot(Race ~ StruckBy, data = SRaceKnown[SRaceKnown$StruckBy != "Not Struck",], shade = TRUE,
           main = "Race to Race Removing (Only Removed)", las = 2)
## this plot shows no large systematic deviation between the races in their rejection habits, this suggests, that
## the rejection that occurs is not as simple as a group identity check
## this might be the wrong race to check, though, perhaps we are better comparing the defendant and victim races to
## strike habits
par(mfrow = c(1,3))
mosaicplot(Race ~ DefRace, data = SRaceKnown[SRaceKnown$DefStruck,], shade = TRUE,
           main = "Race of Defense-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ DefRace, data = SRaceKnown[SRaceKnown$ProStruck,], shade = TRUE,
           main = "Race of Prosecution-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ DefRace, data = SRaceKnown, las = 2, shade = TRUE, main = "Race of Defendant to Venire Race")
par(mfrow = c(1,1))
## this makes the defense look as if they are not racist, though the comparison to the venire distributions in the third
## panel makes that clearer
## these distributions to the venire distribution relative to defendant race, first combine the smaller races into one
## category to make the plot less noisy and more identifiable
## now look at how the two behave relative in their rejections and their acceptance
eikos(WhiteBlack ~ DefWhiteBlack + DefStruck, data = SRaceKnown, xlab_rot = 90,
      main = "Defense Challenges by Race of Venire Member and Defendant")
eikos(WhiteBlack ~ DefWhiteBlack + ProStruck, data = SRaceKnown, xlab_rot = 90,
      main = "Prosecution Challenges by Race of Venire Member and Defendant")
## very interesting, the prosecution seems far more aggressive than the defense
SRaceKnown$DefWhiteBlack[SRaceKnown$DefWhiteBlack == "Black,U"] <- "Black"
SRaceKnown$DefWhiteBlack <- as.factor(as.character(SRaceKnown$DefWhiteBlack))
mosaicplot(DefStruck ~ DefWhiteBlack + WhiteBlack, dir = c("v","v","h"), data = SRaceKnown, shade = TRUE, las = 2,
           xlab = "Defendant Race and Defence Removals", ylab = "Juror Race", main = "Defence Removal by Defendant Race")
mosaicplot(ProStruck ~ DefWhiteBlack + WhiteBlack, dir = c("v","v","h"),  data = SRaceKnown, shade = TRUE, las = 2,
           xlab = "Defendant Race and Prosecution Removals", ylab = "Juror Race", main = "Prosecution Removal by Defendant Race")

## that result is very interesting, the defense strike rates when conditioned on defendant race show no racial
## preference, with a preference to reject white jurors regardless of defendant, but those of the prosecution do,
## maybe by victim race?
mosaicplot(Race ~ VictimRace, data = SRaceKnown[SRaceKnown$DefStruck,], shade = TRUE,
           main = "Race of Defense-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ VictimRace, data = SRaceKnown[SRaceKnown$ProStruck,], shade = TRUE,
           main = "Race of Prosecution-Struck Jurors to Defendant Race", las = 2)
## hard to see anything there, the majority of victim races are unknown, maybe looking at the races removed by defense
## attorney type
mosaicplot(DefAttyType ~ Race, data = SRaceKnown[SRaceKnown$DefStruck,], shade = TRUE, las = 2,
           main = "Race of Defense-Struck Jurors to Defense Attorney Type")
mosaicplot(WhiteBlack ~ DefAttyType, data = SRaceKnown[SRaceKnown$DefStruck,], shade = TRUE, las = 2,
           main = "Race of Defense-Strick Jurors to Defense Attorney Type")
eikos(WhiteBlack ~ DefWhiteBlack + DefAttyType, data = SRaceKnown[SRaceKnown$DefStruck,],
      xlab_rot = 90)
## so what have we seen above is that the prosecuton and defense seem to behave very differently in their jury selection
## tactics, the defense seems to reject white individuals at a high rate regardless of the defendant, while the prosecution
## seems to prefer the rejection of venire members of the same race as the defendant

## this last plot shows that different types of lawyers may have different strategies, suggests a new investigation:
## that of lawyer strategy and success based on lawyer tendencies, aggregating by trial first will be easiest

## identify the unique trials
Trials <- unique(SwapSunshine$TrialNumberID)
## extract information about these trials, note that grouping occurs on the trial ID, defendant ID, and charge ID levels,
## as the trials frequency involve multiple charges and defendants, which makes them less clean
TrialSunshine <- aggregate(SwapSunshine[,TrialVars],
                           by = list(SwapSunshine$TrialNumberID, SwapSunshine$DefendantID.DefendantToTrial,
                                     SwapSunshine$ID.Charges),
                           unique)
TrialSunshine$Group.1 <- NULL
TrialSunshine$Group.2 <- NULL
TrialSunshine$Group.3 <- NULL

## next add some jury characteristics
if ("AllJuries.Rds" %in% list.files()) {
    JurySummarized <- readRDS("AllJuries.Rds")
} else JurySummarized <- JurySummarize()

## merge the summaries to the trial sunshine data
TrialSun.sum <- merge(cbind(TrialNumberID = JurySummarized$Juries$TrialNumberID, JurySummarized$Summaries),
                      TrialSunshine, all = TRUE)

## notice that the total removed variables are incomplete, try to correct this where possible using the jury
## summarized data above
TrialSun.sum$DefRemEst <- RemovedJurorEstimates(TrialSun.sum$DefenseTotalRemoved, data = TrialSun.sum,
                                                ident = "Gender.DefRem", plot = FALSE)
## perform this same procedure for the prosecution removals
TrialSun.sum$ProRemEst <- RemovedJurorEstimates(TrialSun.sum$StateTotalRemoved, data = TrialSun.sum,
                                                ident = "Gender.ProRem", plot = FALSE)
## synthesize some other variables, simple race indicators
TrialSun.sum$DefWhiteBlack <- as.factor(FactorReduce(TrialSun.sum$DefRace, tokeep = c("Black", "White")))
TrialSun.sum$DefWhiteOther <- as.factor(FactorReduce(TrialSun.sum$DefWhiteBlack, tokeep = "White"))
## guilt indicator
TrialSun.sum$Guilty <- as.factor(grepl("Guilty", TrialSun.sum$Outcome))
## the Kullback-Leibler divergence
TrialSun.sum$KLdiv <- kldiv(TrialSun.sum[,grepl("Jury", names(TrialSun.sum))],
                            TrialSun.sum[,grepl("Venire", names(TrialSun.sum))])

## now look at removals across trials for defense and prosecution
with(TrialSun.sum, plot(jitter(DefRemEst, factor = 2), jitter(ProRemEst, factor = 2), pch = 20,
                        xlab = "Defense Strike Count (jittered)", ylab = "Prosecution Strike Count (jittered)",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.3)))
abline(0,1)
legend(x = "topleft", legend = levels(TrialSun.sum$DefWhiteBlack), col = racePal, pch = 20, title = "Defendant Race")
## this is only somewhat informative, it is difficult to see any patterns, use the custom posboxplot function
## first encode relative size of point by alpha blending
with(TrialSun.sum, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal, xlab = "Defense Strike Count",
                              ylab = "Prosecution Strike Count", boxwids = 0.8, alphamin = 0.05))
legend(x = "topleft", legend = levels(TrialSun.sum$DefWhiteBlack), col = racePal, pch = 15, title = "Defendant Race")
## next by area, another encoding option in this function
with(TrialSun.sum, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal, xlab = "Defense Strike Count",
                              ylab = "Prosecution Strike Count", alphaencoding = FALSE, areaencoding = TRUE))

## break apart in more detail for the defense
DefStruckMeans <- with(TrialSun.sum, sapply(levels(DefWhiteBlack),
                                            function(rc) c(mean((Race.DefRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                                na.rm = TRUE),
                                                           mean((Race.DefRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                                na.rm = TRUE))))
with(TrialSun.sum, plot(Race.DefRem.Black/Race.Venire.Black, Race.DefRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Defense Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
abline(0,1)
points(DefStruckMeans[1,], DefStruckMeans[2,], col = racePal, pch = 4, cex = 2, lwd = 1.5)
legend(x = "topright", title = "Defendant Race", col = c(racePal,"black"), pch = c(rep(20,3),4), bg = "white",
       legend = c(levels(TrialSun.sum$DefWhiteBlack),"Mean"))
## hard to see the patterns at the lines, jitter the proportions
with(TrialSun.sum, plot(Race.DefRem.Black/Race.Venire.Black + runif(nrow(TrialSun.sum), min = -0.03, max = 0.03),
                        Race.DefRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Defense Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.1)))


## and for the prosecution
ProStruckMeans <- with(TrialSun.sum, sapply(levels(DefWhiteBlack),
                                            function(rc) c(mean((Race.ProRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                                na.rm = TRUE),
                                                           mean((Race.ProRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                                na.rm = TRUE))))
with(TrialSun.sum, plot(Race.ProRem.Black/Race.Venire.Black, Race.ProRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Prosecution Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
abline(0,1)
points(ProStruckMeans[1,], ProStruckMeans[2,], col = racePal, pch = 4, cex = 2, lwd = 1.5)
legend(x = "topright", title = "Defendant Race", col = c(racePal,"black"), pch = c(rep(20,3),4), bg = "white",
       legend = c(levels(TrialSun.sum$DefWhiteBlack),"Mean"))
## again hard to see, try jittering
with(TrialSun.sum, plot(Race.ProRem.Black/Race.Venire.Black + runif(nrow(TrialSun.sum), min = -0.03, max = 0.03),
                        Race.ProRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Prosecution Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.1)))

## both of these plots show a much higher proportion of the black venire is usually struck for both sides, an unsurprising result
## given the the black venire was shown to be smaller in the aggregate statistics, looking at raw counts next:
## for the defense
with(TrialSun.sum, plot(jitter(Race.DefRem.Black, factor = 2), jitter(Race.DefRem.White, factor = 2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(TrialSun.sum$DefWhiteBlack))
## use custom plot here
with(TrialSun.sum, posboxplot(Race.DefRem.Black, Race.DefRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts"))
with(TrialSun.sum, posboxplot(Race.DefRem.Black, Race.DefRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Defence Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))

## for the prosecution
with(TrialSun.sum, plot(jitter(Race.ProRem.Black, factor = 1.2), jitter(Race.ProRem.White, factor = 1.2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,8), ylim = c(0,8), main = "Prosecution Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(TrialSun.sum$DefWhiteBlack))
## more of the custom plot
with(TrialSun.sum, posboxplot(Race.ProRem.Black, Race.ProRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Prosecution Strike Counts"))
with(TrialSun.sum, posboxplot(Race.ProRem.Black, Race.ProRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Prosecution Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))

## this suggests that there could be a jury-based racial imbalance due to the venire rather than the racial patterns of lawyer strikes

## let's look at the rejection profiles using a pairs plot
pairs(TrialSun.sum[,paste0("Race.", rep(c("Def","Pro"), each = 2), "Rem.", rep(c("Black","White"),2))],
      col = adjustcolor(racePal[as.numeric(TrialSun.sum$DefWhiteBlack)], alpha.f = 0.2), pch = 15)
pairs(TrialSun.sum[,paste0("Race.", rep(c("Def","Pro"), each = 2), "Rem.", rep(c("Black","White"),2))],
      col = adjustcolor(whitePal[as.numeric(TrialSun.sum$DefWhiteOther)], alpha.f = 0.2), pch = 15, cex = 2)

## hard to see anything from that, there don't seem to be any obvious patterns
## so there are some obvious patterns we can see in the aggregated data and in the individual cases, see if these affect outcomes
with(TrialSun.sum, plot(DefRemEst ~ Outcome))
with(TrialSun.sum, plot(ProRemEst ~ Outcome))
## nothing obvious there, but there is no control for charges/crime type

## regularize the charges at the trial level
regCharg <- StringReg(TrialSun.sum$ChargeTxt)
## classify these into a charge tree and aggregate this at the coarsest level
aggCharg <- treeAgg(stringTree(regCharg, chargeTree))
## these can be further classified into crime classes
crimes.trial <- CrimeClassify(aggCharg, regCharg)
## convert these classes into a factor for the data, start with a generic "other" vector
TrialSun.sum$CrimeType <- rep("Other", nrow(TrialSun.sum))
## now populate it
for (nm in sort(names(crimes.trial))) TrialSun.sum$CrimeType[crimes.trial[[nm]]] <- nm
TrialSun.sum$CrimeType <- as.factor(TrialSun.sum$CrimeType)

## do the same processing to the juror summarized data
regCharg <- StringReg(JurorSunshine$ChargeTxt)
aggCharg <- treeAgg(stringTree(regCharg, chargeTree))
crimes.juror <- CrimeClassify(aggCharg, regCharg)
JurorSunshine$CrimeType <- rep("Other", nrow(JurorSunshine))
for (nm in sort(names(crimes.juror))) JurorSunshine$CrimeType[crimes.juror[[nm]]] <- nm
JurorSunshine$CrimeType <- as.factor(JurorSunshine$CrimeType)

## compare these to other variables
mosaicplot(DefRace ~ CrimeType, data = TrialSun.sum, las = 2, main = "Crime and Race", shade = TRUE)
mosaicplot(Outcome ~ CrimeType, data = TrialSun.sum, las = 2, main = "Crime and Outcome", shade = TRUE)
boxplot(DefRemEst ~ CrimeType, data = TrialSun.sum)
with(TrialSun.sum, posboxplot(as.numeric(CrimeType), DefRemEst, DefWhiteBlack, racePal, xaxt = "n",
                              ylab = "Defense Strike Count", xlab = "Crime Type"))
axis(side = 1, at = 1:7, labels = levels(TrialSun.sum$CrimeType))
boxplot(ProRemEst ~ CrimeType, data = TrialSun.sum)
with(TrialSun.sum, posboxplot(as.numeric(CrimeType), ProRemEst, DefWhiteBlack, racePal, xaxt = "n",
                              ylab = "Prosecution Strike Count", xlab = "Crime Type"))
axis(side = 1, at = 1:7, labels = levels(TrialSun.sum$CrimeType))

## try using the positional boxplots
with(TrialSun.sum, posboxplot(DefRemEst, ProRemEst, CrimeType, crimePal))
## too many classes, maybe try drug, sex, theft, other
TrialSun.sum$DrugSexTheft <- as.factor(FactorReduce(TrialSun.sum$CrimeType, tokeep = c("Drug","Sex","Theft")))
with(TrialSun.sum, posboxplot(DefRemEst, ProRemEst, DrugSexTheft, boxcolours = brewer.pal(4, "Set1")))
## also summarize this for the juror data
JurorSunshine$DrugSexTheft <- as.factor(FactorReduce(JurorSunshine$CrimeType, tokeep = c("Drug","Sex","Theft")))

## try something different, plot the tendency of the lawyers themselves
## idea: horizontal axis is lawyers, vertical is strikes


## with all of that attempted, we should try to identify whether the lawyers have significantly different
## patterns of behaviour, or whether they are mostly homogeneous, to evaluate this, try fitting a mixed model for
## rejection at the juror level
mod1 <- glmer(PerempStruck ~ WhiteBlack + DefWhiteBlack + (1|DefAttyID) + (1|ProsecutorID),
              data = JurorSunshine, family = binomial)
## failed to converge, but note that the DefWhiteBlack has one very small group with a black and unknown defendant,
## try grouping this with the larger black defendant group
JurorSunshine$DefWhiteBlack2 <- JurorSunshine$DefWhiteBlack
JurorSunshine$DefWhiteBlack2[JurorSunshine$DefWhiteBlack2 == "Black,U"] <- "Black"
JurorSunshine$DefWhiteBlack2 <- as.factor(as.character(JurorSunshine$DefWhiteBlack2))
## now try again
mod1 <- glmer(PerempStruck ~ WhiteBlack + DefWhiteBlack2 + (1|DefAttyID) + (1|ProsecutorID),
              data = JurorSunshine, family = binomial)
## it converged, now try an interaction term between the races
mod2 <- glmer(PerempStruck ~ WhiteBlack:DefWhiteBlack2 + (1|DefAttyID) + (1|ProsecutorID),
              data = JurorSunshine, family = binomial)
## failed to converge.... maybe subset over only cases with known races
mod2 <- glmer(PerempStruck ~ WhiteBlack:DefWhiteBlack2 + (1|DefAttyID) + (1|ProsecutorID),
              data = JurorSunshine[JurorSunshine$WhiteBlack != "Other" & JurorSunshine$DefWhiteBlack2 != "Other", ],
              family = binomial)
## expand to include the crime type to assess its impact
mod2 <- glmer(PerempStruck ~ WhiteBlack + DefWhiteBlack2 + (1|DefAttyID) + (1|ProsecutorID) + CrimeType,
              data = JurorSunshine, family = binomial)
## that failed to converge as well, maybe drop the defendant race, as the earlier model found that unimportant
mod1 <- glm(PerempStruck ~ WhiteBlack + DefWhiteBlack, data = JurorSunshine, family = binomial)
## that worked, but told nothing useful (no random effect control

## try looking now at the KL divergence values and see what patterns might be there
plot(density(TrialSun.sum$KLdiv))
hist(TrialSun.sum$KLdiv)
## looks like a gamma or beta distribution
## plot the KL div by guilt status
with(TrialSun.sum, plot(KLdiv ~ Outcome))
par(mar = c(5.1,6.1,4.1,2.1))
with(TrialSun.sum, plot(KLdiv, jitter(as.numeric(Outcome)), yaxt = 'n'))
axis(side = 2, at = 1:6, labels = abbreviate(levels(TrialSun.sum$Outcome), minlength = 5), las = 2)
## no strong relationship there that can be seen
with(TrialSun.sum, plot(DefRemEst, KLdiv))
with(TrialSun.sum, plot(ProRemEst, KLdiv))
with(TrialSun.sum, plot(DefRemEst + ProRemEst, KLdiv))

## look at black/white juror counts by guilt
with(TrialSun.sum, posboxplot(Race.Jury.White, Race.Jury.Black, as.factor(Guilty), boxcolours = whitePal,
                              alphaencoding = FALSE, areaencoding = TRUE))
## what about by politics?
with(TrialSun.sum, posboxplot(PolAff.Jury.Rep, PolAff.Jury.Dem, as.factor(Guilty), boxcolours = whitePal,
                              alphaencoding = FALSE, areaencoding = TRUE))
