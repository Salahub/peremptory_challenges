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

## color constants
racePal <- brewer.pal(3, "Set1") # c("steelblue","grey50","firebrick")
whitePal <- c("steelblue","firebrick")

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

## a simple helper to convert multiple races into black, white, and other, due to the prevalence of the first two
## compared to the third
BlackWhiteOther <- function(vals) {
    chars <- as.character(vals)
    ## simply replace elements
    chars[!grepl("Black|White", chars)] <- "Other"
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
    data$WhiteBlack <- BlackWhiteOther(data$Race)
    data$DefWhiteBlack <- BlackWhiteOther(data$DefRace)
    data$VicWhiteBlack <- BlackWhiteOther(data$VictimRace)
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
RemovedJurorEstimates <- function(tofill, data, ident) {
    temp <- FillNAs(tofill, filldata = data, identifier = ident)
    temp2 <- rowSums(data[,grepl(ident, names(data))])
    ## let's see how accurate this is
    plot(temp, temp2, xlab = "Observed and Filled", ylab = "Juror Sums")
    abline(0,1)
    cat("= : ", sum(temp == temp2)/length(temp2), "\n", "< : ", sum(temp2 < temp)/length(temp2), "\n", sep = "")
    ## replace the filled values less than the estimated, for consistency
    temp[temp < temp2] <- temp2[temp < temp2]
    temp
}

## make a text-mining regularization function
StringReg <- function(strs, cosdists = TRUE) {
    ## first set everything to uppercase
    strs <- toupper(strs)
    ## replace punctuation
    strs <- gsub("[^[:alnum:][:space:]']", "", strs)
    ## split on spaces
    strs <- str_split(strs, "\\s+")
    upstop <- paste0(toupper(stopwords()), collapse = " | ")
    ## remove these
    strs1 <- str_replace_all(strs, upstop, " ")
}


## DATA INSPECTION #####################

## load the data if it is not loaded
if ("FullSunshine_Swapped.csv" %in% list.files(ThesisDir)) {
    SwapSunshine <- read.csv(paste0(ThesisDir, "/FullSunshine_Swapped.csv"))
} else source(paste0(ThesisDir, "/DataProcess.R"))
FullSunshine <- read.csv(paste0(ThesisDir, "/FullSunshine.csv"))

## summarize onto the correct scale, the jurors
if ("JuriesAggregated.Rds" %in% list.files()) {
    JurorSunshine <- readRDS("JuriesAggregated.Rds")
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
           main = "Race to Race Removing (Only Struck)", las = 2)
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
mosaicplot(DefStruck ~ DefWhiteBlack + WhiteBlack, data = SRaceKnown, shade = TRUE, las = 2)
mosaicplot(ProStruck ~ DefWhiteBlack + WhiteBlack, data = SRaceKnown, shade = TRUE, las = 2)

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
## and the variables which can be sensibly summarized for each trial
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
JurySummarized <- JurySummarize()

## merge the summaries to the trial sunshine data
TrialSun.sum <- merge(cbind(TrialNumberID = JurySummarized$Juries$TrialNumberID, JurySummarized$Summaries),
                      TrialSunshine, all = TRUE)

## notice that the total removed variables are incomplete, try to correct this where possible using the jury
## summarized data above
TrialSun.sum$DefRemEst <- RemovedJurorEstimates(TrialSun.sum$DefenseTotalRemoved, data = TrialSun.sum,
                                                ident = "Gender.DefRem")
## perform this same procedure for the prosecution removals
TrialSun.sum$ProRemEst <- RemovedJurorEstimates(TrialSun.sum$StateTotalRemoved, data = TrialSun.sum,
                                                ident = "Gender.ProRem")
## synthesize some other variables, simple race indicators
TrialSun.sum$DefWhiteBlack <- as.factor(BlackWhiteOther(TrialSun.sum$DefRace))
TrialSun.sum$DefWhiteOther <- as.factor(c("Other", "White")[grepl("White", TrialSun.sum$DefWhiteBlack) + 1])
## the Kullback-Leibler divergence
TrialSun.sum$KLdiv <- kldiv(TrialSun.sum[,grepl("Jury", names(TrialSun.sum))],
                            TrialSun.sum[,grepl("Venire", names(TrialSun.sum))])

## now look at removals across trials for defense and prosecution
with(TrialSun.sum, plot(jitter(DefRemEst, factor = 2), jitter(ProRemEst, factor = 2), pch = 20,
                        xlab = "Defense Strike Count (jittered)", ylab = "Prosecution Strike Count (jittered)",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.3)))
abline(0,1)
legend(x = "topleft", legend = levels(TrialSun.sum$DefWhiteBlack), col = racePal, pch = 20, title = "Defendant Race")
with(TrialSun.sum, plot(DefRemEst, ProRemEst, pch = 15, cex = 2,
                        xlab = "Defense Strike Count", ylab = "Prosecution Strike Count",
                        col = adjustcolor(whitePal[as.numeric(DefWhiteOther)], alpha.f = 0.2)))
abline(0,1)
legend(x = "topleft", legend = levels(TrialSun.sum$DefWhiteOther), col = racePal, pch = 15, title = "Defendant Race")

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

## both of these plots show a much higher proportion of the black venire is usually struck for both sides, an unsurprising result
## given the the black venire was shown to be smaller in the aggregate statistics, looking at raw counts next:
## for the defense
with(TrialSun.sum, plot(jitter(Race.DefRem.Black, factor = 2), jitter(Race.DefRem.White, factor = 2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(TrialSun.sum$DefWhiteBlack))

with(TrialSun.sum, plot(Race.DefRem.Black, Race.DefRem.White, pch = 15, cex = 2,
                        xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                        xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts",
                        col = adjustcolor(whitePal[as.numeric(DefWhiteOther)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = whitePal, pch = 15, bg = "white", legend = levels(TrialSun.sum$DefWhiteOther))

## for the prosecution
with(TrialSun.sum, plot(jitter(Race.ProRem.Black, factor = 1.2), jitter(Race.ProRem.White, factor = 1.2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,8), ylim = c(0,8), main = "Prosecution Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(TrialSun.sum$DefWhiteBlack))

with(TrialSun.sum, plot(Race.ProRem.Black, Race.ProRem.White, pch = 15, cex = 2,
                        xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                        xlim = c(0,8), ylim = c(0,8), main = "Prosecution Strike Counts",
                        col = adjustcolor(whitePal[as.numeric(DefWhiteOther)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = whitePal, pch = 15, bg = "white", legend = levels(TrialSun.sum$DefWhiteOther))


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
## regularize the charges

## try looking at specific lawyers next, see if they have patterns

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


## GGobi, Diane Cook
