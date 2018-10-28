########################################

## THESIS ANALYSIS SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################


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
LevGen <-  sort(c("F","M","U"))
LevPol <-  sort(c("D","L","R","I","U"))


## DATA INSPECTION #####################

## load the data if it is not loaded
if ("FullSunshine_Swapped.csv" %in% list.files(ThesisDir)) {
    SwapSunshine <- read.csv("FullSunshine_Swapped.csv")
} else source(paste0(ThesisDir, "/DataProcess.R"))
FullSunshine <- read.csv("FullSunshine_Swapped.csv")

## display information about juror rejection tendencies
mosaicplot(Race ~ Disposition, data = SwapSunshine, las = 2, shade = TRUE)
## too busy, synthesize some variables to clearly indicate the results of defense and prosecution selection
SwapSunshine$VisibleMinor <- SwapSunshine$Race != "White"
SwapSunshine$PerempStruck <- grepl("S_rem|D_rem", SwapSunshine$Disposition)
SwapSunshine$DefStruck <- SwapSunshine$Disposition == "D_rem"
SwapSunshine$ProStruck <- SwapSunshine$Disposition == "S_rem"
SwapSunshine$CauseRemoved <- SwapSunshine$Disposition == "C_rem"
## lets look at which race struck each juror
SwapSunshine$StruckBy <- as.factor(sapply(1:nrow(SwapSunshine),
                                          function(ind) {
                                              dis <- as.character(SwapSunshine$Disposition[ind])
                                              if (dis == "S_rem") {
                                                  as.character(SwapSunshine$ProsRace[ind])
                                              } else if (dis == "D_rem") {
                                                  as.character(SwapSunshine$DCRace[ind])
                                              } else "Not Struck"
                                          }))

## create a race filtered data set
SRaceKnown <- SwapSunshine[SwapSunshine$Race != "U",]
SRaceKnown$Race <- as.factor(levels(SRaceKnown$Race)[as.numeric(SRaceKnown$Race)])

## try plotting these
mosaicplot(Race ~ PerempStruck, data = SRaceKnown, main = "Minority vs. Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ Disposition, data = SRaceKnown, main = "Race by Trial Status", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = SRaceKnown, main = "Race by Defense Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = SRaceKnown, main = "Race by Prosecution Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ CauseRemoved, data = SRaceKnown, main = "Race by Removal with Cause", shade = TRUE, las = 2)
## it seems that there are significantly different strike habits between the defense and prosecution, but that
## generally the system does not strike at different rates on average

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
## this plot shows no large systematic deviation between the races in their rejection habits, this suggests, that
## the rejection that occurs is not as simple as a group identity check

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
               "DefGender", "DefDOB", "DCFirstName", "DCLastName", "DCRace",
               "DCGender", "DCPoliticalAff", "DCYrLicensed", "DCResideCity", "DCResideZIP",
               "ProsecutorID", "ProsecutorFirstName", "ProsecutorLastName", "ProsRace",
               "ProsGender", "ProsPoliticalAff", "PYrRegVote", "PYrLicensed",
               "PResideCity", "PResideZip")
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
