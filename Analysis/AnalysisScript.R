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

## color constants
racePal <- brewer.pal(3, "Set2") # c("steelblue","grey50","firebrick")
whitePal <- c("steelblue","firebrick")
crimePal <- brewer.pal(7, "Set1")
dispPal <- brewer.pal(3, "Set2")


## FUNCTIONS ###########################

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

## create a function for proportional line plots
## incorporate possibility to display in a non-proportional absolute way
propparcoord <- function(fact, cats, levs = NULL, proportional = TRUE, includerel = proportional, ylim = NULL,
                         colpal = NULL, ordering = NULL, legpos = "topleft", brptpos = 1, brwid = 4, ...) {
    ## create the x label
    xnm <- deparse(substitute(fact))
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
    ## check if order is null and allow reordering
    if (is.null(ordering)) ordering <- 1:lineLen
    ## set the ylim and other values based on whether a proportional plot is desired
    factab <- table(fact)
    if (proportional) factab <- factab/len else if (is.null(ylim)) ylim <- c(0, max(factab))
    ## generate a palette if one is not given
    if (is.null(colpal)) colpal <- rainbow(length(ctab))
    colpal <- colpal[ordering]
    ## check for missing ylim value
    if (is.null(ylim)) yrng <- c(0,1) else yrng <- ylim
    ## now plot everything
    if (proportional) ynm <- "Proportion" else ynm <- "Count"
    plot(NA, xlim = c(1,lineLen), ylim = yrng, xlab = xnm, ylab = ynm, xaxt = 'n', ...)
    lines(1:lineLen, factab[ordering])
    ## create positions for relative proportions bar chart if this is desired
    if (includerel) {
        ## specify bar chart rectangle bounds
        rectbounds <- seq(0.7, 0.7 - 0.03*(length(ctab)+3), by = -0.03)*yrng[2]
        ## add the reference "total population" bar
        rect(xleft = brptpos, xright = 1+brwid/4, ybottom = rectbounds[1], ytop = rectbounds[2], col = "black")
    }
    ## plot lines and relative size rectangles, depending on options
    for (ii in 1:length(ctab)) {
        ## get the counts for the subset selected by ii
        subsetab <- table(fact[cats == names(ctab)[ii]])
        ## set these proportional if desired
        if (proportional) subsetab <- subsetab/ctab[names(ctab)[ii]]
        ## place these in a line
        lines(1:lineLen, subsetab[ordering], col = colpal[ii])
        ## add the appropriate bar if desired
        if (includerel) {
            rect(xleft = brptpos, xright = 1+(brwid/4)*ctab[ii]/len, ybottom = rectbounds[ii+1], ytop = rectbounds[ii+2],
                 col = colpal[ii])
        }
    }
    ## add a legend and axis
    legend(x = legpos, legend = c("All", names(ctab)), lty = 1, col = c("Black", colpal), title = deparse(substitute(cats)))
    if (includerel) text("Relative Totals", x = 1, y = 0.72*yrng[2], pos = 4)
    axis(1, 1:lineLen, levels(fact)[ordering])
}

## make a specific line plot function
parcoordrace <- function() {
    ## clean up the defwhiteblack variable
    DefWhiteBlack_clean <- as.factor(as.character(gsub(",U", "", JurorSunshine$DefWhiteBlack)))
    ## first generate the necessary mixed factor
    jurorDef <- with(JurorSunshine, as.factor(paste(DefWhiteBlack_clean, WhiteBlack, sep = ":")))
    ## generate positions to associate these factor levels
    xpos <- rep(1:4, each = 3) + rep(c(-0.2,0,0.2), times = 4)
    ## choose disposition levels
    displevs <- c("", "Kept", "S_rem", "D_rem")
    nicelevs <- c("All", "Jury", "Pro. Struck", "Def. Struck")
    ## create a table based on the mixed factor
    mixtab <- with(JurorSunshine, lapply(displevs,
                                         function(disp) {
                                             tab <- table(jurorDef[grepl(disp,Disposition)])/sum(grepl(disp,Disposition))
                                             wraptab <- c(tab,tab[1:3])
                                             wraptab
                                         }))
    ## define a palette
    colpal <- brewer.pal(length(displevs) - 1, "Set2")
    ## extract the max value for plotting purposes
    maxtab <- max(unlist(mixtab))
    ## plot all tables using different colours
    lapply(1:length(mixtab), function(ind) {
        if (ind == 1) {
            plot(x = xpos, y = mixtab[[ind]], xlim = range(xpos), ylim = c(0,maxtab), xlab = "", xaxt = "n", ylab = "Proportion of Data",
                 col = "black", type = 'l', yaxt = 'n')
        } else lines(xpos+0.006*(ind-3)+0.003, mixtab[[ind]], col = colpal[ind-1], lty = 2)})
    ## add axes
    axis(side = 2, at = round(seq(0, maxtab, length.out = 3), digits = 2))
    axis(1, at = xpos, labels = rep(c("Black","Other","White"), times = 4))
    axis(1, at = 1:4, labels = c("Black Defendant","Other","White Defendant","Black Defendant"), pos = -0.05, xpd = NA, tick = FALSE)
    ## add guide lines coloured by disposition
    lapply(2:length(displevs),
           function(ind) {
               sapply(1:12, function(n) rect(xleft = rep(xpos[n],2)+0.006*(ind-3), xright = rep(xpos[n],2)+0.006*(ind-2),
                                             ybottom = mixtab[[1]][n], ytop = mixtab[[ind]][n], border = colpal[ind-1],
                                             col = colpal[ind-1]))
           })
    ## add legend-ish text
    text(x = xpos[1]-0.01, y = mixtab[[2]][1] + 0.0075, labels = nicelevs[2], pos = 2, cex = 0.75, srt = 90,
         col = colpal[1])
    text(x = xpos[1]+0.01, y = mixtab[[3]][1]-0.02, labels = nicelevs[3], pos = 1, cex = 0.75, srt = 90, col = colpal[2])
    text(x = xpos[1]+0.02, y = mixtab[[4]][1]+0.04, labels = nicelevs[4], pos = 1, cex = 0.75, srt = 90, col = colpal[3])
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
                                 colpal = dispPal, ylim = c(0,0.7), ordering = c(3,1,2), includerel = FALSE,
                                 legpos = "topright"))
with(JurorSunshine, propparcoord(WhiteBlack, Disposition, levs = c("Kept","S_rem","D_rem"),
                                 colpal = dispPal, proportional = FALSE))
## now view the defense in detail
with(JurorSunshine[JurorSunshine$Disposition == "D_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  proportional = FALSE, ordering = c(1,3,2), main = "Defense Strikes by Defendant Race"))
with(JurorSunshine[JurorSunshine$Disposition == "D_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  ylim = c(0,0.8), brwid = 2, ordering = c(1,3,2), main = "Defense Strikes by Defendant Race"))
## and the prosecution
with(JurorSunshine[JurorSunshine$Disposition == "S_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  proportional = FALSE, ordering = c(1,3,2)))
with(JurorSunshine[JurorSunshine$Disposition == "S_rem",],
     propparcoord(WhiteBlack, DefWhiteBlack, levs = c("Other","White","Black"), colpal = brewer.pal(3, "Set2"),
                  ylim = c(0,0.7)))

## however, this suggests another question: is this strategy actually successful? That is, does there appear to
## be a relation between the number of peremptory challenges and the court case outcome?
## this may be difficult, there are a lot of factors to consider:
##                  - the lawyer and their track record
##                  - how to judge the success/failure of the case
## see if the presence of challenges is related to the verdict
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

## next add some jury characteristics
if ("AllJuries.Rds" %in% list.files()) {
    JurySummarized <- readRDS("AllJuries.Rds")
} else JurySummarized <- JurySummarize()

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
LawyerTends <- lapply(unique(c(TrialSun.sum$DefAttyName, TrialSun.sum$ProsName)),
                      function(name) list(Prosecution = TrialSun.sum$ProRemEst[sapply(TrialSun.sum$DefAttyName,
                                                                                      function(nms) name %in% nms)],
                                          Defense = TrialSun.sum$DefRemEst[sapply(TrialSun.sum$ProsName,
                                                                                  function(nms) name %in% nms)]))

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
mod1prof <- profile(mod1)
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
