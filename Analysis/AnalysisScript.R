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
library(MultinomialCI)

## CONSTANTS ###########################

## start by defining file locations
ThesisDir <- "c:/Users/Chris/Documents/ETH Zurich/Thesis/Data"
SunshineFile <- paste0(ThesisDir, "/JurySunshineExcel.xlsx")
SunshineSheets <- excel_sheets(SunshineFile)

NorthCarFile <- paste0(ThesisDir,
                       "/Jury Study Data and Materials/NC Jury Selection Study Database6 Dec 2011.csv")

PhillyFile <- paste0(ThesisDir,
                     "/Voir Dire Data & Codebook/capital_venires.csv")

picOut <- "c:/Users/Chris/Documents/ETH Zurich/Thesis/SfSPerempChallenge/Pictures/"

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

## variable group constants
sun.masttabvar <- c("Disposition","DefWhiteBlack", "WhiteBlack", "Gender", "PoliticalAffiliation",
                                       "DefGender")


## FUNCTIONS ###########################

## create a plot which visualizes positional data patterns by a categorical variable
## could encode density as either box sizes or through alpha levels of colour
## the areal representation breaks the "dimensionality" rule of data in Edward Tufte's "The Visual Display of Information",
## to limit the dimensionality of a representation to at most the dimensionality of the data itself
## place the legend labels in the largest box instead of off to the side (didn't really work...)
posboxplot <- function(x, y, cats, boxcolours = NULL, boxwids = 0.8, alphaencoding = TRUE, alphamin = 0.1,
                       areaencoding = FALSE, xlim = range(x) + boxwids*c(-1/1.05,1/1.05), inc.leg = TRUE,
                       ylim = range(y) + boxwids*c(-1/1.05,1/1.05), ...) {
    ## extract the number of categories to be displayed in each small multiple
    ncats <- length(unique(cats))
    ## automatically generate the category colours using color brewer
    if (is.null(boxcolours)) {
        boxcols <- brewer.pal(ncats, "Set2")
        boxcolours <- boxcols
    } else boxcols <- boxcolours
    ## first identify the unique coordinates for the small multiples
    unqPos <- unique(cbind(x,y))
    ## iterate through these, create tables of the categories at each position
    cattabs <- t(apply(unqPos, 1, function(pos) {
        ## generate a count a table
        table(cats[x == pos[1] & y == pos[2]])
    }))
    ## sum these counts to get the total at each position to scale the small multiples
    rowcounts <- rowSums(cattabs)
    ## convert the count table to cumulative proportions at each position
    catprops <- t(apply(cbind(0,cattabs/rowcounts), 1, cumsum))
    ## use these and the table of counts to generate the small multiples
    ## first in the case that opacity encodes density
    if (alphaencoding) {
        ## in the opacity-density case, convert the box colours to rgb and replicate them as necessary
        boxcols <- col2rgb(rep(boxcols, each = nrow(catprops)), alpha = FALSE)/255
        ## convert back to hex, adding the alpha encoding to control opacity
        boxcols <- rgb(t(boxcols),
                       alpha = rep(round((1-alphamin)*rowcounts/max(rowcounts) + alphamin, digits = 4), times = ncats))
      ## in the case size encodes density, simply replicate the colors for the number of positions
    } else boxcols <- rep(boxcols, each = nrow(catprops))
    ## create an empty plot to place the small multiples
    plot(x, y, col = NA, xlim = xlim, ylim = ylim, ...)
    ## determine the width of the small multiple boxes
    if (areaencoding) boxwids <- boxwids*sqrt(rowcounts/max(rowcounts))
    ## define the bottom corner positions of the boxes
    bottomx <- unqPos[,1] - boxwids/2
    bottomy <- unqPos[,2] - boxwids/2
    ## use the bottom corner positions to calculate box extents, with internal borders defined as well
    rectx <- bottomx + catprops*boxwids
    recty <- cbind(rep(bottomy, times = ncats), rep(bottomy + boxwids, times = ncats))
    ## convert the x coordinates into a list of vectors specifying all positions
    xvec <- lapply(1:(ncats+1), function(n) rectx[,n])
    ## place the rectangles by unlisting this structure correctly
    rect(xleft = unlist(xvec[1:ncats]), ybottom = recty[,1], xright = unlist(xvec[2:(ncats+1)]),
         ytop = recty[,2], col = boxcols, border = boxcols)
    ## include a legend if desired
    if (inc.leg) legend(x = "top", legend = colnames(rectx)[-1],fill = boxcolours, bty = "n",
                        xpd = NA, horiz = TRUE)
}

## create a function for proportional parallel coordinate plots
## incorporate possibility to display in a non-proportional absolute way
propparcoord <- function(fact, cats, levs = NULL, proportional = TRUE, includerel = proportional, ylim = NULL,
                         colpal = NULL, ordering = NULL, legpos = "topleft", brptpos = 1, brwid = 4, ...) {
    ## create the x label
    xnm <- deparse(substitute(fact))
    ## perform a type check
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
    if (is.null(colpal)) colpal <- brewer.pal(length(ctab), "Set2")
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

## DEPRECATED: a specific case of the above function which has been replaced by the following function
## make a specific line plot function
parcoordrace <- function() {
    ## clean up the defwhiteblack variable
    DefWhiteBlack_clean <- as.factor(as.character(gsub(",Other|,U", "", sun.juror$DefWhiteBlack)))
    ## first generate the necessary mixed factor
    jurorDef <- with(sun.juror, as.factor(paste(DefWhiteBlack_clean, WhiteBlack, sep = ":")))
    ## generate positions to associate these factor levels
    xpos <- rep(1:5, each = 4) + rep(c(-0.21,-0.07,0.07,0.21), times = 5)
    ## choose disposition levels
    displevs <- c("", "Kept", "S_rem", "D_rem", "C_rem")
    nicelevs <- c("All", "Jury", "Pros.", "Def.", "Cause")
    ## create a table based on the mixed factor
    mixtab <- with(sun.juror, lapply(displevs,
                                         function(disp) {
                                             tab <- table(jurorDef[grepl(disp,Disposition)])/sum(grepl(disp,Disposition))
                                             wraptab <- c(tab,tab[1:4])
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
        } else lines(xpos+0.006*(ind-4)+0.003, mixtab[[ind]], col = colpal[ind-1], lty = 2)})
    ## add axes
    axis(side = 2, at = round(seq(0, maxtab, length.out = 3), digits = 2))
    axis(1, at = xpos, labels = rep(c("Black","Other","Unknown","White"), times = 5))
    axis(1, at = 1:5, labels = c("Black Defendant","Other","Unknown Defendant","White Defendant","Black Defendant"),
         pos = -0.05, xpd = NA, tick = FALSE)
    ## add guide lines coloured by disposition
    lapply(2:length(displevs),
           function(ind) {
               sapply(1:20, function(n) rect(xleft = rep(xpos[n],2)+0.006*(ind-4), xright = rep(xpos[n],2)+0.006*(ind-3),
                                             ybottom = mixtab[[1]][n], ytop = mixtab[[ind]][n], border = colpal[ind-1],
                                             col = colpal[ind-1]))
           })
    ## add legend-ish text
    text(x = xpos[1]-0.01, y = mixtab[[2]][1] + 0.0075, labels = nicelevs[2], pos = 2, cex = 0.75, srt = 90,
         col = colpal[1])
    text(x = xpos[1]+0.01, y = mixtab[[3]][1]-0.02, labels = nicelevs[3], pos = 1, cex = 0.75, srt = 90, col = colpal[2])
    text(x = xpos[1]+0.02, y = mixtab[[4]][1]+0.04, labels = nicelevs[4], pos = 1, cex = 0.75, srt = 90, col = colpal[3])
    text(x = xpos[1]+0.03, y = mixtab[[5]][1], labels = nicelevs[5], pos = 1, cex = 0.75, srt = 90, col = colpal[4])
}

## the better version of the above function, takes an arbitrary three-way contingency table and plots the different conditional
## probabilities of the desired margins
parcoordracev2 <- function(tabl = NULL, tracemar = 1, deslev = NULL, wid = 0.02, addlines = FALSE,
                           space = 0.025, testlines = FALSE, legendlevs = NULL, xtext = NULL, ymax = 0,
                           alpha = 0.05, ...) {
    ## in the default case (no table provided), look at the key race relationships, as these motivated this study
    if (is.null(tabl)) {
        ## for cleanliness, remove those jurors with unknown races
        temp.juror <- sun.juror[sun.juror$WhiteBlack != "U" & sun.juror$DefWhiteBlack != "U",]
        temp.juror$WhiteBlack <- as.factor(as.character(temp.juror$WhiteBlack))
        temp.juror$DefWhiteBlack <- as.factor(as.character(temp.juror$DefWhiteBlack))
        ## perform the same operation for defendant race
        temp.juror$DefWhiteBlack <- as.factor(as.character(gsub(",Other|,U", "", temp.juror$DefWhiteBlack)))
        ## make a table of disposition and both races
        outcometab <- table(temp.juror[,c("Disposition", "DefWhiteBlack", "WhiteBlack")])
    } else { ## if a table is provided simply copy it to the internal "outcometab" argument
        outcometab <- tabl
    }
    ## determine the "non-trace" margins; these specify margins which are combined to define the cases to which the horizontal
    ## line segments correspond
    nontrace <- (1:3)[1:3 != tracemar]
    ## get the dimension names
    tabnames <- dimnames(outcometab)
    ## handle a null desired level setting
    if (is.null(deslev)) deslev <- 1:length(tabnames[[tracemar]])
    ## create a palette
    temPal <- brewer.pal(length(deslev), "Set2")
    ## calculate the conditional probability distribution of outcome given non-trace margins using outcometab
    condout <- apply(outcometab, nontrace, function(margin) margin/sum(margin))
    ## and the sums
    marsums <- apply(outcometab, nontrace, sum)
    ## extract the desired levels from the margin of interest, write this flexibly to allow programmatic margin extraction later
    ## first define the local environment as the calling environment
    evEnv <- environment()
    ## create the language object
    condoutinds <- condoutcall <- quote(condout[,,])
    ## place the levels of interest as the subset argument
    condoutcall[[tracemar+2]] <- deslev
    ## evaluate this to subset the data
    condout <- eval(condoutcall, envir = evEnv)
    ## rename some useful values for ease of reference
    ## the dimensionality of the data
    dims <- dim(condout)
    ## the number of horizontal segments
    nseg <- prod(dims[nontrace])
    ## the number of 'inner' combination values
    innern <- dims[nontrace[1]]
    ## the number of 'outer' combination values (inner and outer refer to axis label positions)
    outern <- dims[nontrace[2]]
    ## add padding between segments to space everything nicely
    ## first replicate the trace margin sums to create a vector of the correct size for the horizontal line generation
    tempx <- rep(c(marsums), times = c(rep(2, nseg-1),1))
    ## replace every even element with the desired space size, then the cumulative sum automatically spaces
    tempx[2*(1:(nseg-1))] <- space*rep(c(rep(1,innern-1),3), length.out = nseg-1)*sum(marsums)
    ## take the cumulative sum to get the positions
    xpos_line <- c(0, cumsum(tempx)/sum(tempx))
    ##xpos <- rep(1:dims[nontrace[2]], each = dims[nontrace[1]]) +
    ##    rep(seq(-0.2, 0.2, length.out = dims[nontrace[1]]), times = dims[nontrace[2]])
    ##xpos <- cumsum(marsums)/sum(marsums)
    ## create the empty plot region
    yscl <- if (ymax == 0) max(condout) else ymax
    plot(NA, xlim = range(xpos_line), ylim = c(0, yscl), xaxt = 'n', xlab = "",
         ylab = "Conditional Probability", ...)
    ## calculate and plot the horizontal lines at the mean values
    ## the old way: calculating the mean of the conditional distributions
    meanline <- apply(condout, nontrace, mean)
    ## this way does not correspond to the hypothesis being tested: that there is no preference for race displayed by
    ## either side, rather under this hypothesis, the expected rate of each combination is given by the product of the
    ## overall rate for each side and the proportion of the venire of
    for(ii in 1:length(meanline)) lines(c(xpos_line[2*ii-1],xpos_line[2*ii]), rep(meanline[ii],2))
    ## add the vertical lines for each cell, save the positions for later
    xpos <- sort(unlist(lapply(1:length(deslev), function(ind) {
        ## first extract the relevant margin to give the y values
        tempind <- condoutinds
        tempind[[tracemar + 2]] <- ind
        yvals <- eval(tempind, envir = evEnv)
        ## use the horizontal line positions and the index to place the vertical lines
        ##adjx <- xpos + wid*(ind - (1/2)*(1 + length(deslev)))
        adjx <- xpos_line[2*(1:nseg)-1] + (ind-1)*diff(xpos_line)[2*(1:nseg)-1]/(dims[tracemar] - 1)
        ## add the corresponding end points
        points(adjx, yvals, col = temPal[ind], pch = 20)
        ## for aesthetics reduce line alpha if confidence intervals are plotted
        if (testlines) lnal <- 0.3 else lnal <- 1
        ## plot the lines
        for (ii in 1:length(adjx)) lines(x = rep(adjx[ii],2), y = c(meanline[ii], yvals[ii]),
                                                         lty = 2, col = adjustcolor(temPal[ind], alpha.f = lnal))
        ## if trace lines (parallel axis plot) are desired plot these
        if (addlines) lines(adjx, yvals, col = temPal[ind], lty = 3)
        ##rect(xleft = adjx - (1/2)*wid, xright = adjx + (1/2)*wid, ybottom = meanline,
        ##     ytop = yvals, col = temPal[ind])
        return(adjx)
    })))
    ## now add the axes
    ## first determine axis label positions
    axpos <- sapply(1:nseg, function(ind) mean(xpos_line[c((2*ind-1),2*ind)]))
    outrpos <- sapply(1:outern, function(ind) mean(range(xpos_line[(2*(ind-1)*innern + 1):(2*ind*innern)])))
    ## add the axis ticks for the inner labels
    axis(1, at = axpos, labels = rep("", length(axpos)))
    ## add the labels to the inner axis
    axis(1, at = axpos, tick = FALSE, labels = rep(tabnames[[nontrace[1]]], times = outern), cex.axis = 0.7,
         pos = -0.02*yscl)
    ## add the labels for the outer axis
    axis(1, at = outrpos, labels = tabnames[[nontrace[2]]], xpd = NA,
         tick = FALSE, pos = -0.08*yscl)
    ## provide the axis title to give context
    if (is.null(xtext)) xtext <- paste0("Inner label: ", names(tabnames)[nontrace[1]],
                                        " | Outer label: ", names(tabnames)[nontrace[2]])
    axis(1, at = mean(range(xpos)), xpd = NA, tick = FALSE, pos = -0.15*yscl,
         labels = xtext)
    ## add testing lines if desired
    if (testlines) {
        ## get x positions
        errpos <- xpos
        ##errpos[3*(1:nseg) - 1] <- axpos
        ## define error bar extensions
        ext <- c(-0.005, 0.005)
        ## iterate through the non-trace margins, apply the multinomialCI function and plot
        cis <- apply(outcometab, nontrace,
                     function(tab) t(multinomialCI(c(tab[deslev], sum(tab[-deslev])), alpha = alpha)[1:length(deslev),]))
        ## reformat this into the correct arrangment dimensionality
        dim(cis) <- c(2, length(cis)/2)
        ## now iterate through and plot
        invisible(sapply(1:length(xpos), function(pos) {
            ## get the appropriate colour
            errcol <- temPal[(pos-1) %% dims[tracemar] + 1]
            ## add the lines
            lines(x = errpos[pos] + ext, y = rep(cis[1,pos], 2), col = adjustcolor(errcol, alpha.f = 0.5))
            lines(x = errpos[pos] + ext, y = rep(cis[2,pos], 2), col = adjustcolor(errcol, alpha.f = 0.5))
            lines(x = rep(errpos[pos], 2), y = cis[,pos], col = adjustcolor(errcol, alpha.f = 0.5))
        }))
    }
    ## add a legend to explain the colours
    if (is.null(legendlevs)) legendlevs <- tabnames[[tracemar]][deslev]
    legend(x = "top", horiz = TRUE, legend = legendlevs, col = temPal, inset = -0.04, cex = 0.7,
           fill = temPal, bg = "white", xpd = NA)
    ##invisible(sapply((0:4)*0.05, function(val) lines(x = c(0,max(xpos)+1), y = rep(val,2), col = "white", lwd = 2)))
}

## the better version of the above function, takes an arbitrary three-way contingency table and plots the different conditional
## probabilities of the desired margins
testplot <- function(tabl = NULL, tracemar = 1, deslev = NULL, wid = 0.02, addlines = FALSE,
                           space = 0.025, testlines = FALSE, expected = NULL, ...) {
    ## in the default case (no table provided), look at the key race relationships, as these motivated this study
    if (is.null(tabl)) {
        ## for cleanliness, remove those jurors with unknown races
        temp.juror <- sun.juror[sun.juror$WhiteBlack != "U" & sun.juror$DefWhiteBlack != "U",]
        temp.juror$WhiteBlack <- as.factor(as.character(temp.juror$WhiteBlack))
        temp.juror$DefWhiteBlack <- as.factor(as.character(temp.juror$DefWhiteBlack))
        ## perform the same operation for defendant race
        temp.juror$DefWhiteBlack <- as.factor(as.character(gsub(",Other|,U", "", temp.juror$DefWhiteBlack)))
        ## make a table of disposition and both races
        outcometab <- table(temp.juror[,c("Disposition", "DefWhiteBlack", "WhiteBlack")])
    } else { ## if a table is provided simply copy it to the internal "outcometab" argument
        outcometab <- tabl
    }
    ## determine the "non-trace" margins; these specify margins which are combined to define the cases to which the horizontal
    ## line segments correspond
    nontrace <- (1:3)[1:3 != tracemar]
    ## get the dimension names
    tabnames <- dimnames(outcometab)
    ## handle a null desired level setting
    if (is.null(deslev)) deslev <- 1:length(tabnames[[tracemar]])
    ## create a palette
    temPal <- brewer.pal(length(deslev), "Set2")
    ## calculate the conditional probability distribution of outcome given non-trace margins using outcometab
    condout <- apply(outcometab, nontrace, function(margin) margin/sum(margin))
    ## and the sums
    marsums <- apply(outcometab, nontrace, sum)
    ## extract the desired levels from the margin of interest, write this flexibly to allow programmatic margin extraction later
    ## first define the local environment as the calling environment
    evEnv <- environment()
    ## create the language object
    condoutinds <- condoutcall <- quote(condout[,,])
    ## place the levels of interest as the subset argument
    condoutcall[[tracemar+2]] <- deslev
    ## evaluate this to subset the data
    condout <- eval(condoutcall, envir = evEnv)
    ## rename some useful values for ease of reference
    ## the dimensionality of the data
    dims <- dim(condout)
    ## the number of horizontal segments
    nseg <- prod(dims)
    ## the number of 'inner' combination values
    innern <- dims[nontrace[1]]
    ## the number of 'outer' combination values (inner and outer refer to axis label positions)
    outern <- dims[nontrace[2]]
    ## the trace dimension as well
    tracen <- dims[tracemar]
    ## add padding between segments to space everything nicely
    ## first replicate the trace margin sums to create a vector of the correct size for the horizontal line generation
    tempx <- rep(c(marsums)/tracen, times = c(rep(tracen + 1, outern*innern-1),tracen))
    ## replace certain elements with the  desired space size, then the cumulative sum automatically spaces
    tempx[(tracen+1)*(1:(innern*outern - 1))] <- space*rep(c(rep(1,innern-1),3),length.out = innern*outern-1)*sum(marsums)
    ## take the cumulative sum to get the positions
    xpos_line <- c(0, cumsum(tempx)/sum(tempx))
    ## get the middle positions using the filter function
    xpos <- c(filter(xpos_line, filter = c(1/2,1/2)))
    ## remove midsections of padding spaces
    xpos <- xpos[-(tracen + 1)*(1:(innern*outern - 1))]
    xpos <- xpos[-length(xpos)]
    ## use the xpos_line to get widths of sections
    xposwids <- diff(xpos_line)[-(tracen + 1)*(1:(innern*outern-1))]
    ## if the expected values are missing, take the original expectation: a uniform distribution conditioned on both
    ## races
    if (is.null(expected)) expected <- rep(apply(condout, nontrace, mean), each = tracen)
    ## create the empty plot region
    plot(NA, xlim = range(xpos_line), ylim = c(0, max(condout[,,])), xaxt = 'n', xlab = "",
         ylab = "Conditional Probability", ...)
    ## plot each line and its corresponding expectation
    invisible(lapply(1:length(xpos), function(ind) {
        ## plot the horizontal line using the relevant values
        lines(x = xpos[ind] + xposwids[ind]*c(-1/2,1/2), y = rep(expected[ind],2))
        ## add the point
        points(x = xpos[ind], y = condout[ind], col = temPal[((ind - 1) %% tracen) + 1], pch = 19)
        ## for aesthetics exclude lines if confidence intervals are plotted
        if (!testlines) lines(x = rep(xpos[ind], 2), y = c(expected[ind], condout[ind]),
                              col = temPal[((ind-1) %% tracen) + 1], lty = 2)
    }))
    ## now add the axes
    ## first determine axis label positions
    axpos <- sapply(1:(innern*outern), function(n) mean(xpos[(tracen*(n-1) + 1):(tracen*n)]))
    outrpos <- sapply(1:outern, function(n) mean(xpos[(tracen*innern*(n-1) + 1):(tracen*innern*n)]))
    ## add the axis ticks for the inner labels
    axis(1, at = axpos, labels = rep("", length(axpos)))
    ## add the labels to the inner axis
    axis(1, at = axpos, tick = FALSE, labels = rep(tabnames[[nontrace[1]]], times = outern), cex.axis = 0.7,
         pos = -0.02*max(condout))
    ## add the labels for the outer axis
    axis(1, at = outrpos, labels = tabnames[[nontrace[2]]], xpd = NA,
         tick = FALSE, pos = -0.08*max(condout[,,]))
    ## provide the axis title to give context
    axis(1, at = mean(range(xpos_line)), xpd = NA, tick = FALSE, pos = -0.15*max(condout),
         labels = paste0("Inner label: ", names(tabnames)[nontrace[1]], " | Outer label: ", names(tabnames)[nontrace[2]]))
    ## add testing lines if desired
    if (testlines) {
        ## get x positions
        errpos <- xpos
        ##errpos[3*(1:nseg) - 1] <- axpos
        ## reformat y positions
        erry <- c(condout)
        ## define error bar extensions
        ext <- c(-0.005, 0.005)
        ## add bars at each position
        invisible(sapply(1:length(erry), function(pos) {
            ## rename the conditional probability for readability
            p <- erry[pos]
            ## extract the relevant margin count
            n <- marsums[floor((pos-1)/dims[tracemar]) + 1]
            ## calculate the binomial error size
            err <- 2*sqrt((p/n)*(1-p))
            ## and the appropriate colour
            errcol <- temPal[(pos-1) %% dims[tracemar] + 1]
            ## add vertical lines and horizontal end lines
            lines(x = errpos[pos] + ext, y = rep(p + err, 2), col = adjustcolor(errcol, alpha.f = 0.5))
            lines(x = errpos[pos] + ext, y = rep(p - err, 2), col = adjustcolor(errcol, alpha.f = 0.5))
            lines(x = rep(errpos[pos], 2), y = c(p + err, p - err), col = adjustcolor(errcol, alpha.f = 0.5))
            }))
    }
    ## add a legen to explain the colours
    legend(x = "top", horiz = TRUE, legend = tabnames[[tracemar]][deslev], col = temPal, inset = -0.04, cex = 0.7,
           fill = temPal, bg = "white", xpd = NA)
}

## back to back histogram plot
back2backh <- function(data1, data2, cols = NULL, legnames = NULL, ...) {
    ## get the data1 and data2 names for the legend if no others are provided
    if (is.null(legnames)) legnames <- c(deparse(substitute(data1)), deparse(substitute(data2)))
    ## generate and save the histograms of the data
    hist1 <- hist(data1, plot = FALSE)
    hist2 <- hist(data2, breaks = hist1$breaks, plot = FALSE)
    ## use these to generate some necessary plotting parameters
    maxden <- max(c(hist1$density, hist2$density))
    nbins <- length(hist1$density)
    ## generate colours if none are provided
    if (is.null(cols)) cols <- c("steelblue","firebrick")
    ## create an empty plot area
    plot(NA, xlim = c(-maxden, maxden), ylim = range(hist1$breaks), xlab = "Relative Frequency", xaxt = 'n', ...)
    ## add vertical separating line
    abline(v = 0)
    ## add an axis
    axispos <- round(seq(0, maxden, length.out = 3),2)
    axis(side = 1, labels = c(axispos[3:2], axispos), at = c(-axispos[3:2], axispos))
    ## plot the histograms back-to-back
    rect(xleft = -hist1$density, ybottom = hist1$breaks[1:nbins],
         xright = rep(0, nbins), ytop = hist1$breaks[2:(nbins+1)], col = cols[1])
    rect(xleft = rep(0, nbins), ybottom = hist2$breaks[1:nbins],
         xright = hist2$density, ytop = hist2$breaks[2:(nbins+1)], col = cols[2])
    ## add a legend
    legend(x = 0, y = max(hist1$breaks), legend = legnames, fill = cols, horiz = TRUE, xpd = NA, xjust = 0.5, yjust = 0,
           bg = "white")
}

## a function to re-level factor variables to make mosaic plots cleaner (useful helper generally)
MatRelevel <- function(data) {
    temp <- lapply(data, function(el) if (is.factor(el)) as.factor(levels(el)[as.numeric(el)]) else el)
    temp <- as.data.frame(temp)
    names(temp) <- names(data)
    temp
}

## a simple helper to convert multiple factor levels into a single 'other' level
FactorReduce <- function(vals, tokeep) {
    chars <- as.character(vals)
    ## simply replace elements
    chars[!grepl(paste0(tokeep, collapse = "|"), chars)] <- "Other"
    chars
}


## DATA INSPECTION #####################

## load the data
if ("FullSunshine_Swapped.csv" %in% list.files(ThesisDir)) {
    sun.swap <- read.csv(paste0(ThesisDir, "/FullSunshine_Swapped.csv"))
} else source(paste0(ThesisDir, "/DataProcess.R"))
FullSunshine <- read.csv(paste0(ThesisDir, "/FullSunshine.csv"))

## summarize onto the correct scale, the jurors
if ("JurorAggregated.Rds" %in% list.files(ThesisDir)) {
    sun.juror <- readRDS(paste0(ThesisDir, "/JurorAggregated.Rds"))
} else sun.juror <- UniqueAgg(sun.swap, by = "JurorNumber", collapse = ",")

## also load the data summarized onto the trial scale
if ("TrialAggregated.Rds" %in% list.files(ThesisDir)) {
    sun.trialsum <- readRDS(paste0(ThesisDir, "/TrialAggregated.Rds"))
} else warning(paste0("No trial aggregated data found in ", ThesisDir))

## there are two juries without charges or other info (noted in the early data cleaning but kept for other analysis), remove these
sun.trialsum <- sun.trialsum[!(sun.trialsum$TrialNumberID %in% c("590-128","710-01")),]

## display information about juror rejection tendencies
mosaicplot(Race ~ Disposition, data = sun.juror, las = 2, shade = TRUE)

## create a race filtered data set
sun.raceknown <- sun.juror[sun.juror$Race != "U" & sun.juror$DefRace != "U",]
sun.raceknown$DefWhiteBlack <- gsub(",U", "", sun.raceknown$DefWhiteBlack)
sun.raceknown <- MatRelevel(sun.raceknown)

## try plotting these
mosaicplot(Race ~ PerempStruck, data = sun.raceknown, main = "Race vs. Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ Disposition, data = sun.raceknown, main = "Race by Trial Status", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = sun.raceknown, main = "Race by Defence Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = sun.raceknown, main = "Race by Prosecution Removal", shade = TRUE, las = 2)
mosaicplot(Race ~ CauseRemoved, data = sun.raceknown, main = "Race by Removal with Cause", shade = TRUE, las = 2)
## it seems that there are significantly different strike habits between the defense and prosecution, but that
## generally the system does not strike at different rates on average
## recall the paper "Ideological Imbalance and the Peremptory Challenge"
par(mfrow = c(1,2))
mosaicplot(Race ~ PoliticalAffiliation, data = sun.raceknown[sun.raceknown$Gender == "M",],
           main = "Affiliation and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ PoliticalAffiliation, data = sun.raceknown[sun.raceknown$Gender == "F",],
           main = "Affiliation and Race (Women)", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = sun.raceknown[sun.raceknown$Gender == "M",],
           main = "Defense Removals and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ DefStruck, data = sun.raceknown[sun.raceknown$Gender == "F",],
           main = "Defense Removals and Race (Women)", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = sun.raceknown[sun.raceknown$Gender == "M",],
           main = "Prosecution Removals and Race (Men)", shade = TRUE, las = 2)
mosaicplot(Race ~ ProStruck, data = sun.raceknown[sun.raceknown$Gender == "F",],
           main = "Prosecution Removals and Race (Women)", shade = TRUE, las = 2)
par(mfrow = c(1,1))
## maybe the same forces are at play here, compare to simulation?
## alternatively, the strong relationship between race and political affiliation provides motivation for even an
## unbiased lawyer to preferentially strike one race or the other

## these mosaic plots can be confusing, and seemed ineffective upon first presentation, try parallel axis plots
## instead
## begin with an overall plot displaying the data at a high level
mosaicplot(PerempStruck ~ WhiteBlack, data = sun.raceknown)
## break it down by race and defendant race, as they were the motivation of this investigation
parcoordracev2(deslev = c(1,2,5), legendlevs = c("Cause","Defence","Prosecution"),
               main = "Conditional Probability of Removal by Race and Race of Defendant",
               xtext = "Inner label: defendant race | outer label: venire member race")
## but are these differences significant?
parcoordracev2(deslev = c(1,2,5), testlines = TRUE, legendlevs = c("Cause","Defence","Prosecution"),
               main = "Conditional Probability of Removal by Race and Race of Defendant",
               xtext = "Inner label: defendant race | outer label: venire member race")

## let's look at other effects by creating a master table which can be summarized in numerous ways
sun.singdef <- MatRelevel(sun.raceknown[!grepl(",", sun.raceknown$DefGender),])
sun.mastab <- table(sun.singdef[,sun.masttabvar])
## break this apart by race and look at the interaction between political affiliation and defendant race
sun.racelist <- lapply(dimnames(sun.mastab)[["WhiteBlack"]],
                       function(race) apply(sun.mastab[,,race,,,], c("Disposition","DefWhiteBlack",
                                                                     "PoliticalAffiliation"), sum))
names(sun.racelist) <- dimnames(sun.mastab)[["WhiteBlack"]]
## turn these into conditional probability tables and plot them on the same scale as the original plot
invisible(lapply(names(sun.racelist), function(nm) {
    tab <- sun.racelist[[nm]]
    tab <- tab[,,c("Dem","Rep","Ind")]
    dev.new()
    parcoordracev2(tab, tracemar = 1, deslev = c(1,2,5), main = paste(nm, "Venire Members"), ymax = 0.2)
}))

## maybe we just have a weird coincidence, what about gender?
parcoordracev2(apply(sun.mastab, c("Disposition","Gender","DefGender"), sum)[,c(1,2),c(1,2)], deslev = c(1,2,5))
## gender and race?
parcoordracev2(apply(sun.mastab, c("Disposition","Gender","WhiteBlack"), sum)[,c(1,2),], deslev = c(1,2,5))
## race and politics?
parcoordracev2(apply(sun.mastab, c("Disposition","PoliticalAffiliation","WhiteBlack"), sum)[,c("Dem", "Rep", "Ind"),],
               deslev = c(1,2,5))

## noticed pattern: the prosecution and judge seem to match, hans and vidmar suggest this could be due to experience (pg 71)
## get lengths
def.lens <- sapply(sun.trialsum$DefAttyName, length)
def.lensyr <- sapply(sun.trialsum$DCYrLicensed, length)
pros.lens <- sapply(sun.trialsum$ProsName, length)
pros.lensyr <- sapply(sun.trialsum$PYrLicensed, length)
## get experiences and plot them
def.exp <- rep(as.numeric(format(sun.trialsum$DateOutcome, "%Y")), times = def.lensyr) - unlist(sun.trialsum$DCYrLicensed)
pros.exp <- rep(as.numeric(format(sun.trialsum$DateOutcome, "%Y")), times = pros.lensyr) - unlist(sun.trialsum$PYrLicensed)
back2backh(def.exp[def.exp < 100 & def.exp > 0 & !is.na(def.exp)],
           pros.exp[pros.exp < 100 & pros.exp > 0 & !is.na(pros.exp)], legnames = c("Defence", "Prosecution"),
           ylab = "Experience")
## seems unlikely, the prosecution is less experienced if anything
## or perhaps the prosecution has more contact with the same judges than the defence?
pros.judge <- table(data.frame(Judge = rep(sun.trialsum$JName, times = sapply(sun.trialsum$ProsName, length)),
                               Pros = unlist(sun.trialsum$ProsName)))
def.judge <- table(data.frame(Judge = rep(sun.trialsum$JName, times = sapply(sun.trialsum$DefAttyName, length)),
                              Def = unlist(sun.trialsum$DefAttyName)))

## models:
##   - try fitting separate logistic regressions Ho model: disp ~ def*pol Ha model: disp ~ def*pol*race
##   - causal modelling: establishing a clear graph of relationships makes the assumptions used to justify adjustments very clear
##   - look into logistic regression modelling again, the statistical rigour of the current tests is not a guarantee that they are
##     good or valid, perhaps the less rigorous
##   - try doing a gender v. def gender fit and plots (negative control or more interesting results)
## do some more thought on models generally, motivate choices more, and be more specific about assumptions

## the independence we want to test here is that of (Race, Disposition)|(Defendant Race)
## filter the data to remove small categories
sun.chitest <- sun.raceknown
sun.chitest$Disposition <- gsub("U_rem", "Unknown", gsub("Foreman", "Kept", sun.chitest$Disposition))
## start by generating a table
dispTab <- table(sun.chitest[,c("DefWhiteBlack", "Disposition", "WhiteBlack")])
## now apply chi-square tests across the proper margin, start by simply generating the residuals
dispRes <- lapply(setNames(1:dim(dispTab)[1], dimnames(dispTab)[[1]]), function(ind) {
    ## extract the two way table of this index
    tab <- dispTab[ind,,]
    tabdf <- dim(tab) - 1
    ## calculate the expected values
    exp <- outer(rowSums(tab), colSums(tab))/sum(tab)
    ## and residuals
    resids <- (tab - exp)/sqrt(exp)
    ## calculate the observed chi-sq value
    chival <- sum(resids^2)
    ## and the p value
    pval <- 1 - pchisq(chival, df = tabdf[1]*tabdf[2])
    ## return these in a list
    list(pval = pval, chisq = chival, df = tabdf[1]*tabdf[2], residuals = resids)
})
## so, there is a significant difference in behaviour at the 5% level, and it is highly significant for white and black jurors

## but these results do not control for much, there could be many factors confounding this result
## first create a new data set for the model building
sun.jurmod <- sun.raceknown
sun.jurmod$DefVisMin <- sun.jurmod$DefWhiteBlack != "White"
sun.jurmod$VisMin <- sun.jurmod$WhiteBlack != "White"
sun.jurmod$DefStruck <- as.logical(sun.jurmod$DefStruck)
sun.jurmod$ProStruck <- as.logical(sun.jurmod$ProStruck)
## now the tricky part, predicting the rejection of a potential juror based on a host of factors, the problem is that we must
## perform multinomial regression on the data, but this multinomial regression makes comparison of certain parameters
## impossible, i.e. there is no mathematical way to compare the impact of race for prosecution and defense rejection statistically
## start by building separate defense and prosecution rejection models
mod.def1 <- glm(DefStruck ~ Race + DefRace + Gender + DefGender + CrimeType + DefAttyType + PoliticalAffiliation,
               data = sun.jurmod, family = binomial)
## very poorly fit model, but the reason should be fairly clear, the crime data in particular has very specific and small
## classes, try building up the model instead, and using the simpler race variable
mod.def2 <- glm(DefStruck ~ WhiteBlack*DefWhiteBlack, data = sun.jurmod, family = binomial)
mod.def3 <- update(mod.def2, formula = DefStruck ~ WhiteBlack + DefWhiteBlack)

## idea: instead of multinomial regression, do poisson regression on the dispTab above, this allows comparisons
sun.rdat <- data.frame(DefRace = rep(c("Black", "Other", "White"), times = 12),
                       Disposition = rep(rep(c("C_rem", "D_rem", "Kept", "S_rem"), each = 3), times = 3),
                       Race = rep(c("Black", "Other", "White"), each = 12),
                       Count = c(dispTab[,c("C_rem","D_rem","Kept","S_rem"),]))
## estimate the saturated model first
mod.rsat <- glm(Count ~ DefRace*Disposition*Race, family = poisson, data = sun.rdat)
## now test if the final interaction term can be removed
mod.r1 <- update(mod.rsat, formula = Count ~ DefRace*Disposition + DefRace*Race + Disposition*Race)
## look at the significance
1 - pchisq(mod.r1$deviance, mod.r1$df.residual)
## so, quite clearly, we cannot remove the three way interaction from the model, as it is highly significant
## the interpretation: the distribution of strikes, kept, etc. depends on both the venire member race and the defendant race
## still, this is perhaps not precise enough, if we change this data to only delineate between those kept and the behaviour of
## the lawyers
sun.rdat2 <- sun.rdat[sun.rdat$Disposition != "C_rem",]
mod.rsat2 <- glm(Count ~ DefRace*Disposition*Race, family = poisson, data = sun.rdat2)

## this is an interesting result, but perhaps it is related to political affiliation (as indicated by the ideological balance
## paper)
## create a table to test this hypothesis
dispTab.pol <- table(MatRelevel(sun.chitest[!(sun.chitest$PoliticalAffiliation %in% c("Lib","U")),
                                            c("Disposition","PoliticalAffiliation","WhiteBlack","DefWhiteBlack")]))
dispTab.pol <- dispTab.pol[c("C_rem","D_rem","Kept","S_rem"),,,]
## convert to a data frame for fitting
sun.pdat <- data.frame(Disp_ = rep(c("C_rem", "D_rem", "Kept", "S_rem"), times = 27),
                       Pol_ = rep(rep(c("Dem", "Ind", "Rep"), each = 4), times = 9),
                       Race_ = rep(rep(c("Black", "Other", "White"), each = 12), times = 3),
                       Def_ = rep(c("Black", "Other", "White"), each = 36),
                       Count = c(dispTab.pol[,,,]))
## fit a model analogous to those fit above, now controlled for political choices in disposition
mod.psat <- glm(Count ~ Def_*Disp_*Race_+ Pol_*Disp_, family = poisson, data = sun.pdat)
## now remove the third order interaction in the model
mod.psattest <- update(mod.psat, formula = Count ~ Def_*Disp_*Race_ + Pol_*Disp_ - Def_:Disp_:Race_)
## test the models
anova(mod.psat, mod.psattest)
1 - pchisq(66.734, 12)

## ideological imbalance, look at politics
parcoordracev2(table(MatRelevel(sun.raceknown[sun.raceknown$PoliticalAffiliation != "U",
                                              c("Disposition","PoliticalAffiliation","WhiteBlack")])),
               deslev = c(1,2,5))

## use radial axis plots to view the lawyers tendencies, especially those who act as both defence and prosecution lawyers
## maybe remove the top lawyers and remodel
## look at the most prolific lawyers for both sides
## subset the data to only lawyers with one case to remove the lawyer dependency

## however, this suggests another question: is this strategy actually successful? That is, does there appear to
## be a relation between the number of peremptory challenges and the court case outcome?
## this may be difficult, there are a lot of factors to consider:
##                  - the lawyer and their track record
##                  - how to judge the success/failure of the case
## see if the presence of challenges is related to the verdict
mosaicplot(PerempStruck ~ Guilty, data = sun.swap, main = "Strikes by Guilt", shade = TRUE)
## on the level of jurors, this is certainly not the case, but this is not the correct scale for the question being
## asked, this question will be addressed again in the case-summarized data

## a third obvious question is a comparison of which races strike or keep which others, used the synthesized variable
## above to try and identify this
mosaicplot(Race ~ StruckBy, data = sun.raceknown, shade = TRUE, main = "Race of Juror to Race Removing Juror",
           las = 2)
mosaicplot(Race ~ StruckBy, data = sun.raceknown[sun.raceknown$StruckBy != "Not Struck",], shade = TRUE,
           main = "Race to Race Removing (Only Removed)", las = 2)
## this plot shows no large systematic deviation between the races in their rejection habits, this suggests, that
## the rejection that occurs is not as simple as a group identity check
## this might be the wrong race to check, though, perhaps we are better comparing the defendant and victim races to
## strike habits
par(mfrow = c(1,3))
mosaicplot(Race ~ DefRace, data = sun.raceknown[as.logical(sun.raceknown$DefStruck),], shade = TRUE,
           main = "Race of Defense-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ DefRace, data = sun.raceknown[as.logical(sun.raceknown$ProStruck),], shade = TRUE,
           main = "Race of Prosecution-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ DefRace, data = sun.raceknown, las = 2, shade = TRUE, main = "Race of Defendant to Venire Race")
par(mfrow = c(1,1))
## this makes the defense look as if they are not racist, though the comparison to the venire distributions in the third
## panel makes that clearer
## these distributions to the venire distribution relative to defendant race, first combine the smaller races into one
## category to make the plot less noisy and more identifiable
## now look at how the two behave relative in their rejections and their acceptance
eikos(WhiteBlack ~ DefWhiteBlack + DefStruck, data = sun.raceknown, xlab_rot = 90,
      main = "Defense Challenges by Race of Venire Member and Defendant")
eikos(WhiteBlack ~ DefWhiteBlack + ProStruck, data = sun.raceknown, xlab_rot = 90,
      main = "Prosecution Challenges by Race of Venire Member and Defendant")
## very interesting, the prosecution seems far more aggressive than the defense
sun.raceknown$DefWhiteBlack[sun.raceknown$DefWhiteBlack == "Black,U"] <- "Black"
sun.raceknown$DefWhiteBlack <- as.factor(as.character(sun.raceknown$DefWhiteBlack))
mosaicplot(DefStruck ~ DefWhiteBlack + WhiteBlack, dir = c("v","v","h"), data = sun.raceknown, shade = TRUE, las = 2,
           xlab = "Defendant Race and Defence Removals", ylab = "Juror Race", main = "Defence Removal by Defendant Race")
mosaicplot(ProStruck ~ DefWhiteBlack + WhiteBlack, dir = c("v","v","h"),  data = sun.raceknown, shade = TRUE, las = 2,
           xlab = "Defendant Race and Prosecution Removals", ylab = "Juror Race", main = "Prosecution Removal by Defendant Race")

## that result is very interesting, the defense strike rates when conditioned on defendant race show no racial
## preference, with a preference to reject white jurors regardless of defendant, but those of the prosecution do,
## maybe by victim race?
mosaicplot(Race ~ VictimRace, data = sun.raceknown[as.logical(sun.raceknown$DefStruck),], shade = TRUE,
           main = "Race of Defense-Struck Jurors to Defendant Race", las = 2)
mosaicplot(Race ~ VictimRace, data = sun.raceknown[as.logical(sun.raceknown$ProStruck),], shade = TRUE,
           main = "Race of Prosecution-Struck Jurors to Defendant Race", las = 2)
## hard to see anything there, the majority of victim races are unknown, maybe looking at the races removed by defense
## attorney type
mosaicplot(DefAttyType ~ Race, data = sun.raceknown[as.logical(sun.raceknown$DefStruck),], shade = TRUE, las = 2,
           main = "Race of Defense-Struck Jurors to Defense Attorney Type")
mosaicplot(WhiteBlack ~ DefAttyType, data = sun.raceknown[as.logical(sun.raceknown$DefStruck),], shade = TRUE, las = 2,
           main = "Race of Defense-Strick Jurors to Defense Attorney Type")
eikos(WhiteBlack ~ DefWhiteBlack + DefAttyType, data = sun.raceknown[as.logical(sun.raceknown$DefStruck),],
      xlab_rot = 90)
## so what have we seen above is that the prosecuton and defense seem to behave very differently in their jury selection
## tactics, the defense seems to reject white individuals at a high rate regardless of the defendant, while the prosecution
## seems to prefer the rejection of venire members of the same race as the defendant

## this last plot shows that different types of lawyers may have different strategies, suggests a new investigation:
## that of lawyer strategy and success based on lawyer tendencies, aggregating by trial first will be easiest

## load the jury summaries
if ("AllJuries.Rds" %in% list.files(ThesisDir)) {
    sun.jursum <- readRDS(paste0(ThesisDir, "/AllJuries.Rds"))
} else cat(paste0("No file 'AllJuries.Rds' in ", ThesisDir))

## now look at removals across trials for defense and prosecution
with(sun.trialsum, plot(jitter(DefRemEst, factor = 2), jitter(ProRemEst, factor = 2), pch = 20,
                        xlab = "Defense Strike Count (jittered)", ylab = "Prosecution Strike Count (jittered)",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.3)))
abline(0,1)
legend(x = "topleft", legend = levels(sun.trialsum$DefWhiteBlack), col = racePal, pch = 20, title = "Defendant Race")
## this is only somewhat informative, it is difficult to see any patterns, use the custom posboxplot function
## first encode relative size of point by alpha blending
with(sun.trialsum, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal, xlab = "Defense Strike Count",
                              ylab = "Prosecution Strike Count", boxwids = 0.8, alphamin = 0.05))
## next by area, another encoding option in this function
with(sun.trialsum, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal, xlab = "Defense Strike Count",
                              ylab = "Prosecution Strike Count", alphaencoding = FALSE, areaencoding = TRUE))

## break apart in more detail for the defense
DefStruckMeans <- with(sun.trialsum, sapply(levels(DefWhiteBlack),
                                            function(rc) c(mean((Race.DefRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                                na.rm = TRUE),
                                                           mean((Race.DefRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                                na.rm = TRUE))))
with(sun.trialsum, plot(Race.DefRem.Black/Race.Venire.Black, Race.DefRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Defense Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
abline(0,1)
points(DefStruckMeans[1,], DefStruckMeans[2,], col = racePal, pch = 4, cex = 2, lwd = 1.5)
legend(x = "topright", title = "Defendant Race", col = c(racePal,"black"), pch = c(rep(20,3),4), bg = "white",
       legend = c(levels(sun.trialsum$DefWhiteBlack),"Mean"))
## hard to see the patterns at the lines, jitter the proportions
with(sun.trialsum, plot(Race.DefRem.Black/Race.Venire.Black + runif(nrow(sun.trialsum), min = -0.03, max = 0.03),
                        Race.DefRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Defense Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.1)))


## and for the prosecution
ProStruckMeans <- with(sun.trialsum, sapply(levels(DefWhiteBlack),
                                            function(rc) c(mean((Race.ProRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                                na.rm = TRUE),
                                                           mean((Race.ProRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                                na.rm = TRUE))))
with(sun.trialsum, plot(Race.ProRem.Black/Race.Venire.Black, Race.ProRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Prosecution Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
abline(0,1)
points(ProStruckMeans[1,], ProStruckMeans[2,], col = racePal, pch = 4, cex = 2, lwd = 1.5)
legend(x = "topright", title = "Defendant Race", col = c(racePal,"black"), pch = c(rep(20,3),4), bg = "white",
       legend = c(levels(sun.trialsum$DefWhiteBlack),"Mean"))
## again hard to see, try jittering
with(sun.trialsum, plot(Race.ProRem.Black/Race.Venire.Black + runif(nrow(sun.trialsum), min = -0.03, max = 0.03),
                        Race.ProRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Prosecution Strike Proportions",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.1)))

## both of these plots show a much higher proportion of the black venire is usually struck for both sides, an unsurprising result
## given the the black venire was shown to be smaller in the aggregate statistics, looking at raw counts next:
## for the defense
with(sun.trialsum, plot(jitter(Race.DefRem.Black, factor = 2), jitter(Race.DefRem.White, factor = 2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(sun.trialsum$DefWhiteBlack))
## use custom plot here
with(sun.trialsum, posboxplot(Race.DefRem.Black, Race.DefRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Defense Strike Counts"))
with(sun.trialsum, posboxplot(Race.DefRem.Black, Race.DefRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Defence Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))

## for the prosecution
with(sun.trialsum, plot(jitter(Race.ProRem.Black, factor = 1.2), jitter(Race.ProRem.White, factor = 1.2), pch = 20,
                        xlab = "Black Venire Strike Count (jittered)", ylab = "White Venire Strike Count (jittered)",
                        xlim = c(0,8), ylim = c(0,8), main = "Prosecution Strike Counts",
                        col = adjustcolor(racePal[as.numeric(DefWhiteBlack)], alpha.f = 0.2)))
legend(x = "topright", title = "Defendant Race", col = racePal, pch = 20, bg = "white", legend = levels(sun.trialsum$DefWhiteBlack))
## more of the custom plot
with(sun.trialsum, posboxplot(Race.ProRem.Black, Race.ProRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Prosecution Strike Counts"))
with(sun.trialsum, posboxplot(Race.ProRem.Black, Race.ProRem.White, DefWhiteBlack, racePal,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Prosecution Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))

## interesting, this shows some patterns in lawyer behaviour at the trial level

## so there are some obvious patterns we can see in the aggregated data and in the individual cases, see if these affect outcomes
with(sun.trialsum, plot(DefRemEst ~ Outcome))
with(sun.trialsum, plot(ProRemEst ~ Outcome))
## nothing obvious there, but there is no control for charges/crime type

## compare these to other variables
mosaicplot(DefRace ~ CrimeType, data = sun.trialsum, las = 2, main = "Crime and Race", shade = TRUE)
mosaicplot(Outcome ~ CrimeType, data = sun.trialsum, las = 2, main = "Crime and Outcome", shade = TRUE)
boxplot(DefRemEst ~ CrimeType, data = sun.trialsum)
with(sun.trialsum, posboxplot(as.numeric(CrimeType), DefRemEst, DefWhiteBlack, racePal, xaxt = "n",
                              ylab = "Defense Strike Count", xlab = "Crime Type"))
axis(side = 1, at = 1:7, labels = levels(sun.trialsum$CrimeType))
boxplot(ProRemEst ~ CrimeType, data = sun.trialsum)
with(sun.trialsum, posboxplot(as.numeric(CrimeType), ProRemEst, DefWhiteBlack, racePal, xaxt = "n",
                              ylab = "Prosecution Strike Count", xlab = "Crime Type"))
axis(side = 1, at = 1:7, labels = levels(sun.trialsum$CrimeType))

## try using the positional boxplots
with(sun.trialsum, posboxplot(DefRemEst, ProRemEst, CrimeType, crimePal))
## too many classes, maybe try drug, sex, theft, other
sun.trialsum$DrugSexTheft <- as.factor(FactorReduce(sun.trialsum$CrimeType, tokeep = c("Drug","Sex","Theft")))
with(sun.trialsum, posboxplot(DefRemEst, ProRemEst, DrugSexTheft, boxcolours = brewer.pal(4, "Set1")))
## also summarize this for the juror data
sun.juror$DrugSexTheft <- as.factor(FactorReduce(sun.juror$CrimeType, tokeep = c("Drug","Sex","Theft")))

## try something different, plot the tendency of the lawyers themselves
## idea: horizontal axis is lawyers, vertical is strikes
LawyerTends <- lapply(unique(c(sun.trialsum$DefAttyName,sun.trialsum$ProsName)),
                      function(name) list(Prosecution = sun.trialsum$ProRemEst[sapply(sun.trialsum$DefAttyName,
                                                                                      function(nms) name %in% nms)],
                                          Defense = sun.trialsum$DefRemEst[sapply(sun.trialsum$ProsName,
                                                                                  function(nms) name %in% nms)]))
## order these by those who did both, then defense, then prosecution
LawyerOrder <- order(sapply(LawyerTends, function(lst) {
    lstlens <- sapply(lst, function(el) length(el) > 0)
    if (all(lstlens)) {
        0
    } else if (lstlens[[2]]) {
        1
    } else 2}
    ))
## reorder the lawyer tendencies
LawyerTends <- LawyerTends[LawyerOrder]
## plot these
plot(NA, xlim = c(1,length(LawyerTends)), ylim = c(0, max(unlist(LawyerTends), na.rm = TRUE)))
invisible(lapply(1:length(LawyerTends), function(ind) {
    vals <- LawyerTends[[ind]]
    points(rep(ind, length(vals$Defense)), vals$Defense, col = adjustcolor("steelblue", alpha.f = 0.p1), pch = 20)
    points(rep(ind, length(vals$Prosecution)), vals$Prosecution, col = adjustcolor("red", alpha.f = 0.1), pch = 20)
}))
lines(1:length(LawyerTends), sapply(LawyerTends, function(el) mean(unlist(el), na.rm = TRUE)))
