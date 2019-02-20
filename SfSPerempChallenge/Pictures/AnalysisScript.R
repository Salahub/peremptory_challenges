########################################

## THESIS ANALYSIS SCRIPT
## Christopher Salahub
## Sept 26, 2018

########################################

## PACKAGES ############################
library(readxl)
library(MASS)
library(RColorBrewer)
library(stringr)
library(tm)
library(MultinomialCI)
library(nnet)
library(lme4)

## CONSTANTS ###########################

## start by defining file locations
ThesisDir <- "c:/Users/Chris/Documents/ETH Zurich/Thesis/Data"
SunshineFile <- paste0(ThesisDir, "/JurySunshineExcel.xlsx")
SunshineSheets <- excel_sheets(SunshineFile)

NorthCarFile <- paste0(ThesisDir,"/StubbornNA.csv")

PhillyFile <- paste0(ThesisDir,"/PhillyNA.csv")

picOut <- "c:/Users/Chris/Documents/ETH Zurich/Thesis/SfSPerempChallenge/Pictures/"

## next the factor level codes as given in the codebook and regularized here
## regularization: - political affiliation "N" replaced with "I" for all entries
LevRace <- sort(c("Asian","Black","Hisp","NatAm","Other","U","White"))
LevGen <-  sort(c("F","M","U"))
LevPol <-  sort(c("Dem","Lib","Rep","Ind","U"))

## color constants
racePal <- brewer.pal(3, "Set2") # c("steelblue","grey50","firebrick")
racePal2 <- c(rgb(0,114,178,maxColorValue=255),rgb(213,94,0,maxColorValue=255),
              rgb(204,121,167,maxColorValue=255))
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
         ytop = recty[,2], col = boxcols, border = "black")##boxcols)
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
mobileplot <- function(tabl = NULL, tracemar = 1, deslev = NULL, wid = 0.02, addlines = FALSE,
                       space = 0.025, testlines = FALSE, legendlevs = NULL, xtext = NULL, ymax = 0,
                       alpha = 0.05, temPal = NULL, ...) {
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
    if (is.null(temPal)) temPal <- brewer.pal(length(deslev), "Set2")
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

## write a simple anova testing function for multiple formulae
anovaGen <- function(formlist, method, data, ...) {
    ## apply each method to the data using all formulae in formlist
    models <- lapply(formlist, function(el) {
        method(formula = el, data = data, ...)
    })
    ## return fits and print anova
    anfit <- do.call(anova, args = sapply(as.list(names(models)), as.symbol), envir = as.environment(models))
    models <- c(models, anfit)
    print(anfit)
    return(models)
}

## write a small function to convert a later table to a latex table
CItoLatex <- function(tab, rnd = 2, refs = c(1,4,7)) {
    paste(sapply(rownames(tab),
                 function(nm) {paste(nm,
                                     paste0(sapply(refs, function(ind) {
                                             paste0(round(tab[nm,ind],rnd), " (",
                                                    round(tab[nm,ind+1],rnd),
                                                    ",", round(tab[nm,ind+2],rnd), ")")}),
                                  collapse = " & "), sep = " & ")}), collapse = "\\")
}

## another small function to plot model parameters in a dot plot
modeldotplot <- function(CItab, xvals, labs, refs = c(1,4,7), ...) {
    ## get y positions of coefficients
    multmod.coefy <- rev(rep(1:nrow(CItab), each = 3) + c(-0.1,0,0.1))
    ## generate a nice plot
    par(mar = c(5.1,10.1,4.1,2.1))
    plot(NA, xlim = range(xvals), ylim = range(multmod.coefy), yaxt = "n", xlab = "Coefficient Value",
         ylab = "")
    axis(side = 2, labels = labs, at = rev(1:nrow(CItab)), las = 2)
    ## add vertical lines to indicate zero and other integer values for reference
    abline(v = floor(seq(min(xvals),max(xvals),by=1)), col = adjustcolor("gray50", alpha.f = 0.3))
    abline(v = 0, col = "black")
    ## add lines for each row and effect
    invisible(sapply(1:nrow(CItab),
                     function(row) sapply(refs,
                       function(ind) {
                           adj <- switch(as.character(ind), "1" = 1, "4" = 2, "7" = 3)
                           vals <- CItab[row,ind:(ind+2)]
                           lines(x = vals[c(2,3)], y = rep(multmod.coefy[3*(row-1) + adj],2),
                                 col = racePal[adj])
                           points(x = vals[1], y = multmod.coefy[3*(row-1) + adj],
                                  col = racePal[adj], pch = 20)
                       })))
    ## add a legend
    legend(x = mean(c(-3,2)), y = max(multmod.coefy)*1.02, horiz = TRUE, xjust = 0.5, yjust = 0,
           legend = c("Cause","Defence","Prosecution"), fill = racePal, xpd = NA, bg = "white", cex = 0.7)
    ## reset parameters
    par(mar = c(5.1,4.1,4.1,2.1))
}

## add a function to display symmetric rejection boundaries for a given distribution function
dispsym <- function(bounds, labels, distfun = dnorm, xrng = c(-3,3), npts = 201, ...) {
    ## set x values and y values
    xpts <- seq(from = xrng[1], to = xrng[2], length.out = npts)
    ypts <- distfun(xpts)
    ## first plot the distribution function
    plot(x = xpts, y = ypts, type = 'l', ...)
    abline(h = 0)
    ## next add symmetric rejection intervals for each provided bound
    absbs <- abs(bounds)
    invisible(sapply(absbs, function(bnd) {
        ltxt <- labels[absbs == bnd]
        lowx <- xpts <= -bnd
        upx <- xpts >= bnd
        polygon(x = c(rep(xpts[lowx], times = c(rep(1, sum(lowx)-1),2)), xpts[1]),
                y = c(ypts[lowx],0,0), col = adjustcolor("firebrick", alpha.f = 0.2))
        polygon(x = c(rep(xpts[upx], times = c(2,rep(1, sum(upx)-1))), xpts[npts]),
                y = c(0,ypts[upx],0), col = adjustcolor("firebrick", alpha.f = 0.2))
        text(x = -bnd, y = 0, labels = ltxt, adj = c(0,-0.5), srt = 90)
    }))
}


## DATA INSPECTION #####################

## load the sunshine data
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

## load the stubborn and Philadelphia data sets
stub <- read.csv(NorthCarFile)
phil <- read.csv(PhillyFile)

## there are two juries without charges or other info (noted in the early data cleaning but kept for other analysis), remove these
sun.trialsum <- sun.trialsum[!(sun.trialsum$TrialNumberID %in% c("590-128","710-01")),]

## also perform a more reasonable collapse of the first degree variable
sun.juror$FirstDeg <- sapply(str_split(sun.juror$FirstDeg, ","), function(el) any(as.logical(el)))
sun.juror$AnyDefVisMin <- sapply(str_split(sun.juror$DefVisMin, ","), function(el) any(as.logical(el)))

## display information about juror rejection tendencies
mosaicplot(Race ~ Disposition, data = sun.juror, las = 2, shade = TRUE)

## create a race filtered data set
sun.raceknown <- sun.juror[sun.juror$Race != "U" & sun.juror$DefRace != "U",]
sun.raceknown$DefWhiteBlack <- gsub(",U", "", sun.raceknown$DefWhiteBlack)
sun.raceknown <- MatRelevel(sun.raceknown)

## begin with an overall plot displaying the data at a high level
mosaicplot(PerempStruck ~ WhiteBlack, data = sun.raceknown)
## break it down by race and defendant race, as they were the motivation of this investigation
mobileplot(deslev = c(1,2,5), legendlevs = c("Cause","Defence","Prosecution"),
               main = "Conditional Probability of Removal by Race and Race of Defendant",
               xtext = "Inner Label: Defendant Race | Outer Label: Venire Member Race")
## but are these differences significant?
mobileplot(deslev = c(1,2,5), testlines = TRUE, legendlevs = c("Cause","Defence","Prosecution"),
               main = "Conditional Probability of Removal by Race and Race of Defendant",
           xtext = "Inner Label: Defendant Race | Outer Label: Venire Member Race")

## let's check for the other data
## this requires some significant filtering of the sunshine data to only include capital trials, and additionally to
## ensure that the disposition data is comparable to the stubborn data, which did not include challenges with cause
with(sun.raceknown[sun.raceknown$FirstDeg & sun.raceknown$Disposition != "C_rem",],
     mobileplot(table(Disposition, DefWhiteBlack == "Black", WhiteBlack == "Black"), deslev = c(2,5), testlines = T,
                temPal = racePal[c(2,3)], ymax = 0.6,
                legendlevs = c("Defence","Prosecution"),
                main = "Conditional Probability of Removal (Sunshine)",
                xtext = "Inner Label: Defendant Black | Outer Label: Venire Member Black"))
## now the stubborn data
with(stub,
     mobileplot(table(DispSimp, DefWhiteBlack == "Black", WhiteBlack == "Black"),
                deslev = c(1,3), testlines = T, legendlevs = c("Defence","Prosecution"),
                temPal = racePal[c(2,3)], ymax = 0.6,
                main = "Conditional Probability of Removal (Stubborn)",
                xtext = "Inner Label: Defendant Black | Outer Label: Venire Member Black"))
## finally the philly data
with(phil[phil$DispSimp != "C_rem",],
     mobileplot(table(DispSimp, DefWhiteBlack %in% "Black", WhiteBlack %in% "Black"),
                deslev = c(2,4), testlines = T, legendlevs = c("Defence","Prosecution"),
                temPal = racePal[c(2,3)], ymax = 0.6,
                main = "Conditional Probability of Removal (Philadelphia)",
                xtext = "Inner Label: Defendant Black | Outer Label: Venire Member Black"))

## what about race and political affiliation
mobileplot(table(MatRelevel(sun.juror[sun.juror$WhiteBlack != "U" & sun.juror$PoliticalAffiliation != "U" &
                                      sun.juror$Gender != "U",
                                      c("PoliticalAffiliation","Gender","WhiteBlack")])),
           deslev = c(1,2,4), main = "Venire Member Political Affiliation by Race and Gender",
           legendlevs = c("Democrat","Independent","Republican"), temPal = c("steelblue","grey50","firebrick"),
           xtext = "Inner Label: Gender | Outer Label: Race")

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
    ## pdf(paste0(picOut, "Pol", nm, ".pdf")) # output values
    dev.new()
    mobileplot(tab, tracemar = 1, deslev = c(1,2,5), main = paste(nm, "Venire Members"), ymax = 0.2,
               xtext = "Inner Label: Defendant Race | Outer Label: Venire Member Poitical Affiliation",
               legendlevs = c("Cause", "Defence", "Prosecution"))
    ## dev.off() # output values
}))

## maybe we just have a weird coincidence, what about gender?
mobileplot(apply(sun.mastab, c("Disposition","Gender","DefGender"), sum)[,c(1,2),c(1,2)], deslev = c(1,2,5),
                      main = "Strike Source by Venire Member Gender and Defendant Gender",
           legendlevs = c("Cause","Defence","Prosecution"),
           xtext = "Inner Level: Gender | Outer Level: Defendant Gender")
## gender and race?
mobileplot(apply(sun.mastab, c("Disposition","Gender","WhiteBlack"), sum)[,c(1,2),], deslev = c(1,2,5),
           main = "Strike Source by Venire Member Race and Gender",
           legendlevs = c("Cause","Defence","Prosecution"),
           xtext = "Inner Level: Gender | Outer Level: Race")
## race and politics?
mobileplot(apply(sun.mastab, c("Disposition","PoliticalAffiliation","WhiteBlack"), sum)[,c("Dem", "Rep", "Ind"),],
           deslev = c(1,2,5))

## try these combinations for the other data
## start with the restricted sunshine data
with(MatRelevel(sun.raceknown[sun.raceknown$FirstDeg & sun.raceknown$Disposition != "C_rem" &
                             sun.raceknown$Gender != "U",]),
     mobileplot(table(DispSimp, Gender, WhiteBlack == "Black"), deslev = c(1,3), testlines = T,
                temPal = racePal[c(2,3)], ymax = 0.6,
                main = "Conditional Probability by Gender and Race (Sunshine)",
                legendlevs = c("Defence","Prosecution"),
                xtext = "Inner Level: Gender | Outer Level: Black"))
## the stubborn data
with(stub,
     mobileplot(table(DispSimp, Gender, WhiteBlack == "Black"), deslev = c(1,3), testlines = T,
                temPal = racePal[c(2,3)], ymax = 0.6,
                main = "Conditional Probability by Gender and Race (Stubborn)",
                legendlevs = c("Defence","Prosecution"),
                xtext = "Inner Level: Gender | Outer Level: Black"))
## now the philly data
with(phil[phil$DispSimp != "C_rem",],
     mobileplot(table(DispSimp, Gender, WhiteBlack == "Black"), deslev = c(2,4), testlines = T,
                temPal = racePal[c(2,3)], ymax = 0.6,
                main = "Conditional Probability by Gender and Race (Philadelphia)",
                legendlevs = c("Defence","Prosecution"),
                xtext = "Inner Level: Gender | Outer Level: Black"))

## do some modelling for more precise controls
## first reorder the levels to make a comparison of those kept (a linear transformation of the rejection average displayed above)
## to all other disposition possibilities
sun.multmod <- MatRelevel(sun.raceknown[!grepl("U", sun.raceknown$Disposition) & !grepl("U", sun.raceknown$PoliticalAffiliation) &
                                        !grepl("U", sun.raceknown$Gender) & !grepl("U|,", sun.raceknown$DefGender),])
sun.multmod$DispSimp <- with(sun.multmod, factor(DispSimp, levels = levels(DispSimp)[c(3,1,2,4)]))
## rename the relevant variables to make the displays easier to read
names(sun.multmod)[match(c("WhiteBlack", "DefWhiteBlack", "PoliticalAffiliation", "Gender", "DefGender"), names(sun.multmod))] <-
    c("Race_", "DRace_", "Pol_", "Sex_", "DSex_")
## create a list of relevant formulae
formulist <- list(extra = as.formula("DispSimp ~ Race_*DRace_ + Pol_ + Sex_*DSex_"),
                  full = as.formula("DispSimp ~ Race_*DRace_ + Pol_ + Sex_ + DSex_"),
                  noraceint = as.formula("DispSimp ~ Race_ + DRace_ + Pol_ + Sex_ + DSex_"),
                  nosex = as.formula("DispSimp ~ Race_ + DRace_ + Pol_ + DSex_"),
                  nopol = as.formula("DispSimp ~ Race_ + DRace_ + Sex_ + DSex_"),
                  norace = as.formula("DispSimp ~ DRace_ + Pol_ + Sex_ + DSex_"))
## fit multinomial regression models using the above formulae, which are chosen to investigate the factors above
multmod.lst <- lapply(formulist, multinom, data = sun.multmod)
## let's look at some anova comparisons between these models, all of which are nested in the full model
multmod.aov <- do.call(anova, sapply(as.list(names(multmod.lst)), as.symbol), envir = as.environment(multmod.lst))
multmod.aov
## these anova comparisons show quite clearly that the race is highly significant even when the other factors are controlled
## in particular, note that the gender interaction is not significant when added to the minimal model tested, so if a final
## model was to be chosen, it would not include this effect
multmod.fin <- multinom(formulist$full, data = sun.multmod)
## test the null deviance of this model, does it fit adequately?
multmod.entir <- multinom(DispSimp ~ Race_*DRace_*Pol_*Sex_*DSex_, data = sun.multmod, maxit = 250)
anova(multmod.entir, multmod.fin)
## combine these values into a table for easy reading and copying
multmod.finsum <- summary(multmod.fin)
multmod.finCI <- cbind(t(multmod.finsum$coefficients),
                       t(multmod.finsum$coefficients) - 2*t(multmod.finsum$standard.errors),
                       t(multmod.finsum$coefficients) + 2*t(multmod.finsum$standard.errors))[,c(1,4,7,2,5,8,3,6,9)]
## generate the table
CItoLatex(multmod.finCI)

## use the model dot plot function to plot these coefficients
modeldotplot(multmod.finCI, xvals = c(-3,2),
             labs = c("(Intercept)", "Other","White","Def. Other","Def. White","Independent","Libertarian",
                      "Republican","Male","Def. Male","Other & Def. Other","White & Def. Other",
                      "Other & Def. White","White & Def. White"))
## and a smaller, cleaner plot
multmod.filtCI <- multmod.finCI[!grepl("Other|Pol_Lib", rownames(multmod.finCI)),]
modeldotplot(multmod.filtCI, xvals = range(multmod.filtCI),
             labs = c("(Intercept)","White","Def. White","Independent",
                          "Republican","Male","Def. Male","White & Def. White"))

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


## TRIAL LEVEL DATA ####################
## load the jury summaries
if ("AllJuries.Rds" %in% list.files(ThesisDir)) {
    sun.jursum <- readRDS(paste0(ThesisDir, "/AllJuries.Rds"))
} else cat(paste0("No file 'AllJuries.Rds' in ", ThesisDir))

## now look at removals across trials for defense and prosecution
with(sun.trialsum, plot(jitter(DefRemEst, factor = 2), jitter(ProRemEst, factor = 2), pch = 20,
                        xlab = "Defence Strike Count (jittered)", ylab = "Prosecution Strike Count (jittered)",
                        col = adjustcolor(racePal2[as.numeric(DefWhiteBlack)], alpha.f = 0.3)))
abline(0,1)
legend(x = "topleft", legend = levels(sun.trialsum$DefWhiteBlack), col = racePal, pch = 20, title = "Defendant Race")
## remove the unknown defendant races
sun.trialrace <- sun.trialsum[sun.trialsum$DefWhiteBlack != "U",]
sun.trialrace$DefWhiteBlack <- as.factor(as.character(sun.trialrace$DefWhiteBlack))
## this is only somewhat informative, it is difficult to see any patterns, use the custom posboxplot function
## first encode relative size of point by alpha blending
with(sun.trialrace, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal2, xlab = "Defence Strike Count",
                              ylab = "Prosecution Strike Count", boxwids = 0.8, alphamin = 0.05))
## next by area, another encoding option in this function
with(sun.trialrace, posboxplot(DefRemEst, ProRemEst, DefWhiteBlack, boxcolours = racePal2, xlab = "Defence Strike Count",
                               ylab = "Prosecution Strike Count", alphaencoding = FALSE, areaencoding = TRUE,
                               main = "Prosecution and Defence Strikes by Trial"))
abline(a = 0, b = 1)

## what about counts for each side?
## use custom plot here
with(sun.trialrace, posboxplot(Race.DefRem.Black, Race.DefRem.White, DefWhiteBlack, racePal2,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Defence Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))
## more of the custom plot for the prosecution
with(sun.trialrace, posboxplot(Race.ProRem.Black, Race.ProRem.White, DefWhiteBlack, racePal2,
                              xlab = "Black Venire Strike Count", ylab = "White Venire Strike Count",
                              xlim = c(0,13), ylim = c(0,13), main = "Prosecution Strike Counts",
                              alphaencoding = FALSE, areaencoding = TRUE))

## break apart in more detail for the defence, first find the means by defendant race
DefStruckMeans <- with(sun.trialrace,
                       sapply(levels(DefWhiteBlack),
                              function(rc) c(mean((Race.DefRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                  na.rm = TRUE),
                                             mean((Race.DefRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                  na.rm = TRUE))))
## plot the proportion of venire members struck by race given defendant race
with(sun.trialrace, plot(Race.DefRem.Black/Race.Venire.Black, Race.DefRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Defence Strike Proportions",
                        col = adjustcolor(racePal2[as.numeric(DefWhiteBlack)], alpha.f = 0.5)))
abline(0,1)
points(DefStruckMeans[1,], DefStruckMeans[2,], bg = racePal2, pch = 22, cex = 1.5, col = "black")
legend(x = "topright", title = "Defendant Race", col = c(racePal2,"black"), pch = c(rep(20,3),22), bg = "white",
       legend = c(levels(sun.trialrace$DefWhiteBlack),"Mean"))

## do the same for the prosecution, start by calculating means
ProStruckMeans <- with(sun.trialrace, sapply(levels(DefWhiteBlack),
                                            function(rc) c(mean((Race.ProRem.Black/Race.Venire.Black)[DefWhiteBlack == rc],
                                                                na.rm = TRUE),
                                                           mean((Race.ProRem.White/Race.Venire.White)[DefWhiteBlack == rc],
                                                                na.rm = TRUE))))
## plot the strike proportions by defendant race
with(sun.trialrace, plot(Race.ProRem.Black/Race.Venire.Black, Race.ProRem.White/Race.Venire.White, pch = 20,
                        xlab = "Black Venire Proportion Struck", ylab = "White Venire Proportion Struck",
                        xlim = c(0,1), ylim = c(0,1), main = "Prosecution Strike Proportions",
                        col = adjustcolor(racePal2[as.numeric(DefWhiteBlack)], alpha.f = 0.5)))
abline(0,1)
points(ProStruckMeans[1,], ProStruckMeans[2,], bg = racePal2, pch = 22, cex = 1.5, col = "black")
legend(x = "topright", title = "Defendant Race", col = c(racePal2,"black"), pch = c(rep(20,3),22), bg = "white",
       legend = c(levels(sun.trialrace$DefWhiteBlack),"Mean"))
