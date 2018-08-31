#Library files for courses provided by: Highland Statistics Ltd.
#To cite these functions, use:
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.

#Copyright Highland Statistics LTD.

#####################################################################
#VIF FUNCTION.
#To use:  corvif(YourDataFile)
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
#END VIF FUNCTIONS





##################################################################
##################################################################
#Here are some functions that we took from the pairs help file and
#modified, or wrote ourselves. To cite these, use the r citation: citation()

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
     cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

##################################################################
panel.smooth2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.smooth = "black", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

##################################################################
panel.lines2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}
}

##################################################################
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
##################################################################
##################################################################



##################################################################
##################################################################
#Functions for variograms
#To cite these functions, use:
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.
#Make a variogram for one variable
#To use, type:  MyVariogram(XUTM, YUTM, E , MyDistance=10)
# XUTM is x coordinates
# XUTM is y coordinates
# E is variable used in sample variogram
# MyDistance is the cutoff value for the distances

MyVariogram <- function(x,y,z, MyDistance) {
  library(gstat)
  mydata      <- data.frame(z, x, y)
  coordinates(mydata)    <- c("x", "y")  
  Var <- variogram(z ~ 1, mydata, cutoff = MyDistance)
  data.frame(Var$np, Var$dist, Var$gamma)
}

#Function for making multiple variograms in an xyplot
#To use, type:  MultiVariogram(Z, MyVar,XUTM, YUTM, MyDistance=10)
# Z is a data frame with all the data 
# Character string with variable names that will be used in the xyplot
# XUTM is x coordinates
# XUTM is y coordinates
# MyDistance is the cutoff value for the distances

MultiVariogram <- function(Z, MyVar, x, y, MyDistance) {
  #Z is the data frame with data
  #MyVar is a list of variables for for which variograms are calculated
  #x, y: spatial coordinates
  #MyDistance: limit for distances in the variogram
  
  library(lattice)
  VarAll<- c(NA,NA,NA,NA)
  for (i in MyVar){
    vi <- MyVariogram(x,y,Z[,i], MyDistance)
    vii <- cbind(vi, i)
    VarAll <- rbind(VarAll,vii)
  }
  VarAll <- VarAll[-1,]
  
  P <- xyplot(Var.gamma ~ Var.dist | factor(i), col = 1, type = "p", pch = 16,
              data = VarAll,
              xlab = "Distance",
              ylab = "Semi-variogram",
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "same"),
                            y = list(relation = "same"))
              )
  
  print(P)
}
#End variogram code
##########################################################

#Function for multi-panel Cleveland dotplot.
#The input file must contain no categorical variables
Mydotplot <- function(DataSelected){

P <- dotplot(as.matrix(as.matrix(DataSelected)),
          groups=FALSE,
          strip = strip.custom(bg = 'white',
                               par.strip.text = list(cex = 1.2)),
          scales = list(x = list(relation = "free", draw = TRUE),
                        y = list(relation = "free", draw = FALSE)),
          col=1, cex  = 0.5, pch = 16,
          xlab = list(label = "Value of the variable", cex = 1.5),
          ylab = list(label = "Order of the data from text file", cex = 1.5))
  
print(P)  
  }


#Add more code here:


Mybwplot <- function(Z, MyVar, TargetVar){
#Multipanel boxplots
#Z: data set
#MyVar: character string
#TargetVar: variable for the x-axis..must be a factor
  
  AllY <- as.vector(as.matrix(Z[,MyVar]))
  AllX <- rep(Z[,TargetVar], length(MyVar))
  ID <- rep(MyVar, each = nrow(Z))
  
P <- bwplot(AllY ~ factor(AllX) | ID, horizontal = FALSE,
         ylab = "", xlab = "",
         scales = list(alternating = TRUE,cex.lab = 1.5,
                       x = list(relation = "same",rot =90, abbreviate = TRUE, cex = 1.5),
                       y = list(relation = "free", draw = FALSE)),
         strip = strip.custom(bg = 'white',
                              par.strip.text = list(cex = 1.2)),
         cex = .5,
         par.settings = list(
           box.rectangle = list(col = 1),
           box.umbrella  = list(col = 1),
           plot.symbol   = list(cex = .5, col = 1)))
print(P)
  }



#######################################################
MyxyplotBin <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX | factor(AllID), col = 1,
              strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = TRUE, 
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              xlab = "Covariate",
              ylab = "Probability of presence",
              panel=function(x,y){
                panel.grid(h=-1, v= 2)
                panel.points(x,y,col=1)
                tmp<-gam(y~s(x, k = 4), family = binomial)
                MyData <- data.frame(x = seq(min(x), max(x), length = 25))
                p1 <- predict(tmp, newdata = MyData, type ="response")
                panel.lines(MyData$x,p1, col = 1, lwd = 3)
              })
  
  print(P)
}
#######################################################

#######################################################
Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
                })
  
  print(P)
}
#######################################################

MyxyplotPolygon <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  Z <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(label = "Explanatory variables", cex = 1.5),
              ylab = "",
              strip = function(bg='white',cex.lab = 1.5,...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                t1 <- gam(y~s(x))
                MD1 <- data.frame(x=seq(from = min(x, na.rm = TRUE),
                                        to = max(x, na.rm = TRUE),
                                        length = 100))
                P1 <- predict(t1,   se.fit = TRUE)
                I1 <- order(x)
                xs <- sort(x)
                panel.lines(xs, P1$fit[I1], col = 1)
                panel.polygon(c(xs, rev(xs)),
                              c(P1$fit[I1]-2*P1$se.fit[I1],
                                rev(P1$fit[I1]+2*P1$se.fit[I1])),
                              col = gray(0.7),
                              density = 10 )
                panel.grid(h=-1, v= 2)
                panel.abline(0,0)
                panel.points(x, y, col = 1)
                
              })
  #Because the xyplot is inside a function you need to print 
  #construction below
  print(Z)
}

################################################
#Mypairs
#Make fancy pair plots
Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
      cex.labels =  2,
      lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
        panel.cor(x, y, digits, prefix, cex.cor)}, 
      upper.panel =  function(x, y) points(x, y, 
                                           pch = 16, cex = 0.8, 
                                           col = gray(0.1)))
 #print(P)
}


###########################################################
#Show double zeros in a row-by-column matrix
#To run: ShowZeros(Species)
#Better use short names for the row labels

ShowZeros <- function(XX) {
  #To run this function type: 
  #  ShowZeros(Species)
  	
  #Load packages	
  library(ggplot2)
  library(reshape2)
  library(zoo)
	
    N <- ncol(XX)
    X <- matrix(nrow = N, ncol = N)
    for (i in 1:N) {
	  for (j in i:N){
       X[i,j] <- sum(XX[,i] == 0 & XX[,j] == 0, na.rm = TRUE)
	   X[j,i] <- X[i,j]
	  }
    }
    Zeros <- X / nrow(XX)
    colnames(Zeros) <- colnames(XX)
    rownames(Zeros) <- colnames(XX)




    #Taken from: http://www.r-bloggers.com/controlling-heatmap-colors-with-ggplot2/
    #Set color representation for specific values of the data distribution
    BreakPoints <- c(0, 0.25, 0.5, 0.75, 1)
    ColourQuantiles <- quantile(Zeros, probs = BreakPoints)

    ## use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
    color_palette <- colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8"))(length(ColourQuantiles) - 1)
  
    #Or change to:
    #color_palette[4] <- "red"
    #color_palette[3] <- "orange"
    #color_palette[2] <- "yellow"
    #color_palette[1] <- "white"

 
    # prepare label text (use two adjacent values for range text)
    label_text <- rollapply(round(ColourQuantiles, 2), 
                            width = 2, 
                            by = 1, 
                            FUN = function(i) paste(i, collapse = " - "))
 
    # discretize matrix; this is the most important step, 
    # where for each value we find category of predefined 
    # ranges (modify probs argument of quantile to detail the colors)
    mod_mat <- matrix(findInterval(Zeros, ColourQuantiles, all.inside = TRUE), 
                      nrow = nrow(Zeros))
 
 
    #Prepare the graph
    Zeros.m          <- melt(Zeros)
    Zeros.m$NewValue <- as.vector(mod_mat)
    p <- ggplot(Zeros.m, aes(x = Var1, y = Var2, fill = factor(NewValue))) 
    p <- p + geom_tile(color = "black")
    p <- p + scale_fill_manual(values = color_palette, name = "", labels = label_text) 
    p <- p + scale_x_discrete(expand = c(0, 0))
    p <- p + scale_y_discrete(expand = c(0, 0))
    p <- p + theme(axis.text.x = element_text(angle=45, hjust = 1, size = 5))
    p <- p + xlab("") + ylab("") 
    p <- p + labs(title = "Percentage zeros in common")
    print(p)
}
################################################################################


MyDotplot.ggp2 <- function(Z, varx) {
  library(ggplot2)	
  K     <- length(varx)
  MyData <- data.frame(Y = rep(1:nrow(Z), K),
                       X = as.vector(as.matrix(Z[, varx])),
                       Var = rep(varx, each = nrow(Z))) 

  p <- ggplot(MyData, aes(y = Y, x = X))
  p <- p + geom_point() + ylab("Order of the data") + xlab("Range of the data")
  p <- p + theme(text = element_text(size=15))
  p <- p + facet_wrap(~ Var, scales = "free_x")
  print(p)	
}

Myfapply <- function(Zfactor, INDEX) {
  INDEX <- factor(INDEX)	
  AllLevels <- levels(INDEX)
  MyNewFactor <- NULL
  for (i in AllLevels) {
	ThisLevel <- as.character(Zfactor[INDEX == i] [1])
	MyNewFactor <- c(MyNewFactor, ThisLevel)
  }
  MyNewFactor <- factor(MyNewFactor)
  MyNewFactor	
}


######################################################
MyMultipanel.ggp2 <- function(Z, varx, vary,
                              ylab = "Response variable",
                              addSmoother = FALSE,
                              addRegressionLine = FALSE,
                              addHorizontalLine = FALSE,
                              save = F, 
                              device = 'svg',
                              path = NULL, 
                              width = 7,
                              height = 7) {
  K <- length(varx)
  MyData <- data.frame(Y = rep(as.vector(as.matrix(Z[,vary])), K),
                       X = as.vector(as.matrix(Z[, varx])),
                       Var = rep(varx, each = nrow(Z))) 
  library(ggplot2)
  p <- ggplot(MyData, aes(y = Y, x = X))
  p <- p + geom_point(size=.25) + ylab(ylab) + xlab("Covariates")
  p <- p + theme(text = element_text(size=15))
  if (addSmoother == TRUE) {
  	 p <- p + geom_smooth(se = TRUE, col = "black", lwd = 1)
  }
  if (addRegressionLine == TRUE) {
  	 p <- p + geom_smooth(se = TRUE, col = "black", lwd = 1, method = "lm")
  }
  if (addRegressionLine == TRUE) {
  	 p <- p + geom_smooth(se = TRUE, col = "black", lwd = 1, method = "lm")
  }
  if (addHorizontalLine == TRUE) {
  	 p <- p + geom_hline(yintercept = 0)
  }
  p <- p + facet_wrap(~ Var, scales = "free_x")
  
  if(save==T){
    ggsave(filename = path, plot = p, device = 'svg', width = width, height = height)
  }
  suppressMessages(print(p))
}
######################################################


#Standardize the continuous covariates
MyStd <- function(x) { (x - mean(x)) / sd(x)}

######################################################
MyggplotXYplots=function(df, y, x, formula){
  dfname= deparse(substitute(df)) 
  df.no.0s=df[y>0,]  
  logy=paste0('log',y)  
  
  ######
  p1<-ggplot(df, aes_string(x = x, y = y)) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data incl 0 values'), y = y, x= x) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red') +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  ######
  p2<-ggplot(df,aes(sqrt(df[,x]+1), sqrt(df[,y]+1))) + geom_point(size=.25) +
    geom_smooth(method = "lm", formula = formula) +
    labs(title=paste0(dfname,' CPUE data incl 0 values'), 
         y = paste0('sqrt (',y,'+1)'), 
         x= paste0('sqrt (',x,'+1)')) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red') +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)),
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  #p2a <- ggExtra::ggMarginal(p2, type = "histogram", bins=100, fill='orange')
  
  #####    
  p3<-ggplot(df,aes(log10(df[,x]+1), log10(df[,y]+1))) + geom_point(size=.25) +  
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data incl 0 values'), 
         y = paste0('log10 ',y,' (log10x+1)'), 
         x= paste0('log10 ',x,'( log10x+1)')) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 4) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  #p3a <- ggExtra::ggMarginal(p3, type = "histogram", bins=100, fill='orange')
  
  #####  
  p4<-ggplot(df.no.0s, aes(df.no.0s[,x], df.no.0s[,y])) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data excl 0 values'), y = y, x= x) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 4) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  #p4a <- ggExtra::ggMarginal(p4, type = "histogram", bins=100, fill='orange')
  
  #####
  p5<-ggplot(df.no.0s, aes(sqrt(df.no.0s[,x]), sqrt(df.no.0s[,y]))) + geom_point(size=.25) +  
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data excl 0 values'), 
         y = paste0('sqrt (',y,'+1)'), 
         x= paste0('sqrt (',x,'+1)')) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 4) +
    geom_rug() +
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  #p5a <- ggExtra::ggMarginal(p5, type = "histogram", bins=100, fill='orange')
  
  #####
  p6<-ggplot(df.no.0s, aes(log10(df.no.0s[,x]), log10(df.no.0s[,y]))) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data excl 0 values'), 
         y = paste0('log10 ',y,' (log10x)'), 
         x= paste0('log10 ',x,' (log10x)')) +
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 4, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.9, size = 4, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 4) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  #p6a <- ggExtra::ggMarginal(p6, type = "histogram", bins=100, fill='orange')
  
  #MyGTables=grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
  MyGTables=arrangeGrob(p1, p2, p3, p4, p5, p6, nrow = 2)
  #return(Mygrid)
  
  
  
  #p1a <- ggExtra::ggMarginal(p1, type = "histogram", bins=100, fill='orange')
  # +
  #     stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE, colour = "black") + 
  #     stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "blue") + 
  #     stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red") + 
  #     stat_smooth(method = "gam", formula = y ~ s(x), size = 1, se = FALSE, colour = "green") + 
  #     stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "violet") +
  #     stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 0.15,
  #     formula = formula, parse = TRUE, size = 4) +
  # stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text',
  #                     aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
  #                     label.x.npc = 'right', label.y.npc = 0.35, size = 3)
  # 
  #   stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")), 
  #                formula = formula, 
  #                parse = TRUE)
  
  
  #a <- ggplot_build(p1)
  
  
  
}
######################################################
MyggplotXYplot_byfactor=function(df, y, x, formula, groupf, f.name){
  dfname= deparse(substitute(df))
  #formula <- y ~ x
  p1<-ggplot(df, aes_string(x = x, y = y)) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0('Smoother = ', as.character(f.name)), y = y, x= x) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 2, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 2) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))+
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  p1
  
}
######################################################
MyggplotXYplot_combo=function(df, y, x, formula, groupf, f.name){
  dfname= deparse(substitute(df))
  #formula <- y ~ x
  p1 <- ggplot(df, aes_string(x = x, y = y)) + geom_point(size=.25) + 
    geom_smooth() +
    #geom_smooth(data = df, aes(x = x, y = y), method = "gam")
    labs(title=paste0('Smoother = ', 'GAM'), y = y, x= x) + 
    theme(text = element_text(size=15)) + 
    theme(plot.title = element_text(size = rel(1.25)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))
  
  
  p2<-ggplot(df, aes_string(x = x, y = y)) + geom_point(size=.25) + 
    #geom_smooth(method = "gam", formula = formula) + 
    geom_smooth(method = "gam") + 
    #labs(title=paste0('Smoother = ', as.character(f.name)), y = y, x= x) + 
    #stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
    #             formula = formula, parse = TRUE, size = 2, color='red') +
    #stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
    #                aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
    #                label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    #stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
    #             formula = formula, parse = TRUE, color='red', size = 2) +
    #geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))+
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  # lay <- rbind(c(1,1,2,2,2),
  #              c(1,1,2,2,2))
  #              
  # grid.arrange(grobs = p1,p2, layout_matrix = lay)
  
  grid.arrange(p1, p2, widths=c(0.35,0.65), nrow=1)
  
}
######################################################
MyggplotXYplots_byfactor=function(df, y, x, formula, groupf){
  dfname= deparse(substitute(df))
  df.no.0s=df[y>0,]
  logresponse=paste0('log',y)
  #formula <- y ~ x
  #####
  p1<-ggplot(df, aes_string(x = x, y = y)) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data incl 0 values'), y = y, x= x) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 2, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 2) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25)))+
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  
  p2<-ggplot(df,aes(log10(df[,x]+1), log10(df[,y]+1))) + geom_point(size=.25) +  
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data incl 0 values'), 
         y = paste0('log10 ',y,' (log10x+1)'), 
         x= paste0('log10 ',x,'( log10x+1)')) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 2, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 2) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25))) +
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  p3<-ggplot(df.no.0s, aes(df.no.0s[,x], df.no.0s[,y])) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data excl 0 values'), y = y, x= x) + 
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 2, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 2) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25))) +
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  p4<-ggplot(df.no.0s, aes(log10(df.no.0s[,x]), log10(df.no.0s[,y]))) + geom_point(size=.25) + 
    geom_smooth(method = "lm", formula = formula) + 
    labs(title=paste0(dfname,' CPUE data excl 0 values'), 
         y = paste0('log10 ',y,' (log10x)'), 
         x= paste0('log10 ',x,' (log10x)')) +
    stat_poly_eq(aes(label = paste(..eq.label..)), label.x.npc = "right", label.y.npc = 'top', 
                 formula = formula, parse = TRUE, size = 2, color='red') +
    stat_fit_glance(method = 'lm', method.args = list(formula = formula), geom = 'text', 
                    aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
                    label.x.npc = 'right', label.y.npc = 0.7, size = 2, color='red') +
    stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")),
                 formula = formula, parse = TRUE, color='red', size = 2) +
    geom_rug() + 
    theme(plot.title = element_text(size = rel(2)), 
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5)), 
          axis.text = element_text(size = rel(1.25))) +
    facet_wrap(as.formula(paste("~", groupf)), scales='free')
  
  #lt=rbind(c(1,2,3,4),c(5,5,6,6),c(7,7,8,8))
  #lt=rbind(c(1,2,3),c(4,5,6),c(7,7,7),c(8,8,8),c(9,9,9),c(10,10,10))
  grid.arrange(p1, p2, p3, p4,ncol = 2)
  
  #grid.arrange(p1a, p2a, p3a, p4a, p5a, p6a, nrow = 2)
  #MyGTables=grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
  #MyGTables=arrangeGrob(p1, p2, p3, p4, p5, p6, nrow = 2)
  #return(Mygrid)
}


#####################################################

MyCorrelogram=function(df, covs, title, sign.level=0.95, order='AOE', cex=1) {
  # correlation test
  cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
  }
  
  # correlogram
  par(cex=cex)
  
  #M <-  cor(df[,covs],use="everything") 
  M <-  cor(df[,covs],use="pairwise.complete.obs")
  #M <-  cor(mackerel.2011[,cont.covars],use="pairwise.complete.obs") 
  res1 <- cor.mtest(M, sign.level)
  #http://stackoverflow.com/questions/26574054/how-to-change-font-size-of-the-correlation-coefficient-in-corrplot
  res.plot = corrplot(M, order=order , p.mat = res1[[1]], insig = "p-value", sig.level = -1, tl.cex=0.75, 
                      number.cex=1/par("cex"), cl.cex = 1*par("cex"), type = "lower", title = title, 
                      mar = c(0,0,1,0), tl.srt = 25, tl.offset = 1)
  
}
