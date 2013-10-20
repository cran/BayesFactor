
## @knitr echo=FALSE,message=FALSE,results='hide'
options(markdown.HTML.stylesheet = 'extra/manual.css')
library(knitr)
options(digits=3)
require(graphics)
set.seed(2)


## @knitr message=FALSE
library(BayesFactor)


## @knitr echo=FALSE,message=FALSE,results='hide'
options(BFprogress = FALSE)
bfversion = BFInfo()
session = sessionInfo()[[1]]
rversion = paste(session$version.string," on ",session$platform,sep="")


## @knitr onesampdata
data(sleep)

## Compute difference scores
diffScores = sleep$extra[1:10] - sleep$extra[11:20]

## Traditional two-tailed t test
t.test(diffScores)


## @knitr onesampt
bf = ttestBF(x = diffScores)
## Equivalently:
## bf = ttestBF(x = sleep$extra[1:10],y=sleep$extra[11:20], paired=TRUE)
bf


## @knitr recip
1 / bf


## @knitr tsamp
chains = posterior(bf, iterations = 1000)
summary(chains)


## @knitr tsamplplot,fig.width=10
chains2 = recompute(chains, iterations = 10000)
plot(chains2[,1:2])


## @knitr onesamptinterval
bfInterval = ttestBF(x = diffScores, nullInterval=c(-Inf,0))
bfInterval


## @knitr onesampledivide
bfInterval[1] / bfInterval[2]


## @knitr onesampcat
allbf = c(bf, bfInterval)
allbf


## @knitr plotonesamp,fig.width=10,fig.height=5
plot(allbf)


## @knitr onesamplist
bfmat = allbf / allbf
bfmat


## @knitr onesamplist2
bfmat[,2]
bfmat[1,]


## @knitr onesamplist3
bfmat[,1:2]
t(bfmat[,1:2])


## @knitr twosampledata
data(chickwts)

## Restrict to two groups
chickwts = chickwts[chickwts$feed %in% c("horsebean","linseed"),]
## Drop unused factor levels
chickwts$feed = factor(chickwts$feed)

## Plot data
plot(weight ~  feed, data = chickwts, main = "Chick weights")


## @knitr 
## traditional t test
t.test(weight ~ feed, data = chickwts, var.eq=TRUE)


## @knitr twosamplet
## Compute Bayes factor
bf = ttestBF(formula = weight ~ feed, data = chickwts)
bf


## @knitr twosampletsamp,fig.width=10
chains = posterior(bf, iterations = 10000)
plot(chains[,1:4])


## @knitr fixeddata,fig.width=10,fig.height=5
data(ToothGrowth)

## Example plot from ?ToothGrowth

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

## Treat dose as a factor
ToothGrowth$dose = factor(ToothGrowth$dose)
levels(ToothGrowth$dose) = c("Low", "Medium", "High")

summary(aov(len ~ supp*dose, data=ToothGrowth))


## @knitr 
bf = anovaBF(len ~ supp*dose, data=ToothGrowth)
bf


## @knitr fixedbf,fig.width=10,fig.height=5
plot(bf[3:4] / bf[2])


## @knitr 
bf = anovaBF(len ~ supp*dose, data=ToothGrowth, whichModels="top")
bf


## @knitr 
bfMainEffects = lmBF(len ~ supp + dose, data = ToothGrowth)
bfInteraction = lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth)
## Compare the two models
bf = bfInteraction / bfMainEffects
bf


## @knitr 
newbf = recompute(bf, iterations = 500000)
newbf


## @knitr 
## Sample from the posterior of the full model
chains = posterior(bfInteraction, iterations = 10000)
## 1:13 are the only "interesting" parameters
summary(chains[,1:13])


## @knitr 
plot(chains[,4:6])


## @knitr 
data(puzzles)


## @knitr puzzlesplot,fig.width=7,fig.height=5,echo=FALSE
## plot the data
aovObj = aov(RT ~ shape*color + Error(ID/(shape*color)), data=puzzles)

matplot(t(matrix(puzzles$RT,12,4)),ty='b',pch=19,lwd=1,lty=1,col=rgb(0,0,0,.2), ylab="Completion time", xlab="Condition",xaxt='n')
axis(1,at=1:4,lab=c("round&mono","square&mono","round&color","square&color"))
mns = tapply(puzzles$RT,list(puzzles$color,puzzles$shape),mean)[c(2,4,1,3)]
points(1:4,mns,pch=22,col="red",bg=rgb(1,0,0,.6),cex=2)
# within-subject standard error, uses MSE from ANOVA
stderr = sqrt(sum(aovObj[[5]]$residuals^2)/11)/sqrt(12)
segments(1:4,mns + stderr,1:4,mns - stderr,col="red")


## @knitr 
summary(aov(RT ~ shape*color + Error(ID/(shape*color)), data=puzzles))


## @knitr tidy=FALSE
bf = anovaBF(RT ~ shape*color + ID, data = puzzles, 
             whichRandom="ID")


## @knitr 
bf


## @knitr testplot,fig.width=10,fig.height=5
plot(bf)


## @knitr 
bfWithoutID = lmBF(RT ~ shape*color, data = puzzles)
bfWithoutID


## @knitr 
bfOnlyID = lmBF(RT ~ ID, whichRandom="ID",data = puzzles)
bf2 = bfWithoutID / bfOnlyID
bf2


## @knitr 
bfall = c(bf,bf2)


## @knitr 
bf[4] / bf2


## @knitr regressData
data(attitude)

## Traditional multiple regression analysis
lmObj = lm(rating ~ ., data = attitude)
summary(lmObj)


## @knitr regressAll
bf = regressionBF(rating ~ ., data = attitude)
length(bf)


## @knitr regressSelect
## Choose a specific model
bf["privileges + learning + raises + critical + advance"]
## Best 6 models
head(bf, n=6)
## Worst 4 models
tail(bf, n=4)


## @knitr regressSelectwhichmax,eval=FALSE
## ## which model index is the best?
## which.max(bf)


## @knitr regressSelectwhichmaxFake,echo=FALSE
## which model index is the best?
BayesFactor::which.max(bf)


## @knitr regressSelect2

## Compare the 5 best models to the best
bf2 = head(bf) / max(bf)
bf2
plot(bf2)


## @knitr regresstop, fig.width=10, fig.height=5
bf = regressionBF(rating ~ ., data = attitude, whichModels = "top")
## The seventh model is the most complex
bf
plot(bf)


## @knitr regressbottom, fig.width=10, fig.height=5
bf = regressionBF(rating ~ ., data = attitude, whichModels = "bottom")
plot(bf)


## @knitr lmregress1
complaintsOnlyBf = lmBF(rating ~ complaints, data = attitude) 
complaintsLearningBf = lmBF(rating ~ complaints + learning, data = attitude) 
## Compare the two models
complaintsOnlyBf / complaintsLearningBf


## @knitr lmposterior
chains = posterior(complaintsLearningBf, iterations = 10000)
summary(chains)


## @knitr lmregressclassical
summary(lm(rating ~ complaints + learning, data = attitude))


## @knitr echo=FALSE,results='hide'
rm(ToothGrowth)


## @knitr GLMdata
data(ToothGrowth)

# model log2 of dose instead of dose directly
ToothGrowth$dose = log2(ToothGrowth$dose)

# Classical analysis for comparison
lmToothGrowth <- lm(len ~ supp + dose + supp:dose, data=ToothGrowth)
summary(lmToothGrowth)


## @knitr GLMs
full <- lmBF(len ~ supp + dose + supp:dose, data=ToothGrowth)
noInteraction <- lmBF(len ~ supp + dose, data=ToothGrowth)
onlyDose <- lmBF(len ~ dose, data=ToothGrowth)
onlySupp <- lmBF(len ~ supp, data=ToothGrowth)

allBFs <- c(full, noInteraction, onlyDose, onlySupp)
allBFs


## @knitr GLMs2
full / noInteraction


## @knitr GLMposterior1
chainsFull <- posterior(full, iterations = 10000)

# summary of the "interesting" parameters
summary(chainsFull[,1:7])


## @knitr GLMposterior2,results='hide',echo=FALSE
chainsNoInt <- posterior(noInteraction, iterations = 10000)


## @knitr GLMplot,echo=FALSE,fig.width=10, fig.height=5
ToothGrowth$dose <- ToothGrowth$dose - mean(ToothGrowth$dose)

cmeans <- colMeans(chainsFull)[1:6]
ints <- cmeans[1] + c(-1, 1) * cmeans[2]
slps <- cmeans[4] + c(-1, 1) * cmeans[5]


par(cex=1.8, mfrow=c(1,2))
plot(len ~ dose, data=ToothGrowth, pch=as.integer(ToothGrowth$supp)+20, bg = rgb(as.integer(ToothGrowth$supp)-1,2-as.integer(ToothGrowth$supp),0,.5),col=NULL,xaxt="n",ylab="Tooth length",xlab="Vitamin C dose (mg)")
abline(a=ints[1],b=slps[1],col=2)
abline(a=ints[2],b=slps[2],col=3)

axis(1,at=-1:1,lab=2^(-1:1))

dataVC <- ToothGrowth[ToothGrowth$supp=="VC",]
dataOJ <- ToothGrowth[ToothGrowth$supp=="OJ",]
lmVC <- lm(len ~ dose, data=dataVC)
lmOJ <- lm(len ~ dose, data=dataOJ)
abline(lmVC,col=2,lty=2)
abline(lmOJ,col=3,lty=2)

mtext("Interaction",3,.1,adj=1,cex=1.3)


# Do single slope

cmeans <- colMeans(chainsNoInt)[1:4]
ints <- cmeans[1] + c(-1, 1) * cmeans[2]
slps <- cmeans[4] 


plot(len ~ dose, data=ToothGrowth, pch=as.integer(ToothGrowth$supp)+20, bg = rgb(as.integer(ToothGrowth$supp)-1,2-as.integer(ToothGrowth$supp),0,.5),col=NULL,xaxt="n",ylab="Tooth length",xlab="Vitamin C dose (mg)")
abline(a=ints[1],b=slps,col=2)
abline(a=ints[2],b=slps,col=3)

axis(1,at=-1:1,lab=2^(-1:1))

mtext("No interaction",3,.1,adj=1,cex=1.3)


## @knitr eval=FALSE
## chainsNoInt <- posterior(noInteraction, iterations = 10000)
## 
## # summary of the "interesting" parameters
## summary(chainsNoInt[,1:5])


## @knitr echo=FALSE
summary(chainsNoInt[,1:5])


## @knitr 
ToothGrowth$doseAsFactor <- factor(ToothGrowth$dose)
levels(ToothGrowth$doseAsFactor) <- c(.5,1,2)

aovBFs <- anovaBF(len ~ doseAsFactor + supp + doseAsFactor:supp, data = ToothGrowth)


## @knitr 
allBFs <- c(aovBFs, full, noInteraction, onlyDose)

## eliminate the supp-only model, since it performs so badly
allBFs <- allBFs[-1]

## Compare to best model
allBFs / max(allBFs)


## @knitr GLMplot2,echo=FALSE,fig.width=10, fig.height=5
plot(allBFs / max(allBFs))


## @knitr 
data(puzzles)

puzzleGenBF <- generalTestBF(RT ~ shape + color + shape:color + ID, data=puzzles, whichRandom="ID")

puzzleGenBF


## @knitr 
puzzleGenBF <- generalTestBF(RT ~ shape + color + shape:color + ID, data=puzzles, whichRandom="ID", neverExclude="ID")

puzzleGenBF


## @knitr 
puzzleGenBF <- generalTestBF(RT ~ shape + color + shape:color + shape:ID + ID, data=puzzles, whichRandom="ID", neverExclude="ID")

puzzleGenBF


## @knitr 
puzzleGenBF <- generalTestBF(RT ~ shape + color + shape:color + shape:ID + ID, data=puzzles, whichRandom="ID", neverExclude="^ID$")

puzzleGenBF


## @knitr 
puzzleCullBF <- generalTestBF(RT ~ shape + color + shape:color + ID, data=puzzles, whichRandom="ID", noSample=TRUE,whichModels='all')

puzzleCullBF


## @knitr 
missing = puzzleCullBF[ is.na(puzzleCullBF) ]
done = puzzleCullBF[ !is.na(puzzleCullBF) ]

missing


## @knitr 
# get the names of the numerator models
missingModels = names(missing)$numerator

# search them to see if they contain "shape" or "color" - 
# results are logical vectors
containsShape = grepl("shape",missingModels)
containsColor = grepl("color",missingModels)

# anything that does not contain "shape" and "color"
containsOnlyOne = !(containsShape & containsColor)

# restrict missing to only those of interest
missingOfInterest = missing[containsOnlyOne]

missingOfInterest


## @knitr 
# recompute the Bayes factors for the missing models of interest
sampledBayesFactors = recompute(missingOfInterest)

sampledBayesFactors

# Add them together with our other Bayes factors, already computed:
completeBayesFactors = c(done, sampledBayesFactors)

completeBayesFactors


## @knitr 
data(puzzles)

# Get MCMC chains corresponding to "full" model
# We prevent sampling so we can see the parameter names
# iterations argument is necessary, but not used
fullModel = lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, noSample=TRUE, posterior = TRUE, iterations=3)

fullModel


## @knitr 
fullModelFiltered = lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, noSample=TRUE, posterior = TRUE, iterations=3,columnFilter="ID")

fullModelFiltered


## @knitr 
# Sample 10000 iterations, eliminating ID columns
chains = lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, posterior = TRUE, iterations=10000,columnFilter="ID")


## @knitr acfplot,fig.width=10,fig.height=5,echo=FALSE
par(mfrow=c(1,2))
plot(as.vector(chains[1:1000,"shape-round"]),type="l",xlab="Iterations",ylab="parameter shape-round")
acf(chains[,"shape-round"])


## @knitr 
chainsThinned = recompute(chains, iterations=20000, thin=2)

# check size of MCMC chain
dim(chainsThinned)


## @knitr acfplot2,fig.width=10,fig.height=5,echo=FALSE
par(mfrow=c(1,2))
plot(as.vector(chainsThinned[1:1000,"shape-round"]),type="l",xlab="Iterations",ylab="parameter shape-round")
acf(chainsThinned[,"shape-round"])


