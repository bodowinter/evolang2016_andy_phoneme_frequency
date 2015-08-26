## Bodo Winter
## August 9, 2015
## Within-language across-language frequency correlation

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in packages:

library(xlsx)
library(lme4)
library(ggplot2)
library(MuMIn)

## Load in data:

setwd("/Users/teeniematlock/Desktop/research/andy_phoneme_frequency/evolang2016/")
phoib <- read.csv("phoible_with_autotyp.csv", stringsAsFactors = F)
andy <- read.xlsx("within_language_frequencies.xlsx", 1)
graff <- read.csv('graff_2012_summary.csv', stringsAsFactors = F)

## Fix Graff segments so that they match Phoible:

graff[graff$segment == 'S', ]$segment <- 'ʃ'
graff[graff$segment == 'Z', ]$segment <- 'ʒ'
graff[graff$segment == 'TH', ]$segment <- 'θ'
graff[graff$segment == 'DH', ]$segment <- 'ð'
graff[graff$segment == 'g', ]$segment <- 'ɡ'

## Create columns to store the Phoible frequencies in:

andy$Phoib_freq <- numeric(nrow(andy))
andy$Phoib_stock_freq <- numeric(nrow(andy))
andy$Phoib_freq_diacritic <- numeric(nrow(andy))		# with diacritics
andy$Phoib_stock_freq_diacritic <- numeric(nrow(andy))		# with diacritics

## Loop through rows (segments):

for(i in 1:nrow(andy)){
	this_segment <- andy$segment[i]
	
	subset <- phoib[phoib$Phoneme == this_segment,]
	andy[i,]$Phoib_freq <- length(unique(subset$LanguageCode))
	andy[i,]$Phoib_stock_freq <- length(unique(subset$AUTOTYP.stock))
	
	subset <- phoib[grep(this_segment,phoib$Phoneme),]
	
	andy[i,]$Phoib_freq_diacritic <- length(unique(subset$LanguageCode))
	andy[i,]$Phoib_stock_freq_diacritic <- length(unique(subset$AUTOTYP.stock))
	
	if(i %% 10 == 0){cat(paste(i,"\n"))}
	}

## August 9, 2015: For now, we set 0's to NA (need to find the actual values later)

andy[andy$Phoib_freq == 0,colnames(andy)[grep("_freq",colnames(andy))]] <- NA



##------------------------------------------------------------------
## Within & across language correlations, analysis:
##------------------------------------------------------------------

## Log-transform predictor frequency:

andy$logfreq <- log(andy$freq)

## Model for without diacritics, data from all languages:

summary(xmdl.crosslang <- glmer(Phoib_freq ~ logfreq +
	(1+logfreq|language),andy,family="poisson"))

## Model for without diacritics, data across stocks:

summary(xmdl.crosslang.stock <- glmer(Phoib_stock_freq ~ logfreq +
	(1+logfreq|language),andy,family="poisson"))

## Model for without diacritics, data from all languages:

summary(xmdl.crosslang.diac <- glmer(Phoib_freq_diacritic ~ logfreq +
	(1+logfreq|language), andy, family="poisson"))

## Model for without diacritics, data across stocks:

summary(xmdl.crosslang.stock.diac <- glmer(Phoib_stock_freq_diacritic ~ logfreq +
	(1+logfreq|language), andy, family="poisson"))



##------------------------------------------------------------------
## Within & across-language correlations, plot:
##------------------------------------------------------------------

## Make a function for getting predictions:

# get_poisson_preds <- function(fit,xvals){
	# newdata <- data.frame(xvals)
	# names(newdata) <- fixef(fit)[2]
	# predict(fit,newdata,se.fit=T,type="response")
	# }

## Make a pretty plot of this, first get z-score all frequency values to have conformable axes:

andy$logfreq_z_bylang <- numeric(nrow(andy))
andy$logfreq_z_phoib <- numeric(nrow(andy))

for(i in seq_along(unique(andy$language))){
	this_language <- unique(andy$language)[i]
	these_z_vals <- scale(andy[andy$language == this_language,]$logfreq)
	andy[andy$language == this_language,]$logfreq_z_bylang <- these_z_vals
	
	these_z_vals_phoib <- scale(log10(andy[andy$language == this_language,]$Phoib_freq + 1))
	andy[andy$language == this_language, ]$logfreq_z_phoib <- these_z_vals_phoib
	}

## Plot this:

quartz('', 11, 5.5)
par(mfrow = c(2, 4))
for (i in 1:8) {
	this_language <- unique(andy$language)[i]
	this_subset <- andy[andy$language == this_language,]
	
	xmdl <- lm(logfreq_z_phoib ~ logfreq_z_bylang, this_subset)
	
	plot(1, 1, type = 'n', xlim = c(-3, 3), ylim = c(-4, 2), xlab = '', ylab = '')
	points(this_subset$logfreq_z_bylang, this_subset$logfreq_z_phoib, pch = 19)
	abline(xmdl, lwd = 2)
	mtext(side = 3, text = as.character(this_language), cex = 1.25, line = 1)
	}




##------------------------------------------------------------------
## Graff 2012 confusions & across-language phoneme frequencies:
##------------------------------------------------------------------

## Create columns to store the Phoible frequencies in:

graff$Phoib_freq <- numeric(nrow(graff))
graff$Phoib_stock_freq <- numeric(nrow(graff))
graff$Phoib_freq_diacritic <- numeric(nrow(graff))		# with diacritics
graff$Phoib_stock_freq_diacritic <- numeric(nrow(graff))		# with diacritics

## Add Phoible data to graff dataset:

for(i in 1:nrow(graff)){
	this_segment <- graff$segment[i]
	
	subset <- phoib[phoib$Phoneme == this_segment, ]
	graff[i, ]$Phoib_freq <- length(unique(subset$LanguageCode))
	graff[i, ]$Phoib_stock_freq <- length(unique(subset$AUTOTYP.stock))
	
	subset <- phoib[grep(this_segment,phoib$Phoneme),]
	
	graff[i,]$Phoib_freq_diacritic <- length(unique(subset$LanguageCode))
	graff[i,]$Phoib_stock_freq_diacritic <- length(unique(subset$AUTOTYP.stock))
	}

## Plot frequency against confusability (i.e., distinctiveness):

quartz("",8,6);plot(graff$Graff2012ConfusabilityAVG,log(graff$Phoib_freq),pch=19)
abline(lm(log(graff$Phoib_freq) ~ graff$Graff2012ConfusabilityAVG),lwd=2)
summary(xmdl <- lm(log(graff$Phoib_freq) ~ graff$Graff2012ConfusabilityAVG))

## Look at residuals:

plot(fitted(xmdl),residuals(xmdl))

## Make a poisson model:

summary(xmdl.pois <- glm(Phoib_freq ~ Graff2012ConfusabilityAVG,graff,family="poisson"))

## Plot a poisson model:

quartz("",8,6);plot(graff$Graff2012ConfusabilityAVG,graff$Phoib_freq,pch=19)
xvals <- seq(from=min(graff$Graff2012ConfusabilityAVG),
	to=max(graff$Graff2012ConfusabilityAVG),0.01)
yvals <- exp(coef(xmdl.pois)[1] + xvals*coef(xmdl.pois)[2])
points(xvals,yvals,lwd=4,type="l")

###### SAME FOR STOCKS:

## Plot stock frequency against confusability (i.e., distinctiveness):

quartz("",8,6);plot(graff$Graff2012ConfusabilityAVG,log(graff$Phoib_stock_freq),pch=19)
abline(lm(log(graff$Phoib_stock_freq) ~ graff$Graff2012ConfusabilityAVG),lwd=2)
summary(xmdl <- lm(log(graff$Phoib_stock_freq) ~ graff$Graff2012ConfusabilityAVG))

## Look at residuals:

plot(fitted(xmdl),residuals(xmdl))

## Make a poisson model:

summary(xmdl.pois <- glm(Phoib_stock_freq ~ Graff2012ConfusabilityAVG,graff,family="poisson"))

## Plot a poisson model:

quartz("",8,6);plot(graff$Graff2012ConfusabilityAVG,graff$Phoib_stock_freq,pch=19)
xvals <- seq(from=min(graff$Graff2012ConfusabilityAVG),
	to=max(graff$Graff2012ConfusabilityAVG),0.01)
yvals <- exp(coef(xmdl.pois)[1] + xvals*coef(xmdl.pois)[2])
points(xvals,yvals,lwd=4,type="l")



##------------------------------------------------------------------
## Graff 2012 confusions & within-language phoneme frequencies:
##------------------------------------------------------------------

## Make andy to a subset that only has the phonemes that are also in Graff:

andy_graff <- andy[andy$segment %in% graff$segment,]

## Add confusability:

andy_graff$distinctiveness <- graff[match(andy_graff$segment, graff$segment), ]$Graff2012ConfusabilityAVG

## Model this:

summary(xmdl.pois <- glmer(freq ~ distinctiveness +
	(1+distinctiveness|language), data = andy_graff, family = 'poisson'))

## Make a pretty plot of this, first get z-score all frequency values to have conformable axes:

andy_graff$logfreq <- log(andy_graff$freq)
andy_graff$logfreq_z <- numeric(nrow(andy_graff))

for(i in seq_along(unique(andy_graff$language))){
	this_language <- unique(andy_graff$language)[i]
	these_z_vals <- scale(andy_graff[andy_graff$language == this_language,]$logfreq)
	andy_graff[andy_graff$language == this_language,]$logfreq_z <- these_z_vals
	}

## Plot this:

quartz('', 11, 5.5)
par(mfrow = c(2, 4))
for (i in 1:8) {
	this_language <- unique(andy$language)[i]
	this_subset <- andy_graff[andy_graff$language == this_language,]
	
	xmdl <- lm(logfreq_z ~ distinctiveness, this_subset)
	
	plot(1, 1, type = 'n', xlim = c(2.8, 3.8), ylim = c(-4, 2), xlab = '', ylab = '')
	points(this_subset$distinctiveness, this_subset$logfreq_z, pch = 19)
	abline(xmdl, lwd = 2)
	mtext(side = 3, text = as.character(this_language), cex = 1.25, line = 1)
	}



