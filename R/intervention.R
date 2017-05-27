#Uses http://cran.r-project.org/web/packages/meta/
#Alternatives:f
# http://cran.r-project.org/web/packages/metafor/ (allows continuity correction)
# http://cran.r-project.org/web/packages/rmeta/
# Discussion of continuity correction:
# http://handbook.cochrane.org/chapter_16/16_9_2_studies_with_zero_cell_counts.htm
intervention <- function(content, measure, hartung, year, pmid, sortby, lefthand, righthand, type, independent_variable, cofactorlabel, topic, label_location, theme) {

if (length(sortby) == 0 || sortby == "sortby"){sortby = "none"}
first.row <- substr(content, 1, regexpr("\n",content))
#year<-substr(first.row, regexpr(",",first.row)+1,nchar(first.row))
#year<-substr(year, 1,regexpr(",",year)-1)
#first.row.header <- FALSE
#if (mytable[1,2]){first.row.header <- TRUE}
num.columns <- str_count(first.row, ",")
num.cofactors <- num.columns - 7

temp <- content
# Uses package meta http://cran.r-project.org/web/packages/meta/
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
#temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\r", ' ', fixed = TRUE, temp)
temp <- gsub("\n", ' ', fixed = TRUE, temp)
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)

temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=',num.columns,', byrow=TRUE)')
x<-eval(parse(file = "", n = NULL, text = temp))
# Delete first row if contains column labels (detected by as.numeric(year) = false)
first.row.header <- FALSE
if (is.na(as.numeric(x[1,2])) == TRUE){first.row.header <- TRUE}
if (first.row.header == TRUE){x <- x[-c(1),]}
# Delete terminal rows if contains instructions (detected by as.numeric(year) = false)
x <- x[!(is.na(as.numeric(x[,2])) == TRUE),]

column.names <- c("Study","year", "pmid", "exp_events", "exp_total","control_events","control_total")
for(i in 1: num.cofactors)
	{
	column.names<- append(column.names,paste('cofactor',i,sep=""))
	}
dimnames(x) <- list(NULL, column.names)
myframe <- data.frame (x)
remove(x)
#stop(independent_variable)
if (type == 'subgroup1' || independent_variable == 'cf1'){myframe$cofactor <- myframe$cofactor1}
if (type == 'subgroup2' || independent_variable == 'cf2'){myframe$cofactor <- myframe$cofactor2}
if (type == 'subgroup3' || independent_variable == 'cf3'){myframe$cofactor <- myframe$cofactor3}
if (type == 'subgroup4' || independent_variable == 'cf4'){myframe$cofactor <- myframe$cofactor4}
if (type == 'subgroup5' || independent_variable == 'cf5'){myframe$cofactor <- myframe$cofactor5}

myframe$Study<-gsub("\'", '', fixed = TRUE, myframe$Study)
myframe$Study<-as.character(str_trim(myframe$Study))
myframe$year<-as.numeric(as.character(str_trim(myframe$year)))
myframe$pmid<-as.numeric(as.character(str_trim(myframe$pmid)))
PosParenth1 <- regexpr("(", myframe$exp_events, fixed=TRUE)
#stop(paste("stop with: ",myframe$exp_events, sep=""))
if (PosParenth1 > 0)
	{
	PosParenth2 <-regexpr(")", myframe$exp_events, fixed=TRUE)
	myframe$exp_mean<-as.numeric(substring(myframe$exp_events, 1, PosParenth1 - 1))
	myframe$exp_sd<-as.numeric(substring(myframe$exp_events, PosParenth1 + 1, PosParenth2 - 1))
	PosParenth1 <-regexpr("(", myframe$control_events, fixed=TRUE)
	PosParenth2 <-regexpr(")", myframe$control_events, fixed=TRUE)
	myframe$control_mean<-as.numeric(substring(myframe$control_events, 1, PosParenth1 - 1))
	myframe$control_sd<-as.numeric(substring(myframe$control_events, PosParenth1 + 1, PosParenth2 - 1))
	}
else
	{
	myframe$exp_events<-as.numeric(as.character(str_trim(myframe$exp_events)))
	myframe$control_events<-as.numeric(as.character(str_trim(myframe$control_events)))
	}
myframe$exp_total<-as.numeric(as.character(str_trim(myframe$exp_total)))
myframe$control_total<-as.numeric(as.character(str_trim(myframe$control_total)))
if (sortby=="weight")
	{
	sortvalue <- NULL
	}
if (sortby=="cofactor1")
	{
	sortvalue <- myframe$cofactor1
	}
if (sortby=="cofactor2")
	{
	sortvalue <- myframe$cofactor2
	}
if (sortby=="cofactor3")
	{
	sortvalue <- myframe$cofactor3
	}
if (sortby=="cofactor4")
	{
	sortvalue <- myframe$cofactor4
	}
if (sortby=="cofactor5")
	{
	sortvalue <- myframe$cofactor5
	}
if (sortby=="baseline")
	{
	if (PosParenth1 > 0)
		{sortvalue <- myframe$control_mean}
	else
		{sortvalue <- myframe$control_events/myframe$control_total}
	}
if (sortby=="year")
	{
	sortvalue <- myframe$year
	}
if (sortby=="study")
	{
	sortvalue <- myframe$Study
	}
for(i in 1:length(myframe$Study))
{
if(myframe$exp_events[i]==0 & myframe$control_events[i]==0)
	{
	#myframe$exp_events[i] == 0.5 
	#myframe$control_events[i] == 0.5
	}
}
attach(myframe)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
pubbiastext = "Test for funnel plot asymmetry"
analyticmethod = "Random effects model"
if (hartung){analyticmethod = paste(analyticmethod," (Hartung-Knapp)")}
#par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue,new = TRUE) #bg=SkyBlue)
if (type=="ignore")
	{
	# from http://cran.r-project.org/web/packages/meta/
	if (PosParenth1 > 0)
		{
		meta1 <- metacont(exp_total, exp_mean, exp_sd, control_total, control_mean, control_sd, data=myframe, sm = measure, hakn = hartung, studlab=paste(Study,", ", year, sep=""))
		if (measure == "MD"){xlimits="s"}else{xlimits=c(-2, 2)}
		#Publication bias
		if (length(myframe$Study)>9)
			{
			pubbias = metabias(meta1, method.bias="linreg", plotit=FALSE)
			pubbiastext = paste(pubbiastext, " (Egger): p= ",round(pubbias$p.value,3),sep="");
			}
		else
			{
			pubbiastext = paste(pubbiastext,": too few studies to test",sep="")
			}
		}
	else
		{
		meta1 <- metabin(exp_events, exp_total, control_events, control_total, data=myframe, sm = measure, hakn = hartung, method="Inverse", level = 0.95, incr = "TA", allstudies = TRUE, studlab=paste(Study,", ", year, sep=""))
		xlimits=c(0.1, 10)
		#Publication bias / small study effect
		if (length(myframe$Study)>9)
			{
			meta1.as <- metabin(exp_events, exp_total, control_events, control_total, data=myframe, sm="ASD", method="I")
			pubbias = metabias(meta1.as, plotit=FALSE)
			pubbiastext = paste(pubbiastext, " (Rucker): p= ",round(pubbias$p.value,3),sep="");
			}
		else
			{
			pubbiastext = paste(pubbiastext,": too few studies to test",sep="")
			}
		}
	if (sortby=="weight")
		{
		sortvalue <- 1/meta1$w.random
		}
	#stop(paste(topic,lefthand, righthand, sep=", "))
	forest(meta1, sortvalue, xlim=xlimits, col.diamond="blue", col.diamond.lines="blue", title = topic, comb.fixed=FALSE,print.I2.ci=TRUE, print.p=FALSE, print.tau2=FALSE, label.left=lefthand, label.right=righthand,text.random=analyticmethod,text.random.w=analyticmethod, fs.random=12, ff.random = 1, ff.hetstat=2, fs.hetstat=12)
	#grid.text(topic, 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
	grid.text(topic, 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
	#main=textGrob(topic, gp=gpar(cex=3), just="top")
	grid.text(pubbiastext, 0.1, 0.02, hjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
	}
if (grepl("subgroup",type))
	{
	# from http://cran.r-project.org/web/packages/meta/
	myframe$cofactor<-gsub("\'", '', fixed = TRUE, myframe$cofactor)
	myframe$cofactor<-as.character(str_trim(myframe$cofactor))
	if (PosParenth1 > 0)
		{
		meta1 <- metacont(exp_total, exp_mean, exp_sd, control_total, control_mean, control_sd, data=myframe, sm = measure, hakn = hartung, studlab=paste(Study,", ", year, sep=""), label.left=lefthand, label.right=righthand, title = topic, byvar=myframe$cofactor, print.byvar = FALSE)
		if (measure == "MD"){xlimits="s"}else{xlimits=c(-2, 2)}
		#Publication bias
		if (length(myframe$Study)>9)
			{
			meta1.egger <- metacont(exp_total, exp_mean, exp_sd, control_total, control_mean, control_sd, data=myframe, sm = measure)
			pubbias = metabias(meta1.egger, method.bias="linreg", plotit=FALSE)
			pubbiastext = paste(pubbiastext, " (Egger): p= ",round(pubbias$p.value,3),sep="");
			}
		else
			{
			pubbiastext = paste(pubbiastext,": too few studies to test",sep="")
			}
		}
	else
		{
		meta1 <- metabin(exp_events, exp_total, control_events,control_total, data=myframe, sm = measure, method="Inverse", hakn = hartung, level = 0.95, incr = "TA", allstudies = TRUE, studlab=paste(Study,", ", year, sep=""), label.left=lefthand, label.right=righthand, title = topic, byvar=myframe$cofactor, print.byvar = FALSE)
		xlimits=c(0.1, 10)
		#Publication bias / small study effect
		if (length(myframe$Study)>9)
			{
			meta1.as <- metabin(exp_events, exp_total, control_events, control_total, data=myframe, sm="ASD", method="I")
			pubbias = metabias(meta1.as, plotit=FALSE)
			pubbiastext = paste(pubbiastext, " (Rucker): p= ",round(pubbias$p.value,3),sep="");
			}
		else
			{
			pubbiastext = paste(pubbiastext,": too few studies to test",sep="")
			}
		}
	if (sortby=="weight")
		{
		sortvalue <- 1/meta1$w.random
		}
	forest(meta1, sortvalue, col.diamond="blue", col.diamond.lines="blue", title = topic, main = topic, comb.fixed=FALSE, print.I2.ci=TRUE, print.p=FALSE, print.tau2=FALSE, label.left=lefthand, label.right=righthand,text.random=analyticmethod,text.random.w=analyticmethod, fs.random=12, ff.random = 1, ff.hetstat=2, fs.hetstat=12)
	grid.text(topic, 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
	grid.text(pubbiastext, 0.1, 0.04, hjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
	#Test for subgroup differences
	#Hartung-Knapp gives Q = 0 if a subgroup has single member
	meta1 <- update(meta1,hakn = FALSE)
	byvartext = 1 - pchisq(meta1$Q.b.random, df = meta1$df.Q.b);
	byvartext = sprintf(byvartext, fmt='%#.3f');
	byvartext = paste("Test for differences among subgroups: p = ", byvartext ,sep="");
	grid.text(byvartext, 0.1, 0.07, hjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
	}
if (type=="metaregression")
	{
	if (independent_variable=="year"){
		myframe$x <- as.numeric(myframe$year)
		}
		else{
			if (independent_variable=="size"){
				myframe$x <- as.numeric(myframe$exp_total) + as.numeric(myframe$control_total)
				}else{
				if (independent_variable=="size"){
					myframe$x <- as.numeric(myframe$exp_total) + as.numeric(myframe$control_total)
					}else{
						if (independent_variable=="cr"){
							#Nothing now, assign after '(' checked for
						}else{
							myframe$x <- as.numeric(as.character(str_trim(myframe$cofactor)))
							}
						}
					}
				}
    #stop(paste("stop with: ",PosParenth1, sep=""))
	attach(myframe)
	if (PosParenth1 > 0){
		#stop(paste(topic,myframe["Study"], sep=", "))
		if (independent_variable=="cr"){myframe$x <- myframe$control_mean}
		# Removing studies with missing data
		myframe[order(myframe$x, na.last = NA),]
		myframe <- na.omit(myframe)
		# Meta-analysis
		dat <- escalc(measure="MD", m1i=exp_mean, sd1i=exp_sd, n1i=exp_total, m2i=control_mean, sd2i=control_sd, n2i=control_total, data=myframe)
		res <- rma.uni(yi, vi, mods = ~ myframe$x,  method="DL", knha=TRUE, data=dat, intercept = TRUE)
		ylabel = "Mean difference"
		}
	else{
		if (independent_variable=="cr"){myframe$x <- myframe$control_events/myframe$control_total}
		# Removing studies with missing data
		myframe[order(myframe$x, na.last = NA),]
		myframe <- na.omit(myframe)
		# Meta-analysis
		dat <- escalc(measure="OR", ai=exp_events, bi=exp_total-exp_events, ci=control_events, di=control_total-control_events, data=myframe)
		res <- rma(yi, vi, mods = ~ myframe$x, data=dat)
		ylabel = "Odds ratio transformed to natural log (ln)"
		}
	# Make confidence limits
	cofactor.range = seq(min(myframe$x), max(myframe$x), (max(myframe$x) - min(myframe$x))/100)
	preds <- predict(res, newmods=cofactor.range)
	preds <- data.frame(cofactor.range,preds$pred,preds$ci.lb,preds$ci.ub)
	# Calculate point sizes by rescaling the standard errors
	wi    <- 1/sqrt(dat$vi)
	size  <- 0.5 + 3.0 * (wi - min(wi))/(max(wi) - min(wi))
	plot(dat$x, dat$yi, pch=19, cex=size, cex.lab = 1.5,font.axis=2,
		xlab="", ylab=ylabel, main=paste("Meta-regression of ", topic),
		xlim=c(min(dat$x)-0.1*(max(dat$x)-min(dat$x)),max(dat$x)+0.1*(max(dat$x)-min(dat$x))),
		ylim=c(min(dat$yi),max(dat$yi)+0.15*(max(dat$yi)-min(dat$yi))),
		las=1, bty="l")
	if ( cofactorlabel != "")
		{
		mtext(side=1,line=2.25,paste("Cofactor: ",cofactorlabel), font=2, cex=1.5)
		}
	lines(preds$cofactor.range, preds$preds.pred)
	lines(preds$cofactor.range, preds$preds.ci.lb, lty="dashed", col="blue")
	lines(preds$cofactor.range, preds$preds.ci.ub, lty="dashed", col="blue")
	#text(par("usr")[2],par("usr")[4]-1.25*strheight("A"),cex=1.2,adj=c(1,0),paste("p (correlation) = ",sprintf(res$pval[2], fmt='%#.3f'), sep=""), font=1)
	#text(par("usr")[2],par("usr")[4]-2.25*strheight("A")-0.5*strheight("A"),cex=1.2,adj=c(1,0),paste("Residual I2 = ",sprintf(res$I2, fmt='%#.1f'),'%', sep=""), font=1)
	text(par("usr")[2],par("usr")[4]-1.2*strheight("A")                     ,cex=1,adj=c(1,0),paste("R2 = ",round(res$R2),"% (QM = ",sprintf(res$QM, fmt='%#.1f'),", p = ",sprintf(res$pval[2], fmt='%#.3f'), ")", sep=""), font=1)
	text(par("usr")[2],par("usr")[4]-1.2*strheight("A")-1.4*strheight("A")  ,cex=1,adj=c(1,0),paste("Regression coefficient = ",sprintf(res$b[2], fmt='%#.1f'), sep=""), font=1)
	text(par("usr")[2],par("usr")[4]-1.2*strheight("A")-2.8*strheight("A")  ,cex=1,adj=c(1,0),paste("Residual I2 = ",sprintf(res$I2, fmt='%#.1f'),'%', sep=""), font=1)
	abline(h=0, lty="dotted")
	if (label_location > 0)
		{
		text(dat$x, dat$yi, paste(dat$Study,", ",dat$year, sep=""), cex=.9, pos = label_location, offset = 1, col=dat$color)
		}
	legend("topleft", adj = 0, xjust = 1, inset = c(0,0), c("Regression line","95% Confidence\ninterval"), pch = NULL, pt.bg = "white", bty = "n", border = "white", lty=c("solid","dashed"), col=c("black","blue"))
	}
#if(theme=="KU"){display_logo(x=1.2,y=0.05)}
}
