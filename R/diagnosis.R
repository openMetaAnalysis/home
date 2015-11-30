#Uses https://cran.r-project.org/web/packages/Metatron/
#Uses https://cran.r-project.org/web/packages/mada/
# Discussion of continuity correction:
# http://handbook.cochrane.org/chapter_16/16_9_2_studies_with_zero_cell_counts.htm
diagnosis <- function(content, measure, year, pmid, sortby, lefthand, righthand, type, cofactorlabel, topic, theme) {
temp <- content
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)
temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=8, byrow=TRUE, dimnames = list(NULL, c("Study","year", "pmid", "TP", "FP","FN","TN","cofactor")))')
x<-eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
myframe$Study<-gsub("\'", '', fixed = TRUE, myframe$Study)
myframe$Study<-as.character(str_trim(myframe$Study))
myframe$year<-as.numeric(as.character(str_trim(myframe$year)))
myframe$pmid<-as.numeric(as.character(str_trim(myframe$pmid)))
myframe$TP<-as.numeric(as.character(str_trim(myframe$TP)))
myframe$FP<-as.numeric(as.character(str_trim(myframe$FP)))
myframe$FN<-as.numeric(as.character(str_trim(myframe$FN)))
myframe$TN<-as.numeric(as.character(str_trim(myframe$TN)))
msg = ""
if (sortby=="weight")
	{
	sortvalue <- NULL
	}
if (sortby=="cofactor")
	{
	sortvalue <- myframe$cofactor
	}
if (sortby=="year")
	{
	sortvalue <- myframe$year
	}
if (sortby=="study")
	{
	sortvalue <- myframe$Study
	}
	
attach(myframe)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
pubbiastext = "Test for funnel plot asymmetry"
analyticmethod = "Hierarchical model (bivariate)"
msg = ""

meta1 <- madad(TP=TP,FN=FN,TN=TN,FP=FP,names=Study,data=myframe, correction = 0.5, correction.control = "all")
prevalence=array(0,length(myframe$names))
studysize=array(0,length(myframe$names))
withoutcome=array(0,length(myframe$names))

#Start of SVG
height = 265 + length(myframe$Study) * 20
svgtext = paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?><svg x=\"0\" y=\"0\" width=\"700px\" height=\"", height, "px\" viewBox=\"0 0 700 ", height, "\" style=\"font-family:Arial, Helvetica, sans-serif\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">",sep="")
#Column names
svgtext = paste(svgtext, "<!-- Header of plot--><text x=\"10\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Study</text><text x=\"275\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Sensitivity (%)</text><text x=\"500\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Specificity (%)</text><!-- Start of studies-->",sep="")
for(i in 1: length(myframe$Study))
	{
	#Citation
	studysize[i] = TP[i] + FP[i] + FN[i] + TN[i]
	withoutcome[i] = TP[i] + FN[i]
	prevalence[i] = round(100*(TP[i] + FN[i])/studysize[i],0)
	svgtext = paste(svgtext, "<a xlink:href=\"http://pubmed.gov/",myframe$pmid[i],"\" xlink:title=\"Study size is ",studysize[i],". Prevalence of the outcome is ",prevalence[i],"%. Click citation to open abstract at PubMed in a new window.\" target=\"_blank\"><text x=\"10\" y=\"" , 15 + i*20 ,"\" fill=\"rgba(0,0,255,1)\">",myframe$Study[i],", ", myframe$year[i],"</text></a>",sep="")
	#Sensitivity
	x = 325 + 100 * meta1$sens[[1]][i]
		#text
		svgtext = paste(svgtext,"<text x=\"225\" y=\"" , 15 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$sens[[1]][i]*100,0)," (", round(meta1$sens$sens.ci[i,1]*100,0) ," - ", round( meta1$sens$sens.ci[i,2]*100,0), ")</text>",sep="")
		#point estimate
		svgtext = paste(svgtext,"<circle cx=\"", x, "\" cy=\"" , 10 + i*20 ,"\" r=\"3\" stroke-width=\"0\" style=\"fill:black;\"/>",sep="")
		#Confidence intervals
		cl.lower = 325 + 100 * meta1$sens$sens.ci[i,1]
		ci.upper = 325 + 100 * meta1$sens$sens.ci[i,2]
		svgtext = paste(svgtext,"<line x1=\"" , cl.lower ,"\" y1=\"" , 10 + i*20 ,"\" x2=\"" , ci.upper ,"\" y2=\"" , 10 + i*20 ,"\" style=\"stroke:rgba(0,0,0,1);stroke-width:2\" />", sep="")
	#Specificity
	x = 550 + 100 * meta1$spec[[1]][i]
		#text
		svgtext = paste(svgtext,"<text x=\"450\" y=\"" , 15 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$spec[[1]][i]*100,0)," (", round(meta1$spec$spec.ci[i,1]*100,0) ," - ", round( meta1$spec$spec.ci[i,2]*100,0), ")</text>",sep="")
		#point estimate
		svgtext = paste(svgtext,"<circle cx=\"", x, "\" cy=\"" , 10 + i*20 ,"\" r=\"3\" stroke-width=\"0\" style=\"fill:black;\"/>",sep="")
		#Confidence intervals
		cl.lower = 550 + 100 * meta1$spec$spec.ci[i,1]
		ci.upper = 550 + 100 * meta1$spec$spec.ci[i,2]
		svgtext = paste(svgtext,"<line x1=\"" , cl.lower ,"\" y1=\"" , 10 + i*20 ,"\" x2=\"" , ci.upper ,"\" y2=\"" , 10 + i*20 ,"\" style=\"stroke:rgba(0,0,0,1);stroke-width:2\" />", sep="")
	}
	#Bottom of plots
	svgtext = paste(svgtext, "<!-- Bottom of plot--><!-- Axes --><g style=\"stroke:rgba(0,0,0,0.2);stroke-width:2\"><line x1=\"325\" y1=\"",25 + i*20,"\" x2=\"425\" y2=\"", 25 + i*20 ,"\" />", sep="")
	svgtext = paste(svgtext, "<line x1=\"550\" y1=\"",25 + i*20,"\" x2=\"650\" y2=\"", 25 + i*20 ,"\" /></g>", sep="")
	svgtext = paste(svgtext, "<!-- Ticks --><g stroke=\"rgba(0,0,0,0.2)\" stroke-width=\"1\" fill=\"rgba(0,0,0,0.2)\"><g transform=\"translate(400, 245) rotate(180)\"><path d=\"M 0,0 L 0,5 M 50,0 L 50,5 M 100,0 L 100,5\" /></g><g transform=\"translate(650, 245) rotate(180)\"><path d=\"M 0,0 L 0,5 M 50,0 L 50,5 M 100,0 L 100,5\" /></g></g>", sep="")
if (type=="ignore")
	{
	#The meta-analysis
	meta2 <- perfect.trees(TP=TP,FN=FN,TN=TN,FP=FP,study=Study,data=myframe)
	#vertical lines for sn and sp
		#sensitivity
		svgtext = paste(svgtext, "<!-- Vertical summary lines --><g style=\"stroke:rgba(0,0,0,0.2);stroke-width:2\" ><line x1=\"", 325 + 100 * meta2$coef[[2]][1], "\" y1=\"25\" x2=\"", 325 + 100 * meta2$coef[[2]][1], "\" y2=\"", 25 + i*20 ,"\" />", sep="")
		#specificity
		svgtext = paste(svgtext, "<line x1=\"", 550 + 100 * meta2$coef[[3]][1], "\" y1=\"25\" x2=\"", 550 + 100 * meta2$coef[[3]][1], "\" y2=\"", 25 + i*20 ,"\"/></g>", sep="")
	#Summary text
		svgtext = paste(svgtext, "<!-- Summary text --><g fill=\"black\" style=\"color:black;opacity:1\"><text x=\"10\" y=\"" , 40 + i*20 ,"\" style=\"font-weight:bold\">Summary</text>",sep="")
		#Sens
		ci.l = round(100*inv.logit(meta2$coef[2,2] - 1.96*meta2$coef[2,3]),0)
		ci.u = round(100*inv.logit(meta2$coef[2,2] + 1.96*meta2$coef[2,3]),0)
		svgtext = paste(svgtext,"<text x=\"225\" y=\"" , 40 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">",round(meta2$coef[[2]][1]*100,0)," (", ci.l ," - ", ci.u, ")</text>",sep="")
		#For base of vert line
		svgtext = paste(svgtext,"<text x=\"", 325 + -10 + 100 * meta2$coef[[2]][1], "\" y=\"" , 40 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">",round(meta2$coef[[2]][1]*100,0),"</text>",sep="")
		#Spec
		ci.l = round(100*inv.logit(meta2$coef[3,2] - 1.96*meta2$coef[3,3]),0)
		ci.u = round(100*inv.logit(meta2$coef[3,2] + 1.96*meta2$coef[3,3]),0)
		svgtext = paste(svgtext,"<text x=\"450\" y=\"" , 40 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">", round(meta2$coef[[3]][1]*100,0), " (",ci.l ," - ", ci.u, ")</text>",sep="")
		#For base of vert line
		svgtext = paste(svgtext,"<text x=\"", 550 + -10 + 100 * meta2$coef[[3]][1], "\" y=\"" , 40 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">",round(meta2$coef[[3]][1]*100,0),"</text>",sep="")
		LRpos = meta2$coef[[2]][1]  / (1 - meta2$coef[[3]][1])
		LRneg = (1 - meta2$coef[[2]][1]) / meta2$coef[[3]][1]
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 60 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">Likelihood ratios: positive ",round(LRpos,1),"; negative ",round(LRneg,1),"</text>",sep="")
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 75 + i*20 ,"\">(hierarchical bivariate model)</text>",sep="")
	#AUC
	auc <- AUC(phm(myframe))
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 95 + i*20 ,"\" style=\"font-weight:bold\">Area under the ROC curve: ", round(auc$AUC[[1]][1],3), "</text>",sep="")
	#Heterogeneity
	#Hetero prelim
	# Method 1
	# cochran.Q(meta1$DOR[[1]],1/studysize) # to be used later in heterogeneity testing
	# Method 2
	# Could also use mada's equality of sensitivities and specificities
	# Method 3
	# madauni
	meta3 <- madauni(myframe, correction = 0.5, correction.control = "all")
		# http://handbook.cochrane.org/chapter_9/9_5_2_identifying_and_measuring_heterogeneity.htm
		# DF = n -1 studies
		I2 <- (meta3$CQ['Q']-meta3$CQ['df'])*100/meta3$CQ['Q']
		I2 <- max(I2,0)
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 115 + i*20 ,"\" style=\"font-weight:bold\">Heterogeneity: ", round(I2,1), "%</text>",sep="")
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 130 + i*20 ,"\" style=\"font-weight:normal\">(For diagnostic odds ratio: Q = ",round(meta3$CQ['Q'],1),", DF = ",meta3$CQ['df'],")</text>",sep="")
	#predictive values
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 150 + i*20 ,"\" style=\"font-weight:bold\">Predictive values:</text>",sep="")
		totalstudied = sum(TP)+sum(FP)+sum(FN)+sum(TN)
		pooledprevalence = (sum(TP)+sum(FN))/(totalstudied)
		PreTestOdds = pooledprevalence / (1 - pooledprevalence)
		pooledprevalence = round(pooledprevalence*100,0)
		PostTestOdds = PreTestOdds * LRpos
		ppv = sprintf("%.1f",(PostTestOdds/(1+PostTestOdds)*100))
		PostTestOdds = PreTestOdds * LRneg
		npv = sprintf("%.1f",(PostTestOdds/(1+PostTestOdds)*100))
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 170 + i*20 ,"\">At the prevalences studied (pooled ", pooledprevalence, "%, median ", median(prevalence), ", range ", round(min(prevalence),0)," - ", round(max(prevalence),0),", odds ",round(PreTestOdds,2),"):</text>",sep="")
		svgtext = paste(svgtext, "<text x=\"20\" y=\"" , 185 + i*20 ,"\">Positive ", ppv, "%; 1- negative ", npv, "%</text>",sep="")
		svgtext = paste(svgtext, "<a xlink:href=\"http://sumsearch.org/calc/calc.aspx?calc_dx_SnSp.aspx?prevalence=", pooledprevalence, "&amp;sensitivity=", round(meta2$coefficients[[2]][1]*100,0), "&amp;specificity=", round(meta2$coefficients[[3]][1]*100,0), "\" xlink:title=\"Adjust prevalence and recalculate predictive values\" target=\"_blank\"><text x=\"20\" y=\"" , 200 + i*20 ,"\" fill=\"rgba(0,0,255,1)\" style=\"font-weight:normal;text-decoration:underline;\">Click here to recalculate predictive values at other prevalences</text></a>",sep="")
	#Heterogeneity exploration
		#svgtext = paste(svgtext, "<!-- Heterogeneity exploration --><text x=\"10\" y=\"" , 185 + i*20 ,"\" style=\"font-weight:bold\">Heterogeneity exploration:</text>",sep="")
		#svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 200 + i*20 ,"\">Equality (chi-square) of sensitivities ", sprintf("%.3f",meta1$sens.htest['p.value'][1]),"; specificities ", sprintf("%.3f",meta1$spec.htest['p.value'][1]),"</text>",sep="")
		#Yearreg <- reitsma(myframe, formula = cbind(tsens, tfpr) ~ year)
		#Prevreg <- reitsma(myframe, formula = cbind(tsens, tfpr) ~ prevalence)
		#Sizereg <- reitsma(myframe, formula = cbind(tsens, tfpr) ~ studysize)
		#svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 215 + i*20 ,"\">Regression with diagnostic odds ratio: year pending; study size pending; prevalence pending.</text>",sep="")
		#svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 195 + i*20 ,"\">Correlation of sensitivities and false positive rates: ",totalstudied,"</text>",sep="")
	#About the studies
		svgtext = paste(svgtext, "<!-- About the studies --><text x=\"10\" y=\"" , 225 + i*20 ,"\" style=\"font-weight:bold\">About the studies:</text>",sep="")
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 245 + i*20 ,"\">",totalstudied," persons (median ",median(studysize),", range ",min(studysize)," - ",max(studysize),") in ",length(myframe$Study)," studies.</text>",sep="")
		svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 260 + i*20 ,"\">",sum(withoutcome)," persons with the outcome (median ",median(withoutcome),", range ",min(withoutcome)," - ",max(withoutcome),").</text>",sep="")
	}
	svgtext = paste(svgtext, "</g>",sep="")
if (type=="subgroup")
	{
	msg = "Under construction (s)"
	}
if (type=="metaregression (m)")
	{
	msg = "Under construction"
	# From mada's reitsma
	#(fit <- reitsma(Hayashino, formula = cbind(tsens, tfpr) ~ Year))
	#summary(fit) ## sensitivities significantly lower for SAQ
	#Could automatically check for sig of year, studysize, prevalence
	}

	#End of SVG
	svgtext = paste(svgtext, "Sorry, your browser does not support inline SVG for dynamic graphics.</svg>")

	msg = paste("<h3 style=\"font-family:Arial, Helvetica, sans-serif\">", topic, "</h3>\n",svgtext,msg,sep="")
	msg = paste(msg,"<table style=\"background-color:#CFCFCF\" border=\"0\"><tbody><tr><td><a class=\"selectall\" href=\"javascript:document.getElementById('svgfile').select()\">Select source code for graphics below</a> (then press control and C together to copy.)<br><textarea id=\"svgfile\" cols=\"80\" rows=\"10\" wrap=\"virtual\" onfocus=\"this.select()\">",svgtext,"</textarea></td></tr></tbody></table>",sep="")
	list(
	message = msg
  )
}
