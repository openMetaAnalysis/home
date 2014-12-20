#Uses http://cran.r-project.org/web/packages/meta/
#Alternatives:f
# http://cran.r-project.org/web/packages/metafor/ (allows continuity correction)
# http://cran.r-project.org/web/packages/rmeta/
# Discussion of continuity correction:
# http://handbook.cochrane.org/chapter_16/16_9_2_studies_with_zero_cell_counts.htm
diagnosis <- function(content, measure, year, pmid, sortby, lefthand, righthand, type, cofactorlabel, topic, theme) {
temp <- content
# Uses package meta http://cran.r-project.org/web/packages/meta/
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

attach(myframe)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
pubbiastext = "Test for funnel plot asymmetry"
analyticmethod = "Hierarchical model (bivariate)"
msg = ""

meta1 <- madad(TP=TP,FN=FN,TN=TN,FP=FP,names=Study,data=myframe)

#Start of SVG
height = 175 + length(myframe$Study) * 20
svgtext = paste("<svg x=\"0px\" y=\"0px\" width=\"800px\" height=\"", height, "px\" viewBox=\"0 0 800 ", height, "\" style=\"font-family:Arial, Helvetica, sans-serif\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">")
for(i in 1: length(myframe$Study))
	{
	#Column names
	svgtext = paste(svgtext, "<text x=\"10\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Study</text></a><text x=\"250\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Sensitivity (%)</text><text x=\"500\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">Specificity (%)</text>",sep="")
	#Citation
	studysize = TP[i] + FP[i] + FN[i] + TN[i]
	prevalence = round(100*(TP[i] + FN[i])/studysize,0)
	svgtext = paste(svgtext, "<a xlink:href=\"http://pubmed.gov/",myframe$pmid[i],"\" title=\"Study size is ",studysize,". Prevalence of the outcome is ",prevalence,"%. Click citation to open abstract at PubMed in a new window.\"target=\"_blank\"><text x=\"10\" y=\"" , 15 + i*20 ,"\" fill=\"rgba(0,0,255,1)\">",myframe$Study[i],", ", myframe$year[i],"</text></a>",sep="")
	#Sensitivity
	x = 300 + 100 * meta1$sens[[1]][i]
		#text
		svgtext = paste(svgtext,"<text x=\"200\" y=\"" , 15 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$sens[[1]][i]*100,0)," (", round(meta1$sens$sens.ci[i,1]*100,0) ," - ", round( meta1$sens$sens.ci[i,2]*100,0), ")</text>",sep="")
		#point estimate
		svgtext = paste(svgtext,"<circle cx=\"", x, "\" cy=\"" , 15 + i*20 ,"\" r=\"3\" stroke-width=\"0\" style=\"fill:black;\"/>",sep="")
		#Confidence intervals
		cl.lower = 300 + 100 * meta1$sens$sens.ci[i,1]
		ci.upper = 300 + 100 * meta1$sens$sens.ci[i,2]
		svgtext = paste(svgtext,"<line x1=\"" , cl.lower ,"\" y1=\"" , 15 + i*20 ,"\" x2=\"" , ci.upper ,"\" y2=\"" , 15 + i*20 ,"\" style=\"stroke:rgba(0,0,0,1);stroke-width:2\" />", sep="")
	#Specificity
	x = 550 + 100 * meta1$spec[[1]][i]
		#text
		svgtext = paste(svgtext,"<text x=\"450\" y=\"" , 15 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$spec[[1]][i]*100,0)," (", round(meta1$spec$spec.ci[i,1]*100,0) ," - ", round( meta1$spec$spec.ci[i,2]*100,0), ")</text>",sep="")
		#point estimate
		svgtext = paste(svgtext,"<circle cx=\"", x, "\" cy=\"" , 15 + i*20 ,"\" r=\"3\" stroke-width=\"0\" style=\"fill:black;\"/>",sep="")
		#Confidence intervals
		cl.lower = 550 + 100 * meta1$spec$spec.ci[i,1]
		ci.upper = 550 + 100 * meta1$spec$spec.ci[i,2]
		svgtext = paste(svgtext,"<line x1=\"" , cl.lower ,"\" y1=\"" , 15 + i*20 ,"\" x2=\"" , ci.upper ,"\" y2=\"" , 15 + i*20 ,"\" style=\"stroke:rgba(0,0,0,1);stroke-width:2\" />", sep="")
	}

if (type=="ignore")
	{
	meta1 <- perfect.trees(TP=TP,FN=FN,TN=TN,FP=FP,study=Study,data=myframe)
	svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 35 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">Summary</text><text x=\"200\" y=\"" , 35 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">",round(meta1$coefficients[[2]][1]*100,0)," <!--(", round(meta1$sens$sens.ci[i,1]*100,0) ," - ", round(meta1$sens$sens.ci[i,1]*100,0), ")--></text><text x=\"450\" y=\"" , 35 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">",round(meta1$coefficients[[3]][1]*100,0)," <!--(", round(meta1$spec$spec.ci[i,1]*100,0) ," - ", round( meta1$spec$spec.ci[i,2]*100,0), ")--></text>",sep="")
	LRpos = meta1$coefficients[[2]][1]  / (1 - meta1$coefficients[[3]][1])
	LRneg = (1 - meta1$coefficients[[2]][1]) / meta1$coefficients[[3]][1]
	svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 50 + i*20 ,"\" fill=\"black\">Likelihood ratios: positive is ",round(LRpos,1),", negative is ",round(LRneg,1),"</text>",sep="")
	svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 65 + i*20 ,"\" fill=\"black\">(hierarchical bivariate model)</text>",sep="")
	#vertical lines for sn and sp
	#sensitivity
	svgtext = paste(svgtext, "<line x1=\"", 300 + 100 * meta1$coefficients[[2]][1], "\" y1=\"25\" x2=\"", 300 + 100 * meta1$coefficients[[2]][1], "\" y2=", 25 + i*20 ," style=\"stroke:rgba(0,0,0,0.2);stroke-width:2\"></line>", sep="")
	#specificity
	svgtext = paste(svgtext, "<line x1=\"", 550 + 100 * meta1$coefficients[[3]][1], "\" y1=\"25\" x2=\"", 550 + 100 * meta1$coefficients[[3]][1], "\" y2=", 25 + i*20 ," style=\"stroke:rgba(0,0,0,0.2);stroke-width:2\"></line>", sep="")
	#AUC
	auc <- AUC(phm(myframe))
	svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 85 + i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">Area under the ROC curve: ", round(auc$AUC[[1]][1],3), "</text>",sep="")
	#Bayesian analysis
	totalstudied = sum(TP)+sum(FP)+sum(FN)+sum(TN)
	prevalence = (sum(TP)+sum(FN))/(totalstudied)
	PreTestOdds = prevalence / (1 - prevalence)
	prevalence = round(prevalence*100,0)
	PostTestOdds = PreTestOdds * LRpos
	ppv = sprintf("%.1f",(PostTestOdds/(1+PostTestOdds)*100))
	PostTestOdds = PreTestOdds * LRneg
	npv = sprintf("%.1f",(PostTestOdds/(1+PostTestOdds)*100))
	svgtext = paste(svgtext, "<text x=\"10\" y=\"" , 105 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">At the mean prevalence of ", prevalence, "% (odds = ",round(PreTestOdds,2),") of ", totalstudied," subjects studied:</text>",sep="")
	svgtext = paste(svgtext, "<text x=\"20\" y=\"" , 120 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">Positive predictive value is: ", ppv, "%</text>",sep="")
	svgtext = paste(svgtext, "<text x=\"20\" y=\"" , 135 + i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">Negative predictive value is: ", npv, "%</text>",sep="")
	svgtext = paste(svgtext, "<a xlink:href=\"http://sumsearch.org/calc/calc.aspx?calc_dx_SnSp.aspx?prevalence=", prevalence, "&amp;sensitivity=", round(meta1$coefficients[[2]][1]*100,0), "&amp;specificity=", round(meta1$coefficients[[3]][1]*100,0), "\" title=\"Adjust prevalence and recalculate predictive values\" target=\"_blank\"><text x=\"20\" y=\"" , 155 + i*20 ,"\" fill=\"rgba(0,0,255,1)\" style=\"font-weight:normal;text-decoration:underline;\">Click here to recalculate predictive values at other prevalences</text></a>",sep="")
	}
if (type=="subgroup")
	{
	msg = "Under construction (s)"
	}
if (type=="metaregression (m)")
	{
	msg = "Under construction"
	}

	#End of SVG
	svgtext = paste(svgtext, "</svg>")

	msg = paste("<div>Under construction</div><h3 style=\"font-family:Arial, Helvetica, sans-serif\">", topic, "</h3>\n",svgtext,msg,sep="")
	msg = paste(msg,"<table style=\"background-color:#CFCFCF\" border=\"0\"><tbody><tr><td><a class=\"selectall\" href=\"javascript:document.getElementById('svgfile').select()\">Select source code for graphics below</a> (then press control and C together to copy.)<br><textarea id=\"svgfile\" cols=\"80\" rows=\"10\" wrap=\"virtual\" onfocus=\"this.select()\">",svgtext,"</textarea></td></tr></tbody></table>",sep="")
	list(
	message = msg
  )
}
