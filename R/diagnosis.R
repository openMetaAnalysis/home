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
msg = "<h3>Under construction</h3>\n"

meta1 <- madad(TP=TP,FN=FN,TN=TN,FP=FP,names=Study,data=myframe)

#Start of SVG
height = 20 + length(myframe$Study) * 20
svgtext = paste("<svg x=\"0px\" y=\"0px\" width=\"600px\" height=\"", height, "px\" viewBox=\"0 0 600 ", height, "\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">")
for(i in 1: length(myframe$Study))
	{
	svgtext = paste(svgtext, "<a xlink:href=\"http://pubmed.gov/",myframe$pmid[i],"\" target=\"_blank\"><text x=\"10\" y=\"" , i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",myframe$Study[i],", ", myframe$year[i],"</text></a>",sep="")
	#svgtext = paste(svgtext,"<text x=\"200\" y=\"" , i*20 ,"\" fill=\"black\" style=\"font-weight:bold\">*</text>")
	#Sensitivity
	x = 200 + 100 * meta1$sens[[1]][i]
	svgtext = paste(svgtext,"<text x=\"",x,"\" y=\"" , i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$sens[[1]][i]*100,1),"</text>",sep="")
	#Specificity
	x = 400 + 100 * meta1$spec[[1]][i]
	svgtext = paste(svgtext,"<text x=\"",x,"\" y=\"" , i*20 ,"\" fill=\"black\" style=\"font-weight:normal\">",round(meta1$spec[[1]][i]*100,1),"</text>\n",sep="")
	}
#End of SVG
svgtext = paste(svgtext, "</svg>")
msg = paste(msg, svgtext)

if (type=="ignore")
	{
	meta1 <- perfect.trees(TP=TP,FN=FN,TN=TN,FP=FP,study=Study,data=myframe)
	msg = paste(msg, "\n<div>Sensitivity=", round(meta1$coefficients[2]*100,1), "%</div>",sep="")
	msg = paste(msg, "\n<div>Specificity=", round(meta1$coefficients[3]*100,1), "%</div>",sep="")
	}
if (type=="subgroup")
	{
	msg = "Under construction (s)"
	}
if (type=="metaregression (m)")
	{
	msg = "Under construction"
	}
  list(
	message = msg
  )
}
