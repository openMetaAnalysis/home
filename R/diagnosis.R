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
#par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue,new = TRUE) #bg=SkyBlue)
if (type=="ignore")
	{
	  list(
		message = "Under construction"
	  )
	}
if (type=="subgroup")
	{
	  list(
		message = "Under construction"
	  )
	}
if (type=="metaregression")
	{
	  list(
		message = "Under construction"
	  )
	}
}
