## This script takes in a .csv output from Excel with the column headers 
## "Protein", "FC", and "Chi2" in that order. You can change these in the script
## to reflect something else. The values under each header are determined using 
## an input file of spectral counts/intensity from two different conditions and 
## a Chi_square test is run on them. The chi2/fold change threshold below should
## be changed to whatever you feel is appropriate. The script outputs the hits
## specified by parameters in two csv files (upreg and downreg) placed in the
## working directory (as well as the resulting volcano plot).
## 
v <- function(data1){
  require(ggplot2)
  datafile <- read.csv(file=data1,header=TRUE)
  upreg <- subset(datafile, Chi2 > 11.345 & FC > 2)
  downreg <- subset(datafile,Chi2 > 11.345 & FC < -2)
  write.csv(upreg,file = "upreg.csv")
  write.csv(downreg, file = "downreg.csv")
  output <- ggplot(datafile, aes(x=FC, y=Chi2)) +
    geom_text(aes(label=ifelse(Chi2 > 11.345 & FC > 2 | FC < -2 & Chi2 > 11.345, as.character(Protein),''),size=0.1)) +
    geom_point(colour="red", size = 1) + geom_hline(yintercept = 11.345,linetype="dotted") +
    geom_vline(xintercept = c(-2,2),linetype="dotted") + geom_vline(xintercept = 0)
  print(output)
  dev.copy(pdf,"FC_V_Chi2.pdf")
  dev.off()
  return(output)
}