#################################################################################
### Read Stata input file and write as txt-file to be used as input in R-MOMO ###
#################################################################################

# Full path and name of A-MOMO Stata input file
MOMOStataFile <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/DoD_DoR.dta"

# Full path and name of R-MOMO txt input file
MOMORFile <- "C:/usr/EuroMOMO/R-MOMO/R_MOMO_DK/DoD_DoR.txt"

library(foreign)
DoD_DoR <- read.dta(MOMOStataFile)
DoD_DoR <- DoD_DoR[,c("DoD","DoR","age")]
DoD_DoR <- subset(DoD_DoR, DoD_DoR$DoD >= Sys.Date()-7*365.25)
write.table(DoD_DoR,file=MOMORFile, sep=";")
rm(DoD_DoR,MOMOStataFile,MOMORFile)

##############################################################################
### Copy Bank Holiday File from A-MOMO to .txt file to be used with R-MOMO ###
##############################################################################

# Full path and name of A-MOMO Stata Bank Holidays input file
MOMOBankHoliday <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/DKwork.dta"

# Full path and name of R-MOMO txt Bank Holiday input file
MOMORBankHoliday <- "C:/usr/EuroMOMO/R-MOMO/R_MOMO_DK/BankHoliday.txt"

library(foreign)
BH <- read.dta(MOMOBankHoliday)
write.table(BH,file=MOMORBankHoliday, sep=";")
rm(BH,MOMOBankHoliday,MOMORBankHoliday)
