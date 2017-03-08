# ==========================
#   ODS - Final Assignment
#   Data Wrangling
# ==========================

# author : Ismo Vihtola
# email  : ismo.vihtola@hotmail.com
# date   : 08.03.2017

# --------------------
#   File Description
# --------------------

# This file has been done for Open Data Science course, which was hold January-March 2017 in Helsinki University. This
# file has been used to describe, how dataset was generated for use in Final Assignment .
#
# Dataset was generated based on information that can be found from the following address :
# http://www.hockeyabstract.com/testimonials. From this web page NHL 2015-16 Player Data was selected and downloaded 
# to own machine. File is named as NHL 2015-16.xls. 

# ---------------------
#   Preliminary tasks
# ---------------------

setwd("C:/DATA/Opiskelu/IODS-final")

# ---------------------------
#   How dataset was created
# ---------------------------

# (A) go to http://www.hockeyabstract.com/testimonials
# (B) download file NHL 2015-16.xls
# (C) Start Excel and generate csv-files from sheets Main Page and Penalty Kill
# (D) After csv-files have been generated and saved start R-Studio
# (E) csv-file Main Page includes basic information about players. Dataframe Basic will be generated, See section E below 
# (F) csv-file Penalty Kill includes information about players how they have been playing as penalty killer,
#     see section F below
# (G) dataframes Basic (dataframe Basic2 and Penalty Kill (PK2) will be combined to dataframe DB1, see section G 
# (H) Several modifications will be made to dataframe DB1, detailed infoarmation -> see section H below

# ---------------------
#   Preliminary tasks
# ---------------------

setwd("C:/DATA/Opiskelu/IODS-final")

library(dplyr)
library(GGally)


# (E) Basic
# ---------

# First Basic information is read in R and modified
Basic1 <- read.table("Basic_information.csv", header=TRUE, sep=";",comment.char="", quote="")
dim(Basic1)
# [1] 898 129
Basic2 <- subset(Basic1, select=c(Last.Name, First.Name, GP, End.Team, DOB, Pos))
dim(Basic2)
# [1] 898   5

# (F) Penalty Kill
# ----------------

# Secondly, Penalty Kill information is read in
PK1 <- read.table("Penalty_Kill.csv", header=TRUE, sep=";",comment.char="", quote="")
# [1] 898  51
PK2 <- subset(PK1, select=c(Last.Name, First.Name, SHTOI, SHBlk, SHTake, TmSHGA, SHP))
# [1] 898   7

# (G) Combining Basic and Penalty Kill
# ------------------------------------

# Basic2 and PK2 will be joined together
DB1 <- inner_join(Basic2, PK2)
# [1] 898  10

str(DB1)
# 'data.frame':	898 obs. of  10 variables:
# $ Last.Name : Factor w/ 817 levels "Abdelkader","Acciari",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ First.Name: Factor w/ 389 levels "Aaron","Adam",..: 197 275 203 17 200 192 175 19 343 253 ...
# $ End.Team  : Factor w/ 30 levels "ANA","ARI","BOS",..: 11 3 7 9 30 6 11 14 16 26 ...
# $ DOB       : Factor w/ 810 levels "1972-02-15","1976-04-13",..: 251 531 574 462 336 721 346 494 649 176 ...
# $ Pos       : Factor w/ 17 levels "C","C/LW","C/LW/RW",..: 14 1 9 10 7 17 1 10 17 9 ...
# $ SHTOI     : int  7904 1460 0 0 13990 47 3242 151 28 60 ...
# $ SHBlk     : int  15 3 0 0 49 0 2 0 0 0 ...
# $ SHTake    : int  10 0 0 0 2 0 1 0 0 1 ...
# $ TmSHGA    : int  8 NA 0 0 21 0 7 0 0 0 ...
# $ SHP       : int  0 0 0 0 0 0 0 0 0 0 ...

# (H) Modifications to DB1
# ------------------------

# Following modifications will be done, codes -> see below
# - New variable Age. Will be calculated as NHL last playing date in season 2016 - Players Date of Birth (DOB).
#   Variables type is integer
# - New variable POS. If variable Pos information is other than D (defenceman), save it as F (forward). Otherwise save
#   as D.
# - At least 30 games have to been played. Otherwise players inforamtion will be deleted from next steps dataframe
# - At least 60 seconds average penalty killing time per match have to been played. Otherwise players inforamtion will
#   be deleted from next steps dataframe.
# - New variable Name. variables Last.Name and First.Name will be combinede. New variables type is chr
# - Several new variables will be generated based on that combining between players is easier. This mean that players
#   Time On Ice when playing penalty kill will be calculated as they would play 60 minutes penalty killing. 
# - Several variables will be deleted from next steps dataframe
# - variable End.Team new name is Team
# - two files will be saved, DB3 and DB4

# DB1$Name <- paste(DB1$Last.Name, DB1$First.Name, sep=', ')

DB2 <- DB1[DB1$GP >= 30,]

DB2$AgeDays <- as.Date(as.character("2016-04-09"), format="%Y-%m-%d") - as.Date(as.character(DB2$DOB), format="%Y-%m-%d")
DB2$Age2 <- as.numeric(DB2$AgeDays, units="days")
DB2$Age <- DB2$Age2/365
DB2$Age <- round(DB2$Age, 0)

DB2 <- within(DB2, {POS = ifelse(Pos=="D","D","F")})

rownames(DB2) <- DB2$Name

DB2$SHTOIP60 <- round(DB2$SHTOI/DB2$GP,0) # SHTOI P60 minutes      # Time on ice when playing short-handed
DB2$SHBlkP60 <- round((3600/DB2$SHTOIP60)*(DB2$SHBlk/DB2$GP),2)    # Personal blocks when playing short-handed
DB2$SHTakeP60 <- round((3600/DB2$SHTOIP60)*(DB2$SHTake/DB2$GP),2)  # Personal takeaways when playing short-handed
DB2$SHPP60 <- round((3600/DB2$SHTOIP60)*(DB2$SHP/DB2$GP),2)        # Personal points gained when playing short-handed
DB2$SHGAP60 <- round((3600/DB2$SHTOIP60)*(DB2$TmSHGA/DB2$GP),2)    # Goals allowed in short-hand, when player was on ice

DB3 <- DB2[DB2$SHTOIP60 >= 60,]                                    # Only players who play short-handed more than 60 seconds per game

DB3$Last.Name <- NULL
DB3$First.Name <- NULL
DB3$DOB <- NULL
DB3$Pos <- NULL
DB3$SHTOI <- NULL
DB3$SHBlk <- NULL
DB3$SHTake <- NULL
DB3$TmSHGA <- NULL
DB3$SHP <- NULL
DB3$Name <- NULL
DB3$AgeDays <- NULL
DB3$Age2 <- NULL
DB3$End.Team <- NULL

write.csv(DB3, file = "DB3.csv")

DB4 <- DB3
DB4$POS <- NULL
write.csv(DB4, file = "DB4.csv"