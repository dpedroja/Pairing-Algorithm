# make sure packages are installed

library(readr)
library(data.table)
library(pdist)
library(Matrix)
library(dplyr)





# import huc and gage data tables (keep separate from each other to use pdist)
# these tables are output as the last step of the COMID_HUC12_LINK script.

hucs <- read_csv("//ca.epa.local/SB/DWR/VOL1/WQC-PT/Cannabis - Regional Policies/Database 2.0/DATA_PROJECTS/PAIRING/ALGORITHMIC/areas_pairing.csv")
gages <- read_csv("//ca.epa.local/SB/DWR/VOL1/WQC-PT/Cannabis - Regional Policies/Database 2.0/DATA_PROJECTS/PAIRING/ALGORITHMIC/gages_pairing.csv")
hucs <- as.data.frame(hucs) 
gages <- as.data.frame(gages[1:18])



# name rows & columns
dimnames(hucs) = list(hucs[[18]],  names(hucs))  
dimnames(gages) = list(gages[[18]], names(gages))
r <- gages[[18]]
c <- hucs[[18]]

# function for later
interval01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x, na.rm=TRUE))}

################################ HYDROLOGY ###########################

h1 <- as.matrix(pdist(gages[,2:13], hucs[,2:13])) # area ids x gages

dimnames(h1) = list( gages[[18]], hucs[[18]])

h1scaled <- scale(h1, colMeans(h1), scale = TRUE)
h1scaled01 <- (1-interval01(h1scaled)) # CONTAINS NaNs

hydrorankings <- cbind(   (rep(c, times = 1, each = nrow(h1scaled01))), (rep(r, times = ncol(h1scaled01), each = 1)), as.vector(h1scaled01))
colnames(hydrorankings) <- c("AREA_ID", "GAGE_ID", "HYDRO_SIMILARITY")

############################### DISTANCE ###########################

d1 <- as.matrix(pdist(gages[,14:15], hucs[,14:15])) # gages x hucs
d1 <- d1/1000


dimnames(d1) = list( gages[[18]], hucs[[18]])

d1scaled <- scale(d1, center = TRUE, scale=TRUE)

d1scaled01 <- (1 - interval01(d1scaled))

distancerankings <- cbind(   (rep(c, times = 1, each = nrow(d1scaled01))), (rep(r, times = ncol(d1scaled01), each = 1)), as.vector(d1scaled01))
colnames(distancerankings) <- c("AREA_ID", "GAGE_ID", "DIST_SIMILARITY")

############################## DRAINAGE AREA ######################
# Natural Log
#hist(gages[, 16])

logG <- as.data.frame(log(gages[, 16]))
logH <- as.data.frame(log(hucs[, 16]))

loga1b <- matrix(nrow = nrow(gages), ncol = nrow(hucs) )
for(i in 1:nrow(gages)){
  for(j in 1:nrow(hucs)){
    loga1b[i,j] <- sqrt( (logG[i, 1] - logH[j, 1])^2 )
  }
}

dimnames(loga1b) = list(gages[[18]], hucs[[18]])

loga1bscaled <- scale(loga1b, colMeans(loga1b), scale=TRUE)
loga1bscaled01 <- (1 - interval01(loga1bscaled))

arearankingsb <- cbind(   (rep(c, times = 1, each = nrow(loga1bscaled01))), (rep(r, times = ncol(loga1bscaled01), each = 1)), as.vector(loga1bscaled01))
colnames(arearankingsb) <- c("AREA_ID", "GAGE_ID", "DRAIN_AREA_SIMILARITY")

############################## HUC 12 DIFFERENTIAL###################################################

hucdiff1 <- matrix(nrow = nrow(gages), ncol = nrow(hucs) )
downstream <- matrix(nrow = nrow(gages), ncol = nrow(hucs) )

# this takes the difference between the huc12 numbers. The reason for taking the minimum of this difference and 99,999,999 is to exclude pairs that
# share a huc 4 only


huc_digits <- as.data.frame(hucs$AREA_ID)
  
huc_digits$twelve <- as.numeric(substr(hucs$IN_HUC12, 11,12))
huc_digits$ten <- as.numeric(substr(hucs$IN_HUC12, 9,10))
huc_digits$eight <- as.numeric(substr(hucs$IN_HUC12, 7,8))
huc_digits$six <- as.numeric(substr(hucs$IN_HUC12, 5,6))
huc_digits$four <- as.numeric(substr(hucs$IN_HUC12, 3,4))

colnames(huc_digits) <- c("AREA_ID", "TWELVE", "TEN", "EIGHT", "SIX", "FOUR")

gage_digits <- as.data.frame(gages$GAGE_ID)

gage_digits$twelve <- as.numeric(substr(gages$IN_HUC12, 11,12))
gage_digits$ten <- as.numeric(substr(gages$IN_HUC12, 9,10))
gage_digits$eight <- as.numeric(substr(gages$IN_HUC12, 7,8))
gage_digits$six <- as.numeric(substr(gages$IN_HUC12, 5,6))
gage_digits$four <- as.numeric(substr(gages$IN_HUC12, 3,4))

colnames(gage_digits) <- c("GAGE_ID", "TWELVE", "TEN", "EIGHT", "SIX", "FOUR")




for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){
    
  if(gage_digits[i,"TWELVE"] == huc_digits[j,"TWELVE"] & huc_digits[j, "TEN"] == gage_digits[i, "TEN"] & huc_digits[j, "EIGHT"] == gage_digits[i, "EIGHT"] & huc_digits[j, "SIX"]== gage_digits[i, "SIX"] & huc_digits[j, "FOUR"]== gage_digits[i, "FOUR"]) {
      hucdiff1[i,j] <- 12
      }else if(huc_digits[j, "TEN"] == gage_digits[i, "TEN"] & huc_digits[j, "EIGHT"] == gage_digits[i, "EIGHT"] & huc_digits[j, "SIX"]== gage_digits[i, "SIX"] & huc_digits[j, "FOUR"]== gage_digits[i, "FOUR"]){
        hucdiff1[i,j] <- 10
      }else if(huc_digits[j, "EIGHT"] == gage_digits[i, "EIGHT"] & huc_digits[j, "SIX"]== gage_digits[i, "SIX"] & huc_digits[j, "FOUR"]== gage_digits[i, "FOUR"]){
        hucdiff1[i,j] <- 8
      }else if(huc_digits[j, "SIX"]== gage_digits[i, "SIX"] & huc_digits[j, "FOUR"]== gage_digits[i, "FOUR"]){
        hucdiff1[i,j] <- 6
                }else {
                  hucdiff1[i,j] <- 4          
                }
          }
      }
      
  
  
  
  

for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){  
  
      ifelse((gages[i,17] > hucs[j, 17]), downstream[i,j] <- 1, downstream[i,j] <- 0)
  
  }
}
  

# View(hucdiff1[1:10, 1:10 ])


# end of nested for loops


# Don't worry about this:
# x <- 0
# a <- table(hucdiff1)
# x <- a[names(a)==99]
# x <- x + a[names(a)==9999]


# TABLE V1      HUCDIFF_MAX (X)   LN(X+1)
# SAME HUC 12	  0	                0	          
# SAME HUC 10	  99	              4.61	  	
# SAME HUC 8	  9,999	            9.21	      	           
# SAME HUC 6	  999,999	          13.82	                    
# SAME HUC 4	  99,999,999 	      18.42	      	          

scale <- 1 - interval01(c(0, 4.61, 9.21, 13.82, 18.42))

# scale (approximate)
# TABLE V1    
# SAME HUC 12	    1  
# SAME HUC 10	    0.75
# SAME HUC 8	    0.5
# SAME HUC 6	    0.25
# SAME HUC 4	    0

samehuc12 <- scale[[1]]
samehuc10 <- scale[[2]]
samehuc8 <- scale[[3]]
samehuc6 <- scale[[4]]
samehuc4 <- scale[[5]]

# another function
HUC <- function(x,y) { ####### See scale table below
  if (x == 12) {
    result <- samehuc12 
    
  } else if (x == 10) {
    result <- samehuc10
  
  } else if (x == 8) {
    result <- samehuc8
    
  } else if (x == 6) {
    result <- samehuc6
    
  } else {(x == 4) 
    result <- samehuc4
    
  }
}

HUCdiffscores01 <- apply(hucdiff1, c(1,2), FUN = HUC)
dimnames(HUCdiffscores01) = list(gages[[18]], hucs[[18]])

######################################################################################
# DOWNSTREAM MODIFIERs

for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){
if (HUCdiffscores01[i,j] == samehuc10 & downstream[i,j] == 1){
    HUCdiffscores01[i,j] <- (HUCdiffscores01[i,j] + 0.125)
  }
  }
}

for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){
    if (HUCdiffscores01[i,j] == samehuc8 & downstream[i,j] == 1){
      HUCdiffscores01[i,j] <- (HUCdiffscores01[i,j] + 0.125)
    }
  }
}

for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){
    if (HUCdiffscores01[i,j] == samehuc6 & downstream[i,j] == 1){
      HUCdiffscores01[i,j] <- (HUCdiffscores01[i,j] + 0.0)
    }
  }
}

for(i in 1:nrow(gages)){
  
  for(j in 1:nrow(hucs)){
    if (HUCdiffscores01[i,j] == samehuc4 & downstream[i,j] == 1){
      HUCdiffscores01[i,j] <- (HUCdiffscores01[i,j] + 0.0)
    }
  }
}


HUCrankings <- cbind(   (rep(c, times = 1, each = nrow(HUCdiffscores01))), (rep(r, times = ncol(HUCdiffscores01), each = 1)), as.vector(HUCdiffscores01))
colnames(HUCrankings) <- c("AREA_ID", "GAGE_ID", "HUC_SCORE")

#################################





############### COMBINE AND CALCULATE PERCENT MATCH ###########################################

a1 <- as.data.frame(hydrorankings)
b1 <- as.data.frame(distancerankings) 
c1 <- as.data.frame(arearankingsb)
d1 <- as.data.frame(HUCrankings)

e1 <- merge(a1, b1, by = c("AREA_ID" , "GAGE_ID"))
e1 <- merge(e1, c1, by = c("AREA_ID" , "GAGE_ID"))
e1 <- merge(e1, d1, by = c("AREA_ID" , "GAGE_ID"))

e1$HYDRO_SIMILARITY <- as.numeric(as.character(e1$HYDRO_SIMILARITY))
e1$DIST_SIMILARITY <- as.numeric(as.character(e1$DIST_SIMILARITY))
e1$DRAIN_AREA_SIMILARITY <- as.numeric(as.character(e1$DRAIN_AREA_SIMILARITY))
e1$HUC_SCORE <- as.numeric(as.character(e1$HUC_SCORE))

# e1$percent_match_unweighted <- (e1[ , 3] + e1[ ,4] + e1[ , 5] + e1[ , 6])/4

e1[is.na(e1) ] <- 0 # change NaNs to zero 



# WEIGHTS 
#################################################################################
#################################################################################
#################################################################################


HydroW <- 0.25
DistW <- 0.50
AreaW <-  0.10
HUCW <- 0.15

#################################################################################
#################################################################################
#################################################################################



# weighted average
e1$SCORE <- HydroW * e1[ , 3] + DistW * e1[ ,4] + AreaW * e1[ , 5] + HUCW * e1[ , 6] 

### ADD A COLUMN WITH DISTANCE RANKING 
AREA_ID_RANKINGS <- as.data.frame(e1[order(e1[, 1], e1$SCORE, decreasing = TRUE),])
AREA_ID_RANKINGS$RANK <- rep(1:nrow(gages), times = nrow(hucs), each = 1)

### ADD A COLUMN WITH DISTANCE RANKING 
distrank  <- as.data.frame(e1[order(e1[, 1], e1$DIST_SIMILARITY, decreasing = TRUE),])
distrank$distrank <- rep(1:nrow(gages), times = nrow(hucs), each = 1)


### ADD A COLUMN WITH HYDRO RANKING 
hydrorank <- as.data.frame(e1[order(e1[, 1], e1$HYDRO_SIMILARITY, decreasing = TRUE),])
hydrorank$hydrorank <- rep(1:nrow(gages), times = nrow(hucs), each = 1)

### ADD A COLUMN WITH DRAINAGE AREA RANKING 
DArank <- as.data.frame(e1[order(e1[, 1], e1$DRAIN_AREA_SIMILARITY, decreasing = TRUE),])
DArank$DArank <- rep(1:nrow(gages), times = nrow(hucs), each = 1)

### ADD A COLUMN WITH HUC_SCORE RANKING, TIE-BREAKER = DIST_SIMILARITY
hucrank <- as.data.frame(e1[order(e1[, 1], e1$HUC_SCORE, e1$DIST_SIMILARITY, decreasing = TRUE),])
hucrank$hucrank <- rep(1:nrow(gages), times = nrow(hucs), each = 1)

allranks <- merge(AREA_ID_RANKINGS, hydrorank[ ,c(1:2, 8)], by = c("AREA_ID" , "GAGE_ID"))
allranks <- merge(allranks, distrank[ ,c(1:2, 8)], by = c("AREA_ID" , "GAGE_ID"))
allranks <- merge(allranks, DArank[ ,c(1:2, 8)], by = c("AREA_ID" , "GAGE_ID"))
allranks <- merge(allranks, hucrank[ , c(1:2, 8)], by = c("AREA_ID" , "GAGE_ID"))
  
allfactorranks <- allranks[ order(allranks[ ,1], allranks$SCORE, decreasing = TRUE),]

top_ten <- allfactorranks[allfactorranks$RANK < 11, ]

write.table(top_ten, file="//ca.epa.local/SB/DWR/VOL1/WQC-PT/Cannabis - Regional Policies/Database 2.0/DATA_PROJECTS/PAIRING/ALGORITHMIC/top_ten_factor_ranks.csv", sep = ",", row.names = FALSE )



# View(top_ten[1:100, ])



one <- allfactorranks[allfactorranks$RANK == 1, ] 
two <- allfactorranks[allfactorranks$RANK == 2, ] 
three <- allfactorranks[allfactorranks$RANK == 3, ] 
four <- allfactorranks[allfactorranks$RANK == 4, ]

one_two <- merge(one, two, by = "AREA_ID")
one_three <- merge(one_two, three, by = "AREA_ID")
one_four <- merge(one_three, four, by = "AREA_ID")

fields <- colnames(one)
fields_1 <- c(fields[[1]], paste(fields[2:12],"1", sep = "_"))
fields_2 <- paste(fields[2:12],"2", sep = "_")  
fields_3 <- paste(fields[2:12],"3", sep = "_")
fields_4 <- paste(fields[2:12],"4", sep = "_")  
fields1_4 <- c(fields_1, fields_2, fields_3, fields_4)
colnames(one_four) <- fields1_4



pgbg <- one_four[, c(1:2, 13)]
pgbg$PG_ASSIGNMENT_CODE <- 20
pgbg$BG_ASSIGNMENT_CODE <- 20

pgbg <- pgbg[,c(1,2,4,3,5) ]

colnames(pgbg) <- c("AREA_ID", "PRIMARY_GAGE_ID", "PG_ASSIGNMENT_CODE", "BACKUP_GAGE_ID", "BG_ASSIGNMENT_CODE")

write.table(pgbg, file = "//ca.epa.local/SB/DWR/VOL1/WQC-PT/Cannabis - Regional Policies/Database 2.0/DATA_PROJECTS/PAIRING/ALGORITHMIC/FINAL_PAIRING_ALGORITHM_OUTPUT.csv", sep = ",", row.names = FALSE) 











