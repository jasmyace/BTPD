
balanceAssign <- function(userID,assign,tblNames,partnerValid){
  
  #  userID <- 873
  #  partnerValid <- partnerValid$userID
  
  #   ---- Restrict to the doubly assigned.  We use these to get frequencies
  #   ---- of what's been done so far.  
  doubles <- assign[ !is.na(assign$digiDouble) & assign$digiDouble == 1,]
 
  #   ---- Identify the unique users up to this point.  Note that partnerValid
  #   ---- excludes the userID by default, but theUsers includes userID.  
  theUsers <- unique(c(doubles$digiPrimary,doubles$digiSecondary))
  theUsers <- c(theUsers[theUsers %in% partnerValid],userID)
 
  #   ---- Identify the options of assigning a partner.  
  theDoublyActives <- c(tblNames[tblNames$doubleActive == 1 & tblNames$userID != userID,]$userID)
  theDoublyActives <- theDoublyActives[theDoublyActives %in% partnerValid]
  
  #   ---- Count up the unique instances of users.  Call it 'Dim' since it forms
  #   ---- a square matrix, c.f. the table called below, prior to being pushed 
  #   ---- into a data frame.  We combine these two to allow new users into the mix.
  #   ---- They enter via theDoublyActives.  They can never ben in theUsers. 
  allOptions <- unique(c(theDoublyActives,theUsers))
  Dim <- length(allOptions)
    
  #   ---- Pretty up the list output from the table.  
    
  #   ---- This is roundabout, but when we have a new user, that user can only be
  #   ---- one of primary or secondary.  This means the table is non-square.  But 
  #   ---- everything assumes that it is.  So, hijack everything to ensure that 
  #   ---- the code that assumes that fact still works, even when it's not true. 
  # balance <- as.data.frame(table(doubles$digiPrimary,doubles$digiSecondary))

  #   ---- Make the square matrix, based on the number of distinct users.  
  balance <- matrix(NA,nrow=Dim,ncol=Dim)
  rownames(balance) <- allOptions
  colnames(balance) <- allOptions
    
  #   ---- Count the number of instances.  This emulates the table function.  
  for( i in 1:Dim ){
    for (j in 1:Dim ){
      balance[i,j] <- sum(doubles$digiPrimary == allOptions[i] & doubles$digiSecondary == allOptions[j])
    }
  }
    
  #   ---- This emulates the table-to-date-frame functionality.  
  balance <- as.data.frame(as.table(balance))
    
  #   ---- Now, we continue on as we did before.  
  names(balance)[names(balance) == "Var1"] <- "digiPrimary"
  names(balance)[names(balance) == "Var2"] <- "digiSecondary"
  balance$Row <- rep(seq(1,Dim,1),Dim)
  balance$Col <- balance$Row[order(balance$Row)]
    
  #   ---- Identify the parts of the original matrix in the list of rows 
  #   ---- we now have from the original table.  
  balance$Diag <- ifelse(balance$Row == balance$Col,1,0)
  balance$Upper <- ifelse(balance$Row < balance$Col,1,0)
  balance$Lower <- ifelse(balance$Row > balance$Col,1,0)
    
  #   ---- The userID is the one pulling the cell, and that person must be either
  #   ---- primary or secondary.  So, reduce to that person's data.  But also 
  #   ---- get rid of the diagonals while we're at it.  Note that the upper triangular
  #   ---- region of the original matrix holds PRIMARY, while the lower triangular 
  #   ---- region of the original matrix holds SECONDARY.  
  assignBalance <- balance[(balance$digiPrimary == userID | balance$digiSecondary == userID) & balance$Diag == 0,]
  
  
  #   ---- Sort by the userID's Freq counts.  The lower number is the data for 
  #   ---- we want to assign people.  
  assignBalance <- assignBalance[order(assignBalance$Freq),]
  
  if(assignBalance[1,]$digiPrimary == userID){
    userIDAssign <- "Primary"
    thePartnerAssign <- as.character(droplevels(assignBalance[1,]$digiSecondary))
  } else {
    userIDAssign <- "Secondary"
    thePartnerAssign <- as.character(droplevels(assignBalance[1,]$digiPrimary))
  }
  
  assignInfo <- data.frame(userID=userID,
                           userIDAssign=userIDAssign,
                           thePartnerAssign=thePartnerAssign)

#   
#   
#   #   ---- Check if we have a new user.  
#   if( sum(assignBalance$Freq) == 0 ){
#     primaryProp <- 0
#     secondaryProp <- 0
#   } else {
#     primaryProp <- sum(assignBalance[assignBalance$digiPrimary == userID,]$Freq) / sum(assignBalance$Freq)
#     secondaryProp <- sum(assignBalance[assignBalance$digiSecondary == userID,]$Freq) / sum(assignBalance$Freq)
#   }
# 
#   #   ---- Given the proportion of primary and secondary, assign the userID to be 
#   #   ---- the type of digitizer with less.  This helps balance out that userID's 
#   #   ---- assignments, given his/her assignments so far.  Note that I don't 
#   #   ---- randomly assign in the case of a tie...I just assign those to secondary.
#   #   ---- This is because I allow for the greater than or equal.  
#   userIDAssign <- ifelse(primaryProp >= secondaryProp,'Secondary','Primary')
#     
#   if(userIDAssign == "Secondary"){
#       
#     #   ---- Reduce to the lower triangular matrix.
#     userBalance <- assignBalance[assignBalance$Lower == 1,]
#       
#   } else {
#       
#     #   ---- Reduce to the upper triangular matrix.  
#     userBalance <- assignBalance[assignBalance$Upper == 1,]
#   }
#   
#     
# 
#       
#     #   ---- Get the coupling records for primary. 
#     theOptions <- as.data.frame(table(doubles$digiPrimary))
#     
#     #   ---- Reduce the options to those currently active in tblNames.  We should have
#     #   ---- at least one because we check for at least the availability of two doubly 
#     #   ---- digitizers before we enter the function.  
#     
#     #   ---- Check if 
#     theOptions$Var1 %in% theDoublyActives
#     
#     theOptions <- theOptions[theOptions$Var1 %in% theDoublyActives,]
#     thePartnerAssign <- theOptions[order(theOptions$Freq),]$Var1[1]
#     thePartnerAssign <- as.numeric(levels(thePartnerAssign))[thePartnerAssign]
#     theLowRank <- theOptions[order(theOptions$Freq),]$Freq[1]
#   } else {
#     
#       #   ---- Adjust factors levels to what they should be.  
#       userBalance$digiPrimary <- as.numeric(levels(userBalance$digiPrimary))[userBalance$digiPrimary]
#       userBalance$digiSecondary <- as.numeric(levels(userBalance$digiSecondary))[userBalance$digiSecondary]
#     
#       #   ---- Now, the idea is make the partner with the less number of pairings with 
#       #   ---- the userID the partner.  But, we could have a tie. Who do we choose then?
#       #   ---- Let's just keep things simple and choose randomly.  We could look at other
#       #   ---- frequency tables, and choose the person with the least examined cells
#       #   ---- overall, etc.  Finally, we must consider the possibility that there is a 
#       #   ---- new userID in the list of doubleAssign that hasn't actually digitized a 
#       #   ---- doubly cell yet.  This individual will not be in theUsers, but will be 
#       #   ---- in theDoublyActives.  
#     
#       #   ---- Get theDoublyActives not in theUsers.
#       theNewOnes <- theDoublyActives[!(theDoublyActives %in% theUsers)]
#     
#       #   ---- If we have a new user, add them to the list of possibilities.  
#       if( length(theNewOnes) > 0){
#         theNewUsers <- NULL
#         for(i in 1:length(theNewOnes)){
#           if(userIDAssign == "Secondary"){
#             thisNewUser <- data.frame(digiPrimary=theNewOnes[i],digiSecondary=userID,Freq=0,Row=0,Col=0,Diag=0,Upper=0,Lower=0)
#           } else {
#             thisNewUser <- data.frame(digiPrimary=userID,digiSecondary=theNewOnes[i],Freq=0,Row=0,Col=0,Diag=0,Upper=0,Lower=0)
#           }
#           theNewUsers <- rbind(theNewUsers,thisNewUser)
#         }
#         userBalance <- rbind(userBalance,theNewUsers)
#       }
#       userBalance <- userBalance[order(userBalance$Freq),]
#       theLowRank <- userBalance$Freq[1]
#       userBalanceLowRank <- userBalance[userBalance$Freq == theLowRank,]
#     
#       if( nrow(userBalanceLowRank) > 1 ){
#       
#         #   ---- We have a tie.  Choose one randomly, formatting as we go. Don't choose
#         #   ---- the userID!
#       theOptions <- userBalance[userBalance$Freq == theLowRank,]
#       theOptionsDF <- data.frame(theOptions=unique(c(theOptions$digiPrimary,theOptions$digiSecondary)))
#       theOptionsDF$RUnif <- runif(nrow(theOptionsDF))
#       thePartnerAssign <- theOptionsDF[theOptionsDF$theOptions != userID,]$theOptions[1]
#       
#       } else {
#       
#         #   ---- No tie.  Make sure we don't choose the userID, and format as well.  
#         theOptions <- userBalance[1,]
#         theOptionsVec <- unique(c(theOptions$digiPrimary,theOptions$digiSecondary))
#         thePartnerAssign <- theOptionsVec[theOptionsVec != userID]
#       }
#     }
#    
#    
#   assignInfo <- data.frame(userID=userID,
#                            primaryProp=primaryProp,
#                            secondaryProp=secondaryProp,
#                            userIDAssign=userIDAssign,
#                            theLowRank=theLowRank,
#                            thePartnerAssign=thePartnerAssign)

  return(assignInfo)
  }