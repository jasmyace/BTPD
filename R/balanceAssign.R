
balanceAssign <- function(userID,assign,tblNames){
  
  #   ---- Restrict to the doubly assigned.  We use these to get frequencies
  #   ---- of what's been done so far.  
  doubles <- assign[ !is.na(assign$digiDouble) & assign$digiDouble == 1,]
 
  #   ---- Identify the unique users up to this point.  
  theUsers <- unique(c(doubles$digiPrimary,doubles$digiSecondary))
 
  #   ---- Identify the options of assigning a partner.  
  # tblNames <- checkUser(userID)
  theDoublyActives <- c(tblNames[tblNames$doubleActive == 1 & tblNames$userID != userID,]$userID)

  if( userID %in% theUsers ){
    
    #   ---- Count up the unique instances of users.  Call it 'Dim' since it forms
    #   ---- a square matrix, c.f. the table called below, prior to being pushed 
    #   ---- into a data frame.  
    Dim <- length(theUsers)
    
    #   ---- Pretty up the list output from the table.  
    balance <- as.data.frame(table(doubles$digiPrimary,doubles$digiSecondary))
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
    primaryProp <- sum(assignBalance[assignBalance$digiPrimary == userID,]$Freq) / sum(assignBalance$Freq)
    secondaryProp <- sum(assignBalance[assignBalance$digiSecondary == userID,]$Freq) / sum(assignBalance$Freq)
    
    #   ---- Given the proportion of primary and secondary, assign the userID to be 
    #   ---- the type of digitizer with less.  This helps balance out that userID's 
    #   ---- assignments, given his/her assignments so far.  Note that I don't 
    #   ---- randomly assign in the case of a tie...I just assign those to secondary.
    #   ---- This is because I allow for the greater than or equal.  
    userIDAssign <- ifelse(primaryProp >= secondaryProp,'Secondary','Primary')
    
    if(userIDAssign == "Secondary"){
      
      #   ---- Reduce to the lower triangular matrix.
      userBalance <- assignBalance[assignBalance$Lower == 1,]
      
    } else {
      
      #   ---- Reduce to the upper triangular matrix.  
      userBalance <- assignBalance[assignBalance$Upper == 1,]
    }
  
    #   ---- Adjust factors levels to what they should be.  
    userBalance$digiPrimary <- as.numeric(levels(userBalance$digiPrimary))[userBalance$digiPrimary]
    userBalance$digiSecondary <- as.numeric(levels(userBalance$digiSecondary))[userBalance$digiSecondary]
    
    #   ---- Now, the idea is make the partner with the less number of pairings with 
    #   ---- the userID the partner.  But, we could have a tie. Who do we choose then?
    #   ---- Let's just keep things simple and choose randomly.  We could look at other
    #   ---- frequency tables, and choose the person with the least examined cells
    #   ---- overall, etc.  Finally, we must consider the possibility that there is a 
    #   ---- new userID in the list of doubleAssign that hasn't actually digitized a 
    #   ---- doubly cell yet.  This individual will not be in theUsers, but will be 
    #   ---- in theDoublyActives.  
    
    #   ---- Get theDoublyActives not in theUsers.
    theNewOnes <- theDoublyActives[!(theDoublyActives %in% theUsers)]
    
    #   ---- If we have a new user, add them to the list of possibilities.  
    if( length(theNewOnes) > 0){
      theNewUsers <- NULL
      for(i in 1:length(theNewOnes)){
        if(userIDAssign == "Secondary"){
          thisNewUser <- data.frame(digiPrimary=theNewOnes[i],digiSecondary=userID,Freq=0,Row=0,Col=0,Diag=0,Upper=0,Lower=0)
        } else {
          thisNewUser <- data.frame(digiPrimary=userID,digiSecondary=theNewOnes[i],Freq=0,Row=0,Col=0,Diag=0,Upper=0,Lower=0)
        }
        theNewUsers <- rbind(theNewUsers,thisNewUser)
      }
      userBalance <- rbind(userBalance,theNewUsers)
    }
    userBalance <- userBalance[order(userBalance$Freq),]
    theLowRank <- userBalance$Freq[1]
    userBalanceLowRank <- userBalance[userBalance$Freq == theLowRank,]
    
    if( nrow(userBalanceLowRank) > 1 ){
      
      #   ---- We have a tie.  Choose one randomly, formatting as we go. Don't choose
      #   ---- the userID!
      theOptions <- userBalance[userBalance$Freq == theLowRank,]
      theOptionsDF <- data.frame(theOptions=unique(c(theOptions$digiPrimary,theOptions$digiSecondary)))
      theOptionsDF$RUnif <- runif(nrow(theOptionsDF))
      thePartnerAssign <- theOptionsDF[theOptionsDF$theOptions != userID,]$theOptions[1]
      
    } else {
      
      #   ---- No tie.  Make sure we don't choose the userID, and format as well.  
      theOptions <- userBalance[1,]
      theOptionsVec <- unique(c(theOptions$digiPrimary,theOptions$digiSecondary))
      thePartnerAssign <- theOptionsVec[theOptionsVec != userID]
    }
  } else {
    
    #   ---- This is a new user not in the database.  Just put them in as secondary,
    #   ---- assign active doubly person with least number of digitized cells active 
    #   ---- as primary. 
    primaryProp <- secondaryProp <- 0
    userIDAssign <- "Secondary"
    theOptions <- as.data.frame(table(doubles$digiPrimary))
    
    #   ---- Reduce the options to those currently active in tblNames.  We should have
    #   ---- at least one because we check for at least the availability of two doubly 
    #   ---- digitizers before we enter the function.  
    theOptions <- theOptions[theOptions$Var1 %in% theDoublyActives,]
    thePartnerAssign <- theOptions[order(theOptions$Freq),]$Var1[1]
    thePartnerAssign <- as.numeric(levels(thePartnerAssign))[thePartnerAssign]
    theLowRank <- theOptions[order(theOptions$Freq),]$Freq[1]
  }
   
  assignInfo <- data.frame(userID=userID,
                           primaryProp=primaryProp,
                           secondaryProp=secondaryProp,
                           userIDAssign=userIDAssign,
                           theLowRank=theLowRank,
                           thePartnerAssign=thePartnerAssign)
  names(assignInfo) <- c("userID","primaryProp","secondaryProp","userIDAssign","theLowRank","thePartnerAssign")
  
  return(assignInfo)
  }