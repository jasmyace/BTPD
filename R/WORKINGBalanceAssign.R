
balanceAssign <- function(userID){

  #   ---- Get the information up to this point.  
  assign <- getCellStatus()
  
  #   ---- Restrict to the doubly assigned.  We use these to get frequencies
  #   ---- of what's been done so far.  
  doubles <- assign[ !is.na(assign$digiDouble) & assign$digiDouble == 1,]
  
  #   ---- Identify the unique users up to this point.  
  theUsers <- unique(c(doubles$digiPrimary,doubles$digiSecondary))
  
  #   ---- Identify the options of assigning a partner.  
  tblNames <- checkUser(userID)
  tblNames[tblNames$doubleActive == 1 & tblNames$userID != userID,]

  if( userID %in% theUsers ){
    

# #   ---- Example.
# r1 <- c(219,100)
# r2 <- c(219,100)
# r3 <- c(219,100)
# r4 <- c(219,100)
# r5 <- c(100,219)
# r6 <- c(100,219)
# r7 <- c(100,219)
# r8 <- c(100,655)
# r9 <- c(100,655)
# r10 <- c(655,100)
# r11 <- c(100,655)
# r12 <- c(100,655)
# r13 <- c(219,655)
# r14 <- c(655,219)
# r15 <- c(655,219)
# 
# dat <- data.frame(rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15))
# colnames(dat) <- c('digiPrimary','digiSecondary')
# rownames(dat) <- NULL
# doubles <- dat

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
    primaryProp <- sum(assignBalance[assignBalance$Upper == 1,]$Freq) / sum(assignBalance$Freq)
    secondaryProp <- sum(assignBalance[assignBalance$Lower == 1,]$Freq) / sum(assignBalance$Freq)
    
    #   ---- Given the proportion of primary and secondary, assign the userID to be 
    #   ---- the type of digitizer with less.  This helps balance out that userID's 
    #   ---- assignments, given his/her assignments so far.  
    userIDAssign <- ifelse(primaryProp >= secondaryProp,'Secondary','Primary')
    
    if(userIDAssign == "Secondary"){
      
      #   ---- Reduce to the lower triangular matrix.
      userBalance <- assignBalance[assignBalance$Lower == 1,]
    } else {
      
      #   ---- Reduce to the upper triangular matrix.  
      userBalance <- assignBalance[assignBalance$Upper == 1,]
    }
  
    #   ---- Now, the idea is make the partner with the less number of pairings with 
    #   ---- the userID the partner.  But, we could have a tie. Who do we choose then?
    #   ---- Let's just keep things simple and choose randomly.  We could look at other
    #   ---- frequency tables, and choose the person with the least examined cells
    #   ---- overall, etc.  
    userBalance <- userBalance[order(userBalance$Freq),]
    theLowRank <- userBalance$Freq[1]
    userBalanceLowRank <- userBalance[userBalance$Freq == theLowRank,]
    
    if( nrow(userBalanceLowRank) > 1 ){
      
      #   ---- We have a tie.  Choose one randomly, formatting as we go. Don't choose
      #   ---- the userID!
      theOptions <- userBalance[userBalance$Freq == theLowRank,]
      theOptions$digiPrimary <-  as.numeric(levels(theOptions$digiPrimary))[theOptions$digiPrimary]
      theOptions$digiSecondary <-  as.numeric(levels(theOptions$digiSecondary))[theOptions$digiSecondary]
      theOptionsDF <- data.frame(theOptions=unique(c(theOptions$digiPrimary,theOptions$digiSecondary)))
      theOptionsDF$RUnif <- runif(nrow(theOptionsDF))
      thePartnerAssign <- theOptionsDF[theOptionsDF$theOptions != userID,]$theOptions[1]
      
    } else {
      
      #   ---- No tie.  Make sure we don't choose the userID, and format as well.  
      theOptions <- userBalance[1,]
      theOptions$digiPrimary <-  as.numeric(levels(theOptions$digiPrimary))[theOptions$digiPrimary]
      theOptions$digiSecondary <-  as.numeric(levels(theOptions$digiSecondary))[theOptions$digiSecondary]
      theOptionsVec <- unique(c(theOptions$digiPrimary,theOptions$digiSecondary))
      thePartnerAssign <- theOptionsVec[theOptionsVec != userID]
    }
  } else {
    
    #   ---- This is a new user not in the database.  Just put them in as secondary,
    #   ---- assign person with least number of digitized cells active as primary. 
    primaryProp <- secondaryProp <- 0
    userIDAssign <- "Secondary"
    theOptions <- as.data.frame(table(doubles$digiPrimary))
    thePartnerAssign <- theOptions[order(theOptions$Freq),]$Var1[1]
    thePartnerAssign <- as.numeric(levels(thePartnerAssign))[thePartnerAssign]
    theLowRank <- theOptions[order(theOptions$Freq),]$Freq[1]
  }
   
  assignInfo <- c(userID,primaryProp,secondaryProp,userIDAssign,theLowRank,thePartnerAssign)
  names(assignInfo) <- c("userID","primaryProp","secondaryProp","userIDAssign","theLowRank","thePartnerAssign")
  
  return(assignInfo)
  }