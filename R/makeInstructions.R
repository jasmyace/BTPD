#' @export makeInstructions
#'   
#' @title Make an instruction text file.

makeInstructions <- function(userID,out){
  
  
  
  #   ---- Get information from our helping files.
  theNext <- out[[2]]#read.dbf(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/theNext.dbf"),as.is=TRUE)
  theRange <- out[[3]]#read.dbf(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/theRange.dbf"),as.is=TRUE)
  thepFirstName <- out[[4]]#read.dbf(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/thepFirstName.dbf"),as.is=TRUE)
  thesFirstName <- out[[5]]#read.dbf(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/thesFirstName.dbf"),as.is=TRUE)
  
  theWinPath <- "\\\\LAR-FILE-SRV\\Data\\BTPD_2016\\Digitizing"
  thePath <- "//LAR-FILE-SRV/Data/BTPD_2016/Digitizing"
  
  pasteString <- paste0(thePath,"/",theRange,"/",theNext,"/Instructions for Digitizing Cell ",theNext,".txt")
  
  if(thesFirstName == 'No'){
    
    #   ---- What to write if a singly digitized cell.  
    
    #   ---- If there happens to be a text file already, delete it
    #   ---- out.  Otherwise, new stuff is just added at the bottom
    #   ---- of the old.  
    if(file.exists(pasteString)){
      file.remove(pasteString)
    } 
    
    #   ---- Start making a text file.  
    options(useFancyQuotes=FALSE)
    log_con <- file(pasteString,open="a")
    
cat(paste0("This text file contains the instructions for digitizing cell ",theNext,".\n\n"),sep="",append=TRUE,file=log_con)  
  
cat(paste0("This cell is a singly digitized cell assigned to ",thepFirstName,".\n\n"),sep="",append=TRUE,file=log_con)
cat(paste0("So, ",thepFirstName," will digitize this cell alone.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("While consultation with other digitizers is allowed, referencing GoogleEarth and other\n"),sep="",append=TRUE,file=log_con)
cat(paste0("external imagery is prohibited.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("The files for this cell can always be found in folder\n"),sep="",append=TRUE,file=log_con)
cat(paste0(theWinPath,"\\",theRange,"\\",theNext,".\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("Steps to digitize this singly digitized cell include the following:\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  1. Open the .mxd file entitled p",paste0(thepFirstName,"_",theNext),".mxd, and add the shapefiles \n"),sep="",append=TRUE,file=log_con)
cat(paste0("     entitled LocalGrid_",theNext,", LocalMiniGrid_",theNext,", and possibly shapefile\n"),sep="",append=TRUE,file=log_con)
cat(paste0("     LocalTowns_",theNext," containing already digitized features local to cell ",theNext,".\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  2. Also add shapefile p",paste0(thepFirstName,"_",theNext),", in which you will digitize.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  3. When done digitizing, save your changes.  If you found no towns, skip to step 6.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  4. Now, check for digitizing issues. Submit the following to the R Console window.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("               checkCellValidity('p",thepFirstName,"_Towns_",theNext,"')\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  5. If the above returns any issues, fix and run again. Repeat until no errors remain.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  6. At this point, digitizing of this cell is complete. Check in this cell before \n"),sep="",append=TRUE,file=log_con)
cat(paste0("     obtaining your next.  To do that, submit the following to the R Console window.\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("               checkInCell('",theNext,"',",userID,")\n\n"),sep="",append=TRUE,file=log_con)

cat(paste0("  7. This concludes the digitizing process for this cell.  Use R function\n\n"),sep="",append=TRUE,file=log_con)  
cat(paste0("               checkOutCell(",userID,")\n\n"),sep="",append=TRUE,file=log_con)
cat(paste0("     to start the process anew with a different cell."),sep="",append=TRUE,file=log_con)  
close(log_con)
options(useFancyQuotes=TRUE)
  } else {
  
  #   ---- What to write if a doubly digitized cell.  
  
    #   ---- If there happens to be a text file already, delete it
    #   ---- out.  Otherwise, new stuff is just added at the bottom
    #   ---- of the old.  
    if(file.exists(pasteString)){
      file.remove(pasteString)
    } 
  
  #   ---- Start making a text file.  
  options(useFancyQuotes=FALSE)
  log_con <- file(pasteString,open="a")
  
  cat(paste0("This text file contains the instructions for digitizing cell ",theNext,".\n\n"),sep="",append=TRUE,file=log_con)  
  
  cat(paste0("This cell is a doubly digitized cell assigned to ",thepFirstName," and ",thesFirstName,".\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("So, ",thepFirstName," and ",thesFirstName," will each first independently digitize this cell alone, after\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("which, they will then reconcile their changes.  In the end then, there will be three\n"),sep="",append=TRUE,file=log_con) 
  cat(paste0("shapefiles digitized; one from each of the digitizers, and then a third containing\n"),sep="",append=TRUE,file=log_con)  
  cat(paste0("reconciliations.\n\n"),sep="",append=TRUE,file=log_con) 

  cat(paste0(thepFirstName," is the primary, and ",thesFirstName," is the secondary.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("While consultation with other digitizers is allowed, referencing GoogleEarth and\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("other external imagery is prohibited.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("The files for this cell can always be found in folder\n"),sep="",append=TRUE,file=log_con)
  cat(paste0(theWinPath,"\\",theRange,"\\",theNext,".\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("Steps to digitize this doubly digitized cell include the following. Note that each\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("of the two digitizers both independently perform this first set of steps.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  1. Open up the .mxd file tied to your name.  Note that the primary file has a 'p'\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     prefix, while the secondary has an 's' prefix.  These two files are entitled\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     p",paste0(thepFirstName,"_",theNext)," and s",paste0(thesFirstName,"_",theNext),", respectively.  Add the shapefiles\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     entitled LocalGrid_",theNext,", LocalMiniGrid_",theNext,", and possibly shapefile\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     LocalTowns_",theNext," containing already digitized features local to cell ",theNext,".\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  2. Also add your eponoymous shapefile, in which you will digitize.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  3. When done digitizing, save changes. If neither digitizer found any towns, and \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     after consultation with each other, both still agree no towns are present, run \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     the function in step 6, followed by the check-in function in step 9.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  4. Now, check for digitizing issues. Submit the following to the R Console window.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("     ------------------------- ",toupper(thepFirstName),": Use this function. -------------------------------\n\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("               checkCellValidity('p",thepFirstName,"_Towns_",theNext,"')\n\n"),sep="",append=TRUE,file=log_con)

  cat(paste0("     ------------------------- ",toupper(thesFirstName),": Use this function. ---------------------------------\n\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("               checkCellValidity('s",thesFirstName,"_Towns_",theNext,"')\n\n"),sep="",append=TRUE,file=log_con)

  cat(paste0("  5. If the above returns any issues, fix and run again. Repeat until no errors remain.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  6. At this point, both ",thepFirstName," and ",thesFirstName," are done with their initial digitizing.  Now,\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     create a reconciling .mxd file.  To do this, only one of either ",thepFirstName," or ",thesFirstName," need\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     submit the following to the R Console window.\n\n"),sep="",append=TRUE,file=log_con)

  cat(paste0("               setUpReconcile('",theNext,"')\n\n"),sep="",append=TRUE,file=log_con)

  cat(paste0("  7. Now, open new file reconciling_",theNext," and add in all the same shapefiles as before, \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     this time adding in the new reconciling_Towns_",theNext," shapefile, along with the \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     original digitized ones created by ",thepFirstName," and ",thesFirstName,".  Remember that primary \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     digitizer ",thepFirstName," breaks all ties.\n\n"),sep="",append=TRUE,file=log_con)

  cat(paste0("  8. When complete, check for town digitizing errors in the new reconciling shapefile.\n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     Submit the following to the R Console window.  Resubmit until no errors remain.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("               checkCellValidity('reconciling_",theNext,"')\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("  9. At this point, digitizing of this cell is complete. Check in this cell before \n"),sep="",append=TRUE,file=log_con)
  cat(paste0("     obtaining your next.  To do that, submit the following to the R Console window.\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0("               checkInCell('",theNext,"',",userID,")\n\n"),sep="",append=TRUE,file=log_con)
  
  cat(paste0(" 10. This concludes the digitizing process for this cell.  Use R function 'checkOutCell' \n"),sep="",append=TRUE,file=log_con)  
  cat(paste0("     with your userID to start the process anew with a different cell."),sep="",append=TRUE,file=log_con)  
  close(log_con)
  options(useFancyQuotes=TRUE)
 
  }
  
  #   ---- Communicate to the checker-outer that he/she has a partner.  
  if( thesFirstName != 'No' ){
    cat(paste0("This is a doubly sampled cell.  Your partner is ",thesFirstName,".\n"))
  }
  
  #   ---- Open the folder for the user.  
  opendir <- function(dir){
    if (.Platform['OS.type'] == "windows"){
      shell.exec(dir)
    } else {
      system(paste(Sys.getenv("R_BROWSER"), dir))
    }
  }
  
  opendir(paste0("L:/BTPD_2016/Digitizing/",theRange,"/",theNext))
  cat(paste0("The digitizing folder can be found at L:/BTPD_2016/Digitizing/",theRange,"/",theNext,".\n"))
  
}