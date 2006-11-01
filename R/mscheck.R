mscheck<-function(a)
{
  repeat{
  if ( a[2,2] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  a<-edit(a)
  }
 else{
   break
   return(edit(a))
 }
}
  cat("\n")
  show(a)
}

ymscheck<-function(a)
{
  repeat{
  if ( a[2,7] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  a<-edit(a)
  }
 else{
   break
   return(edit(a))
 }
}
  cat("\n")
  show(a)
}

zmscheck<-function(a)
{
  repeat{
  if ( a[2,5] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  a<-edit(a)
  }
 else{
   break
   return(edit(a))
 }
}
  cat("\n")
  show(a)
}

wmscheck<-function(a)
{
  repeat{
  if ( a[2,4] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  a<-edit(a)
  }
 else{
   break
   return(edit(a))
 }
}
  cat("\n")
  show(a)
}