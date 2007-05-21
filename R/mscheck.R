# check if the user correctly enters initial values for PK parameters ([2,2] can't be NA )
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

# To check if the user correctly enters initial values for PK parameters ([2,7] can't be NA )
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

# To check if the user correctly enters initial values for PK parameters ([2,5] can't be NA )
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

# To check if the user correctly enters initial values for PK parameters ([2,4] can't be NA )
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