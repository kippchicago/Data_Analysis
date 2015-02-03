# The following function performs CPS's Fall to prior SY spring 
# RIT score equating

cps_equate_scalar <- function(rit_score, subject, grade){
  require(assertthat)

  
  inner <- function(rit_score, subject, grade) {
    #assert_that(subject %in% c("Mathematics", "Reading"))
    if(grade<=2) return(rit_score)
    grade<-as.character(grade)
    if(subject=="Mathematics"){
      alpha <- 24.72433 +  
        switch(grade,
               "3" = -2.11454,
               "4" = -1.44296,
               "5" = -0.43114,
               "6" = 0.94745,
               "7" = 0.235,
               "8" = 0)
      beta <- 0.89331
      
      new_rit <- alpha + beta*rit_score
    } else {
      alpha <- 40.12476 +  
        switch(grade,
               "3" = -4.89886,
               "4" = -4.24598,
               "5" = -1.81953,
               "6" = -0.34371,
               "7" = -0.87584,
               "8" = 0)
      beta <- 0.82178
      new_rit <- alpha + beta*rit_score
    }
    new_rit
  }
  outer <- Vectorize(inner)
  
  outer(rit_score, subject, grade)
}

cps_equate <- Vectorize(cps_equate_scalar)
