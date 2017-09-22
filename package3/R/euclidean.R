#' Euclidean algorithm for GCD.
#' 
#' The function returns the greatest common divisor (GCD) of two numbers.
#' @param x numeric scalars of integers.
#' @param y numeric scalars of integers.
#'
#' @return If input data is numeric or integer then 
#' the output will be a integer otheriwise it will return an error.
#' @export
#' @references \href{https://en.wikipedia.org/wiki/Euclideanalgorithm}{Euclideanalgorithm}
#'
#' @examples euclidean(100, 1000)
#' 

euclidean<- function(x,y){
  x<-abs(x)
  y<-abs(y)

  data <- c(x,y)
              
           if(x>y){
                 a<-x
                 b<-y}
               else {
                 a<-y
                 b<-x}

if(is.numeric(data))
  {if((floor(data[1])==data[1]) && (floor(data[2])==data[2]))
     {
    flag=FALSE
    while(flag==FALSE){
      if(a==0 || b==0)
        {
          if(a==0)
          {
            GCD<-b
            flag<-TRUE
          }
          else (b==0)
          {
            GCD<-a
            flag<-TRUE
          }
        }
      
     else
       {
           c<-a%%b
           d<-a/b
           a<-b
           b<-c
        }
  }
    return(GCD)
  }
  else
    stop("Data is not integer ! \n", call. = FALSE)
    
  }
 else 
   stop("Data is not numeric ! \n",call. = FALSE)
}

