## This function returns back a list of the functions - getlocaldata() , set(), get(), getinverse() and setinverse() 
#Since the assignment requires us to also check if the current matrix on which the inverse is to be done, is the same as the previous matrix
#on which the inverse was performed, i have saved the previous matrix and its inverse in a global variable and have compared it with the current matrix 
#of which the inverse is required.

##the getlocaldata() - returns back the current matrix passed as the variable in this function
##the set() - assigns the matrix value to the global variable "globaldata" for future reference
##the get() - returns the value of the previous matrix for which the inverse was computed from "globaldata"
##the setinverse() - assigns the value of the inverse to the global variable "globalinverse" for future reference
##the getinverse() - returns the value of the inverse as stored previously in "globalinverse"

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix(),...) {
  inverse <- NULL  								#local variable "inverse" being declared null					
  e <- globalenv()								#"e" is the global environment where the old matrix and its calculated inverse is stored
  getlocaldata <- function(){						#function for returning the current matrix for which the inverse is to be done
    return(x)								#returning the current matrix passed from the function argument named "data
  }
  set <- function(y) {							#function for setting the current matrix into the global environment
    x <<- y									#assigning the value to the local variable "data"
    assign("globaldata", x, envir = e)		#assigning the local var "data" to the global variable "globaldata" for future reference
    assign("globalinverse", NULL, envir = e)	#assigning NULL to the global variable "globalinverse" as this is yet to be calculated	
  }
  get <- function() {								#function for getting the old matrix from the global variable "globaldata
    if(!is.null(e[["globaldata"]]))				#if the value retrieved from the globaldata is not null
      x <- e$globaldata						#put the value from "globaldata" to the local variable "data" 
    else										#if it is null
      x <- NULL								#assign NULL to "data"
    x										#return "data" either contaning the "globaldata" value or NULL
  }
  setinverse <- function(inv) {					#function for setting the calculated inverse to the global variable "globalinverse"
    assign("globalinverse", inv, envir = e)		#assign the calculated value to "globalinverse" for future reference
  }
  getinverse <- function() {						#function for getting the already calculated inverse matrix from the global var "globalinverse"
    if(!is.null(e[["globalinverse"]]))			#if the value retrieved from "globalinverse" is not null
      inverse <- e$globalinverse					#set the local varibale "inverse" with the value from cache "globalinverse"
    else inverse <- NULL						#if the value from "globalinverse" is null set "inverse" to NULL
    inverse
    }									#return back "inverse" with the value retrieved from cache
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, getlocaldata = getlocaldata) #returning a list of all the functions created
}


## this function takes as an argument the function call to the function "makeCacheMatrix"
##this function first gets the previous matrix from the global variable "globaldata" and performs a check to see 
#if the current matrix is the same as the previous one. If so, it gets and returns back the inverse already stored
# in the global variable "globalinverse", otherwise if the matrices are not same, an inverse is calculated for the 
# current matrix using solve and returned back.The matrix and its inverse are stored in the global variables for
# future reference.

cacheSolve <- function(fun, ...) {
        
  m <- fun$get()   					# getting the old matrix from cached global environment
  mat <- fun$getlocaldata() 			# getting the current matrix for being calculated
  
  if(!is.null(m)){    				#checking if the old matrix retrieved from cache is null or not
    if(identical(m,mat)) {				#checking if the ols matrix is identical to the new one
      inv <- fun$getinverse()				#if identical, getting the already calculated inverse from cache
      
      if(!is.null(inv)) {					#checking if the inverse retrieved is null or not
        message("getting inverse from cache")
        return (inv)					#returning the already calculated inverse from cache
      }
      else{								#if the obtained inverse from cache is null
        inv <- solve(mat)				#using "solve" to calculate the inverse
        fun$set(mat)					#setting the matrix into global environment for future use
        fun$setinverse(inv)				#setting this calculated inverse into global environment for future use
        message("getting inverse from identical matrix but no inversedata available from cache")
        return(fun$getinverse())		#returning the calculated value of the inverse
      }
    }
    else {								#if the current matrix is a new one (not identical to the preious one) 
      message("new matrix found")	
      fun$set(mat)					#setting this new matrix in the global environment for future use
      inv <- solve(mat)				#calculating the inverse using the "solve" function
      fun$setinverse(inv)				#setting the calculated inverse into global environment for future use
      message("calculating and storing inverse for this new matrix")
      return(fun$getinverse())		#returning the calculated inverse
    }
  }
  else {								#if the old matrix as retrieved from the global environment is null 
    message("inside data from cache is null")
    fun$set(mat)					#setting this new matrix in the global environment for future use 
    inv <- solve(mat)				#calculating the inverse using the "solve" function
    fun$setinverse(inv)				#setting the calculated inverse into global environment for future use
    message("getting inverse from unidentical matrix")
    return(fun$getinverse())		#returning the calculated inverse
  }
  
}
