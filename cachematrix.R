## The following functions find the inverse of a matrix.
## makeCacheMatrix stores the result of the inverse in the cache
## cacheSolve evaluates the inverse, but looks to see if it
## has already been evaluated and stored in the cache.
## On the first call to cacheSolve the inverse needs to be evaluated
## but in subsequent calls it is retrieved from the cache.


#A function to make a matrix and cache its output.
#Adapted from the example makeVector.
makeCacheMatrix <- function(x = matrix()) { #Takes a matrix as input

    invx <- NULL # Set the result of the matrix inverse to NULL

    #Now make the set function
    #Use the <<- assignment operator so that variables are defined
    #locally in the function call and not set in the global environment.
    set <- function(y){
        x <<- y
        invx <<- NULL
    }
  
    get <- function() x                   #Return the input matrix
    setInv <- function(inv) invx <<- inv  #Set the inverse
    getInv <-function() invx          #Get the inverse

    #As in makeVector, pass all these to a list to return
    list (set = set,
          get = get,
          setInv = setInv,
          getInv = getInv)
}

#Function to evaluate the inverse of a matrix
#Looks to see if inverse is stored in cache first.
#If it is, uses it, if not then evaluates it.
#Adapted from cachemean above.
cacheSolve <- function(x, ...) {
    m <- x$getInv() # get the inverse of the matrix
    #If the inverse has been cached, get it
    if(!is.null(m)) {           
        message("getting cached data")
        return(m) #Return the calculated inversion
    }
    #If not, get the matrix and evaluate the inverse
    data <- x$get()
    m <- solve(data) #Use "solve" from base package to find inverse.
    x$setInv(m)      #The set the inverse to x.
    m                #Return the inverse
}

