## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        j<-NULL                           ##initialise inv as NULL ; will hold value of matrix invese
        set<-function(y){                 ## define the set function to assign new
        x<<-y                             ##value of matrix  in parent environment
         j<<-NULL                         ## if there is a new matrix , reset j to NULL
        }
       get <-function()x                                    ##define the get function- return value of the matrix argument
        setInverse <- function(inverse) j <<-inverse        ## assigns value of inv in parent environment.
        getInverse <- function() j                          ## gets the value of j where called
        list(set = set ,get=get , setInverse=setInverse , getInverse = getInverse)  ##you need this in orderto refer
                                                                                    ##to the function with the $ operation
}


## Write a short comment describing this function
## Thisfunction computes the inverse of thespecial  "matrix" returned by makeCaheMatrix above.
##if the inverse hasalready been calculated(abdthematrix has not changed),
## then cacheSolve will retrive the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
j<- x$getInverse()
        if(!is.null(j)){
        message("getting cached data")
        return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}

