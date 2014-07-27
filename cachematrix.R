#Programming Assignment 2: Lexical Scoping

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        ##set the value of matrix
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        ##get the value of matrix
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        
        ##final cached list
        list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
                
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##get inverse matric
        matrx<-x$getinverse()
        if(!is.null(matrx)) {
                message("getting cached matrix")
                
                ##return the matrix
                return(matrx)
        }
        
        ##if no inverse, get the actual matrix
        dat<-x$get()
        
        ##use the solve function to calculate the inverse
        ##Computing the inverse of a square matrix can be done with the solve function in R
        matrx<-solve(dat, ...)
        
        ##Set inverse back to cache
        x$setinverse(matrx)
        matrx
}

##THE END
