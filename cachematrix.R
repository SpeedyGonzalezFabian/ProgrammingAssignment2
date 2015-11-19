##Two functions to create a special matrix object that can cache its inverse (makeCacheMatrix)
##and that retrieve the inverse of a matrix if it has been cached already or calculate it and then cache it (solveCache)


##function creating a special matrix that can cache its inverse
##note: this also works of "a" is a numeric vector that can be turned into an invertible matrix
##input: a = matrix or numeric vector; rows = number of rows and columns of matrix, if a is vector

makeCacheMatrix <- function(a ,rows = 2) {
        #set "inv" to NULL, because new matrix was created
        inv <- NULL
        
        #check whether matrix or vector was given
        if (is.matrix(a)){
                x<-a
        } else {
                #if "a" is not a matrix, turn it into matrix with number of rows and columns = "rows"
                x<-matrix(a,rows,rows)
        }
        
        #create function to re-set matrix, either by giving new matrix or numeric vector and number of rows and columns
        set <- function(y, rows) {
                if (is.matrix(y)){
                        x<<-y
                } else {
                        x<<-matrix(y,rows,rows)
                }
                #set "inv" to NULL, because new matrix was created
                inv <<- NULL
        }
        
        #create function to retrieve matrix "x"
        get <- function() x
        
        #create function to set "inv" to "inverse"
        setinv <- function(inverse) inv <<- inverse
        
        #create function to retrieve "inv"
        getinv <- function() inv
        
        #return list of above specified functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



##function calculating the inverse of a matrix and caching its inverse
##input: x = matrix created with function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        #set "inv" to existing inverse with function getinv
        inv <- x$getinv()
        
        #check whether inverse has been calculated (i.e. is not NULL)
        if(!is.null(inv)) {
                #if inverse has been calculated, print message and "inv"
                message("getting cached data")
                return(inv)
        }
        
        #if inverse has not been calculated, get matrix
        data <- x$get()
        
        #set "inv" to inverse of matrix by using function solve
        inv <- solve(data, ...)
        
        #cache "inv" with function setinv
        x$setinv(inv)
        
        #print "inv"
        inv
}
