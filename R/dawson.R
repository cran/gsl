"dawson" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("dawson",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

