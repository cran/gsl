"legendre_P1" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("legendre_P1_e",
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

"legendre_P2" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("legendre_P2_e",
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

"legendre_P3" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("legendre_P3_e",
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

"legendre_Pl" <- function(l, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(l,x)
  l.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("legendre_Pl_e",
           as.integer(l.vec),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }  
}
  
"legendre_Pl_array" <- function(lmax,x,give=FALSE,strict=TRUE){
  if(length(lmax)>1){stop("lmax should be of length 1")}
  lmax.single <- lmax
  x.vec <- as.vector(x)
  x.out <- rep(x.vec,(lmax+1))
  jj <- .C("legendre_Pl_array",
           as.integer(lmax.single),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.out),
           status=as.integer(x.vec),
           PACKAGE="gsl"
           )

  val <- jj$val
  dim(val) <- c(lmax.single+1 , length(x.vec))
  status <- jj$status
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,status=status))
  } else {
    return(val)
  }  
}  

"legendre_Q0" <- function(x,give=FALSE,strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("legendre_Q0_e",
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


"legendre_Q1" <- function(x,give=FALSE,strict=TRUE){
  x.vec <- as.vector(x)

  jj <- .C("legendre_Q1_e",
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


"legendre_Ql" <- function(l, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(l,x)
  l.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("legendre_Ql_e",
           as.integer(l.vec),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

  if(strict){
    val <- strictify(val,status)
  }

  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }  
}  

"legendre_Plm" <- function(l, m, x, give=FALSE, strict=TRUE){
  jj <- process.3.args(l,m,x)
  l.vec <- jj$arg1
  m.vec <- jj$arg2
  x.vec <- jj$arg3
  attr <- jj$attr


  jj <- .C("legendre_Plm_e",
           as.integer(l.vec),
           as.integer(m.vec),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

    if(strict){
    val <- strictify(val,status)
  }


  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }  
}  

"legendre_Plm_array" <- function(lmax,m,x,give=FALSE,strict=TRUE){
  if(length(lmax)>1){stop("lmax should be of length 1")}
  if(length(m)>1){stop("m should be of length 1")}
  lmax.single <- lmax
  m.single <- m
  x.vec <- as.vector(x)
  x.out <- rep(x.vec,(lmax-m.single+1))
  jj <- .C("legendre_Plm_array",
           as.integer(lmax.single),
           as.integer(m.single),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.out),
           status=as.integer(x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  dim(val) <- c(lmax.single-m.single+1 , length(x.vec))
  status <- jj$status
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,status=status))
  } else {
    return(val)
  }  
}  

"legendre_sphPlm" <- function(l,m,x,give=FALSE,strict=TRUE){
  jj <- process.3.args(l,m,x)
  l.vec <- jj$arg1
  m.vec <- jj$arg2
  x.vec <- jj$arg3
  attr <- jj$attr

  jj <- .C("legendre_sphPlm_e",
           as.integer(l.vec),
           as.integer(m.vec),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }  
}


"legendre_sphPlm_array" <- function(lmax,m,x,give=FALSE,strict=TRUE){
  if(length(lmax)>1){stop("lmax should be of length 1")}
  if(length(m)>1){stop("m should be of length 1")}
  lmax.single <- lmax
  m.single <- m
  x.vec <- as.vector(x)
  x.out <- rep(x.vec,(lmax.single - m.single +1))
  jj <- .C("legendre_sphPlm_array",
           as.integer(lmax.single),
           as.integer(m.single),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.out),
           status=as.integer(x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  dim(val) <- c(lmax.single-m.single+1 , length(x.vec))
  status <- jj$status
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
    return(list(val=val,status=status))
  } else {
    return(val)
  }  
}  

