"elljac" <- function(u, m, give=FALSE, strict=TRUE){
  jj <- process.args(u, m)
  u.vec <- jj$arg1
  m.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("elljac_e",
           as.double(u.vec),
           as.double(m.vec),
           as.integer(length(u.vec)),
           sn=as.double(u.vec),
           cn=as.double(u.vec),
           dn=as.double(u.vec),
           status=as.integer(0*u.vec),
           PACKAGE="gsl"
           )

  sn <- jj$sn
  cn <- jj$cn
  dn <- jj$dn
  attributes(sn) <- attr
  attributes(cn) <- attr
  attributes(dn) <- attr

  status <- jj$status
  attributes(status) <- attr
  
  if(strict){
    sn <- strictify(sn,status)
    cn <- strictify(cn,status)
    dn <- strictify(dn,status)
  }
  
  if(give){
      return(list(list(sn=sn,cn=cn,dn=dn),status=status))
  } else {
    return(list(sn=sn,cn=cn,dn=dn))
  }
}

"sn" <- function(z,m){
  jj.r <- elljac(Re(z),m)
  s <- jj.r$sn
  c <- jj.r$cn
  d <- jj.r$dn
  if(is.complex(z)){
    jj.i <- elljac(Im(z),1-m)
    s1 <- jj.i$sn
    c1 <- jj.i$cn
    d1 <- jj.i$dn
    return(
           (s*d1+1i*c*d*s1*c1)/(c1^2+m*s^2*s1^2)
           )
  } else {
    return(s)
  }
}


"cn" <- function(z,m){
  jj.r <- elljac(Re(z),m)
  s <- jj.r$sn
  c <- jj.r$cn
  d <- jj.r$dn
  if(is.complex(z)){
    jj.i <- elljac(Im(z),1-m)
    s1 <- jj.i$sn
    c1 <- jj.i$cn
    d1 <- jj.i$dn
    
    return(
           (c*c1-1i*s*d*s1*d1)/(c1^2+m*s^2*s1^2)
           )
  } else {
    return(c)
  }
}

"dn" <- function(z,m){
  jj.r <- elljac(Re(z),m)
  s <- jj.r$sn
  c <- jj.r$cn
  d <- jj.r$dn
  if(is.complex(z)){
    jj.i <- elljac(Im(z),1-m)
    s1 <- jj.i$sn
    c1 <- jj.i$cn
    d1 <- jj.i$dn
    
    return(
           (d*c1*d1-1i*m*s*c*s1)/(c1^2+m*s^2*s1^2)
           )
  } else {
    return(d)
  }
}

ns <- function(z,m){1/sn(z,m)}
nc <- function(z,m){1/cn(z,m)}
nd <- function(z,m){1/dn(z,m)}

sc <- function(z,m){sn(z,m)/cn(z,m)}
sd <- function(z,m){sn(z,m)/dn(z,m)}

cs <- function(z,m){cn(z,m)/sn(z,m)}
cd <- function(z,m){cn(z,m)/dn(z,m)}

ds <- function(z,m){dn(z,m)/sn(z,m)}
dc <- function(z,m){dn(z,m)/cn(z,m)}


  
