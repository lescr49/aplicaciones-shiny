############ Cargar los paquetes de R que se utilizarán #################

library(shiny)
library(shinythemes)
library(xtable)
############ FUNCION PARA CALCULAR EL RIESGO RELATIVO ####################
############ Y  MEDIDAS DE IMPACTO  ######################################

cohorte.ia<- function(a,b,c,d,decimal=2)
{    
  totexp <- a+b
  totnoexp<-c+d
  # probabilidades
  probnoexp<-(c+ d)/(a+b+c+d)
  probenf<-(a+c)/(a+b+c+d)
  probenfentreexp <- a/totexp
  probenfentrenoexp <- c/totnoexp
  probexp<-totexp/(totexp+totnoexp)
  # riesgo relativo
  riesgorelativo<-  probenfentreexp/ probenfentrenoexp
  # intervalo de confianza para el RR
  sigmarr<- sqrt((1/a) - (1/totexp) + (1/c) - (1/totnoexp))
  alpha=0.05
  z <- qnorm(1-(alpha/2))
  li <- round(riesgorelativo * exp(-z * sigmarr),3)
  ls <- round(riesgorelativo * exp( z * sigmarr),3)
  #print(paste(riesgorelativo,li,ls))
  
  # riesgo atribuible en expuestos
  RAE<-probenfentreexp-probenfentrenoexp
  sigmarae <- sqrt(probenfentreexp*(1-probenfentreexp)/
                     totexp+ probenfentrenoexp*(1-probenfentrenoexp) /
                     totnoexp)
  
  # sigmarae es el error estandar del RAE
  lirae <- RAE -z * sigmarae
  lsrae <- RAE + z * sigmarae
  
  #calculo del riesgo atribuible porcentual en expuestos RAEp
  RAEp<-(probenfentreexp-probenfentrenoexp)/probenfentreexp*100
  
  #resumen
  re<-probenfentreexp
  rne<- probenfentrenoexp
  RR<-riesgorelativo

 
  #calculo del NNT con IC
  NNT<-1/RAE
  lsnnt<-if(lirae<0) NA else round(1/lirae,2)
  linnt<-round(1/lsrae,2)
  
  lirae<-round(max(0,lirae),4)
  lsrae<-round(lsrae,4)
  
  #fraccion etilogica en expuestos con ic
  FEE<-((riesgorelativo-1)/riesgorelativo)
  
  phi<-probenfentrenoexp/ probenfentreexp
  sephi<-phi*sqrt((1-probenfentrenoexp)/(totnoexp*probenfentrenoexp)+(1-probenfentreexp)/(totexp*probenfentreexp))
  liFEE<-round(max(0,FEE-z*sephi),4)
  lsFEE<-round(min(FEE+z*sephi,1),4)
  
  #riesgo atribuible poblacional
  RAP<-probexp*(RR-1)/(probexp*(RR-1)+1)
  
  phi1<-1-RAP
  varphi1<-(phi1^2)*((1-c/(a+b+c+d))/c-(probnoexp+probenf-2*c/(a+b+c+d))/(a+b+c+d)*probnoexp*probenf)
  liRAP<-max(round(1-phi1-z*sqrt(varphi1),3),0)
  lsRAP<-round(min(1-phi1+z*sqrt(varphi1),1),3)
  
  #construccion de la tabla
  
  col1a<-c("Riesgo en expuestos", "Riesgo en no expuestos","Riesgo relativo (RR)")
  col1b<-c("Riesgo atribuible en expuestos (RAE)","Número de personas a tratar (NNT)")
  col1c<-c("Fracción etiológica en expuestos","Riesgo atribuible poblacional")
  col1<-c(col1a,col1b,col1c)
  col2<-round(c(re,rne,RR,RAE,NNT,FEE,RAP),4)
  col3<-c("","",li,lirae,linnt,liFEE,liRAP)
  col4<-c("","",ls,lsrae,lsnnt,lsFEE,lsRAP)
  ret<-as.data.frame(cbind(col1,col2,col3,col4))
  ret$col2<-as.numeric(as.character(ret$col2))
  colnames(ret)<-c("Medida","Valor","inferior","superior" )
  retlist<-list(ret)
  print(ret)
  #library(xtable)
  #print(xtable(ret))
}
############ FUNCION PARA CALCULAR ODDS RATIO ####################
OR <- function(a,b,c,d,decimal=2)
{         totExposed <- a+b
          totUnexposed <- c+d
# probabilidades          
          probDiseaseGivenExposed <- a/totExposed
          probDiseaseGivenUnexposed <- c/totUnexposed
          probControlGivenExposed <- b/totExposed
          probControlGivenUnexposed <- d/totUnexposed
          
# odds ratio          
          oddsRatio <-round((probDiseaseGivenExposed*probControlGivenUnexposed)/
                              (probControlGivenExposed*probDiseaseGivenUnexposed),3)
          
          
# intervalos de confianza    
          sigma <- sqrt((1/a)+(1/b)+(1/c)+(1/d))
          alpha=0.05
          z <- qnorm(1-(alpha/2))
          lowervalue <- round(oddsRatio * exp(-z * sigma),3)
          uppervalue <- round(oddsRatio * exp( z * sigma),3)
# resumen        
          oee<-round( a/c ,3)
          oene<-round( b/d ,3)
          OR<-round( oddsRatio,3)
          
# Construccion de la tabla          
          col1<-c("Odds de exposición en los enfermos", "Odds de exposición en los no enfermos","Odds ratio (OR)")
          col2<-c(oee,oene,OR)
          
          col3<-c("","",lowervalue)
          col4<-c("","",uppervalue)
          ret<-as.data.frame(cbind(col1,col2,col3,col4))
          ret$col2<-as.numeric(as.character(ret$col2))
          colnames(ret)<-c("Medida","Valor","inferior","superior" )
          retlist<-list(ret)
          print(ret)
          
}
############ FUNCION PARA CALCULAR LA RAZON DE TASAS ####################
cohorte.di<- function(a,b,c,d,decimal=2)
{     
  casosexp <- a
  casosnoexp <- c
  totcasos <- a+c
# probabilidades  
  totPY<-b+d
  alpha=0.05
  Texp<-casosexp/b
  lim_infTexp<-round(qchisq(alpha/2,2*casosexp)/2/b,5)
  lim_supTexp<-round(qchisq(1-alpha/2,2*(casosexp+1))/2/b,5)
  Tnoexp<-casosnoexp/d
  lim_infTnoexp<-round(qchisq(alpha/2,2*casosnoexp)/2/d,5)
  lim_supTnoexp<-round(qchisq(1-alpha/2,2*(casosnoexp+1))/2/d,5)
  
# RT e intervalos de confianza  
  RT<-Texp/Tnoexp
  E1<-totcasos*b/totPY
  E2<-totcasos*d/totPY
  chi<-round((a-E1)^2/E1+(c-E2)^2/E2,3)
  valorp<-round(1-pchisq(chi,1),3)
  z <- qnorm(1-(alpha/2))
  sd<-sqrt(1/casosexp+1/casosnoexp)
  Li<-round(exp(log(RT)-z*sd),3)
  Ls<-round(exp(log(RT)+z*sd),3)
  RT<-round(RT,4)
  
# construccion de la tabla 
  col1<-c("Tasa en expuestos", "Tasa en no expuestos","Razón de tasas (RT)")
  col2<-round(c(Texp,Tnoexp,RT),4)
  col3<-c(lim_infTexp,lim_infTnoexp,Li)
  col4<-c(lim_supTexp,lim_supTnoexp,Ls)
  ret<-as.data.frame(cbind(col1,col2,col3,col4))
  ret$col2<-as.numeric(as.character(ret$col2))
  colnames(ret)<-c("Medida","Valor","inferior","superior" )
  retlist<-list(ret)
  print(ret)
}
############ FUNCION PARA CALCULAR OR ESTRATIFICADO ####################
ORMH<-function (x)
{
  conjunta = apply(x, 1:2, sum)
  OR = conjunta[1, 1] * conjunta[2, 2]/conjunta[1, 2]/conjunta[2, 1]
  k = dim(x)[3]
  n11k = x[1, 1, ]
  n21k = x[2, 1, ]
  n12k = x[1, 2, ]
  n22k = x[2, 2, ]
  
  ORk = x[1, 1, ] * x[2, 2, ]/x[1, 2, ]/x[2, 1, ]
  alpha=0.05
  sigma <- sqrt((1/x[1,1,])+(1/x[1,2,])+(1/x[2,1,])+(1/x[2,2,]))
  
  z <- qnorm(1-(alpha/2))
  limi <- round(ORk * exp(-z * sigma),3)
  lims <- round(ORk * exp( z * sigma),3)

  row1sums = n11k + n12k
  row2sums = n21k + n22k
  col1sums = n11k + n21k
  
  n = apply(x, 3, sum)
  u11 = row1sums * col1sums/n
  var11 = row1sums * row2sums * col1sums * (n - col1sums)/(n^2)/(n -1)
  num = (sum(n11k - u11))^2
  deno = sum(var11)
  cmh = num/deno
  cmh.p.value = 1 - pchisq(cmh, 1)
  DNAME = deparse(substitute(x))
  METHOD = "OR de Mantel-Haenszel "
  s.diag <- sum(x[1, 1, ] * x[2, 2, ]/n)
  s.offd <- sum(x[1, 2, ] * x[2, 1, ]/n)
  MH.ESTIMATE <- s.diag/s.offd
  orkname = paste("OR del estrato", 1:k)
  g<-x[1, 1, ] * x[2, 2, ]/n
  h<-x[1, 2, ] * x[2, 1, ]/n
  p<-(x[1, 1, ] + x[2, 2, ])/n
  q<-(x[1, 2, ] + x[2, 1, ])/n
  ormh<-sum(g)/sum(h)
  var1<-sum(g*p)/(2*(sum(g))^2)
  var2<-sum(g*q+h*p)/(2*sum(g)*sum(h))
  var3<-sum(h*q)/(2*sum(h)^2)
  var<-var1+var2+var3
  ici<-round(exp(log(ormh)-1.96*sqrt(var)),3)
  ics<-round(exp(log(ormh)+1.96*sqrt(var)),3)
  DNAME = deparse(substitute(x))
  METHOD = "OR de Mantel-Haenszel "
  MH.ESTIMATE <- s.diag/s.offd
  orkname = paste("OR del estrato", 1:k)
  
#OR en los estratos  
  ORk<-round(ORk,4)
  col00<-c(ORk)
  col01<-c(limi)
  col02<-c(lims)
  ret0<-as.data.frame(cbind(col00,col01,col02))
 
  colnames(ret0)<-c("OR en los estratos","Inferior","Superior")
  retlist0<-list(ret0)
  print(ret0)
}
ORMH2<-function (x){
  conjunta = apply(x, 1:2, sum)
OR = conjunta[1, 1] * conjunta[2, 2]/conjunta[1, 2]/conjunta[2, 1]
k = dim(x)[3]
n11k = x[1, 1, ]
n21k = x[2, 1, ]
n12k = x[1, 2, ]
n22k = x[2, 2, ]

ORk = x[1, 1, ] * x[2, 2, ]/x[1, 2, ]/x[2, 1, ]
alpha=0.05
sigma <- sqrt((1/x[1,1,])+(1/x[1,2,])+(1/x[2,1,])+(1/x[2,2,]))

z <- qnorm(1-(alpha/2))
limi <- round(ORk * exp(-z * sigma),3)
lims <- round(ORk * exp( z * sigma),3)

row1sums = n11k + n12k
row2sums = n21k + n22k
col1sums = n11k + n21k

n = apply(x, 3, sum)
u11 = row1sums * col1sums/n
var11 = row1sums * row2sums * col1sums * (n - col1sums)/(n^2)/(n -1)
num = (sum(n11k - u11))^2
deno = sum(var11)
cmh = num/deno
cmh.p.value = 1 - pchisq(cmh, 1)
DNAME = deparse(substitute(x))
METHOD = "OR de Mantel-Haenszel "
s.diag <- sum(x[1, 1, ] * x[2, 2, ]/n)
s.offd <- sum(x[1, 2, ] * x[2, 1, ]/n)
MH.ESTIMATE <- s.diag/s.offd
orkname = paste("OR del estrato", 1:k)
g<-x[1, 1, ] * x[2, 2, ]/n
h<-x[1, 2, ] * x[2, 1, ]/n
p<-(x[1, 1, ] + x[2, 2, ])/n
q<-(x[1, 2, ] + x[2, 1, ])/n
ormh<-sum(g)/sum(h)
var1<-sum(g*p)/(2*(sum(g))^2)
var2<-sum(g*q+h*p)/(2*sum(g)*sum(h))
var3<-sum(h*q)/(2*sum(h)^2)
var<-var1+var2+var3
ici<-round(exp(log(ormh)-1.96*sqrt(var)),3)
ics<-round(exp(log(ormh)+1.96*sqrt(var)),3)
DNAME = deparse(substitute(x))
METHOD = "OR de Mantel-Haenszel "
MH.ESTIMATE <- s.diag/s.offd
orkname = paste("OR del estrato", 1:k)

#OR ajustada  
  col1<-c("OR Mantel-Haenszel (ORmh)","chi-cuadrado","gl","P")
  col2<-c(ormh,cmh,1,cmh.p.value)
  col3<-c(ici,"","","")
  col4<-c(ics,"","","")
  ret<-as.data.frame(cbind(col1,col2,col3,col4)) 
  ret$col2<-round(as.numeric(as.character(ret$col2)),3)
  colnames(ret)<-c("Medida","Valor","Inferior","Superior")
  retlist<-list(ret)
  print(ret)
}
############ FUNCION PARA CALCULAR RAZON DE TASAS ESTRATIFICADA ####################
cohorte.di.mh<-function (x,decimal=3)
{
  conjunta2 = apply(x, 1:2, sum)
  RT = conjunta2[1, 1] * conjunta2[2, 2]/conjunta2[1, 2]/conjunta2[2, 1]
  RT<-round(RT,4)
  k = dim(x)[3]
  nEk = x[1, 1, ]
  nNEk = x[2, 1, ]
  nNE<-sum(nNEk)
  nk=x[1,1,]+x[2,1,]
  nE<-sum(nEk)
  mEk = x[1, 2, ]
  mE<-sum(mEk)
  mNEk = x[2, 2, ]
  mNE<-sum(mNEk)
  TE<-nE/mE *100000
  TE<-round(TE,3)
  TNE<-nNE/mNE*100000
  TNE<-round(TNE,3)
  RTk = round(x[1, 1, ] * x[2, 2, ]/x[1, 2, ]/x[2, 1, ],4)
  
  #RT en los estratos  
  alpha=0.05
  z <- qnorm(1-(alpha/2))
  sd<-sqrt(1/x[1,1,]+1/x[2,1,])
  Limi<-round(exp(log(RTk)-z*sd),3)
  Lims<-round(exp(log(RTk)+z*sd),3)
  
  
  
  #RT estandarizada
  mk=x[1,2, ]+x[2,2, ]
  METHOD2 = "RTMH de Mantel-Haenszel "
  numerador <- sum(x[1, 1, ] * x[2, 2, ]/mk)
  denominador <- sum(x[1, 2, ] * x[2, 1, ]/mk)
  RTMH.ESTIMATE <- round(numerador/denominador,3)
  dechi=sum(as.numeric(x[1,2,])*as.numeric(x[2,2,])*nk/(mk)^2)
  nuchi=(sum(x[1,1,])-sum (x[1,2,]*nk/mk))^2     
  chi=round(nuchi/dechi,3)
  P<-round(1-pchisq(chi,1),3)
  varlog_num<-sum((as.numeric(x[1,1,])+as.numeric(x[2,1,]))*as.numeric(x[1,2,])*as.numeric(x[2,2,])/mk^2)
  varlog_den<-sum(x[1,1,]*x[2,2,]/mk)*sum(x[2,1,]*x[1,2,]/mk)
  varlog<-varlog_num/varlog_den    
  eelog<-sqrt(varlog)
  lims<-round(RTMH.ESTIMATE*exp(1.96*eelog),3)
  limi<-round( RTMH.ESTIMATE/exp(1.96*eelog),3)
  
  #RT en los estratos  
  col00<-c(RTk)
  col01<-c(Limi)
  col02<-c(Lims)
  ret0<-as.data.frame(cbind(col00,col01,col02))
  colnames(ret0)<-c("RT en los estratos","Inferior","Superior")
  retlist0<-list(ret0)
  print(ret0)
}
cohorte.di.mh2<-function (x,decimal=3)
{
  conjunta2 = apply(x, 1:2, sum)
  RT = conjunta2[1, 1] * conjunta2[2, 2]/conjunta2[1, 2]/conjunta2[2, 1]
  RT<-round(RT,4)
  k = dim(x)[3]
  nEk = x[1, 1, ]
  nNEk = x[2, 1, ]
  nNE<-sum(nNEk)
  nk=x[1,1,]+x[2,1,]
  nE<-sum(nEk)
  mEk = x[1, 2, ]
  mE<-sum(mEk)
  mNEk = x[2, 2, ]
  mNE<-sum(mNEk)
  TE<-nE/mE *100000
  TE<-round(TE,3)
  TNE<-nNE/mNE*100000
  TNE<-round(TNE,3)
  RTk = round(x[1, 1, ] * x[2, 2, ]/x[1, 2, ]/x[2, 1, ],4)
  
  #RT en los estratos  
  alpha=0.05
  z <- qnorm(1-(alpha/2))
  sd<-sqrt(1/x[1,1,]+1/x[2,1,])
  Limi<-round(exp(log(RTk)-z*sd),3)
  Lims<-round(exp(log(RTk)+z*sd),3)
  
  
  
  #RT estandarizada
  mk=x[1,2, ]+x[2,2, ]
  METHOD2 = "RTMH de Mantel-Haenszel "
  numerador <- sum(x[1, 1, ] * x[2, 2, ]/mk)
  denominador <- sum(x[1, 2, ] * x[2, 1, ]/mk)
  RTMH.ESTIMATE <- round(numerador/denominador,3)
  dechi=sum(as.numeric(x[1,2,])*as.numeric(x[2,2,])*nk/(mk)^2)
  nuchi=(sum(x[1,1,])-sum (x[1,2,]*nk/mk))^2     
  chi=round(nuchi/dechi,3)
  P<-round(1-pchisq(chi,1),3)
  varlog_num<-sum((as.numeric(x[1,1,])+as.numeric(x[2,1,]))*as.numeric(x[1,2,])*as.numeric(x[2,2,])/mk^2)
  varlog_den<-sum(x[1,1,]*x[2,2,]/mk)*sum(x[2,1,]*x[1,2,]/mk)
  varlog<-varlog_num/varlog_den    
  eelog<-sqrt(varlog)
  lims<-round(RTMH.ESTIMATE*exp(1.96*eelog),3)
  limi<-round( RTMH.ESTIMATE/exp(1.96*eelog),3)
  
  #RT estandarizada 
  col11<-c("Razón de tasas de Mantel-Haenszel","Chi cuadrado","p valor")
  col21<-c( RTMH.ESTIMATE,chi,P)
  col31<-c(limi,"","")
  col41<-c(lims,"","")
  ret1<-as.data.frame(cbind(col11,col21,col31,col41))
  ret1$col21<-as.numeric(as.character(ret1$col21))
  colnames(ret1)<-c("Medida","Valor","Inferior","Superior")
  retlist1<-list(ret1)
  print(ret1)
}
############ FUNCION PARA CALCULAR VALIDEZ DE PRUEBAS DIAGNOSTICAS ####################
epi.tests<-function (dat, conf.level = 0.95,decimal=3) 
{
  elements <- list()
  elements <- within(elements, {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    .funincrisk <- function(cdat, conf.level) {
      N. <- 1 - ((1 - conf.level)/2)
      a <- cdat[, 1]
      n <- cdat[, 2]
      b <- n - a
      p <- a/n
      a. <- ifelse(a == 0, a + 1, a)
      b. <- ifelse(b == 0, b + 1, b)
      low <- a./(a. + (b. + 1) * (1/qf(1 - N., 2 * a., 
                                       2 * b. + 2)))
      up <- (a. + 1)/(a. + 1 + b./(1/qf(1 - N., 2 * b., 
                                        2 * a. + 2)))
      low <- ifelse(a == 0, 0, low)
      up <- ifelse(a == n, 1, up)
      rval <- data.frame(est = p, lower = low, upper = up)
      rval
    }
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    M1 <- a + c
    M0 <- b + d
    N1 <- a + b
    N0 <- c + d
    total <- a + b + c + d
    tdat <- as.matrix(cbind(M1, total))
    trval <- .funincrisk(tdat, conf.level)
    tp <- trval$est
    tp.low <- trval$lower
    tp.up <- trval$upper
    tprev <- data.frame(est = tp, lower = tp.low, upper = tp.up)
    tdat <- as.matrix(cbind(N1, total))
    trval <- .funincrisk(tdat, conf.level)
    ap <- trval$est
    ap.low <- trval$lower
    ap.up <- trval$upper
    aprev <- data.frame(est = ap, lower = ap.low, upper = ap.up)
    tdat <- as.matrix(cbind(a, M1))
    trval <- .funincrisk(tdat, conf.level)
    se <- round(trval$est,3)
    se.low <-round(trval$lower,3)
    se.up <- round(trval$upper,3)
    sensitivity <- data.frame(est = se, lower = se.low, upper = se.up)
    tdat <- as.matrix(cbind(d, M0))
    trval <- .funincrisk(tdat, conf.level)
    sp <- trval$est
    sp.low <- trval$lower
    sp.up <- trval$upper
    specificity <- data.frame(est = sp, lower = sp.low, upper = sp.up)
    tdat <- as.matrix(cbind(a, N1))
    trval <- .funincrisk(tdat, conf.level)
    ppv <- trval$est
    ppv.low <- trval$lower
    ppv.up <- trval$upper
    pv.positive <- data.frame(est = ppv, lower = ppv.low, 
                              upper = ppv.up)
    tdat <- as.matrix(cbind(d, N0))
    trval <- .funincrisk(tdat, conf.level)
    npv <- trval$est
    npv.low <- trval$lower
    npv.up <- trval$upper
    pv.negative <- data.frame(est = npv, lower = npv.low, 
                              upper = npv.up)
    lrpos <- (a/M1)/(1 - (d/M0))
    lrpos.low <- exp(log(lrpos) - z * sqrt((1 - se)/(M1 *se) + (sp)/(M0 * (1 - sp))))
    lrpos.up <- exp(log(lrpos) + z * sqrt((1 - se)/(M1 *se) + (sp)/(M0 * (1 - sp))))
    lr.positive <- data.frame(est = lrpos, lower = lrpos.low,upper = lrpos.up)
    lrneg <- (1 - (a/M1))/(d/M0)
    lrneg.low <- exp(log(lrneg) - z * sqrt((se)/(M1 * (1 -se)) + (1 - sp)/(M0 * (sp))))
    lrneg.up <- exp(log(lrneg) + z * sqrt((se)/(M1 * (1 -se)) + (1 - sp)/(M0 * (sp))))
    lr.negative <- data.frame(est = lrneg, lower = lrneg.low,upper = lrneg.up)
    tdat <- as.matrix(cbind((a + d), total))
    trval <- .funincrisk(tdat, conf.level)
    da <- trval$est
    da.low <- trval$lower
    da.up <- trval$upper
    diag.acc <- data.frame(est = da, lower = da.low, upper = da.up)
    dOR.p <- (a * d)/(b * c)
    lndOR <- log(dOR.p)
    lndOR.var <- 1/a + 1/b + 1/c + 1/d
    lndOR.se <- sqrt(1/a + 1/b + 1/c + 1/d)
    lndOR.l <- lndOR - (z * lndOR.se)
    lndOR.u <- lndOR + (z * lndOR.se)
    dOR.se <- exp(lndOR.se)
    dOR.low <- exp(lndOR.l)
    dOR.up <- exp(lndOR.u)
    diag.or <- data.frame(est = dOR.p, lower = dOR.low, upper = dOR.up)
    ndx <- 1/(se - (1 - sp))
    ndx.1 <- 1/(se.low - (1 - sp.low))
    ndx.2 <- 1/(se.up - (1 - sp.up))
    ndx.low <- min(ndx.1, ndx.2)
    ndx.up <- max(ndx.1, ndx.2)
    nnd <- data.frame(est = ndx, lower = ndx.low, upper = ndx.up)
    c.p <- se - (1 - sp)
    c.1 <- se.low - (1 - sp.low)
    c.2 <- se.up - (1 - sp.up)
    c.low <- min(c.1, c.2)
    c.up <- max(c.1, c.2)
    youden <- data.frame(est = c.p, lower = c.low, upper = c.up)
    rval <- list(aprev = elements$aprev, tprev = elements$tprev, 
                 se = elements$sensitivity, sp = elements$specificity, 
                 diag.acc = elements$diag.acc, diag.or = elements$diag.or, 
                 nnd = elements$nnd, youden = elements$youden, ppv = elements$pv.positive, 
                 npv = elements$pv.negative, plr = elements$lr.positive, 
                 nlr = elements$lr.negative)
    
  })
}
