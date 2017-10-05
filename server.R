shinyServer(function(input, output) {
  
 #RR para 2 filas####################################################################### 
  output$r1<- renderTable({
    a<-input$cexp
    b<-input$ncexp
    c<-input$cnoexp
    d<-input$nocnoexp
   
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t1<- renderText({"Medidas de riesgo y de impacto en expuestos frente a no expuestos"
  }) 
 
   
 #RR para 3 filas####################################################################### 
  output$r2<- renderTable({
    a2<-input$cexpa
    b2<-input$ncexpa
    c2<-input$cnoexpa
    d2<-input$nocnoexpa
    cohorte.ia(a2,b2,c2,d2)
  },digits = 3)
  output$t2<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel I frente a no expuestos"
  }) 
  output$r2b<- renderTable({
    a2b<-input$cexp2
    b2b<-input$ncexp2
    c2b<-input$cnoexpa
    d2b<-input$nocnoexpa
    cohorte.ia(a2b,b2b,c2b,d2b)
  },digits = 3)
  output$t2b<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel II frente a no expuestos"
  }) 
 #RR para 4 filas####################################################################### 
  output$r3<- renderTable({
    a<-input$cexpb
    b<-input$ncexpb
    c<-input$cnoexpb
    d<-input$nocnoexpb
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t3<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel I frente a no expuestos"
  }) 
  output$r3b<- renderTable({
    a<-input$cexp2b
    b<-input$ncexp2b
    c<-input$cnoexpb
    d<-input$nocnoexpb
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t3b<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel II frente a no expuestos"
  }) 
  output$r3c<- renderTable({
    a<-input$cexp3b
    b<-input$ncexp3b
    c<-input$cnoexpb
    d<-input$nocnoexpb
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t3c<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel III frente a no expuestos"
  })
 #RR para 5 filas####################################################################### 
  output$r4<- renderTable({
    a<-input$cexpc
    b<-input$ncexpc
    c<-input$cnoexpc
    d<-input$nocnoexpc
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t4<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel I frente a no expuestos"
  }) 
  output$r4a<- renderTable({
    a<-input$cexp2c
    b<-input$ncexp2c
    c<-input$cnoexpc
    d<-input$nocnoexpc
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t4a<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel II frente a no expuestos"
  }) 
  output$r4b<- renderTable({
    a<-input$cexp3c
    b<-input$ncexp3c
    c<-input$cnoexpc
    d<-input$nocnoexpc
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t4b<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel III frente a no expuestos"
  }) 
  output$r4c<- renderTable({
    a<-input$cexp4c
    b<-input$ncexp4c
    c<-input$cnoexpc
    d<-input$nocnoexpc
    cohorte.ia(a,b,c,d)
  },digits = 3)
  output$t4c<- renderText({"Medidas de riesgo y de impacto en expuestos en el nivel IV frente a no expuestos"
  }) 
 #OR para 2 filas####################################################################### 
 output$resultado<- renderTable({
   a<-input$casoexp
   b<-input$nocasoexp
   c<-input$casonoexp
   d<-input$nocasonoexp
   OR(a,b,c,d)
 })
 output$texto1<- renderText({"Odds ratio de expuestos frente a no expuestos"
 }) 
 #OR para 3 filas####################################################################### 
 output$resultado2<- renderTable({
   a2<-input$casoexpa
   b2<-input$nocasoexpa
   c2<-input$casonoexpa
   d2<-input$nocasonoexpa
   OR(a2,b2,c2,d2)
 })
 output$texto2<- renderText({"Odds ratio de expuestos en el nivel I frente a no expuestos"
 }) 
 output$resultado2b<- renderTable({
   a2b<-input$casoexp2
   b2b<-input$nocasoexp2
   c2b<-input$casonoexpa
   d2b<-input$nocasonoexpa
   OR(a2b,b2b,c2b,d2b)
 })
 output$texto2b<- renderText({"Odds ratio de expuestos en el nivel II frente a no expuestos"
 }) 
 #OR para 4 filas####################################################################### 
 output$resultado3<- renderTable({
   a2c<-input$casoexpb
   b2c<-input$nocasoexpb
   c2c<-input$casonoexpb
   d2c<-input$nocasonoexpb
   OR(a2c,b2c,c2c,d2c)
 })
 output$texto3<- renderText({"Odds ratio de expuestos en el nivel I frente a no expuestos"
 }) 
 output$resultado3b<- renderTable({
   a<-input$casoexp2b
   b<-input$nocasoexp2b
   c<-input$casonoexpb
   d<-input$nocasonoexpb
   OR(a,b,c,d)
 })
 output$texto3b<- renderText({"Odds ratio de expuestos en el nivel II frente a no expuestos"
 }) 
 output$resultado3c<- renderTable({
   a<-input$casoexp3b
   b<-input$nocasoexp3b
   c<-input$casonoexpb
   d<-input$nocasonoexpb
   OR(a,b,c,d)
 })
 output$texto3c<- renderText({"Odds ratio de expuestos en el nivel III frente a no expuestos"
 })
 #OR para 5 filas####################################################################### 
 output$resultado4<- renderTable({
   a<-input$casoexpc
   b<-input$nocasoexpc
   c<-input$casonoexpc
   d<-input$nocasonoexpc
   OR(a,b,c,d)
 })
 output$texto4<- renderText({"Odds ratio de expuestos en el nivel I frente a no expuestos"
 }) 
 output$resultado4a<- renderTable({
   a<-input$casoexp2c
   b<-input$nocasoexp2c
   c<-input$casonoexpc
   d<-input$nocasonoexpc
   OR(a,b,c,d)
 })
 output$texto4a<- renderText({"Odds ratio de expuestos en el nivel II frente a no expuestos"
 }) 
 output$resultado4b<- renderTable({
   a<-input$casoexp3c
   b<-input$nocasoexp3c
   c<-input$casonoexpc
   d<-input$nocasonoexpc
   OR(a,b,c,d)
 })
 output$texto4b<- renderText({"Odds ratio de expuestos en el nivel III frente a no expuestos"
 }) 
 output$resultado4c<- renderTable({
   a<-input$casoexp4c
   b<-input$nocasoexp4c
   c<-input$casonoexpc
   d<-input$nocasonoexpc
   OR(a,b,c,d)
 })
 output$texto4c<- renderText({"Odds ratio de expuestos en el nivel IV frente a no expuestos"
 }) 
 #RT para 2 filas####################################################################### 
 output$res<- renderTable({
   a<-input$c1exp
   b<-input$praexp
   c<-input$c1noexp
   d<-input$pranoexp
   cohorte.di(a,b,c,d)
 })
 output$text1<- renderText({"Razón de incidencia de expuestos frente a no expuestos"
 }) 
 #RT para 3 filas####################################################################### 
 output$res2<- renderTable({
   a2<-input$c1expa
   b2<-input$praexpa
   c2<-input$c1noexpa
   d2<-input$pranoexpa
   cohorte.di(a2,b2,c2,d2)
 })
 output$text2<- renderText({"Razón de incidencia de expuestos en el nivel I frente a no expuestos"
 }) 
 output$res2b<- renderTable({
   a2b<-input$c1exp2
   b2b<-input$praexp2
   c2b<-input$c1noexpa
   d2b<-input$pranoexpa
   cohorte.di(a2b,b2b,c2b,d2b)
 })
 output$text2b<- renderText({"Razón de incidencia de expuestos en el nivel II frente a no expuestos"
 }) 
 #RT para 4 filas####################################################################### 
 output$res3<- renderTable({
   a<-input$c1expb
   b<-input$praexpb
   c<-input$c1noexpb
   d<-input$pranoexpb
   cohorte.di(a,b,c,d)
 })
 output$text3<- renderText({"Razón de incidencia de expuestos en el nivel I frente a no expuestos"
 }) 
 output$res3b<- renderTable({
   a<-input$c1exp2b
   b<-input$praexp2b
   c<-input$c1noexpb
   d<-input$pranoexpb
   cohorte.di(a,b,c,d)
 })
 output$text3b<- renderText({"Razón de incidencia de expuestos en el nivel II frente a no expuestos"
 }) 
 output$res3c<- renderTable({
   a<-input$c1exp3b
   b<-input$praexp3b
   c<-input$c1noexpb
   d<-input$pranoexpb
   cohorte.di(a,b,c,d)
 })
 output$text3c<- renderText({"Razón de incidencia de expuestos en el nivel III frente a no expuestos"
 })
 #RT para 5 filas####################################################################### 
 output$res4<- renderTable({
   a<-input$c1expc
   b<-input$praexpc
   c<-input$c1noexpc
   d<-input$pranoexpc
   cohorte.di(a,b,c,d)
 })
 output$text4<- renderText({"Razón de incidencia de expuestos en el nivel I frente a no expuestos"
 }) 
 output$res4a<- renderTable({
   a<-input$c1exp2c
   b<-input$praexp2c
   c<-input$c1noexpc
   d<-input$pranoexpc
   cohorte.di(a,b,c,d)
 })
 output$text4a<- renderText({"Razón de incidencia de expuestos en el nivel II frente a no expuestos"
 }) 
 output$res4b<- renderTable({
   a<-input$c1exp3c
   b<-input$praexp3c
   c<-input$c1noexpc
   d<-input$pranoexpc
   cohorte.di(a,b,c,d)
 })
 output$text4b<- renderText({"Razón de incidencia de expuestos en el nivel III frente a no expuestos"
 }) 
 output$res4c<- renderTable({
   a<-input$c1exp4c
   b<-input$praexp4c
   c<-input$c1noexpc
   d<-input$pranoexpc
   cohorte.di(a,b,c,d)
 })
 output$text4c<- renderText({"Razón de incidencia de expuestos en el nivel IV frente a no expuestos"
 })
 ############################estratificado#####################################
 #ormh para 2 filas####################################################################### 
 output$re1<- renderTable({
   a<-input$cexpestra
   b<-input$ncexpestra
   c<-input$cnoexpestra
   d<-input$nocnoexpestra
   
   OR(a,b,c,d)
 })
 output$te1<- renderText({"Odds Ratio cruda"
 }) 
 
 output$re2<- renderTable({
   a<-input$cexp
   b<-input$ncexp
   c<-input$cnoexp
   d<-input$nocnoexp
   a1<-input$cexpe1
   b1<-input$ncexpe1
   c1<-input$cnoexpe1
   d1<-input$nocnoexpe1
   a2<-input$cexpe2
   b2<-input$ncexpe2
   c2<-input$cnoexpe2
   d2<-input$nocnoexpe2
   tabla <- array(c(a1,c1,b1,d1,a2,c2,b2,d2),dim=c(2,2,2))
   ORMH(tabla)
 },rownames=TRUE)
 
 output$te2aa<- renderText({"Odds Ratio en los estratos "
 }) 
 
 output$re2aa<- renderTable({
   a<-input$cexp
   b<-input$ncexp
   c<-input$cnoexp
   d<-input$nocnoexp
   a1<-input$cexpe1
   b1<-input$ncexpe1
   c1<-input$cnoexpe1
   d1<-input$nocnoexpe1
   a2<-input$cexpe2
   b2<-input$ncexpe2
   c2<-input$cnoexpe2
   d2<-input$nocnoexpe2
   tablax <- array(c(a1,c1,b1,d1,a2,c2,b2,d2),dim=c(2,2,2))
   ORMH2(tablax)
 })
 output$te2<- renderText({"Odds Ratio estandarizada (Mantel-Haenszel) "
 }) 
 #ormh para 3 filas####################################################################### 
 output$re3<- renderTable({
   aa<-input$cexp3
   bb<-input$ncexp3
   cc<-input$cnoexp3
   dd<-input$nocnoexp3
   
   OR(aa,bb,cc,dd)
 })
 output$te3<- renderText({"Odds Ratio cruda"
 }) 
 output$re4<- renderTable({
   aa<-input$cexp3
   bb<-input$ncexp3
   cc<-input$cnoexp3
   dd<-input$nocnoexp3
   aa1<-input$cexpe4
   bb1<-input$ncexpe4
   cc1<-input$cnoexpe4
   dd1<-input$nocnoexpe4
   aa2<-input$cexpe5
   bb2<-input$ncexpe5
   cc2<-input$cnoexpe5
   dd2<-input$nocnoexpe5
   aa3<-input$cexpe6
   bb3<-input$ncexpe6
   cc3<-input$cnoexpe6
   dd3<-input$nocnoexpe6
   
   
   
   tabla <- array(c(aa1,cc1,bb1,dd1,aa2,cc2,bb2,dd2,aa3,cc3,bb3,dd3),dim=c(2,2,3))
   ORMH(tabla)
 },rownames = TRUE)
 output$re4aa<- renderTable({
   aa<-input$cexp3
   bb<-input$ncexp3
   cc<-input$cnoexp3
   dd<-input$nocnoexp3
   aa1<-input$cexpe4
   bb1<-input$ncexpe4
   cc1<-input$cnoexpe4
   dd1<-input$nocnoexpe4
   aa2<-input$cexpe5
   bb2<-input$ncexpe5
   cc2<-input$cnoexpe5
   dd2<-input$nocnoexpe5
   aa3<-input$cexpe6
   bb3<-input$ncexpe6
   cc3<-input$cnoexpe6
   dd3<-input$nocnoexpe6
   
   
   
   tabla <- array(c(aa1,cc1,bb1,dd1,aa2,cc2,bb2,dd2,aa3,cc3,bb3,dd3),dim=c(2,2,3))
   ORMH2(tabla)
 })
 output$te4aa<- renderText({"Odds Ratio en los estratos "})
 
 output$te4<- renderText({"Odds Ratio estandarizada (Mantel-Haenszel)"
 }) 
#################################################################################################
 #rt para 2 filas####################################################################### 
 output$rcmp1<- renderTable({
   aaa<-input$ce
   bbb<-input$pte
   ccc<-input$cnoe
   ddd<-input$ptnoe
   
   cohorte.di(aaa,bbb,ccc,ddd)
 })
 
 output$tcmp1<- renderText({"Razon de Tasas cruda"
 }) 
 
 output$rcmp2aa<- renderTable({
   aaa<-input$ce
   bbb<-input$pte
   ccc<-input$cnoe
   ddd<-input$ptnoe
   a1<-input$ce1
   b1<-input$pte1
   c1<-input$cnoe1
   d1<-input$ptnoe1
   a2<-input$ce2
   b2<-input$pte2
   c2<-input$cnoe2
   d2<-input$ptnoe2
   tabla <- array(c(a1,c1,b1,d1,a2,c2,b2,d2),dim=c(2,2,2))
   cohorte.di.mh2(tabla)
   
 })
 output$tcmp2aa<- renderText({"Razon de Tasas en los estratos "
 }) 
 output$rcmp2<- renderTable({
   aaa<-input$ce
   bbb<-input$pte
   ccc<-input$cnoe
   ddd<-input$ptnoe
   a1<-input$ce1
   b1<-input$pte1
   c1<-input$cnoe1
   d1<-input$ptnoe1
   a2<-input$ce2
   b2<-input$pte2
   c2<-input$cnoe2
   d2<-input$ptnoe2
   tabla <- array(c(a1,c1,b1,d1,a2,c2,b2,d2),dim=c(2,2,2))
   cohorte.di.mh(tabla)
   
 },rownames=TRUE)
 output$tcmp2<- renderText({"Razon de Tasas estratificada (Mantel-Haenszel)"
 }) 
 output$rcmp3<- renderTable({
   aa<-input$ce3
   bb<-input$pte3
   cc<-input$cnoe3
   dd<-input$ptnoe3
   
   cohorte.di(aa,bb,cc,dd)
 })
 
 
 output$tcmp3<- renderText({"Razon de tasas cruda"
 }) 
 output$rcmp4aa<- renderTable({
   aa<-input$ce3
   bb<-input$pte3
   cc<-input$cnoe3
   dd<-input$ptnoe3
   aa1<-input$ce4
   bb1<-input$pte4
   cc1<-input$cnoe4
   dd1<-input$ptnoe4
   aa2<-input$ce5
   bb2<-input$pte5
   cc2<-input$cnoe5
   dd2<-input$ptnoe5
   aa3<-input$ce6
   bb3<-input$pte6
   cc3<-input$cnoe6
   dd3<-input$ptnoe6
   
   
   
   tabla <- array(c(aa1,cc1,bb1,dd1,aa2,cc2,bb2,dd2,aa3,cc3,bb3,dd3),dim=c(2,2,3))
   cohorte.di.mh2(tabla)
 })
 output$tcmp4<- renderText({"Razon de Tasas estratificada (Mantel-Haenszel) "
 }) 
 output$tcmp4aa<- renderText({"Razon de Tasas en los estratos "
 }) 
 output$rcmp4<- renderTable({
   aa<-input$ce3
   bb<-input$pte3
   cc<-input$cnoe3
   dd<-input$ptnoe3
   aa1<-input$ce4
   bb1<-input$pte4
   cc1<-input$cnoe4
   dd1<-input$ptnoe4
   aa2<-input$ce5
   bb2<-input$pte5
   cc2<-input$cnoe5
   dd2<-input$ptnoe5
   aa3<-input$ce6
   bb3<-input$pte6
   cc3<-input$cnoe6
   dd3<-input$ptnoe6
   
   
   
   tabla <- array(c(aa1,cc1,bb1,dd1,aa2,cc2,bb2,dd2,aa3,cc3,bb3,dd3),dim=c(2,2,3))
   cohorte.di.mh(tabla)
 },rownames = TRUE)
############################pruebas diagnosticas############################################################3
output$cmp2<- renderTable({
  vp1<-input$vp
  fn1<-input$fn
  fp1<-input$fp
  vn1<-input$vn
  dat <- as.table(matrix(c(vp1,fp1,fn1,vn1), nrow = 2, byrow = TRUE))
  rval <- epi.tests(dat, conf.level = 0.95)
  col11<-c("Sensibilidad","Especificidad","Proporción de falsos positivos","Proporción de falsos negativos",
           "Valor predictivo positivo","Valor predictivo negativo","Exactitud",
           "Odds ratio diagnóstica","Índice de Youden","Razón de verosimilitud positiva","Razón de verosimilitud negativa")
  col21<-c(rval$sensitivity$est,rval$specificity$est,
           1-rval$specificity$est,1-rval$sensitivity$est,
           rval$pv.positive$est,rval$pv.negative$est,
           rval$diag.acc$est,rval$diag.or$est,rval$youden$est,
           rval$lr.positive$est,rval$lr.negative$est)
  col31<-c(rval$sensitivity$lower,rval$specificity$lower,1-rval$specificity$upper,
           1-rval$sensitivity$upper,rval$pv.positive$lower,rval$pv.negative$lower,
           rval$diag.acc$lower,rval$diag.or$lower,rval$youden$lower,rval$lr.positive$lower,
           rval$lr.negative$lower)
  col41<-c(rval$sensitivity$upper,rval$specificity$upper,1-rval$specificity$lower,
           1-rval$sensitivity$lower,rval$pv.positive$upper,rval$pv.negative$upper,
           rval$diag.acc$upper,rval$diag.or$upper,rval$youden$upper,rval$lr.positive$upper,
           rval$lr.negative$upper)
  ret1<-as.data.frame(cbind(col11,col21,col31,col41))
  ret1$col21<-round(as.numeric(as.character(ret1$col21)),3)
  ret1$col31<-round(as.numeric(as.character(ret1$col31)),3)
  ret1$col41<-round(as.numeric(as.character(ret1$col41)),3)
  colnames(ret1)<-c("Medida","Valor","Inferior","Superior")
  retlist1<-list(ret1)
  print(ret1)
   
})
output$cmp1<- renderText({"Validez de la prueba diagnostica"
}) 
######################ESTANDARIZACION TASAS###################################################################################
######################ESTANDARIZACION TASAS###################################################################################

sumpem=100000
sumpee=100000
############### TABLA ENTRADA DE DATOS #################################
output$param = renderUI({
  if (input$ngrupos == "3")  {
    
    fixedRow(
      column(width = 2, h4("POBLACION 1"),numericInput("c1","Casos 0-65",23),numericInput("c2","Casos 66-75",12),
             numericInput("c3","Casos > 76",45)
             
      ),
      column(width = 2,
             h4("POBLACION 1"),numericInput("p1","Personas 0-65",1000),numericInput("p2","Personas 66-75",1500),
             numericInput("p3","Personas > 76",800)
      ),
      
      
      
      
      column(width = 2,
             h4("POBLACION 2"),numericInput("c1a","Casos 0-65",56),numericInput("c2a","Casos 66-75",44),
             numericInput("c3a","Casos > 76",33)
             
      ),
      column(width = 2,
             h4("POBLACION 2"),numericInput("p1a","Personas 0-65",1100),numericInput("p2a","Personas 66-75",1200),
             numericInput("p3a","Personas > 76",900)
      ),
      column(width = 2, h4("P. EUROPEA"),numericInput("peaaaa11","Personas 0-65",96000),
             numericInput("peaaaaa1","Personas 66-75",7000),numericInput("peaaaaaa1","Personas > 76",4000)
      ),
      column(width = 2, h4("P. MUNDIAL"),numericInput("pmaaaa11","Personas 0-65",93000),
             numericInput("pmaaaaa1","Personas 66-75",5000),numericInput("pmaaaaaa1","Personas > 76",2000)
      )
    )
    
  }else{
    if (input$ngrupos == "5") { 
      fixedRow(
        column(width = 2, h4("POBLACION 1"),numericInput("c0","Casos 0-45",22),
               numericInput("c00","Casos 46-55",22),numericInput("c000","Casos 56-65",55),numericInput("c0000","Casos 66-75",60),
               numericInput("c00000","Casos > 76",33)
               
        ),
        column(width = 2, h4("POBLACION 1"),numericInput("p0","Personas 0-45",234),
               numericInput("p00","Personas 46-55",1200),numericInput("p000","Personas 56-65",800),numericInput("p0000","Personas 66-75",654),
               numericInput("p00000","Personas > 76",900)
               
        ),
        
        
        
        column(width = 2, h4("POBLACION 2"),numericInput("c0a","Casos 0-45",44),
               numericInput("c00a","Casos 46-55",34),numericInput("c000a","Casos 56-65",55),numericInput("c0000a","Casos 66-75",66),
               numericInput("c00000a","Casos > 76",33)
               
        ),
        column(width = 2, h4("POBLACION 2"),numericInput("p0a","Personas 0-45",234),
               numericInput("p00a","Personas 46-55",345),numericInput("p000a","Personas 56-65",444),numericInput("p0000a","Personas 66-75",675),
               numericInput("p00000a","Personas > 76",213)
               
               
        ),
        column(width = 2, h4("P. EUROPEA"),numericInput("peaa11","Personas 0-45",64000),
               numericInput("peaaa1","Personas 46-55",14000),numericInput("peaaaa1","Personas 56-65",11000),
               numericInput("peaaaaa1","Personas 66-75",7000),numericInput("peaaaaaa1","Personas > 76",4000)
        ),
        column(width = 2, h4("P. MUNDIAL"),numericInput("pmaa11","Personas 0-45",74000),
               numericInput("pmaaa1","Personas 46-55",11000),numericInput("pmaaaa1","Personas 56-65",8000),
               numericInput("pmaaaaa1","Personas 66-75",5000),numericInput("pmaaaaaa1","Personas > 76",2000)
        )
      )
      
    }
    else{
      if(input$ngrupos == "7"){
        fixedRow(
          column(width = 2, h4("POBLACION 1"),numericInput("ca","Casos 0-25",56),
                 numericInput("caa","Casos 26-35",43),numericInput("caaa","Casos 36-45",54),
                 numericInput("caaaa","Casos 46-55",23),numericInput("caaaaa","Casos 56-65",99),numericInput("caaaaaa","Casos 66-75",33),
                 numericInput("caaaaaaa","Casos > 76",90)
                 
          ),
          column(width = 2, h4("POBLACION 1"),numericInput("pa","Personas 0-25",656),
                 numericInput("paa","Personas 26-35",345),numericInput("paaa","Personas 36-45",675),
                 numericInput("paaaa","Personas 46-55",234),numericInput("paaaaa","Personas 56-65",432),
                 numericInput("paaaaaa","Personas 66-75",678),numericInput("paaaaaaa","Personas > 76",300)
                 
          ),
          
          column(width = 2, h4("POBLACION 2"),numericInput("ca1","Casos 0-25",33),
                 numericInput("caa1","Casos 26-35",22),numericInput("caaa1","Casos 36-45",32),
                 numericInput("caaaa1","Casos 46-55",33),numericInput("caaaaa1","Casos 56-65",78),
                 numericInput("caaaaaa1","Casos 66-75",44),numericInput("caaaaaaa1","Casos > 76",54)
                 
          ),
          column(width = 2, h4("POBLACION 2"),numericInput("pa1","Personas 0-25",123),
                 numericInput("paa1","Personas 26-35",213),numericInput("paaa1","Personas 36-45",666),
                 numericInput("paaaa1","Personas 46-55",432),numericInput("paaaaa1","Personas 56-65",555),
                 numericInput("paaaaaa1","Personas 66-75",324),numericInput("paaaaaaa1","Personas > 76",444)
          ),
          column(width = 2, h4("P. EUROPEA"),numericInput("pe11","Personas 0-25",06000),
                 numericInput("pea1","Personas 26-35",14000),numericInput("peaa1","Personas 36-45",14000),
                 numericInput("peaaa1","Personas 46-55",14000),numericInput("peaaaa1","Personas 56-65",11000),
                 numericInput("peaaaaa1","Personas 66-75",7000),numericInput("peaaaaaa1","Personas > 76",4000)
          ),
          column(width = 2, h4("P. MUNDIAL"),numericInput("pm11","Personas 0-25",48000),
                 numericInput("pma1","Personas 26-35",14000),numericInput("pmaa1","Personas 36-45",12000),
                 numericInput("pmaaa1","Personas 46-55",11000),numericInput("pmaaaa1","Personas 56-65",8000),
                 numericInput("pmaaaaa1","Personas 66-75",5000),numericInput("pmaaaaaa1","Personas > 76",2000)
          )
        )
        
      }
      else{
        if(input$ngrupos == "8"){
          fixedRow(
            column(width = 2, h4("POBLACION 1"),numericInput("ca_1","Casos 0-15",230),numericInput("ca","Casos 16-25",156),
                   numericInput("caa","Casos 26-35",560),numericInput("caaa","Casos 36-45",100),
                   numericInput("caaaa","Casos 46-55",230),numericInput("caaaaa","Casos 56-65",150),numericInput("caaaaaa","Casos 66-75",200),
                   numericInput("caaaaaaa","Casos > 76",145)
                   
            ),
            column(width = 2, h4("POBLACION 1"),numericInput("pa_1","Personas 0-15",1000),numericInput("pa","Personas 16-25",789),
                   numericInput("paa","Personas 26-35",2234),numericInput("paaa","Personas 36-45",1000),
                   numericInput("paaaa","Personas 46-55",2134),numericInput("paaaaa","Personas 56-65",999),
                   numericInput("paaaaaa","Personas 66-75",2345),numericInput("paaaaaaa","Personas > 76",1560)
                   
            ),
            
            
            
            column(width = 2, h4("POBLACION 2"),numericInput("ca_11x","Casos 0-15",45),numericInput("ca1x","Casos 16-25",23),
                   numericInput("caa1x","Casos 26-35",56),numericInput("caaa1x","Casos 36-45",12),
                   numericInput("caaaa1x","Casos 46-55",78),numericInput("caaaaa1x","Casos 56-65",76),
                   numericInput("caaaaaa1x","Casos 66-75",90),numericInput("caaaaaaa1x","Casos > 76",45)
                   
            ),
            column(width = 2, h4("POBLACION 2"),numericInput("pa_11x","Personas 0-15",456),numericInput("pa1x","Personas 16-25",666),
                   numericInput("paa1x","Personas 26-35",212),numericInput("paaa1x","Personas 36-45",543),
                   numericInput("paaaa1x","Personas 46-55",675),numericInput("paaaaa1x","Personas 56-65",321),
                   numericInput("paaaaaa1x","Personas 66-75",453),numericInput("paaaaaaa1x","Personas > 76",333)
            ),
            column(width = 2, h4("P. EUROPEA"),numericInput("pe_11","Personas 0-15",22000),numericInput("pe1","Personas 16-25",14000),
                   numericInput("pea1","Personas 26-35",14000),numericInput("peaa1","Personas 36-45",14000),
                   numericInput("peaaa1","Personas 46-55",14000),numericInput("peaaaa1","Personas 56-65",11000),
                   numericInput("peaaaaa1","Personas 66-75",7000),numericInput("peaaaaaa1","Personas > 76",4000)
            ),
            column(width = 2, h4("P. MUNDIAL"),numericInput("pm_11","Personas 0-15",31000),numericInput("pm1","Personas 16-25",17000),
                   numericInput("pma1","Personas 26-35",14000),numericInput("pmaa1","Personas 36-45",12000),
                   numericInput("pmaaa1","Personas 46-55",11000),numericInput("pmaaaa1","Personas 56-65",8000),
                   numericInput("pmaaaaa1","Personas 66-75",5000),numericInput("pmaaaaaa1","Personas > 76",2000)
            )
          )  
        }
        
      }}}
  
  
})


#########################################################################
#tasas brutas

output$brutas1<-renderText({
  if (input$ngrupos == "3")  {
    paste("Tasa Bruta: ",round(((input$c1+input$c2+input$c3)/(input$p1+input$p2+input$p3))*
                                 as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      paste("Tasa Bruta: ",round(((input$c0+ input$c00+input$c000+input$c0000+input$c00000)/
                                    (input$p0+input$p00+input$p000+input$p0000+input$p00000))*
                                   as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        paste("Tasa Bruta: ",round(((input$ca+ input$caa+input$caaa+input$caaaa+input$caaaaa+input$caaaaaa+input$caaaaaaa)/
                                      (input$pa+input$paa+input$paaa+input$paaaa+input$paaaaa+input$paaaaaa+input$paaaaaaa))*
                                     as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      else{
        if (input$ngrupos == "8") {
          paste("Tasa Bruta: ",round(((input$ca_1+input$ca+ input$caa+input$caaa+input$caaaa+input$caaaaa+input$caaaaaa+input$caaaaaaa)/
                                        (input$pa_1+input$pa+input$paa+input$paaa+input$paaaa+input$paaaaa+input$paaaaaa+input$paaaaaaa))*
                                       as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      }}}
  
  
})
output$brutas2<-renderText({
  if (input$ngrupos == "3")  {
    paste("Tasa Bruta: ",round(((input$c1a+input$c2a+input$c3a)/(input$p1a+input$p2a+input$p3a))*
                                 as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      paste("Tasa Bruta: ",round(((input$c0a+ input$c00a+input$c000a+input$c0000a+input$c00000a)/
                                    (input$p0a+input$p00a+input$p000a+input$p0000a+input$p00000a))*
                                   as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        paste("Tasa Bruta: ",round(((input$ca1+ input$caa1+input$caaa1+input$caaaa1+input$caaaaa1+input$caaaaaa1+input$caaaaaaa1)/
                                      (input$pa1+input$paa1+input$paaa1+input$paaaa1+input$paaaaa1+input$paaaaaa1+input$paaaaaaa1))*
                                     as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      else{
        if (input$ngrupos == "8") {
          paste("Tasa Bruta: ",round(((input$ca_11x+input$ca1x+ input$caa1x+input$caaa1x+input$caaaa1x+input$caaaaa1x+input$caaaaaa1x+input$caaaaaaa1x)/
                                        (input$pa_11x+input$pa1x+input$paa1x+input$paaa1x+input$paaaa1x+input$paaaaa1x+input$paaaaaa1x+input$paaaaaaa1x))*
                                       as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      }}}
  
  
})
##############TABLAS POR EDADES ##############################
output$tabla1<-renderTable({
  if (input$ngrupos == "3")  {
    tablaedades<-matrix(c(input$c1,input$p1,(input$c1/input$p1)*as.numeric(input$tipotasa),(input$c1/input$p1)*input$peaaaa11,input$c2,input$p2,(input$c2/input$p2)*as.numeric(input$tipotasa),(input$c2/input$p2)*input$peaaaaa1,
                          input$c3,input$p3,(input$c3/input$p3)*as.numeric(input$tipotasa),(input$c3/input$p3)*input$peaaaaaa1),ncol=4,byrow = TRUE)
    
    g1<-paste("0-65")
    g2<-paste("66-75")
    g3<-paste("76 o mas")
    
    
    colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
    rownames(tablaedades) = c(g1,g2,g3)
    tablaedades}
  else{
    if (input$ngrupos == "5")  {
      tablaedades<-matrix(c(input$c0,input$p0,(input$c0/input$p0)*as.numeric(input$tipotasa),(input$c0/input$p0)*input$peaa11,
                            input$c00,input$p00,(input$c00/input$p00)*as.numeric(input$tipotasa),(input$c00/input$p00)*input$peaaa1,
                            input$c000,input$p000,(input$c000/input$p000)*as.numeric(input$tipotasa),(input$c000/input$p000)*input$peaaaa1,
                            input$c0000,input$p000,(input$c0000/input$p000)*as.numeric(input$tipotasa),(input$c0000/input$p000)*input$peaaaaa1,
                            input$c00000,input$p0000,(input$c00000/input$p0000)*as.numeric(input$tipotasa),(input$c00000/input$p0000)*input$peaaaaa1
      ),ncol=4,byrow = TRUE)
      
      g1<-paste("0-45")
      g2<-paste("46-55")
      g3<-paste("56-65")
      g4<-paste("66-75")
      g5<-paste("76 o mas")
      
      colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
      rownames(tablaedades) = c(g1,g2,g3,g4,g5)
      tablaedades}
    else{
      if (input$ngrupos == "7")  {
        tablaedades<-matrix(c(input$ca,input$pa,(input$ca/input$pa)*as.numeric(input$tipotasa),(input$ca/input$pa)*input$pe11,
                              input$caa,input$paa,(input$caa/input$paa)*as.numeric(input$tipotasa),(input$caa/input$paa)*input$pea1,
                              input$caaa,input$paaa,(input$caaa/input$paaa)*as.numeric(input$tipotasa),(input$caaa/input$paaa)*input$peaa1,
                              input$caaaa,input$paaa,(input$caaaa/input$paaa)*as.numeric(input$tipotasa),(input$caaaa/input$paaa)*input$peaaa1,
                              input$caaaaa,input$paaaa,(input$caaaaa/input$paaaa)*as.numeric(input$tipotasa),(input$caaaaa/input$paaaa)*input$peaaaa1,
                              input$caaaaaa,input$paaaaa,(input$caaaaaa/input$paaaaa)*as.numeric(input$tipotasa),(input$caaaaaa/input$paaaaa)*input$peaaaaa1,
                              input$caaaaaaa,input$paaaaaa,(input$caaaaaaa/input$paaaaaa)*as.numeric(input$tipotasa),(input$caaaaaaa/input$paaaaaa)*input$peaaaaaa1
        ),ncol=4,byrow = TRUE)
        
        g1<-paste("0-25")
        g2<-paste("26-35")
        g3<-paste("36-45")
        g4<-paste("46-55")
        g5<-paste("56-65")
        g6<-paste("66-75")
        g7<-paste("76 o mas")
        
        colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
        rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7)
        tablaedades}
      else{
        if (input$ngrupos == "8")  {
          tablaedades<-matrix(c(input$ca_1,input$pa_1,(input$ca_1/input$pa_1)*as.numeric(input$tipotasa),(input$ca_1/input$pa_1)*input$pe_11,
                                input$ca,input$pa,(input$ca/input$pa)*as.numeric(input$tipotasa),(input$ca/input$pa)*input$pe1,
                                input$caa,input$paa,(input$caa/input$paa)*as.numeric(input$tipotasa),(input$caa/input$paa)*input$pea1,
                                input$caaa,input$paa,(input$caaa/input$paa)*as.numeric(input$tipotasa),(input$caaa/input$paa)*input$peaa1,
                                input$caaaa,input$paaa,(input$caaaa/input$paaa)*as.numeric(input$tipotasa),(input$caaaa/input$paaa)*input$peaaa1,
                                input$caaaaa,input$paaaa,(input$caaaaa/input$paaaa)*as.numeric(input$tipotasa),(input$caaaaa/input$paaaa)*input$peaaaa1,
                                input$caaaaaa,input$paaaaa,(input$caaaaaa/input$paaaaa)*as.numeric(input$tipotasa),(input$caaaaaa/input$paaaaa)*input$peaaaaa1,
                                input$caaaaaaa,input$paaaaaa,(input$caaaaaaa/input$paaaaa)*as.numeric(input$tipotasa),(input$caaaaaaa/input$paaaaaa)*input$peaaaaaa1
          ),ncol=4,byrow = TRUE)
          g1<-paste("0-15")
          g2<-paste("16-25")
          g3<-paste("26-35")
          g4<-paste("36-45")
          g5<-paste("46-55")
          g6<-paste("56-65")
          g7<-paste("66-75")
          g8<-paste("76 o mas")
          
          colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
          rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7,g8)
          tablaedades}
      }}}
  
})
output$tabla2<-renderTable({
  if (input$ngrupos == "3")  {
    tablaedades<-matrix(c(input$c1a,input$p1a,(input$c1a/input$p1a)*as.numeric(input$tipotasa),
                          (input$c1a/input$p1a)*input$peaaaa11,
                          input$c2a,input$p2a,(input$c2a/input$p2a)*as.numeric(input$tipotasa),(input$c2a/input$p2a)*input$peaaaaa1,
                          input$c3a,input$p3a,(input$c3a/input$p3a)*as.numeric(input$tipotasa),(input$c3a/input$p3a)*input$peaaaaaa1),ncol=4,byrow = TRUE)
    
    g1<-paste("0-65")
    g2<-paste("66-75")
    g3<-paste("76 o mas")
    
    
    colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
    rownames(tablaedades) = c(g1,g2,g3)
    tablaedades}
  else{
    if (input$ngrupos == "5")  {
      tablaedades<-matrix(c(input$c0a,input$p0a,(input$c0a/input$p0a)*as.numeric(input$tipotasa),(input$c0a/input$p0a)*input$peaa11,
                            input$c00a,input$p00a,(input$c00a/input$p00a)*as.numeric(input$tipotasa),(input$c00a/input$p00a)*input$peaaa1,
                            input$c000a,input$p000a,(input$c000a/input$p000a)*as.numeric(input$tipotasa),(input$c000a/input$p000a)*input$peaaaa1,
                            input$c0000a,input$p0000a,(input$c0000a/input$p000a)*as.numeric(input$tipotasa),(input$c0000a/input$p000a)*input$peaaaaa1,
                            input$c00000a,input$p0000a,(input$c00000a/input$p0000a)*as.numeric(input$tipotasa),(input$c00000a/input$p0000a)*input$peaaaaa1
      ),ncol=4,byrow = TRUE)
      
      g1<-paste("0-45")
      g2<-paste("46-55")
      g3<-paste("56-65")
      g4<-paste("66-75")
      g5<-paste("76 o mas")
      
      colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
      rownames(tablaedades) = c(g1,g2,g3,g4,g5)
      tablaedades}
    else{
      if (input$ngrupos == "7")  {
        tablaedades<-matrix(c(input$ca1,input$pa1,(input$ca1/input$pa1)*as.numeric(input$tipotasa),(input$ca1/input$pa1)*input$pe11,
                              input$caa1,input$paa1,(input$caa1/input$paa1)*as.numeric(input$tipotasa),(input$caa1/input$paa1)*input$pea1,
                              input$caaa1,input$paaa1,(input$caaa1/input$paaa1)*as.numeric(input$tipotasa),(input$caaa1/input$paaa1)*input$peaa1,
                              input$caaaa1,input$paaa1,(input$caaaa1/input$paaa1)*as.numeric(input$tipotasa),(input$caaaa1/input$paaa1)*input$peaaa1,
                              input$caaaaa1,input$paaaa1,(input$caaaaa1/input$paaaa1)*as.numeric(input$tipotasa),(input$caaaaa1/input$paaaa1)*input$peaaaa1,
                              input$caaaaaa1,input$paaaaa1,(input$caaaaaa1/input$paaaaa1)*as.numeric(input$tipotasa),(input$caaaaaa1/input$paaaaa1)*input$peaaaaa1,
                              input$caaaaaaa1,input$paaaaaa1,(input$caaaaaaa1/input$paaaaaa1)*as.numeric(input$tipotasa),(input$caaaaaaa1/input$paaaaaa1)*input$peaaaaaa1
        ),ncol=4,byrow = TRUE)
        
        g1<-paste("0-25")
        g2<-paste("26-35")
        g3<-paste("36-45")
        g4<-paste("46-55")
        g5<-paste("56-65")
        g6<-paste("66-75")
        g7<-paste("76 o mas")
        
        colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
        rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7)
        tablaedades}
      else{
        if (input$ngrupos == "8")  {
          tablaedades<-matrix(c(input$ca_11x,input$pa_11x,(input$ca_11x/input$pa_11x)*as.numeric(input$tipotasa),(input$ca_11x/input$pa_11x)*input$pe_11,
                                input$ca1x,input$pa1x,(input$ca1x/input$pa1x)*as.numeric(input$tipotasa),(input$ca1x/input$pa1x)*input$pe1,
                                input$caa1x,input$paa1x,(input$caa1x/input$paa1x)*as.numeric(input$tipotasa),(input$caa1x/input$paa1x)*input$pea1,
                                input$caaa1x,input$paa1x,(input$caaa1x/input$paa1x)*as.numeric(input$tipotasa),(input$caaa1x/input$paa1x)*input$peaa1,
                                input$caaaa1x,input$paaa1x,(input$caaaa1x/input$paaa1x)*as.numeric(input$tipotasa),(input$caaaa1x/input$paaa1x)*input$peaaa1,
                                input$caaaaa1x,input$paaaa1x,(input$caaaaa1x/input$paaaa1x)*as.numeric(input$tipotasa),(input$caaaaa1x/input$paaaa1x)*input$peaaaa1,
                                input$caaaaaa1x,input$paaaaa1x,(input$caaaaaa1x/input$paaaaa1x)*as.numeric(input$tipotasa),(input$caaaaaa1x/input$paaaaa1x)*input$peaaaaa1,
                                input$caaaaaaa1x,input$paaaaaa1x,(input$caaaaaaa1x/input$paaaaa1x)*as.numeric(input$tipotasa),(input$caaaaaaa1x/input$paaaaaa1x)*input$peaaaaaa1
          ),ncol=4,byrow = TRUE)
          g1<-paste("0-15")
          g2<-paste("16-25")
          g3<-paste("26-35")
          g4<-paste("36-45")
          g5<-paste("46-55")
          g6<-paste("56-65")
          g7<-paste("66-75")
          g8<-paste("76 o mas")
          
          colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
          rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7,g8)
          tablaedades}
      }}}
  
})

output$gedad1<-renderText({
  paste( "tasas especificas por edad")
})
output$gedad2<-renderText({
  paste( "tasas especificas por edad")
})
output$comentario1<-renderText({
  paste( "(*) Ajustando por poblacion europea")
})
output$comentario2<-renderText({
  paste( "(*) Ajustando por poblacion europea ")
})
######################### TASAS AJUSTADAS ########################
output$ajustadas3<-renderText({
  if (input$ngrupos == "3")  {
    t1<-(((input$c1/input$p1)*input$peaaaa11)+((input$c2/input$p2)*input$peaaaaa1)+((input$c3/input$p3)*input$peaaaaaa1))/sumpee
    paste( "Tasa estandarizada por poblacion europea",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      
      t1<-(((input$c0/input$p0)*input$peaa11)+((input$c00/input$p00)*input$peaaa1)+
             ((input$c000/input$p000)*input$peaaaa1)+((input$c0000/input$p0000)*input$peaaaaa1)+
             ((input$c00000/input$p00000)*input$peaaaaaa1))/sumpee
      paste( "Tasa estandarizada por poblacion europea",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        
        t1<-(((input$ca/input$pa)*input$pe11)+((input$caa/input$paa)*input$pea1)+
               ((input$caaa/input$paaa)*input$peaa1)+((input$caaaa/input$paaaa)*input$peaaa1)+
               ((input$caaaaa/input$paaaaa)*input$peaaaa1)+((input$caaaaaa/input$paaaaaa)*input$peaaaaa1)+
               ((input$caaaaaaa/input$paaaaaaa)*input$peaaaaa1))/sumpee
        paste( "Tasa estandarizada por poblacion europea",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
      
      
      else{
        if (input$ngrupos == "8") {
          t1<-(((input$ca_1/input$pa_1)*input$pe_11)+
                 ((input$ca/input$pa)*input$pe1)+
                 ((input$caa/input$paa)*input$pea1)+
                 ((input$caaa/input$paaa)*input$peaa1)+
                 ((input$caaaa/input$paaaa)*input$peaaa1)+
                 ((input$caaaaa/input$paaaaa)*input$peaaaa1)+
                 ((input$caaaaaa/input$paaaaaa)*input$peaaaaa1)+
                 ((input$caaaaaaa/input$paaaaaaa)*input$peaaaaaa1))/sumpee
          paste( "Tasa estandarizada por poblacion europea",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
        
        
      }}}
})
output$ajustadas4<-renderText({
  if (input$ngrupos == "3")  {
    t2<-(((input$c1a/input$p1a)*input$peaaaa11)+((input$c2a/input$p2a)*input$peaaaaa1)+((input$c3a/input$p3a)*input$peaaaaaa1))/sumpee
    paste( "Tasa estandarizada por poblacion europea",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      
      t2<-(((input$c0a/input$p0a)*input$peaa11)+((input$c00a/input$p00a)*input$peaaa1)+
             ((input$c000a/input$p000a)*input$peaaaa1)+((input$c0000a/input$p0000a)*input$peaaaaa1)+
             ((input$c00000a/input$p00000a)*input$peaaaaaa1))/sumpee
      paste( "Tasa estandarizada por poblacion europea",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        
        t2<-(((input$ca1/input$pa1)*input$pe11)+((input$caa1/input$paa1)*input$pea1)+
               ((input$caaa1/input$paaa1)*input$peaa1)+((input$caaaa1/input$paaaa1)*input$peaaa1)+
               ((input$caaaaa1/input$paaaaa1)*input$peaaaa1)+((input$caaaaaa1/input$paaaaaa1)*input$peaaaaa1)+
               ((input$caaaaaaa1/input$paaaaaaa1)*input$peaaaaa1))/sumpee
        paste( "Tasa estandarizada por poblacion europea",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
      
      
      else{
        if (input$ngrupos == "8") {
          t2<-(((input$ca_11x/input$pa_11x)*input$pe_11)+
                 ((input$ca1x/input$pa1x)*input$pe1)+
                 ((input$caa1x/input$paa1x)*input$pea1)+
                 ((input$caaa1x/input$paa1x)*input$peaa1)+
                 ((input$caaaa1x/input$paaaa1x)*input$peaaa1)+
                 ((input$caaaaa1x/input$paaaaa1x)*input$peaaaa1)+
                 ((input$caaaaaa1x/input$paaaaaa1x)*input$peaaaaa1)+
                 ((input$caaaaaaa1x/input$paaaaaaa1x)*input$peaaaaaa1))/sumpee}
        paste( "Tasa estandarizada por poblacion europea",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)
        
        
      }}}
})

output$ric<-renderText({
  if (input$ngrupos == "3")  {
    t1<-(((input$c1/input$p1)*input$peaaaa11)+((input$c2/input$p2)*input$peaaaaa1)+((input$c3/input$p3)*input$peaaaaaa1))/sumpee
    t2<-(((input$c1a/input$p1a)*input$peaaaa11)+((input$c2a/input$p2a)*input$peaaaaa1)+((input$c3a/input$p3a)*input$peaaaaaa1))/sumpee}
  else{
    if (input$ngrupos == "5") {
      
      t1<-(((input$c0/input$p0)*input$peaa11)+((input$c00/input$p00)*input$peaaa1)+
             ((input$c000/input$p000)*input$peaaaa1)+((input$c0000/input$p0000)*input$peaaaaa1)+
             ((input$c00000/input$p00000)*input$peaaaaaa1))/sumpee
      t2<-(((input$c0a/input$p0a)*input$peaa11)+((input$c00a/input$p00a)*input$peaaa1)+
             ((input$c000a/input$p000a)*input$peaaaa1)+((input$c0000a/input$p0000a)*input$peaaaaa1)+
             ((input$c00000a/input$p00000a)*input$peaaaaaa1))/sumpee}
    else{
      if (input$ngrupos == "7") {
        
        t1<-(((input$ca/input$pa)*input$pe11)+((input$caa/input$paa)*input$pea1)+
               ((input$caaa/input$paaa)*input$peaa1)+((input$caaaa/input$paaaa)*input$peaaa1)+
               ((input$caaaaa/input$paaaaa)*input$peaaaa1)+((input$caaaaaa/input$paaaaaa)*input$peaaaaa1)+
               ((input$caaaaaaa/input$paaaaaaa)*input$peaaaaa1))/sumpee
        t2<-(((input$ca1/input$pa1)*input$pe11)+((input$caa1/input$paa1)*input$pea1)+
               ((input$caaa1/input$paaa1)*input$peaa1)+((input$caaaa1/input$paaaa1)*input$peaaa1)+
               ((input$caaaaa1/input$paaaaa1)*input$peaaaa1)+((input$caaaaaa1/input$paaaaaa1)*input$peaaaaa1)+
               ((input$caaaaaaa1/input$paaaaaaa1)*input$peaaaaa1))/sumpee}
      else{
        if (input$ngrupos == "8") {
          t1<-(((input$ca_1/input$pa_1)*input$pe_11)+((input$ca/input$pa)*input$pe1)+
                 ((input$caa/input$paa)*input$pea1)+((input$caaa/input$paaa)*input$peaa1)+
                 ((input$caaaa/input$paaaa)*input$peaaa1)+((input$caaaaa/input$paaaaa)*input$peaaaa1)+
                 ((input$caaaaaa/input$paaaaaa)*input$peaaaaa1)+((input$caaaaaa/input$paaaaaa)*input$peaaaaaa1))/sumpee
          
          t2<-(((input$ca_11x/input$pa_11x)*input$pe_11)+((input$ca1x/input$pa1x)*input$pe1)+
                 ((input$caa1x/input$paa1x)*input$pea1)+((input$caaa1x/input$paaa1x)*input$peaa1)+
                 ((input$caaaa1x/input$paaaa1x)*input$peaaa1)+((input$caaaaa1x/input$paaaaa1x)*input$peaaaa1)+
                 ((input$caaaaaa1x/input$paaaaaa1x)*input$peaaaaa1)+((input$caaaaaaa1x/input$paaaaaaa1x)*input$peaaaaaa1))/sumpee}
      }}}
  
  
  ric<-round(t1/t2,2)
  paste("Razon de incidencia comparativa (RIC tasa p1/tasa p2): ",ric)
})
#####################################################################################################
output$brutas1a<-renderText({
  if (input$ngrupos == "3")  {
    paste("Tasa Bruta: ",round(((input$c1+input$c2+input$c3)/(input$p1+input$p2+input$p3))*
                                 as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      paste("Tasa Bruta: ",round(((input$c0+ input$c00+input$c000+input$c0000+input$c00000)/
                                    (input$p0+input$p00+input$p000+input$p0000+input$p00000))*
                                   as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        paste("Tasa Bruta: ",round(((input$ca+ input$caa+input$caaa+input$caaaa+input$caaaaa+input$caaaaaa+input$caaaaaaa)/
                                      (input$pa+input$paa+input$paaa+input$paaaa+input$paaaaa+input$paaaaaa+input$paaaaaaa))*
                                     as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      else{
        if (input$ngrupos == "8") {
          paste("Tasa Bruta: ",round(((input$ca_1+input$ca+ input$caa+input$caaa+input$caaaa+input$caaaaa+input$caaaaaa+input$caaaaaaa)/
                                        (input$pa_1+input$pa+input$paa+input$paaa+input$paaaa+input$paaaaa+input$paaaaaa+input$paaaaaaa))*
                                       as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      }}}
  
  
})
output$brutas2a<-renderText({
  if (input$ngrupos == "3")  {
    paste("Tasa Bruta: ",round(((input$c1a+input$c2a+input$c3a)/(input$p1a+input$p2a+input$p3a))*
                                 as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      paste("Tasa Bruta: ",round(((input$c0a+ input$c00a+input$c000a+input$c0000a+input$c00000a)/
                                    (input$p0a+input$p00a+input$p000a+input$p0000a+input$p00000a))*
                                   as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        paste("Tasa Bruta: ",round(((input$ca1+ input$caa1+input$caaa1+input$caaaa1+input$caaaaa1+input$caaaaaa1+input$caaaaaaa1)/
                                      (input$pa1+input$paa1+input$paaa1+input$paaaa1+input$paaaaa1+input$paaaaaa1+input$paaaaaaa1))*
                                     as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      else{
        if (input$ngrupos == "8") {
          paste("Tasa Bruta: ",round(((input$ca_11x+input$ca1x+ input$caa1x+input$caaa1x+input$caaaa1x+input$caaaaa1x+input$caaaaaa1x+input$caaaaaaa1x)/
                                        (input$pa_11x+input$pa1x+input$paa1x+input$paaa1x+input$paaaa1x+input$paaaaa1x+input$paaaaaa1x+input$paaaaaaa1x))*
                                       as.numeric(input$tipotasa),1)," por ", input$tipotasa)}
      }}}
  
  
})
output$tabla1a<-renderTable({
  
  if (input$ngrupos == "3")  {
    tablaedades<-matrix(c(input$c1,input$p1,(input$c1/input$p1)*as.numeric(input$tipotasa),
                          (input$c1/input$p1)*input$pmaaaa11,
                          input$c2,input$p2,(input$c2/input$p2)*as.numeric(input$tipotasa),
                          (input$c2/input$p2)*input$pmaaaaa1,
                          input$c3,input$p3,(input$c3/input$p3)*as.numeric(input$tipotasa),
                          (input$c3/input$p3)*input$pmaaaaaa1),ncol=4,byrow = TRUE)
    
    g1<-paste("0-65")
    g2<-paste("66-75")
    g3<-paste("76 o mas")
    
    
    colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
    rownames(tablaedades) = c(g1,g2,g3)
    tablaedades}
  else{
    if (input$ngrupos == "5")  {
      tablaedades<-matrix(c(input$c0,input$p0,(input$c0/input$p0)*as.numeric(input$tipotasa),
                            (input$c0/input$p0)*input$pmaa11,
                            input$c00,input$p00,(input$c00/input$p00)*as.numeric(input$tipotasa),
                            (input$c00/input$p00)*input$pmaaa1,
                            input$c000,input$p000,(input$c000/input$p000)*as.numeric(input$tipotasa),
                            (input$c000/input$p000)*input$pmaaaa1,
                            input$c0000,input$p000,(input$c0000/input$p000)*as.numeric(input$tipotasa),
                            (input$c0000/input$p000)*input$pmaaaaa1,
                            input$c00000,input$p0000,(input$c00000/input$p0000)*as.numeric(input$tipotasa),
                            (input$c00000/input$p0000)*input$pmaaaaa1
      ),ncol=4,byrow = TRUE)
      
      g1<-paste("0-45")
      g2<-paste("46-55")
      g3<-paste("56-65")
      g4<-paste("66-75")
      g5<-paste("76 o mas")
      
      colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
      rownames(tablaedades) = c(g1,g2,g3,g4,g5)
      tablaedades}
    else{
      if (input$ngrupos == "7")  {
        tablaedades<-matrix(c(input$ca,input$pa,(input$ca/input$pa)*as.numeric(input$tipotasa),
                              (input$ca/input$pa)*input$pm11,
                              input$caa,input$paa,(input$caa/input$paa)*as.numeric(input$tipotasa),
                              (input$caa/input$paa)*input$pma1,
                              input$caaa,input$paaa,(input$caaa/input$paaa)*as.numeric(input$tipotasa),
                              (input$caaa/input$paaa)*input$pmaa1,
                              input$caaaa,input$paaa,(input$caaaa/input$paaa)*as.numeric(input$tipotasa),
                              (input$caaaa/input$paaa)*input$pmaaa1,
                              input$caaaaa,input$paaaa,(input$caaaaa/input$paaaa)*as.numeric(input$tipotasa),
                              (input$caaaaa/input$paaaa)*input$pmaaaa1,
                              input$caaaaaa,input$paaaaa,(input$caaaaaa/input$paaaaa)*as.numeric(input$tipotasa),
                              (input$caaaaaa/input$paaaaa)*input$pmaaaaa1,
                              input$caaaaaaa,input$paaaaaa,(input$caaaaaaa/input$paaaaaa)*as.numeric(input$tipotasa),
                              (input$caaaaaaa/input$paaaaaa)*input$pmaaaaaa1
        ),ncol=4,byrow = TRUE)
        
        g1<-paste("0-25")
        g2<-paste("26-35")
        g3<-paste("36-45")
        g4<-paste("46-55")
        g5<-paste("56-65")
        g6<-paste("66-75")
        g7<-paste("76 o mas")
        
        colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
        rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7)
        tablaedades}
      else{
        if (input$ngrupos == "8")  {
          tablaedades<-matrix(c(input$ca_1,input$pa_1,(input$ca_1/input$pa_1)*as.numeric(input$tipotasa),
                                (input$ca_1/input$pa_1)*input$pm_11,
                                input$ca,input$pa,(input$ca/input$pa)*as.numeric(input$tipotasa),
                                (input$ca/input$pa)*input$pm1,
                                input$caa,input$paa,(input$caa/input$paa)*as.numeric(input$tipotasa),
                                (input$caa/input$paa)*input$pma1,
                                input$caaa,input$paa,(input$caaa/input$paa)*as.numeric(input$tipotasa),
                                (input$caaa/input$paa)*input$pmaa1,
                                input$caaaa,input$paaa,(input$caaaa/input$paaa)*as.numeric(input$tipotasa),
                                (input$caaaa/input$paaa)*input$pmaaa1,
                                input$caaaaa,input$paaaa,(input$caaaaa/input$paaaa)*as.numeric(input$tipotasa),
                                (input$caaaaa/input$paaaa)*input$pmaaaa1,
                                input$caaaaaa,input$paaaaa,(input$caaaaaa/input$paaaaa)*as.numeric(input$tipotasa),
                                (input$caaaaaa/input$paaaaa)*input$pmaaaaa1,
                                input$caaaaaaa,input$paaaaaa,(input$caaaaaaa/input$paaaaa)*as.numeric(input$tipotasa),
                                (input$caaaaaaa/input$paaaaaa)*input$pmaaaaaa1
          ),ncol=4,byrow = TRUE)
          g1<-paste("0-15")
          g2<-paste("16-25")
          g3<-paste("26-35")
          g4<-paste("36-45")
          g5<-paste("46-55")
          g6<-paste("56-65")
          g7<-paste("66-75")
          g8<-paste("76 o mas")
          
          colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
          rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7,g8)
          tablaedades}
      }}}
  
})
output$tabla2a<-renderTable({
  if (input$ngrupos == "3")  {
    tablaedades<-matrix(c(input$c1a,input$p1a,(input$c1a/input$p1a)*as.numeric(input$tipotasa),
                          (input$c1a/input$p1a)*input$pmaaaa11,
                          input$c2a,input$p2a,(input$c2a/input$p2a)*as.numeric(input$tipotasa),(input$c2a/input$p2a)*input$pmaaaaa1,
                          input$c3a,input$p3a,(input$c3a/input$p3a)*as.numeric(input$tipotasa),(input$c3a/input$p3a)*input$pmaaaaaa1),ncol=4,byrow = TRUE)
    
    g1<-paste("0-65")
    g2<-paste("66-75")
    g3<-paste("76 o mas")
    
    
    colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
    rownames(tablaedades) = c(g1,g2,g3)
    tablaedades}
  else{
    if (input$ngrupos == "5")  {
      tablaedades<-matrix(c(input$c0a,input$p0a,(input$c0a/input$p0a)*as.numeric(input$tipotasa),(input$c0a/input$p0a)*input$pmaa11,
                            input$c00a,input$p00a,(input$c00a/input$p00a)*as.numeric(input$tipotasa),(input$c00a/input$p00a)*input$pmaaa1,
                            input$c000a,input$p000a,(input$c000a/input$p000a)*as.numeric(input$tipotasa),(input$c000a/input$p000a)*input$pmaaaa1,
                            input$c0000a,input$p0000a,(input$c0000a/input$p000a)*as.numeric(input$tipotasa),(input$c0000a/input$p000a)*input$pmaaaaa1,
                            input$c00000a,input$p0000a,(input$c00000a/input$p0000a)*as.numeric(input$tipotasa),(input$c00000a/input$p0000a)*input$pmaaaaa1
      ),ncol=4,byrow = TRUE)
      
      g1<-paste("0-45")
      g2<-paste("46-55")
      g3<-paste("56-65")
      g4<-paste("66-75")
      g5<-paste("76 o mas")
      
      colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
      rownames(tablaedades) = c(g1,g2,g3,g4,g5)
      tablaedades}
    else{
      if (input$ngrupos == "7")  {
        tablaedades<-matrix(c(input$ca1,input$pa1,(input$ca1/input$pa1)*as.numeric(input$tipotasa),(input$ca1/input$pa1)*input$pm11,
                              input$caa1,input$paa1,(input$caa1/input$paa1)*as.numeric(input$tipotasa),(input$caa1/input$paa1)*input$pma1,
                              input$caaa1,input$paaa1,(input$caaa1/input$paaa1)*as.numeric(input$tipotasa),(input$caaa1/input$paaa1)*input$pmaa1,
                              input$caaaa1,input$paaa1,(input$caaaa1/input$paaa1)*as.numeric(input$tipotasa),(input$caaaa1/input$paaa1)*input$pmaaa1,
                              input$caaaaa1,input$paaaa1,(input$caaaaa1/input$paaaa1)*as.numeric(input$tipotasa),(input$caaaaa1/input$paaaa1)*input$pmaaaa1,
                              input$caaaaaa1,input$paaaaa1,(input$caaaaaa1/input$paaaaa1)*as.numeric(input$tipotasa),(input$caaaaaa1/input$paaaaa1)*input$pmaaaaa1,
                              input$caaaaaaa1,input$paaaaaa1,(input$caaaaaaa1/input$paaaaaa1)*as.numeric(input$tipotasa),(input$caaaaaaa1/input$paaaaaa1)*input$pmaaaaaa1
        ),ncol=4,byrow = TRUE)
        
        g1<-paste("0-25")
        g2<-paste("26-35")
        g3<-paste("36-45")
        g4<-paste("46-55")
        g5<-paste("56-65")
        g6<-paste("66-75")
        g7<-paste("76 o mas")
        
        colnames(tablaedades) = c("casos2","poblacion2","tasa","Esperados (*)")
        rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7)
        tablaedades}
      else{
        if (input$ngrupos == "8")  {
          tablaedades<-matrix(c(input$ca_11x,input$pa_11x,(input$ca_11x/input$pa_11x)*as.numeric(input$tipotasa),(input$ca_11x/input$pa_11x)*input$pm_11,
                                input$ca1x,input$pa1x,(input$ca1x/input$pa1x)*as.numeric(input$tipotasa),(input$ca1x/input$pa1x)*input$pm1,
                                input$caa1x,input$paa1x,(input$caa1x/input$paa1x)*as.numeric(input$tipotasa),(input$caa1x/input$paa1x)*input$pma1,
                                input$caaa1x,input$paa1x,(input$caaa1x/input$paa1x)*as.numeric(input$tipotasa),(input$caaa1x/input$paa1x)*input$pmaa1,
                                input$caaaa1x,input$paaa1x,(input$caaaa1x/input$paaa1x)*as.numeric(input$tipotasa),(input$caaaa1x/input$paaa1x)*input$pmaaa1,
                                input$caaaaa1x,input$paaaa1x,(input$caaaaa1x/input$paaaa1x)*as.numeric(input$tipotasa),(input$caaaaa1x/input$paaaa1x)*input$pmaaaa1,
                                input$caaaaaa1x,input$paaaaa1x,(input$caaaaaa1x/input$paaaaa1x)*as.numeric(input$tipotasa),(input$caaaaaa1x/input$paaaaa1x)*input$pmaaaaa1,
                                input$caaaaaaa1x,input$paaaaaa1x,(input$caaaaaaa1x/input$paaaaa1x)*as.numeric(input$tipotasa),(input$caaaaaaa1x/input$paaaaaa1x)*input$pmaaaaaa1
          ),ncol=4,byrow = TRUE)
          g1<-paste("0-15")
          g2<-paste("16-25")
          g3<-paste("26-35")
          g4<-paste("36-45")
          g5<-paste("46-55")
          g6<-paste("56-65")
          g7<-paste("66-75")
          g8<-paste("76 o mas")
          
          colnames(tablaedades) = c("casos1","poblacion1","tasa","Esperados (*)")
          rownames(tablaedades) = c(g1,g2,g3,g4,g5,g6,g7,g8)
          tablaedades}
      }}}
  
})



output$gedad1a<-renderText({
  paste( "tasas especificas por edad")
})
output$gedad2a<-renderText({
  paste( "tasas especificas por edad")
})
output$comentario1a<-renderText({
  paste( "(*) Ajustando por poblacion mundial")
})
output$comentario2a<-renderText({
  paste( "(*) Ajustando por poblacion mundial")
})

output$ajustadas3a<-renderText({
  if (input$ngrupos == "3")  {
    t1<-(((input$c1/input$p1)*input$pmaaaa11)+((input$c2/input$p2)*input$pmaaaaa1)+((input$c3/input$p3)*input$pmaaaaaa1))/sumpee
    paste( "Tasa estandarizada por poblacion mundial",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      
      t1<-(((input$c0/input$p0)*input$pmaa11)+((input$c00/input$p00)*input$pmaaa1)+
             ((input$c000/input$p000)*input$pmaaaa1)+((input$c0000/input$p0000)*input$pmaaaaa1)+
             ((input$c00000/input$p00000)*input$pmaaaaaa1))/sumpee
      paste( "Tasa estandarizada por poblacion mundial",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        
        t1<-(((input$ca/input$pa)*input$pm11)+((input$caa/input$paa)*input$pma1)+
               ((input$caaa/input$paaa)*input$pmaa1)+((input$caaaa/input$paaaa)*input$pmaaa1)+
               ((input$caaaaa/input$paaaaa)*input$pmaaaa1)+((input$caaaaaa/input$paaaaaa)*input$pmaaaaa1)+
               ((input$caaaaaaa/input$paaaaaaa)*input$pmaaaaa1))/sumpee
        paste( "Tasa estandarizada por poblacion mundial",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
      
      
      else{
        if (input$ngrupos == "8") {
          t1<-(((input$ca_1/input$pa_1)*input$pm_11)+
                 ((input$ca/input$pa)*input$pm1)+
                 ((input$caa/input$paa)*input$pma1)+
                 ((input$caaa/input$paaa)*input$pmaa1)+
                 ((input$caaaa/input$paaaa)*input$pmaaa1)+
                 ((input$caaaaa/input$paaaaa)*input$pmaaaa1)+
                 ((input$caaaaaa/input$paaaaaa)*input$pmaaaaa1)+
                 ((input$caaaaaaa/input$paaaaaaa)*input$pmaaaaaa1))/sumpee
          paste( "Tasa estandarizada por poblacion mundial",round(t1,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
        
        
      }}}
})
output$ajustadas4a<-renderText({
  if (input$ngrupos == "3")  {
    t2<-(((input$c1a/input$p1a)*input$pmaaaa11)+((input$c2a/input$p2a)*input$pmaaaaa1)+((input$c3a/input$p3a)*input$pmaaaaaa1))/sumpee
    paste( "Tasa estandarizada por poblacion mundial",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
  else{
    if (input$ngrupos == "5") {
      
      t2<-(((input$c0a/input$p0a)*input$pmaa11)+((input$c00a/input$p00a)*input$pmaaa1)+
             ((input$c000a/input$p000a)*input$pmaaaa1)+((input$c0000a/input$p0000a)*input$pmaaaaa1)+
             ((input$c00000a/input$p00000a)*input$pmaaaaaa1))/sumpee
      paste( "Tasa estandarizada por poblacion mundial",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
    else{
      if (input$ngrupos == "7") {
        
        t2<-(((input$ca1/input$pa1)*input$pm11)+((input$caa1/input$paa1)*input$pma1)+
               ((input$caaa1/input$paaa1)*input$pmaa1)+((input$caaaa1/input$paaaa1)*input$pmaaa1)+
               ((input$caaaaa1/input$paaaaa1)*input$pmaaaa1)+((input$caaaaaa1/input$paaaaaa1)*input$pmaaaaa1)+
               ((input$caaaaaaa1/input$paaaaaaa1)*input$pmaaaaa1))/sumpee
        paste( "Tasa estandarizada por poblacion mundial",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
      
      
      else{
        if (input$ngrupos == "8") {
          t2<-(((input$ca_11x/input$pa_11x)*input$pm_11)+
                 ((input$ca1x/input$pa1x)*input$pm1)+
                 ((input$caa1x/input$paa1x)*input$pma1)+
                 ((input$caaa1x/input$paaa1x)*input$pmaa1)+
                 ((input$caaaa1x/input$paaaa1x)*input$pmaaa1)+
                 ((input$caaaaa1x/input$paaaaa1x)*input$pmaaaa1)+
                 ((input$caaaaaa1x/input$paaaaaa1x)*input$pmaaaaa1)+
                 ((input$caaaaaa1x/input$paaaaaa1x)*input$pmaaaaaa1))/sumpee
          paste( "Tasa estandarizada por poblacion mundial",round(t2,4)*as.numeric(input$tipotasa),"por ",input$tipotasa)}
        
        
      }}}
})
output$rica<-renderText({
  if (input$ngrupos == "3")  {
    t1<-(((input$c1/input$p1)*input$pmaaaa11)+((input$c2/input$p2)*input$pmaaaaa1)+((input$c3/input$p3)*input$pmaaaaaa1))/sumpee
    t2<-(((input$c1a/input$p1a)*input$pmaaaa11)+((input$c2a/input$p2a)*input$pmaaaaa1)+((input$c3a/input$p3a)*input$pmaaaaaa1))/sumpee}
  else{
    if (input$ngrupos == "5") {
      
      t1<-(((input$c0/input$p0)*input$pmaa11)+((input$c00/input$p00)*input$pmaaa1)+
             ((input$c000/input$p000)*input$pmaaaa1)+((input$c0000/input$p0000)*input$pmaaaaa1)+
             ((input$c00000/input$p00000)*input$pmaaaaaa1))/sumpee
      t2<-(((input$c0a/input$p0a)*input$pmaa11)+((input$c00a/input$p00a)*input$pmaaa1)+
             ((input$c000a/input$p000a)*input$pmaaaa1)+((input$c0000a/input$p0000a)*input$pmaaaaa1)+
             ((input$c00000a/input$p00000a)*input$pmaaaaaa1))/sumpee}
    else{
      if (input$ngrupos == "7") {
        
        t1<-(((input$ca/input$pa)*input$pm11)+((input$caa/input$paa)*input$pma1)+
               ((input$caaa/input$paaa)*input$pmaa1)+((input$caaaa/input$paaaa)*input$pmaaa1)+
               ((input$caaaaa/input$paaaaa)*input$pmaaaa1)+((input$caaaaaa/input$paaaaaa)*input$pmaaaaa1)+
               ((input$caaaaaaa/input$paaaaaaa)*input$pmaaaaa1))/sumpee
        t2<-(((input$ca1/input$pa1)*input$pm11)+((input$caa1/input$paa1)*input$pma1)+
               ((input$caaa1/input$paaa1)*input$pmaa1)+((input$caaaa1/input$paaaa1)*input$pmaaa1)+
               ((input$caaaaa1/input$paaaaa1)*input$pmaaaa1)+((input$caaaaaa1/input$paaaaaa1)*input$pmaaaaa1)+
               ((input$caaaaaaa1/input$paaaaaaa1)*input$pmaaaaa1))/sumpee}
      else{
        if (input$ngrupos == "8") {
          t1<-(((input$ca_1/input$pa_1)*input$pm_11)+
                 ((input$ca/input$pa)*input$pm1)+
                 ((input$caa/input$paa)*input$pma1)+
                 ((input$caaa/input$paaa)*input$pmaa1)+
                 ((input$caaaa/input$paaaa)*input$pmaaa1)+
                 ((input$caaaaa/input$paaaaa)*input$pmaaaa1)+
                 ((input$caaaaaa/input$paaaaaa)*input$pmaaaa1)+
                 ((input$caaaaaaa/input$paaaaaaa)*input$pmaaaaa1))/sumpee
          
          t2<-(((input$ca_11x/input$pa_11x)*input$pm_11)+
                 ((input$ca1x/input$pa1x)*input$pm1)+
                 ((input$caa1x/input$paa1x)*input$pma1)+
                 ((input$caaa1x/input$paaa1x)*input$pmaa1)+
                 ((input$caaaa1x/input$paaaa1x)*input$pmaaa1)+
                 ((input$caaaaa1x/input$paaaaa1x)*input$pmaaaa1)+
                 ((input$caaaaaa1x/input$paaaaaa1x)*input$pmaaaaa1)+
                 ((input$caaaaaaa1x/input$paaaaaaa1x)*input$pmaaaaaa1))/sumpee}
      }}}
  
  
  ric<-round(t1/t2,2)
  paste("Razon de incidencia comparativa (RIC tasa p1/tasa p2): ",ric)
})
#########################metodo indirecto ############################
output$param2 = renderUI({
  if (input$ngrupos2 == "3")  {
    
    fixedRow(
      column(width = 3, h4("POBLACION REFERENCIA"),numericInput("cr065","Casos 0-65",23),numericInput("cr6675","Casos 66-75",12),
             numericInput("cr76","Casos > 76",45)
             
      ),
      column(width = 1, h4("")
             
      ),
      column(width = 3,
             h4("POBLACION REFERENCIA"),numericInput("pr065","Personas 0-65",1000),numericInput("pr6675","Personas 66-75",1500),
             numericInput("pr76","Personas > 76",800)
      ),
      column(width = 1, h4("")
             
      ),
      column(width = 3,
             h4("POBLACION  DE ESTUDIO"),numericInput("pr065e","Personas 0-65",1000),numericInput("pr6675e","Personas 66-75",1500),
             numericInput("pr76e","Personas > 76",800)
      )
    )
    
  }else{
    if (input$ngrupos2 == "5") { 
      fixedRow(
        column(width = 3, h4("POBLACION  DE REFERENCIA"),numericInput("cr045","Casos 0-45",22),
               numericInput("cr4655","Casos 46-55",22),numericInput("cr5665","Casos 56-65",55),
               numericInput("cr6675_2","Casos 66-75",60),
               numericInput("cr76_2","Casos > 76",33)
               
        ),
        column(width = 1, h4("")
               
        ),
        column(width = 3, h4("POBLACION  DE REFERENCIA"),numericInput("pr045","Personas 0-45",234),
               numericInput("pr4655","Personas 46-55",1200),numericInput("pr5665","Personas 56-65",800),
               numericInput("pr6675_2","Personas 66-75",654),
               numericInput("pr76_2","Personas > 76",900)
        ),
        column(width = 1, h4("")
               
        ),
        column(width = 3,
               h4("POBLACION  DE ESTUDIO"),numericInput("pr045e","Personas 0-45",1000),
               numericInput("pr4655e","Personas 46-55",1500),
               numericInput("pr5665e","Personas 56-65",600),
               numericInput("pr6675e","Personas 66-75",800),
               numericInput("pr76e","Personas >76",700)
        )
      )
      
    }
    else{
      if(input$ngrupos2 == "7"){
        fixedRow(
          column(width =3, h4("POBLACION  DE REFERENCIA"),numericInput("cr025","casos 0-25",56),
                 numericInput("cr2635","Casos 26-35",43),numericInput("cr3645","Casos 36-45",54),
                 numericInput("cr4655_2","Casos 46-55",23),numericInput("cr5665_2","Casos 56-65",99),
                 numericInput("cr6675_3","Casos 66-75",33),
                 numericInput("cr76_3","Casos > 76",90)
                 
          ),
          column(width = 1, h4("")
                 
          ),
          column(width = 3, h4("POBLACION  DE REFERENCIA"),numericInput("pr025","Personas 0-25",656),
                 numericInput("pr2635","Personas 26-35",345),numericInput("pr3645","Personas 36-45",675),
                 numericInput("pr4655_2","Personas 46-55",234),numericInput("pr5665_2","Personas 56-65",432),
                 numericInput("pr6675_3","Personas 66-75",678),numericInput("pr76_3","Personas > 76",300)
          ),
          column(width = 1, h4("")
                 
          ),
          column(width = 3,
                 h4("POBLACION  DE ESTUDIO"),numericInput("pr025e","Personas 0-25",1000),
                 numericInput("pr2635e","Personas 26-35",900),
                 numericInput("pr3645e","Personas 36-45",1100),
                 numericInput("pr4655e","Personas 46-55",1000),
                 numericInput("pr5665e","Personas 56-65",600),
                 numericInput("pr6675e","Personas 66-75",800),
                 numericInput("pr76e","Personas 56-65",700)    
          )
          
          
        )
        
      }
      else{
        if(input$ngrupos2 == "8"){
          fixedRow(
            column(width = 3, h4("POBLACION  DE REFERENCIA"),numericInput("cr015","Casos 0-15",230),
                   numericInput("cr1625","Casos 16-25",156),
                   numericInput("cr2635_2","Casos 26-35",560),numericInput("cr3645_2","Casos 36-45",100),
                   numericInput("cr4655_3","Casos 46-55",230),numericInput("cr5665_3","Casos 56-65",150),
                   numericInput("cr6675_4","Casos 66-75",200),
                   numericInput("cr76_4","Casos > 76",145)
                   
            ),
            column(width = 1, h4("")
                   
            ),
            column(width = 3, h4("POBLACION  DE REFERENCIA"),numericInput("pr015","Personas 0-15",1000),numericInput("pr1625","Personas 16-25",789),
                   numericInput("pr2635_2","Personas 26-35",2234),numericInput("pr3645_2","Personas 36-45",1000),
                   numericInput("pr4655_3","Personas 46-55",2134),numericInput("pr5665_3","Personas 56-65",999),
                   numericInput("pr6675_4","Personas 66-75",2345),numericInput("pr76_4","Personas > 76",1560)
            ),
            column(width = 1, h4("")
                   
                   
            ),
            column(width = 3,
                   h4("POBLACION  DE ESTUDIO"),numericInput("pr015e","Personas 0-15",1000),
                   numericInput("pr1625e","Personas 16-25",1000),
                   numericInput("pr2635e","Personas 26-35",900),
                   numericInput("pr3645e","Personas 36-45",1100),
                   numericInput("pr4655e","Personas 46-55",1000),
                   numericInput("pr5665e","Personas 56-65",600),
                   numericInput("pr6675e","Personas 66-75",800),
                   numericInput("pr76e","Personas 56-65",700)     
            )
          )  
        }
        
      }}}
})

output$creferencia<-renderText({
  if (input$ngrupos2 == "3")  {
    paste("Casos en la poblacion de referencia: ",input$cr065+input$cr6675+input$cr76)
  }
  else{
    if (input$ngrupos2 == "5")  {
      paste("Casos en la poblacion de referencia: ",input$cr045+input$cr4655+input$cr5665+
              input$cr6675_2+input$cr76_2)
    }
    else{
      if (input$ngrupos2 == "7")  {
        paste("Casos en la poblacion de referencia: ",input$cr025+input$cr2635 +input$cr3645+
                input$cr4655_2+input$cr5665_2+input$cr6675_3+input$cr76_3)
      }
      else{
        if (input$ngrupos2 == "8")  {
          paste("Casos en la poblacion de referencia: ",input$cr015+input$cr1625+input$cr2635_2 +
                  input$cr3645_2+input$cr4655_3+input$cr5665_3+input$cr6675_4+input$cr76_4)
        }
      }}}
})
output$explicacion<-renderText({
  paste("Casos esperados si las tasas por edad fuesen iguales ")
})
output$tindirec<-renderTable({
  if (input$ngrupos2 == "5")  {
    q<-input$cr045
    q1<-input$cr4655
    q2<-input$cr5665
    q3<-input$cr6675_2
    q4<-input$cr76_2
    r<-input$pr045
    r1<-input$pr4655
    r2<-input$pr5665
    r3<-input$pr6675_2
    r4<-input$pr76_2
    s<-q/r
    s1<-q1/r1
    s2<-q2/r2
    s3<-q3/r3
    s4<-q4/r4
    t<-as.numeric(input$tipotasa)
    
    tablaindirec<-matrix(c(q,r,s*t,s*t*input$pr045e/t,
                           q1,r1,s1*t,s1*t*input$pr4655e/t,
                           q2,r2,s2*t,s2*t*input$pr5665e/t,
                           q3,r3,s3*t,s3*t*input$pr6675e/t,
                           q4,r4,s4*t,s4*t*input$pr76e/t),ncol=4,byrow = TRUE)
    g1<-paste("0-45")
    g2<-paste("46-55")
    g3<-paste("56-65")
    g4<-paste("66-75")
    g5<-paste("76 o mas")
    
    colnames(tablaindirec) = c("casos ref","poblacion ref","tasa ref","casos esperados")
    rownames(tablaindirec) = c(g1,g2,g3,g4,g5)
    tablaindirec}
  
  else{
    if (input$ngrupos2 == "3")  {
      q<-input$cr065
      q1<-input$cr6675
      q2<-input$cr76
      r<-input$pr065
      r1<-input$pr6675
      r2<-input$pr76
      s<-input$cr065/input$pr065
      s1<-q1/r1
      s2<-q2/r2
      t<-as.numeric(input$tipotasa)
      
      tablaindirec<-matrix(c(q,r,s*t,s*t*input$pr065e/t,
                             q1,r1,s1*t,s1*t*input$pr6675e/t,
                             q2,r2,s2*t,s2*t*input$pr76e/t),ncol=4,byrow = TRUE)
      g1<-paste("0-65")
      g2<-paste("66-75")
      g3<-paste("76 o mas")
      
      
      colnames(tablaindirec) = c("casos ref","poblacion ref","tasa ref","casos esperados")
      rownames(tablaindirec) = c(g1,g2,g3)
      tablaindirec}
    else{ 
      if (input$ngrupos2 == "7")  {
        q<-input$cr025
        q1<-input$cr2635
        q2<-input$cr3645
        q3<-input$cr4655_2
        q4<-input$cr5665_2
        q5<-input$cr6675_3
        q6<-input$cr76_3
        r<-input$pr025
        r1<-input$pr2635
        r2<-input$pr3645
        r3<-input$pr4655_2
        r4<-input$pr5665_2
        r5<-input$pr6675_3
        r6<-input$pr76_3
        s<-q/r
        s1<-q1/r1
        s2<-q2/r2
        s3<-q3/r3
        s4<-q4/r4
        s5<-q5/r5
        s6<-q6/r6
        t<-as.numeric(input$tipotasa)
        
        tablaindirec<-matrix(c(q,r,s*t,s*t*input$pr025e/t,
                               q1,r1,s1*t,s1*t*input$pr2635e/t,
                               q2,r2,s2*t,s2*t*input$pr3645e/t,
                               q3,r3,s3*t,s3*t*input$pr4655e/t,
                               q4,r4,s4*t,s4*t*input$pr5665e/t,
                               q5,r5,s5*t,s5*t*input$pr6675e/t,
                               q6,r6,s6*t,s6*t*input$pr76e/t),ncol=4,byrow = TRUE)
        g1<-paste("0-25")
        g2<-paste("26-35")
        g3<-paste("36-45")
        g4<-paste("46-55")
        g5<-paste("56-65")
        g6<-paste("66-75")
        g7<-paste("76 o mas")
        
        colnames(tablaindirec) = c("casos ref","poblacion ref","tasa ref","casos esperados")
        rownames(tablaindirec) = c(g1,g2,g3,g4,g5,g6,g7)
        tablaindirec}
      else{ 
        if (input$ngrupos2 == "8")  {
          q<-input$cr015
          q1<-input$cr1625
          q2<-input$cr2635_2
          q3<-input$cr3645_2
          q4<-input$cr4655_3
          q5<-input$cr5665_3
          q6<-input$cr6675_4
          q7<-input$cr76_4
          
          r<-input$pr015
          r1<-input$pr1625
          r2<-input$pr2635_2
          r3<-input$pr3645_2
          r4<-input$pr4655_3
          r5<-input$pr5665_3
          r6<-input$pr6675_4
          r7<-input$pr76_4
          s<-q/r
          s1<-q1/r1
          s2<-q2/r2
          s3<-q3/r3
          s4<-q4/r4
          s5<-q5/r5
          s6<-q6/r6
          s7<-q7/r7
          t<-as.numeric(input$tipotasa)
          
          tablaindirec<-matrix(c(q,r,s*t,s*t*input$pr015e/t,
                                 q1,r1,s1*t,s1*t*input$pr1625e/t,
                                 q2,r2,s2*t,s2*t*input$pr2635e/t,
                                 q3,r3,s3*t,s3*t*input$pr3645e/t,
                                 q4,r4,s4*t,s4*t*input$pr4655e/t,
                                 q5,r5,s5*t,s5*t*input$pr5665e/t,
                                 q6,r6,s6*t,s6*t*input$pr6675e/t,
                                 q7,r7,s7*t,s7*t*input$pr76e/t),ncol=4,byrow = TRUE)
          g1<-paste("0-15")
          g2<-paste("16-25")
          g3<-paste("26-35")
          g4<-paste("36-45")
          g5<-paste("46-55")
          g6<-paste("56-65")
          g7<-paste("66-75")
          g8<-paste("76 o mas")
          
          colnames(tablaindirec) = c("casos ref","poblacion ref","tasa ref","casos esperados")
          rownames(tablaindirec) = c(g1,g2,g3,g4,g5,g6,g7,g8)
          tablaindirec}
      }}}
})
output$texindirect<-renderText({
  if (input$ngrupos2 == "3")  {
    q<-input$cr065
    q1<-input$cr6675
    q2<-input$cr76
    r<-input$pr065
    r1<-input$pr6675
    r2<-input$pr76
    s<-input$cr065/input$pr065
    s1<-q1/r1
    s2<-q2/r2
    t<-as.numeric(input$tipotasa)
    
    cepe<-(s*t*input$pr065e/t+s1*t*input$pr6675e/t+s2*t*input$pr76e/t)
    
    paste("Casos esperados en poblacion de estudio:  ",round(cepe,2))}
  else{
    if (input$ngrupos2 == "5")  {
      q<-input$cr045
      q1<-input$cr4655
      q2<-input$cr5665
      q3<-input$cr6675_2
      q4<-input$cr76_2
      r<-input$pr045
      r1<-input$pr4655
      r2<-input$pr5665
      r3<-input$pr6675_2
      r4<-input$pr76_2
      s<-q/r
      s1<-q1/r1
      s2<-q2/r2
      s3<-q3/r3
      s4<-q4/r4
      t<-as.numeric(input$tipotasa)
      
      cepe<-(s*t*input$pr045e/t+s1*t*input$pr4655e/t+s2*t*input$pr5665e/t+
               s3*t*input$pr6675e/t+s4*t*input$pr76e/t  )
      
      paste("Casos esperados en poblacion de estudio:  ",round(cepe,2))}
    else{
      if (input$ngrupos2 == "7")  {
        q<-input$cr025
        q1<-input$cr2635
        q2<-input$cr3645
        q3<-input$cr4655_2
        q4<-input$cr5665_2
        q5<-input$cr6675_3
        q6<-input$cr76_3
        r<-input$pr025
        r1<-input$pr2635
        r2<-input$pr3645
        r3<-input$pr4655_2
        r4<-input$pr5665_2
        r5<-input$pr6675_3
        r6<-input$pr76_3
        s<-q/r
        s1<-q1/r1
        s2<-q2/r2
        s3<-q3/r3
        s4<-q4/r4
        s5<-q5/r5
        s6<-q6/r6
        t<-as.numeric(input$tipotasa)
        
        cepe<-(s*t*input$pr025e/t+s1*t*input$pr2635e/t+s2*t*input$pr3645e/t+
                 s3*t*input$pr4655e/t+s4*t*input$pr5665e/t+ 
                 s5*t*input$pr6675e/t+s6*t*input$pr76e/t)
        
        paste("Casos esperados en poblacion de estudio:  ",round(cepe,2))}
      else{
        if (input$ngrupos2 == "8")  {
          q<-input$cr015
          q1<-input$cr1625
          q2<-input$cr2635_2
          q3<-input$cr3645_2
          q4<-input$cr4655_3
          q5<-input$cr5665_3
          q6<-input$cr6675_4
          q7<-input$cr76_4
          
          r<-input$pr015
          r1<-input$pr1625
          r2<-input$pr2635_2
          r3<-input$pr3645_2
          r4<-input$pr4655_3
          r5<-input$pr5665_3
          r6<-input$pr6675_4
          r7<-input$pr76_4
          s<-q/r
          s1<-q1/r1
          s2<-q2/r2
          s3<-q3/r3
          s4<-q4/r4
          s5<-q5/r5
          s6<-q6/r6
          s7<-q7/r7
          t<-as.numeric(input$tipotasa)
          
          cepe<-(s*t*input$pr015e/t+s1*t*input$pr1625e/t+s2*t*input$pr2635e/t+
                   s3*t*input$pr3645e/t+s4*t*input$pr4655e/t+ 
                   s5*t*input$pr5665e/t+s6*t*input$pr6675e/t+s7*t*input$pr76e/t)
          
          paste("Casos esperados en poblacion de estudio:  ",round(cepe,2))}
      }}}
  
})
output$texindirect2<-renderText({
  if (input$ngrupos2 == "3")  {
    q<-input$cr065
    q1<-input$cr6675
    q2<-input$cr76
    r<-input$pr065
    r1<-input$pr6675
    r2<-input$pr76
    s<-input$cr065/input$pr065
    s1<-q1/r1
    s2<-q2/r2
    t<-as.numeric(input$tipotasa)
    
    cepe<-(s*t*input$pr065e/t+s1*t*input$pr6675e/t+s2*t*input$pr76e/t)
    
    paste("Razon de incidencia estandarizada  ",round((input$casos1/cepe),2))}
  else{
    if (input$ngrupos2 == "5")  {
      q<-input$cr045
      q1<-input$cr4655
      q2<-input$cr5665
      q3<-input$cr6675_2
      q4<-input$cr76_2
      r<-input$pr045
      r1<-input$pr4655
      r2<-input$pr5665
      r3<-input$pr6675_2
      r4<-input$pr76_2
      s<-q/r
      s1<-q1/r1
      s2<-q2/r2
      s3<-q3/r3
      s4<-q4/r4
      t<-as.numeric(input$tipotasa)
      
      cepe<-(s*t*input$pr045e/t+s1*t*input$pr4655e/t+s2*t*input$pr5665e/t+
               s3*t*input$pr6675e/t+s4*t*input$pr76e/t  )
      
      paste("Razon de incidencia estandarizada  ",round((input$casos1/cepe),2))}
    else{
      if (input$ngrupos2 == "7")  {
        q<-input$cr025
        q1<-input$cr2635
        q2<-input$cr3645
        q3<-input$cr4655_2
        q4<-input$cr5665_2
        q5<-input$cr6675_3
        q6<-input$cr76_3
        r<-input$pr025
        r1<-input$pr2635
        r2<-input$pr3645
        r3<-input$pr4655_2
        r4<-input$pr5665_2
        r5<-input$pr6675_3
        r6<-input$pr76_3
        s<-q/r
        s1<-q1/r1
        s2<-q2/r2
        s3<-q3/r3
        s4<-q4/r4
        s5<-q5/r5
        s6<-q6/r6
        t<-as.numeric(input$tipotasa)
        cepe<-(s*t*input$pr025e/t+s1*t*input$pr2635e/t+s2*t*input$pr3645e/t+
                 s3*t*input$pr4655e/t+s4*t*input$pr5665e/t+ 
                 s5*t*input$pr6675e/t+s6*t*input$pr76e/t)
        paste("Razon de incidencia estandarizada  ",round((input$casos1/cepe),2))}
      else{
        if (input$ngrupos2 == "8")  {
          q<-input$cr015
          q1<-input$cr1625
          q2<-input$cr2635_2
          q3<-input$cr3645_2
          q4<-input$cr4655_3
          q5<-input$cr5665_3
          q6<-input$cr6675_4
          q7<-input$cr76_4
          
          r<-input$pr015
          r1<-input$pr1625
          r2<-input$pr2635_2
          r3<-input$pr3645_2
          r4<-input$pr4655_3
          r5<-input$pr5665_3
          r6<-input$pr6675_4
          r7<-input$pr76_4
          s<-q/r
          s1<-q1/r1
          s2<-q2/r2
          s3<-q3/r3
          s4<-q4/r4
          s5<-q5/r5
          s6<-q6/r6
          s7<-q7/r7
          t<-as.numeric(input$tipotasa)
          
          cepe<-(s*t*input$pr015e/t+s1*t*input$pr1625e/t+s2*t*input$pr2635e/t+
                   s3*t*input$pr3645e/t+s4*t*input$pr4655e/t+ 
                   s5*t*input$pr5665e/t+s6*t*input$pr6675e/t+s7*t*input$pr76e/t)
          
          paste("Razon de incidencia estandarizada  ",round((input$casos1/cepe),2))}
      }}}
})
output$salida<-renderText({
  paste("Casos en la poblacion de estudio: ",input$casos1 )
})
})
