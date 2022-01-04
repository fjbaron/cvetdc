separaLineas<- function(texto){
  resultado=str_split(texto,"\n") %>% unlist() %>% str_trim() %>% .[str_length(.)>=1]
  if(length(resultado)==0) resultado=""
  resultado
}

extraePartes <- function(patron,texto){
  expresionRegular=str_c("^",patron)
  expresionRegular=regex(str_c("^",patron), ignore_case = TRUE)
  texto %>% keep(~str_detect(.,expresionRegular)) %>%
    str_replace(.,expresionRegular,"") %>%
    str_trim() %>% separaLineas() %>%
    keep(function(.x){str_trim(.x)!=""})
}



extraePregunta <-function(texto){
  partes=texto %>% str_split("###") %>% .[[1]] %>% str_trim()
  referencia=partes[1]
  campos=c("iniciosVacios","enunciadosEquivalentes","finalesVacios","opcionesValidas","opcionesInvalidas","numOpciones","numValidas")
  lista=list(campos %>% set_names(campos) %>%
               map(extraePartes %>%
                     partial(texto=partes[-1]))  %>%
               append(list(referencia=referencia))) %>%
    set_names(referencia)
  if(!is.null(lista$numOpciones) & length(lista$numOpciones)>0) lista$numOpciones=as.integer(as.numeric(lista$numOpciones))
  if(!is.null(lista$numvalidas) & length(lista$numValidas)>0) lista$numValidas=as.integer(as.numeric(lista$numValidas))
  lista$opcionesValidas=lista$opcionesValidas %>% keep(~{str_trim(.x)!=""})
  lista$opcionesInvalidas=lista$opcionesInvalidas %>% keep(~{str_trim(.x)!=""})
  lista$enunciadosEquivalentes=lista$enunciadosEquivalentes %>% keep(~{str_trim(.x)!=""})
  lista
}





maquetaOpcionXML=function(validas,invalidas,OK,KO,opcionNula=NA_character_){
  opciones=c(validas,invalidas)
  if(!is.na(opcionNula) & str_length(str_trim(opcionNula))>4)  opciones=c(opciones,opcionNula)

  opcionesXML=sprintf("<answer fraction=\"%s\" format=\"html\">
      <text><![CDATA[%s]]></text>
      <feedback format=\"html\">
        <text></text>
      </feedback>
    </answer>
",case_when(opciones %in% validas ~ OK,
            opciones %in% invalidas ~ KO,
            TRUE ~ 0), opciones)
  opcionesXML %>% sample(replace=FALSE) %>% paste0(collapse="")
}



generaCombinaciones=function(  numColumnas,numOpciones){
  #  message("Me llamaron")
  # print(numOpciones)
  numColumnas=as.numeric(numColumnas)
  tmp=data.frame(v1=1:(numOpciones-numColumnas+1))
  if(numColumnas>1){
    for (j in 2:numColumnas){
      vj=data.frame(j:(numOpciones-numColumnas+j)) %>% set_names(sprintf("v%d",j))
      tmp=tmp %>% crossing(vj)
      nfilas=nrow(tmp)
      filtrar=rep(TRUE,nfilas)
      for (i in 1:nfilas){
        filtrar[i]=filtrar[i] && tmp[i,j]>tmp[i,j-1]
      }
      tmp=tmp[filtrar,]
    }
  }
  #print(tmp)
  tmp %>% as_tibble()
}




todasLasValidas <- function(numValidas,opciones){
  nOpciones=length(opciones)
  numValidas=min(nOpciones,numValidas)
  resultado=generaCombinaciones(numValidas,nOpciones)
  resultado$nv=apply(resultado,1,function(.x)list(unlist(keep(.x,function(x)!is.na(x))) %>%  set_names(NULL)))
  resultado %>% select(nv) %>% mutate(elegidas=pmap(.,function(nv)opciones[nv[[1]]])) %>% select(elegidas)
}


puntos=function(vector,...){
  ifelse(length(vector)==0,0,100/length(vector))
}

puntosKO=function(invalidas,...){
  ifelse(length(invalidas)==0,0,-100/length(invalidas))
}




preguntasHTML=function(dfPreguntas){
  dfPreguntas %>% mutate(texto=pmap(.,maqueta))
}

