#Crespo Bravo Gerardo
#Programa para resolver el problema de la rata y el laberinto 

library(markovchain)

Plab<-array(c(0,1/3,0,1/3,0,0,0,0,0,.5,0,.5,0,.25,0,0,0,0,0,1/3,0,
              0,0,1/3,0,0,0,.5,0,0,0,.25,0,.5,0,0,0,1/3,0,1/3,0,1/3,
              0,1/3,0,0,0,.5,0,.25,0,0,0,.5,0,0,0,1/3,0,0,0,1/3,0,0,
              0,0,0,.25,0,.5,0,.5,0,0,0,0,0,1/3,0,1/3,0),dim=c(9,9))
Plab

#estados
Labstates=as.character(seq(1:9))
Lab<-new("markovchain",transitionMatrix=Plab,states=Labstates,name="Laberinto")


Lab.TLlegada<-function(n,l,Lab){
  T<-numeric(n)
  for(i in 1:n){
    visita<-0
    pasos<-0
    e<-l
    while(visita==0){
      #Simulamos un paso sobre la cadena partiendo de e
      e<-rmarkovchain(1,Lab,t0=e)
      pasos<-pasos+1
      if(e=="9"){visita<-1;T[i]<-pasos}
    }
  }
  sum(T)/n
}


Lab.TLlegada(1000,"1",Lab)
Lab.TLlegada(1000,"2",Lab)
Lab.TLlegada(1000,"3",Lab)
Lab.TLlegada(1000,"4",Lab)
Lab.TLlegada(1000,"5",Lab)
Lab.TLlegada(1000,"6",Lab)
Lab.TLlegada(1000,"7",Lab)
Lab.TLlegada(1000,"8",Lab)
Lab.TLlegada(1000,"9",Lab)


##Pregunta 1b)

# transición en "n" pasos
n=100
Lab^n



##Pregunta 1c)

##Respuesta: Si, por que todos los estados son recurrentes positivos, eso quiere 
##decir que si vamos a regresar en un periodo finito de tiempo al mismo estado.

library(Biodem)
n=100
Matriz_n_pasos=mtx.exp(Plab, n)
Matriz_n_pasos

rowSums(Matriz_n_pasos) #Para ver que suma 1 en cada renglon 


##Pregunta 1d)

Lab.NR_antesDComida<-function(n,l,Lab){
  for(i in 1:n){
    visita<-0
    pasos<-0
    regresos<-0
    e<-l
    while(visita==0){
      #Simulamos un paso sobre la cadena partiendo de e    {visita<-1;T[i]<-pasos}
      #print(e) Para ver todos activese esta parte del codigo 
      e<-rmarkovchain(1,Lab,t0=e)
      
      pasos<-pasos+1
      if(e=="9"){
        visita<-1}else{
          if(e==l){ 
            regresos<-regresos +1 }else{
            }
        }
    }
  }
  
  print(pasos)
  print(regresos)
  regresos/n
  
}

Lab.NR_antesDComida(1000,"1",Lab)