rename_cols<-function(tabla){
  colnames(tabla)<-gsub(x = colnames(tabla),pattern = "\\º",replacement = "um")
  colnames(tabla)<-gsub(x = colnames(tabla),pattern = "\\+",replacement = "pos")
  colnames(tabla)<-gsub(x = colnames(tabla),pattern = "([[:blank:]]|[[:punct:]])",replacement = "_")
  colnames(tabla)<-tolower(colnames(tabla))
  return(tabla)
}

AcB<-function(lista1,lista2){
  tabla3<-c(lista1,lista2)
  todos<-unique(tabla3)
  lista1[lista1%in%lista2]->uno_in_two
  lista2[lista2%in%lista1]->two_in_one
  iguales<-c(uno_in_two,two_in_one)
  iguales<-unique(iguales)
  distintos<-tabla3[(tabla3%in%iguales)==FALSE]
  
  lista<-list("todos"=todos,"iguales"=iguales,"distintos"=distintos)
  return(lista)
}
