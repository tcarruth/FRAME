

Data_trim<-function(Data){

  DataT<-Data
  orglength<-length(Data@Year)
  ind<-(1:length(Data@Year))[Data@Year<(Data1@LHYear+1)]
  newlength<-length(ind)
  slots<-slotNames(Data)

  for(i in 1:length(slots)){
    temp<-slot(Data,slots[i])
    if(orglength%in%dim(temp)){
      dims<-dim(temp)
      ndim<-length(dims)
      if(ndim==2){
        slot(DataT,slots[i])<-array(slot(Data,slots[i])[,ind],c(dim(temp)[1],newlength))
      }else if(ndim==3){
        slot(DataT,slots[i])<-array(slot(Data,slots[i])[,ind,],c(dim(temp)[1],newlength,dim(temp)[3]))
      }
    }
  }

  DataT

}
