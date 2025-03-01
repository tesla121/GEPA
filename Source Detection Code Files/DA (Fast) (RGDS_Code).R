#Give paths to input underlying graphs and infection graphs
#Set the working directory
file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")

library(igraph)
library(RSpectra)

#Code for Dynamic Age (DA)
shift_list_estimated=c()
DA_FB_Ht_2=list() 
DA_sum=0
i=1
while(i<=length(file_list_LIG)){
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[i],sep=''))

	source=V(g)[1]$label
	sg=g
	A=as.matrix(get.adjacency(sg)) 
	l=list()


	func=function(A,j){
			tryCatch(
		{return(abs(ev-eigs(A[-j,-j],1,which="LM")$values)/ev)}
		,
		error=function(e){
			return(abs(ev-max(eigen(A[-j,-j])$values))/ev)
		}
		)
	}

	j=1
	ev=eigs(A,1,which="LM")$values
	while(j<=nrow(A)){
		l[j]=func(A,j)
		
	j=j+1
}
	l=unlist(l)
	estimated_DA=V(sg)[as.numeric(which(max(l)==l))]$label

	d_DA=distances(sg, v = (estimated_DA), to = (source), mode = c("all"), weights=NULL, algorithm = c("unweighted"))

	samp=sample(length(d_DA),1)
	DA_sum=DA_sum+as.numeric(d_DA[samp])
	DA_FB_Ht_2[[i]]=as.numeric(d_DA[samp])
		
	i=i+1

	centers=names(which(min(sort((eccentricity(sg, vids = V(sg), mode = c("all")))))==sort((eccentricity(sg, vids = V(sg), mode = c("all"))))))
	est_source=dimnames(d_DA)[[1]][samp]
	shift=distances(sg, v = as.character(centers), to = as.character(est_source) , mode = c("all"), weights=NULL, algorithm = c("unweighted"))

	shift_list_estimated=append(shift_list_estimated,ceiling(min(shift)))
}



df=as.data.frame(cbind(file_list_LIG,unlist(DA_FB_Ht_2),shift_list_estimated))
colnames(df)=c('graph','DA','shift_list_estimated')


write.csv(df,"./Covid 19 Results DA.csv")
