cumError = function(sy, freqFile, i)
{
	temp = read.csv(freqFile)
	obsY = temp$Freq
	sum((sy[1:(2*i)]-obsY[1:(2*i)])^2)
}


simrep = function(tp = matrix(c(0.03,0.03,0.03,0.03,0.03,0.03),nrow=3,ncol=2),
                     ve = matrix(c(0.01,0.01,0.01,0.01,0.01,0.01),nrow=3,ncol=2),
                     postfix = "",
                     var = FALSE,
                     para)
{
  para$prob_transmission = tp
  para$prob_transmission_vac = ve
  sim.title = gsub(" ", "_",para$title)

  simflu6(para, verbose= FALSE, postfix = postfix)
  require(data.table)
  if (var==FALSE) {
    inc.file = sprintf(paste(getwd(),"/","%s_Incidence_Monthly_overall_%s.csv",sep = ""),sim.title,postfix)
    restab = read.csv(file=inc.file)
    R = restab$Inc3
  }
  else {
    inc.file = sprintf(paste(getwd(),"/","%s_Incidence_Monthly_each_Sim_%s.csv",sep = ""),sim.title,postfix)
    data = data.table(read.csv(file=inc.file))
    restab=data.frame(data[,list(N=mean(N),Inc2=mean(Inc2), Inc2_var = var(Inc2),
                                 Rate2=mean(Rate2),Rate2_var=var(Rate2),Inc3=mean(Inc3),
                                 Inc3_var = var(Inc3), Rate3=mean(Rate3),Rate3_var=var(Rate3),
                                 IncT=mean(IncT), IncT_var = var(IncT), RateT=mean(RateT),
                                 RateT_var=var(RateT)),by=list(Month,Year,Stratum,V)])
    #R = data.frame(Inc3=restab$Inc3,Inc3_var=restab$Inc3_var)
    R = c(restab$Inc3,restab$Inc3_var)
  }
  system(sprintf("rm \"%s\"",inc.file))
  return(R)
}




fluesim = function(para, filename, freqfile, clusternumber = 40, type = "grid", gridsize = seq(0.0,0.25,0.005), var = FALSE)
{
	require(foreach)
	require(doSNOW)
	cluster = makeCluster(clusternumber, type = "SOCK")
	registerDoSNOW(cluster)

  tp0 = matrix(0.1,nrow=para$months,ncol=para$strata)
  tp1 = matrix(0.1,nrow=para$months,ncol=para$strata)

	p0.gr = gridsize
	p1.gr = gridsize
	gr.mn = length(p0.gr)*length(p1.gr)
	sims = matrix(NA,nrow= (para$months*gr.mn),ncol=para$months*para$strata*4+1)
	
	# sims columns
	# frequencies for each month and stratum [num of columns: :nStrata * nMonths * 2]
	# tp0 [num of columns: :nStrata * nMonths]
	# tp1 [num of columns: :nStrata * nMonths]
	# e [one column]

	start = proc.time()
	for(i in seq(1,para$months)){
		simY = foreach(p0=p0.gr, .combine=rbind) %:%
          foreach(p1=p1.gr, .combine=rbind) %dopar% {
            tp0[i] = p0
            tp1[i] = p1
            ve = 1 - tp1/tp0
	          pf = paste0(p0,p1)
            pf = paste0(pf,ceiling(runif(1,1,100000)))
            sY = simrep(tp=tp0,ve=ve,postfix=pf,var=var,para=para)
            e = cumError(sY[1:(para$months*2)],freqfile, i)
            s = c(sY,tp0,tp1,e)
		      }
	    ind = which.min(simY[,ncol(simY)])
	    tp0[i] = simY[ind,((2*para$months)+i)]
	    tp1[i] = simY[ind,((3*para$months)+i)]
	    print(i)
	    print(proc.time() - start)
	    sims[(gr.mn*(i-1)+1):(gr.mn*i),] = simY
	    write.csv(sims, filename, row.names=FALSE)
	}
	times = proc.time() - start
	stopCluster(cluster)

}
