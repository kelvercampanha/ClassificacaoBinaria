#Importe de pacote
library(AUC)
library(caret)

#Exemplo com dados sintéticos
#Na prática, y é a resposta original e adj o output de algum modelo
set.seed(123)#Semente para reprodutibilidade do exemplo
adj=runif(500,0,1)
y=rbinom(500,1,adj)

#Definindo a função
P2Class=function(y,adj)
{
 dev.new()
 #Gráfico da curva ROC
 plot(roc(adj,as.factor(y)),col="blue",lwd=2, main="ROC curve")
 ROCc=auc(roc(adj,as.factor(y)))#Estimativa pontual do AUC

 #Cálculo do KS (máxima diferença entre as curvas da distribuição acumulada entre os níveis da variável target)
 dat=as.data.frame(cbind(y,adj))
 sample1 <- subset(dat,dat$y==0)
 sample2 <- subset(dat,dat$y==1)
 cdf1 <- ecdf(sample1$adj) 
 cdf2 <- ecdf(sample2$adj) 
 
 dev.new()#Plot do KS
 plot(cdf1, verticals=TRUE, do.points=FALSE, col="blue",lwd=2,xlim=c(0.035,0.965),main="KS Curve") 
 plot(cdf2, verticals=TRUE, do.points=FALSE, col="red",lwd=2, add=TRUE)

 #Tracejado no ponto máximo entre as curvas
 minMax <- seq(min(sample1$adj, sample2$adj), max(sample1$adj, sample1$adj2), length.out=length(sample1$adj)) 
 x0 <- mean(minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))))])
 y0 <- mean(cdf1(x0)) 
 y1 <- mean(cdf2(x0)) 

 points(c(x0, x0), c(y0, y1), pch=16, col="black") 
 segments(x0, y0, x0, y1, col="black", lty="dotted")
 ks=abs(y0-y1)#Estimativa pontual do KS

 #métricas da qualidade de ajuste extraídas da tabela de contingência 
 #variando a probabilidade de corte para classificação positiva
 cutindex=seq(0.01,0.99,0.01)
 prec=NULL
 rec=NULL
 acc=NULL
 sens=NULL
 espec=NULL
 for (i in 1:length(cutindex))
 {
   adj_cat=as.numeric(adj>=cutindex[i])
   tab=table(adj_cat,y)
   acc[i]=sum(diag(tab))/sum(tab)
   sens[i]=tab[2,2]/sum(tab[,2])
   espec[i]=tab[1,1]/sum(tab[,1])
   prec[i]=tab[2,2]/sum(tab[2,])
   rec[i]=tab[2,2]/sum(tab[,2])
 }
 f1=2*prec*rec/(prec+rec)

 #Plot dos gráficos
 dev.new()
 plot(cutindex,acc,xlab="Cutoff",ylab="Value",ylim=c(0,1),type="l",lwd=2,axes=T,col="red",
      main=("Sensitivity and Specificity Comportament"),cex.main=1)
 lines(cutindex,sens,col="blue",lwd=2)
 lines(cutindex,espec,col="darkgreen",lwd=2)
 legend("bottom",col=c("red","blue","darkgreen"),lwd=c(2,2,2),
        lty=c(2,2,2),c("Accuracy","Sensitivity","Specificity"),bty="n",ncol=3)
 dev.new()
 plot(cutindex,prec,xlab="Cutoff",ylab="Value",ylim=c(0,1),type="l",lwd=2,axes=T,col="red",
      main=("Precision, Recall and F1 Comportament"),cex.main=1)
 lines(cutindex,rec,col="blue",lwd=2)
 lines(cutindex,f1,col="darkgreen",lwd=2)
 legend("bottom",col=c("red","blue","darkgreen"),lwd=c(2,2,2),
        lty=c(2,2,2),c("Precision","Recall","F1"),bty="n",ncol=3)
 
 #Matriz de resultados default
 result=round(matrix(c(ROCc,ks,acc[50],sens[50],espec[50],prec[50],rec[50],f1[50]),8,1),digits=4)
 rownames(result)=c("Roc","KS","Acuracy","Sensitivity","Especificity","Precision","Recall","F1")
 colnames(result)=""
 return(result)
}

P2Class(y,adj)


