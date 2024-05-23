library(plspm)
library(spainfoot)

install.packages("pacman")
pacman::p_load(pacman)
pacman::p_load(rio)

#dset_ICI<-import("C:/smartpls_ws/datasetICI1.csv")
dset_ICI<-import("C:/Users/r3dal/Google Drive/Learn R/datasetICI1.csv")
head(dset_ICI)
dim(dset_ICI) #rows, columns
summary(dset_ICI)

dset_ICI<-dset_ICI[-c(0:4)]
names(dset_ICI)
head(dset_ICI)
View(dset_ICI)
#__________________________________Missing Data__________________________

#missing data
which(is.na(dset_ICI))# what are the missing Data
sum(is.na(dset_ICI)) # count of missing data


pacman::p_load(mice)
md.pattern(dset_ICI,rotate.names = T)

dset_ICI[is.na(dset_ICI$AR_students_understanding),]
dset_ICI<-dset_ICI[-c(18,25,32,60,83),]
md.pattern(dset_ICI,rotate.names = T)
which(is.na(dset_ICI))# what are the missing Data
sum(is.na(dset_ICI)) # count of missing data
dim(dset_ICI) #rows, columns

mice_result<-mice(dset_ICI)
str(mice_result)
summary(mice_result)
?mice
dset_comp<-complete(mice_result)
md.pattern(dset_comp,rotate.names = T)

cor(dset_comp[,10:14])
pacman::p_load(plsdepot)
A_Invst<-nipals(dset_comp[,10:14])
plot(A_Invst,main='Invest',cex=1)
#_____________Looking at the Data

aux_distrib<-table(dset_ICI[,1])/nrow(dset_ICI)
barplot(aux_distrib,main=colnames(dset_ICI)[1])
pacman::p_load(RColorBrewer)

Cat_P<-c("Gender","Experience" ,"Age","Classes")
op<-par(mfrow=c(2,2),mar=c(2.5,3.2,2,0.8))
for(j in 1:4){
  dis<-table(dset_ICI[,j])/nrow(dset_ICI)
  barplot(dis,border=NA,col=brewer.pal(8,'Blues')[2:8],
          axes=F,main=Cat_P[j],cex.main=1)
  axis(side = 2,las=2)
  box("figure",col='gray70')
}
par(op)

names(dset_comp)


#_____________Inner Model_____
P_useful_T<-c(0,0,0,0,0,0,0,0,0,0,0,0)
P_useful_S<-c(0,0,0,0,0,0,0,0,0,0,0,0)
Invest<-c(0,0,0,0,0,0,0,0,0,0,0,0)
AT_Invest<-c(0,0,0,0,0,0,0,0,0,0,0,0)
Pp_useful<-c(1,1,1,1,0,0,0,0,0,0,0,0)
ATT<-c(1,1,1,0,1,0,0,0,0,0,0,0)
AU<-c(1,1,1,1,1,1,0,0,0,0,0,0)
VR<-c(1,1,1,1,1,1,1,0,0,0,0,0)
AR<-c(1,1,1,1,1,1,1,0,0,0,0,0)
AI<-c(1,1,1,1,1,1,1,0,0,0,0,0)
SM<-c(1,1,1,1,1,1,1,0,0,0,0,0)
OSER<-c(1,1,1,1,1,1,1,0,0,0,0,0)

ICI_path<-rbind(P_useful_T, P_useful_S, Invest ,AT_Invest, Pp_useful, ATT, AU, VR, AR, AI, SM, OSER)
#,AR,AI,SM,OSER
ICI_path
colnames(ICI_path)<-rownames(ICI_path)
pos <- cbind (c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.65, 0.8, 0.65, 0.8, 0.65, 0.8), 
              c(0.2, 0.35, 0.5, 0.65, 0.8, 0.95, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

#pos <- cbind (c(0.2, 0.2, 0.6, 0.8, 0.6, 0.8, 0.6, 0.8), 
             # c(0.2, 0.5, 0.2, 0.3, 0.5, 0.6, 0.8, 0.9))

innerplot(ICI_path,box.size = 0.04, pos=pos ,arr.pos = 0.8)
View(dset_comp)
dset_comp<-dset_comp[-c(1:4)]
View(dset_comp)

ICI_blocks<-list(11:18,19:29,6:10,46:49,37:45,2:4,c(1,30:36),50:56,57:64,65:72,73:81,82:87)
#
head(dset_comp[,c(46:49)])
ICI_modes<-rep('A',12)
ICI_modes

ICI_plspm1<-plspm(dset_comp,ICI_path,ICI_blocks,modes = ICI_modes)
ICI_plspm1$gof
ICI_plspm1$unidim

outerplot(ICI_plspm1, what = "loadings",arr.tcol='black',colneg='red',arr.pos = 0.7)

ICI_plspm1$outer_model

pacman::p_load(ggplot2)

ggplot(data=ICI_plspm1$outer_model,
        aes(x=name,y=loading,fill=block)) +
  geom_bar(stat = 'identity',position = 'dodge') +
  geom_hline(yintercept=0.7,color='red') +
  ggtitle("loadings") +
  theme(axis.text.x = element_text(angle=90))
View(ICI_plspm1$outer_model)
ICI_plspm1$outer_model[ICI_plspm1$outer_model$loading<0.7,,drop=F]

#_____Model 2__________________
ICI_blocks<-list(11:18,c(19:21,23:29),c(6,8:10),46:49,37:45,3:4,c(30:34,36),50:56,57:64,65:72,73:81,82:87)
ICI_plspm2<-plspm(dset_comp,ICI_path,ICI_blocks,modes = ICI_modes)
ICI_plspm2$gof
ICI_plspm2$unidim

outerplot(ICI_plspm2, what = "loadings",arr.tcol='black',colneg='red',arr.pos = 0.7)

ICI_plspm2$outer_model


ggplot(data=ICI_plspm2$outer_model,
       aes(x=name,y=loading,fill=block)) +
  geom_bar(stat = 'identity',position = 'dodge') +
  geom_hline(yintercept=0.7,color='red') +
  ggtitle("loadings") +
  theme(axis.text.x = element_text(angle=90))
View(ICI_plspm2$outer_model)
ICI_plspm2$outer_model[ICI_plspm2$outer_model$loading<0.7,,drop=F]

#______________model-3________________
# delete invest 
#____Inner Model_____
P_useful_T<-c(0,0,0,0,0,0,0,0,0,0,0)
P_useful_S<-c(0,0,0,0,0,0,0,0,0,0,0)
 AT_Invest<-c(0,0,0,0,0,0,0,0,0,0,0)
 Pp_useful<-c(1,1,1,0,0,0,0,0,0,0,0)
       ATT<-c(1,1,1,1,0,0,0,0,0,0,0)
        AU<-c(1,1,1,1,1,0,0,0,0,0,0)
        VR<-c(0,0,0,1,1,1,0,0,0,0,0)
        AR<-c(0,0,0,1,1,1,0,0,0,0,0)
        AI<-c(0,0,0,1,1,1,0,0,0,0,0)
        SM<-c(0,0,0,1,1,1,0,0,0,0,0)
      OSER<-c(0,0,0,1,1,1,0,0,0,0,0)


ICI_path<-rbind(P_useful_T, P_useful_S ,AT_Invest, Pp_useful, ATT, AU, VR, AR, AI, SM, OSER)
#, Invest 
ICI_path
colnames(ICI_path)<-rownames(ICI_path)
pos <- cbind (c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.65, 0.8, 0.65, 0.65, 0.8), 
              c(0.2, 0.35, 0.5, 0.65, 0.8, 0.95, 0.2, 0.35, 0.5, 0.8, 0.95))
#


innerplot(ICI_path,box.size = 0.04, pos=pos ,arr.pos = 0.8)
View(dset_comp)
dset_comp<-dset_comp[-c(1:4)]
View(dset_comp)

ICI_blocks<-list(11:18,c(19:21,23:29),46:49,37:45,3:4,c(30:34,36),50:56,57:64,65:72,73:81,82:87)
#6:10,
head(dset_comp[,c(6:10)])
ICI_modes<-rep('A',11)
ICI_modes

ICI_plspm3<-plspm(dset_comp,ICI_path,ICI_blocks,modes = ICI_modes)
ICI_plspm3$gof
ICI_plspm3$unidim

outerplot(ICI_plspm3, what = "loadings",arr.tcol='black',colneg='red',arr.pos = 0.7)

ICI_plspm3$outer_model

pacman::p_load(ggplot2)

ggplot(data=ICI_plspm3$outer_model,
       aes(x=name,y=loading,fill=block)) +
  geom_bar(stat = 'identity',position = 'dodge') +
  geom_hline(yintercept=0.7,color='red') +
  ggtitle("loadings") +
  theme(axis.text.x = element_text(angle=90))
View(ICI_plspm3$outer_model)
ICI_plspm1$outer_model[ICI_plspm3$outer_model$loading<0.7,,drop=F]



#____Cross Loadings_______________
ICI_plspm3$crossloadings
View(ICI_plspm3$crossloadings)
subset(ICI_plspm3$crossloadings,block=='P_useful_T')

#________Plot
pos <- cbind (c(0.1, 0.3, 0.55, 0.75, 0.95,
                0.1, 0.5, 0.95,
                0.1, 0.5, 0.95), 
              c(0.1, 0.1, 0.1,0.1, 0.1, 
                0.4,0.6,0.7, 
                0.95, 0.95, 0.95))
plot(ICI_plspm3,box.size = 0.04, pos=pos ,arr.pos = 0.75,arr.tcol='black',arr.lwd=2,arr.width=0.1)

ICI_plspm3$path_coefs
View(ICI_plspm3$path_coefs)
ICI_plspm3$inner_model
View(ICI_plspm3$inner_model$ATT)
View(ICI_plspm3$effects)
#___Plot Effects
eff_rows<-c(3:7,9:10,12:45)
path_effs<-as.matrix(ICI_plspm3$effects[eff_rows,2:3])
rownames(path_effs)<-ICI_plspm3$effects[eff_rows,1]
p_effe<-as.data.frame(ICI_plspm3$effects[eff_rows,])
path_effs
op=par(mar=c(8,3,1,0.5))
barplot(t(path_effs),border = NA,col=c('#9E9Ac8','#DADAEE'),
        las=2,cex.names=0.8,cex.axis=0.8, ylim = c(-0.1:0.9),
        legend=c('Direct','Indirect'),
        args.legend = list(x='top',ncol=2,border=NA,bty='n',title='Effects'))

ggplot(p_effe, aes(x=p_effe$relationships, y=c(p_effe$direct))) + 
  geom_bar (stat="identity") +
  scale_y_continuous(breaks = seq(-1,1,0.02),limits = c(-0.06,0.6))


ICI_plspm3$inner_summary
#R^2 0.2 low, 0.5 moderate, >0.5 High
#R^2 indicate the amount of variance in the endog-LV explaind by its indep-LV
# Block_Communality 0.5
# Mean_Redundancy exp. mean_Redun of 0.29 mean that AU predicts 29% of AI
#AVE 0.5

ICI_plspm2$gof
#Goodness of fit, 42% of the data is explained by the model 
#no threshold, the higher the better, acceptable >0.7, depends on the study


#_______________Bootstraping________________________

ICI_pls_boot<-plspm(dset_comp,ICI_path,ICI_blocks,modes = ICI_modes,boot.val = T,br=1000)

ICI_pls_boot$boot
ICI_pls_boot$boot$paths

scors<-rescale(ICI_plspm2)
summary(scors)
