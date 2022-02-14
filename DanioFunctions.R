library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(effects)

#I put together 5 functions to summarise danioscope data
#Functions are very simple and can be easily modified
#They are meant to work sequentially 
#The forth function puts the three main together and can be used alternatively
#The fifth-one is used to remove wells from the dataset and can be used before or after



##The first function adds 2 more variables: a "1 minue index" and a "5 minute index".
#it also removes the outliers based on the "1 minute" and the "distance moved" variable
#(removes what is bigger than "out")
##If "out" is not specified it doesn't remove anything 

danio_min<- function (x,out=max(x$Distance_moved_mm)){#x is the dataset, out is the outliers value (in distance moved)
        x[1,(ncol(x)+1)]<-1        
        names(x)[(ncol(x))]<-"Minute"
        for (i in 2:nrow(x)){
                if(x[i,2]==x[(i-1),2]){
                        x[i,(ncol(x))]<-x[(i-1),(ncol(x))]+1}
                else{
                        x[i,(ncol(x))]<-1
                }
        }
        x$five_min_index <- rep(c(1,rep((1:(rle(x$Pozzetto)[[1]][[1]]-1))%/%5)+1),length(unique(x$Pozzetto)))
        x$five_min_index2<- ((x$five_min_index*5))
        y<-x[x$Distance_moved_mm<=out,]
        y}

#function to add the dark/light treatment (condition"). 
darkcond<-function(x,y,z){#file of the list, y=starting dark, z=ending dark
        darkness<-(between(x$five_min_index2,(y+5),z))
        condition<-ifelse(darkness,"dark","light")
        x<-cbind(x,condition)
        x}

##semplice funzione per calcolare le medie della velocit? nei 5 minuti e la somma dello spazio percorso
# e dei tempi di attivit? e inattivit?. 
# La funzione Filtra i valori in base alle distanza percorsa nei 5 minuti (minore/uguale a "out")
# Non specificando "out" non si rimuovono gli outliers  
# La funzione rimuove gli ultimi 5 minuti (che son sempre incasinati)

danio_5<-function(x,out=max(y$Distance_movedT)){y<-x %>%
        group_by(Trial,Treatment,Pozzetto,five_min_index2,condition) %>% 
        summarise(Distance_movedT=sum(Distance_moved_mm),
                  Velocity=mean(Velocity_mms),
                  MovingTimeT=sum(MovingTime_s),
                  Not_MovingTimeT=sum(Not_MovingTime_s))
y<-filter(y,Distance_movedT<=out)
y<-filter(y,five_min_index2<max(y$five_min_index2))
y
}

#The three functions above can combined in a new formula (a bit less "clean") called daniofix()
daniofix<-function(x,start,end,out1=max(x$Distance_moved_mm),out5=100000){
        first<-danio_min(x,out1)
        second<-darkcond(first,start,end)
        final<-danio_5(second,out5)
        final
}
#daniofix() works combining the three functions above. 
#You need to fpecify AT LEAST the dataset and the start/end of the dark period.
#you can also define an outlier cut based on 1 minute data (out1) or 5 minutes data (out2). 


###############################################################################
#Below the last function, danio_rem(). It is used toremove wells or keep wells.
#this function can be used before daniofix (or any other function above) or after adding 
#minutes and summarising (it is more efficient to use before).
#you have to specify the dataset, if you want to keep (write "k" with brackets) or discard (anything else) the wells
#and the first and last wells to keep or discard
#

danio_rem<-function(x,KR="k",first,last){
        wells<-unique(x$Pozzetto)
        if(KR=="k"){keep_wells<-c(wells[first:last])#keep all the wells from first to last
        y<-x[x$Pozzetto%in%keep_wells,]}
        else{rem_wells<-c(wells[first:last]) #remove all the wells fromfrom first to last
        y<-x[!x$Pozzetto%in%rem_wells,]}
        y}




#Testing the functions on a dataset:
#First test functions one by one
#filenames<-list.files("C:/Users/Color&Sound/Experiments/Padova/ExpCameretteDaniovision/Datasets")
danio<-read_excel("C:/Users/Color&Sound/Experiments/Padova/ExpCameretteDaniovision/Datasets/Statistics-AleMaria07_12.xlsx", sheet=2)

#available functions (!!!to use sequentially!!)
#danio_min(dataset,out): This function adds 3 grouping factors at the end of the dataset (baset on timing).
#                               It also remove outliers if specified (specify "out" to determine the lower values to exclude)
#                               As this function works with a loop, it takes some time (about 8 seconds for a 12000 lines file)
#                               (test it with system.time(danio_min(danio)))
#darkcond(dataset,start,end): This function adds to the dataset a variable with "dark" or "light".
#                               You need to specify when the dark starts and ends
#danio_5(dataset,out): This function summrise the dataset in 5 minutes bins. Specifically it calculates for the 5 min bin
#                       Distance_movedT: total distance moved in the 5 minutes 
#                       Velocity: average velocity in the 5 minutes
#                       MovingTimeT: Total moving time in the 5 minutes 
#                       Not_MovingTimeT: Total NON-moving time in the 5 minutes 
#               This function can also remove the outliers based on the total distance moved in the 5 minutes.
#               To do so specify "out" to determine the lower values to exclude. Do not specify "out" if you
#               don't want to remove outliers
#               The function also removes the very last bin, which is usually shorter and strange


danio2<-danio_min(danio)
danio3<-darkcond(danio2,150,160)
danioSUMMARY<-danio_5(danio3,1000)
danioSUMMARY %>%  ggplot(aes(x=as.factor(five_min_index2),y=Velocity, col=condition))+geom_boxplot()+facet_wrap(~Pozzetto)

#the tree functions can be written in line
danioSUMMARY<-danio_5(darkcond(danio_min(danio),150,160),1000)

############################################################################
##############        TESTING DANIOFIX()        ############################
############################################################################

test1<-daniofix(danio,150,160) ###No outlier exclusion

test2<-daniofix(danio,150,160,out1=200) ###excluding outliers at the "1 minute level". 
## everythin biggher or equals to 200 is excluded

test3<-daniofix(danio,150,160,out5=1000) ###excluding outliers at the "5 minute level". 
#everythin biggher or equals to 1000 is excluded

#quick Plot of the results
test1 %>%  ggplot(aes(x=as.factor(five_min_index2),y=Velocity, col=condition))+geom_boxplot()
test2 %>%  ggplot(aes(x=as.factor(five_min_index2),y=Velocity, col=condition))+geom_boxplot()
test3 %>%  ggplot(aes(x=as.factor(five_min_index2),y=Velocity, col=condition))+geom_boxplot()

############################################################################
##############        TESTING danio_rem()        ############################
############################################################################


test4<-danio_rem(danio,"r", 2,10) #remove wells from 2 to 10 in the original dataset
test5<-danio_rem(test3,"k", 2,10) #keep wells from 2 to 10 in the dataset where outliers and dark is present

