# operate the following command in bash before using this function:
# sed -i "N, Md" filename; the N indicate the starter number and M indicate the end numbr
library(outliers)

# raw data import and processing
rtdata = read.csv(file.choose())
rtdata = rtdata[,c(1,3,5,11)]
rtdata = data.frame(Wellx = substring(rtdata$Well, 1, 1),
                     Welly = substring(rtdata$Well, 2),
                     rtdata[,-1],
                    Subject = as.factor(rep(1:32, each = 3)))

# calculate the average mean of samples, 3 for each
for (i in seq(1, dim(rtdata)[1], 3)){
  subrtdata = rtdata[i:(i+2),]
  
  # Tm within certain range
  subrtdata = subrtdata[subrtdata$Tm > (Tm-Tmrange) & subrtdata$Tm < (Tm-Tmrange), ]
  
  # eliminate the outliers
  
  
  
  
}




# calculate the R squared, slope and intercept for standard curve
sv_lm = lm(y~x, data = sv)
sv_r = summary(sv_lm)$r.squared
sv_slop = sv_lm$coefficients[2]
sv_intercept = sv_lm$coefficients[1]

# test whether R squared and slope are within normal range
restriccondition = sv_r >= 0.99 & sv_slop < -3 & sv_slop > -3.5
if(!restriccondition){
  pointsajustment('x', 'y', sv)
}


pointsajustment = function(x, y, data, maxnpointdelet = 3){
  x = data[,x]
  y = data[,y]
  # initiat the number of points to be deleted as 1
  npointdelet = 0
  while(!(sv_r >= 0.99 & sv_slop < -3 & sv_slop > -3.5)){
    npointdelet = npointdelet +1
    if(npointdelet > maxnpointdelet){
      return('The validated points are less than 5, you`d better do your experiment again!')
    }
    pointdelet = combn(1:length(x), npointdelet)
    for(i in 1:(length(pointdelet)/npointdelet)){
      data_del = data[-pointdelet[,i],]
      data_del_lm = lm(y~x, data = data_del)
      sv_r = summary(data_del_lm)$r.squared
      sv_slop = data_del_lm$coefficients[2]
      sv_intercept = data_del_lm$coefficients[1]
      if(sv_r >= 0.99 & sv_slop < -3 & sv_slop > -3.5){
        return(list(r = sv_r, 
                    slop = as.numeric(sv_slop), 
                    intercept = as.numeric(sv_intercept),
                    deletedpoints = pointdelet[,i]))
      }
    }
  }
}
