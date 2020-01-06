#read in/preprocess data
library(readr)
activity <- read_csv("C:/Users/braden.c.glover/Desktop/Training/JHU Data Science Certificate/Reproducible Research/Week Two Project/activity.csv")

#aggregate data for charts/analysis
tot_step <- aggregate(steps~date,
                      data=activity,
                      sum,
                      na.rm=T)
intervals <- aggregate(steps~interval, 
                       data = activity, 
                       sum, 
                       na.rm = T)