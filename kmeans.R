## normalizing data

# normalize data for total number of mental health conselors per state
# subset data
state_emp <- sqldf("select STATE, sum(EMP), avg(EMP) FROM clusterdf GROUP BY STATE")
#remove on contiguous states
state_emp <-state_emp[!(state_emp$STATE=="District Of Columbia"),]
state_emp <-state_emp[!(state_emp$STATE=="Virgin Islands"),]
state_emp <-state_emp[!(state_emp$STATE=="Puerto Rico"),]
# find avg and sd
avg_emp <- sum(state_emp$`avg(EMP)`) / 50
sd_emp <- sd(state_emp$`avg(EMP)`)
# normalize data using avg and sd
x <- (state_emp$`avg(EMP)` - avg_emp) / sd_emp
# bind to data frame
state_emp <- mutate(state_emp, nor_emp = x)

# normalize data for total number of guns reported lost per state
# subset data
state_gl <- sqldf("select STATE, sum(GL), avg(GL) FROM clusterdf GROUP BY STATE")
#remove on contiguous states
state_gl <-state_gl[!(state_gl$STATE=="District Of Columbia"),]
state_gl <-state_gl[!(state_gl$STATE=="Virgin Islands"),]
state_gl <-state_gl[!(state_gl$STATE=="Puerto Rico"),]
# find avg and sd
avg_gl <- sum(state_gl$`avg(gl)`) / 50
sd_gl <- sd(state_gl$`avg(gl)`)
# normalize data using avg and sd
x <- (state_gl$`avg(gl)` - avg_gl) / sd_gl
# bind to data frame
state_gl <- mutate(state_gl, nor_gl = x)

# normalize data for total number of guns registered per state
# subset data
state_gr <- sqldf("select STATE, sum(GR), avg(GR) FROM clusterdf GROUP BY STATE")
#remove on contiguous states
state_gr <-state_gr[!(state_gl$STATE=="District Of Columbia"),]
state_gr <-state_gr[!(state_gl$STATE=="Virgin Islands"),]
state_gr <-state_gr[!(state_gl$STATE=="Puerto Rico"),]
# find avg and sd
avg_gr <- sum(state_gr$`avg(gr)`) / 50
sd_gr <- sd(state_gr$`avg(gr)`)
# normalize data using avg and sd
x <- (state_gr$`avg(gr)` - avg_gr) / sd_gr
# bind to data frame
state_gr <- mutate(state_gr, nor_gr = x)

# normalize data for total victins from gun violence events per state
# subset data
state_tv <- sqldf("select STATE, sum(GR), avg(GR) FROM clusterdf GROUP BY STATE")
#remove on contiguous states
state_tv <-state_tv[!(state_tv$STATE=="District Of Columbia"),]
state_tv <-state_tv[!(state_tv$STATE=="Virgin Islands"),]
state_tv <-state_tv[!(state_tv$STATE=="Puerto Rico"),]
# find avg and sd
avg_tv <- sum(state_tv$`avg(gr)`) / 50
sd_tv <- sd(state_tv$`avg(gr)`)
# normalize data using avg and sd
x <- (state_gr$`avg(tv)` - avg_tv) / sd_tv
# bind to data frame
state_tv <- mutate(state_tv, nor_tv = x)


# normalize data for total number of guns reported lost per state
# subset data
state_gl <- sqldf("select STATE, sum(GL), avg(GL) FROM clusterdf GROUP BY STATE")
#remove on contiguous states
state_gl <-state_gl[!(state_gl$STATE=="District Of Columbia"),]
state_gl <-state_gl[!(state_gl$STATE=="Virgin Islands"),]
state_gl <-state_gl[!(state_gl$STATE=="Puerto Rico"),]
# find avg and sd
avg_gl <- sum(state_gl$`avg(gl)`) / 50
sd_gl <- sd(state_gl$`avg(gl)`)
# normalize data using avg and sd
x <- (state_gl$`avg(gl)` - avg_gl) / sd_gl
# bind to data frame
state_gl <- mutate(state_gl, nor_gl = x)


# binding kmeans df
kmeans <- merge(state_emp, state_gl, by = "STATE", all=TRUE)
kmeans <- merge(kmeans), state_gr, by = "STATE", all=TRUE)
kmeans <- merge(kmeans, state_tv, by = "STATE", all=TRUE)
#selcting only the normalized data, removing sum and avg columns
kmeans <- select(test, "nor_emp","nor_gl","nor_gf","nor_tv")


# plot data to observe patterns
plot(kmeans)

# use nbclust to determine number of clusters 
library(NbClust)
set.seed(1234)
nc <- NbClust(kmeans, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# fit kmeans model
set.seed(1234)
fit.km <- kmeans(df, centers=14,  nstart=25)

# bind cluster category to data frame
fitkm <- cbind(kmeans, clusterNum = kmeans$cluster)

# save df
write.csv(kmeans, "kmeans.CSV")