#FIXED EFFECTS MODEL 

# scaling the data frame
# replace all * and ** values with NA
CAPSTONE_DATA_FINAL[CAPSTONE_DATA_FINAL == "**"] <- NA
CAPSTONE_DATA_FINAL[CAPSTONE_DATA_FINAL == "*"] <- NA

# remove years before 2006
CAPSTONE_DATA_FINAL <- CAPSTONE_DATA_FINAL[which(CAPSTONE_DATA_FINAL$YEAR > 2006), ]

# create scaled data frame
scaled_cdf <- CAPSTONE_DATA_FINAL

# convert scaled df to numeric
scaled_cdf$`TOTAL EMPLOYED` <- as.numeric(as.character(scaled_cdf$`TOTAL EMPLOYED`))
scaled_cdf$`SALARY MEAN` <- as.numeric(as.character(scaled_cdf$`SALARY MEAN`))
scaled_cdf$`FIREARMS REPORTED STOLEN` <- as.numeric(as.character(scaled_cdf$`FIREARMS REPORTED STOLEN`))
scaled_cdf$`FIREAMRS REGISTERED` <- as.numeric(as.character(scaled_cdf$`FIREAMRS REGISTERED`))
scaled_cdf$`MEDIAN STATE INCOME` <- as.numeric(as.character(scaled_cdf$`MEDIAN STATE INCOME`))
scaled_cdf$`TOTAL VICTIMS` <- as.numeric(as.character(scaled_cdf$`TOTAL VICTIMS`))

# aggregate data
scaled_cdf <- aggregate(x = scaled_cdf[c("TOTAL EMPLOYED", "SALARY MEAN", "FIREARMS REPORTED STOLEN", "FIREAMRS REGISTERED","MEDIAN STATE INCOME")],
                        by = test[c("YEAR", "clusterNum")],
                        FUN = sum
)


# scale data
scaled_cdf[,3:8] <- scale(scaled_cdf[,3:8])


# rename data frame
names(scaled_cdf) <- c("year","clusternum", "total_employees","mean_salary","guns_stolen","guns_registered","median_state_income","total_victims")


# load plm package
library(plm)

# model fixed regression 
plm_mod <- plm(total_victims ~ total_employed + salary_mean + firearms_stolen + firearms_registered + median_state_income, data = scaled_aggregated, index=c("clusternum","year"), model = "within", random.method = "walhus")
summary(plm_mod)



  


