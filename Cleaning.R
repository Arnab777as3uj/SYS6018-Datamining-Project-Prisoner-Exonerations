# Import packages
library(stringr)

# Read in data and get summary
data = read.csv("publicspreadsheet.csv")
summary(data)


# Note values where data has no age
data[is.na(data$Age),] 


# Store median age
med_age = median(data$Age[!is.na(data$Age)])

# Impute missing age values with median
# (there are only five such observations, 
# so this shouldn't affect too much)
data$Age[is.na(data$Age)] = med_age


# Combine County and State into one vector
# (we're not concerned with 'Harris' counties
# across different states)
testvec = paste(data$County, data$State)
data$County = as.factor(testvec)


# Drop Additional Crimes variables since we see too wide
# a variance, and intuition tells us that it won't offer
# much more than we can expect from Worst Crime
data = data[, !names(data) %in% c('List.Add.l.Crimes.Recode')]


# Drop variable for indicating whether or not this
# observation was included in a different project; it
# won't affect our conclusions
data = data[, !names(data) %in% c('X.')]


# Drop Posting Date because we don't want a predictive model
# based on the timing of the release of the information (by
# the definition of our project)
data = data[, !names(data) %in% c('Posting.Date')]


# Drop ID because it's a unique identifier that's unrelated
# to our variables of interest
data = data[, !names(data) %in% c('ID')]

# Split Tags variable into a list of tags for parsing through
# for each row
data$TagSplit = str_split(data$Tags, ";#")

# Iterate through each row
for(i in 1:nrow(data)) {
  
  # If our TagSplit column has anything in it...
  if(data[i, 'TagSplit'] != "") {
    
    # Set the entry for that column name to 1
    for(tag in data[i, 'TagSplit'][[1]]) {
      data[i, tag] = 1
    }
  
  }
  
}


# Replace all NAs in dataframe with 0s 
# (these are only those introduced by the above loop)
data[is.na(data)] = 0


# Drop Tags and TagSplit columns since we're done with them
data = data[!names(data) %in% c('TagSplit', 'Tags')]


# Set any entries in DNA, FC, MWID, F.MFE, P.FA, OM, and ILD
# columns to 1 since they're the only entries, and we're
# leaning toward a one-hot encoding model (and 0 otherwise)
data = within(data, {
  DNA = ifelse(DNA == "", 0, 1)
})
  
data = within(data, {
  FC = ifelse(FC == "", 0, 1)
})

data = within(data, {
  MWID = ifelse(MWID == "", 0, 1)
})

data = within(data, {
  F.MFE = ifelse(F.MFE == "", 0, 1)
})

data = within(data, {
  P.FA = ifelse(P.FA == "", 0, 1)
})

data = within(data, {
  OM = ifelse(OM == "", 0, 1)
})

data = within(data, {
  ILD = ifelse(ILD == "", 0, 1)
})
