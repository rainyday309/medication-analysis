# input file: UTF-8
# issue - some medication does not have dosage
# example: 昆泰研究用藥, 甘油球, Soma

library('dplyr')
source('tools.r')

raw <- read.csv('raw_meds_UTF8.csv', header=FALSE, stringsAsFactors=FALSE)

# aim: 
# 1. capture all medication kinds
# generate list of all medication
# group medication by real content

# remove all trailing spaces

for (i in 1:length(raw)) {
  if (typeof(raw[,i])=='character') {
    raw[,i] = trim(raw[,i])
  }
}


# get unique names of medication
names = c()

for (i in 1:length(raw)) {
  if (typeof(raw[,i])=='character') {
    names = c(names, unique(raw[,i]))
  }
}

# remove empty strings
names = names[names > 0]
# get unique names
names = unique(names)
names = sort(names)

# create a dictionary of medications
dict <- as.data.frame(names, stringsAsFactors = FALSE)
dict$include <- vector(mode = 'logical', length=length(names))

# retained list is check by Dr. Li DJ
retain_medication <- c(15,16,17,28,36:41,44:46,57:59,66:67,70,72,74,75,79:81,85:90,94,96:97,102:104,111,113,118,119,121,123,124,127:130,133,135,137,142:144,149,150,152,153,159,163:166,170:173,179,180,183:186,189:200,204:208,211,214:218,223,224,233,234,236,242,244,255:260,262,264,266:271,283:292,294,296,299,300,302,304,311:315,325,331:333,336,337,339:344,351:353,357,358,360,362,363,365)
dict$include[retain_medication] = TRUE
final_dict <- filter(dict, include)

# get medication dosage
final_dict$dose <- regmatches(final_dict$names, regexpr("\\d+mg", final_dict$names))
final_dict$dose <- as.numeric(sub(pattern = "mg",replacement = "",final_dict$dose))

# haloperidol (61), depakine solution(35), risperidal solution (116), dosage need to be adjusted

# search syntax: which(raw[,2] %in% "Depakine (500mg)")

