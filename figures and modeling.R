# import saved dataframes from all age groups and other studies for plotting and analysis
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(reshape2)

# import dataframes for 24 and 2 octave for each age group----
setwd("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai")
newborn.24 = readRDS("newborn.24.rds")
newborn.2 = readRDS("newborn.2.rds")
six.24 = readRDS("six.24.rds")
six.2 = readRDS("six.2.rds")
twelve.24 = readRDS("twelve.24.rds")
twelve.2 = readRDS("twelve.2.rds")
eighteen.24 = readRDS("eighteen.24.rds")
eighteen.2 = readRDS("eighteen.2.rds")
ages = readRDS("ages.rds")
setwd("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/Working directory")

# first join as wide df - need to rename all abs, mag, pha with the age group
# there are a couple of extra test retest columns in the data - need to remove
newborn.24 = newborn.24[,-c(125,233)]
six.24 = six.24[,-c(124, 232)]
twelve.24 = twelve.24[,-c(124, 232)]
eighteen.24 = eighteen.24[,-c(124, 232)]
# remove tymp data (all ages) and dems for 6-18 mths
newborn.24 = newborn.24[,-c(4:7)]
six.24 = six.24[,-c(3:16)]
twelve.24 = twelve.24[,-c(3:16)]
eighteen.24 = eighteen.24[,-c(3:16)]

names(newborn.24) = c("aabr0", "tymp0", "dpoae0", "gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", 
                      "maternal.ethnicity", "age.0.hrs", "id.res", "ear", "abs0.226", "abs0.257.33", "abs0.280.62", "abs0.297.3", 
                      "abs0.324.21", "abs0.343.49", "abs0.363.91", "abs0.385.55", "abs0.408.48", "abs0.432.77", "abs0.458.5",      
                      "abs0.471.94", "abs0.500", "abs0.514.65", "abs0.545.25", "abs0.561.23", "abs0.577.68", "abs0.594.6",      
                      "abs0.629.96", "abs0.648.42", "abs0.667.42", "abs0.686.98", "abs0.707.11", "abs0.727.83", "abs0.749.15",         
                      "abs0.771.11", "abs0.793.7", "abs0.816.96", "abs0.840.9", "abs0.865.54", "abs0.890.9", "abs0.917",     
                      "abs0.943.87", "abs0.971.53", "abs0.1000", "abs0.1029.3", "abs0.1059.46", "abs0.1090.51", "abs0.1122.46",        
                      "abs0.1155.35", "abs0.1189.21", "abs0.1224.05", "abs0.1259.92", "abs0.1296.84", "abs0.1334.84", "abs0.1373.95",        
                      "abs0.1414.21", "abs0.1455.65", "abs0.1498.31", "abs0.1542.21", "abs0.1587.4", "abs0.1633.92", "abs0.1681.79",      
                      "abs0.1731.07", "abs0.1781.8", "abs0.1834.01", "abs0.1887.75", "abs0.1943.06", "abs0.2000", "abs0.2058.6",         
                      "abs0.2118.93", "abs0.2181.02", "abs0.2244.92", "abs0.2310.71", "abs0.2378.41", "abs0.2448.11", "abs0.2519.84",        
                      "abs0.2593.68", "abs0.2669.68", "abs0.2747.91", "abs0.2828.43", "abs0.2911.31", "abs0.2996.61", "abs0.3084.42",       
                      "abs0.3174.8", "abs0.3267.83", "abs0.3363.59", "abs0.3462.15", "abs0.3563.59", "abs0.3668.02", "abs0.3775.5",       
                      "abs0.3886.13", "abs0.4000", "abs0.4117.21", "abs0.4237.85", "abs0.4362.03", "abs0.4489.85", "abs0.4621.41",        
                      "abs0.4756.83", "abs0.4896.21", "abs0.5039.68", "abs0.5187.36", "abs0.5339.36", "abs0.5495.81", "abs0.5656.85",        
                      "abs0.5822.61", "abs0.5993.23", "abs0.6168.84", "abs0.6349.6", "abs0.6535.66", "abs0.6727.17", "abs0.6924.29",        
                      "abs0.7127.19", "abs0.7336.03", "abs0.7550.99", "abs0.7772.26", "abs0.8000", "mag0.226",            
                      "mag0.257.33", "mag0.280.62", "mag0.297.3", "mag0.324.21", "mag0.343.49", "mag0.363.91", "mag0.385.55",         
                      "mag0.408.48", "mag0.432.77", "mag0.458.5", "mag0.471.94", "mag0.500", "mag0.514.65", "mag0.545.25",        
                      "mag0.561.23", "mag0.577.68", "mag0.594.6", "mag0.629.96", "mag0.648.42", "mag0.667.42", "mag0.686.98",         
                      "mag0.707.11", "mag0.727.83", "mag0.749.15", "mag0.771.11", "mag0.793.7", "mag0.816.96", "mag0.840.9",          
                      "mag0.865.54", "mag0.890.9", "mag0.917", "mag0.943.87", "mag0.971.53", "mag0.1000", "mag0.1029.3",         
                      "mag0.1059.46", "mag0.1090.51", "mag0.1122.46", "mag0.1155.35", "mag0.1189.21", "mag0.1224.05", "mag0.1259.92",        
                      "mag0.1296.84", "mag0.1334.84", "mag0.1373.95", "mag0.1414.21", "mag0.1455.65", "mag0.1498.31", "mag0.1542.21",       
                      "mag0.1587.4", "mag0.1633.92", "mag0.1681.79", "mag0.1731.07", "mag0.1781.8", "mag0.1834.01", "mag0.1887.75",     
                      "mag0.1943.06", "mag0.2000", "mag0.2058.6", "mag0.2118.93", "mag0.2181.02", "mag0.2244.92", "mag0.2310.71",        
                      "mag0.2378.41", "mag0.2448.11", "mag0.2519.84", "mag0.2593.68", "mag0.2669.68", "mag0.2747.91", "mag0.2828.43",        
                      "mag0.2911.31", "mag0.2996.61", "mag0.3084.42", "mag0.3174.8", "mag0.3267.83", "mag0.3363.59", "mag0.3462.15",        
                      "mag0.3563.59", "mag0.3668.02", "mag0.3775.5", "mag0.3886.13", "mag0.4000", "mag0.4117.21", "mag0.4237.85",        
                      "mag0.4362.03", "mag0.4489.85", "mag0.4621.41", "mag0.4756.83", "mag0.4896.21", "mag0.5039.68", "mag0.5187.36",        
                      "mag0.5339.36", "mag0.5495.81", "mag0.5656.85", "mag0.5822.61", "mag0.5993.23", "mag0.6168.84", "mag0.6349.6",         
                      "mag0.6535.66", "mag0.6727.17", "mag0.6924.29", "mag0.7127.19", "mag0.7336.03", "mag0.7550.99", "mag0.7772.26",        
                      "mag0.8000", "pha0.226", "pha0.257.33", "pha0.280.62", "pha0.297.3", "pha0.324.21",         
                      "pha0.343.49", "pha0.363.91", "pha0.385.55", "pha0.408.48", "pha0.432.77", "pha0.458.5", "pha0.471.94",         
                      "pha0.500", "pha0.514.65", "pha0.545.25", "pha0.561.23", "pha0.577.68", "pha0.594.6", "pha0.629.96",         
                      "pha0.648.42", "pha0.667.42", "pha0.686.98", "pha0.707.11", "pha0.727.83", "pha0.749.15", "pha0.771.11",         
                      "pha0.793.7", "pha0.816.96", "pha0.840.9", "pha0.865.54", "pha0.890.9", "pha0.917", "pha0.943.87",         
                      "pha0.971.53", "pha0.1000", "pha0.1029.3", "pha0.1059.46", "pha0.1090.51", "pha0.1122.46", "pha0.1155.35",        
                      "pha0.1189.21", "pha0.1224.05", "pha0.1259.92", "pha0.1296.84", "pha0.1334.84", "pha0.1373.95", "pha0.1414.21",        
                      "pha0.1455.65", "pha0.1498.31", "pha0.1542.21", "pha0.1587.4", "pha0.1633.92", "pha0.1681.79", "pha0.1731.07",        
                      "pha0.1781.8", "pha0.1834.01", "pha0.1887.75", "pha0.1943.06", "pha0.2000", "pha0.2058.6", "pha0.2118.93",       
                      "pha0.2181.02", "pha0.2244.92", "pha0.2310.71", "pha0.2378.41", "pha0.2448.11", "pha0.2519.84", "pha0.2593.68",        
                      "pha0.2669.68", "pha0.2747.91", "pha0.2828.43", "pha0.2911.31", "pha0.2996.61", "pha0.3084.42", "pha0.3174.8",       
                      "pha0.3267.83", "pha0.3363.59", "pha0.3462.15", "pha0.3563.59", "pha0.3668.02", "pha0.3775.5", "pha0.3886.13",       
                      "pha0.4000", "pha0.4117.21", "pha0.4237.85", "pha0.4362.03", "pha0.4489.85", "pha0.4621.41", "pha0.4756.83",     
                      "pha0.4896.21", "pha0.5039.68", "pha0.5187.36", "pha0.5339.36", "pha0.5495.81", "pha0.5656.85", "pha0.5822.61",        
                      "pha0.5993.23", "pha0.6168.84", "pha0.6349.6", "pha0.6535.66", "pha0.6727.17", "pha0.6924.29", "pha0.7127.19",        
                      "pha0.7336.03", "pha0.7550.99", "pha0.7772.26", "pha0.8000")

# omit the demographics
six.24 = six.24 # minus demographics for each age group
names(six.24) = c("tymp6", "dpoae6", "abs6.226", "abs6.257.33", "abs6.280.62", "abs6.297.3", 
                  "abs6.324.21", "abs6.343.49", "abs6.363.91", "abs6.385.55", "abs6.408.48", "abs6.432.77", "abs6.458.5",      
                  "abs6.471.94", "abs6.500", "abs6.514.65", "abs6.545.25", "abs6.561.23", "abs6.577.68", "abs6.594.6",      
                  "abs6.629.96", "abs6.648.42", "abs6.667.42", "abs6.686.98", "abs6.707.11", "abs6.727.83", "abs6.749.15",         
                  "abs6.771.11", "abs6.793.7", "abs6.816.96", "abs6.840.9", "abs6.865.54", "abs6.890.9", "abs6.917",     
                  "abs6.943.87", "abs6.971.53", "abs6.1000", "abs6.1029.3", "abs6.1059.46", "abs6.1090.51", "abs6.1122.46",        
                  "abs6.1155.35", "abs6.1189.21", "abs6.1224.05", "abs6.1259.92", "abs6.1296.84", "abs6.1334.84", "abs6.1373.95",        
                  "abs6.1414.21", "abs6.1455.65", "abs6.1498.31", "abs6.1542.21", "abs6.1587.4", "abs6.1633.92", "abs6.1681.79",      
                  "abs6.1731.07", "abs6.1781.8", "abs6.1834.01", "abs6.1887.75", "abs6.1943.06", "abs6.2000", "abs6.2058.6",         
                  "abs6.2118.93", "abs6.2181.02", "abs6.2244.92", "abs6.2310.71", "abs6.2378.41", "abs6.2448.11", "abs6.2519.84",        
                  "abs6.2593.68", "abs6.2669.68", "abs6.2747.91", "abs6.2828.43", "abs6.2911.31", "abs6.2996.61", "abs6.3084.42",       
                  "abs6.3174.8", "abs6.3267.83", "abs6.3363.59", "abs6.3462.15", "abs6.3563.59", "abs6.3668.02", "abs6.3775.5",       
                  "abs6.3886.13", "abs6.4000", "abs6.4117.21", "abs6.4237.85", "abs6.4362.03", "abs6.4489.85", "abs6.4621.41",        
                  "abs6.4756.83", "abs6.4896.21", "abs6.5039.68", "abs6.5187.36", "abs6.5339.36", "abs6.5495.81", "abs6.5656.85",        
                  "abs6.5822.61", "abs6.5993.23", "abs6.6168.84", "abs6.6349.6", "abs6.6535.66", "abs6.6727.17", "abs6.6924.29",        
                  "abs6.7127.19", "abs6.7336.03", "abs6.7550.99", "abs6.7772.26", "abs6.8000", "mag6.226",            
                  "mag6.257.33", "mag6.280.62", "mag6.297.3", "mag6.324.21", "mag6.343.49", "mag6.363.91", "mag6.385.55",         
                  "mag6.408.48", "mag6.432.77", "mag6.458.5", "mag6.471.94", "mag6.500", "mag6.514.65", "mag6.545.25",        
                  "mag6.561.23", "mag6.577.68", "mag6.594.6", "mag6.629.96", "mag6.648.42", "mag6.667.42", "mag6.686.98",         
                  "mag6.707.11", "mag6.727.83", "mag6.749.15", "mag6.771.11", "mag6.793.7", "mag6.816.96", "mag6.840.9",          
                  "mag6.865.54", "mag6.890.9", "mag6.917", "mag6.943.87", "mag6.971.53", "mag6.1000", "mag6.1029.3",         
                  "mag6.1059.46", "mag6.1090.51", "mag6.1122.46", "mag6.1155.35", "mag6.1189.21", "mag6.1224.05", "mag6.1259.92",        
                  "mag6.1296.84", "mag6.1334.84", "mag6.1373.95", "mag6.1414.21", "mag6.1455.65", "mag6.1498.31", "mag6.1542.21",       
                  "mag6.1587.4", "mag6.1633.92", "mag6.1681.79", "mag6.1731.07", "mag6.1781.8", "mag6.1834.01", "mag6.1887.75",     
                  "mag6.1943.06", "mag6.2000", "mag6.2058.6", "mag6.2118.93", "mag6.2181.02", "mag6.2244.92", "mag6.2310.71",        
                  "mag6.2378.41", "mag6.2448.11", "mag6.2519.84", "mag6.2593.68", "mag6.2669.68", "mag6.2747.91", "mag6.2828.43",        
                  "mag6.2911.31", "mag6.2996.61", "mag6.3084.42", "mag6.3174.8", "mag6.3267.83", "mag6.3363.59", "mag6.3462.15",        
                  "mag6.3563.59", "mag6.3668.02", "mag6.3775.5", "mag6.3886.13", "mag6.4000", "mag6.4117.21", "mag6.4237.85",        
                  "mag6.4362.03", "mag6.4489.85", "mag6.4621.41", "mag6.4756.83", "mag6.4896.21", "mag6.5039.68", "mag6.5187.36",        
                  "mag6.5339.36", "mag6.5495.81", "mag6.5656.85", "mag6.5822.61", "mag6.5993.23", "mag6.6168.84", "mag6.6349.6",         
                  "mag6.6535.66", "mag6.6727.17", "mag6.6924.29", "mag6.7127.19", "mag6.7336.03", "mag6.7550.99", "mag6.7772.26",        
                  "mag6.8000", "pha6.226", "pha6.257.33", "pha6.280.62", "pha6.297.3", "pha6.324.21",         
                  "pha6.343.49", "pha6.363.91", "pha6.385.55", "pha6.408.48", "pha6.432.77", "pha6.458.5", "pha6.471.94",         
                  "pha6.500", "pha6.514.65", "pha6.545.25", "pha6.561.23", "pha6.577.68", "pha6.594.6", "pha6.629.96",         
                  "pha6.648.42", "pha6.667.42", "pha6.686.98", "pha6.707.11", "pha6.727.83", "pha6.749.15", "pha6.771.11",         
                  "pha6.793.7", "pha6.816.96", "pha6.840.9", "pha6.865.54", "pha6.890.9", "pha6.917", "pha6.943.87",         
                  "pha6.971.53", "pha6.1000", "pha6.1029.3", "pha6.1059.46", "pha6.1090.51", "pha6.1122.46", "pha6.1155.35",        
                  "pha6.1189.21", "pha6.1224.05", "pha6.1259.92", "pha6.1296.84", "pha6.1334.84", "pha6.1373.95", "pha6.1414.21",        
                  "pha6.1455.65", "pha6.1498.31", "pha6.1542.21", "pha6.1587.4", "pha6.1633.92", "pha6.1681.79", "pha6.1731.07",        
                  "pha6.1781.8", "pha6.1834.01", "pha6.1887.75", "pha6.1943.06", "pha6.2000", "pha6.2058.6", "pha6.2118.93",       
                  "pha6.2181.02", "pha6.2244.92", "pha6.2310.71", "pha6.2378.41", "pha6.2448.11", "pha6.2519.84", "pha6.2593.68",        
                  "pha6.2669.68", "pha6.2747.91", "pha6.2828.43", "pha6.2911.31", "pha6.2996.61", "pha6.3084.42", "pha6.3174.8",       
                  "pha6.3267.83", "pha6.3363.59", "pha6.3462.15", "pha6.3563.59", "pha6.3668.02", "pha6.3775.5", "pha6.3886.13",       
                  "pha6.4000", "pha6.4117.21", "pha6.4237.85", "pha6.4362.03", "pha6.4489.85", "pha6.4621.41", "pha6.4756.83",     
                  "pha6.4896.21", "pha6.5039.68", "pha6.5187.36", "pha6.5339.36", "pha6.5495.81", "pha6.5656.85", "pha6.5822.61",        
                  "pha6.5993.23", "pha6.6168.84", "pha6.6349.6", "pha6.6535.66", "pha6.6727.17", "pha6.6924.29", "pha6.7127.19",        
                  "pha6.7336.03", "pha6.7550.99", "pha6.7772.26", "pha6.8000")

names(twelve.24) = c("tymp12", "dpoae12", "abs12.226", "abs12.257.33", "abs12.280.62", "abs12.297.3", 
                     "abs12.324.21", "abs12.343.49", "abs12.363.91", "abs12.385.55", "abs12.408.48", "abs12.432.77", "abs12.458.5",      
                     "abs12.471.94", "abs12.500", "abs12.514.65", "abs12.545.25", "abs12.561.23", "abs12.577.68", "abs12.594.6",      
                     "abs12.629.96", "abs12.648.42", "abs12.667.42", "abs12.686.98", "abs12.707.11", "abs12.727.83", "abs12.749.15",         
                     "abs12.771.11", "abs12.793.7", "abs12.816.96", "abs12.840.9", "abs12.865.54", "abs12.890.9", "abs12.917",     
                     "abs12.943.87", "abs12.971.53", "abs12.1000", "abs12.1029.3", "abs12.1059.46", "abs12.1090.51", "abs12.1122.46",        
                     "abs12.1155.35", "abs12.1189.21", "abs12.1224.05", "abs12.1259.92", "abs12.1296.84", "abs12.1334.84", "abs12.1373.95",        
                     "abs12.1414.21", "abs12.1455.65", "abs12.1498.31", "abs12.1542.21", "abs12.1587.4", "abs12.1633.92", "abs12.1681.79",      
                     "abs12.1731.07", "abs12.1781.8", "abs12.1834.01", "abs12.1887.75", "abs12.1943.06", "abs12.2000", "abs12.2058.6",         
                     "abs12.2118.93", "abs12.2181.02", "abs12.2244.92", "abs12.2310.71", "abs12.2378.41", "abs12.2448.11", "abs12.2519.84",        
                     "abs12.2593.68", "abs12.2669.68", "abs12.2747.91", "abs12.2828.43", "abs12.2911.31", "abs12.2996.61", "abs12.3084.42",       
                     "abs12.3174.8", "abs12.3267.83", "abs12.3363.59", "abs12.3462.15", "abs12.3563.59", "abs12.3668.02", "abs12.3775.5",       
                     "abs12.3886.13", "abs12.4000", "abs12.4117.21", "abs12.4237.85", "abs12.4362.03", "abs12.4489.85", "abs12.4621.41",        
                     "abs12.4756.83", "abs12.4896.21", "abs12.5039.68", "abs12.5187.36", "abs12.5339.36", "abs12.5495.81", "abs12.5656.85",        
                     "abs12.5822.61", "abs12.5993.23", "abs12.6168.84", "abs12.6349.6", "abs12.6535.66", "abs12.6727.17", "abs12.6924.29",        
                     "abs12.7127.19", "abs12.7336.03", "abs12.7550.99", "abs12.7772.26", "abs12.8000", "mag12.226",            
                     "mag12.257.33", "mag12.280.62", "mag12.297.3", "mag12.324.21", "mag12.343.49", "mag12.363.91", "mag12.385.55",         
                     "mag12.408.48", "mag12.432.77", "mag12.458.5", "mag12.471.94", "mag12.500", "mag12.514.65", "mag12.545.25",        
                     "mag12.561.23", "mag12.577.68", "mag12.594.6", "mag12.629.96", "mag12.648.42", "mag12.667.42", "mag12.686.98",         
                     "mag12.707.11", "mag12.727.83", "mag12.749.15", "mag12.771.11", "mag12.793.7", "mag12.816.96", "mag12.840.9",          
                     "mag12.865.54", "mag12.890.9", "mag12.917", "mag12.943.87", "mag12.971.53", "mag12.1000", "mag12.1029.3",         
                     "mag12.1059.46", "mag12.1090.51", "mag12.1122.46", "mag12.1155.35", "mag12.1189.21", "mag12.1224.05", "mag12.1259.92",        
                     "mag12.1296.84", "mag12.1334.84", "mag12.1373.95", "mag12.1414.21", "mag12.1455.65", "mag12.1498.31", "mag12.1542.21",       
                     "mag12.1587.4", "mag12.1633.92", "mag12.1681.79", "mag12.1731.07", "mag12.1781.8", "mag12.1834.01", "mag12.1887.75",     
                     "mag12.1943.06", "mag12.2000", "mag12.2058.6", "mag12.2118.93", "mag12.2181.02", "mag12.2244.92", "mag12.2310.71",        
                     "mag12.2378.41", "mag12.2448.11", "mag12.2519.84", "mag12.2593.68", "mag12.2669.68", "mag12.2747.91", "mag12.2828.43",        
                     "mag12.2911.31", "mag12.2996.61", "mag12.3084.42", "mag12.3174.8", "mag12.3267.83", "mag12.3363.59", "mag12.3462.15",        
                     "mag12.3563.59", "mag12.3668.02", "mag12.3775.5", "mag12.3886.13", "mag12.4000", "mag12.4117.21", "mag12.4237.85",        
                     "mag12.4362.03", "mag12.4489.85", "mag12.4621.41", "mag12.4756.83", "mag12.4896.21", "mag12.5039.68", "mag12.5187.36",        
                     "mag12.5339.36", "mag12.5495.81", "mag12.5656.85", "mag12.5822.61", "mag12.5993.23", "mag12.6168.84", "mag12.6349.6",         
                     "mag12.6535.66", "mag12.6727.17", "mag12.6924.29", "mag12.7127.19", "mag12.7336.03", "mag12.7550.99", "mag12.7772.26",        
                     "mag12.8000", "pha12.226", "pha12.257.33", "pha12.280.62", "pha12.297.3", "pha12.324.21",         
                     "pha12.343.49", "pha12.363.91", "pha12.385.55", "pha12.408.48", "pha12.432.77", "pha12.458.5", "pha12.471.94",         
                     "pha12.500", "pha12.514.65", "pha12.545.25", "pha12.561.23", "pha12.577.68", "pha12.594.6", "pha12.629.96",         
                     "pha12.648.42", "pha12.667.42", "pha12.686.98", "pha12.707.11", "pha12.727.83", "pha12.749.15", "pha12.771.11",         
                     "pha12.793.7", "pha12.816.96", "pha12.840.9", "pha12.865.54", "pha12.890.9", "pha12.917", "pha12.943.87",         
                     "pha12.971.53", "pha12.1000", "pha12.1029.3", "pha12.1059.46", "pha12.1090.51", "pha12.1122.46", "pha12.1155.35",        
                     "pha12.1189.21", "pha12.1224.05", "pha12.1259.92", "pha12.1296.84", "pha12.1334.84", "pha12.1373.95", "pha12.1414.21",        
                     "pha12.1455.65", "pha12.1498.31", "pha12.1542.21", "pha12.1587.4", "pha12.1633.92", "pha12.1681.79", "pha12.1731.07",        
                     "pha12.1781.8", "pha12.1834.01", "pha12.1887.75", "pha12.1943.06", "pha12.2000", "pha12.2058.6", "pha12.2118.93",       
                     "pha12.2181.02", "pha12.2244.92", "pha12.2310.71", "pha12.2378.41", "pha12.2448.11", "pha12.2519.84", "pha12.2593.68",        
                     "pha12.2669.68", "pha12.2747.91", "pha12.2828.43", "pha12.2911.31", "pha12.2996.61", "pha12.3084.42", "pha12.3174.8",       
                     "pha12.3267.83", "pha12.3363.59", "pha12.3462.15", "pha12.3563.59", "pha12.3668.02", "pha12.3775.5", "pha12.3886.13",       
                     "pha12.4000", "pha12.4117.21", "pha12.4237.85", "pha12.4362.03", "pha12.4489.85", "pha12.4621.41", "pha12.4756.83",     
                     "pha12.4896.21", "pha12.5039.68", "pha12.5187.36", "pha12.5339.36", "pha12.5495.81", "pha12.5656.85", "pha12.5822.61",        
                     "pha12.5993.23", "pha12.6168.84", "pha12.6349.6", "pha12.6535.66", "pha12.6727.17", "pha12.6924.29", "pha12.7127.19",        
                     "pha12.7336.03", "pha12.7550.99", "pha12.7772.26", "pha12.8000")

names(eighteen.24) = c("tymp18", "dpoae18", "abs18.226", "abs18.257.33", "abs18.280.62", "abs18.297.3", 
                       "abs18.324.21", "abs18.343.49", "abs18.363.91", "abs18.385.55", "abs18.408.48", "abs18.432.77", "abs18.458.5",      
                       "abs18.471.94", "abs18.500", "abs18.514.65", "abs18.545.25", "abs18.561.23", "abs18.577.68", "abs18.594.6",      
                       "abs18.629.96", "abs18.648.42", "abs18.667.42", "abs18.686.98", "abs18.707.11", "abs18.727.83", "abs18.749.15",         
                       "abs18.771.11", "abs18.793.7", "abs18.816.96", "abs18.840.9", "abs18.865.54", "abs18.890.9", "abs18.917",     
                       "abs18.943.87", "abs18.971.53", "abs18.1000", "abs18.1029.3", "abs18.1059.46", "abs18.1090.51", "abs18.1122.46",        
                       "abs18.1155.35", "abs18.1189.21", "abs18.1224.05", "abs18.1259.92", "abs18.1296.84", "abs18.1334.84", "abs18.1373.95",        
                       "abs18.1414.21", "abs18.1455.65", "abs18.1498.31", "abs18.1542.21", "abs18.1587.4", "abs18.1633.92", "abs18.1681.79",      
                       "abs18.1731.07", "abs18.1781.8", "abs18.1834.01", "abs18.1887.75", "abs18.1943.06", "abs18.2000", "abs18.2058.6",         
                       "abs18.2118.93", "abs18.2181.02", "abs18.2244.92", "abs18.2310.71", "abs18.2378.41", "abs18.2448.11", "abs18.2519.84",        
                       "abs18.2593.68", "abs18.2669.68", "abs18.2747.91", "abs18.2828.43", "abs18.2911.31", "abs18.2996.61", "abs18.3084.42",       
                       "abs18.3174.8", "abs18.3267.83", "abs18.3363.59", "abs18.3462.15", "abs18.3563.59", "abs18.3668.02", "abs18.3775.5",       
                       "abs18.3886.13", "abs18.4000", "abs18.4117.21", "abs18.4237.85", "abs18.4362.03", "abs18.4489.85", "abs18.4621.41",        
                       "abs18.4756.83", "abs18.4896.21", "abs18.5039.68", "abs18.5187.36", "abs18.5339.36", "abs18.5495.81", "abs18.5656.85",        
                       "abs18.5822.61", "abs18.5993.23", "abs18.6168.84", "abs18.6349.6", "abs18.6535.66", "abs18.6727.17", "abs18.6924.29",        
                       "abs18.7127.19", "abs18.7336.03", "abs18.7550.99", "abs18.7772.26", "abs18.8000", "mag18.226",            
                       "mag18.257.33", "mag18.280.62", "mag18.297.3", "mag18.324.21", "mag18.343.49", "mag18.363.91", "mag18.385.55",         
                       "mag18.408.48", "mag18.432.77", "mag18.458.5", "mag18.471.94", "mag18.500", "mag18.514.65", "mag18.545.25",        
                       "mag18.561.23", "mag18.577.68", "mag18.594.6", "mag18.629.96", "mag18.648.42", "mag18.667.42", "mag18.686.98",         
                       "mag18.707.11", "mag18.727.83", "mag18.749.15", "mag18.771.11", "mag18.793.7", "mag18.816.96", "mag18.840.9",          
                       "mag18.865.54", "mag18.890.9", "mag18.917", "mag18.943.87", "mag18.971.53", "mag18.1000", "mag18.1029.3",         
                       "mag18.1059.46", "mag18.1090.51", "mag18.1122.46", "mag18.1155.35", "mag18.1189.21", "mag18.1224.05", "mag18.1259.92",        
                       "mag18.1296.84", "mag18.1334.84", "mag18.1373.95", "mag18.1414.21", "mag18.1455.65", "mag18.1498.31", "mag18.1542.21",       
                       "mag18.1587.4", "mag18.1633.92", "mag18.1681.79", "mag18.1731.07", "mag18.1781.8", "mag18.1834.01", "mag18.1887.75",     
                       "mag18.1943.06", "mag18.2000", "mag18.2058.6", "mag18.2118.93", "mag18.2181.02", "mag18.2244.92", "mag18.2310.71",        
                       "mag18.2378.41", "mag18.2448.11", "mag18.2519.84", "mag18.2593.68", "mag18.2669.68", "mag18.2747.91", "mag18.2828.43",        
                       "mag18.2911.31", "mag18.2996.61", "mag18.3084.42", "mag18.3174.8", "mag18.3267.83", "mag18.3363.59", "mag18.3462.15",        
                       "mag18.3563.59", "mag18.3668.02", "mag18.3775.5", "mag18.3886.13", "mag18.4000", "mag18.4117.21", "mag18.4237.85",        
                       "mag18.4362.03", "mag18.4489.85", "mag18.4621.41", "mag18.4756.83", "mag18.4896.21", "mag18.5039.68", "mag18.5187.36",        
                       "mag18.5339.36", "mag18.5495.81", "mag18.5656.85", "mag18.5822.61", "mag18.5993.23", "mag18.6168.84", "mag18.6349.6",         
                       "mag18.6535.66", "mag18.6727.17", "mag18.6924.29", "mag18.7127.19", "mag18.7336.03", "mag18.7550.99", "mag18.7772.26",        
                       "mag18.8000", "pha18.226", "pha18.257.33", "pha18.280.62", "pha18.297.3", "pha18.324.21",         
                       "pha18.343.49", "pha18.363.91", "pha18.385.55", "pha18.408.48", "pha18.432.77", "pha18.458.5", "pha18.471.94",         
                       "pha18.500", "pha18.514.65", "pha18.545.25", "pha18.561.23", "pha18.577.68", "pha18.594.6", "pha18.629.96",         
                       "pha18.648.42", "pha18.667.42", "pha18.686.98", "pha18.707.11", "pha18.727.83", "pha18.749.15", "pha18.771.11",         
                       "pha18.793.7", "pha18.816.96", "pha18.840.9", "pha18.865.54", "pha18.890.9", "pha18.917", "pha18.943.87",         
                       "pha18.971.53", "pha18.1000", "pha18.1029.3", "pha18.1059.46", "pha18.1090.51", "pha18.1122.46", "pha18.1155.35",        
                       "pha18.1189.21", "pha18.1224.05", "pha18.1259.92", "pha18.1296.84", "pha18.1334.84", "pha18.1373.95", "pha18.1414.21",        
                       "pha18.1455.65", "pha18.1498.31", "pha18.1542.21", "pha18.1587.4", "pha18.1633.92", "pha18.1681.79", "pha18.1731.07",        
                       "pha18.1781.8", "pha18.1834.01", "pha18.1887.75", "pha18.1943.06", "pha18.2000", "pha18.2058.6", "pha18.2118.93",       
                       "pha18.2181.02", "pha18.2244.92", "pha18.2310.71", "pha18.2378.41", "pha18.2448.11", "pha18.2519.84", "pha18.2593.68",        
                       "pha18.2669.68", "pha18.2747.91", "pha18.2828.43", "pha18.2911.31", "pha18.2996.61", "pha18.3084.42", "pha18.3174.8",       
                       "pha18.3267.83", "pha18.3363.59", "pha18.3462.15", "pha18.3563.59", "pha18.3668.02", "pha18.3775.5", "pha18.3886.13",       
                       "pha18.4000", "pha18.4117.21", "pha18.4237.85", "pha18.4362.03", "pha18.4489.85", "pha18.4621.41", "pha18.4756.83",     
                       "pha18.4896.21", "pha18.5039.68", "pha18.5187.36", "pha18.5339.36", "pha18.5495.81", "pha18.5656.85", "pha18.5822.61",        
                       "pha18.5993.23", "pha18.6168.84", "pha18.6349.6", "pha18.6535.66", "pha18.6727.17", "pha18.6924.29", "pha18.7127.19",        
                       "pha18.7336.03", "pha18.7550.99", "pha18.7772.26", "pha18.8000")

wai.24 = cbind.data.frame(newborn.24, six.24, twelve.24, eighteen.24)

# create a reference standard column for each, then omit the tymp and dpoae results (and aabr)
wai.24$rs0 = as.factor(with(wai.24, ifelse(aabr0=="pass" & tymp0=="pass" & dpoae0=="pass", "pass", NA)))
wai.24$rs6 = as.factor(with(wai.24, ifelse(tymp6=="pass" & dpoae6=="pass", "pass", NA)))
wai.24$rs12 = as.factor(with(wai.24, ifelse(tymp12=="pass" & dpoae12=="pass", "pass", NA)))
wai.24$rs18 = as.factor(with(wai.24, ifelse(tymp18=="pass" & dpoae18=="pass", "pass", NA)))

wai.24 = dplyr::select(wai.24, -c(aabr0, tymp0, dpoae0, tymp6, dpoae6, tymp18, dpoae18))

# infants who attended at least one follow up
wai.24.full = filter(wai.24, rs0=="pass" & rs6=="pass" | rs12=="pass" | rs18=="pass")

## create a df of ears that were tested and passed all of the follow up sessions
attended.all = filter(wai.24, rs0=="pass" & rs6=="pass" & rs12=="pass" & rs18=="pass")
setwd("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/WAI norms app/data frames")
saveRDS(attended.all, "attended.all.rds")
setwd("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/Working directory")

# subset wai for each age group and remove the worst outliers (abs < -0.5 at freqs <8000 Hz)
wai.24.full.0 = dplyr::select(wai.24.full, gender:pha0.8000, rs0)
wai.24.full.0 = wai.24.full.0[!(wai.24.full.0$id.res==661 & wai.24.full.0$ear=="L"),]
wai.24.full.0 = wai.24.full.0[!(wai.24.full.0$id.res==165 & wai.24.full.0$ear=="R"),]
wai.24.full.6 = dplyr::select(wai.24.full, c(gender:ear, abs6.226:pha6.8000, rs6))
wai.24.full.12 = dplyr::select(wai.24.full, c(gender:ear, abs12.226:pha12.8000, rs12))
wai.24.full.12 = wai.24.full.12[!(wai.24.full.12$id.res==13 & wai.24.full.12$ear=="R"),]
wai.24.full.12 = wai.24.full.12[!(wai.24.full.12$id.res==30 & wai.24.full.12$ear=="L"),]
wai.24.full.18 = dplyr::select(wai.24.full, c(gender:ear, abs18.226:pha18.8000, rs18))
wai.24.full.18 = wai.24.full.18[!(wai.24.full.18$id.res==69 & wai.24.full.18$ear=="R"),]
wai.24.full.18 = wai.24.full.18[!(wai.24.full.18$id.res==86 & wai.24.full.18$ear=="R"),]

# only want observations with complete wai results (there may be some with "pass" rs but cnt wai)
wai.24.full.0 = wai.24.full.0[complete.cases(wai.24.full.0[,-c(1:10)]), ]
wai.24.full.6 = wai.24.full.6[complete.cases(wai.24.full.6[,-c(1:10)]), ]
wai.24.full.12 = wai.24.full.12[complete.cases(wai.24.full.12[,-c(1:10)]), ]
wai.24.full.18 = wai.24.full.18[complete.cases(wai.24.full.18[,-c(1:10)]), ]

# extract each wai for each age then join all ages together with rbind for each wai for plotting
dem.0 = dplyr::select(wai.24.full.0, 1:10)
dem.6 = dplyr::select(wai.24.full.6, 1:10)
dem.12 = dplyr::select(wai.24.full.12, 1:10)
dem.18 = dplyr::select(wai.24.full.18, 1:10)

abs.24.full.0 = dplyr::select(wai.24.full.0, 1:10, abs0.226:abs0.8000, rs0)
mag.24.full.0 = dplyr::select(wai.24.full.0, 1:10, mag0.226:mag0.8000, rs0)
pha.24.full.0 = dplyr::select(wai.24.full.0, 1:10, pha0.226:pha0.8000, rs0)

names(abs.24.full.0)[names(abs.24.full.0) == "rs0"] <- "rs"
names(mag.24.full.0)[names(mag.24.full.0) == "rs0"] <- "rs"
names(pha.24.full.0)[names(pha.24.full.0) == "rs0"] <- "rs"

# keep only pass
abs.24.full.0 = filter(abs.24.full.0, rs=="pass")
mag.24.full.0 = filter(mag.24.full.0, rs=="pass")
pha.24.full.0 = filter(pha.24.full.0, rs=="pass")

# add an age variable
abs.24.full.0$age.group = "Neonate"
mag.24.full.0$age.group = "Neonate"
pha.24.full.0$age.group = "Neonate"

############ need to add rs column when select to check that they are all pass (done for newborn - need to do for other ages)
# if not all pass need to filter

abs.24.full.6 = dplyr::select(wai.24.full.6, 1:10, abs6.226:abs6.8000, rs6)
mag.24.full.6 = dplyr::select(wai.24.full.6, 1:10, mag6.226:mag6.8000, rs6)
pha.24.full.6 = dplyr::select(wai.24.full.6, 1:10, pha6.226:pha6.8000, rs6)

names(abs.24.full.6)[names(abs.24.full.6) == "rs6"] <- "rs"
names(mag.24.full.6)[names(mag.24.full.6) == "rs6"] <- "rs"
names(pha.24.full.6)[names(pha.24.full.6) == "rs6"] <- "rs"

# keep only pass
abs.24.full.6 = filter(abs.24.full.6, rs=="pass")
mag.24.full.6 = filter(mag.24.full.6, rs=="pass")
pha.24.full.6 = filter(pha.24.full.6, rs=="pass")

abs.24.full.6$age.group = "6 months"
mag.24.full.6$age.group = "6 months"
pha.24.full.6$age.group = "6 months"

abs.24.full.12 = dplyr::select(wai.24.full.12, 1:10, abs12.226:abs12.8000, rs12)
mag.24.full.12 = dplyr::select(wai.24.full.12, 1:10, mag12.226:mag12.8000, rs12)
pha.24.full.12 = dplyr::select(wai.24.full.12, 1:10, pha12.226:pha12.8000, rs12)

names(abs.24.full.12)[names(abs.24.full.12) == "rs12"] <- "rs"
names(mag.24.full.12)[names(mag.24.full.12) == "rs12"] <- "rs"
names(pha.24.full.12)[names(pha.24.full.12) == "rs12"] <- "rs"

# keep only pass
abs.24.full.12 = filter(abs.24.full.12, rs=="pass")
mag.24.full.12 = filter(mag.24.full.12, rs=="pass")
pha.24.full.12 = filter(pha.24.full.12, rs=="pass")

abs.24.full.12$age.group = "12 months"
mag.24.full.12$age.group = "12 months"
pha.24.full.12$age.group = "12 months"

abs.24.full.18 = dplyr::select(wai.24.full.18, 1:10, abs18.226:abs18.8000, rs18)
mag.24.full.18 = dplyr::select(wai.24.full.18, 1:10, mag18.226:mag18.8000, rs18)
pha.24.full.18 = dplyr::select(wai.24.full.18, 1:10, pha18.226:pha18.8000, rs18)

names(abs.24.full.18)[names(abs.24.full.18) == "rs18"] <- "rs"
names(mag.24.full.18)[names(mag.24.full.18) == "rs18"] <- "rs"
names(pha.24.full.18)[names(pha.24.full.18) == "rs18"] <- "rs"

# keep only pass
abs.24.full.18 = filter(abs.24.full.18, rs=="pass")
mag.24.full.18 = filter(mag.24.full.18, rs=="pass")
pha.24.full.18 = filter(pha.24.full.18, rs=="pass")

abs.24.full.18$age.group = "18 months"
mag.24.full.18$age.group = "18 months"
pha.24.full.18$age.group = "18 months"

# rename columns so all the same
dem.freq.names <- c("gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", "maternal.ethnicity", "age.0.hrs", "id.res", "ear", 
                    "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                    "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                    "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                    "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                    "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                    "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                    "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                    "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                    "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                    "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                    "7127.19", "7336.03", "7550.99", "7772.26", "8000.00", "rs", "age.group")

colnames(abs.24.full.0) = dem.freq.names
colnames(abs.24.full.6) = dem.freq.names
colnames(abs.24.full.12) = dem.freq.names
colnames(abs.24.full.18) = dem.freq.names

colnames(mag.24.full.0) = dem.freq.names
colnames(mag.24.full.6) = dem.freq.names
colnames(mag.24.full.12) = dem.freq.names
colnames(mag.24.full.18) = dem.freq.names

colnames(pha.24.full.0) = dem.freq.names
colnames(pha.24.full.6) = dem.freq.names
colnames(pha.24.full.12) = dem.freq.names
colnames(pha.24.full.18) = dem.freq.names

# now put abs, mag, pha together for each age group
abs.24.full = rbind.data.frame(abs.24.full.0, abs.24.full.6, abs.24.full.12, abs.24.full.18)
mag.24.full = rbind.data.frame(mag.24.full.0, mag.24.full.6, mag.24.full.12, mag.24.full.18)
pha.24.full = rbind.data.frame(pha.24.full.0, pha.24.full.6, pha.24.full.12, pha.24.full.18)

abs.24.full$age.group = as.factor(abs.24.full$age.group)
mag.24.full$age.group = as.factor(mag.24.full$age.group)
pha.24.full$age.group = as.factor(pha.24.full$age.group)

abs.24.full$age.group = factor(abs.24.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
mag.24.full$age.group = factor(mag.24.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
pha.24.full$age.group = factor(pha.24.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

# don't need the dem data for this plot - remove
abs.24.full = dplyr::select(abs.24.full, -c(1:10, 118))
mag.24.full = dplyr::select(mag.24.full, -c(1:10, 118))
pha.24.full = dplyr::select(pha.24.full, -c(1:10, 118))

# now make long form
abs.24.full <- group_by(abs.24.full, age.group)
mag.24.full <- group_by(mag.24.full, age.group)
pha.24.full <- group_by(pha.24.full, age.group)

abs.median.full <- summarise_all(abs.24.full, funs(median))
mag.median.full <- summarise_all(mag.24.full, funs(median))
pha.median.full <- summarise_all(pha.24.full, funs(median))

abs.median.long.full <- gather(abs.median.full, Frequency, absorbance, 2:108)
mag.median.long.full <- gather(mag.median.full, Frequency, magnitude, 2:108)
pha.median.long.full <- gather(pha.median.full, Frequency, phase, 2:108)

abs.median.long.full$Frequency = as.numeric(abs.median.long.full$Frequency)
mag.median.long.full$Frequency = as.numeric(mag.median.long.full$Frequency)
pha.median.long.full$Frequency = as.numeric(pha.median.long.full$Frequency)

# plot
abs.plot.full <- ggplot(abs.median.long.full, aes(x=Frequency, y=absorbance, group=age.group, colour=age.group)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(-0.05, 1.05)) +
  theme_bw() +
  scale_colour_discrete(drop=TRUE, limits = levels(abs.median.long.full$age.group), breaks = levels(abs.median.long.full$age.group)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.07,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
   #theme(legend.position="none")
#print(abs.plot.full)
ggsave("abs.plot.jpeg", abs.plot.full, height=3, width=6, dpi=500)

mag.plot.full <- ggplot(mag.median.long.full, aes(x=Frequency, y=magnitude, group=age.group, colour=age.group)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
  theme_bw() +
  scale_colour_discrete(drop=TRUE, limits = levels(mag.median.long.full$age.group), breaks = levels(mag.median.long.full$age.group)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
print(mag.plot.full)

pha.plot.full <- ggplot(pha.median.long.full, aes(x=Frequency, y=phase, group=age.group, colour=age.group)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-60, -30, 0, 30, 60, 90), limits=c(-60, 90)) +
  theme_bw() +
  #geom_hline(yintercept=45, linetype = "dashed", size = 0.4) +
  scale_colour_discrete(drop=TRUE, limits = levels(pha.median.long.full$age.group), breaks = levels(pha.median.long.full$age.group)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position="none") 
print(pha.plot.full)

wai.plot.full <- plot_grid(abs.plot.full, mag.plot.full, pha.plot.full, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 
ggsave("wai.plot.full.jpeg", wai.plot.full, height=9, width=6, dpi=500)

### plot size of IQR for each age group
# abs.24.full <- group_by(abs.24.full, age.group)
# mag.24.full <- group_by(mag.24.full, age.group)
# pha.24.full <- group_by(pha.24.full, age.group)
# 
# abs.25.full <- summarise_all(abs.24.full, funs(quantile(., 0.25)))
# abs.25.full = dplyr::select(abs.25.full, -age.group)
# abs.75.full <- summarise_all(abs.24.full, funs(quantile(., 0.75)))
# abs.75.full = dplyr::select(abs.75.full, -age.group)
# abs.iqr = abs.75.full - abs.25.full
# abs.iqr$age.group = c("Neonate", "6 months", "12 months", "18 months")
# 
# mag.25.full <- summarise_all(mag.24.full, funs(quantile(., 0.25)))
# mag.25.full = dplyr::select(mag.25.full, -age.group)
# mag.75.full <- summarise_all(mag.24.full, funs(quantile(., 0.75)))
# mag.75.full = dplyr::select(mag.75.full, -age.group)
# mag.iqr = mag.75.full - mag.25.full
# mag.iqr$age.group = c("Neonate", "6 months", "12 months", "18 months")
# 
# pha.25.full <- summarise_all(pha.24.full, funs(quantile(., 0.25)))
# pha.25.full = dplyr::select(pha.25.full, -age.group)
# pha.75.full <- summarise_all(pha.24.full, funs(quantile(., 0.75)))
# pha.75.full = dplyr::select(pha.75.full, -age.group)
# pha.iqr = pha.75.full - pha.25.full
# pha.iqr$age.group = c("Neonate", "6 months", "12 months", "18 months")
# 
# abs.iqr.long <- gather(abs.iqr, Frequency, Absorbance, 1:107)
# mag.iqr.long <- gather(mag.iqr, Frequency, Magnitude, 1:107)
# pha.iqr.long <- gather(pha.iqr, Frequency, Phase, 1:107)
# 
# abs.iqr.long$Frequency = as.numeric(abs.iqr.long$Frequency)
# mag.iqr.long$Frequency = as.numeric(mag.iqr.long$Frequency)
# pha.iqr.long$Frequency = as.numeric(pha.iqr.long$Frequency)
# 
# abs.iqr.plot <- ggplot(abs.iqr.long, aes(x=Frequency, y=Absorbance, group=age.group, colour=age.group)) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("A")))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 0.6)) +
#   theme_bw() +
#   scale_colour_discrete(drop=TRUE, limits = levels(pha.median.long.full$age.group), breaks = levels(pha.median.long.full$age.group)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
#         legend.position=c(0,1)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   theme(legend.position="none") 
# print(abs.iqr.plot)
# 
# mag.iqr.plot <- ggplot(mag.iqr.long, aes(x=Frequency, y=Magnitude, group=age.group, colour=age.group)) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
#   theme_bw() +
#   scale_colour_discrete(drop=TRUE, limits = levels(pha.median.long.full$age.group), breaks = levels(pha.median.long.full$age.group)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
#         legend.position=c(0,1)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   theme(legend.position="none") 
# print(mag.iqr.plot)
# 
# pha.iqr.plot <- ggplot(pha.iqr.long, aes(x=Frequency, y=Phase, group=age.group, colour=age.group)) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-60, -30, 0, 30, 60, 90), limits=c(0, 120)) +
#   theme_bw() +
#   scale_colour_discrete(drop=TRUE, limits = levels(pha.median.long.full$age.group), breaks = levels(pha.median.long.full$age.group)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
#         legend.position=c(0,1)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   theme(legend.position="none") 
# print(pha.iqr.plot)

# Demographics plots - by sex, ear and ethnicity 
# sex plot
abs.24.sex = rbind.data.frame(abs.24.full.0, abs.24.full.6, abs.24.full.12, abs.24.full.18)
mag.24.sex = rbind.data.frame(mag.24.full.0, mag.24.full.6, mag.24.full.12, mag.24.full.18)
pha.24.sex = rbind.data.frame(pha.24.full.0, pha.24.full.6, pha.24.full.12, pha.24.full.18)

abs.24.sex$age.group = as.factor(abs.24.sex$age.group)
mag.24.sex$age.group = as.factor(mag.24.sex$age.group)
pha.24.sex$age.group = as.factor(pha.24.sex$age.group)

abs.24.sex$age.group = factor(abs.24.sex$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
mag.24.sex$age.group = factor(mag.24.sex$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
pha.24.sex$age.group = factor(pha.24.sex$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

abs.24.sex = dplyr::select(abs.24.sex, -c(ga:ear, rs))
abs.24.m = filter(abs.24.sex, gender == "M")
abs.24.f = filter(abs.24.sex, gender == "F")
abs.24.m = dplyr::select(abs.24.m, -gender)
abs.24.f = dplyr::select(abs.24.f, -gender)
abs.24.m = group_by(abs.24.m, age.group)
abs.24.f = group_by(abs.24.f, age.group)

mag.24.sex = dplyr::select(mag.24.sex, -c(ga:ear, rs))
mag.24.m = filter(mag.24.sex, gender == "M")
mag.24.f = filter(mag.24.sex, gender == "F")
mag.24.m = dplyr::select(mag.24.m, -gender)
mag.24.f = dplyr::select(mag.24.f, -gender)
mag.24.m = group_by(mag.24.m, age.group)
mag.24.f = group_by(mag.24.f, age.group) 
  
pha.24.sex = dplyr::select(pha.24.sex, -c(ga:ear, rs))
pha.24.m = filter(pha.24.sex, gender == "M")
pha.24.f = filter(pha.24.sex, gender == "F")
pha.24.m = dplyr::select(pha.24.m, -gender)
pha.24.f = dplyr::select(pha.24.f, -gender)
pha.24.m = group_by(pha.24.m, age.group)
pha.24.f = group_by(pha.24.f, age.group)

abs.median.m <- summarise_all(abs.24.m, funs(median))
mag.median.m <- summarise_all(mag.24.m, funs(median))
pha.median.m <- summarise_all(pha.24.m, funs(median))

abs.median.f <- summarise_all(abs.24.f, funs(median))
mag.median.f <- summarise_all(mag.24.f, funs(median))
pha.median.f <- summarise_all(pha.24.f, funs(median))

abs.long.m <- melt(abs.median.m, id.vars=c("age.group"))
abs.long.m <- plyr::rename(abs.long.m, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.m$Frequency <- as.numeric(as.character(abs.long.m$Frequency))

abs.long.f <- melt(abs.median.f, id.vars=c("age.group"))
abs.long.f <- plyr::rename(abs.long.f, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.f$Frequency <- as.numeric(as.character(abs.long.f$Frequency))

mag.long.m <- melt(mag.median.m, id.vars=c("age.group"))
mag.long.m <- plyr::rename(mag.long.m, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.m$Frequency <- as.numeric(as.character(mag.long.m$Frequency))

mag.long.f <- melt(mag.median.f, id.vars=c("age.group"))
mag.long.f <- plyr::rename(mag.long.f, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.f$Frequency <- as.numeric(as.character(mag.long.f$Frequency))

pha.long.m <- melt(pha.median.m, id.vars=c("age.group"))
pha.long.m <- plyr::rename(pha.long.m, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.m$Frequency <- as.numeric(as.character(pha.long.m$Frequency))

pha.long.f <- melt(pha.median.f, id.vars=c("age.group"))
pha.long.f <- plyr::rename(pha.long.f, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.f$Frequency <- as.numeric(as.character(pha.long.f$Frequency))

abs.long.m$Sex = "Male"
abs.long.f$Sex = "Female"
mag.long.m$Sex = "Male"
mag.long.f$Sex = "Female"
pha.long.m$Sex = "Male"
pha.long.f$Sex = "Female"

abs.long.sex = rbind(abs.long.m, abs.long.f)
mag.long.sex = rbind(mag.long.m, mag.long.f)
pha.long.sex = rbind(pha.long.m, pha.long.f)

abs.sex.plot <- ggplot(abs.long.sex) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Absorbance, linetype=factor(Sex), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) + 
  theme(legend.position="none")
#print(abs.sex.plot)  

mag.sex.plot <- ggplot(mag.long.sex) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Magnitude, linetype=factor(Sex), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 14)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.98))
  #theme(legend.position="none")
#print(mag.sex.plot)  

pha.sex.plot <- ggplot(pha.long.sex) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Phase, linetype=factor(Sex), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-60, -30, 0, 30, 60, 90), limits=c(-60, 90)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) + 
  theme(legend.position="none")
#print(pha.sex.plot)  

sex.plot.full <- plot_grid(abs.sex.plot, mag.sex.plot, pha.sex.plot, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 
ggsave("sex.plot.full.jpeg", sex.plot.full, height=9, width=6, dpi=500)

# ear plot
abs.24.ear = rbind.data.frame(abs.24.full.0, abs.24.full.6, abs.24.full.12, abs.24.full.18)
mag.24.ear = rbind.data.frame(mag.24.full.0, mag.24.full.6, mag.24.full.12, mag.24.full.18)
pha.24.ear = rbind.data.frame(pha.24.full.0, pha.24.full.6, pha.24.full.12, pha.24.full.18)

abs.24.ear$age.group = as.factor(abs.24.ear$age.group)
mag.24.ear$age.group = as.factor(mag.24.ear$age.group)
pha.24.ear$age.group = as.factor(pha.24.ear$age.group)

abs.24.ear$age.group = factor(abs.24.ear$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
mag.24.ear$age.group = factor(mag.24.ear$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
pha.24.ear$age.group = factor(pha.24.ear$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

abs.24.ear = dplyr::select(abs.24.ear, -c(gender:id.res, rs))
abs.24.ear$ear = as.factor(abs.24.ear$ear)
abs.24.ear$ear = relevel(abs.24.ear$ear, "R")
abs.24.r = filter(abs.24.ear, ear == "R")
abs.24.l = filter(abs.24.ear, ear == "L")
abs.24.r = dplyr::select(abs.24.r, -ear)
abs.24.l = dplyr::select(abs.24.l, -ear)
abs.24.r = group_by(abs.24.r, age.group)
abs.24.l = group_by(abs.24.l, age.group)

mag.24.ear = dplyr::select(mag.24.ear, -c(gender:id.res, rs))
mag.24.ear$ear = as.factor(mag.24.ear$ear)
mag.24.ear$ear = relevel(mag.24.ear$ear, "R")
mag.24.r = filter(mag.24.ear, ear == "R")
mag.24.l = filter(mag.24.ear, ear == "L")
mag.24.r = dplyr::select(mag.24.r, -ear)
mag.24.l = dplyr::select(mag.24.l, -ear)
mag.24.r = group_by(mag.24.r, age.group)
mag.24.l = group_by(mag.24.l, age.group)

pha.24.ear = dplyr::select(pha.24.ear, -c(gender:id.res, rs))
pha.24.ear$ear = as.factor(pha.24.ear$ear)
pha.24.ear$ear = relevel(pha.24.ear$ear, "R")
pha.24.r = filter(pha.24.ear, ear == "R")
pha.24.l = filter(pha.24.ear, ear == "L")
pha.24.r = dplyr::select(pha.24.r, -ear)
pha.24.l = dplyr::select(pha.24.l, -ear)
pha.24.r = group_by(pha.24.r, age.group)
pha.24.l = group_by(pha.24.l, age.group)

abs.median.r <- summarise_all(abs.24.r, funs(median))
mag.median.r <- summarise_all(mag.24.r, funs(median))
pha.median.r <- summarise_all(pha.24.r, funs(median))

abs.median.l <- summarise_all(abs.24.l, funs(median))
mag.median.l <- summarise_all(mag.24.l, funs(median))
pha.median.l <- summarise_all(pha.24.l, funs(median))

abs.long.r <- melt(abs.median.r, id.vars=c("age.group"))
abs.long.r <- plyr::rename(abs.long.r, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.r$Frequency <- as.numeric(as.character(abs.long.r$Frequency))

abs.long.l <- melt(abs.median.l, id.vars=c("age.group"))
abs.long.l <- plyr::rename(abs.long.l, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.l$Frequency <- as.numeric(as.character(abs.long.l$Frequency))

mag.long.r <- melt(mag.median.r, id.vars=c("age.group"))
mag.long.r <- plyr::rename(mag.long.r, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.r$Frequency <- as.numeric(as.character(mag.long.r$Frequency))

mag.long.l <- melt(mag.median.l, id.vars=c("age.group"))
mag.long.l <- plyr::rename(mag.long.l, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.l$Frequency <- as.numeric(as.character(mag.long.l$Frequency))

pha.long.r <- melt(pha.median.r, id.vars=c("age.group"))
pha.long.r <- plyr::rename(pha.long.r, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.r$Frequency <- as.numeric(as.character(pha.long.r$Frequency))

pha.long.l <- melt(pha.median.l, id.vars=c("age.group"))
pha.long.l <- plyr::rename(pha.long.l, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.l$Frequency <- as.numeric(as.character(pha.long.l$Frequency))

abs.long.r$ear = "R"
abs.long.l$ear = "L"
mag.long.r$ear = "R"
mag.long.l$ear = "L"
pha.long.r$ear = "R"
pha.long.l$ear = "L"

abs.long.ear = rbind(abs.long.r, abs.long.l)
mag.long.ear = rbind(mag.long.r, mag.long.l)
pha.long.ear = rbind(pha.long.r, pha.long.l)

abs.ear.plot <- ggplot(abs.long.ear) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Absorbance, linetype=factor(ear), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) + 
  theme(legend.position="none")
#print(abs.ear.plot)  

mag.ear.plot <- ggplot(mag.long.ear) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Magnitude, linetype=factor(ear), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.05, 0.95))
  #theme(legend.position="none")
#print(mag.ear.plot)  

pha.ear.plot <- ggplot(pha.long.ear) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Phase, linetype=factor(ear), colour=Age))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-60, -30, 0, 30, 60, 90), limits=c(-60, 90)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) + 
  theme(legend.position="none")
#print(pha.ear.plot)  

#ear.plot.full <- plot_grid(abs.ear.plot, mag.ear.plot, pha.ear.plot, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 
#ggsave("ear.plot.full.jpeg", ear.plot.full, height=9, width=6, dpi=500)

# ethnicity plot
abs.24.eth = rbind.data.frame(abs.24.full.0, abs.24.full.6, abs.24.full.12, abs.24.full.18)
mag.24.eth = rbind.data.frame(mag.24.full.0, mag.24.full.6, mag.24.full.12, mag.24.full.18)
pha.24.eth = rbind.data.frame(pha.24.full.0, pha.24.full.6, pha.24.full.12, pha.24.full.18)

abs.24.eth$age.group = as.factor(abs.24.eth$age.group)
mag.24.eth$age.group = as.factor(mag.24.eth$age.group)
pha.24.eth$age.group = as.factor(pha.24.eth$age.group)

abs.24.eth$age.group = factor(abs.24.eth$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
mag.24.eth$age.group = factor(mag.24.eth$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
pha.24.eth$age.group = factor(pha.24.eth$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

abs.24.eth = dplyr::select(abs.24.eth, -c(gender:body.length, age.0.hrs:ear, rs))
abs.24.eth$maternal.ethnicity = as.factor(abs.24.eth$maternal.ethnicity)
abs.24.eth$maternal.ethnicity[abs.24.eth$maternal.ethnicity == "African"] = NA
abs.24.eth$maternal.ethnicity[abs.24.eth$maternal.ethnicity == "Oceanian"] = NA
abs.24.eth$maternal.ethnicity[abs.24.eth$maternal.ethnicity == "South American"] = NA
abs.24.cauc = filter(abs.24.eth, maternal.ethnicity == "Caucasian")
abs.24.asian = filter(abs.24.eth, maternal.ethnicity == "Asian")
abs.24.cauc = dplyr::select(abs.24.cauc, -maternal.ethnicity)
abs.24.asian = dplyr::select(abs.24.asian, -maternal.ethnicity)
abs.24.cauc = group_by(abs.24.cauc, age.group)
abs.24.asian = group_by(abs.24.asian, age.group)

mag.24.eth = dplyr::select(mag.24.eth, -c(gender:body.length, age.0.hrs:ear, rs))
mag.24.eth$maternal.ethnicity = as.factor(mag.24.eth$maternal.ethnicity)
mag.24.eth$maternal.ethnicity[mag.24.eth$maternal.ethnicity == "African"] = NA
mag.24.eth$maternal.ethnicity[mag.24.eth$maternal.ethnicity == "Oceanian"] = NA
mag.24.eth$maternal.ethnicity[mag.24.eth$maternal.ethnicity == "South American"] = NA
mag.24.cauc = filter(mag.24.eth, maternal.ethnicity == "Caucasian")
mag.24.asian = filter(mag.24.eth, maternal.ethnicity == "Asian")
mag.24.cauc = dplyr::select(mag.24.cauc, -maternal.ethnicity)
mag.24.asian = dplyr::select(mag.24.asian, -maternal.ethnicity)
mag.24.cauc = group_by(mag.24.cauc, age.group)
mag.24.asian = group_by(mag.24.asian, age.group)

pha.24.eth = dplyr::select(pha.24.eth, -c(gender:body.length, age.0.hrs:ear, rs))
pha.24.eth$maternal.ethnicity = as.factor(pha.24.eth$maternal.ethnicity)
pha.24.eth$maternal.ethnicity[pha.24.eth$maternal.ethnicity == "African"] = NA
pha.24.eth$maternal.ethnicity[pha.24.eth$maternal.ethnicity == "Oceanian"] = NA
pha.24.eth$maternal.ethnicity[pha.24.eth$maternal.ethnicity == "South American"] = NA
pha.24.cauc = filter(pha.24.eth, maternal.ethnicity == "Caucasian")
pha.24.asian = filter(pha.24.eth, maternal.ethnicity == "Asian")
pha.24.cauc = dplyr::select(pha.24.cauc, -maternal.ethnicity)
pha.24.asian = dplyr::select(pha.24.asian, -maternal.ethnicity)
pha.24.cauc = group_by(pha.24.cauc, age.group)
pha.24.asian = group_by(pha.24.asian, age.group)

abs.median.cauc <- summarise_all(abs.24.cauc, funs(median))
mag.median.cauc <- summarise_all(mag.24.cauc, funs(median))
pha.median.cauc <- summarise_all(pha.24.cauc, funs(median))

abs.median.asian <- summarise_all(abs.24.asian, funs(median))
mag.median.asian <- summarise_all(mag.24.asian, funs(median))
pha.median.asian <- summarise_all(pha.24.asian, funs(median))

abs.long.cauc <- melt(abs.median.cauc, id.vars=c("age.group"))
abs.long.cauc <- plyr::rename(abs.long.cauc, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.cauc$Frequency <- as.numeric(as.character(abs.long.cauc$Frequency))
abs.long.asian <- melt(abs.median.asian, id.vars=c("age.group"))
abs.long.asian <- plyr::rename(abs.long.asian, c("age.group"="Age", "variable"="Frequency", "value"="Absorbance"))
abs.long.asian$Frequency <- as.numeric(as.character(abs.long.asian$Frequency))

mag.long.cauc <- melt(mag.median.cauc, id.vars=c("age.group"))
mag.long.cauc <- plyr::rename(mag.long.cauc, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.cauc$Frequency <- as.numeric(as.character(mag.long.cauc$Frequency))
mag.long.asian <- melt(mag.median.asian, id.vars=c("age.group"))
mag.long.asian <- plyr::rename(mag.long.asian, c("age.group"="Age", "variable"="Frequency", "value"="Magnitude"))
mag.long.asian$Frequency <- as.numeric(as.character(mag.long.asian$Frequency))

pha.long.cauc <- melt(pha.median.cauc, id.vars=c("age.group"))
pha.long.cauc <- plyr::rename(pha.long.cauc, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.cauc$Frequency <- as.numeric(as.character(pha.long.cauc$Frequency))
pha.long.asian <- melt(pha.median.asian, id.vars=c("age.group"))
pha.long.asian <- plyr::rename(pha.long.asian, c("age.group"="Age", "variable"="Frequency", "value"="Phase"))
pha.long.asian$Frequency <- as.numeric(as.character(pha.long.asian$Frequency))

abs.long.cauc$eth = "Caucasian"
abs.long.asian$eth = "Asian"

mag.long.cauc$eth = "Caucasian"
mag.long.asian$eth = "Asian"

pha.long.cauc$eth = "Caucasian"
pha.long.asian$eth = "Asian"

abs.long.eth = rbind(abs.long.cauc, abs.long.asian)
mag.long.eth = rbind(mag.long.cauc, mag.long.asian)
pha.long.eth = rbind(pha.long.cauc, pha.long.asian)

abs.eth.plot <- ggplot(abs.long.eth) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Absorbance, colour=factor(eth)))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(-0.1, 1.1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.98)) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(abs.eth.plot)  
ggsave("abs.eth.plot.jpeg", abs.eth.plot, height=6, width=10, dpi=500)

mag.eth.plot <- ggplot(mag.long.eth) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Magnitude, colour=factor(eth)))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.98)) +
  facet_wrap(~Age, ncol = 2, scales="free")
print(mag.eth.plot)  
ggsave("mag.eth.plot.jpeg", mag.eth.plot, height=6, width=10, dpi=500)

pha.eth.plot <- ggplot(pha.long.eth) +
  theme_bw() +
  geom_line(aes(x = Frequency, y = Phase, colour=factor(eth)))  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-90, -60, -30, 0, 30, 60, 90), limits=c(-95, 95)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.75)) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(pha.eth.plot)  
ggsave("pha.eth.plot.jpeg", pha.eth.plot, height=6, width=10, dpi=500)

################### Plot each age group in terms of G and B (can plot on same plot because same units - mmho)
# newborn
mag.only.0 = dplyr::select(mag.24.full.0, 11:117)
pha.only.0 = dplyr::select(pha.24.full.0, 11:117)
# need to convert to radians
pha.only.0.rad = pha.only.0 * pi / 180
dems = dplyr::select(mag.24.full.0, 1:8)
## conversion is from ASHA principles of tympanometry table 1.1 p. 3
G.0 = mag.only.0 * cos(pha.only.0.rad)
B.0 = mag.only.0 * sin(pha.only.0.rad)

G.median <- summarise_all(G.0, funs(median))
B.median = summarise_all(B.0, funs(median))
G.median$Y = "G"
B.median$Y = "B"
rect.median.0 = rbind.data.frame(G.median, B.median)
rect.median.long.0 = gather(rect.median.0, Frequency, Value, 1:107)
rect.median.long.0$Frequency = as.numeric(rect.median.long.0$Frequency)
rect.median.long.0$Age = "Neonate"

# 6 mth
mag.only.6 = dplyr::select(mag.24.full.6, 11:117)
pha.only.6 = dplyr::select(pha.24.full.6, 11:117)
# need to convert to radians
pha.only.6.rad = pha.only.6 * pi / 180
dems = dplyr::select(mag.24.full.6, 1:8)
G.6 = mag.only.6 * cos(pha.only.6.rad)
B.6 = mag.only.6 * sin(pha.only.6.rad)

G.median <- summarise_all(G.6, funs(median))
B.median = summarise_all(B.6, funs(median))
G.median$Y = "G"
B.median$Y = "B"
rect.median.6 = rbind.data.frame(G.median, B.median)
rect.median.long.6 = gather(rect.median.6, Frequency, Value, 1:107)
rect.median.long.6$Frequency = as.numeric(rect.median.long.6$Frequency)
rect.median.long.6$Age = "6 months"

# 12 mth
mag.only.12 = dplyr::select(mag.24.full.12, 11:117)
pha.only.12 = dplyr::select(pha.24.full.12, 11:117)
# need to convert to radians
pha.only.12.rad = pha.only.12 * pi / 180
dems = dplyr::select(mag.24.full.12, 1:8)
G.12 = mag.only.12 * cos(pha.only.12.rad)
B.12 = mag.only.12 * sin(pha.only.12.rad)

G.median <- summarise_all(G.12, funs(median))
B.median = summarise_all(B.12, funs(median))
G.median$Y = "G"
B.median$Y = "B"
rect.median.12 = rbind.data.frame(G.median, B.median)
rect.median.long.12 = gather(rect.median.12, Frequency, Value, 1:107)
rect.median.long.12$Frequency = as.numeric(rect.median.long.12$Frequency)
rect.median.long.12$Age = "12 months"

# 18 mth
mag.only.18 = dplyr::select(mag.24.full.18, 11:117)
pha.only.18 = dplyr::select(pha.24.full.18, 11:117)
# need to convert to radians
pha.only.18.rad = pha.only.18 * pi / 180
dems = dplyr::select(mag.24.full.18, 1:8)
G.18 = mag.only.18 * cos(pha.only.18.rad)
B.18 = mag.only.18 * sin(pha.only.18.rad)

G.median <- summarise_all(G.18, funs(median))
B.median = summarise_all(B.18, funs(median))
G.median$Y = "G"
B.median$Y = "B"
rect.median.18 = rbind.data.frame(G.median, B.median)
rect.median.long.18 = gather(rect.median.18, Frequency, Value, 1:107)
rect.median.long.18$Frequency = as.numeric(rect.median.long.18$Frequency)
rect.median.long.18$Age = "18 months"

rect.full.long = rbind.data.frame(rect.median.long.0, rect.median.long.6, rect.median.long.12, rect.median.long.18)
rect.full.long$Age = factor(rect.full.long$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))

rect.plot <- ggplot(rect.full.long, aes(x=Frequency, y=Value, group=Y, colour=Y)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("Y"), ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-4, -2, 0, 2, 4, 6, 8, 10), limits=c(-4, 10)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.02, 0.98)) +
  facet_wrap(~Age, ncol = 2, scales="free")
print(rect.plot)

ggsave("rect.plot.full.jpeg", rect.plot, height=6, width=10, dpi=500)

# or plot G and B for each age group
rect.median.long.0$Age = "Neonate"
rect.median.long.6$Age = "6 months"
rect.median.long.12$Age = "12 months"
rect.median.long.18$Age = "18 months"

rect.full = rbind.data.frame(rect.median.long.0, rect.median.long.6, rect.median.long.12, rect.median.long.18)
g.rect.full = filter(rect.full, Y=="G")
b.rect.full = filter(rect.full, Y=="B")
g.rect.full$Age = factor(g.rect.full$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))
b.rect.full$Age = factor(b.rect.full$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))

# conductance on dB scale (see Keefe 1993 discussion)
#g.db = 10 * log10(g.rect.full$Value)
#g.rect.full$g.db = g.db

g.plot <- ggplot(g.rect.full, aes(x=Frequency, y=Value, group=Age, colour=Age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("G")["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(-0.2, 10.2)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.05, 0.95)) +
 theme(legend.position="none")
#print(g.plot)

b.plot <- ggplot(b.rect.full, aes(x=Frequency, y=Value, group=Age, colour=Age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("B")["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-4, -2, 0, 2, 4, 6, 8), limits=c(-4, 8)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position="none")
#print(b.plot)

gb.plots <- plot_grid(g.plot, b.plot, nrow=2, ncol=1, align = "v", labels = c("A", "B")) 
ggsave("gb.plots.jpeg", gb.plots, height=6, width=6, dpi=500)

# admittance at the tip
Y.plots <- plot_grid(mag.plot.full, pha.plot.full, g.plot, b.plot, nrow=2, ncol=2, align = "v", labels = c("A", "B", "C", "D")) 
ggsave("Y.plots.jpeg", Y.plots, height=6, width=12, dpi=500)

###### normalized admittance
# Zc = pc/S = rho * c / S  (needs to be cgs units)
# calculate ear canal (S) area acoustically
## S = pc/average resistance
## first set resistance <0 to 0
## Caution! Resistance is NOT 1/G - it is G / (G2 + B2) = G / |Y|^2
# ear canal length is estimated from group delay so I couldn't do it

# ear canal diameter from keefe 1993
# to convert to area in cm^2 go D/2 to get radius (r), then pi*r^2, then multiply by 0.01 to convert to cm^2
diameter.to.area = function(x) {
  pi * ((x / 2)^2)
}

# 6 mth 6.3 mm diameter
diameter.to.area(6.3) # area = 31.17 mm^2 
# 12 mth 7 mm diameter
diameter.to.area(7) # area = 38.48 mm^2 
# 24 mth 7.7 mm diameter
diameter.to.area(7.7) # area = 47.57 mm^2 

# hunter (2016) presents ear canal area in cm^2 (ls means) 
#  x100 to get mm^2
# newborn = 0.06 cm^2 = 6 mm^2
# 6 mths = 0.24 cm^2 = 24 mm^2
#12 mths = 0.22 cm^2 = 22 mm^2

# Resistance = G / (G^2 + B^2) = G / |Y|^2
# convert G and B to ohm
G.0.mho = G.0 * 0.001
B.0.mho = B.0 * 0.001
G.6.mho = G.6 * 0.001
B.6.mho = B.6 * 0.001
G.12.mho = G.12 * 0.001
B.12.mho = B.12 * 0.001
G.18.mho = G.18 * 0.001
B.18.mho = B.18 * 0.001

r.0 = G.0.mho / (G.0.mho^2 + B.0.mho^2)
r.6 = G.6.mho / (G.6.mho^2 + B.6.mho^2)
r.12 = G.12.mho / (G.12.mho^2 + B.12.mho^2)
r.18 = G.18.mho / (G.18.mho^2 + B.18.mho^2)

# change <0 to 0 - see keefe 2015 and keefe and abdula 2007
r.0[r.0 < 0] = 0
r.6[r.6 < 0] = 0
r.12[r.12 < 0] = 0
r.18[r.18 < 0] = 0

# then take rowMeans()
r.0.ave = as.data.frame(rowMeans(r.0))
r.6.ave = as.data.frame(rowMeans(r.6))
r.12.ave = as.data.frame(rowMeans(r.12))
r.18.ave = as.data.frame(rowMeans(r.18))

# then calculate S (for each ear)
# need to use cgs units
## rho g/cm^3 (rho) and c cm*s^-1 (c)
## rho and c values are in Benade 1968
# Table 2 gives the values at 26.85 degrees, with a correction factor
# rho is (1.1769*10^-3) * (1-0.00335 * (x - 26.85)) - where x is the room temperature
# so for 22 degrees:

rho = (1.1769*10^-3) * (1-0.00335 * (22 - 26.85)) 
c = (3.4723*10^4) * (1 + 0.00166 * (22 - 26.85)) 
# note in R "e" means "*10^"

S.0 = (rho*c)/r.0.ave
S.6 = (rho*c)/r.6.ave
S.12 = (rho*c)/r.12.ave
S.18 = (rho*c)/r.18.ave

names(S.0) = "S.cm2"
names(S.6) = "S.cm2"
names(S.12) = "S.cm2"
names(S.18) = "S.cm2"

S.0$age.group = "Neonate"
S.6$age.group = "6 months"
S.12$age.group = "12 months"
S.18$age.group = "18 months"
# add dems for modeling with 1/2 octave

# average S (area) for each age group
s.0.ave = mean(S.0$S.cm2) 
s.6.ave = mean(S.6$S.cm2)
s.12.ave = mean(S.12$S.cm2)
s.18.ave = mean(S.18$S.cm2)

s.mm.0 = s.0.ave * 100
s.mm.6 = s.6.ave * 100
s.mm.12 = s.12.ave * 100
s.mm.18 =  s.18.ave * 100

# plot ear canal area
hunter.canal.mm2 = c("6", "24", "22", NA, NA)
keefe.canal.mm2 = c(NA, "31.17", "38.48", NA, "47.57")
myers.canal.mm2 = c(s.mm.0, s.mm.6, s.mm.12, s.mm.18, NA)
ear.canal.other.studies = rbind.data.frame(hunter.canal.mm2, keefe.canal.mm2, myers.canal.mm2)
names(ear.canal.other.studies) = c("Neonate", "6 months", "12 months", "18 months", "24 months")
ear.canal.other.studies$Study = c("Hunter et al. (2016)", "Keefe et al. (1993)", "Present study")
ear.canal.studies.long = gather(ear.canal.other.studies, Age, Area, 1:5)
ear.canal.studies.long$Age = factor(ear.canal.studies.long$Age, levels = c("Neonate", "6 months", "12 months", "18 months", "24 months"))
ear.canal.studies.long$Area = as.numeric(ear.canal.studies.long$Area)
ear.canal.studies.long = na.omit(ear.canal.studies.long)
ear.canal.plot = ggplot(ear.canal.studies.long, aes(x = Age, y = Area, group = Study, colour = Study)) +
  geom_line() +
  theme_bw()
#print(ear.canal.plot)

# options(scipen = 999) # to turn of scientific notation
# options(scipen = 0) to turn it back on
# to get diameter go S.area/pi then take sqrt (this is radius) and muliply by 2 for diameter
(sqrt(s.0.ave/pi)) *2

# then Zc for each ear
# zc = rho * c * S
Zc.0 = as.data.frame((rho*c)/S.0$S.cm2)
Zc.6 = as.data.frame((rho*c)/S.6$S.cm2)
Zc.12 = as.data.frame((rho*c)/S.12$S.cm2)
Zc.18 = as.data.frame((rho*c)/S.18$S.cm2)
names(Zc.0) = "Zc"
names(Zc.6) = "Zc"
names(Zc.12) = "Zc"
names(Zc.18) = "Zc"

# convert mag to mho 
# although doesn't seem to matter
mag.only.0.mho = mag.only.0 * 0.001
mag.only.0.mho = cbind.data.frame(mag.only.0.mho, Zc.0)
mag.only.0.mho = group_by(mag.only.0.mho, Zc)
mag.0.norm = mutate_all(mag.only.0.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(mag.0.norm)
mag.0.norm = ungroup(mag.0.norm)
mag.0.norm = dplyr::select(mag.0.norm, -Zc)
mag.0.norm$Age = "Neonate"

mag.only.6.mho = mag.only.6 * 0.001
mag.only.6.mho = cbind.data.frame(mag.only.6.mho, Zc.6)
mag.only.6.mho = group_by(mag.only.6.mho, Zc)
mag.6.norm = mutate_all(mag.only.6.mho, funs(. * Zc)) 
is.grouped_df(mag.6.norm)
mag.6.norm = ungroup(mag.6.norm)
mag.6.norm = dplyr::select(mag.6.norm, -Zc)
mag.6.norm$Age = "6 months"

mag.only.12.mho = mag.only.12 * 0.001
mag.only.12.mho = cbind.data.frame(mag.only.12.mho, Zc.12)
mag.only.12.mho = group_by(mag.only.12.mho, Zc)
mag.12.norm = mutate_all(mag.only.12.mho, funs(. * Zc)) 
is.grouped_df(mag.12.norm)
mag.12.norm = ungroup(mag.12.norm)
mag.12.norm = dplyr::select(mag.12.norm, -Zc)
mag.12.norm$Age = "12 months"

mag.only.18.mho = mag.only.18 * 0.001
mag.only.18.mho = cbind.data.frame(mag.only.18.mho, Zc.18)
mag.only.18.mho = group_by(mag.only.18.mho, Zc)
mag.18.norm = mutate_all(mag.only.18.mho, funs(. * Zc)) 
is.grouped_df(mag.18.norm)
mag.18.norm = ungroup(mag.18.norm)
mag.18.norm = dplyr::select(mag.18.norm, -Zc)
mag.18.norm$Age = "18 months"

mag.norm = rbind.data.frame(mag.0.norm, mag.6.norm, mag.12.norm, mag.18.norm)
mag.norm$Age = factor(mag.norm$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))
# now make long form
mag.norm <- group_by(mag.norm, Age)
mag.norm.median <- summarise_all(mag.norm, funs(median))
mag.norm.median.long <- gather(mag.norm.median, Frequency, magnitude, 2:108)
mag.norm.median.long$Frequency = as.numeric(mag.norm.median.long$Frequency)

mag.norm.plot <- ggplot(mag.norm.median.long, aes(x=Frequency, y=magnitude, group=Age, colour=Age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5, 2), limits=c(0, 2)) +
  theme_bw() +
  scale_colour_discrete(drop=TRUE, limits = levels(mag.norm.median.long$Age), breaks = levels(mag.norm.median.long$Age)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = "none")
#print(mag.norm.plot)

## or on dB scale 
mag.0.dB = 10 * log10(mag.only.0.mho)
mag.0.dB.norm = group_by(mag.only.0.mho, Zc)
mag.0.dB.norm = mutate_all(mag.0.dB.norm, funs(. * Zc)) # this is Ynorm
is.grouped_df(mag.0.dB.norm)
mag.0.dB.norm = ungroup(mag.0.dB.norm)
mag.0.dB.norm = dplyr::select(mag.0.dB.norm, -Zc)
mag.0.dB.norm = 10 * log10(mag.0.dB.norm)

mag.6.dB = 10 * log10(mag.only.6.mho)
mag.6.dB.norm = group_by(mag.only.6.mho, Zc)
mag.6.dB.norm = mutate_all(mag.6.dB.norm, funs(. * Zc)) # this is Ynorm
is.grouped_df(mag.6.dB.norm)
mag.6.dB.norm = ungroup(mag.6.dB.norm)
mag.6.dB.norm = dplyr::select(mag.6.dB.norm, -Zc)
mag.6.dB.norm = 10 * log10(mag.6.dB.norm)

mag.12.dB = 10 * log10(mag.only.12.mho)
mag.12.dB.norm = group_by(mag.only.12.mho, Zc)
mag.12.dB.norm = mutate_all(mag.12.dB.norm, funs(. * Zc)) # this is Ynorm
is.grouped_df(mag.12.dB.norm)
mag.12.dB.norm = ungroup(mag.12.dB.norm)
mag.12.dB.norm = dplyr::select(mag.12.dB.norm, -Zc)
mag.12.dB.norm = 10 * log10(mag.12.dB.norm)

mag.18.dB = 10 * log10(mag.only.18.mho)
mag.18.dB.norm = group_by(mag.only.18.mho, Zc)
mag.18.dB.norm = mutate_all(mag.18.dB.norm, funs(. * Zc)) # this is Ynorm
is.grouped_df(mag.18.dB.norm)
mag.18.dB.norm = ungroup(mag.18.dB.norm)
mag.18.dB.norm = dplyr::select(mag.18.dB.norm, -Zc)
mag.18.dB.norm = 10 * log10(mag.18.dB.norm)

# do the median
mag.0.dB = summarise_all(mag.0.dB, funs(median))
mag.0.dB.norm = summarise_all(mag.0.dB.norm, funs(median))
mag.0.dB$age = "Neonate"
mag.0.dB.norm$age = "Neonate"

mag.6.dB = summarise_all(mag.6.dB, funs(median))
mag.6.dB.norm = summarise_all(mag.6.dB.norm, funs(median))
mag.6.dB$age = "6 months"
mag.6.dB.norm$age = "6 months"

mag.12.dB = summarise_all(mag.12.dB, funs(median))
mag.12.dB.norm = summarise_all(mag.12.dB.norm, funs(median))
mag.12.dB$age = "12 months"
mag.12.dB.norm$age = "12 months"

mag.18.dB = summarise_all(mag.18.dB, funs(median))
mag.18.dB.norm = summarise_all(mag.18.dB.norm, funs(median))
mag.18.dB$age = "18 months"
mag.18.dB.norm$age = "18 months"

mag.db = rbind.data.frame(mag.0.dB, mag.6.dB, mag.12.dB, mag.18.dB)
mag.db.norm = rbind.data.frame(mag.0.dB.norm, mag.6.dB.norm, mag.12.dB.norm, mag.18.dB.norm)

mag.db.long = gather(mag.db, Frequency, Value, 1:107)
mag.db.long$Frequency = as.numeric(mag.db.long$Frequency)
mag.db.long$age = factor(mag.db.long$age, levels = c("Neonate", "6 months", "12 months", "18 months"))

mag.db.norm.long = gather(mag.db.norm, Frequency, Value, 1:107)
mag.db.norm.long$Frequency = as.numeric(mag.db.norm.long$Frequency)
mag.db.norm.long$age = factor(mag.db.norm.long$age, levels = c("Neonate", "6 months", "12 months", "18 months"))

db.plot <- ggplot(mag.db.long, aes(x=Frequency, y=Value, group=age, colour=age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|,", " dB"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  #scale_y_continuous(expand=c(0, 0), breaks=c(0, 1, 2, 3), limits=c(0, 3)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.98))
#theme(legend.position="none")
#print(db.plot)

db.norm.plot <- ggplot(mag.db.norm.long, aes(x=Frequency, y=Value, group=age, colour=age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|,", " dB"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  #scale_y_continuous(expand=c(0, 0), breaks=c(0, 1, 2, 3), limits=c(0, 3)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = c(0.02, 0.98))
#theme(legend.position="none")
#print(db.norm.plot) 

####### G
g.0.only.mho = cbind.data.frame(G.0.mho, Zc.0)
g.0.only.mho = group_by(g.0.only.mho, Zc)
g.0.norm = mutate_all(g.0.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(g.0.norm)
g.0.norm = ungroup(g.0.norm)
g.0.norm = dplyr::select(g.0.norm, -Zc)
g.0.norm$Age = "Neonate"

g.6.only.mho = cbind.data.frame(G.6.mho, Zc.6)
g.6.only.mho = group_by(g.6.only.mho, Zc)
g.6.norm = mutate_all(g.6.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(g.6.norm)
g.6.norm = ungroup(g.6.norm)
g.6.norm = dplyr::select(g.6.norm, -Zc)
g.6.norm$Age = "6 months"

g.12.only.mho = cbind.data.frame(G.12.mho, Zc.12)
g.12.only.mho = group_by(g.12.only.mho, Zc)
g.12.norm = mutate_all(g.12.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(g.12.norm)
g.12.norm = ungroup(g.12.norm)
g.12.norm = dplyr::select(g.12.norm, -Zc)
g.12.norm$Age = "12 months"

g.18.only.mho = cbind.data.frame(G.18.mho, Zc.18)
g.18.only.mho = group_by(g.18.only.mho, Zc)
g.18.norm = mutate_all(g.18.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(g.18.norm)
g.18.norm = ungroup(g.18.norm)
g.18.norm = dplyr::select(g.18.norm, -Zc)
g.18.norm$Age = "18 months"

g.norm = rbind.data.frame(g.0.norm, g.6.norm, g.12.norm, g.18.norm)
g.norm$Age = factor(g.norm$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))
# now make long form
g.norm <- group_by(g.norm, Age)
g.norm.median <- summarise_all(g.norm, funs(median))
g.norm.median.long <- gather(g.norm.median, Frequency, G, 2:108)
g.norm.median.long$Frequency = as.numeric(g.norm.median.long$Frequency)

g.norm.plot <- ggplot(g.norm.median.long, aes(x=Frequency, y=G, group=Age, colour=Age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("G")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5), limits=c(-0.05, 1.55)) +
  theme_bw() +
  scale_colour_discrete(drop=TRUE, limits = levels(g.norm.median.long$Age), breaks = levels(g.norm.median.long$Age)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
  #theme(legend.position = "none")
#print(g.norm.plot)

############ B
b.0.only.mho = cbind.data.frame(B.0.mho, Zc.0)
b.0.only.mho = group_by(b.0.only.mho, Zc)
b.0.norm = mutate_all(b.0.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(b.0.norm)
b.0.norm = ungroup(b.0.norm)
b.0.norm = dplyr::select(b.0.norm, -Zc)
b.0.norm$Age = "Neonate"

b.6.only.mho = cbind.data.frame(B.6.mho, Zc.6)
b.6.only.mho = group_by(b.6.only.mho, Zc)
b.6.norm = mutate_all(b.6.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(b.6.norm)
b.6.norm = ungroup(b.6.norm)
b.6.norm = dplyr::select(b.6.norm, -Zc)
b.6.norm$Age = "6 months"

b.12.only.mho = cbind.data.frame(B.12.mho, Zc.12)
b.12.only.mho = group_by(b.12.only.mho, Zc)
b.12.norm = mutate_all(b.12.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(b.12.norm)
b.12.norm = ungroup(b.12.norm)
b.12.norm = dplyr::select(b.12.norm, -Zc)
b.12.norm$Age = "12 months"

b.18.only.mho = cbind.data.frame(B.18.mho, Zc.18)
b.18.only.mho = group_by(b.18.only.mho, Zc)
b.18.norm = mutate_all(b.18.only.mho, funs(. * Zc)) # this is Ynorm
is.grouped_df(b.18.norm)
b.18.norm = ungroup(b.18.norm)
b.18.norm = dplyr::select(b.18.norm, -Zc)
b.18.norm$Age = "18 months"

b.norm = rbind.data.frame(b.0.norm, b.6.norm, b.12.norm, b.18.norm)
b.norm$Age = factor(b.norm$Age, levels = c("Neonate", "6 months", "12 months", "18 months"))
# now make long form
b.norm <- group_by(b.norm, Age)
b.norm.median <- summarise_all(b.norm, funs(median))
b.norm.median.long <- gather(b.norm.median, Frequency, B, 2:108)
b.norm.median.long$Frequency = as.numeric(b.norm.median.long$Frequency)

b.norm.plot <- ggplot(b.norm.median.long, aes(x=Frequency, y=B, group=Age, colour=Age)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("B")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-0.5, 0, 1, 0.5, 1, 1.5), limits=c(-0.5, 1.5)) +
  theme_bw() +
  scale_colour_discrete(drop=TRUE, limits = levels(b.norm.median.long$Age), breaks = levels(b.norm.median.long$Age)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = "none")
#print(b.norm.plot)

gb.norm.plots <- plot_grid(mag.norm.plot, g.norm.plot, b.norm.plot, nrow=3, ncol=1, align = "v", 
                      labels = c("A", "B", "C")) 
ggsave("gb.norm.plots.jpeg", gb.norm.plots, height=9, width=6, dpi=500)

################### Plot each age group in terms of normalized G and B 
b.norm.median.long2 = b.norm.median.long
g.norm.median.long2 = g.norm.median.long
names(b.norm.median.long2) = c("Age", "Frequency", "Ynorm")
names(g.norm.median.long2) = c("Age", "Frequency", "Ynorm")
b.norm.median.long2$Y.measure = "Normalized susceptance"
g.norm.median.long2$Y.measure = "Normalized conductance"
y.norm.median.long = rbind.data.frame(b.norm.median.long2, g.norm.median.long2)

rect.plot.norm <- ggplot(y.norm.median.long, aes(x=Frequency, y=Ynorm, group=Y.measure, colour=Y.measure)) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("Y")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-1, 0, 1, 2), limits=c(-1, 2)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.02, 0.98)) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(rect.plot.norm)

ggsave("rect.plot.norm.jpeg", rect.plot.norm, height=6, width=10, dpi=500)

############################################## Modelling (1/2 OCTAVE) ##########################################################################
# 1/2 octave for modeling

# remove tymp data (all ages) and dems for 6-18 mths
newborn.2 = newborn.2[,-c(4:7)]
six.2 = six.2[,-c(3:16)]
twelve.2 = twelve.2[,-c(3:16)]
eighteen.2 = eighteen.2[,-c(3:16)]

names(newborn.2) = c("aabr0", "tymp0", "dpoae0", "gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", 
                      "maternal.ethnicity", "age.0.hrs", "id.res", "ear", "abs0.250",  "abs0.354",  "abs0.500",  "abs0.707",  "abs0.1000", "abs0.1414", 
                      "abs0.2000", "abs0.2828", "abs0.4000", "abs0.5657", "abs0.8000", "mag0.250", "mag0.354", "mag0.500",  "mag0.707",  "mag0.1000", 
                     "mag0.1414", "mag0.2000", "mag0.2828", "mag0.4000", "mag0.5657", "mag0.8000", "pha0.250",  "pha0.354",  "pha0.500",  "pha0.707",  
                     "pha0.1000", "pha0.1414", "pha0.2000", "pha0.2828", "pha0.4000", "pha0.5657", "pha0.8000")

names(six.2) = c("tymp6", "dpoae6", "abs6.250",  "abs6.354",  "abs6.500",  "abs6.707",  "abs6.1000", "abs6.1414", "abs6.2000", "abs6.2828", "abs6.4000", 
                 "abs6.5657", "abs6.8000", "mag6.250", "mag6.354", "mag6.500",  "mag6.707",  "mag6.1000", "mag6.1414", "mag6.2000", "mag6.2828", 
                 "mag6.4000", "mag6.5657", "mag6.8000", "pha6.250",  "pha6.354",  "pha6.500",  "pha6.707",  "pha6.1000", "pha6.1414", "pha6.2000", 
                 "pha6.2828", "pha6.4000", "pha6.5657", "pha6.8000")

names(twelve.2) = c("tymp12", "dpoae12", "abs12.250",  "abs12.354",  "abs12.500",  "abs12.707",  "abs12.1000", "abs12.1414", "abs12.2000", "abs12.2828", 
                    "abs12.4000", "abs12.5657", "abs12.8000", "mag12.250", "mag12.354", "mag12.500",  "mag12.707",  "mag12.1000", "mag12.1414", 
                    "mag12.2000", "mag12.2828", "mag12.4000", "mag12.5657", "mag12.8000", "pha12.250",  "pha12.354",  "pha12.500",  "pha12.707",  
                    "pha12.1000", "pha12.1414", "pha12.2000", "pha12.2828", "pha12.4000", "pha12.5657", "pha12.8000")

names(eighteen.2) = c("tymp18", "dpoae18", "abs18.250",  "abs18.354",  "abs18.500",  "abs18.707",  "abs18.1000", "abs18.1414", "abs18.2000", "abs18.2828",
                      "abs18.4000", "abs18.5657", "abs18.8000", "mag18.250", "mag18.354", "mag18.500",  "mag18.707",  "mag18.1000", "mag18.1414", 
                      "mag18.2000", "mag18.2828", "mag18.4000", "mag18.5657", "mag18.8000", "pha18.250",  "pha18.354",  "pha18.500",  "pha18.707",  
                      "pha18.1000", "pha18.1414", "pha18.2000", "pha18.2828", "pha18.4000", "pha18.5657", "pha18.8000")
wai.2 = cbind.data.frame(newborn.2, six.2, twelve.2, eighteen.2)

# create a reference standard column for each, then omit the tymp and dpoae results (and aabr)
wai.2$rs0 = as.factor(with(wai.2, ifelse(aabr0=="pass" & tymp0=="pass" & dpoae0=="pass", "pass", NA)))
wai.2$rs6 = as.factor(with(wai.2, ifelse(tymp6=="pass" & dpoae6=="pass", "pass", NA)))
wai.2$rs12 = as.factor(with(wai.2, ifelse(tymp12=="pass" & dpoae12=="pass", "pass", NA)))
wai.2$rs18 = as.factor(with(wai.2, ifelse(tymp18=="pass" & dpoae18=="pass", "pass", NA)))

wai.2 = dplyr::select(wai.2, -c(aabr0, tymp0, dpoae0, tymp6, dpoae6, tymp18, dpoae18))

# infants who attended at least one follow up
wai.2.full = filter(wai.2, rs0=="pass" & rs6=="pass" | rs12=="pass" | rs18=="pass")

#give each ear its own id
wai.2.full$ear.id = 1:411

# check for NA
wai.2.na = apply(wai.2.full, 2, function(x) any(is.na(x)))
# ethnicity has some NA
summary(wai.2.full$maternal.ethnicity) # impute this missing later - for modeling

# subset wai for each age group
wai.2.full.0 = dplyr::select(wai.2.full, gender:pha0.8000, ear.id, rs0)
wai.2.full.6 = dplyr::select(wai.2.full, c(gender:ear, abs6.250:pha6.8000, ear.id, rs6))
wai.2.full.12 = dplyr::select(wai.2.full, c(gender:ear, abs12.250:pha12.8000, ear.id, rs12))
wai.2.full.18 = dplyr::select(wai.2.full, c(gender:ear, abs18.250:pha18.8000, ear.id, rs18))

# only want observations with complete wai results (there may be some with "pass" rs but cnt wai)
# also remove the worst outliers (abs < -0.5 at freqs <8000 Hz)
wai.2.full.0 = wai.2.full.0[complete.cases(wai.2.full.0[,-c(1:10)]), ]
wai.2.full.0 = wai.2.full.0[!(wai.2.full.0$id.res==661 & wai.2.full.0$ear=="L"),]
wai.2.full.0 = wai.2.full.0[!(wai.2.full.0$id.res==165 & wai.2.full.0$ear=="R"),]
wai.2.full.6 = wai.2.full.6[complete.cases(wai.2.full.6[,-c(1:10)]), ]
wai.2.full.12 = wai.2.full.12[complete.cases(wai.2.full.12[,-c(1:10)]), ]
wai.2.full.12 = wai.2.full.12[!(wai.2.full.12$id.res==13 & wai.2.full.12$ear=="R"),]
wai.2.full.12 = wai.2.full.12[!(wai.2.full.12$id.res==30 & wai.2.full.12$ear=="L"),]
wai.2.full.18 = wai.2.full.18[complete.cases(wai.2.full.18[,-c(1:10)]), ]
wai.2.full.18 = wai.2.full.18[!(wai.2.full.18$id.res==69 & wai.2.full.18$ear=="R"),]
wai.2.full.18 = wai.2.full.18[!(wai.2.full.18$id.res==86 & wai.2.full.18$ear=="R"),]

# extract each wai for each age then join all ages together with rbind for each wai for plotting
dem.0 = dplyr::select(wai.2.full.0, 1:10)
dem.6 = dplyr::select(wai.2.full.6, 1:10)
dem.12 = dplyr::select(wai.2.full.12, 1:10)
dem.18 = dplyr::select(wai.2.full.18, 1:10)

abs.2.full.0 = dplyr::select(wai.2.full.0, 1:10, abs0.250:abs0.8000, ear.id, rs0)
mag.2.full.0 = dplyr::select(wai.2.full.0, 1:10, mag0.250:mag0.8000, ear.id, rs0)
pha.2.full.0 = dplyr::select(wai.2.full.0, 1:10, pha0.250:pha0.8000, ear.id, rs0)

names(abs.2.full.0)[names(abs.2.full.0) == "rs0"] <- "rs"
names(mag.2.full.0)[names(mag.2.full.0) == "rs0"] <- "rs"
names(pha.2.full.0)[names(pha.2.full.0) == "rs0"] <- "rs"

# keep only pass
abs.2.full.0 = filter(abs.2.full.0, rs=="pass")
mag.2.full.0 = filter(mag.2.full.0, rs=="pass")
pha.2.full.0 = filter(pha.2.full.0, rs=="pass")

# add an age variable
abs.2.full.0$age.group = "Neonate"
mag.2.full.0$age.group = "Neonate"
pha.2.full.0$age.group = "Neonate"

abs.2.full.6 = dplyr::select(wai.2.full.6, 1:10, abs6.250:abs6.8000, ear.id, rs6)
mag.2.full.6 = dplyr::select(wai.2.full.6, 1:10, mag6.250:mag6.8000, ear.id, rs6)
pha.2.full.6 = dplyr::select(wai.2.full.6, 1:10, pha6.250:pha6.8000, ear.id, rs6)

names(abs.2.full.6)[names(abs.2.full.6) == "rs6"] <- "rs"
names(mag.2.full.6)[names(mag.2.full.6) == "rs6"] <- "rs"
names(pha.2.full.6)[names(pha.2.full.6) == "rs6"] <- "rs"

# keep only pass
abs.2.full.6 = filter(abs.2.full.6, rs=="pass")
mag.2.full.6 = filter(mag.2.full.6, rs=="pass")
pha.2.full.6 = filter(pha.2.full.6, rs=="pass")

abs.2.full.6$age.group = "6 months"
mag.2.full.6$age.group = "6 months"
pha.2.full.6$age.group = "6 months"

abs.2.full.12 = dplyr::select(wai.2.full.12, 1:10, abs12.250:abs12.8000, ear.id, rs12)
mag.2.full.12 = dplyr::select(wai.2.full.12, 1:10, mag12.250:mag12.8000, ear.id, rs12)
pha.2.full.12 = dplyr::select(wai.2.full.12, 1:10, pha12.250:pha12.8000, ear.id, rs12)

names(abs.2.full.12)[names(abs.2.full.12) == "rs12"] <- "rs"
names(mag.2.full.12)[names(mag.2.full.12) == "rs12"] <- "rs"
names(pha.2.full.12)[names(pha.2.full.12) == "rs12"] <- "rs"

# keep only pass
abs.2.full.12 = filter(abs.2.full.12, rs=="pass")
mag.2.full.12 = filter(mag.2.full.12, rs=="pass")
pha.2.full.12 = filter(pha.2.full.12, rs=="pass")

abs.2.full.12$age.group = "12 months"
mag.2.full.12$age.group = "12 months"
pha.2.full.12$age.group = "12 months"

abs.2.full.18 = dplyr::select(wai.2.full.18, 1:10, abs18.250:abs18.8000, ear.id, rs18)
mag.2.full.18 = dplyr::select(wai.2.full.18, 1:10, mag18.250:mag18.8000, ear.id, rs18)
pha.2.full.18 = dplyr::select(wai.2.full.18, 1:10, pha18.250:pha18.8000, ear.id, rs18)

names(abs.2.full.18)[names(abs.2.full.18) == "rs18"] <- "rs"
names(mag.2.full.18)[names(mag.2.full.18) == "rs18"] <- "rs"
names(pha.2.full.18)[names(pha.2.full.18) == "rs18"] <- "rs"

# keep only pass - this is unnecessary
abs.2.full.18 = filter(abs.2.full.18, rs=="pass")
mag.2.full.18 = filter(mag.2.full.18, rs=="pass")
pha.2.full.18 = filter(pha.2.full.18, rs=="pass")

abs.2.full.18$age.group = "18 months"
mag.2.full.18$age.group = "18 months"
pha.2.full.18$age.group = "18 months"

num.dem.freq.names <- c("gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", "maternal.ethnicity", "age.0.hrs", "id.res", "ear", 
                        "250",  "354",  "500",  "707",  "1000", "1414", "2000", "2828", "4000", "5657", "8000", "ear.id", "rs", "age.group")

colnames(abs.2.full.0) = num.dem.freq.names
colnames(abs.2.full.6) = num.dem.freq.names
colnames(abs.2.full.12) = num.dem.freq.names
colnames(abs.2.full.18) = num.dem.freq.names

colnames(mag.2.full.0) = num.dem.freq.names
colnames(mag.2.full.6) = num.dem.freq.names
colnames(mag.2.full.12) = num.dem.freq.names
colnames(mag.2.full.18) = num.dem.freq.names

colnames(pha.2.full.0) = num.dem.freq.names
colnames(pha.2.full.6) = num.dem.freq.names
colnames(pha.2.full.12) = num.dem.freq.names
colnames(pha.2.full.18) = num.dem.freq.names

# get the demographics
# first of all subject specific information
subs.0 = str(as.factor(abs.2.full.0$id.res)) # 201 subjects at birth
unique.0 = abs.2.full.0[!duplicated(abs.2.full.0$id.res),]
sex.0 = summary(unique.0$gender) # 97 F; 104 M
sex.0
# there was one GA = 55! set to NA
unique.0$ga[unique.0$ga == 55] = NA
ga = summary(unique.0$ga) # 34 to 41.6 (median = 39, IQR 38.2 to 40.1) 
ga
birth.weight = summary(unique.0$birth.weight) # 2390 to 5120, median 3478, 3181 to 3810 - is 5120 feasible?
birth.weight
birth.type = summary(unique.0$type.of.birth) # 123 VB; 95 CS
birth.type
summary(unique.0$maternal.ethnicity) # 173 caucasian; 22 asian; 17 ATSI; 3 oceanian; 1 african; 1 sth american; 1 unknown
# then ear specific

# head circum - 48 is wrong - too big
unique.0$head.circum[unique.0$head.circum == 48.0] = NA
summary(unique.0$head.circum)

unique.0$body.length = as.numeric(unique.0$body.length)
summary(unique.0$body.length)

ears.0 = str(as.factor(abs.2.full.0$ear.id)) # 361 ears at birth

subs.6 = str(as.factor(abs.2.full.6$id.res)) # 172 subjects at 6mth
unique.6 = abs.2.full.6[!duplicated(abs.2.full.6$id.res),]
sex.6 = summary(unique.6$gender) # 90 F; 82 M
sex.6
summary(unique.6$maternal.ethnicity) # 138 caucasian; 19 asian; 12 ATSI; 2 oceanian; 1 sth american
# then ear specific
ears.6 = str(as.factor(abs.2.full.6$ear.id)) # 386 ears at 6 mths

subs.12 = str(as.factor(abs.2.full.12$id.res)) # 119 subjects at 12mth
unique.12 = abs.2.full.12[!duplicated(abs.2.full.12$id.res),]
sex.12 = summary(unique.12$gender) # 50 F; 69 M
sex.12
summary(unique.12$maternal.ethnicity) # 97 caucasian; 8 asian; 7 ATSI; 3 oceanian; 2 sth american, 1 african, 1 unknown
# then ear specific
ears.12 = str(as.factor(abs.2.full.12$ear.id)) # 207 ears at 12 mths

subs.18 = str(as.factor(abs.2.full.18$id.res)) # 87 subjects at 18mth
unique.18 = abs.2.full.18[!duplicated(abs.2.full.18$id.res),]
sex.18 = summary(unique.18$gender) # 39 F; 48 M
sex.18
summary(unique.18$maternal.ethnicity) # 64 caucasian; 13 asian; 6 ATSI; 3 oceanian; 1 sth american
# then ear specific
ears.18 = str(as.factor(abs.2.full.18$ear.id)) # 151 ears at 18 mths

# I didn't save the ages for 6, 12 and 18 mths in the demographics had to go back and save a new file (ages.rda)
## need to match id.res with the unique.6, unique.12 etc
age.6 = ages[,c(1:2)]
age.12 = ages[,c(1,3)]
age.18 = ages[,c(1,4)]

age.6 = merge(age.6, unique.6, by="id.res")
age.12 = merge(age.12, unique.12, by="id.res")
age.18 = merge(age.18, unique.18, by="id.res")

age.6$age.6.mth.in.weeks[age.6$age.6.mth.in.weeks == 0] = NA
summary(age.6$age.6.mth.in.weeks)
age.12$age.12.mth.in.weeks[age.12$age.12.mth.in.weeks == 70.00] = NA
summary(age.12$age.12.mth.in.weeks)
age.18$age.18.mth.in.weeks[age.18$age.18.mth.in.weeks < 35] = NA
summary(age.18$age.18.mth.in.weeks)

# now put abs, mag, pha together for each age group
abs.2.full = rbind.data.frame(abs.2.full.0, abs.2.full.6, abs.2.full.12, abs.2.full.18)
mag.2.full = rbind.data.frame(mag.2.full.0, mag.2.full.6, mag.2.full.12, mag.2.full.18)
pha.2.full = rbind.data.frame(pha.2.full.0, pha.2.full.6, pha.2.full.12, pha.2.full.18)

abs.2.full$age.group = as.factor(abs.2.full$age.group)
mag.2.full$age.group = as.factor(mag.2.full$age.group)
pha.2.full$age.group = as.factor(pha.2.full$age.group)

abs.2.full$age.group = factor(abs.2.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
mag.2.full$age.group = factor(mag.2.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))
pha.2.full$age.group = factor(pha.2.full$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

abs.2.full$body.length = as.numeric(abs.2.full$body.length)
mag.2.full$body.length = as.numeric(mag.2.full$body.length)
pha.2.full$body.length = as.numeric(pha.2.full$body.length)

abs.2.full$ear = as.factor(abs.2.full$ear)
mag.2.full$ear = as.factor(mag.2.full$ear)
pha.2.full$ear = as.factor(pha.2.full$ear)

# make them long for modeling
abs.2.full <- group_by(abs.2.full, age.group)
abs.2.long <- gather(abs.2.full, Frequency, Absorbance, 11:21)
abs.2.long$Frequency = as.numeric(abs.2.long$Frequency)

mag.2.full <- group_by(mag.2.full, age.group)
mag.2.long <- gather(mag.2.full, Frequency, Magnitude, 11:21)
mag.2.long$Frequency = as.numeric(mag.2.long$Frequency)

pha.2.full <- group_by(pha.2.full, age.group)
pha.2.long <- gather(pha.2.full, Frequency, Phase, 11:21)
pha.2.long$Frequency = as.numeric(pha.2.long$Frequency)

# make freq a factor
abs.2.long$Frequency = as.factor(as.character(abs.2.long$Frequency))
abs.2.long$Frequency = factor(abs.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

mag.2.long$Frequency = as.factor(as.character(mag.2.long$Frequency))
mag.2.long$Frequency = factor(mag.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

pha.2.long$Frequency = as.factor(as.character(pha.2.long$Frequency))
pha.2.long$Frequency = factor(pha.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

str(abs.2.long)
str(mag.2.long)
str(pha.2.long)
abs.2.long$id.res = as.factor(abs.2.long$id.res)
abs.2.long$ear.id = as.factor(abs.2.long$ear.id)
mag.2.long$id.res = as.factor(mag.2.long$id.res)
mag.2.long$ear.id = as.factor(mag.2.long$ear.id)
pha.2.long$id.res = as.factor(pha.2.long$id.res)
pha.2.long$ear.id = as.factor(pha.2.long$ear.id)

# Asian = other for modeling
abs.2.long$maternal.ethnicity[abs.2.long$maternal.ethnicity == "African"] = "Asian"
abs.2.long$maternal.ethnicity[abs.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
abs.2.long$maternal.ethnicity[abs.2.long$maternal.ethnicity == "South American"] = "Asian"
# set NA to Other (Asian)
abs.2.long$maternal.ethnicity[is.na(abs.2.long$maternal.ethnicity)] = "Asian"
abs.2.long$maternal.ethnicity = droplevels(abs.2.long$maternal.ethnicity)

mag.2.long$maternal.ethnicity[mag.2.long$maternal.ethnicity == "African"] = "Asian"
mag.2.long$maternal.ethnicity[mag.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
mag.2.long$maternal.ethnicity[mag.2.long$maternal.ethnicity == "South American"] = "Asian"
mag.2.long$maternal.ethnicity[is.na(mag.2.long$maternal.ethnicity)] = "Asian"
mag.2.long$maternal.ethnicity = droplevels(mag.2.long$maternal.ethnicity)

pha.2.long$maternal.ethnicity[pha.2.long$maternal.ethnicity == "African"] = "Asian"
pha.2.long$maternal.ethnicity[pha.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
pha.2.long$maternal.ethnicity[pha.2.long$maternal.ethnicity == "South American"] = "Asian"
pha.2.long$maternal.ethnicity[is.na(pha.2.long$maternal.ethnicity)] = "Asian"
pha.2.long$maternal.ethnicity = droplevels(pha.2.long$maternal.ethnicity)

## make a rectangular version of Y
mag.only = mag.2.long$Magnitude
pha.only = pha.2.long$Phase

pha.only.rad = pha.only * pi / 180
g = mag.only * cos(pha.only.rad)
b = mag.only * sin(pha.only.rad)

g.2.long = dplyr::select(mag.2.long, -Magnitude)
g.2.long$G = g

b.2.long = dplyr::select(pha.2.long, -Phase)
b.2.long$B = b

## normalized B and G
# need to add demographics to G and B 
# and 1/2 octave average
g.names = c("g.226",            
            "g.257.33", "g.280.62", "g.297.3", "g.324.21", "g.343.49", "g.363.91", "g.385.55",         
            "g.408.48", "g.432.77", "g.458.5", "g.471.94", "g.500", "g.514.65", "g.545.25",        
            "g.561.23", "g.577.68", "g.594.6", "g.629.96", "g.648.42", "g.667.42", "g.686.98",         
            "g.707.11", "g.727.83", "g.749.15", "g.771.11", "g.793.7", "g.816.96", "g.840.9",          
            "g.865.54", "g.890.9", "g.917", "g.943.87", "g.971.53", "g.1000", "g.1029.3",         
            "g.1059.46", "g.1090.51", "g.1122.46", "g.1155.35", "g.1189.21", "g.1224.05", "g.1259.92",        
            "g.1296.84", "g.1334.84", "g.1373.95", "g.1414.21", "g.1455.65", "g.1498.31", "g.1542.21",       
            "g.1587.4", "g.1633.92", "g.1681.79", "g.1731.07", "g.1781.8", "g.1834.01", "g.1887.75",     
            "g.1943.06", "g.2000", "g.2058.6", "g.2118.93", "g.2181.02", "g.2244.92", "g.2310.71",        
            "g.2378.41", "g.2448.11", "g.2519.84", "g.2593.68", "g.2669.68", "g.2747.91", "g.2828.43",        
            "g.2911.31", "g.2996.61", "g.3084.42", "g.3174.8", "g.3267.83", "g.3363.59", "g.3462.15",        
            "g.3563.59", "g.3668.02", "g.3775.5", "g.3886.13", "g.4000", "g.4117.21", "g.4237.85",        
            "g.4362.03", "g.4489.85", "g.4621.41", "g.4756.83", "g.4896.21", "g.5039.68", "g.5187.36",        
            "g.5339.36", "g.5495.81", "g.5656.85", "g.5822.61", "g.5993.23", "g.6168.84", "g.6349.6",         
            "g.6535.66", "g.6727.17", "g.6924.29", "g.7127.19", "g.7336.03", "g.7550.99", "g.7772.26",        
            "g.8000")

b.names = c("b.226",            
            "b.257.33", "b.280.62", "b.297.3", "b.324.21", "b.343.49", "b.363.91", "b.385.55",         
            "b.408.48", "b.432.77", "b.458.5", "b.471.94", "b.500", "b.514.65", "b.545.25",        
            "b.561.23", "b.577.68", "b.594.6", "b.629.96", "b.648.42", "b.667.42", "b.686.98",         
            "b.707.11", "b.727.83", "b.749.15", "b.771.11", "b.793.7", "b.816.96", "b.840.9",          
            "b.865.54", "b.890.9", "b.917", "b.943.87", "b.971.53", "b.1000", "b.1029.3",         
            "b.1059.46", "b.1090.51", "b.1122.46", "b.1155.35", "b.1189.21", "b.1224.05", "b.1259.92",        
            "b.1296.84", "b.1334.84", "b.1373.95", "b.1414.21", "b.1455.65", "b.1498.31", "b.1542.21",       
            "b.1587.4", "b.1633.92", "b.1681.79", "b.1731.07", "b.1781.8", "b.1834.01", "b.1887.75",     
            "b.1943.06", "b.2000", "b.2058.6", "b.2118.93", "b.2181.02", "b.2244.92", "b.2310.71",        
            "b.2378.41", "b.2448.11", "b.2519.84", "b.2593.68", "b.2669.68", "b.2747.91", "b.2828.43",        
            "b.2911.31", "b.2996.61", "b.3084.42", "b.3174.8", "b.3267.83", "b.3363.59", "b.3462.15",        
            "b.3563.59", "b.3668.02", "b.3775.5", "b.3886.13", "b.4000", "b.4117.21", "b.4237.85",        
            "b.4362.03", "b.4489.85", "b.4621.41", "b.4756.83", "b.4896.21", "b.5039.68", "b.5187.36",        
            "b.5339.36", "b.5495.81", "b.5656.85", "b.5822.61", "b.5993.23", "b.6168.84", "b.6349.6",         
            "b.6535.66", "b.6727.17", "b.6924.29", "b.7127.19", "b.7336.03", "b.7550.99", "b.7772.26",        
            "b.8000")

mag.names = c("mag.226",            
            "mag.257.33", "mag.280.62", "mag.297.3", "mag.324.21", "mag.343.49", "mag.363.91", "mag.385.55",         
            "mag.408.48", "mag.432.77", "mag.458.5", "mag.471.94", "mag.500", "mag.514.65", "mag.545.25",        
            "mag.561.23", "mag.577.68", "mag.594.6", "mag.629.96", "mag.648.42", "mag.667.42", "mag.686.98",         
            "mag.707.11", "mag.727.83", "mag.749.15", "mag.771.11", "mag.793.7", "mag.816.96", "mag.840.9",          
            "mag.865.54", "mag.890.9", "mag.917", "mag.943.87", "mag.971.53", "mag.1000", "mag.1029.3",         
            "mag.1059.46", "mag.1090.51", "mag.1122.46", "mag.1155.35", "mag.1189.21", "mag.1224.05", "mag.1259.92",        
            "mag.1296.84", "mag.1334.84", "mag.1373.95", "mag.1414.21", "mag.1455.65", "mag.1498.31", "mag.1542.21",       
            "mag.1587.4", "mag.1633.92", "mag.1681.79", "mag.1731.07", "mag.1781.8", "mag.1834.01", "mag.1887.75",     
            "mag.1943.06", "mag.2000", "mag.2058.6", "mag.2118.93", "mag.2181.02", "mag.2244.92", "mag.2310.71",        
            "mag.2378.41", "mag.2448.11", "mag.2519.84", "mag.2593.68", "mag.2669.68", "mag.2747.91", "mag.2828.43",        
            "mag.2911.31", "mag.2996.61", "mag.3084.42", "mag.3174.8", "mag.3267.83", "mag.3363.59", "mag.3462.15",        
            "mag.3563.59", "mag.3668.02", "mag.3775.5", "mag.3886.13", "mag.4000", "mag.4117.21", "mag.4237.85",        
            "mag.4362.03", "mag.4489.85", "mag.4621.41", "mag.4756.83", "mag.4896.21", "mag.5039.68", "mag.5187.36",        
            "mag.5339.36", "mag.5495.81", "mag.5656.85", "mag.5822.61", "mag.5993.23", "mag.6168.84", "mag.6349.6",         
            "mag.6535.66", "mag.6727.17", "mag.6924.29", "mag.7127.19", "mag.7336.03", "mag.7550.99", "mag.7772.26",        
            "mag.8000")

## normalized G
g.0.norm = dplyr::select(g.0.norm, -Age)
names(g.0.norm) = c(g.names)
g.250 <- transmute(g.0.norm, g.250 = (g.226 +	g.257.33 +	g.280.62 +	g.297.3)/4) # 226 - 297.30
g.354 <- transmute(g.0.norm, g.354 = (g.324.21 +	g.343.49 +	g.363.91 +	g.385.55 +	g.408.48)/5) # 297.31 - 420.45
g.500 <- transmute(g.0.norm, g.500 = (g.432.77 +	g.458.5 +	g.471.94 +	g.500 +	g.514.65 +	g.545.25 +	g.561.23 +	
                                     g.577.68 +	g.594.6)/9) # 420.46 - 594.60
g.707 <- transmute(g.0.norm, g.707 =	(g.629.96 +	g.648.42 +	g.667.42 +	g.686.98 +	g.707.11 +	g.727.83 +	g.749.15 +	
                                     g.771.11 +	g.793.7 +	g.816.96 +	g.840.9)/11) # 594.61 - 840.90
g.1000 <- transmute(g.0.norm, g.1000 = (g.865.54 +	g.890.9 +	g.917 +	g.943.87 +	g.971.53 +	g.1000 +	g.1029.3 +	
                                       g.1059.46 +	g.1090.51 +	g.1122.46 +	g.1155.35 +	g.1189.21)/12) # 840.91 - 1189.21
g.1414 <- transmute(g.0.norm, g.1414 = (g.1224.05 +	g.1259.92 +	g.1296.84 +	g.1334.84 +	g.1373.95 +	g.1414.21 +	
                                       g.1455.65 +	g.1498.31 +	g.1542.21 +	g.1587.4 +	g.1633.92 +	g.1681.79)/12) # 1189.22 - 1681.79
g.2000 <- transmute(g.0.norm, g.2000 = (g.1731.07 +	g.1781.8 +	g.1834.01 +	g.1887.75 +	g.1943.06 +	g.2000 +	
                                       g.2058.6 +	g.2118.93 +	g.2181.02 +	g.2244.92 +	g.2310.71 +	g.2378.41)/12) # 1681.80 - 2378.41
g.2828 <- transmute(g.0.norm, g.2828 = (g.2448.11 +	g.2519.84 +	g.2593.68 +	g.2669.68 +	g.2747.91 +	g.2828.43 +	
                                       g.2911.31 +	g.2996.61 +	g.3084.42 +	g.3174.8 +	g.3267.83 +	g.3363.59)/12) # 2378.42 - 3363.59
g.4000 <- transmute(g.0.norm, g.4000 = (g.3462.15 +	g.3563.59 +	g.3668.02 +	g.3775.5 +	g.3886.13 +	g.4000 +	
                                       g.4117.21 +	g.4237.85 +	g.4362.03 +	g.4489.85 +	g.4621.41 +	g.4756.83)/12) # 3363.60 - 4756.83
g.5657 <- transmute(g.0.norm, g.5657 = (g.4896.21 +	g.5039.68 +	g.5187.36 +	g.5339.36 +	g.5495.81 +	g.5656.85 +	
                                       g.5822.61 +	g.5993.23 +	g.6168.84 +	g.6349.6 +	g.6535.66 +	g.6727.17)/12) # 4756.84 - 6727.17
g.8000 <- transmute(g.0.norm, g.8000 = (g.6924.29 +	g.7127.19 +	g.7336.03 +	g.7550.99 +	g.7772.26 +	g.8000)/6) # 6727.18 - 8000
g.0.norm.2 <- cbind(g.250, g.354, g.500, g.707, g.1000, g.1414, g.2000, g.2828, g.4000, g.5657, g.8000)
g.0.norm.2$age.group = "Neonate"
# add dems
dems.0 = dplyr::select(mag.2.full.0, c(gender:ear, ear.id))
g.0.norm.2 = cbind.data.frame(dems.0, g.0.norm.2)

g.6.norm = dplyr::select(g.6.norm, -Age)
names(g.6.norm) = c(g.names)
g.250 <- transmute(g.6.norm, g.250 = (g.226 +	g.257.33 +	g.280.62 +	g.297.3)/4) # 226 - 297.30
g.354 <- transmute(g.6.norm, g.354 = (g.324.21 +	g.343.49 +	g.363.91 +	g.385.55 +	g.408.48)/5) # 297.31 - 420.45
g.500 <- transmute(g.6.norm, g.500 = (g.432.77 +	g.458.5 +	g.471.94 +	g.500 +	g.514.65 +	g.545.25 +	g.561.23 +	
                                      g.577.68 +	g.594.6)/9) # 420.46 - 594.60
g.707 <- transmute(g.6.norm, g.707 =	(g.629.96 +	g.648.42 +	g.667.42 +	g.686.98 +	g.707.11 +	g.727.83 +	g.749.15 +	
                                      g.771.11 +	g.793.7 +	g.816.96 +	g.840.9)/11) # 594.61 - 840.90
g.1000 <- transmute(g.6.norm, g.1000 = (g.865.54 +	g.890.9 +	g.917 +	g.943.87 +	g.971.53 +	g.1000 +	g.1029.3 +	
                                        g.1059.46 +	g.1090.51 +	g.1122.46 +	g.1155.35 +	g.1189.21)/12) # 840.91 - 1189.21
g.1414 <- transmute(g.6.norm, g.1414 = (g.1224.05 +	g.1259.92 +	g.1296.84 +	g.1334.84 +	g.1373.95 +	g.1414.21 +	
                                        g.1455.65 +	g.1498.31 +	g.1542.21 +	g.1587.4 +	g.1633.92 +	g.1681.79)/12) # 1189.22 - 1681.79
g.2000 <- transmute(g.6.norm, g.2000 = (g.1731.07 +	g.1781.8 +	g.1834.01 +	g.1887.75 +	g.1943.06 +	g.2000 +	
                                        g.2058.6 +	g.2118.93 +	g.2181.02 +	g.2244.92 +	g.2310.71 +	g.2378.41)/12) # 1681.80 - 2378.41
g.2828 <- transmute(g.6.norm, g.2828 = (g.2448.11 +	g.2519.84 +	g.2593.68 +	g.2669.68 +	g.2747.91 +	g.2828.43 +	
                                        g.2911.31 +	g.2996.61 +	g.3084.42 +	g.3174.8 +	g.3267.83 +	g.3363.59)/12) # 2378.42 - 3363.59
g.4000 <- transmute(g.6.norm, g.4000 = (g.3462.15 +	g.3563.59 +	g.3668.02 +	g.3775.5 +	g.3886.13 +	g.4000 +	
                                        g.4117.21 +	g.4237.85 +	g.4362.03 +	g.4489.85 +	g.4621.41 +	g.4756.83)/12) # 3363.60 - 4756.83
g.5657 <- transmute(g.6.norm, g.5657 = (g.4896.21 +	g.5039.68 +	g.5187.36 +	g.5339.36 +	g.5495.81 +	g.5656.85 +	
                                        g.5822.61 +	g.5993.23 +	g.6168.84 +	g.6349.6 +	g.6535.66 +	g.6727.17)/12) # 4756.84 - 6727.17
g.8000 <- transmute(g.6.norm, g.8000 = (g.6924.29 +	g.7127.19 +	g.7336.03 +	g.7550.99 +	g.7772.26 +	g.8000)/6) # 6727.18 - 8000
g.6.norm.2 <- cbind(g.250, g.354, g.500, g.707, g.1000, g.1414, g.2000, g.2828, g.4000, g.5657, g.8000)
g.6.norm.2$age.group = "6 months"
dems.6 = dplyr::select(mag.2.full.6, c(gender:ear, ear.id))
g.6.norm.2 = cbind.data.frame(dems.6, g.6.norm.2)

g.12.norm = dplyr::select(g.12.norm, -Age)
names(g.12.norm) = c(g.names)
g.250 <- transmute(g.12.norm, g.250 = (g.226 +	g.257.33 +	g.280.62 +	g.297.3)/4) # 226 - 297.30
g.354 <- transmute(g.12.norm, g.354 = (g.324.21 +	g.343.49 +	g.363.91 +	g.385.55 +	g.408.48)/5) # 297.31 - 420.45
g.500 <- transmute(g.12.norm, g.500 = (g.432.77 +	g.458.5 +	g.471.94 +	g.500 +	g.514.65 +	g.545.25 +	g.561.23 +	
                                      g.577.68 +	g.594.6)/9) # 420.46 - 594.60
g.707 <- transmute(g.12.norm, g.707 =	(g.629.96 +	g.648.42 +	g.667.42 +	g.686.98 +	g.707.11 +	g.727.83 +	g.749.15 +	
                                      g.771.11 +	g.793.7 +	g.816.96 +	g.840.9)/11) # 594.61 - 840.90
g.1000 <- transmute(g.12.norm, g.1000 = (g.865.54 +	g.890.9 +	g.917 +	g.943.87 +	g.971.53 +	g.1000 +	g.1029.3 +	
                                        g.1059.46 +	g.1090.51 +	g.1122.46 +	g.1155.35 +	g.1189.21)/12) # 840.91 - 1189.21
g.1414 <- transmute(g.12.norm, g.1414 = (g.1224.05 +	g.1259.92 +	g.1296.84 +	g.1334.84 +	g.1373.95 +	g.1414.21 +	
                                        g.1455.65 +	g.1498.31 +	g.1542.21 +	g.1587.4 +	g.1633.92 +	g.1681.79)/12) # 1189.22 - 1681.79
g.2000 <- transmute(g.12.norm, g.2000 = (g.1731.07 +	g.1781.8 +	g.1834.01 +	g.1887.75 +	g.1943.06 +	g.2000 +	
                                        g.2058.6 +	g.2118.93 +	g.2181.02 +	g.2244.92 +	g.2310.71 +	g.2378.41)/12) # 1681.80 - 2378.41
g.2828 <- transmute(g.12.norm, g.2828 = (g.2448.11 +	g.2519.84 +	g.2593.68 +	g.2669.68 +	g.2747.91 +	g.2828.43 +	
                                        g.2911.31 +	g.2996.61 +	g.3084.42 +	g.3174.8 +	g.3267.83 +	g.3363.59)/12) # 2378.42 - 3363.59
g.4000 <- transmute(g.12.norm, g.4000 = (g.3462.15 +	g.3563.59 +	g.3668.02 +	g.3775.5 +	g.3886.13 +	g.4000 +	
                                        g.4117.21 +	g.4237.85 +	g.4362.03 +	g.4489.85 +	g.4621.41 +	g.4756.83)/12) # 3363.60 - 4756.83
g.5657 <- transmute(g.12.norm, g.5657 = (g.4896.21 +	g.5039.68 +	g.5187.36 +	g.5339.36 +	g.5495.81 +	g.5656.85 +	
                                        g.5822.61 +	g.5993.23 +	g.6168.84 +	g.6349.6 +	g.6535.66 +	g.6727.17)/12) # 4756.84 - 6727.17
g.8000 <- transmute(g.12.norm, g.8000 = (g.6924.29 +	g.7127.19 +	g.7336.03 +	g.7550.99 +	g.7772.26 +	g.8000)/6) # 6727.18 - 8000
g.12.norm.2 <- cbind(g.250, g.354, g.500, g.707, g.1000, g.1414, g.2000, g.2828, g.4000, g.5657, g.8000)
g.12.norm.2$age.group = "12 months"
dems.12 = dplyr::select(mag.2.full.12, c(gender:ear, ear.id))
g.12.norm.2 = cbind.data.frame(dems.12, g.12.norm.2)

g.18.norm = dplyr::select(g.18.norm, -Age)
names(g.18.norm) = c(g.names)
g.250 <- transmute(g.18.norm, g.250 = (g.226 +	g.257.33 +	g.280.62 +	g.297.3)/4) # 226 - 297.30
g.354 <- transmute(g.18.norm, g.354 = (g.324.21 +	g.343.49 +	g.363.91 +	g.385.55 +	g.408.48)/5) # 297.31 - 420.45
g.500 <- transmute(g.18.norm, g.500 = (g.432.77 +	g.458.5 +	g.471.94 +	g.500 +	g.514.65 +	g.545.25 +	g.561.23 +	
                                      g.577.68 +	g.594.6)/9) # 420.46 - 594.60
g.707 <- transmute(g.18.norm, g.707 =	(g.629.96 +	g.648.42 +	g.667.42 +	g.686.98 +	g.707.11 +	g.727.83 +	g.749.15 +	
                                      g.771.11 +	g.793.7 +	g.816.96 +	g.840.9)/11) # 594.61 - 840.90
g.1000 <- transmute(g.18.norm, g.1000 = (g.865.54 +	g.890.9 +	g.917 +	g.943.87 +	g.971.53 +	g.1000 +	g.1029.3 +	
                                        g.1059.46 +	g.1090.51 +	g.1122.46 +	g.1155.35 +	g.1189.21)/12) # 840.91 - 1189.21
g.1414 <- transmute(g.18.norm, g.1414 = (g.1224.05 +	g.1259.92 +	g.1296.84 +	g.1334.84 +	g.1373.95 +	g.1414.21 +	
                                        g.1455.65 +	g.1498.31 +	g.1542.21 +	g.1587.4 +	g.1633.92 +	g.1681.79)/12) # 1189.22 - 1681.79
g.2000 <- transmute(g.18.norm, g.2000 = (g.1731.07 +	g.1781.8 +	g.1834.01 +	g.1887.75 +	g.1943.06 +	g.2000 +	
                                        g.2058.6 +	g.2118.93 +	g.2181.02 +	g.2244.92 +	g.2310.71 +	g.2378.41)/12) # 1681.80 - 2378.41
g.2828 <- transmute(g.18.norm, g.2828 = (g.2448.11 +	g.2519.84 +	g.2593.68 +	g.2669.68 +	g.2747.91 +	g.2828.43 +	
                                        g.2911.31 +	g.2996.61 +	g.3084.42 +	g.3174.8 +	g.3267.83 +	g.3363.59)/12) # 2378.42 - 3363.59
g.4000 <- transmute(g.18.norm, g.4000 = (g.3462.15 +	g.3563.59 +	g.3668.02 +	g.3775.5 +	g.3886.13 +	g.4000 +	
                                        g.4117.21 +	g.4237.85 +	g.4362.03 +	g.4489.85 +	g.4621.41 +	g.4756.83)/12) # 3363.60 - 4756.83
g.5657 <- transmute(g.18.norm, g.5657 = (g.4896.21 +	g.5039.68 +	g.5187.36 +	g.5339.36 +	g.5495.81 +	g.5656.85 +	
                                        g.5822.61 +	g.5993.23 +	g.6168.84 +	g.6349.6 +	g.6535.66 +	g.6727.17)/12) # 4756.84 - 6727.17
g.8000 <- transmute(g.18.norm, g.8000 = (g.6924.29 +	g.7127.19 +	g.7336.03 +	g.7550.99 +	g.7772.26 +	g.8000)/6) # 6727.18 - 8000
g.18.norm.2 <- cbind(g.250, g.354, g.500, g.707, g.1000, g.1414, g.2000, g.2828, g.4000, g.5657, g.8000)
g.18.norm.2$age.group = "18 months"
dems.18 = dplyr::select(mag.2.full.18, c(gender:ear, ear.id))
g.18.norm.2 = cbind.data.frame(dems.18, g.18.norm.2)

g.norm.2 = rbind.data.frame(g.0.norm.2, g.6.norm.2, g.12.norm.2, g.18.norm.2)
names(g.norm.2) = c("gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", "maternal.ethnicity", "age.0.hrs", "id.res", "ear",             
                  "ear.id", "250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000", "age.group")
g.norm.2$age.group = factor(g.norm.2$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

g.norm.2 <- group_by(g.norm.2, age.group)
g.norm.2.long <- gather(g.norm.2, Frequency, g.norm, 12:22)
g.norm.2.long$Frequency = as.numeric(g.norm.2.long$Frequency)

g.norm.2.long$Frequency = as.factor(as.character(g.norm.2.long$Frequency))
g.norm.2.long$Frequency = factor(g.norm.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

str(g.norm.2.long)
g.norm.2.long$id.res = as.factor(g.norm.2.long$id.res)
g.norm.2.long$ear.id = as.factor(g.norm.2.long$ear.id)
g.norm.2.long$ear = as.factor(g.norm.2.long$ear)

# Asian = other for modeling
g.norm.2.long$maternal.ethnicity[g.norm.2.long$maternal.ethnicity == "African"] = "Asian"
g.norm.2.long$maternal.ethnicity[g.norm.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
g.norm.2.long$maternal.ethnicity[g.norm.2.long$maternal.ethnicity == "South American"] = "Asian"
g.norm.2.long$maternal.ethnicity[is.na(g.norm.2.long$maternal.ethnicity)] = "Asian"
g.norm.2.long$maternal.ethnicity = droplevels(g.norm.2.long$maternal.ethnicity)

## normalized B
b.0.norm = dplyr::select(b.0.norm, -Age)
names(b.0.norm) = c(b.names)
b.250 <- transmute(b.0.norm, b.250 = (b.226 +	b.257.33 +	b.280.62 +	b.297.3)/4) # 226 - 297.30
b.354 <- transmute(b.0.norm, b.354 = (b.324.21 +	b.343.49 +	b.363.91 +	b.385.55 +	b.408.48)/5) # 297.31 - 420.45
b.500 <- transmute(b.0.norm, b.500 = (b.432.77 +	b.458.5 +	b.471.94 +	b.500 +	b.514.65 +	b.545.25 +	b.561.23 +	
                                      b.577.68 +	b.594.6)/9) # 420.46 - 594.60
b.707 <- transmute(b.0.norm, b.707 =	(b.629.96 +	b.648.42 +	b.667.42 +	b.686.98 +	b.707.11 +	b.727.83 +	b.749.15 +	
                                      b.771.11 +	b.793.7 +	b.816.96 +	b.840.9)/11) # 594.61 - 840.90
b.1000 <- transmute(b.0.norm, b.1000 = (b.865.54 +	b.890.9 +	b.917 +	b.943.87 +	b.971.53 +	b.1000 +	b.1029.3 +	
                                        b.1059.46 +	b.1090.51 +	b.1122.46 +	b.1155.35 +	b.1189.21)/12) # 840.91 - 1189.21
b.1414 <- transmute(b.0.norm, b.1414 = (b.1224.05 +	b.1259.92 +	b.1296.84 +	b.1334.84 +	b.1373.95 +	b.1414.21 +	
                                        b.1455.65 +	b.1498.31 +	b.1542.21 +	b.1587.4 +	b.1633.92 +	b.1681.79)/12) # 1189.22 - 1681.79
b.2000 <- transmute(b.0.norm, b.2000 = (b.1731.07 +	b.1781.8 +	b.1834.01 +	b.1887.75 +	b.1943.06 +	b.2000 +	
                                        b.2058.6 +	b.2118.93 +	b.2181.02 +	b.2244.92 +	b.2310.71 +	b.2378.41)/12) # 1681.80 - 2378.41
b.2828 <- transmute(b.0.norm, b.2828 = (b.2448.11 +	b.2519.84 +	b.2593.68 +	b.2669.68 +	b.2747.91 +	b.2828.43 +	
                                        b.2911.31 +	b.2996.61 +	b.3084.42 +	b.3174.8 +	b.3267.83 +	b.3363.59)/12) # 2378.42 - 3363.59
b.4000 <- transmute(b.0.norm, b.4000 = (b.3462.15 +	b.3563.59 +	b.3668.02 +	b.3775.5 +	b.3886.13 +	b.4000 +	
                                        b.4117.21 +	b.4237.85 +	b.4362.03 +	b.4489.85 +	b.4621.41 +	b.4756.83)/12) # 3363.60 - 4756.83
b.5657 <- transmute(b.0.norm, b.5657 = (b.4896.21 +	b.5039.68 +	b.5187.36 +	b.5339.36 +	b.5495.81 +	b.5656.85 +	
                                        b.5822.61 +	b.5993.23 +	b.6168.84 +	b.6349.6 +	b.6535.66 +	b.6727.17)/12) # 4756.84 - 6727.17
b.8000 <- transmute(b.0.norm, b.8000 = (b.6924.29 +	b.7127.19 +	b.7336.03 +	b.7550.99 +	b.7772.26 +	b.8000)/6) # 6727.18 - 8000
b.0.norm.2 <- cbind(b.250, b.354, b.500, b.707, b.1000, b.1414, b.2000, b.2828, b.4000, b.5657, b.8000)
b.0.norm.2$age.group = "Neonate"

# add dems
dems.0 = dplyr::select(mag.2.full.0, c(gender:ear, ear.id))
b.0.norm.2 = cbind.data.frame(dems.0, b.0.norm.2)

b.6.norm = dplyr::select(b.6.norm, -Age)
names(b.6.norm) = c(b.names)
b.250 <- transmute(b.6.norm, b.250 = (b.226 +	b.257.33 +	b.280.62 +	b.297.3)/4) # 226 - 297.30
b.354 <- transmute(b.6.norm, b.354 = (b.324.21 +	b.343.49 +	b.363.91 +	b.385.55 +	b.408.48)/5) # 297.31 - 420.45
b.500 <- transmute(b.6.norm, b.500 = (b.432.77 +	b.458.5 +	b.471.94 +	b.500 +	b.514.65 +	b.545.25 +	b.561.23 +	
                                      b.577.68 +	b.594.6)/9) # 420.46 - 594.60
b.707 <- transmute(b.6.norm, b.707 =	(b.629.96 +	b.648.42 +	b.667.42 +	b.686.98 +	b.707.11 +	b.727.83 +	b.749.15 +	
                                      b.771.11 +	b.793.7 +	b.816.96 +	b.840.9)/11) # 594.61 - 840.90
b.1000 <- transmute(b.6.norm, b.1000 = (b.865.54 +	b.890.9 +	b.917 +	b.943.87 +	b.971.53 +	b.1000 +	b.1029.3 +	
                                        b.1059.46 +	b.1090.51 +	b.1122.46 +	b.1155.35 +	b.1189.21)/12) # 840.91 - 1189.21
b.1414 <- transmute(b.6.norm, b.1414 = (b.1224.05 +	b.1259.92 +	b.1296.84 +	b.1334.84 +	b.1373.95 +	b.1414.21 +	
                                        b.1455.65 +	b.1498.31 +	b.1542.21 +	b.1587.4 +	b.1633.92 +	b.1681.79)/12) # 1189.22 - 1681.79
b.2000 <- transmute(b.6.norm, b.2000 = (b.1731.07 +	b.1781.8 +	b.1834.01 +	b.1887.75 +	b.1943.06 +	b.2000 +	
                                        b.2058.6 +	b.2118.93 +	b.2181.02 +	b.2244.92 +	b.2310.71 +	b.2378.41)/12) # 1681.80 - 2378.41
b.2828 <- transmute(b.6.norm, b.2828 = (b.2448.11 +	b.2519.84 +	b.2593.68 +	b.2669.68 +	b.2747.91 +	b.2828.43 +	
                                        b.2911.31 +	b.2996.61 +	b.3084.42 +	b.3174.8 +	b.3267.83 +	b.3363.59)/12) # 2378.42 - 3363.59
b.4000 <- transmute(b.6.norm, b.4000 = (b.3462.15 +	b.3563.59 +	b.3668.02 +	b.3775.5 +	b.3886.13 +	b.4000 +	
                                        b.4117.21 +	b.4237.85 +	b.4362.03 +	b.4489.85 +	b.4621.41 +	b.4756.83)/12) # 3363.60 - 4756.83
b.5657 <- transmute(b.6.norm, b.5657 = (b.4896.21 +	b.5039.68 +	b.5187.36 +	b.5339.36 +	b.5495.81 +	b.5656.85 +	
                                        b.5822.61 +	b.5993.23 +	b.6168.84 +	b.6349.6 +	b.6535.66 +	b.6727.17)/12) # 4756.84 - 6727.17
b.8000 <- transmute(b.6.norm, b.8000 = (b.6924.29 +	b.7127.19 +	b.7336.03 +	b.7550.99 +	b.7772.26 +	b.8000)/6) # 6727.18 - 8000
b.6.norm.2 <- cbind(b.250, b.354, b.500, b.707, b.1000, b.1414, b.2000, b.2828, b.4000, b.5657, b.8000)
b.6.norm.2$age.group = "6 months"
dems.6 = dplyr::select(mag.2.full.6, c(gender:ear, ear.id))
b.6.norm.2 = cbind.data.frame(dems.6, b.6.norm.2)

b.12.norm = dplyr::select(b.12.norm, -Age)
names(b.12.norm) = c(b.names)
b.250 <- transmute(b.12.norm, b.250 = (b.226 +	b.257.33 +	b.280.62 +	b.297.3)/4) # 226 - 297.30
b.354 <- transmute(b.12.norm, b.354 = (b.324.21 +	b.343.49 +	b.363.91 +	b.385.55 +	b.408.48)/5) # 297.31 - 420.45
b.500 <- transmute(b.12.norm, b.500 = (b.432.77 +	b.458.5 +	b.471.94 +	b.500 +	b.514.65 +	b.545.25 +	b.561.23 +	
                                       b.577.68 +	b.594.6)/9) # 420.46 - 594.60
b.707 <- transmute(b.12.norm, b.707 =	(b.629.96 +	b.648.42 +	b.667.42 +	b.686.98 +	b.707.11 +	b.727.83 +	b.749.15 +	
                                       b.771.11 +	b.793.7 +	b.816.96 +	b.840.9)/11) # 594.61 - 840.90
b.1000 <- transmute(b.12.norm, b.1000 = (b.865.54 +	b.890.9 +	b.917 +	b.943.87 +	b.971.53 +	b.1000 +	b.1029.3 +	
                                         b.1059.46 +	b.1090.51 +	b.1122.46 +	b.1155.35 +	b.1189.21)/12) # 840.91 - 1189.21
b.1414 <- transmute(b.12.norm, b.1414 = (b.1224.05 +	b.1259.92 +	b.1296.84 +	b.1334.84 +	b.1373.95 +	b.1414.21 +	
                                         b.1455.65 +	b.1498.31 +	b.1542.21 +	b.1587.4 +	b.1633.92 +	b.1681.79)/12) # 1189.22 - 1681.79
b.2000 <- transmute(b.12.norm, b.2000 = (b.1731.07 +	b.1781.8 +	b.1834.01 +	b.1887.75 +	b.1943.06 +	b.2000 +	
                                         b.2058.6 +	b.2118.93 +	b.2181.02 +	b.2244.92 +	b.2310.71 +	b.2378.41)/12) # 1681.80 - 2378.41
b.2828 <- transmute(b.12.norm, b.2828 = (b.2448.11 +	b.2519.84 +	b.2593.68 +	b.2669.68 +	b.2747.91 +	b.2828.43 +	
                                         b.2911.31 +	b.2996.61 +	b.3084.42 +	b.3174.8 +	b.3267.83 +	b.3363.59)/12) # 2378.42 - 3363.59
b.4000 <- transmute(b.12.norm, b.4000 = (b.3462.15 +	b.3563.59 +	b.3668.02 +	b.3775.5 +	b.3886.13 +	b.4000 +	
                                         b.4117.21 +	b.4237.85 +	b.4362.03 +	b.4489.85 +	b.4621.41 +	b.4756.83)/12) # 3363.60 - 4756.83
b.5657 <- transmute(b.12.norm, b.5657 = (b.4896.21 +	b.5039.68 +	b.5187.36 +	b.5339.36 +	b.5495.81 +	b.5656.85 +	
                                         b.5822.61 +	b.5993.23 +	b.6168.84 +	b.6349.6 +	b.6535.66 +	b.6727.17)/12) # 4756.84 - 6727.17
b.8000 <- transmute(b.12.norm, b.8000 = (b.6924.29 +	b.7127.19 +	b.7336.03 +	b.7550.99 +	b.7772.26 +	b.8000)/6) # 6727.18 - 8000
b.12.norm.2 <- cbind(b.250, b.354, b.500, b.707, b.1000, b.1414, b.2000, b.2828, b.4000, b.5657, b.8000)
b.12.norm.2$age.group = "12 months"
dems.12 = dplyr::select(mag.2.full.12, c(gender:ear, ear.id))
b.12.norm.2 = cbind.data.frame(dems.12, b.12.norm.2)

b.18.norm = dplyr::select(b.18.norm, -Age)
names(b.18.norm) = c(b.names)
b.250 <- transmute(b.18.norm, b.250 = (b.226 +	b.257.33 +	b.280.62 +	b.297.3)/4) # 226 - 297.30
b.354 <- transmute(b.18.norm, b.354 = (b.324.21 +	b.343.49 +	b.363.91 +	b.385.55 +	b.408.48)/5) # 297.31 - 420.45
b.500 <- transmute(b.18.norm, b.500 = (b.432.77 +	b.458.5 +	b.471.94 +	b.500 +	b.514.65 +	b.545.25 +	b.561.23 +	
                                       b.577.68 +	b.594.6)/9) # 420.46 - 594.60
b.707 <- transmute(b.18.norm, b.707 =	(b.629.96 +	b.648.42 +	b.667.42 +	b.686.98 +	b.707.11 +	b.727.83 +	b.749.15 +	
                                       b.771.11 +	b.793.7 +	b.816.96 +	b.840.9)/11) # 594.61 - 840.90
b.1000 <- transmute(b.18.norm, b.1000 = (b.865.54 +	b.890.9 +	b.917 +	b.943.87 +	b.971.53 +	b.1000 +	b.1029.3 +	
                                         b.1059.46 +	b.1090.51 +	b.1122.46 +	b.1155.35 +	b.1189.21)/12) # 840.91 - 1189.21
b.1414 <- transmute(b.18.norm, b.1414 = (b.1224.05 +	b.1259.92 +	b.1296.84 +	b.1334.84 +	b.1373.95 +	b.1414.21 +	
                                         b.1455.65 +	b.1498.31 +	b.1542.21 +	b.1587.4 +	b.1633.92 +	b.1681.79)/12) # 1189.22 - 1681.79
b.2000 <- transmute(b.18.norm, b.2000 = (b.1731.07 +	b.1781.8 +	b.1834.01 +	b.1887.75 +	b.1943.06 +	b.2000 +	
                                         b.2058.6 +	b.2118.93 +	b.2181.02 +	b.2244.92 +	b.2310.71 +	b.2378.41)/12) # 1681.80 - 2378.41
b.2828 <- transmute(b.18.norm, b.2828 = (b.2448.11 +	b.2519.84 +	b.2593.68 +	b.2669.68 +	b.2747.91 +	b.2828.43 +	
                                         b.2911.31 +	b.2996.61 +	b.3084.42 +	b.3174.8 +	b.3267.83 +	b.3363.59)/12) # 2378.42 - 3363.59
b.4000 <- transmute(b.18.norm, b.4000 = (b.3462.15 +	b.3563.59 +	b.3668.02 +	b.3775.5 +	b.3886.13 +	b.4000 +	
                                         b.4117.21 +	b.4237.85 +	b.4362.03 +	b.4489.85 +	b.4621.41 +	b.4756.83)/12) # 3363.60 - 4756.83
b.5657 <- transmute(b.18.norm, b.5657 = (b.4896.21 +	b.5039.68 +	b.5187.36 +	b.5339.36 +	b.5495.81 +	b.5656.85 +	
                                         b.5822.61 +	b.5993.23 +	b.6168.84 +	b.6349.6 +	b.6535.66 +	b.6727.17)/12) # 4756.84 - 6727.17
b.8000 <- transmute(b.18.norm, b.8000 = (b.6924.29 +	b.7127.19 +	b.7336.03 +	b.7550.99 +	b.7772.26 +	b.8000)/6) # 6727.18 - 8000
b.18.norm.2 <- cbind(b.250, b.354, b.500, b.707, b.1000, b.1414, b.2000, b.2828, b.4000, b.5657, b.8000)
b.18.norm.2$age.group = "18 months"
dems.18 = dplyr::select(mag.2.full.18, c(gender:ear, ear.id))
b.18.norm.2 = cbind.data.frame(dems.18, b.18.norm.2)

b.norm.2 = rbind.data.frame(b.0.norm.2, b.6.norm.2, b.12.norm.2, b.18.norm.2)
names(b.norm.2) = c("gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", "maternal.ethnicity", "age.0.hrs", "id.res", "ear",             
                  "ear.id", "250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000", "age.group")
b.norm.2$age.group = factor(b.norm.2$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

b.norm.2 <- group_by(b.norm.2, age.group)
b.norm.2.long <- gather(b.norm.2, Frequency, b.norm, 12:22)
b.norm.2.long$Frequency = as.numeric(b.norm.2.long$Frequency)

b.norm.2.long$Frequency = as.factor(as.character(b.norm.2.long$Frequency))
b.norm.2.long$Frequency = factor(b.norm.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

str(b.norm.2.long)
b.norm.2.long$id.res = as.factor(b.norm.2.long$id.res)
b.norm.2.long$ear.id = as.factor(b.norm.2.long$ear.id)
b.norm.2.long$ear = as.factor(b.norm.2.long$ear)

# Asian = other for modeling
b.norm.2.long$maternal.ethnicity[b.norm.2.long$maternal.ethnicity == "African"] = "Asian"
b.norm.2.long$maternal.ethnicity[b.norm.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
b.norm.2.long$maternal.ethnicity[b.norm.2.long$maternal.ethnicity == "South American"] = "Asian"
b.norm.2.long$maternal.ethnicity[is.na(b.norm.2.long$maternal.ethnicity)] = "Asian"
b.norm.2.long$maternal.ethnicity = droplevels(b.norm.2.long$maternal.ethnicity)

## normalized mag
mag.0.norm = dplyr::select(mag.0.norm, -Age)
names(mag.0.norm) = c(mag.names)
mag.250 <- transmute(mag.0.norm, mag.250 = (mag.226 +	mag.257.33 +	mag.280.62 +	mag.297.3)/4) # 226 - 297.30
mag.354 <- transmute(mag.0.norm, mag.354 = (mag.324.21 +	mag.343.49 +	mag.363.91 +	mag.385.55 +	mag.408.48)/5) # 297.31 - 420.45
mag.500 <- transmute(mag.0.norm, mag.500 = (mag.432.77 +	mag.458.5 +	mag.471.94 +	mag.500 +	mag.514.65 +	mag.545.25 +	mag.561.23 +	
                                        mag.577.68 +	mag.594.6)/9) # 420.46 - 594.60
mag.707 <- transmute(mag.0.norm, mag.707 =	(mag.629.96 +	mag.648.42 +	mag.667.42 +	mag.686.98 +	mag.707.11 +	mag.727.83 +	mag.749.15 +	
                                        mag.771.11 +	mag.793.7 +	mag.816.96 +	mag.840.9)/11) # 594.61 - 840.90
mag.1000 <- transmute(mag.0.norm, mag.1000 = (mag.865.54 +	mag.890.9 +	mag.917 +	mag.943.87 +	mag.971.53 +	mag.1000 +	mag.1029.3 +	
                                          mag.1059.46 +	mag.1090.51 +	mag.1122.46 +	mag.1155.35 +	mag.1189.21)/12) # 840.91 - 1189.21
mag.1414 <- transmute(mag.0.norm, mag.1414 = (mag.1224.05 +	mag.1259.92 +	mag.1296.84 +	mag.1334.84 +	mag.1373.95 +	mag.1414.21 +	
                                          mag.1455.65 +	mag.1498.31 +	mag.1542.21 +	mag.1587.4 +	mag.1633.92 +	mag.1681.79)/12) # 1189.22 - 1681.79
mag.2000 <- transmute(mag.0.norm, mag.2000 = (mag.1731.07 +	mag.1781.8 +	mag.1834.01 +	mag.1887.75 +	mag.1943.06 +	mag.2000 +	
                                          mag.2058.6 +	mag.2118.93 +	mag.2181.02 +	mag.2244.92 +	mag.2310.71 +	mag.2378.41)/12) # 1681.80 - 2378.41
mag.2828 <- transmute(mag.0.norm, mag.2828 = (mag.2448.11 +	mag.2519.84 +	mag.2593.68 +	mag.2669.68 +	mag.2747.91 +	mag.2828.43 +	
                                          mag.2911.31 +	mag.2996.61 +	mag.3084.42 +	mag.3174.8 +	mag.3267.83 +	mag.3363.59)/12) # 2378.42 - 3363.59
mag.4000 <- transmute(mag.0.norm, mag.4000 = (mag.3462.15 +	mag.3563.59 +	mag.3668.02 +	mag.3775.5 +	mag.3886.13 +	mag.4000 +	
                                          mag.4117.21 +	mag.4237.85 +	mag.4362.03 +	mag.4489.85 +	mag.4621.41 +	mag.4756.83)/12) # 3363.60 - 4756.83
mag.5657 <- transmute(mag.0.norm, mag.5657 = (mag.4896.21 +	mag.5039.68 +	mag.5187.36 +	mag.5339.36 +	mag.5495.81 +	mag.5656.85 +	
                                          mag.5822.61 +	mag.5993.23 +	mag.6168.84 +	mag.6349.6 +	mag.6535.66 +	mag.6727.17)/12) # 4756.84 - 6727.17
mag.8000 <- transmute(mag.0.norm, mag.8000 = (mag.6924.29 +	mag.7127.19 +	mag.7336.03 +	mag.7550.99 +	mag.7772.26 +	mag.8000)/6) # 6727.18 - 8000
mag.0.norm.2 <- cbind(mag.250, mag.354, mag.500, mag.707, mag.1000, mag.1414, mag.2000, mag.2828, mag.4000, mag.5657, mag.8000)
mag.0.norm.2$age.group = "Neonate"

# add dems
dems.0 = dplyr::select(mag.2.full.0, c(gender:ear, ear.id))
mag.0.norm.2 = cbind.data.frame(dems.0, mag.0.norm.2)

mag.6.norm = dplyr::select(mag.6.norm, -Age)
names(mag.6.norm) = c(mag.names)
mag.250 <- transmute(mag.6.norm, mag.250 = (mag.226 +	mag.257.33 +	mag.280.62 +	mag.297.3)/4) # 226 - 297.30
mag.354 <- transmute(mag.6.norm, mag.354 = (mag.324.21 +	mag.343.49 +	mag.363.91 +	mag.385.55 +	mag.408.48)/5) # 297.31 - 420.45
mag.500 <- transmute(mag.6.norm, mag.500 = (mag.432.77 +	mag.458.5 +	mag.471.94 +	mag.500 +	mag.514.65 +	mag.545.25 +	mag.561.23 +	
                                        mag.577.68 +	mag.594.6)/9) # 420.46 - 594.60
mag.707 <- transmute(mag.6.norm, mag.707 =	(mag.629.96 +	mag.648.42 +	mag.667.42 +	mag.686.98 +	mag.707.11 +	mag.727.83 +	mag.749.15 +	
                                        mag.771.11 +	mag.793.7 +	mag.816.96 +	mag.840.9)/11) # 594.61 - 840.90
mag.1000 <- transmute(mag.6.norm, mag.1000 = (mag.865.54 +	mag.890.9 +	mag.917 +	mag.943.87 +	mag.971.53 +	mag.1000 +	mag.1029.3 +	
                                          mag.1059.46 +	mag.1090.51 +	mag.1122.46 +	mag.1155.35 +	mag.1189.21)/12) # 840.91 - 1189.21
mag.1414 <- transmute(mag.6.norm, mag.1414 = (mag.1224.05 +	mag.1259.92 +	mag.1296.84 +	mag.1334.84 +	mag.1373.95 +	mag.1414.21 +	
                                          mag.1455.65 +	mag.1498.31 +	mag.1542.21 +	mag.1587.4 +	mag.1633.92 +	mag.1681.79)/12) # 1189.22 - 1681.79
mag.2000 <- transmute(mag.6.norm, mag.2000 = (mag.1731.07 +	mag.1781.8 +	mag.1834.01 +	mag.1887.75 +	mag.1943.06 +	mag.2000 +	
                                          mag.2058.6 +	mag.2118.93 +	mag.2181.02 +	mag.2244.92 +	mag.2310.71 +	mag.2378.41)/12) # 1681.80 - 2378.41
mag.2828 <- transmute(mag.6.norm, mag.2828 = (mag.2448.11 +	mag.2519.84 +	mag.2593.68 +	mag.2669.68 +	mag.2747.91 +	mag.2828.43 +	
                                          mag.2911.31 +	mag.2996.61 +	mag.3084.42 +	mag.3174.8 +	mag.3267.83 +	mag.3363.59)/12) # 2378.42 - 3363.59
mag.4000 <- transmute(mag.6.norm, mag.4000 = (mag.3462.15 +	mag.3563.59 +	mag.3668.02 +	mag.3775.5 +	mag.3886.13 +	mag.4000 +	
                                          mag.4117.21 +	mag.4237.85 +	mag.4362.03 +	mag.4489.85 +	mag.4621.41 +	mag.4756.83)/12) # 3363.60 - 4756.83
mag.5657 <- transmute(mag.6.norm, mag.5657 = (mag.4896.21 +	mag.5039.68 +	mag.5187.36 +	mag.5339.36 +	mag.5495.81 +	mag.5656.85 +	
                                          mag.5822.61 +	mag.5993.23 +	mag.6168.84 +	mag.6349.6 +	mag.6535.66 +	mag.6727.17)/12) # 4756.84 - 6727.17
mag.8000 <- transmute(mag.6.norm, mag.8000 = (mag.6924.29 +	mag.7127.19 +	mag.7336.03 +	mag.7550.99 +	mag.7772.26 +	mag.8000)/6) # 6727.18 - 8000
mag.6.norm.2 <- cbind(mag.250, mag.354, mag.500, mag.707, mag.1000, mag.1414, mag.2000, mag.2828, mag.4000, mag.5657, mag.8000)
mag.6.norm.2$age.group = "6 months"
dems.6 = dplyr::select(mag.2.full.6, c(gender:ear, ear.id))
mag.6.norm.2 = cbind.data.frame(dems.6, mag.6.norm.2)

mag.12.norm = dplyr::select(mag.12.norm, -Age)
names(mag.12.norm) = c(mag.names)
mag.250 <- transmute(mag.12.norm, mag.250 = (mag.226 +	mag.257.33 +	mag.280.62 +	mag.297.3)/4) # 226 - 297.30
mag.354 <- transmute(mag.12.norm, mag.354 = (mag.324.21 +	mag.343.49 +	mag.363.91 +	mag.385.55 +	mag.408.48)/5) # 297.31 - 420.45
mag.500 <- transmute(mag.12.norm, mag.500 = (mag.432.77 +	mag.458.5 +	mag.471.94 +	mag.500 +	mag.514.65 +	mag.545.25 +	mag.561.23 +	
                                         mag.577.68 +	mag.594.6)/9) # 420.46 - 594.60
mag.707 <- transmute(mag.12.norm, mag.707 =	(mag.629.96 +	mag.648.42 +	mag.667.42 +	mag.686.98 +	mag.707.11 +	mag.727.83 +	mag.749.15 +	
                                         mag.771.11 +	mag.793.7 +	mag.816.96 +	mag.840.9)/11) # 594.61 - 840.90
mag.1000 <- transmute(mag.12.norm, mag.1000 = (mag.865.54 +	mag.890.9 +	mag.917 +	mag.943.87 +	mag.971.53 +	mag.1000 +	mag.1029.3 +	
                                           mag.1059.46 +	mag.1090.51 +	mag.1122.46 +	mag.1155.35 +	mag.1189.21)/12) # 840.91 - 1189.21
mag.1414 <- transmute(mag.12.norm, mag.1414 = (mag.1224.05 +	mag.1259.92 +	mag.1296.84 +	mag.1334.84 +	mag.1373.95 +	mag.1414.21 +	
                                           mag.1455.65 +	mag.1498.31 +	mag.1542.21 +	mag.1587.4 +	mag.1633.92 +	mag.1681.79)/12) # 1189.22 - 1681.79
mag.2000 <- transmute(mag.12.norm, mag.2000 = (mag.1731.07 +	mag.1781.8 +	mag.1834.01 +	mag.1887.75 +	mag.1943.06 +	mag.2000 +	
                                           mag.2058.6 +	mag.2118.93 +	mag.2181.02 +	mag.2244.92 +	mag.2310.71 +	mag.2378.41)/12) # 1681.80 - 2378.41
mag.2828 <- transmute(mag.12.norm, mag.2828 = (mag.2448.11 +	mag.2519.84 +	mag.2593.68 +	mag.2669.68 +	mag.2747.91 +	mag.2828.43 +	
                                           mag.2911.31 +	mag.2996.61 +	mag.3084.42 +	mag.3174.8 +	mag.3267.83 +	mag.3363.59)/12) # 2378.42 - 3363.59
mag.4000 <- transmute(mag.12.norm, mag.4000 = (mag.3462.15 +	mag.3563.59 +	mag.3668.02 +	mag.3775.5 +	mag.3886.13 +	mag.4000 +	
                                           mag.4117.21 +	mag.4237.85 +	mag.4362.03 +	mag.4489.85 +	mag.4621.41 +	mag.4756.83)/12) # 3363.60 - 4756.83
mag.5657 <- transmute(mag.12.norm, mag.5657 = (mag.4896.21 +	mag.5039.68 +	mag.5187.36 +	mag.5339.36 +	mag.5495.81 +	mag.5656.85 +	
                                           mag.5822.61 +	mag.5993.23 +	mag.6168.84 +	mag.6349.6 +	mag.6535.66 +	mag.6727.17)/12) # 4756.84 - 6727.17
mag.8000 <- transmute(mag.12.norm, mag.8000 = (mag.6924.29 +	mag.7127.19 +	mag.7336.03 +	mag.7550.99 +	mag.7772.26 +	mag.8000)/6) # 6727.18 - 8000
mag.12.norm.2 <- cbind(mag.250, mag.354, mag.500, mag.707, mag.1000, mag.1414, mag.2000, mag.2828, mag.4000, mag.5657, mag.8000)
mag.12.norm.2$age.group = "12 months"
dems.12 = dplyr::select(mag.2.full.12, c(gender:ear, ear.id))
mag.12.norm.2 = cbind.data.frame(dems.12, mag.12.norm.2)

mag.18.norm = dplyr::select(mag.18.norm, -Age)
names(mag.18.norm) = c(mag.names)
mag.250 <- transmute(mag.18.norm, mag.250 = (mag.226 +	mag.257.33 +	mag.280.62 +	mag.297.3)/4) # 226 - 297.30
mag.354 <- transmute(mag.18.norm, mag.354 = (mag.324.21 +	mag.343.49 +	mag.363.91 +	mag.385.55 +	mag.408.48)/5) # 297.31 - 420.45
mag.500 <- transmute(mag.18.norm, mag.500 = (mag.432.77 +	mag.458.5 +	mag.471.94 +	mag.500 +	mag.514.65 +	mag.545.25 +	mag.561.23 +	
                                         mag.577.68 +	mag.594.6)/9) # 420.46 - 594.60
mag.707 <- transmute(mag.18.norm, mag.707 =	(mag.629.96 +	mag.648.42 +	mag.667.42 +	mag.686.98 +	mag.707.11 +	mag.727.83 +	mag.749.15 +	
                                         mag.771.11 +	mag.793.7 +	mag.816.96 +	mag.840.9)/11) # 594.61 - 840.90
mag.1000 <- transmute(mag.18.norm, mag.1000 = (mag.865.54 +	mag.890.9 +	mag.917 +	mag.943.87 +	mag.971.53 +	mag.1000 +	mag.1029.3 +	
                                           mag.1059.46 +	mag.1090.51 +	mag.1122.46 +	mag.1155.35 +	mag.1189.21)/12) # 840.91 - 1189.21
mag.1414 <- transmute(mag.18.norm, mag.1414 = (mag.1224.05 +	mag.1259.92 +	mag.1296.84 +	mag.1334.84 +	mag.1373.95 +	mag.1414.21 +	
                                           mag.1455.65 +	mag.1498.31 +	mag.1542.21 +	mag.1587.4 +	mag.1633.92 +	mag.1681.79)/12) # 1189.22 - 1681.79
mag.2000 <- transmute(mag.18.norm, mag.2000 = (mag.1731.07 +	mag.1781.8 +	mag.1834.01 +	mag.1887.75 +	mag.1943.06 +	mag.2000 +	
                                           mag.2058.6 +	mag.2118.93 +	mag.2181.02 +	mag.2244.92 +	mag.2310.71 +	mag.2378.41)/12) # 1681.80 - 2378.41
mag.2828 <- transmute(mag.18.norm, mag.2828 = (mag.2448.11 +	mag.2519.84 +	mag.2593.68 +	mag.2669.68 +	mag.2747.91 +	mag.2828.43 +	
                                           mag.2911.31 +	mag.2996.61 +	mag.3084.42 +	mag.3174.8 +	mag.3267.83 +	mag.3363.59)/12) # 2378.42 - 3363.59
mag.4000 <- transmute(mag.18.norm, mag.4000 = (mag.3462.15 +	mag.3563.59 +	mag.3668.02 +	mag.3775.5 +	mag.3886.13 +	mag.4000 +	
                                           mag.4117.21 +	mag.4237.85 +	mag.4362.03 +	mag.4489.85 +	mag.4621.41 +	mag.4756.83)/12) # 3363.60 - 4756.83
mag.5657 <- transmute(mag.18.norm, mag.5657 = (mag.4896.21 +	mag.5039.68 +	mag.5187.36 +	mag.5339.36 +	mag.5495.81 +	mag.5656.85 +	
                                           mag.5822.61 +	mag.5993.23 +	mag.6168.84 +	mag.6349.6 +	mag.6535.66 +	mag.6727.17)/12) # 4756.84 - 6727.17
mag.8000 <- transmute(mag.18.norm, mag.8000 = (mag.6924.29 +	mag.7127.19 +	mag.7336.03 +	mag.7550.99 +	mag.7772.26 +	mag.8000)/6) # 6727.18 - 8000
mag.18.norm.2 <- cbind(mag.250, mag.354, mag.500, mag.707, mag.1000, mag.1414, mag.2000, mag.2828, mag.4000, mag.5657, mag.8000)
mag.18.norm.2$age.group = "18 months"
dems.18 = dplyr::select(mag.2.full.18, c(gender:ear, ear.id))
mag.18.norm.2 = cbind.data.frame(dems.18, mag.18.norm.2)

mag.norm.2 = rbind.data.frame(mag.0.norm.2, mag.6.norm.2, mag.12.norm.2, mag.18.norm.2)
names(mag.norm.2) = c("gender", "ga", "type.of.birth", "birth.weight", "head.circum", "body.length", "maternal.ethnicity", "age.0.hrs", "id.res", "ear",             
                    "ear.id", "250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000", "age.group")
mag.norm.2$age.group = factor(mag.norm.2$age.group, levels = c("Neonate", "6 months", "12 months", "18 months"))

mag.norm.2 <- group_by(mag.norm.2, age.group)
mag.norm.2.long <- gather(mag.norm.2, Frequency, mag.norm, 12:22)
mag.norm.2.long$Frequency = as.numeric(mag.norm.2.long$Frequency)

mag.norm.2.long$Frequency = as.factor(as.character(mag.norm.2.long$Frequency))
mag.norm.2.long$Frequency = factor(mag.norm.2.long$Frequency, levels = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000"))

str(mag.norm.2.long)
mag.norm.2.long$id.res = as.factor(mag.norm.2.long$id.res)
mag.norm.2.long$ear.id = as.factor(mag.norm.2.long$ear.id)
mag.norm.2.long$ear = as.factor(mag.norm.2.long$ear)

# Asian = other for modeling
mag.norm.2.long$maternal.ethnicity[mag.norm.2.long$maternal.ethnicity == "African"] = "Asian"
mag.norm.2.long$maternal.ethnicity[mag.norm.2.long$maternal.ethnicity == "Oceanian"] = "Asian"
mag.norm.2.long$maternal.ethnicity[mag.norm.2.long$maternal.ethnicity == "South American"] = "Asian"
mag.norm.2.long$maternal.ethnicity[is.na(mag.norm.2.long$maternal.ethnicity)] = "Asian"
mag.norm.2.long$maternal.ethnicity = droplevels(mag.norm.2.long$maternal.ethnicity)

# make ear canal area data fram
ear.canal.0 = cbind.data.frame(dems.0, S.0)
ear.canal.6 = cbind.data.frame(dems.6, S.6)
ear.canal.12 = cbind.data.frame(dems.12, S.12)
ear.canal.18 = cbind.data.frame(dems.18, S.18)
ear.canal.df = rbind.data.frame(ear.canal.0, ear.canal.6, ear.canal.12, ear.canal.18)

str(ear.canal.df)
ear.canal.df$ear = as.factor(ear.canal.df$ear)
ear.canal.df$ear.id = as.factor(ear.canal.df$ear.id)
ear.canal.df$maternal.ethnicity[ear.canal.df$maternal.ethnicity == "African"] = "Asian"
ear.canal.df$maternal.ethnicity[ear.canal.df$maternal.ethnicity == "Oceanian"] = "Asian"
ear.canal.df$maternal.ethnicity[ear.canal.df$maternal.ethnicity == "South American"] = "Asian"
ear.canal.df$maternal.ethnicity[is.na(ear.canal.df$maternal.ethnicity)] = "Asian"
ear.canal.df$maternal.ethnicity = droplevels(ear.canal.df$maternal.ethnicity)

# modeling
#library(lme4)
library(lmerTest)
library(MASS)
library(rcompanion)
library(car)
library(emmeans)
library(afex) # this makes the || notation work for factors
options(scipen = 10)
# (1 | g1/g2); Intercept varying among g1 and g2 within g1. (g1 is the group), for example - (1|school/class) models classes nested within schools
# start with simple model and increase complexity until no longer improving fit - then compare to null model
# compare models using anova (model1, model2) with the simpler model first 
# use Anova from car package, rather than anova - https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html # can't though because glmmTMB only has anova
# when comparing with anova - set reml as F, then update with reml = T for final model update(f, REML = T)

# age vs ethnicity
# newborns
abs.2.full.0$maternal.ethnicity[abs.2.full.0$maternal.ethnicity == "African"] = "Asian"
abs.2.full.0$maternal.ethnicity[abs.2.full.0$maternal.ethnicity == "Oceanian"] = "Asian"
abs.2.full.0$maternal.ethnicity[abs.2.full.0$maternal.ethnicity == "South American"] = "Asian"
# set NA to Other (Asian)
abs.2.full.0$maternal.ethnicity[is.na(abs.2.full.0$maternal.ethnicity)] = "Asian"
abs.2.full.0$maternal.ethnicity = droplevels(abs.2.full.0$maternal.ethnicity)

# select just one ear
age.0.df = group_by(abs.2.full.0, id.res)
age.0.df = sample_n(age.0.df, 1)
t.0 = t.test(age.0.hrs ~ maternal.ethnicity, age.0.df)
t.0.table = data.frame(matrix(vector(), 1, 4,
                              dimnames=list(c(), c("Age", "Statistic", "DF", "p.value"))),
                       stringsAsFactors=F) 
t.0.table$Statistic = round(t.0$statistic, 2)
t.0.table$DF = round(t.0$parameter, 2)
t.0.table$p.value = round(t.0$p.value, 3)
t.0.table$Age = "Neonate"

# 6 mths
age.6$maternal.ethnicity[age.6$maternal.ethnicity == "African"] = "Asian"
age.6$maternal.ethnicity[age.6$maternal.ethnicity == "Oceanian"] = "Asian"
age.6$maternal.ethnicity[age.6$maternal.ethnicity == "South American"] = "Asian"
# set NA to Other (Asian)
age.6$maternal.ethnicity[is.na(age.6$maternal.ethnicity)] = "Asian"
age.6$maternal.ethnicity = droplevels(age.6$maternal.ethnicity)
t.6 = t.test(age.6.mth.in.weeks ~ maternal.ethnicity, age.6)
t.6.table = data.frame(matrix(vector(), 1, 4,
                              dimnames=list(c(), c("Age", "Statistic", "DF", "p.value"))),
                       stringsAsFactors=F) 
t.6.table$Statistic = round(t.6$statistic, 2)
t.6.table$DF = round(t.6$parameter, 2)
t.6.table$p.value = round(t.6$p.value, 3)
t.6.table$Age = "6 months"

# 12 mths
age.12$maternal.ethnicity[age.12$maternal.ethnicity == "African"] = "Asian"
age.12$maternal.ethnicity[age.12$maternal.ethnicity == "Oceanian"] = "Asian"
age.12$maternal.ethnicity[age.12$maternal.ethnicity == "South American"] = "Asian"
# set NA to Other (Asian)
age.12$maternal.ethnicity[is.na(age.12$maternal.ethnicity)] = "Asian"
age.12$maternal.ethnicity = droplevels(age.12$maternal.ethnicity)
t.12 = t.test(age.12.mth.in.weeks ~ maternal.ethnicity, age.12)
t.12.table = data.frame(matrix(vector(), 1, 4,
                              dimnames=list(c(), c("Age", "Statistic", "DF", "p.value"))),
                       stringsAsFactors=F) 
t.12.table$Statistic = round(t.12$statistic, 2)
t.12.table$DF = round(t.12$parameter, 2)
t.12.table$p.value = round(t.12$p.value, 3)
t.12.table$Age = "12 months"

# 18 mths
age.18$maternal.ethnicity[age.18$maternal.ethnicity == "African"] = "Asian"
age.18$maternal.ethnicity[age.18$maternal.ethnicity == "Oceanian"] = "Asian"
age.18$maternal.ethnicity[age.18$maternal.ethnicity == "South American"] = "Asian"
# set NA to Other (Asian)
age.18$maternal.ethnicity[is.na(age.18$maternal.ethnicity)] = "Asian"
age.18$maternal.ethnicity = droplevels(age.18$maternal.ethnicity)
t.18 = t.test(age.18.mth.in.weeks ~ maternal.ethnicity, age.18)
t.18.table = data.frame(matrix(vector(), 1, 4,
                               dimnames=list(c(), c("Age", "Statistic", "DF", "p.value"))),
                        stringsAsFactors=F) 
t.18.table$Statistic = round(t.18$statistic, 2)
t.18.table$DF = round(t.18$parameter, 2)
t.18.table$p.value = round(t.18$p.value, 3)
t.18.table$Age = "18 months"

age.t.test.df = rbind.data.frame(t.0.table, t.6.table, t.12.table, t.18.table)
write.table(age.t.test.df, file = "age.t.test.txt", sep = ",", quote = FALSE, row.names = F)

# ear canal area model
ear.canal.f = lmer(S.cm2 ~ age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), ear.canal.df, REML = F)
# check assumptions
plot(ear.canal.f) 

# transform
plotNormalHistogram(ear.canal.df$S.cm2) # original

# log
ear.canal.df$S_log = log(ear.canal.df$S.cm2)
plotNormalHistogram(ear.canal.df$S_log) 

# sqrt
ear.canal.df$S_sqrt = sqrt(ear.canal.df$S.cm2)
ear.canal.f_sqrt = lmer(S_sqrt ~ age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), ear.canal.df, REML = F) 
plot(ear.canal.f_sqrt) # not this one

# cube root
ear.canal.df$S_cube = abs(ear.canal.df$S.cm2)^(1/3)
ear.canal.f_cube = lmer(S_cube ~ age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), ear.canal.df, REML = F) 
plot(ear.canal.f_cube) # no

# box cox
# just do linear model to choose lambda (lmer model didn't work anymore after update)
ear.canal.lm = lm(S.cm2 ~ age.group * maternal.ethnicity + gender + ear, ear.canal.df)
Box = boxCox(ear.canal.lm, lambda = seq(-2,2,0.1))      
Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest log likelihood
lambda = Cox2[1, "Box.x"]                 # Extract that lambda

ear.canal.df$S_box = (ear.canal.df$S.cm2 ^ lambda - 1)/lambda   # Transform the original data
ear.canal.f_box = lmer(S_box ~ age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), ear.canal.df, REML = F) 
plot(ear.canal.f_box) 

# log and boxcox are very similar - use log
ear.canal.f_log = lmer(S_log ~ age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), ear.canal.df, REML = F)
plot(ear.canal.f_log) 
anova(ear.canal.f_log)

ear.canal.f.final = update(ear.canal.f_log, REML = T)
# post hoc analyses (plot 95% ci)
ear.canal.lsmean.age = emmeans(ear.canal.f.final, specs = "age.group") 
ear.canal.lsmean.age = summary(ear.canal.lsmean.age)
ear.canal.lsmean.age = dplyr::select(ear.canal.lsmean.age, -c(3:4))
names(ear.canal.lsmean.age) = c("Age", "mean", "lower", "upper")

ear.canal.lsmean.age_t = ear.canal.lsmean.age
ear.canal.lsmean.age_t$mean = exp(ear.canal.lsmean.age_t$mean)
ear.canal.lsmean.age_t$lower = exp(ear.canal.lsmean.age_t$lower)
ear.canal.lsmean.age_t$upper = exp(ear.canal.lsmean.age_t$upper)

# report in mm^2
ear.canal.lsmean.age_t = mutate_at(ear.canal.lsmean.age_t, vars(mean, lower, upper), funs(. *100))
ear.canal.lsmean.age_t = mutate_at(ear.canal.lsmean.age_t, vars(mean, lower, upper), funs(round(.,2)))
ear.canal.lsmean.age_t = t(ear.canal.lsmean.age_t)
write.table(ear.canal.lsmean.age_t, file = "ear.canal.emmeans.txt", sep = ",", quote = FALSE, row.names = F)

####################
# abs
# first fit with reml = F to compare with anova then final model update with reml = T
# regarding the random effects - the most correct option is:
#fit = lmer(Absorbance ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency | id.res/ear.id), abs.2.long, REML = F) 
# however - need a massive dataset to fit this - another option is to use afex package (see || in the random part)
#fit = lmer_alt(Absorbance ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), abs.2.long, REML = F) 
# You can also remove the correlation parameters manually (https://rpubs.com/Reinhold/22193) 
# Or, you can just use the simplified 1|id.res/ear.id - which assumes compound symmetry:
#fit2 = lmer(Absorbance ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), abs.2.long, REML = F) 

# linear model
abs.f1 = lmer(Absorbance ~ Frequency * age.group + gender + maternal.ethnicity + ear + (1 | id.res/ear.id), abs.2.long, REML = F)
abs.f = lmer(Absorbance ~ Frequency * age.group + gender + maternal.ethnicity + ear + (1 | id.res/ear.id), abs.2.long, REML = F)
anova(abs.f, abs.f1) # including /Frequency doesn't change much
AIC(abs.f)
AIC(abs.f1) 
# check assumptions
plot(abs.f) 
# there is a pattern in the residuals - because it is a proportion (bounded by 0 and 1 - not linear, therefore)
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
# set <0 to 0.0001 and use logit transform
abs.2.long$Absorbance_0 = abs.2.long$Absorbance
abs.2.long$Absorbance_0[abs.2.long$Absorbance_0 <= 0] <- 0.0001
abs.2.long$Absorbance_t = logit(abs.2.long$Absorbance_0)

abs.f_t = lmer_alt(Absorbance_t ~ Frequency * age.group * maternal.ethnicity + gender +  + ear + (Frequency || id.res/ear.id), abs.2.long, REML = F)
summary(abs.f_t)
anova(abs.f_t)
Anova(abs.f_t)

abs.f.final = update(abs.f_t, REML = TRUE) 

# post hoc analyses (plot 95% ci)
abs.lsmean.age = emmeans(abs.f.final, specs = "Frequency", by = "age.group") 
abs.lsmean.age = summary(abs.lsmean.age)
abs.lsmean.age = dplyr::select(abs.lsmean.age, -c(4:5))
names(abs.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
abs.lsmean.age$Frequency = as.numeric(as.character(abs.lsmean.age$Frequency))

# "type = response" didn't work so need to convert it back to original scale 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

abs.lsmean.age$mean_t = logit2prob(abs.lsmean.age$mean)
abs.lsmean.age$lower_t = logit2prob(abs.lsmean.age$lower)
abs.lsmean.age$upper_t = logit2prob(abs.lsmean.age$upper)
abs.lsmean_t = abs.lsmean.age

# plot
abs.plot.lsmean <- ggplot(abs.lsmean_t, aes(x=Frequency, y=mean_t, ymin=lower_t, ymax=upper_t, group=Age, colour=Age, fill=Age)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = "none")
print(abs.plot.lsmean)
#ggsave("abs.ls.jpeg", abs.plot.lsmean, height=3, width=5, dpi=500)

# plot ethnicity effect
abs.lsmean.eth = emmeans(abs.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
abs.lsmean.eth = summary(abs.lsmean.eth)
abs.lsmean.eth = dplyr::select(abs.lsmean.eth, -c(5:6))
names(abs.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
abs.lsmean.eth$Frequency = as.numeric(as.character(abs.lsmean.eth$Frequency))

abs.lsmean.eth$mean_t = logit2prob(abs.lsmean.eth$mean)
abs.lsmean.eth$lower_t = logit2prob(abs.lsmean.eth$lower)
abs.lsmean.eth$upper_t = logit2prob(abs.lsmean.eth$upper)

abs.lsmean.eth$Ethnicity = as.character(abs.lsmean.eth$Ethnicity)
abs.lsmean.eth$Ethnicity[abs.lsmean.eth$Ethnicity == "Asian"] = "Other"

abs.eth.ls <- ggplot(abs.lsmean.eth, aes(x=Frequency, y=mean_t, ymin=lower_t, ymax=upper_t, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(legend.position = c(0.02, 0.98)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(abs.eth.ls)
ggsave("abs.eth.ls.jpeg", abs.eth.ls, height=6, width=10, dpi=500)

## Admittance
mag.norm.f = lmer(mag.norm ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F) 
plot(mag.norm.f) # Heteroscedasticity - transforming may help

pha.f = lmer(Phase ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), pha.2.long, REML = F) 
plot(pha.f) # boundry effect - passive system bounded by -90 and 90 degrees (keefe 1993)

## PLAN
# do mag tip and normalized mag
# then normalized G and B

# mag at the tip
mag.f = lmer(Magnitude ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.2.long, REML = F)
plot(mag.f) # Heteroscedasticity - transform
AIC(mag.f)

# try some different transforms (another option is powertransform function in car package - not sure how to back transfrom though)
# log
mag.2.long$mag_log = log(mag.2.long$Magnitude)
mag.f_t.log = lmer(mag_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.2.long, REML = F)
plot(mag.f_t.log) # looks good

# sqrt
mag.2.long$mag_sqrt = sqrt(mag.2.long$Magnitude)
mag.f_t.sqrt = lmer(mag_sqrt ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.2.long, REML = F)
plot(mag.f_t.sqrt) # log was better

# cube root
mag.2.long$mag_cube = abs(mag.2.long$Magnitude)^(1/3)
mag.f_t.cube = lmer(mag_cube ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.2.long, REML = F)
plot(mag.f_t.cube) # log was better

# box cox
plotNormalHistogram(mag.2.long$Magnitude) # original
# refit positive
mag.f_lm = lm(Magnitude ~ Frequency * age.group * maternal.ethnicity + gender + ear, mag.2.long)
Box = boxCox(mag.f_lm, lambda = seq(-2,2,0.1))

Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest log likelihood
lambda = Cox2[1, "Box.x"]                 # Extract that lambda

mag.2.long$mag_box = (mag.2.long$Magnitude ^ lambda - 1)/lambda   # Transform the original (positive) data
mag.f_t.box = lmer(mag_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.2.long, REML = F)
plot(mag.f_t.box) # log was better

# use log transform
mag.f_t = lmer_alt(mag_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), mag.2.long, REML = F)
plot(mag.f_t)
AIC(mag.f_t) 
summary(mag.f_t)
anova(mag.f_t)
Anova(mag.f_t) 

# refit model with reml = T
mag.f.final = update(mag.f_t, REML = T)
#anova(mag.f.final) # should only do anova if reml = F

# post hoc plots
mag.lsmean.age = emmeans(mag.f.final, specs = "Frequency", by = "age.group", type = "response")
mag.lsmean.age = summary(mag.lsmean.age)
mag.lsmean.age = dplyr::select(mag.lsmean.age, -c(4:5))
names(mag.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
mag.lsmean.age$Frequency = as.numeric(as.character(mag.lsmean.age$Frequency))

# back transform mean and 95% CI
mag.lsmean.age_t = mag.lsmean.age
mag.lsmean.age_t$mean = exp(mag.lsmean.age$mean)
mag.lsmean.age_t$lower = exp(mag.lsmean.age$lower)
mag.lsmean.age_t$upper = exp(mag.lsmean.age$upper)

# plot
mag.plot.lsmean <- ggplot(mag.lsmean.age_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(0, 10)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
print(mag.plot.lsmean)
ggsave("mag.ls.jpeg", mag.plot.lsmean, height=4, width=6, dpi=500)

# plot ethnicity effect
mag.lsmean.eth = emmeans(mag.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
mag.lsmean.eth = summary(mag.lsmean.eth)
mag.lsmean.eth = dplyr::select(mag.lsmean.eth, -c(5:6))
names(mag.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
mag.lsmean.eth$Frequency = as.numeric(as.character(mag.lsmean.eth$Frequency))

mag.lsmean.eth_t = mag.lsmean.eth
mag.lsmean.eth_t$mean = exp(mag.lsmean.eth$mean)
mag.lsmean.eth_t$lower = exp(mag.lsmean.eth$lower)
mag.lsmean.eth_t$upper = exp(mag.lsmean.eth$upper)

mag.lsmean.eth_t$Ethnicity = as.character(mag.lsmean.eth_t$Ethnicity)
mag.lsmean.eth_t$Ethnicity[mag.lsmean.eth_t$Ethnicity == "Asian"] = "Other"

mag.eth.ls <- ggplot(mag.lsmean.eth_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(-0.1, 11)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0,1)) +
  theme(legend.position = c(0.02, 0.98)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(mag.eth.ls)
ggsave("mag.eth.ls.jpeg", mag.eth.ls, height=6, width=10, dpi=500)

## mag.norm
mag.norm.f = lmer(mag.norm ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f) # Heteroscedasticity - transform
AIC(mag.norm.f)

# try some different transforms (another option is powertransform function in car package - not sure how to back transfrom though)
# log
mag.norm.2.long$mag_log = log(mag.norm.2.long$mag.norm)
mag.norm.f_t.log = lmer(mag_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f_t.log) # looks good

# sqrt
mag.norm.2.long$mag_sqrt = sqrt(mag.norm.2.long$mag.norm)
mag.norm.f_t.sqrt = lmer(mag_sqrt ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f_t.sqrt) # log was better

# cube root
mag.norm.2.long$mag_cube = abs(mag.norm.2.long$mag.norm)^(1/3)
mag.norm.f_t.cube = lmer(mag_cube ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f_t.cube) # log was better

# box cox
#plotNormalHistogram(mag.norm.2.long$mag.norm) # original
# refit positive
mag.norm.f_pos = lm(mag.norm ~ Frequency * age.group * maternal.ethnicity + gender + ear, mag.norm.2.long)
Box = boxCox(mag.norm.f_pos, lambda = seq(-2,2,0.1))

Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest log likelihood
lambda = Cox2[1, "Box.x"]                 # Extract that lambda

mag.norm.2.long$mag_box = (mag.norm.2.long$mag.norm ^ lambda - 1)/lambda   # Transform the original (positive) data
mag.norm.f_t.box = lmer(mag_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f_t.box) 

# use log
mag.norm.f_t = lmer_alt(mag_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), mag.norm.2.long, REML = F)
plot(mag.norm.f_t)
AIC(mag.norm.f_t) 
summary(mag.norm.f_t)
anova(mag.norm.f_t)
Anova(mag.norm.f_t) 

# refit model with reml = T
mag.norm.f.final = update(mag.norm.f_t, REML = T)
#anova(mag.norm.f.final) # should only do anova if reml = F

# post hoc plots
mag.norm.lsmean.age = emmeans(mag.norm.f.final, specs = "Frequency", by = "age.group", type = "response")
mag.norm.lsmean.age = summary(mag.norm.lsmean.age)
mag.norm.lsmean.age = dplyr::select(mag.norm.lsmean.age, -c(4:5))
names(mag.norm.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
mag.norm.lsmean.age$Frequency = as.numeric(as.character(mag.norm.lsmean.age$Frequency))

mag.norm.lsmean.age_t = mag.norm.lsmean.age
mag.norm.lsmean.age_t$mean = exp(mag.norm.lsmean.age$mean)
mag.norm.lsmean.age_t$lower = exp(mag.norm.lsmean.age$lower)
mag.norm.lsmean.age_t$upper = exp(mag.norm.lsmean.age$upper)

# plot
mag.norm.plot.lsmean <- ggplot(mag.norm.lsmean.age_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5, 2), limits=c(0, 2)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position = "none")
#print(mag.norm.plot.lsmean)
ggsave("mag.norm.ls.jpeg", mag.norm.plot.lsmean, height=4, width=6, dpi=500)

# plot ethnicity effect
mag.norm.lsmean.eth = emmeans(mag.norm.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
mag.norm.lsmean.eth = summary(mag.norm.lsmean.eth)
mag.norm.lsmean.eth = dplyr::select(mag.norm.lsmean.eth, -c(5:6))
names(mag.norm.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
mag.norm.lsmean.eth$Frequency = as.numeric(as.character(mag.norm.lsmean.eth$Frequency))

mag.norm.lsmean.eth_t = mag.norm.lsmean.eth
mag.norm.lsmean.eth_t$mean = exp(mag.norm.lsmean.eth$mean)
mag.norm.lsmean.eth_t$lower = exp(mag.norm.lsmean.eth$lower)
mag.norm.lsmean.eth_t$upper = exp(mag.norm.lsmean.eth$upper)

mag.norm.lsmean.eth_t$Ethnicity = as.character(mag.norm.lsmean.eth_t$Ethnicity)
mag.norm.lsmean.eth_t$Ethnicity[mag.norm.lsmean.eth_t$Ethnicity == "Asian"] = "Other"

mag.norm.eth.ls <- ggplot(mag.norm.lsmean.eth_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5, 2), limits=c(0, 2.1)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
        legend.position=c(0,1)) +
  theme(legend.position = c(0.02, 0.98)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(mag.norm.eth.ls)
ggsave("mag.norm.eth.ls.jpeg", mag.norm.eth.ls, height=6, width=10, dpi=500)

### G and B at the tip - not using - just doing mag at the tip (G and B only normalized models)
## g
# g.f = lmer(G ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# plot(g.f) # Heteroscedasticity - transform
# AIC(g.f)
# 
# # transform g
# g.2.long$G1 = g.2.long$G + 1
# 
# # try some different transforms (another option is powertransform function in car package - not sure how to back transfrom though)
# # log
# g.2.long$G_log = log(g.2.long$G1)
# g.f_t.log = lmer(G_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# plot(g.f_t.log) # looks good
# 
# # sqrt
# g.2.long$G_sqrt = sqrt(g.2.long$G1)
# g.f_t.sqrt = lmer(G_sqrt ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# plot(g.f_t.sqrt) # log was better
# 
# # cube root
# g.2.long$G_cube = abs(g.2.long$G1)^(1/3)
# g.f_t.cube = lmer(G_cube ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# plot(g.f_t.cube) # log was better
# 
# # box cox
# plotNormalHistogram(g.2.long$G) # original
# # refit positive
# g.f_pos = lmer(G1 ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# Box = boxCox(g.f_pos, lambda = seq(-2,2,0.1))
# 
# Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda = Cox2[1, "Box.x"]                 # Extract that lambda
# 
# g.2.long$G_box = (g.2.long$G ^ lambda - 1)/lambda   # Transform the original data
# g.f_t.box = lmer(G_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# plot(g.f_t.box) # log was better
# 
# # use log transform
# g.f_t = lmer_alt(G_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), g.2.long, REML = F)
# plot(g.f_t)
# AIC(g.f_t) 
# summary(g.f_t)
# anova(g.f_t)
# Anova(g.f_t) 
# 
# # test is anova(mod), is the same as anova(mod1, mod2)
# # model -gender
# #g.f.sex = lmer(G_log ~ Frequency * age.group * maternal.ethnicity + ear + (1 | id.res/ear.id), g.2.long, REML = F)
# #anova(g.f.sex, g.f_t) # is the same as the other anova? Yes
# 
# # refit model with reml = T
# g.f.final = update(g.f_t, REML = T)
# #anova(g.f.final) # should only do anova if reml = F
# 
# # post hoc plots
# g.lsmean.age = emmeans(g.f.final, specs = "Frequency", by = "age.group", type = "response")
# g.lsmean.age = summary(g.lsmean.age)
# g.lsmean.age = dplyr::select(g.lsmean.age, -c(4:5))
# names(g.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
# g.lsmean.age$Frequency = as.numeric(as.character(g.lsmean.age$Frequency))
# 
# # back transform mean and 95% CI
# g.lsmean.age_t = g.lsmean.age
# g.lsmean.age_t$mean = exp(g.lsmean.age$mean)
# g.lsmean.age_t$mean = g.lsmean.age_t$mean - 1
# g.lsmean.age_t$lower = exp(g.lsmean.age$lower)
# g.lsmean.age_t$lower = g.lsmean.age_t$lower -1
# g.lsmean.age_t$upper = exp(g.lsmean.age$upper)
# g.lsmean.age_t$upper = g.lsmean.age_t$upper - 1
# 
# # plot
# g.plot.lsmean <- ggplot(g.lsmean.age_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("G"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(0, 10)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.01,0.99)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
# print(g.plot.lsmean)
# ggsave("g.ls.jpeg", g.plot.lsmean, height=4, width=6, dpi=500)
# 
# # plot ethnicity effect
# g.lsmean.eth = emmeans(g.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
# g.lsmean.eth = summary(g.lsmean.eth)
# g.lsmean.eth = dplyr::select(g.lsmean.eth, -c(5:6))
# names(g.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
# g.lsmean.eth$Frequency = as.numeric(as.character(g.lsmean.eth$Frequency))
# 
# g.lsmean.eth_t = g.lsmean.eth
# g.lsmean.eth_t$mean = exp(g.lsmean.eth$mean)
# g.lsmean.eth_t$mean = g.lsmean.eth_t$mean - 1
# g.lsmean.eth_t$lower = exp(g.lsmean.eth$lower)
# g.lsmean.eth_t$lower = g.lsmean.eth_t$lower -1
# g.lsmean.eth_t$upper = exp(g.lsmean.eth$upper)
# g.lsmean.eth_t$upper = g.lsmean.eth_t$upper - 1
# 
# g.lsmean.eth_t$Ethnicity = as.character(g.lsmean.eth_t$Ethnicity)
# g.lsmean.eth_t$Ethnicity[g.lsmean.eth_t$Ethnicity == "Asian"] = "Other"
# 
# g.eth.ls <- ggplot(g.lsmean.eth_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("G"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(-0.1, 10.1)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0,1)) +
#   theme(legend.position = c(0.02, 0.98)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   facet_wrap(~Age, ncol = 2, scales="free")
# print(g.eth.ls)
# ggsave("g.eth.ls.jpeg", g.eth.ls, height=6, width=10, dpi=500)
# 
# # plot sex effect
# g.lsmean.sex = emmeans(g.f.final, specs = "Frequency", by = c("age.group", "gender"), type = "response")
# g.lsmean.sex = summary(g.lsmean.sex)
# g.lsmean.sex = dplyr::select(g.lsmean.sex, -c(5:6))
# names(g.lsmean.sex) = c("Frequency", "Age", "Gender", "mean", "lower", "upper")
# g.lsmean.sex$Frequency = as.numeric(as.character(g.lsmean.sex$Frequency))
# 
# g.lsmean_t = g.lsmean.sex
# g.lsmean_t$mean = exp(g.lsmean.sex$mean)
# g.lsmean_t$mean = g.lsmean_t$mean - 1
# g.lsmean_t$lower = exp(g.lsmean.sex$lower)
# g.lsmean_t$lower = g.lsmean_t$lower -1
# g.lsmean_t$upper = exp(g.lsmean.sex$upper)
# g.lsmean_t$upper = g.lsmean_t$upper - 1
# 
# g.lsmean_t$Gender = as.character(g.lsmean_t$Gender)
# g.lsmean_t$Gender[g.lsmean_t$Gender == "F"] = "Female"
# g.lsmean_t$Gender[g.lsmean_t$Gender == "M"] = "Male"
# g.lsmean_t$Gender = as.factor(g.lsmean_t$Gender)
# 
# g.sex.ls <- ggplot(g.lsmean_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Gender, colour=Gender, fill=Gender)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("G"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10), limits=c(0, 10)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0,1)) +
#   theme(legend.position = c(0.02, 0.98)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   facet_wrap(~Age, ncol = 2, scales="free")
# print(g.sex.ls)
# ggsave("g.sex.ls.jpeg", g.sex.ls, height=6, width=10, dpi=500)
# 
# ## another option could be to do glmm with gamma (log) link - but needs to be positive, so need to set =<0 to 0.0001
# ## HOWEVER see https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
# ## GAMMA is difficult to implement GLMM - can't do it
# 
# ## b
# b.f = lmer_alt(B ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), b.2.long, REML = F)
# plot(b.f) # not too bad - the gap is just where there are no values
# AIC(b.f)
# anova(b.f)
# Anova(b.f) # main effect ethnicity not significant, but age * interaction is (same as G)
#            # sex not significant (for G it was, but abs wasn't also)
# 
# # refit model with reml = T
# b.f.final = update(b.f, REML = T)
# 
# # post hoc plots
# b.lsmean.age = emmeans(b.f.final, specs = "Frequency", by = "age.group", type = "response")
# b.lsmean.age = summary(b.lsmean.age)
# b.lsmean.age = dplyr::select(b.lsmean.age, -c(4:5))
# names(b.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
# b.lsmean.age$Frequency = as.numeric(as.character(b.lsmean.age$Frequency))
# 
# # plot
# b.plot.lsmean <- ggplot(b.lsmean.age, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("B"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-4, -2, 0, 2, 4, 6, 8), limits=c(-4, 8)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.01,0.99)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   theme(legend.position = "none")
# #print(b.plot.lsmean)
# #ggsave("b.ls.jpeg", b.plot.lsmean, height=4, width=6, dpi=500)
# 
# ## multiplot - A, G, B
# ls.plot.full <- plot_grid(abs.plot.lsmean, g.plot.lsmean, b.plot.lsmean, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C"))
# ggsave("ls.plot.full.jpeg", ls.plot.full, height=9, width=6, dpi=500)
# 
# # plot ethnicity effect
# b.lsmean.eth = emmeans(b.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
# b.lsmean.eth = summary(b.lsmean.eth)
# b.lsmean.eth = dplyr::select(b.lsmean.eth, -c(5:6))
# names(b.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
# b.lsmean.eth$Frequency = as.numeric(as.character(b.lsmean.eth$Frequency))
# 
# b.lsmean.eth$Ethnicity = as.character(b.lsmean.eth$Ethnicity)
# b.lsmean.eth$Ethnicity[b.lsmean.eth$Ethnicity == "Asian"] = "Other"
# 
# b.eth.ls <- ggplot(b.lsmean.eth, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("B"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-4, -2, 0, 2, 4, 6, 8), limits=c(-4, 8)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0,1)) +
#   theme(legend.position = c(0.02, 0.98)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   facet_wrap(~Age, ncol = 2, scales="free")
# #print(b.eth.ls)
# ggsave("b.eth.ls.jpeg", b.eth.ls, height=6, width=10, dpi=500)

####### Normalized G and B
## g.norm
# the random effect for the final model is Frequency || id.res/ear.id - but it takes a long time to run, so just use the simplified initially
g.norm.f = lmer(g.norm ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.norm.2.long, REML = F) 
plot(g.norm.f) # Heteroscedasticity - transform
AIC(g.norm.f)

# transform g
g.norm.2.long$g.norm1 = g.norm.2.long$g.norm + 1 # make positive for log and boxcox

# log
g.norm.2.long$g.norm_log = log(g.norm.2.long$g.norm1)
g.norm.f_t.log = lmer(g.norm_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.norm.2.long, REML = F) 
plot(g.norm.f_t.log) 

# cube root
g.norm.2.long$g.norm_cube = abs(g.norm.2.long$g.norm1)^(1/3)
g.norm.f_t.cube = lmer(g.norm_cube ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.norm.2.long, REML = F) 
plot(g.norm.f_t.cube) # not too bad actually - I think better than log

# square root
g.norm.2.long$g.norm_sqrt = sqrt(g.norm.2.long$g.norm1)
g.norm.f_t.sqrt = lmer(g.norm_sqrt ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.norm.2.long, REML = F) 
plot(g.norm.f_t.sqrt) # still an issue

# box cox
plotNormalHistogram(g.norm.2.long$g.norm) # original
g.norm.f.lm = lm(g.norm1 ~ Frequency * age.group * maternal.ethnicity + gender + ear, g.norm.2.long) 
Box = boxCox(g.norm.f.lm, lambda = seq(-2, 2, 0.1))  
# Try values -2 to 2 by 0.1 (ususally should be within -2 or 2 or there is some other issue with the model)

Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]                                  # Display the lambda with the greatest log likelihood
lambda = Cox2[1, "Box.x"]                 # Extract that lambda

g.norm.2.long$G_box = (g.norm.2.long$g.norm1 ^ lambda - 1)/lambda   # Transform the original data
g.f_t.box = lmer(G_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), g.norm.2.long, REML = F) 
plot(g.f_t.box) 

# use box cox
g.norm.f_t = lmer_alt(G_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), g.norm.2.long, REML = F) 
AIC(g.norm.f_t)
plot(g.norm.f_t) 
AIC(g.norm.f_t) # much better AIC
summary(g.norm.f_t)
anova(g.norm.f_t)
Anova(g.norm.f_t) 

# refit model with reml = T
g.norm.f.final = update(g.norm.f_t, REML = T)
#anova(g.f.final) # should only do anova if reml = F

# post hoc plots
g.norm.lsmean.age = emmeans(g.norm.f.final, specs = "Frequency", by = "age.group", type = "response")
g.norm.lsmean.age = summary(g.norm.lsmean.age)
g.norm.lsmean.age = dplyr::select(g.norm.lsmean.age, -c(4:5))
names(g.norm.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
g.norm.lsmean.age$Frequency = as.numeric(as.character(g.norm.lsmean.age$Frequency))

# back transform mean and 95% CI
invBoxCox <- function(x, lambda) 
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 

g.norm.lsmean.age_t = g.norm.lsmean.age
g.norm.lsmean.age_t$mean = invBoxCox(g.norm.lsmean.age$mean, lambda = lambda)
g.norm.lsmean.age_t$mean = g.norm.lsmean.age_t$mean - 1
g.norm.lsmean.age_t$lower = invBoxCox(g.norm.lsmean.age$lower, lambda = lambda)
g.norm.lsmean.age_t$lower = g.norm.lsmean.age_t$lower -1
g.norm.lsmean.age_t$upper = invBoxCox(g.norm.lsmean.age$upper, lambda = lambda)
g.norm.lsmean.age_t$upper = g.norm.lsmean.age_t$upper - 1

# plot
g.norm.plot.lsmean <- ggplot(g.norm.lsmean.age_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("G")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5), limits=c(0, 1.5)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
#print(g.norm.plot.lsmean)
#ggsave("g.norm.ls.jpeg", g.norm.plot.lsmean, height=4, width=6, dpi=500)

# plot ethnicity effect
g.norm.lsmean.eth = emmeans(g.norm.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
g.norm.lsmean.eth = summary(g.norm.lsmean.eth)
g.norm.lsmean.eth = dplyr::select(g.norm.lsmean.eth, -c(5:6))
names(g.norm.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
g.norm.lsmean.eth$Frequency = as.numeric(as.character(g.norm.lsmean.eth$Frequency))

g.norm.lsmean.eth_t = g.norm.lsmean.eth
g.norm.lsmean.eth_t$mean = invBoxCox(g.norm.lsmean.eth$mean, lambda = lambda)
g.norm.lsmean.eth_t$mean = g.norm.lsmean.eth_t$mean - 1
g.norm.lsmean.eth_t$lower = invBoxCox(g.norm.lsmean.eth$lower, lambda = lambda)
g.norm.lsmean.eth_t$lower = g.norm.lsmean.eth_t$lower -1
g.norm.lsmean.eth_t$upper = invBoxCox(g.norm.lsmean.eth$upper, lambda = lambda)
g.norm.lsmean.eth_t$upper = g.norm.lsmean.eth_t$upper - 1

g.norm.lsmean.eth_t$Ethnicity = as.character(g.norm.lsmean.eth_t$Ethnicity)
g.norm.lsmean.eth_t$Ethnicity[g.norm.lsmean.eth_t$Ethnicity == "Asian"] = "Other"

g.norm.eth.ls <- ggplot(g.norm.lsmean.eth_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("G")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5), limits=c(-0.05, 1.55)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(legend.position = c(0.02, 0.98)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(g.norm.eth.ls)
ggsave("g.norm.eth.ls.jpeg", g.norm.eth.ls, height=6, width=10, dpi=500)

# plot sex effect
# g.norm.lsmean.sex = emmeans(g.norm.f.final, specs = "Frequency", by = c("age.group", "gender"), type = "response")
# g.norm.lsmean.sex = summary(g.norm.lsmean.sex)
# g.norm.lsmean.sex = dplyr::select(g.norm.lsmean.sex, -c(5:6))
# names(g.norm.lsmean.sex) = c("Frequency", "Age", "Gender", "mean", "lower", "upper")
# g.norm.lsmean.sex$Frequency = as.numeric(as.character(g.norm.lsmean.sex$Frequency))
# 
# g.norm.lsmean_t = g.norm.lsmean.sex
# g.norm.lsmean_t$mean = invBoxCox(g.norm.lsmean.sex$mean, lambda = lambda)
# g.norm.lsmean_t$mean = g.norm.lsmean_t$mean - 1
# g.norm.lsmean_t$lower = invBoxCox(g.norm.lsmean.sex$lower, lambda = lambda)
# g.norm.lsmean_t$lower = g.norm.lsmean_t$lower -1
# g.norm.lsmean_t$upper = invBoxCox(g.norm.lsmean.sex$upper, lambda = lambda)
# g.norm.lsmean_t$upper = g.norm.lsmean_t$upper - 1
# 
# g.norm.lsmean_t$Gender = as.character(g.norm.lsmean_t$Gender)
# g.norm.lsmean_t$Gender[g.norm.lsmean_t$Gender == "F"] = "Female"
# g.norm.lsmean_t$Gender[g.norm.lsmean_t$Gender == "M"] = "Male"
# g.norm.lsmean_t$Gender = as.factor(g.norm.lsmean_t$Gender)
# 
# g.norm.sex.ls <- ggplot(g.norm.lsmean_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Gender, colour=Gender, fill=Gender)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste("Normalized ", italic("G")))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.5, 1, 1.5), limits=c(0, 1.5)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
#         legend.position=c(0,1)) +
#   theme(legend.position = c(0.02, 0.98)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   facet_wrap(~Age, ncol = 2, scales="free")
# print(g.norm.sex.ls)
# ggsave("g.norm.sex.ls.jpeg", g.norm.sex.ls, height=6, width=10, dpi=500)

### B norm
b.norm.f = lmer(b.norm ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), b.norm.2.long, REML = F) 
plot(b.norm.f) 
AIC(b.norm.f)

plotNormalHistogram(b.norm.2.long$b.norm) # original

# transform b
b.norm.2.long$b.norm1 = b.norm.2.long$b.norm + 2

# log
b.norm.2.long$b.norm_log = log(b.norm.2.long$b.norm1)
b.norm.f_t.log = lmer(b.norm_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), b.norm.2.long, REML = F) 
plot(b.norm.f_t.log) 

# # cubed
# b.norm.2.long$b.norm_squared = 1/sqrt(b.norm.2.long$b.norm1)
# b.norm.f_t.squared = lmer(b.norm_squared ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), b.norm.2.long, REML = F) 
# plot(b.norm.f_t.squared) 
# 
# # box cox
# b.norm.f.pos = lmer(b.norm1 ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), b.norm.2.long, REML = F) 
# Box = boxCox(b.norm.f.pos, lambda = seq(-2, 2, 0.1))  
# # Try values -2 to 2 by 0.1 (ususally should be within -2 or 2 or there is some other issue with the model)
# 
# Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
# Cox2[1,]                                  # Display the lambda with the greatest log likelihood
# lambda = Cox2[1, "Box.x"]                 # Extract that lambda
# 
# b.norm.2.long$B_box = (b.norm.2.long$b.norm1 ^ lambda - 1)/lambda   # Transform the original data
# b.f_t.box = lmer(B_box ~ Frequency * age.group * maternal.ethnicity + gender + ear + (1 | id.res/ear.id), b.norm.2.long, REML = F) 
# plot(b.f_t.box) 

# use log transform
b.norm.f_t = lmer_alt(b.norm_log ~ Frequency * age.group * maternal.ethnicity + gender + ear + (Frequency || id.res/ear.id), b.norm.2.long, REML = F) 
plot(b.norm.f_t) 
AIC(b.norm.f_t) 
summary(b.norm.f_t)
anova(b.norm.f_t, test="Chisq")
Anova(b.norm.f_t) 

# refit model with reml = T
b.norm.f.final = update(b.norm.f_t, REML = T)
#anova(g.f.final) # should only do anova if reml = F

# post hoc plots
b.norm.lsmean.age = emmeans(b.norm.f.final, specs = "Frequency", by = "age.group", type = "response")
b.norm.lsmean.age = summary(b.norm.lsmean.age)
b.norm.lsmean.age = dplyr::select(b.norm.lsmean.age, -c(4:5))
names(b.norm.lsmean.age) = c("Frequency", "Age", "mean", "lower", "upper")
b.norm.lsmean.age$Frequency = as.numeric(as.character(b.norm.lsmean.age$Frequency))

# back transform mean and 95% CI
b.norm.lsmean.age_t = b.norm.lsmean.age
b.norm.lsmean.age_t$mean = exp(b.norm.lsmean.age$mean)
b.norm.lsmean.age_t$mean = b.norm.lsmean.age_t$mean -2
b.norm.lsmean.age_t$lower = exp(b.norm.lsmean.age$lower)
b.norm.lsmean.age_t$lower = b.norm.lsmean.age_t$lower -2
b.norm.lsmean.age_t$upper = exp(b.norm.lsmean.age$upper)
b.norm.lsmean.age_t$upper = b.norm.lsmean.age_t$upper -2

# plot
b.norm.plot.lsmean <- ggplot(b.norm.lsmean.age_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("B")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-0.5, 0, 1, 0.5, 1, 1.5), limits=c(-0.5, 1.5)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.01,0.99)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.position="none")
#print(b.norm.plot.lsmean)
#ggsave("b.norm.ls.jpeg", b.norm.plot.lsmean, height=4, width=6, dpi=500)

# plot ethnicity effect
b.norm.lsmean.eth = emmeans(b.norm.f.final, specs = "Frequency", by = c("age.group", "maternal.ethnicity"), type = "response")
b.norm.lsmean.eth = summary(b.norm.lsmean.eth)
b.norm.lsmean.eth = dplyr::select(b.norm.lsmean.eth, -c(5:6))
names(b.norm.lsmean.eth) = c("Frequency", "Age", "Ethnicity", "mean", "lower", "upper")
b.norm.lsmean.eth$Frequency = as.numeric(as.character(b.norm.lsmean.eth$Frequency))

b.norm.lsmean.eth_t = b.norm.lsmean.eth
b.norm.lsmean.eth_t$mean = exp(b.norm.lsmean.eth$mean)
b.norm.lsmean.eth_t$mean = b.norm.lsmean.eth_t$mean -2
b.norm.lsmean.eth_t$lower = exp(b.norm.lsmean.eth$lower)
b.norm.lsmean.eth_t$lower = b.norm.lsmean.eth_t$lower -2
b.norm.lsmean.eth_t$upper = exp(b.norm.lsmean.eth$upper)
b.norm.lsmean.eth_t$upper = b.norm.lsmean.eth_t$upper -2

b.norm.lsmean.eth_t$Ethnicity = as.character(b.norm.lsmean.eth_t$Ethnicity)
b.norm.lsmean.eth_t$Ethnicity[b.norm.lsmean.eth_t$Ethnicity == "Asian"] = "Other"

b.norm.eth.ls <- ggplot(b.norm.lsmean.eth_t, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Ethnicity, colour=Ethnicity, fill=Ethnicity)) +
  geom_ribbon(linetype=0 ,alpha = 0.3) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("B")["n"]))) +
  scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-0.5, 0, 1, 0.5, 1, 1.5), limits=c(-0.5, 1.5)) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(legend.position = c(0.02, 0.98)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  facet_wrap(~Age, ncol = 2, scales="free")
#print(b.norm.eth.ls)
ggsave("b.norm.eth.ls.jpeg", b.norm.eth.ls, height=6, width=10, dpi=500)

## multiplots 
# A, Y, Y norm
ls.plot.ay <- plot_grid(abs.plot.lsmean, mag.plot.lsmean, mag.norm.plot.lsmean, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 
ggsave("ls.plots.abs.mag.jpeg", ls.plot.ay, height=9, width=6, dpi=500)

# G and B norm
ls.plot.gb <- plot_grid(g.norm.plot.lsmean, b.norm.plot.lsmean, nrow=2, ncol=1, align = "v", labels = c("A", "B")) 
ggsave("ls.plots.GB.jpeg", ls.plot.gb, height=6, width=6, dpi=500)


###################################
# # then convert rect lsmeans to polar and plot
# # didn't work
# #mag = sqrt g^2 + b^2
# #pha = arctan g/b (or should it be b/g ?)

# mag.lsmean.age = g.lsmean.age_t
# mag.lsmean.age$mean = sqrt(g.lsmean.age_t$mean^2 + b.lsmean.age$mean^2)
# mag.lsmean.age$lower = sqrt(g.lsmean.age_t$lower^2 + b.lsmean.age$lower^2)
# mag.lsmean.age$upper = sqrt(g.lsmean.age_t$upper^2 + b.lsmean.age$upper^2)
# # 
# pha.lsmean.age = b.lsmean.age
# # # 180/pi * to convert to degrees
# pha.lsmean.age$mean = 180 / pi * atan2(b.lsmean.age$mean, g.lsmean.age_t$mean) # atan2(y, x) gives phase angle for x + iy (so susceptance should be first argument)
# pha.lsmean.age$lower = 180 / pi * atan2(b.lsmean.age$lower, g.lsmean.age_t$lower)
# pha.lsmean.age$upper = 180 / pi * atan2(b.lsmean.age$upper, g.lsmean.age_t$upper)
# 
# could try plotting the 1/2 octave raw data to compare with these plots
# mag.plot.lsmean <- ggplot(mag.lsmean.age, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.01,0.99)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
# print(mag.plot.lsmean) # something wrong
# 
# pha.plot.lsmean <- ggplot(pha.lsmean.age, aes(x=Frequency, y=mean, ymin=lower, ymax=upper, group=Age, colour=Age, fill=Age)) +
#   geom_ribbon(linetype=0 ,alpha = 0.3) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000), limits=c(250,8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-60, -30, 0, 30, 60, 90), limits=c(-60, 90)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.04,0.23)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
# print(pha.plot.lsmean)

# 2 1/2 oct to compare the mag
# mag.2.temp = mag.2.full
# mag.2.temp = dplyr::select(mag.2.temp, -c(1:10,22,23))
# mag.2.temp <- group_by(mag.2.temp, age.group)
# mag.median.2 <- summarise_all(mag.2.temp, funs(median))
# mag.median.long.2 <- gather(mag.median.2, Frequency, magnitude, 2:12)
# mag.median.long.2$Frequency = as.numeric(mag.median.long.2$Frequency)
# 
# mag.2.plot <- ggplot(mag.median.long.2, aes(x=Frequency, y=magnitude, group=age.group, colour=age.group)) +
#   geom_line()  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste("|", italic("Y"), "|,", " mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
#         legend.position=c(0.01,0.99)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
# print(mag.2.plot) # it is different - sth wrong with the ls mag conversion

############################################## COMPARE MY DATA AGAINST NORMATIVE ###################################################################
# import normative data and plot against my data
# plot 24 octave and 1/2 octave lsmeans (can compart hunter lsmeans)

#other studies data
# 9 panel columns are age, rows are abs, mag, pha
# import spreadsheets in the absorbance folder
# make the main one long 
# other ones do conversions from R as necessary
# joint together in one long df with study, age, freq, abs
# repeat for mag and pha - there will be less studies (newborn will only have my study raw data 1/24 and 1/2 oct lsmeans)

# abs
# newborn need:
# aithal 2013
# aithal 2014
# hunter 2010 
# hunter 2016
# merchant 2010
# sanford 2009
# present study 24 oct raw data (do mean would be better)
# present study lsmean 1/2 octave

# 6mth:
# aithal 2014
# hunter 2016
# keefe 1993
# sanford and feeney 2008
# shahnaz 2014
# werner 2010
# present study raw data 24 oct (do mean, and change in other figure)
# present study lsmean 1/2 octave

# 12, 18, 24 mth:
# hunter 2016 (12 mth)
# keefe 1993 (12 and 24)
# present study (12 and 18 mth) - 24 octave raw data and 1/2 octave ls mean
# note: didn't use hunter 2008 because of wide age ranges of groups 

abs.studies <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/absorbance.csv", row.names=NULL)
colnames(abs.studies) = c("Study", "Age", "250",	"281",	"300",	"350",	"364",	"396.85",	"458",	"500",	"578",	"600",	"700",	"728",	"800",
                          "917",	"1000", "1155",	"1250", "1400",	"1455",	"1500",	"1834",	"2000",	"2311",	"2500",	"2911",	"3000",	"3668", "4000",	
                          "4622",	"5000",	"5823",	"6000",	"7336",	"8000")

#use tidyr to reshape into long form - gather function
abs.studies.long <- gather(abs.studies, Frequency, Absorbance, 3:36)
#abs.long$Frequency <- sub(".", "", abs.long$Frequency)
abs.studies.long$Frequency = as.numeric(abs.studies.long$Frequency)
abs.studies.long$Study = as.character(abs.studies.long$Study)
abs.studies.long$Study[abs.studies.long$Study == "Aithal et al. 2014"] <- "Aithal et al. (2014b)"
abs.studies.long$Study[abs.studies.long$Study == "Hunter et al. 2016"] <- "Hunter et al. (2016)"
abs.studies.long$Study[abs.studies.long$Study == "Sanford & Feeney 2008"] <- "Sanford & Feeney (2008)"
abs.studies.long$Study[abs.studies.long$Study == "Werner et al. 2010"] <- "Werner et al. (2010)"
abs.studies.long$Study = as.factor(abs.studies.long$Study)

# now add the others
shahnaz <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/shahnaz 2014.csv", row.names=NULL)
colnames(shahnaz) = c("Frequency", "Absorbance", "Study", "Age")
shahnaz = shahnaz[c("Study", "Age", "Frequency", "Absorbance")]
shahnaz$Study = "Shahnaz et al. (2014)"

library(readxl)
hunter <- read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/hunter 2010.xlsx", sheet = 1)
# change reflectance% to absorbance proportion
hunter$Reflectance = hunter$ReflectancePercent/100
hunter$Absorbance = 1 - hunter$Reflectance
hunter = dplyr::select(hunter, -c(ReflectancePercent, Reflectance))
hunter$Age = "0"
hunter = hunter[c("Study", "Age", "Frequency", "Absorbance")]
hunter$Study = "Hunter et al. (2000)"

keefe <- read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/keefe 1993.xlsx", sheet = 1)
keefe$Absorbance = 1 - keefe$Reflectance
keefe = dplyr::select(keefe, -Reflectance)
keefe = keefe[c("Study", "Age", "Frequency", "Absorbance")]
keefe$Study = "Keefe et al. (1993)"

merchant <- read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/merchant 2010.xlsx", sheet = 1)
merchant$Absorbance = 1 - merchant$Reflectance
merchant = dplyr::select(merchant, -Reflectance)
merchant = merchant[c("Study", "Age", "Frequency", "Absorbance")]
merchant$Study = "Merchant et al. (2010)"

sanford <- read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/sanford 2009.xls", sheet = 1)
colnames(sanford) = c("Frequency", "Absorbance", "Study", "Age")
sanford = sanford[c("Study", "Age", "Frequency", "Absorbance")]
sanford$Study = "Sanford et al. (2009)"

keefe2000 = read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/absorbance/keefe 2000 fig 5.xlsx", sheet = 1)
colnames(keefe2000) = c("Frequency", "Reflectance", "EqVol", "G", "Age", "Study")
keefe2000$Absorbance = 1 - keefe2000$Reflectance
keefe2000 = dplyr::select(keefe2000, Frequency, Study, Age, Absorbance)
keefe2000 = keefe2000[c("Study", "Age", "Frequency", "Absorbance")]
keefe2000$Frequency = as.numeric(keefe2000$Frequency)

abs.other.studies = rbind(abs.studies.long, shahnaz, hunter, keefe, merchant, sanford, keefe2000)
abs.other.studies$Age = as.numeric(abs.other.studies$Age)

# add my data
present.raw = abs.median.long.full
present.raw$Study = "This study"
names(present.raw) = c("Age", "Frequency", "Absorbance", "Study")
present.raw = present.raw[c("Study", "Age", "Frequency", "Absorbance")]
present.raw$Age = as.numeric(present.raw$Age)
present.raw$Age[present.raw$Age == 1] = 0
present.raw$Age[present.raw$Age == 2] = 6
present.raw$Age[present.raw$Age == 3] = 12
present.raw$Age[present.raw$Age == 4] = 18

abs.other.studies = rbind.data.frame(abs.other.studies, present.raw)
abs.other.studies = na.omit(abs.other.studies)
abs.other.studies$Age = as.character(abs.other.studies$Age)
abs.other.studies$Age[abs.other.studies$Age == 0] = "Neonate"
abs.other.studies$Age[abs.other.studies$Age == 6] = "6 months"
abs.other.studies$Age[abs.other.studies$Age == 12] = "12 months"
abs.other.studies$Age[abs.other.studies$Age == 18] = "18 to 24 months"
abs.other.studies$Age[abs.other.studies$Age == 24] = "18 to 24 months"
abs.other.studies$Age = factor(abs.other.studies$Age, levels = c("Neonate", "6 months", "12 months", "18 to 24 months"))

# facet 
abs.other.studies.plot <- ggplot(abs.other.studies, aes(x=Frequency, y=Absorbance, group=Study, colour=Study)) +
  geom_line() + # size = 0.8
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic("A")))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000), limits=c(226,8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(-0.05, 1.05)) +
  #scale_color_manual(values=c("chocolate2", "gold3", "palegreen3", "deepskyblue", "orchid2", "blue")) +
  scale_colour_discrete(drop=TRUE, limits = levels(abs.other.studies$Study), breaks = levels(abs.other.studies$Study)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(legend.title=element_blank()) + #, legend.justification=c(1,0), legend.position=c(0,1)) +
  #theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  #ggtitle("Neonate") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Age, ncol = 2, scales="free", drop = T)
abs.other.studies.plot
ggsave("abs.other.studies.jpeg", abs.other.studies.plot, height=6, width=12, dpi=500)

# mag
# 6mth - werner, keefe
# 12-24 mths - keefe
mag.other.6 <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/magnitude/six month mag.csv", row.names=NULL)
keefe.6 = dplyr::select(mag.other.6, 1:2)
keefe.6 = na.omit(keefe.6)
keefe.6$Study = "Keefe et al. (1993)"
keefe.6$Age = 6
colnames(keefe.6) = c("Frequency", "Magnitude", "Study", "Age")
keefe.6 = keefe.6[c("Study", "Age", "Frequency", "Magnitude")]

werner.6 = dplyr::select(mag.other.6, 1,3)
werner.6 = na.omit(werner.6)
werner.6$Study = "Werner et al. (2010)"
werner.6$Age = 6
colnames(werner.6) = c("Frequency", "Magnitude", "Study", "Age")
werner.6 = werner.6[c("Study", "Age", "Frequency", "Magnitude")]

keefe.mag.12.24 <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/magnitude/keefe 12 24 mth mag.csv", row.names=NULL)
keefe.mag.12.24 = keefe.mag.12.24[c("Study", "Age", "Frequency", "Magnitude")]

keefe.mag.2000 = read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/magnitude/keefe 2000 fig 5.xlsx", sheet = 1)
colnames(keefe.mag.2000) = c("Frequency", "Reflectance", "EqVol", "G", "Age", "Study")
keefe.mag.2000 = dplyr::select(keefe.mag.2000, -Reflectance)
keefe.mag.2000$Frequency = as.numeric(keefe.mag.2000$Frequency)
keefe.mag.2000$B = 2 * pi * keefe.mag.2000$Frequency * keefe.mag.2000$EqVol / (rho * c^2) * 1000
keefe.mag.2000$Magnitude = sqrt(keefe.mag.2000$G^2 + keefe.mag.2000$B^2)
keefe.mag.2000$Phase = 180 / pi * atan2(keefe.mag.2000$B, keefe.mag.2000$G)  # atan2(y, x) gives phase angle for x + iy (so susceptance should be first argument)
keefe.pha.2000 = keefe.mag.2000
keefe.mag.2000 = dplyr::select(keefe.mag.2000, Frequency, Age, Study, Magnitude)
keefe.mag.2000 = keefe.mag.2000[c("Study", "Age", "Frequency", "Magnitude")]

sanford.mag.2009 = read_excel("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/magnitude/Sanford 2009.xlsx", sheet = 2) ## day 2 data
sanford.mag.2009 = dplyr::select(sanford.mag.2009, c(1, 9, 15))
names(sanford.mag.2009) = c("Frequency", "Magnitude", "Phase")
sanford.mag.2009$Age = 0
sanford.mag.2009$Frequency = sanford.mag.2009$Frequency * 1000  # was in kHz
sanford.mag.2009$Study = "Sanford et al. 2009"
sanford.pha.2009 = sanford.mag.2009
sanford.mag.2009 = dplyr::select(sanford.mag.2009, -Phase)
sanford.mag.2009 = sanford.mag.2009[c("Study", "Age", "Frequency", "Magnitude")]
sanford.pha.2009 = dplyr::select(sanford.pha.2009, -Magnitude)
sanford.pha.2009 = sanford.pha.2009[c("Study", "Age", "Frequency", "Phase")]
sanford.pha.2009$Phase = sanford.pha.2009$Phase * 360 # convert cycles to degrees

# add my data
present.raw = mag.median.long.full
present.raw$Study = "This study"
names(present.raw) = c("Age", "Frequency", "Magnitude", "Study")
present.raw = present.raw[c("Study", "Age", "Frequency", "Magnitude")]
present.raw$Age = as.numeric(present.raw$Age)
present.raw$Age[present.raw$Age == 1] = 0
present.raw$Age[present.raw$Age == 2] = 6
present.raw$Age[present.raw$Age == 3] = 12
present.raw$Age[present.raw$Age == 4] = 18

mag.other.studies = rbind(keefe.6, werner.6, keefe.mag.12.24, present.raw, keefe.mag.2000, sanford.mag.2009)
mag.other.studies = na.omit(mag.other.studies)
mag.other.studies$Age = as.character(mag.other.studies$Age)
mag.other.studies$Age[mag.other.studies$Age == 0] = "Neonate"
mag.other.studies$Age[mag.other.studies$Age == 6] = "6 months"
mag.other.studies$Age[mag.other.studies$Age == 12] = "12 months"
mag.other.studies$Age[mag.other.studies$Age == 18] = "18 to 24 months"
mag.other.studies$Age[mag.other.studies$Age == 24] = "18 to 24 months"
mag.other.studies$Age = factor(mag.other.studies$Age, levels = c("Neonate", "6 months", "12 months", "18 to 24 months"))

# facet 
mag.other.studies.plot <- ggplot(mag.other.studies, aes(x=Frequency, y=Magnitude, group=Study, colour=Study)) +
  geom_line() + # size = 0.8
  xlab("Frequency, Hz") +
  ylab(expression(paste("|", italic("Y"), "|"["t"], ", mmho"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000), limits=c(226,8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 2, 4, 6, 8, 10, 12), limits=c(0, 12)) +
  #scale_color_manual(values=c("chocolate2", "gold3", "palegreen3", "deepskyblue", "orchid2", "blue")) +
  #scale_colour_discrete(drop=TRUE, limits = levels(abs.other.studies$Study), breaks = levels(mag.other.studies$Study)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(legend.title=element_blank()) + #, legend.justification=c(1,0), legend.position=c(0,1)) +
  #theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  #ggtitle("Neonate") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Age, ncol = 2, scales="free", drop = T)
mag.other.studies.plot
ggsave("mag.other.studies.jpeg", mag.other.studies.plot, height=6, width=12, dpi=500)

# pha
keefe.pha.2000 = dplyr::select(keefe.pha.2000, Frequency, Age, Study, Phase)
keefe.pha.2000 = keefe.pha.2000[c("Study", "Age", "Frequency", "Phase")]

pha.other.6 <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/phase/six month pha.csv", row.names=NULL)
keefe.6 = dplyr::select(pha.other.6, 1:2)
keefe.6 = na.omit(keefe.6)
keefe.6$Study = "Keefe et al. (1993)"
keefe.6$Age = 6
colnames(keefe.6) = c("Frequency", "Phase", "Study", "Age")
keefe.6 = keefe.6[c("Study", "Age", "Frequency", "Phase")]

werner.6 = dplyr::select(pha.other.6, 1,3)
werner.6 = na.omit(werner.6)
werner.6$Study = "Werner et al. (2010)"
werner.6$Age = 6
colnames(werner.6) = c("Frequency", "Phase", "Study", "Age")
werner.6 = werner.6[c("Study", "Age", "Frequency", "Phase")]

keefe.pha.12.24 <- read.csv("/Users/Joshua/Dropbox/R/RProjects/WidebandAbsorbance/DataSets and scripts/developmental wai/other studies data/phase/keefe 12 24 mth pha.csv", row.names=NULL)
keefe.pha.12.24 = keefe.pha.12.24[c("Study", "Age", "Frequency", "Phase")]

# add my data
present.raw = pha.median.long.full
present.raw$Study = "This study"
names(present.raw) = c("Age", "Frequency", "Phase", "Study")
present.raw = present.raw[c("Study", "Age", "Frequency", "Phase")]
present.raw$Age = as.numeric(present.raw$Age)
present.raw$Age[present.raw$Age == 1] = 0
present.raw$Age[present.raw$Age == 2] = 6
present.raw$Age[present.raw$Age == 3] = 12
present.raw$Age[present.raw$Age == 4] = 18

pha.other.studies = rbind(keefe.6, werner.6, keefe.pha.12.24, present.raw, keefe.pha.2000, sanford.pha.2009)
pha.other.studies = na.omit(pha.other.studies)
pha.other.studies$Age = as.character(pha.other.studies$Age)
pha.other.studies$Age[pha.other.studies$Age == 0] = "Neonate"
pha.other.studies$Age[pha.other.studies$Age == 6] = "6 months"
pha.other.studies$Age[pha.other.studies$Age == 12] = "12 months"
pha.other.studies$Age[pha.other.studies$Age == 18] = "18 to 24 months"
pha.other.studies$Age[pha.other.studies$Age == 24] = "18 to 24 months"
pha.other.studies$Age = factor(pha.other.studies$Age, levels = c("Neonate", "6 months", "12 months", "18 to 24 months"))

# facet 
pha.other.studies.plot <- ggplot(pha.other.studies, aes(x=Frequency, y=Phase, group=Study, colour=Study)) +
  geom_line() + # size = 0.8
  xlab("Frequency, Hz") +
  ylab(expression(paste(italic(phi1[italic("Y")])~", degrees"))) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000), limits=c(226,8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(-90, -60, -30, 0, 30, 60, 90), limits=c(-95, 95)) +
  #scale_color_manual(values=c("chocolate2", "gold3", "palegreen3", "deepskyblue", "orchid2", "blue")) +
  #scale_colour_discrete(drop=TRUE, limits = levels(abs.other.studies$Study), breaks = levels(pha.other.studies$Study)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(legend.title=element_blank()) + #, legend.justification=c(1,0), legend.position=c(0,1)) +
  #theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  #ggtitle("Neonate") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Age, ncol = 2, scales="free", drop = T)
pha.other.studies.plot
ggsave("pha.other.studies.jpeg", pha.other.studies.plot, height=6, width=12, dpi=500)




