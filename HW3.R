#Technique 1: Heatmap -or- Bubble Plot 
#nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
# https://flowingdata.com/2010/01/21/how-to-make-a-heatmap-a-quick-and-easy-solution/

#Technique 3: Parallel Coordinates Plot # parcoord, MASS

# ---------------------------------------- #
# LIBRARIES
# ---------------------------------------- #

library(dplyr)
library(broman)
library(ggplot2)
library(magrittr)
library(reshape2)
library(ggvis)
library(lmtest)
library(MASS)
library(car)
library(boot)
library(caret)
library(Matrix)
library(knitr)
library(stats)
library(RColorBrewer)
library(GGally)

#install.packages("GGally")

# -------------------------------------------- #
# HEATMAP
# -------------------------------------------- #
fb <- read.csv('dataset_Facebook.csv', sep = ";")
names(fb) <- c("page.total.likes",                                                   
               "Type",                                                   
               "Category",                                                  
               "Post.Month",                                                   
               "Post.Weekday",                                                    
               "Post.Hour",                                                   
               "Paid",                                                  
               "lt.post.reach",                                          
               "lt.post.impressions",                                    
               "lt.post.engaged.users",                                   
               "lt.post.consumers",                                            
               "lt.post.consumptions",                                         
               "lt.friend.impressions",       
               "lt.friend.reach",      
               "lt.friend.count",
               "comment",
               "like",                                                              
               "share",                                                              
               "total.interactions")

fb$Post.Weekday <- factor(fb$Post.Weekday)
levels(fb$Post.Weekday) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
# ---------------------------------------------- #
# HEATMAP THEME
# ---------------------------------------------- #
heat.theme <- theme(panel.background = element_rect(fill = "#ffffff", color = "grey75", size=0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.ticks       = element_blank(),
                    legend.position  = "bottom" )


# ---------------------------------------- #
# SMALL MULTIPLES INPUT DATA
# ---------------------------------------- #

dia <- read.csv('diabetic_data.csv')
dia <- dia[(dia$admission_type_id %in% c(1,2,3)),]

dia <- dia %>% 
  mutate(admission.name = factor(admission_type_id, levels=c(3,2,1), 
                                 labels=c('Elective','Urgent','Emergency'))) %>%
  filter(gender %in% c('Female', 'Male')) %>%
  filter(race %in% c('Caucasian', 'AfricanAmerican', 'Asian', 'Hispanic'))

trauma.pal <-rev(brewer.pal(3,'RdYlGn'))

# ---------------------------------------- #
# MANIPULATE DATA
# ---------------------------------------- #

df <- data.frame(group.var      = factor(),
                 admission.name = factor(),
                 admit.ct       = integer(),
                 pct.admit      = double(),
                 group.name     = character(),
                 stringsAsFactors = FALSE)

summarize_by_group <- function(x, data.in, data.out ){
  
  group.sum <- data.in %>%
    group_by_(x, 'admission.name') %>%
    summarize(admit.ct = n())        %>%
    mutate(pct.admit = (admit.ct / sum(admit.ct))*100) %>%
    arrange(-pct.admit) %>%
    ungroup()
  
  group.sum$group.name <- x
  
  names(group.sum)[1] <- "group.var"
  
  return(group.sum)
}

data.group <- summarize_by_group('change', dia, df)
df <- rbind(df, as.data.frame(data.group))

data.group <- summarize_by_group('gender', dia, df)
df <- rbind(df, as.data.frame(data.group))

data.group <- summarize_by_group('age', dia, df)
df <- rbind(df, as.data.frame(data.group))

data.group <- summarize_by_group('race', dia, df)
df <- rbind(df, as.data.frame(data.group))

# MAKE PRETTY LABELS
df$group.name <- factor(df$group.name)
levels(df$group.name) <- c("Age", "Change", "Gender", "Race")

levels(df$group.var) <- c("Yes","No","Female", "Male","Unknown/Invalid", "10", "20","30","40",        
                          "50","60","70", "80", "90","100","?","AfricanAmerican","Asian"          
                          ,"Caucasian", "Hispanic","Other")

# ---------------------------------------------- #
# SMALLMULTIPLES THEME
# ---------------------------------------------- #
facet.theme <- theme(panel.background = element_rect(fill = "#ffffff"), #, color = "grey75", size=0.5),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks       = element_blank(),
                     axis.title.x     = element_blank(),
                     axis.title.y     = element_blank(),
                     axis.text.y      = element_blank(),
                     strip.background = element_rect(fill = "#ffffff", colour = NA),
                     legend.position="bottom" )
