
# Libraries
library(ggplot2)
library(gridExtra)

# Prepare dataset
#####

# Original dataset
data <- read.csv("Qualtrics_data_2020.07.13.csv", header=TRUE, na.strings=c("", "NA", "<NA>"))

# List of non-conditional items
d2 <- data[, c(14:18, 20:53, 58:60, 62, 64, 69:71, 82, 87:121, 124:149, 162:226, 234:272)]

# Missing data in non-conditional items
data$Missing_cells <- rowSums(is.na(d2))
data$Proportion_missing_data <- rowSums(is.na(d2))/213
data$Include_over90data <- ifelse(data$Proportion_missing_data < 0.2, "Included", "Excluded")

# Clean age variable
data$Demo_2_year_cleaned <- data$Demo_2_year # copy columm to new variable

data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "03 9 1945")] <- "1945"
data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "1948./to")] <- "1948"
data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "around 1959")] <- "1959"
data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "i939")] <- "1939"
data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "Nineteenhundredsixtysix")] <- "1966"
data$Demo_2_year_cleaned[grep(x = data$Demo_2_year_cleaned, pattern = "Powell River")] <- NA

data$Demo_2_year_cleaned <- as.numeric(as.character(data$Demo_2_year_cleaned))

# New variable: age at study
data$Age <- 2019 - data$Demo_2_year_cleaned

# Relabelled gender
data$Sex <- ifelse(data$Demo_1_gender == 1, "Male", "Female")

# Relabelled work
data$Demo_3_employ.f <- ifelse(data$Demo_3_employ == 1, "Working", 
                               ifelse(data$Demo_3_employ == 2, "Retired", NA))
data$Demo_3_employ.f <- factor(data$Demo_3_employ.f, levels = c("Working", "Retired"))

# Re-coding: Subjective cognitive impairment
data$SCI_worse <- ifelse(data$SCI_1 == 1, "Yes", 
                         ifelse(data$SCI_1 == 2, "No", NA))
data$SCI_worse <- factor(data$SCI_worse, levels = c("No", "Yes"))

data$SCI_worry <- ifelse(data$SCI_2 == 1, "Yes", 
                         ifelse(data$SCI_2 == 2, "No", NA))
data$SCI_worry <- factor(data$SCI_worry, levels = c("No", "Yes"))

data$SCI_compare <- ifelse(data$SCI_3 == 1, "Yes", 
                         ifelse(data$SCI_3 == 2, "No", NA))
data$SCI_compare <- factor(data$SCI_compare, levels = c("No", "Yes"))

# Re-coding: Mobility needs
data$Mobility_needs <- ifelse(data$Mobility_1 == 1, "Yes", 
                              ifelse(data$Mobility_1 == 2, "No", NA))


# Composite scores

# Physical co-morbidities: total count
data$Comorb_count <- rowSums(data[, c(22:52)] == 1, na.rm=TRUE)

# SSQ 15-item: mean of all items, and three subscales
data$SSQ15i_avg <- rowMeans(data[, c(87:101)], na.rm=TRUE)
data$SSQ15i_avg[which(is.nan(data$SSQ15i_avg))] <- NA #transform NaN to NA
# Qualities subscale
data$SSQ15i_qualities <- rowMeans(data[, c(87:91)], na.rm=TRUE)
data$SSQ15i_qualities[which(is.nan(data$SSQ15i_qualities))] <- NA
# Spatial subscale
data$SSQ15i_spatial <- rowMeans(data[, c(92:96)], na.rm=TRUE)
data$SSQ15i_spatial[which(is.nan(data$SSQ15i_spatial))] <- NA
# Speech subscale
data$SSQ15i_speech <- rowMeans(data[, c(97:101)], na.rm=TRUE)
data$SSQ15i_speech[which(is.nan(data$SSQ15i_speech))] <- NA

# Social Isolation Measure (SIM): sum
data$SIM_total <- rowSums(data[, c(102:106)], na.rm=FALSE)

# Emo-CheQ four items: sum
data$Emocheq4_total <- rowSums(data[, c(107:110)], na.rm=FALSE)

# HHIE-S: total, and two subscales
data$HHIES_total <- rowSums(data[, c(111:120)], na.rm=FALSE)
data$HHIES_emo <- rowSums(data[, c(111, 112, 114, 117, 119)], na.rm=FALSE)
data$HHIES_soc <- rowSums(data[, c(113, 115, 116, 118, 120)], na.rm=FALSE) 

# Cognitive Self-Report Questionnaire (CSRQ): reverse-coded variables so that a high score means better cognition; original response '6' ("does not apply") recoded to NA
cog <- data.frame(case = as.numeric(c(1:527))) # Create new temp dataset

cog$CSRQ_1_recoded <- 6 - data$CSRQ_1 #better
cog$CSRQ_1_recoded[cog$CSRQ_1_recoded == 0] <- NA

cog$CSRQ_2_recoded <- data$CSRQ_2
cog$CSRQ_2_recoded[cog$CSRQ_2_recoded == 6] <- NA

cog$CSRQ_3_recoded <- 6 - data$CSRQ_3 #better
cog$CSRQ_3_recoded[cog$CSRQ_3_recoded == 0] <- NA

cog$CSRQ_4_recoded <- data$CSRQ_4
cog$CSRQ_4_recoded[cog$CSRQ_4_recoded == 6] <- NA

cog$CSRQ_5_recoded <- 6 - data$CSRQ_5 #better
cog$CSRQ_5_recoded[cog$CSRQ_5_recoded == 0] <- NA

cog$CSRQ_6_recoded <- 6 - data$CSRQ_6 #better
cog$CSRQ_6_recoded[cog$CSRQ_6_recoded == 6] <- NA

cog$CSRQ_7_recoded <- data$CSRQ_7
cog$CSRQ_7_recoded[cog$CSRQ_7_recoded == 6] <- NA

cog$CSRQ_8_recoded <- 6 - data$CSRQ_8 #better
cog$CSRQ_8_recoded[cog$CSRQ_8_recoded == 0] <- NA

cog$CSRQ_9_recoded <- data$CSRQ_9
cog$CSRQ_9_recoded[cog$CSRQ_9_recoded == 6] <- NA

cog$CSRQ_10_recoded <- data$CSRQ_10
cog$CSRQ_10_recoded[cog$CSRQ_10_recoded == 6] <- NA

cog$CSRQ_11_recoded <- 6 - data$CSRQ_11 #better
cog$CSRQ_11_recoded[cog$CSRQ_11_recoded == 0] <- NA

cog$CSRQ_12_recoded <- data$CSRQ_12
cog$CSRQ_12_recoded[cog$CSRQ_12_recoded == 6] <- NA

cog$CSRQ_13_recoded <- data$CSRQ_13
cog$CSRQ_13_recoded[cog$CSRQ_13_recoded == 6] <- NA

cog$CSRQ_14_recoded <- 6 - data$CSRQ_14 #better
cog$CSRQ_14_recoded[cog$CSRQ_14_recoded == 0] <- NA

cog$CSRQ_15_recoded <- 6 - data$CSRQ_15 #better
cog$CSRQ_15_recoded[cog$CSRQ_15_recoded == 0] <- NA

cog$CSRQ_16_recoded <- data$CSRQ_16
cog$CSRQ_16_recoded[cog$CSRQ_16_recoded == 6] <- NA

cog$CSRQ_17_recoded <- data$CSRQ_17
cog$CSRQ_17_recoded[cog$CSRQ_17_recoded == 6] <- NA

cog$CSRQ_18_recoded <- 6 - data$CSRQ_18 #better
cog$CSRQ_18_recoded[cog$CSRQ_18_recoded == 0] <- NA

cog$CSRQ_19_recoded <- data$CSRQ_19
cog$CSRQ_19_recoded[cog$CSRQ_19_recoded == 6] <- NA

cog$CSRQ_20_recoded <- 6 - data$CSRQ_20 #better
cog$CSRQ_20_recoded[cog$CSRQ_20_recoded == 0] <- NA

cog$CSRQ_21_recoded <- 6 - data$CSRQ_21 #better
cog$CSRQ_21_recoded[cog$CSRQ_21_recoded == 0] <- NA

cog$CSRQ_22_recoded <- data$CSRQ_22
cog$CSRQ_22_recoded[cog$CSRQ_22_recoded == 6] <- NA

cog$CSRQ_23_recoded <- data$CSRQ_23
cog$CSRQ_23_recoded[cog$CSRQ_23_recoded == 6] <- NA

cog$CSRQ_24_recoded <- data$CSRQ_24
cog$CSRQ_24_recoded[cog$CSRQ_24_recoded == 6] <- NA

cog$CSRQ_25_recoded <- 6 - data$CSRQ_25 #better
cog$CSRQ_25_recoded[cog$CSRQ_25_recoded == 0] <- NA

cog <- cog[, -1]
cog$CSRQ_total <- rowSums(cog[, c(1:25)], na.rm=FALSE)

# CSRQ: sum; transfer final summed variable to official dataset 
data$CSRQ_total <- cog$CSRQ_total

# Activities-Specific Balance Confidence Scale: mean; high is good balance
data$ABC_average <- rowMeans(data[, c(162:177)], na.rm=TRUE)

# Satisfaction with Life Scale (SWLS): mean; low is poor satisfaction
data$SWLS_average <- rowMeans(data[, c(178:182)], na.rm=TRUE)

# WHOQOL-BREF: following instructions in Table 3 of manual 
who <- data[, c(183:208)] # Create new temp dataset

#summary(who) # Check responses of each variable between 1 to 5

who$WHO_3_pai_med_enj_me_1 <- 6 - who$WHO_3_pai_med_enj_me_1 # Reverse-code Q3
who$WHO_3_pai_med_enj_me_2 <- 6 - who$WHO_3_pai_med_enj_me_2 # Reverse-code Q4
who$WHO_8_neg_feelings_1 <- 6 - who$WHO_8_neg_feelings_1 # Reverse-code Q26

# Keep Q1 and Q2 separate
who$WHOQOL_Q1_qol <- data$WHO_1_rate_qol_1 
who$WHOQOL_Q2_health <- data$WHO_2_health_satisf_1

# Compute domain scores; individual domains set to NA if too many NAs.
who$WHOQOL_D1 <- ifelse(rowSums(is.na(who[, c(3, 4, 10, 15, 16, 17, 18)])) <= 1, 
  rowMeans(who[, c(3, 4, 10, 15, 16, 17, 18)], na.rm=TRUE) * 4, NA)
who$WHOQOL_D2 <- ifelse(rowSums(is.na(who[, c(5, 6, 7, 11, 19, 26)])) <= 1, 
  rowMeans(who[, c(5, 6, 7, 11, 19, 26)], na.rm=TRUE) * 4, NA)
who$WHOQOL_D3 <- ifelse(rowSums(is.na(who[, c(20, 21, 22)])) <= 1, 
  rowMeans(who[, c(20, 21, 22)], na.rm=TRUE) * 4, NA)
who$WHOQOL_D4 <- ifelse(rowSums(is.na(who[, c(8,9,12,13,14,23,24,25)])) <= 2, 
  rowMeans(who[, c(8,9,12,13,14,23,24,25)], na.rm=TRUE) * 4, NA)

# At least 21 of 26 questions must be answered, otherwise entire row will be set to NA; this happens to already be the case, but add these steps to make sure
who$WHOQOL_Q1_qol[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
who$WHOQOL_Q2_health[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
who$WHOQOL_D1[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
who$WHOQOL_D2[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
who$WHOQOL_D3[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
who$WHOQOL_D4[rowSums(is.na(who[, c(1:26)])) > 5] <- NA
# checking into which cases have how many NAs
#table(rowSums(is.na(who[, 1:26])))
#which(rowSums(is.na(who[, 1:26])) == 2)
#cbind(c(1:26), t(who[323,1:26])) #D3 should be set to NA; correct

# Check min and max of each domain are within 4 to 20
#summary(who$WHOQOL_D1)
#summary(who$WHOQOL_D2)
#summary(who$WHOQOL_D3)
#summary(who$WHOQOL_D4)

# Copy temp variables to official dataset
data$WHOQOL_Q1_qol <- who$WHOQOL_Q1_qol   
data$WHOQOL_Q2_health <- who$WHOQOL_Q2_health
data$WHOQOL_D1 <- who$WHOQOL_D1
data$WHOQOL_D2 <- who$WHOQOL_D2
data$WHOQOL_D3 <- who$WHOQOL_D3
data$WHOQOL_D4 <- who$WHOQOL_D4

# Patient Health Questionnaire (PHQ): sum
data$PHQ4_total <- rowSums(data[, c(209:212)], na.rm=TRUE)

# Loneliness
# "Participants were classified as lonely if they responded “some of the time” (1 to 2 days), “occasionally” (3 to 4 days), or “all of the time” (5 to 7 days). Those who responded “rarely or never” (<1 day) were considered not lonely." (Mick et al, 2018)

data$Lonely <- rep(NA, 527)
data$Lonely[data$CLSA_lonely == 1] <- 0
data$Lonely[data$CLSA_lonely > 1] <- 1
# labelled
data$Lonely.f <- ifelse(data$Lonely == 0, "No", 
                              ifelse(data$Lonely == 1, "Yes", NA))


# Social Network Index

# "Participants scored 1 point if they were married or in a domestic partnership. They also received 1 point (each) if they had interpersonal contact at least every 1 to 2 weeks (during the past year) with children, other close family members, friends, neighbours, work colleagues, schoolmates, fellow volunteers, members of nonreligious community groups, and members of religious groups." (Mick et al, 2018)

data$SNI_total <- rep(NA, 527)
  
sni <- data.frame(case = c(1:527)) # Create new temp dataset

sni$Partner <- ifelse(data$Demo_6_marital == 1, 1, 
                  ifelse(data$Demo_6_marital > 1, 0, NA)) 
#table(sni$Partner, data$Demo_6_marital, useNA="always")

sni$Children <- ifelse(data$Social_SNI_1_1 == 1, 1,
                  ifelse(data$Social_SNI_1_1 == 2, 1, 
                   ifelse(data$Social_SNI_1_1 == 7, 1,  
                    ifelse(data$Social_SNI_1_1 > 2 & data$Social_SNI_1_1 < 7, 0, NA))))
#table(sni$Children, data$Social_SNI_1_1, useNA="always")

sni$Siblings <- ifelse(data$Social_SNI_1_2 == 1, 1,
                  ifelse(data$Social_SNI_1_2 == 2, 1, 
                   ifelse(data$Social_SNI_1_2 == 7, 1,  
                    ifelse(data$Social_SNI_1_2 > 2 & data$Social_SNI_1_2 < 7, 0, NA))))
#table(sni$Siblings, data$Social_SNI_1_2, useNA="always")

sni$Relatives <- ifelse(data$Social_SNI_1_3 == 1, 1,
                  ifelse(data$Social_SNI_1_3 == 2, 1, 
                   ifelse(data$Social_SNI_1_3 == 7, 1,  
                    ifelse(data$Social_SNI_1_3 > 2 & data$Social_SNI_1_3 < 7, 0, NA))))
#table(sni$Relatives, data$Social_SNI_1_3, useNA="always")

sni$Siblings[is.na(sni$Siblings)] <- 0 #collapse Siblings and Relatives
sni$Relatives[is.na(sni$Relatives)] <- 0
sni$CloseRelatives <- ifelse(sni$Siblings == 1 & sni$Relatives == 1, 1,
                      ifelse(sni$Siblings == 1 & sni$Relatives == 0, 1, 
                      ifelse(sni$Siblings == 0 & sni$Relatives == 1, 1,
                      ifelse(sni$Siblings == 0 & sni$Relatives == 0, 0, NA))))
#head(sni[, c('Siblings', 'Relatives', 'CloseRelatives')], 50L)

sni$Friends <- ifelse(data$Social_SNI_1_4 == 1, 1,
                  ifelse(data$Social_SNI_1_4 == 2, 1, 
                    ifelse(data$Social_SNI_1_4 > 2, 0, NA)))
#table(sni$Friends, data$Social_SNI_1_4, useNA="always")

sni$Neighbours <- ifelse(data$Social_SNI_2_1 == 1, 1,
                    ifelse(data$Social_SNI_2_1 == 2, 1, 
                     ifelse(data$Social_SNI_2_1 > 2, 0, NA)))
#table(sni$Neighbours, data$Social_SNI_2_1, useNA="always")

sni$Colleagues <- ifelse(data$Demo_3_employ == 1, 1,
                    ifelse(data$Demo_3_employ == 2, 0, NA))
#table(sni$Colleagues, data$Demo_3_employ, useNA="always")

sni$Schoolmates <- ifelse(data$Social_participate_4 == 1, 1,
                    ifelse(data$Social_participate_4 == 2, 1, 
                     ifelse(data$Social_participate_4 > 2, 0, NA)))
#Educational and cultural activities 
# table(sni$Schoolmates, data$Social_participate_4, useNA="always")

sni$Volunteers <- ifelse(data$Social_participate_7 == 1, 1,
                    ifelse(data$Social_participate_7 == 2, 1, 
                     ifelse(data$Social_participate_7 > 2, 0, NA)))
#Volunteer or charity work
#table(sni$Volunteers, data$Social_participate_7, useNA="always")

sni$Community <- ifelse(data$Social_participate_6 == 1, 1,
                    ifelse(data$Social_participate_6 == 2, 1, 
                     ifelse(data$Social_participate_6 > 2, 0, NA)))
#Neighbourhood, community or professional association 
#table(sni$Community, data$Social_participate_6, useNA="always")

sni$Religious <- ifelse(data$Social_participate_2 == 1, 1,
                    ifelse(data$Social_participate_2 == 2, 1, 
                     ifelse(data$Social_participate_2 > 2, 0, NA)))
#Church or religious activities 
#table(sni$Religious, data$Social_participate_2, useNA="always")

sni$SNI_total <- rowSums(sni[, c("Partner", "Children", "CloseRelatives", "Friends", "Neighbours", "Colleagues", "Schoolmates", "Volunteers", "Community", "Religious")], na.rm=TRUE)
#table(sni$SNI_total, useNA='always')

data$SNI_total <- sni$SNI_total

# Social Participation (CCHS / CLSA)

# "Social participation was measured using 8 items developed for the Canadian Community Health Survey. A composite scale comprising a combination of responses to the social participation items has not been validated, so we classified individuals as having low social participation if they did not participate in any social activities at least once per week. Activities included family or friendship activities outside of the household, church or religious activities, sports or physical activities with others, educational or cultural activities with others, service club activities, community or professional association activities, volunteer work, or any other recreational activity involving other people." (Mick et al, 2018)

# If at least one Social Participation variable has response 1 or 2, coded as '1'; if responses are 3 or greater only, then coded 0; if all are NA's, coded NA
data$Participation <- rep(0, 527)
data$Participation[apply(X = data[, 219:226], MARGIN = 1, FUN = function(r) any(r %in% c(1, 2)))] <- 1
data$Participation[rowSums(is.na(data[, 219:226])) == 8] <- NA
# Make into factor
data$Participation.f <- ifelse(data$Participation == 0, "Low", 
                               ifelse(data$Participation == 1, "High", NA))

# General Relationship Satisfaction: average
rs <- data.frame(case = c(1:527)) # Create new temp dataset
rs[, 2:8] <- data[, 227:233] # Copy variables into temp dataset
rs$Relation_satisf_2_1 <- 6 - rs$Relation_satisf_2_1 # Reverse-coded 4th item
rs$Relation_satisf_5_1 <- 6 - rs$Relation_satisf_5_1 # Reverse-coded 7th item
rs$RelSatisf_avg <- rowMeans(rs[, c(2:8)], na.rm=TRUE)
rs$RelSatisf_avg[is.nan(rs$RelSatisf_avg) == TRUE] <- NA
data$RelSatisf_avg <- rs$RelSatisf_avg
#table(rowSums(is.na(rs[, 2:8]) == TRUE)) 
# recall this scale is only filled out if in a permanent relationship; about 1 in 3 did not fill this out

# MOS Social Support Survey

# 4 domains of social support (emotional or informational, tangible, affectionate, and positive interactions)
# "For the overall score and the score for each domain, participants were categorized as having low availability of social support if their scores were below the median." (Mick et al, 2018)
# not clear from Mick (2018) or Sherbourne (1991) if the mean or sum should be taken; I chose the mean
# item 13 was not in the original Sherbourne (1991) set; I classed it under POS
# item 20 (pet) was not in the original Sherbourne (1991) set; I left it out

data$SocSuppMOS_overall <- rowMeans(data[, c(234:252)], na.rm=TRUE)
data$SocSuppMOS_overall[is.nan(data$SocSuppMOS_overall) == TRUE] <- NA

data$SocSuppMOS_emoinfo <- rowMeans(data[, c('Social_MOS_2', 'Social_MOS_8', 'Social_MOS_15', 'Social_MOS_18', 'Social_MOS_3', 'Social_MOS_7', 'Social_MOS_12', 'Social_MOS_16')], na.rm=TRUE)
data$SocSuppMOS_emoinfo[is.nan(data$SocSuppMOS_emoinfo) == TRUE] <- NA

data$SocSuppMOS_tangible <- rowMeans(data[, c('Social_MOS_1', 'Social_MOS_4', 'Social_MOS_11', 'Social_MOS_14')], na.rm=TRUE)
data$SocSuppMOS_tangible[is.nan(data$SocSuppMOS_tangible) == TRUE] <- NA

data$SocSuppMOS_affection <- rowMeans(data[, c('Social_MOS_5', 'Social_MOS_9', 'Social_MOS_19')], na.rm=TRUE)
data$SocSuppMOS_affection[is.nan(data$SocSuppMOS_affection) == TRUE] <- NA

data$SocSuppMOS_positive <- rowMeans(data[, c('Social_MOS_6', 'Social_MOS_10', 'Social_MOS_17', 'Social_MOS_13')], na.rm=TRUE)
data$SocSuppMOS_positive[is.nan(data$SocSuppMOS_positive) == TRUE] <- NA

# Calculate PTA4 left, right, and better ear
data$PTA4_left <- rowMeans(data[, c('LE500c', 'LE1000c', 'LE2000c', 'LE4000c')])
data$PTA4_right <- rowMeans(data[, c('RE500c', 'RE1000c', 'RE2000c', 'RE4000c')])
data$PTA4_better_ear <- pmin(data$PTA4_left, data$PTA4_right)
data$PTA4_worse_ear <- pmax(data$PTA4_left, data$PTA4_right)

# Under subjective hearing,  "Which aids do you use?" has multiple answers per cell
Subj_hear_4_list_temp <- paste0(data$Subj_hear_4_list, ",") # temp variable; add a comma to the end of each response, to make single digit answers available for matching procedure below
data$Use_HA <- NA
data$Use_HA[grep(pattern = "^1,", x = Subj_hear_4_list_temp)] <- 1
rm(Subj_hear_4_list_temp)
#data.frame(data$Subj_hear_4_list, Subj_hear_4_list_comma, Use_HA)

# Participant group: "NH" is PTA4 better ear ≤ 25; "HL" is > 25; "HA" is defined by an answer of "1" or "1,..." on the item Subj_hear_4_list.
data$Group <- rep("tbd", times = 527)
data$Group[data$PTA4_better_ear <= 25.0] <- "NH"
data$Group[data$PTA4_better_ear > 25.0 & is.na(data$Use_HA) == TRUE] <- "HL"
data$Group[data$Use_HA == 1] <- "HA"
data$Group[data$Group == "tbd"] <- "Unknown"
# Set factor levels
data$Group <- factor(data$Group, levels = c("NH", "HL", "HA", "Unknown"))

# Clean up
rm(list = c('d2', 'cog', 'rs', 'sni', 'who'))

# Those with fairly complete data
#data.comp <- data.frame(data[data$Include_over90data == "Included", ], stringsAsFactors=FALSE)

# Exclude an additional 56 for unknown hearing
data.known <- data.frame(data[data$Group != "Unknown" 
                         & data$Include_over90data == "Included", ], stringsAsFactors=FALSE)
# drop "Unknown" level from group variable
data.known$Group <- factor(data.known$Group, levels = c("NH", "HL", "HA"))

#####

# Figure: Missing data
#####
# Proportion missing cells, by group and by time spent on survey  
g1 <- (ggplot(data, aes(x = Group, y = Proportion_missing_data)) + 
  geom_jitter(alpha = 0.3, width = 0.3) + 
  geom_boxplot(outlier.shape=NA, lwd = 0.8, fatten = 1.4) + 
  scale_y_continuous(name = "Proportion of missing cells", limits=c(-0.05,1.05), breaks=seq(0,1,0.1)) + 
  theme_bw() +   
  theme(panel.background = element_rect(colour = "black", size = 1)) +    
  theme(axis.title = element_text(size = 20), 
    axis.text = element_text(colour = "black", size = 18)) )
  
g2 <- (ggplot(data, aes(x = Duration.in.seconds/60, y = Proportion_missing_data)) + 
  geom_point(alpha = 0.3) + 
  scale_x_continuous(name = "Time spent on survey (min)", limits = c(0, 125)) + 
  scale_y_continuous(name = "Proportion of missing cells", limits=c(-0.05,1.05), breaks=seq(0,1,0.1)) + 
  theme_bw() +   
  theme(panel.background = element_rect(colour = "black", size = 1)) +  
  theme(axis.title = element_text(size = 20), 
    axis.text = element_text(colour = "black", size = 18)) )

grid.arrange(g1, g2, ncol = 2, nrow = 1)  
#####

# Figure: Age
#####
ggplot(data = data.known, aes(x = Group, y = Age, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(width = 0.1, alpha = 0.7) + 
  facet_grid(~ Sex) + 
  coord_flip(ylim=c(50, 100), xlim=c(1, 3)) + 
  theme_bw() +
  theme(strip.background = element_rect(color = "black", size = 1, fill="white")) + 
  theme(panel.spacing = unit(0.5, "cm")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(legend.position = "none")

#####

# Figures: Hearing
#####
ggplot(data = data.known, 
       aes(x = PTA4_better_ear, y = PTA4_worse_ear, colour = Group, shape = Group)) + 
  geom_point(size = 3, alpha = 0.7) + 
  scale_colour_manual(name = "Group", values = c("dodgerblue2", "firebrick2", "forestgreen")) +
  scale_shape_manual(name = "Group", values = c(16, 17, 15)) + 
  xlab("PTA better ear (dB HL)") +
  ylab("PTA worse ear (dB HL)") +
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(legend.position = c(0.85, 0.8), 
        legend.box.background = element_rect(color = "grey20", size = 0.5))

# SSQ
ssq <- ( ggplot(data = data.known, aes(x = Group, y = SSQ15i_avg, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) +   
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("SSQ-15i average score") + 
  coord_flip(ylim=c(0, 10), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) +  
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")  )

# HHIE-S
hhie <- ( ggplot(data = data.known, aes(x = Group, y = HHIES_total, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) +   
  geom_jitter(height = 0.2, width = 0.2, alpha = 0.7) + 
  ylab("HHIE-S total score") + 
  coord_flip(ylim=c(0, 40), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")  )

grid.arrange(ssq, hhie, nrow = 1, ncol = 2, widths = c(0.52, 0.48))
#####

# Figures: Cognition
#####
sci1 <- ( ggplot(data = data.known, aes(x = SCI_worse, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 0.7)) + 
  xlab("Response") +
  ylab("Percentage of group") + 
  ggtitle('Is your memory/thinking becoming worse?') + 
  theme_bw() + 
  theme(strip.background = element_rect(color = "black", size = 1, fill = "white")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) +   
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) )

sci2 <- ( ggplot(data = data.known, aes(x = SCI_worry, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 0.7)) + 
  xlab("Response") +
  ggtitle('If yes, are you worried?') + 
  theme_bw() + 
  theme(strip.background = element_rect(color = "black", size = 1, fill="grey80")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) +   
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())  )

sci3 <- ( ggplot(data = data.known, aes(x = SCI_compare, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + 
  xlab("Response") +
  ggtitle('Is your memory/thinking worse than others your age?') + 
  theme_bw() + 
  theme(strip.background = element_rect(color = "black", size = 1, fill="white")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) +   
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5)) + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) )

grid.arrange(sci1, sci2, sci3, widths = c(0.4, 0.3, 0.3), nrow = 1, ncol = 3)

# CSRQ
ggplot(data = data.known, aes(x = Group, y = CSRQ_total, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Cognitive Self Report (total score)") + 
  coord_flip(ylim=c(50, 130), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")
#####

# Chronic illness
#####
# Use trimmed data to compare number of chronic illnesses
comorb <- data.known[, c('Comorb_count', 'Group')]
comorb.t <- comorb[comorb$Comorb_count <= 7, ]

anova(lm(Comorb_count ~ Group, data = comorb.t))

# NH vs HL
t.test(comorb.t[comorb.t$Group != "HA", ]$Comorb_count ~ comorb.t[comorb.t$Group != "HA", ]$Group)

# HL vs HA
t.test(comorb.t[comorb.t$Group != "NH", ]$Comorb_count ~ comorb.t[comorb.t$Group != "NH", ]$Group)

# NA vs HA
t.test(comorb.t[comorb.t$Group != "HL", ]$Comorb_count ~ comorb.t[comorb.t$Group != "HL", ]$Group)
#####

# Figures: Chronic illness
#####
# Make count of chronic illnesses into factor variable for plotting
data.known$Comorb_count.f <- factor(data.known$Comorb_count)

# Plot counts of participants by number of chronic illnesses, coded by group
ggplot(data = data.known, aes(x = Comorb_count.f, fill = Group)) +
  geom_bar() + 
  scale_fill_manual(values = c("dodgerblue", "firebrick", "darkgreen")) + 
  xlab("Number of chronic health problems") +
  ylab("Number of participants") + 
  coord_cartesian(xlim=c(0, 15), ylim=c(0, 100)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(legend.position = c(0.9, 0.85), 
        legend.box.background = element_rect(color = "grey20", size = 0.5))
#####

# Table: Chronic illness
#####
# Re-name variables
colnames(data.known)[22:52] <- c('cataracts', 'eye', 'allergies', 'asthma', 'lung', 'diabetes', 'hypothyroid', 'hyperthyroid', 'cancer', 'heart', 'vascular', 'highbp', 'stroke', 'kidney', 'urinary', 'ulcers', 'bowel.disorder', 'bowel.incont', 'osteoarth', 'rheumatoid', 'arthritis', 'osteoporosis', 'back', 'memory', 'dementia', 'parkinson', 'ms', 'epilepsy', 'migraine', 'anxiety', 'depression')

# Group into broader categories of chronic illnesses
data.known$Presence_vision <- ifelse(data.known$cataracts == 1 | data.known$eye == 1, 1, 0)
data.known$Presence_cardio <- ifelse(data.known$heart == 1 | data.known$vascular == 1 | data.known$highbp == 1 | data.known$stroke == 1, 1, 0)
data.known$Presence_arthritis <- ifelse(data.known$rheumatoid == 1 | data.known$arthritis == 1, 1, 0)
data.known$Presence_diabetes <- ifelse(data.known$diabetes == 1, 1, 0)
data.known$Presence_mobility <- ifelse(data.known$osteoarth == 1 | data.known$back == 1 | data.known$osteoporosis == 1, 1, 0)
data.known$Presence_memory <- ifelse(data.known$memory == 1 | data.known$dementia == 1, 1, 0)
data.known$Presence_neuro <- ifelse(data.known$parkinson == 1 | data.known$ms == 1 | data.known$epilepsy == 1 | data.known$migraine == 1, 1, 0)
data.known$Presence_psych <- ifelse(data.known$anxiety == 1 | data.known$depression == 1, 1, 0)
data.known$Presence_allergy <- ifelse(data.known$allergies == 1, 1, 0)
data.known$Presence_lung <- ifelse(data.known$asthma == 1 | data.known$lung == 1, 1, 0)
data.known$Presence_thyroid <- ifelse(data.known$hypothyroid == 1 | data.known$hyperthyroid == 1, 1, 0)
data.known$Presence_urinary <- ifelse(data.known$kidney == 1 | data.known$urinary == 1, 1, 0)
data.known$Presence_gastro <- ifelse(data.known$ulcers == 1 | data.known$bowel.disorder == 1 | data.known$bowel.incont == 1, 1, 0)
data.known$Presence_cancer <- ifelse(data.known$cancer == 1, 1, 0)

NH <- subset(data.known, Group == "NH")
HL <- subset(data.known, Group == "HL")
HA <- subset(data.known, Group == "HA")

# Table with percentages, by group
round(data.frame( 
  NH = colSums(NH[, c('Presence_diabetes', 'Presence_cardio', 'Presence_lung', 'Presence_allergy', 'Presence_vision',  'Presence_arthritis',  'Presence_mobility', 'Presence_memory', 'Presence_neuro', 'Presence_psych', 'Presence_thyroid', 'Presence_urinary', 'Presence_gastro', 'Presence_cancer')] == 1, na.rm=TRUE) / nrow(NH) * 100, 
  HL = colSums(HL[, c('Presence_diabetes', 'Presence_cardio', 'Presence_lung', 'Presence_allergy', 'Presence_vision',  'Presence_arthritis',  'Presence_mobility', 'Presence_memory', 'Presence_neuro', 'Presence_psych', 'Presence_thyroid', 'Presence_urinary', 'Presence_gastro', 'Presence_cancer')] == 1, na.rm=TRUE) / nrow(HL) * 100, 
  HA = colSums(HA[, c('Presence_diabetes', 'Presence_cardio', 'Presence_lung', 'Presence_allergy', 'Presence_vision',  'Presence_arthritis',  'Presence_mobility', 'Presence_memory', 'Presence_neuro', 'Presence_psych', 'Presence_thyroid', 'Presence_urinary', 'Presence_gastro', 'Presence_cancer')] == 1, na.rm=TRUE) / nrow(HA) * 100), digits=1)
#####

# Figures: Mobility, balance
#####
ggplot(data = data.known, aes(x = Mobility_needs, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + 
  xlab("Response") +
  ylab("Percentage of group") + 
  ggtitle('Do you have special mobility needs?') + 
  theme_bw() + 
  theme(strip.background = element_rect(color = "black", size = 1, fill="white")) + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5))

ggplot(data = data.known, aes(x = Group, y = ABC_average, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Activities-Specific Balance Confidence Scale (average score)") + 
  coord_flip(ylim=c(0, 100), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")
#####

# Figures: Psychological health
#####
ggplot(data = data.known, aes(x = Group, y = PHQ4_total, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Anxiety/Depression PHQ4 (total score)") + 
  annotate("text", x=0.5, y=1.5, label = "Low", size = 7, col = "dodgerblue") +
  annotate("text", x=0.5, y=17.5, label = "High", size = 7, col = "darkblue") +  
  coord_flip(ylim=c(0, 20), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")

ggplot(data = data.known, aes(x = Group, y = SWLS_average, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) +
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Satisfaction With Life Scale (average)") + 
  coord_flip(ylim=c(0, 8), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")

ggplot(data = data.known, aes(x = Lonely.f, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + 
  xlab("Lonely") +
  ylab("Percentage of group") + 
  theme_bw() + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(strip.background = element_rect(color = "black", size = 1, fill="white")) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5))
#####

# Figures: Social
#####
# Social isolation SIM
ggplot(data = data.known, aes(x = Group, y = SIM_total, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Hearing-related social isolation (SIM)") + 
  coord_flip(ylim=c(0, 50), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")

# Working status
working <- data.known[data.known$Demo_3_employ == 1, ]
retired <- data.known[data.known$Demo_3_employ == 2, ]

# Participation
p1 <- ( ggplot(data = working, aes(x = Participation.f, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + 
  xlab("Participation in social activities") +
  ylab("Percentage of group") + 
  ggtitle('Working') + 
  theme_bw() + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) +   
  theme(strip.background = element_rect(color = "black", size = 1, fill="white")) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) )

p2 <- ( ggplot(data = retired, aes(x = Participation.f, group = Group)) +
  geom_bar(aes(y = ..prop..)) + 
  facet_grid(~Group) + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y= ..prop..), 
            stat="count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1)) + 
  xlab("Participation in social activities") +
  ylab("Percentage of group") + 
  ggtitle('Retired') + 
  theme_bw() + 
  theme(panel.background = element_rect(colour = 'black', size = 1)) +   
  theme(strip.background = element_rect(color = "black", size = 1, fill="grey80")) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16), 
        text = element_text(size = 16)) + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) )

grid.arrange(p1, p2, nrow = 1, ncol = 2, widths = c(0.53, 0.47))

# Social network index
ggplot(data = data.known, aes(x = Group, y = SNI_total, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Social Network Index") + 
  scale_y_continuous(breaks = seq(0, 10, 2)) + 
  coord_flip(ylim=c(0, 10), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) +
  theme(legend.position = "none")

# Social support MOS
ggplot(data = data.known, aes(x = Group, y = SocSuppMOS_overall, fill = Group)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  scale_fill_manual(values = c("cadetblue1", "pink", "olivedrab2")) + 
  geom_jitter(height = 0.1, width = 0.2, alpha = 0.7) + 
  xlab("Group (according to better ear)") + 
  ylab("Social support (MOS average score)") + 
  coord_flip(ylim=c(1, 5), xlim=c(1, 3)) + 
  theme_bw() +
  theme(panel.background = element_rect(colour = 'black', size = 1)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(colour = "black", size = 16)) + 
  theme(legend.position = "none")

#####

# Other
#####
# "Do you have enough money to meet your needs?"
ret <- data.known[data.known$Demo_3_employ.f == "Retired", ]
describeBy(ret$WHO_5_en_bd_mn_in_ls_3, ret$Group)
# Money makes a difference for HA or not
ret.hl <- data.known[data.known$Demo_3_employ.f == "Retired" & data.known$Group != "NH", ]
t.test(ret.hl$WHO_5_en_bd_mn_in_ls_3 ~ ret.hl$Group) #t = -3.0964, p-value = 0.002488

#####
