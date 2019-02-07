#check for outliers

iphone1$ID <- 1:12911


camPOS <- filter(iphone1, iphonecampos > iphonecamneg & iphonesentiment == 1) 
camPOS1 <- select(camPOS, starts_with("iphone"), "ID")

camPOS1$moreCAMpos <- camPOS1$iphonecampos >= camPOS1$iphonecamneg
camPOS1$moreDISpos <- camPOS1$iphonedispos >= camPOS1$iphonedisneg
camPOS1$morePERpos <- camPOS1$iphoneperpos >= camPOS1$iphoneperneg

which(camPOS1 == FALSE)
summary(camPOS1)
campos <- camPOS1$ID 

#remove the rows from camPOS1 from iphone1
'%ni%' <- Negate('%in%')
iphone3 <- iphone1[iphone1$ID %ni% campos,]


disPOS <- filter(iphone3, iphonedispos > iphonedisneg & iphonesentiment == 1) 
disPOS1 <- select(disPOS, starts_with("iphone"), "ID")

disPOS1$moreCAMpos <- disPOS1$iphonecampos >= disPOS1$iphonecamneg
disPOS1$moreDISpos <- disPOS1$iphonedispos >= disPOS1$iphonedisneg
disPOS1$morePERpos <- disPOS1$iphoneperpos >= disPOS1$iphoneperneg

dispos <- disPOS1$ID 

#remove the rows from camPOS1 from iphone1
'%ni%' <- Negate('%in%')
iphone3 <- iphone3[iphone3$ID %ni% dispos,]


perPOS <- filter(iphone3, iphoneperpos > iphoneperneg & iphonesentiment == 1) 
perPOS1 <- select(perPOS, starts_with("iphone"), "ID")

perPOS1$moreCAMpos <- perPOS1$iphonecampos >= perPOS1$iphonecamneg
perPOS1$moreDISpos <- perPOS1$iphonedispos >= perPOS1$iphonedisneg
perPOS1$morePERpos <- perPOS1$iphoneperpos >= perPOS1$iphoneperneg

summary(perPOS1)
perpos <- perPOS1$ID 

#remove the rows from camPOS1 from iphone1
'%ni%' <- Negate('%in%')
iphone3 <- iphone3[iphone3$ID %ni% perpos,]

saveRDS(iphone3, "iphone3")


#check for negative
camNEG <- filter(iphone3, iphonecampos < iphonecamneg & iphonesentiment == 2) 
camNEG1 <- select(camNEG, starts_with("iphone"), "ID")

camNEG1$moreCAMpos <- camNEG1$iphonecampos >= camNEG1$iphonecamneg
camNEG1$moreDISpos <- camNEG1$iphonedispos >= camNEG1$iphonedisneg
camNEG1$morePERpos <- camNEG1$iphoneperpos >= camNEG1$iphoneperneg

which(camNEG1 == FALSE)
summary(camNEG1)
camneg <- camNEG1$ID 

