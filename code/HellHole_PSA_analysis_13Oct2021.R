# Processing and plotting results from laser particle size analyzer

# Levi Pedology lab
# University of Georgia

# SUMMARY
# This will open output files from laser PSA, process them to plot continuous distribution and then plot on the soil texture triangle

# Samples being analyzed: 
#  Hellhole swamp site from NSF study with Grant Snitker et al. 


# 1.0. - Load packages

library(xlsx)
library(soiltexture)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)
library(purrr)
library(aqp)
require(RColorBrewer)
library(soilDB)
library(sharpshootR)
library(Hmisc)
library(lattice)
library(MASS)

# 1.1 - set working directory
# setwd("D:/Projects/Projects/2017_LTAR_SoilQuality_Restoration/NM soil analysis/NM soil 6-20-2020")
# setwd("D:/Projects/Projects/2017_LTAR_SoilQuality_Restoration/NM soil analysis/All_NM_lpsa")
setwd("D:/Projects/Projects/NSF_Grant_Hellhole_Hampton")
getwd()

# 2.0 - Load  file

########################
##########
#
#
# Load multiple files at once according to : https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
#$ls.xls # https://stackoverflow.com/questions/4876813/using-r-to-list-all-files-with-a-specified-extension 

temp = list.files(pattern="*av.xls$")

temp[1]
read.table(temp[1],sep="\t")
(temp[1])

temp
myfiles = lapply(temp, read.table,sep="\t")

str(myfiles[1])
class((myfiles[1]))
class(myfiles[1])


# making new selctions to get the sand, silt, clay from these samples

myfiles[[1]][31 , ]
myfiles[[1]][36 , ]

myfiles[[1]][30 , 2]
myfiles[[1]][35 , 2]

clay <- as.numeric(levels(myfiles[[1]][31 , 2]))[myfiles[[1]][31 , 2]]
sand <- as.numeric(levels(myfiles[[1]][36 , 2]))[myfiles[[1]][36 , 2]]

silt <- 100 - sand - clay


### change this one to make it work on lpsa data
length(temp)
d = data.frame( CLAY=rep(0, length(temp)), SAND=rep(0,length(temp)), SILT=rep(0, length(temp)))

i<-1
nsf.text.1 <- list()
for(i in 1:length(temp)) {
  soil.id <- myfiles[[i]][4 , 2]  # This gives me the name of the file for the specific data frame in the list
  clay <- as.numeric(myfiles[[i]][31 , 2])
  sand <- as.numeric(myfiles[[i]][36 , 2])
  silt <- 100 - sand - clay
  nsf.text.1[[i]] <- c(soil.id, sand, silt, clay)
}
nsf.text.1
str(nsf.text.1)

nsf.text.1.df <- as.data.frame(matrix(unlist(nsf.text.1), ncol=4, byrow=T))
names(nsf.text.1.df ) <- c("Sample.ID", "SAND", "SILT", "CLAY" )
nsf.text.1.df$SAND <- as.numeric(nsf.text.1.df$SAND )
nsf.text.1.df$SILT <- as.numeric(nsf.text.1.df$SILT )
nsf.text.1.df$CLAY <- as.numeric(nsf.text.1.df$CLAY )


summary(nsf.text.1.df)
str(nsf.text.1.df)
  
TT.plot(tri.data=nsf.text.1.df, class.sys= "USDA.TT", col="Red", pch= 19, cex = 0.1, main = "USDA Texture - nsf", cex.lab=0.8, cex.axis=0.8, lwd.lab=0.8, arrows.show=F)

# check to see how many of each number I have
table(nsf.text.1.df$Sample.ID)
# Write xlsx

write.xlsx2(nsf.text.1.df, file="nsf_lpsa_all.xlsx", sheetName = "All")
 

# Now load the averaged psa data along with the pH and color

clean.profile <- read.xlsx(file =  "NSF_profile_info_horizons_pH_texture.xlsx" , sheetIndex = 1)
str(clean.profile)
unique(clean.profile$Value)

clean.profile$Value <- as.numeric(clean.profile$Value)
clean.profile$Chroma <- as.numeric(clean.profile$Chroma)


# Create a profile name

clean.profile$profile <- c(rep("FP", 59))

# Create a SoilProfileCollection

depths(clean.profile) <- Site ~ horizon.top + horizon.bottom
clean.profile$name <- clean.profile$Horizon

clean.profile$soil_color <- with(clean.profile, munsell2rgb(clean.profile$Hue, clean.profile$Value, clean.profile$Chroma))
depths(clean.profile) <- profile ~  hztop + hzbottom

plot(clean.profile)
plotSPC(clean.profile)
plotSPC(clean.profile[ 2, ])


library(dplyr)

d.summarize <- d %>%
  group_by(labid) %>%
  summarise_at(vars(CLAY, SAND, SILT), funs(mean(., na.rm=TRUE)))
str(d.summarize)  


TT.plot(tri.data=as.data.frame(d.summarize), class.sys= "USDA.TT", col="Red", pch= 19, cex = 0.1, main = "USDA Texture - WHFP", cex.lab=0.8, cex.axis=0.8, lwd.lab=0.8, arrows.show=F)

clean.profile@horizons <- 
  
  bind_cols(clean.profile@horizons, d.summarize)

melt2.nsf <- melt(data = clean.profile[ 2, ]@horizons[ , c(3,7, 14:16 , 12)], id = c('Site', 'horizon.top'))


# This code works to put a specified vertical line in each panel (90 percentile of each prehistoric period for each respective element)
# This cycles through the panels with the specific 90% confidence interval for each respective element/panel
xyplot(horizon.top ~ value | variable, data=melt2.nsf, ylab='Depth (cm)', main = "Hellhole",
       xlab='%',
       ylim=c(200,-2),
       panel=function(x, y, ...){
         panel.xyplot(x,y, type='l')
         #   panel.abline(v=q.90.all8[panel.number()], col = 'red')
      #   panel.abline(h=108, col = 'black', lty = 1, lwd = 2) # Ab' surface presumed pre-contact boundary
       #  panel.abline(h=175, col = 'black', lty = 4, lwd = 2) # C14 charcoal  1565 AD
       #  panel.abline(h=36, col = 'black', lty = 4, lwd = 2) # C14 charcoal from Ab 36 cm (middle of 34-38 cm horizon)
         alpha=0.25
         sync.colors=TRUE
         par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2))},
       prepanel=prepanel.depth_function,
       layout=c(4,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=5, alternating=3, relation='free'), y=list(tick.number=3, alternating=1, relation='same'))
)



#  Toying with the plotting for the full size distribution of a lpsa output

read.table(temp[1],sep="\t")

read.csv(file = temp[1] , sep="\t")


read.table(dat, header = FALSE, sep = ",", 
           col.names = paste0("V",seq_len(7)), fill = TRUE)
# From: https://stackoverflow.com/questions/18922493/how-can-you-read-a-csv-file-in-r-with-different-number-of-columns 
psd.data <-read.table(file = temp[1] ,  header = FALSE, sep = "\t", 
           col.names = paste0("V",seq_len(6)), fill = TRUE)

psd.data[45:nrow(psd.data) , ] # Narrow to the size fraction and volume

psd.data[49:nrow(psd.data) , 1:2] # Further narrow to data and then provide names
psd.data.for.plot <- psd.data[49:nrow(psd.data) , 1:2] # Further narrow to data and then provide names

names(psd.data.for.plot) <- c("Channel.Diameter.um", "Diff.vol.pct")
psd.data.for.plot$Channel.Diameter.um <- as.numeric(psd.data.for.plot$Channel.Diameter.um)
psd.data.for.plot$Diff.vol.pct <- as.numeric(psd.data.for.plot$Diff.vol.pct)
# plot continous psd distribution

p <- ggplot(data=psd.data.for.plot, mapping = aes(x = Channel.Diameter.um, y= Diff.vol.pct))
p + geom_line()

# Basic line plot with points
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line()+
  geom_point()


# For a second sample

psd.data.2 <-read.table(file = temp[50] ,  header = FALSE, sep = "\t", 
                      col.names = paste0("V",seq_len(6)), fill = TRUE)

psd.data.2.for.plot <- psd.data.2[49:nrow(psd.data.2) , 1:2] # Further narrow to data and then provide names

names(psd.data.2.for.plot) <- c("Channel.Diameter.um", "Diff.vol.pct")
psd.data.2.for.plot$Channel.Diameter.um <- as.numeric(psd.data.2.for.plot$Channel.Diameter.um)
psd.data.2.for.plot$Diff.vol.pct <- as.numeric(psd.data.2.for.plot$Diff.vol.pct)
# plot continous psd distribution

p <- ggplot(data=psd.data.2.for.plot, mapping = aes(x = Channel.Diameter.um, y= Diff.vol.pct))
p + geom_line(col="blue") +  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                           labels = trans_format("log10", math_format(10^.x)))


p + geom_line(col="blue") + annotation_logticks()  


# Basic line plot with points
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line()+
  geom_point()



# put plots together into 1

psd.data.plotting <- cbind(psd.data.for.plot, psd.data.2.for.plot$Diff.vol.pct)
names(psd.data.plotting) <- c( "Channel.Diameter.um" ,  "Sample1" ,"Sample2")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash") 

p <- ggplot(data=psd.data.plotting, mapping = aes( x= Channel.Diameter.um))
p + geom_line(aes(y = Sample1), col="blue") +
  geom_line(aes(y = Sample2), col="red")


# Look at sand size distributions
# very fine sand 50 - 100 microns
# fine sand - 100 - 250
# medium sand - 250 - 500
# coarse sand - 500 - 1000
# very coarse sand - 1000 - 2000

# fine clay < 0.2 microns
# coarse clay - 0.2-2 
# fine silt - 2-20
# coarse silt - 20-50


ed_exp5 <- select(filter(psd.data.plotting, Channel.Diameter.um  < 2),c(State,Minor.Population:Education.Expenditures))

ed_exp5 <- select(filter(education, Region == 2),c(State,Minor.Population:Education.Expenditures))

psd.data.plotting %>% dplyr::filter(Channel.Diameter.um > 50)
                      
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um < 0.2) # Fine Clay
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 0.2 & Channel.Diameter.um < 2)

psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 2 & Channel.Diameter.um < 50)
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 50 & Channel.Diameter.um < 50)

                      
testd %.%
  select(employee, salary) %.%
  filter(salary > 25000) %.%
  summarise(mean = mean(salary))

testd %.%
  select(employee, salary) %.%
  filter(salary > 25000) %.%
  summarise(mean = mean(salary))


psd.data.plotting %>% dplyr::filter(Channel.Diameter.um < 0.2) %>% summarise(fine.clay = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 0.2 & Channel.Diameter.um < 2) %>% summarise(coarse.clay = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 2 & Channel.Diameter.um < 20) %>% summarise(fine.silt = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 20 & Channel.Diameter.um < 50) %>% summarise(coarse.silt = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 50 & Channel.Diameter.um < 100) %>% summarise(very.fine.sand = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 100 & Channel.Diameter.um < 250) %>% summarise(fine.sand = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 250 & Channel.Diameter.um < 500) %>% summarise(medium.sand = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 500 & Channel.Diameter.um < 1000) %>% summarise(coarse.sand = sum(Sample1, na.rm=T))
psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 1000 & Channel.Diameter.um < 2000) %>% summarise(very.coarse.sand = sum(Sample1, na.rm=T))


fc <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um < 0.2) %>% summarise(fine.clay = sum(Sample1, na.rm=T))
cc <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 0.2 & Channel.Diameter.um < 2) %>% summarise(coarse.clay = sum(Sample1, na.rm=T))
fsi <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 2 & Channel.Diameter.um < 20) %>% summarise(fine.silt = sum(Sample1, na.rm=T))
csi <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 20 & Channel.Diameter.um < 50) %>% summarise(coarse.silt = sum(Sample1, na.rm=T))
vfsa <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 50 & Channel.Diameter.um < 100) %>% summarise(very.fine.sand = sum(Sample1, na.rm=T))
fsa <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 100 & Channel.Diameter.um < 250) %>% summarise(fine.sand = sum(Sample1, na.rm=T))
msa <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 250 & Channel.Diameter.um < 500) %>% summarise(medium.sand = sum(Sample1, na.rm=T))
csa <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 500 & Channel.Diameter.um < 1000) %>% summarise(coarse.sand = sum(Sample1, na.rm=T))
vcsa <- psd.data.plotting %>% dplyr::filter(Channel.Diameter.um >= 1000 & Channel.Diameter.um < 2000) %>% summarise(very.coarse.sand = sum(Sample1, na.rm=T))

detailed.sep <- cbind(fc, cc, fsi, csi, vfsa, fsa, msa, csa, vcsa)
sum(detailed.sep)



# Now use the for loop to calculate the detailed fractions of sand, silt, and clay

j<-2
nsf.detailed <- list()
for(j in 1:length(temp)) {
  soil.id <- myfiles[[j]][4 , 2]  # This gives me the name of the file for the specific data frame in the list
  psd.data <-read.table(file = temp[j] ,  header = FALSE, sep = "\t", 
                        col.names = paste0("V",seq_len(6)), fill = TRUE)
  
  psd.data.for.plot <- psd.data[49:nrow(psd.data) , 1:2] # Further narrow to data and then provide names
  
  names(psd.data.for.plot) <- c("Channel.Diameter.um", "Diff.vol.pct")
  psd.data.for.plot$Channel.Diameter.um <- as.numeric(psd.data.for.plot$Channel.Diameter.um)
  psd.data.for.plot$Diff.vol.pct <- as.numeric(psd.data.for.plot$Diff.vol.pct)
  fc <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um < 0.2) %>% summarise(fine.clay = sum(Diff.vol.pct, na.rm=T))
  cc <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 0.2 & Channel.Diameter.um < 2) %>% summarise(coarse.clay = sum(Diff.vol.pct, na.rm=T))
  fsi <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 2 & Channel.Diameter.um < 20) %>% summarise(fine.silt = sum(Diff.vol.pct, na.rm=T))
  csi <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 20 & Channel.Diameter.um < 50) %>% summarise(coarse.silt = sum(Diff.vol.pct, na.rm=T))
  vfsa <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 50 & Channel.Diameter.um < 100) %>% summarise(very.fine.sand = sum(Diff.vol.pct, na.rm=T))
  fsa <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 100 & Channel.Diameter.um < 250) %>% summarise(fine.sand = sum(Diff.vol.pct, na.rm=T))
  msa <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 250 & Channel.Diameter.um < 500) %>% summarise(medium.sand = sum(Diff.vol.pct, na.rm=T))
  csa <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 500 & Channel.Diameter.um < 1000) %>% summarise(coarse.sand = sum(Diff.vol.pct, na.rm=T))
  vcsa <- psd.data.for.plot %>% dplyr::filter(Channel.Diameter.um >= 1000 & Channel.Diameter.um < 2000) %>% summarise(very.coarse.sand = sum(Diff.vol.pct, na.rm=T))
  

  nsf.detailed[[j]] <- c(soil.id, fc, cc, fsi, csi, vfsa, fsa, msa, csa, vcsa)
}
nsf.detailed
str(nsf.detailed)

nsf.detailed.df <- as.data.frame(matrix(unlist(nsf.detailed), ncol=10, byrow=T))
names(nsf.detailed.df ) <- c("soil.id", "fc", "cc", "fsi", "csi", "vfsa", "fsa", "msa", "csa", "vcsa" )
head(nsf.detailed.df)
nsf.detailed.df$fc <- as.numeric(nsf.detailed.df$fc )
nsf.detailed.df$cc <- as.numeric(nsf.detailed.df$cc )
nsf.detailed.df$fsi <- as.numeric(nsf.detailed.df$fsi )
nsf.detailed.df$csi <- as.numeric(nsf.detailed.df$csi )
nsf.detailed.df$vfsa <- as.numeric(nsf.detailed.df$vfsa )
nsf.detailed.df$fsa <- as.numeric(nsf.detailed.df$fsa )
nsf.detailed.df$msa <- as.numeric(nsf.detailed.df$msa )
nsf.detailed.df$csa <- as.numeric(nsf.detailed.df$csa )
nsf.detailed.df$vcsa <- as.numeric(nsf.detailed.df$vcsa )


summary(nsf.detailed.df)
str(nsf.detailed.df)

TT.plot(tri.data=nsf.text.1.df, class.sys= "USDA.TT", col="Red", pch= 19, cex = 0.1, main = "USDA Texture - nsf", cex.lab=0.8, cex.axis=0.8, lwd.lab=0.8, arrows.show=F)

# check to see how many of each number I have
table(nsf.text.1.df$Sample.ID)
# Write xlsx

# write.xlsx2(nsf.detailed.df, file="nsf_lpsa_detailed_ssic.xlsx", sheetName = "All")

