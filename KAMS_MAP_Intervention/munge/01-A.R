# Preporcessing Script for KAMS Intervention Analysis 


#Need to want replace results stored as strings (i.e. "18%", "-23%") as numeric (i.e, 18, 23).  Need to loop through columns names and run gsub to remove punctuation.


anet.math.1112<-PrepANet(anet.math.1112)
anet.read.1112<-PrepANet(anet.read.1112)

anet.math.1112<-PrepANet(anet.math.1213)
anet.read.1112<-PrepANet(anet.read.1213)

########################
#  Get MAP Data Ready ##
########################

map.1112<-PrepMAP(map.1112, "Fall11", "Spring12")
map.1213<-PrepMAP(map.1213, "Fall12", "Spring13")

# Separate Math.
map.math.1112<-map.1112[Subject=="Mathematics"]
map.math.1213<-map.1213[Subject=="Mathematics"]

# Separarte Reading
map.read.1112<-map.1112[Subject=="Reading"]
map.read.1213<-map.1213[Subject=="Reading"]

# Set key for merge
setkey(map.math.1112, ID)
setkey(map.math.1213, ID)
setkey(map.read.1112, ID)
setkey(map.read.1213, ID)

# Merge Data ANet and MAP Data by student ID and subject.

# 1112 Math
map.anet.math.1112<-map.math.1112[anet.math.1112][,list(ID, StudentFirstName,  StudentLastName, FirstName, LastName, Fall11_Grade, Fall11_RIT, Fall11_Pctl, Spring12_RIT, Spring12_Pctl, AVERAGE_School, DIFFERENCE_Network, Interim_1_School, Interim_1_Network, Interim_2_School, Interim_2_Network, Interim_3_School, Interim_3_Network)]

# 1112 Reading
map.anet.read.1112<-map.read.1112[anet.read.1112][,list(ID, StudentFirstName,  StudentLastName, FirstName, LastName, Fall11_Grade, Fall11_RIT, Fall11_Pctl, Spring12_RIT, Spring12_Pctl, AVERAGE_School, DIFFERENCE_Network, Interim_1_School, Interim_1_Network, Interim_2_School, Interim_2_Network, Interim_3_School, Interim_3_Network)]

# 1213 Math
map.anet.math.1213<-map.math.1213[anet.math.1112][,list(ID, StudentFirstName,  StudentLastName, FirstName, LastName, Fall12_Grade, Fall12_RIT, Fall12_Pctl, Spring13_RIT, Spring13_Pctl, AVERAGE_School, DIFFERENCE_Network, Interim_1_School, Interim_1_Network, Interim_2_School, Interim_2_Network, Interim_3_School, Interim_3_Network)]

# 1213 Reading
map.anet.read.1213<-map.read.1213[anet.read.1213][,list(ID, StudentFirstName,  StudentLastName, FirstName, LastName, Fall12_Grade, Fall12_RIT, Fall12_Pctl, Spring13_RIT, Spring13_Pctl, AVERAGE_School, DIFFERENCE_Network, Interim_1_School, Interim_1_Network, Interim_2_School, Interim_2_Network, Interim_3_School, Interim_3_Network)]
