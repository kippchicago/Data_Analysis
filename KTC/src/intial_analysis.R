# Initial analysis of KTC sales force data.

require(dplyr)
require(silounloadr)


# get alumni tables

contact <- get_alumni("contact") #%>% collect
account <- get_alumni("account") #%>% collect
enrollment_c <- get_alumni("enrollment__c")  #%>% collect()
contact_note_c <- get_alumni("contact_note__c") #%>% collect
college_persistence_c <- get_alumni("college_persistence__c")


glimpse(contact)
glimpse(account)
glimpse(enrollment_c)
glimpse(contact_note_c)
glimpse(college_persistence_c)
