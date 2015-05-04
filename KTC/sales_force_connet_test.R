username <- "eleman@kippchicago.org"
password <- "lemKIPP5qz6gY31wUAJGPmn88a10EiGz"
instanceURL <- "https://na8.salesforce.com/"
apiVersion <- "25.0"
session <- rforcecom.login(username, password, instanceURL, apiVersion)

soqlQuery <- "SELECT Id, Name, Address FROM Account" # this deosn't work

x<-rforcecom.query(session, soqlQuery)

objectName <- "SchoolForce__Student__c"
fields <- c("SchoolForce__Student_First_Name__c")
xx<-rforcecom.retrieve(session, objectName, fields)


stu_qry<-"
SELECT  LastName,
        FirstName,
        MailingStreet,
        MailingCity,
        MailingState,
        MailingPostalCode,
        Current_Level__c,
        Expected_HS_Graduation__c
FROM Contact
"

x<-rforcecom.query(session, stu_qry)
glimp
