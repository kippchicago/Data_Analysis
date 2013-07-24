ISAT.combined<-rbind(ISAT.1213.Ascend,ISAT.1213.KCCP)

ISAT.combined[Tested_School_ID==400044, School:="KAMS"]
ISAT.combined[Tested_School_ID==400146, School:="KCCP"]

ISAT.combined[, list(PctMeetsReading=sum(ReadingPerformanceLevel=="Meet")/.N,
                     PctExcReading=sum(ReadingPerformanceLevel=="Exceed")/.N,
                     PctMEReading=sum(ReadingPerformanceLevel=="Meet" | 
                                      ReadingPerformanceLevel=="Exceed"  )/.N,
                     PctMeetsMath=sum(MathPerformanceLevel=="Meet")/.N,
                     PctExcMath=sum(MathPerformanceLevel=="Exceed")/.N,
                     PctMEMath=sum(MathPerformanceLevel=="Meet" | 
                                        MathPerformanceLevel=="Exceed"  )/.N), 
              by=list(School, Grade_Level)]


#Get performance levels with old ISAT cut scores
#5th Reading
ISAT.combined[Grade_Level==5 & ReadingScaleScore %in% c(120:160), 
              AdjReadingPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==5 & ReadingScaleScore %in% c(161:214), 
              AdjReadingPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==5 & ReadingScaleScore %in% c(215:246), 
              AdjReadingPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==5 & ReadingScaleScore %in% c(247:351), 
              AdjReadingPerformanceLevel:="Exceed"]

#6th Reading
ISAT.combined[Grade_Level==6 & ReadingScaleScore %in% c(120:166), 
              AdjReadingPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==6 & ReadingScaleScore %in% c(167:219), 
              AdjReadingPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==6 & ReadingScaleScore %in% c(220:256), 
              AdjReadingPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==6 & ReadingScaleScore %in% c(257:360), 
              AdjReadingPerformanceLevel:="Exceed"]

#7th Reading
ISAT.combined[Grade_Level==7 & ReadingScaleScore %in% c(120:173), 
              AdjReadingPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==7 & ReadingScaleScore %in% c(174:225), 
              AdjReadingPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==7 & ReadingScaleScore %in% c(226:266), 
              AdjReadingPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==7 & ReadingScaleScore %in% c(267:369), 
              AdjReadingPerformanceLevel:="Exceed"]

#8th Reading
ISAT.combined[Grade_Level==8 & ReadingScaleScore %in% c(120:179), 
              AdjReadingPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==8 & ReadingScaleScore %in% c(180:230), 
              AdjReadingPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==8 & ReadingScaleScore %in% c(231:277), 
              AdjReadingPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==8 & ReadingScaleScore %in% c(278:364), 
              AdjReadingPerformanceLevel:="Exceed"]


#5th MAth
ISAT.combined[Grade_Level==5 & MathScaleScore %in% c(120:179), 
              AdjMathPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==5 & MathScaleScore %in% c(180:213), 
              AdjMathPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==5 & MathScaleScore %in% c(214:270), 
              AdjMathPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==5 & MathScaleScore %in% c(271:369), 
              AdjMathPerformanceLevel:="Exceed"]

#6th MAth
ISAT.combined[Grade_Level==6 & MathScaleScore %in% c(120:193), 
              AdjMathPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==6 & MathScaleScore %in% c(194:224), 
              AdjMathPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==6 & MathScaleScore %in% c(225:275), 
              AdjMathPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==6 & MathScaleScore %in% c(276:369), 
              AdjMathPerformanceLevel:="Exceed"]

#7th MAth
ISAT.combined[Grade_Level==7 & MathScaleScore %in% c(120:206), 
              AdjMathPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==7 & MathScaleScore %in% c(207:234), 
              AdjMathPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==7 & MathScaleScore %in% c(235:280), 
              AdjMathPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==7 & MathScaleScore %in% c(281:392), 
              AdjMathPerformanceLevel:="Exceed"]

#8th MAth
ISAT.combined[Grade_Level==8 & MathScaleScore %in% c(120:220), 
              AdjMathPerformanceLevel:="Warning"]
ISAT.combined[Grade_Level==8 & MathScaleScore %in% c(221:245), 
              AdjMathPerformanceLevel:="Below"]
ISAT.combined[Grade_Level==8 & MathScaleScore %in% c(246:287), 
              AdjMathPerformanceLevel:="Meet"]
ISAT.combined[Grade_Level==8 & MathScaleScore %in% c(288:410), 
              AdjMathPerformanceLevel:="Exceed"]


ISAT.combined[, list(PctMeetsReading=sum(AdjReadingPerformanceLevel=="Meet")/.N,
                     PctExcReading=sum(AdjReadingPerformanceLevel=="Exceed")/.N,
                     PctMEReading=sum(AdjReadingPerformanceLevel=="Meet" | 
                                        AdjReadingPerformanceLevel=="Exceed"  )/.N,
                     PctMeetsMath=sum(AdjMathPerformanceLevel=="Meet")/.N,
                     PctExcMath=sum(AdjMathPerformanceLevel=="Exceed")/.N,
                     PctMEMath=sum(AdjMathPerformanceLevel=="Meet" | 
                                     AdjMathPerformanceLevel=="Exceed"  )/.N), 
              by=list(School, Grade_Level)]

