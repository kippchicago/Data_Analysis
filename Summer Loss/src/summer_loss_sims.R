# Average Summer loss ####
# Need to get average summer loss by grade and subject over the years

# subset data into two data.frames
map.springs<-map.all[Season=="Spring" & GrowthMeasureYN=="TRUE"]
map.falls<-map.all[Season=="Fall" & GrowthMeasureYN=="TRUE"]

map.springs[, MatchYear:=Year2]
map.falls[, MatchYear:=Year1]
setkey(map.springs, StudentID, MeasurementScale, MatchYear)
setkey(map.falls, StudentID, MeasurementScale, MatchYear)

map.sl<-map.springs[map.falls, nomatch=0]

norms.sl<-map.sl[MeasurementScale %in% c("Reading", "Mathematics"),
       list(MeanDiff=mean(TestRITScore.1-TestRITScore),
            SDDiff=sd(TestRITScore.1-TestRITScore)),
       by=list(MeasurementScale, Grade, Grade.1)][
         Grade.1-Grade==1][
           order(MeasurementScale, Grade)]



#to do:
# o add summer loss
# o add summer reduction factor
# o add college ready growth



norm_sim2 <- function (start.grade = 5, 
                       end.grade = 8, 
                       start.subject = "Mathematics", 
                       start.rit = 190, 
                       yearly.cycle = c(41, 12), 
                       n.samples = 5000,
                       summer.loss.norms=norms.sl,
                       summer.loss=FALSE,
                       ) 
{
  if (!exists("norms_students_2011")) 
    data(norms_students_2011)
  norms <- data.table(norms_students_2011)
  x.t <- data.table(ID = c(1:n.samples), StartGrade = rep(start.grade, 
                                                          n.samples), MeasurementScale = rep(start.subject, n.samples), 
                    StartRIT = rep(start.rit, n.samples), Season = yearly.cycle[1])
  setkey(x.t, MeasurementScale, StartGrade, StartRIT)
  setkey(norms, MeasurementScale, StartGrade, StartRIT)
  f <- function(.m, .s) {
    .m <- as.name(.m)
    .s <- as.name(.s)
    q <- substitute(list(MeasurementScale, StartGrade, StartRIT, 
                         Mean = .m, SD = .s))
    x.var[norms[, eval(q)], nomatch = 0][, round(StartRIT + 
                                                   rnorm(Mean, Mean, SD))]
  }
  for (g in c(start.grade:end.grade)) {
    s.counter <- 1
    for (s in yearly.cycle) {
      x.var <- copy(x.t[StartGrade == g & Season == s])
      .mean <- paste0("T", s)
      .sd <- paste0("S", s)
      x.var[, `:=`(StartRIT, f(.mean, .sd))]
      if (s.counter != length(yearly.cycle)) {
        s.counter <- s.counter + 1
        x.var[, `:=`(Season, yearly.cycle[s.counter])]
      }
      else {
        x.var[, `:=`(Season, yearly.cycle[1])]
        x.var[, `:=`(StartGrade, g + 1)]
      }
      x.t <- rbindlist(list(x.t, x.var))
      setkey(x.t, MeasurementScale, StartGrade, StartRIT)
    }
  }
  x.t
}
