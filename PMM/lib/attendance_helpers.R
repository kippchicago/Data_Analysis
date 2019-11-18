
week_in_sy <- function(ref_date, first_day = '2017-08-21') {
  (floor_date(ymd(ref_date), unit="week") - floor_date(ymd(first_day), unit="week"))/dweeks(1)+1
}
