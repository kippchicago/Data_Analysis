kipp_map<-Historical.MAP.2014.07.21 %>%
  dplyr::mutate(met_tiered=as.numeric(str_replace(met_tiered, "%", ""))/100,
                met_typical=as.numeric(str_replace(met_typical, "%", ""))/100,
                n_met_tiered=round(n_count*met_tiered),
                n_met_typical=round(n_count*met_typical))




cps_map<-NWEAreportSchool.2014.growth %>%
  rename_("n_tested"="X..Tested",
          "pct_met"="X..Students.Making.National.Average.Growth") %>%
  mutate(pct_met=pct_met/100,
         n_met=round(pct_met*n_tested),
         over_70=pct_met>=.7) 
