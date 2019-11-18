separate_cdf <- function(combinded_cdf, district_name = "Not provided"){
  ar_names <- names(ex_CombinedAssessmentResults) %>% tolower
  stu_names <- names(ex_CombinedStudentsBySchool) %>% tolower

  if (!"districtname" %in% tolower(names(combinded_cdf))) {
    combinded_cdf <- combinded_cdf %>% mutate_(districtname = ~district_name)
  }

  roster<-combinded_cdf %>%
    select_(.dots = stu_names) %>%
    unique

  cdf<-combinded_cdf %>% select(-studentlastname:-studentfirstname,
                                -studentmi:-studentgender,
                                -grade) %>%
    mutate(TestID=as.character(testid))

  out <- list(cdf = cdf,
              roster = roster)

}


#  Project growth function ####

# Take a mapvizieR growth_df and grab the rit growth for spring - winter, then
# scale that growth by a multiplier and record in a vector (id, subj, growth)
# copy the winter cdf, add mulitplied growth ro Test RIT score, update term to
# spring 15-16, append to cdf, then
# output new cdf with projected season.

grow_by_factor <- function(
                    mv_obj,
                    growth_factor = 1.1,
                    neg_growth_adj = 0
                    ) {
  data("student_status_norms_2015_dense_extended")

  #neg_growth_adj <- "reported"

  if(neg_growth_adj == "reported"){
    nga <- sprintf("ifelse(
                          rit_growth>0,
                          ceiling(rit_growth*%s),
                          reported_growth)",
                   growth_factor)
  } else {
    nga <- sprintf("ifelse(
                          rit_growth>0,
                          ceiling(rit_growth*%s),
                          %s)",
                   growth_factor,
                   neg_growth_adj)
  }


  # calc new growth based on spring-winter growth
  new_growth <- mv_obj$growth_df %>%
    filter(complete_obsv,
           growth_window == "Spring to Winter") %>%
    mutate_("new_growth" = nga) %>%
    select(studentid, measurementscale, new_growth)

  # now pull winter cdf and add new growth to winter current scores, and
  # relabel as spring.  Also up date growth metrics, including percentiles.
  out <- mv_obj$cdf %>%
    filter(fallwinterspring == "Winter") %>%
    inner_join(new_growth, by = c("studentid", "measurementscale")) %>%
    mutate(testritscore = ceiling(testritscore + new_growth),
           fallwinterspring = "Spring",
           termname = sprintf("%s %s-%s",
                              fallwinterspring,
                              map_year_academic,
                              map_year_academic + 1)) %>%
    left_join(student_status_norms_2015_dense_extended %>%
                select(-school_percentile),
              by = c("measurementscale",
                     "fallwinterspring",
                     "grade",
                     "testritscore" = "RIT")) %>%
    mutate(testpercentile = student_percentile,
           consistent_percentile = student_percentile,
           testquartile = kipp_quartile(testpercentile)) %>%
    select(-new_growth, -student_percentile)

  out
}



# Growth_by sensative srpq calcs #####





estimate_sqrp_by_growth <- function(mv_obj,
                                    growth_factor = 1.25,
                                    neg_growth_adj = 0,
                                    ada=.96,
                                    mvms="WO",
                                    dqi=.99) {

  map_cdf_S15 <- mv_obj$cdf %>%
    filter(termname == "Spring 2014-2015") %>%
    left_join(
      mv_obj$roster %>%
        select(studentid, termname, studentethnicgroup, sped),
      by = c("studentid", "termname")
    )

  map_cdf_S16_projected <- grow_by_factor(mv_obj,
                                          growth_factor = growth_factor,
                                          neg_growth_adj = neg_growth_adj
  ) %>%
    left_join(mv_obj$roster %>%
                filter(termname=="Winter 2015-2016") %>%
                select(studentid, studentethnicgroup, sped),
              by="studentid")



  map_sqrp_proj <- bind_rows(map_cdf_S15, map_cdf_S16_projected)



  #map_sqrp_proj <- map_sqrp_proj %>% left_join(sped, by="studentid")



  kcs_growth <- school_growth_percentile(map_sqrp_proj)

  #calculate_attainment
  kcs_attain <- school_attainment_percentile(map_cdf_S16_projected)


  #calculate growth priority groups ####
  #AA
  kcs_growth_aa <- priority_group(kcs_growth,
                                  group_column = "studentethnicgroup",
                                  group_id = "Black or African American"
  )

  #IEP
  kcs_growth_dl <- priority_group(kcs_growth,
                                  group_column = "sped",
                                  group_id = "TRUE"
  )

  # pct m/e
  kcs_pct_me<-calc_pct_me(kcs_growth)




  schools<- c("Ascend", "Create", "Bloom")

  sqrp_results<-schools %>%
    map(~sqrp_level(.,
                    kcs_growth,
                    kcs_attain,
                    kcs_growth_aa,
                    kcs_growth_dl,
                    kcs_pct_me,
                    ada = ada,
                    mvms = mvms,
                    dqi= dqi)
    ) %>%
    bind_rows %>%
    mutate(school = schools)

  out  <-
    kcs_pct_me %>%
    ungroup %>%
    mutate(school = str_extract(school, "Ascend|Bloom|Create")) %>%
    select(school, pct_met) %>%
    inner_join(sqrp_results, by = "school") %>%
    mutate(growth_factor = growth_factor,
           neg_growth_adjst_type = as.character(neg_growth_adj))

  # return
  out
}









