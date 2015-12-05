# Regional/School Leader Roll-ups from Illuminate



setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes")
require(ProjectTemplate)
load.project()
require(illuminater)


ill<-connect_illuminate(server = "54.172.11.151",
                        database = "Illuminate_mirror",
                        user = "silo",
                        password = "silKIPP1",
                        db_type = "warehouse")



ps<-connect_illuminate(server = "54.172.11.151",
                       database = "PS_mirror",
                       user = "silo",
                       password = "silKIPP1",
                       db_type = "warehouse")

ill_obj <- illuminater(ill, ps, "students")


# Add some school names and make KAS adjustment
school_ids_names <- data_frame(schoolid = c(78102, 7810, 400146, 400163),
                               school = c("KAP", "KAMS", "KCCP", "KBCP")
)


ill_obj$roster <- ill_obj$roster %>%
  inner_join(school_ids_names, by="schoolid") %>%
  mutate(school = ifelse(school == "KBCP" & grade_level == 5,
                         "KAS",
                         school))

# build roster scaffold of max unit numbers
results_max_unit <- ill_obj$results %>%
  mutate(unit_number = as.integer(str_extract(assm_unit, "\\d+"))) %>%
  inner_join(ill_obj$roster %>%
               mutate(local_student_id = as.character(student_number)) %>%
               select(local_student_id, school_name = school),
             by="local_student_id"
  ) %>%
  filter(local_id_conforms) %>%
  group_by(school_name, assm_subj, assm_grade) %>%
  filter(unit_number==max(unit_number))


# get all units, not just max.
results_all_unit <- ill_obj$results %>%
  mutate(unit_number = as.integer(str_extract(assm_unit, "\\d+"))) %>%
  inner_join(ill_obj$roster %>%
               mutate(local_student_id = as.character(student_number)) %>%
               select(local_student_id, school_name = school),
             by="local_student_id"
  ) %>%
  filter(local_id_conforms)



roster_current <- ill_obj$roster %>%
  filter(enroll_status == 0)

roster_subj_unit_scaffold <- results_all_unit %>%
  ungroup %>%
  select(local_student_id, subj = assm_subj, grade = assm_grade, unit = assm_unit) %>%
  unique %>%
  inner_join(roster_current %>%
               mutate(local_student_id = as.character(student_number)),
             by = "local_student_id"
  )


source("lib/mastery_summary_plot.R")

#Let's begin testign this viz ####
require(purrr)

# Need to calculate modal dates
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

results_max_unit_2<-results_max_unit %>%
  group_by(assm_school, assm_type, assm_subj, assm_unit, assm_week) %>%
  mutate(modal_date = Mode(ymd_hms(updated_at)))


results_all_unit_2<-results_all_unit %>%
  group_by(assm_school, assm_type, assm_subj, assm_unit, assm_week) %>%
  mutate(modal_date = Mode(ymd_hms(updated_at)))


rosters<-roster_subj_unit_scaffold %>%
  split(list(.$subj, .$grade_level)) %>%
  discard(~nrow(.)==0)



rosters$ela.5 %>%
  mastery_summary_plot(results_all_unit_2,
                                .,
                                academic_year==2016,
                                assm_grade==unique(.$grade_level),
                                assm_subj == unique(.$subj)
  )





summaries<-rosters %>%
map(~try(mastery_summary_plot(results_all_unit_2,
                           .,
                           academic_year==2016,
                           assm_grade==unique(.$grade_level),
                           assm_subj == unique(.$subj)
                           ) +
           ggtitle(sprintf("Mastery Summary: %s %s",
                           unique(.$grade_level),
                           unique(.$subj)))
         )
)

cairo_pdf("graphs/mastery_summary_plot_test.pdf", height=11, width = 17, onefile=TRUE)
 summaries
dev.off()
