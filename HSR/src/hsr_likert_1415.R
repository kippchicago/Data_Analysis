require(ProjectTemplate)
load.project()




hsr_teach<-hsr %>% filter(role=="Teacher")



p<-list()
p$mcs<-plot_lickert_2(hsr_teach, "Motivation, Commitment, and Satisfaction") +
  ggtitle("Teacher Responses\nMotivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert_2(hsr_teach, c("Values and Expectations",
                                       "Results orientation"))

p$c_sap<-plot_lickert_2(hsr_teach, c("Curriculum",
                                       "Student academic preparedness"))

p$spm_srr_se<-plot_lickert_2(hsr_teach, c("Student academic preparedness",
                                            "Staff recruitment and retention",
                                            "School Environment"))

p$inspl_instrl_dl<-plot_lickert_2(hsr_teach, c("Inspirational Leadership",
                                                 "Instructional Leadership",
                                                 "Distributed leadership"))

p$is_ip<-plot_lickert_2(hsr_teach, c("Instructional Strategies",
                                       "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert_2(hsr_teach, c("Student Behavior Management",
                                                "Leadership bench depth and development",
                                                "Family engagement",
                                                "Diversity and inclusivity"))



pdf("graphs/hsr_teachers_1415.pdf", height=11, width=8.5)
p
dev.off()

hsr_staff<-hsr %>% filter(role=="Staff")

p<-list()
p$mcs<-plot_lickert_2(hsr_staff, "Motivation, Commitment, and Satisfaction") +
  ggtitle("Staff (APs, DOO, OMs, Americorps) Responses\nMotivation, Commitment, and Satisfaction")

p$ve<-plot_lickert_2(hsr_staff, c("Values and Expectations"))

p$re<-plot_lickert_2(hsr_staff, c("Results orientation"))

p$sbm<-plot_lickert_2(hsr_staff, c("Student Behavior Management",
                                   "School Environment"))

p$spm<-plot_lickert_2(hsr_staff, c("Staff performance management")) +
  theme(strip.text.x=element_text(size=4),
        axis.text.y=element_text(size=4))

p$spm_srr<-plot_lickert_2(hsr_staff, c("Staff recruitment and retention"))

p$inspl_instrl_dl<-plot_lickert_2(hsr_staff, c("Inspirational Leadership",
                                               "Leadership bench depth and development",
                                               "Distributed leadership"))

p$sbm_lbdd_fe_di<-plot_lickert_2(hsr_staff, c("Communications and stakeholder management",
                                              "Family engagement",
                                              "Diversity and inclusivity"))



pdf("graphs/hsr_staff_1415.pdf", height=11, width=8.5)
p
dev.off()

# Parents ####
hsr_parent<-hsr %>% filter(role=="Parent")


p<-list()


p$mcs<-plot_lickert_2(hsr_parent, "Motivation, Commitment, and Satisfaction") +
  ggtitle("Parent Responses\nMotivation, Commitment, and Satisfaction")

p$ve<-plot_lickert_2(hsr_parent, c("Values and Expectations",
                                   "School Environment"))

p$se<-plot_lickert_2(hsr_parent, c(
                                   "Family engagement",
                                   "Diversity and inclusivity"     ))

p$sabm<-plot_lickert_2(hsr_parent, c( "Student academic preparedness" ,
                                     "Student Behavior Management" ))


p$sbm_lbdd_fe_di<-plot_lickert_2(hsr_parent, c("Communications and stakeholder management",
                                                                   "Family engagement",
                                                                   "Diversity and inclusivity",
                                               "Staff recruitment and retention"))



pdf("graphs/hsr_parents_1415.pdf", height=11, width=8.5)
 p
dev.off()
