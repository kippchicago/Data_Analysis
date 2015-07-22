cut_list <- list("map_year_academic", 'school', 'grade')
call_list <- list(FALSE, FALSE, TRUE)

longt <- report_dispatcher(mapvizieR_obj = map_mv_filtered,
                                           cut_list = cut_list,
                                           call_list = call_list,
                                           func_to_call = "student_npr_history_plot",
                                           arg_list = list(
                                             "subject" = "Mathematics"
                                             )
                                           )



longt_reading <- report_dispatcher(mapvizieR_obj = map_mv,
                           cut_list = cut_list,
                           call_list = call_list,
                           func_to_call = "student_npr_history_plot",
                           arg_list = list(
                             "subject" = "Reading"
                           )
)
