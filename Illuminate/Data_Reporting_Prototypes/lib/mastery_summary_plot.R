# New Function starts here #####
mastery_summary_plot <- function(results,
                                 roster,
                                 ...) {


  results_filtered <- results %>%
    dplyr::mutate(local_student_id = as.integer(local_student_id)) %>%
    dplyr::inner_join(roster,
                      by = c("local_student_id" = "student_number")
    ) %>%
    dplyr::filter(points_possible > 0) %>% #removes extra_credit
    dplyr::filter(...)

  results_stu_obj_assm <- results_filtered %>%
    dplyr::mutate(mastered = percent_correct >= 80,
                  objective = paste(custom_code, stringr::str_wrap(description.x,
                                                                   width = 30),
                                    sep="\n")
    ) %>%
    dplyr::group_by(local_student_id,
                    school,
                    assm_name,
                    assm_week,
                    modal_date,
                    assm_type,
                    custom_code,
                    objective) %>%
    dplyr::summarize(points = sum(points),
                     points_possible = sum(points_possible),
                     percent_correct = points/points_possible*100,
                     mastered = percent_correct >= 80)

  stopifnot(nrow(results_stu_obj_assm) > 0 )

  stu_mastery <- results_stu_obj_assm %>%
    dplyr::group_by(assm_name,
                    assm_week,
                    modal_date,
                    assm_type,
                    school,
                    custom_code,
                    objective) %>%
    dplyr::summarize(n_stus = n(),
                     n_mastered = sum(mastered),
                     pct_mastered = round(n_mastered/n_stus*100),
                     avg_score = sum(points)/sum(points_possible)*100)



  stu_mastery_long <- stu_mastery %>%
    mutate(obj_wrap = stringr::str_wrap(objective, width = 40),
            obj_title = sprintf("%s\n %s", custom_code, obj_wrap)
            ) %>%
    select(modal_date, assm_type, school, custom_code, objective, obj_title, pct_mastered, avg_score) %>%
    tidyr::gather(variable, value, pct_mastered, avg_score)

  # create color scale
#   mastery_cols <- c('#ff001a',
#                     '#ffbf42',
#                     '#fefe56',
#                     '#91fd57',
#                     '#00ca3f')
#
#
#   domain_cuts <- unique(cut(0:100,
#                             right = FALSE,
#                             breaks = c(0,60,70,80,90,100),
#                             include.lowest = TRUE)
#   )
#
#
#   mastery_cols_fn <- scales::col_factor(palette = mastery_cols,
#                                         domain = domain_cuts
#   )
#
#   stu_mastery_2  <- stu_mastery %>%
#     ungroup %>%
#     mutate(assm_name = factor(assm_name,
#                               levels = assm_order$assm_name),
#            label = sprintf("%s%% mastered\n%s%% average",
#                            round(pct_mastered),
#                            round(avg_score)
#            ),
#            mastery_cat = cut(pct_mastered,right = FALSE,
#                              breaks = c(0,60,70,80,90,100),
#                              include.lowest = TRUE),
#            mastery_col = mastery_cols_fn(mastery_cat)
#
#     )
#
#   categories <- stu_mastery_2 %>%
#     dplyr::select(mastery_cat) %>%
#     dplyr::arrange(mastery_cat) %>%
#     unique %>%
#     dplyr::mutate(mastery_cat = as.character(mastery_cat))

  p <- ggplot(stu_mastery_long, aes(x=modal_date, y=value)) +
    geom_line(aes(group=school, color=school)) +
    geom_point(aes(color=school, shape=assm_type)) +
    facet_grid(obj_title~variable) +
    theme_bw() +
    theme(strip.text.y = element_text(vjust = .5, angle = 0, size = 7 ))


  p

}
