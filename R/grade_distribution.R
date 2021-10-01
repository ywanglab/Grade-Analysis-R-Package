#########################################################
# Analysis by grade distribution
########################################################

#' Barplot of grade distribution in a course compared to the department average
#'
#' @param combined_db combined data base inlcuding the course data and department average data
#' @param comp_gp a vector of group names
#' @param earliest_sem earliest semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_grade_distrib <- function(combined_db = by_course_grades_combined,
                               comp_gp = c("703", "Dept"),
                               earliest_sem = earliest_sem_in_plot) {
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  
  #label_color <- ifelse(combined_db$proportion > 20,"white","black")
  p <-  combined_db %>%
    filter(CourseNumber %in% comp_gp) %>%
    # mutate(CourseNumber=factor(CourseNumber,levels=comp_gp)) %>%
    #mutate(type=factor(type,levels=c("W","B"))) %>%
    ggplot(aes(grade, proportion, fill = CourseNumber)) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = TRUE) +
    geom_text(
      aes(label = round(proportion, 1), 
          vjust = ifelse( proportion >90, 1.4, -0.4)),
      position = position_dodge(width = 1),
      size = 2.5
      
    ) +
    ylim(0, 100) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    xlab("") +
    ylab("%") +
    facet_wrap(~ Term, ncol = 3) +
    ggtitle(paste("Grade distribution in MATH",comp_gp[1]," per semester compared to Dept avg distribution")) 
  
}

# Plot DFW curves vs. Term
#' Plot DFW distribution curves vs. Term
#'
#' @param combined_db combined database of the course and the departmetn average
#' @param comp_gp compared gorup names, a vector.
#' @param comp_grade compared grades
#' @param earliest_sem earliest semester to display in the graph
#'
#' @return
#' @export
#'
#' @examples
plot_DFW_distribution <- function (combined_db = by_course_grades_combined,
                                   comp_gp = c("703", "Dept"),
                                   comp_grade =c("D","F","W"),
                                   earliest_sem = earliest_sem_in_plot) {
  combined_db <- combined_db %>% filter(Term >= earliest_sem)
  dept_db <- combined_db %>%  
    filter(CourseNumber %in% comp_gp[2]) %>%
    filter(grade %in% c(comp_grade)) 
  
  p<- combined_db %>% 
    filter(CourseNumber %in% comp_gp[1]) %>%
    filter(grade %in% c(comp_grade)) %>%
    ggplot(aes(factor(Term), proportion,color=grade)) +
    geom_point() +
    geom_line(aes(group=grade)) +
    ylim(0,100)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("%") +
    ggtitle(paste("DFW grade distribution in MATH",comp_gp[1]," per semester (Dept avg by dotted)")) +
    
    geom_point(
      mapping = aes(factor(Term), proportion,color=grade),
      data = dept_db,
      #color = "red",
      shape = 1
    ) +
    geom_line(mapping = aes(factor(Term), proportion,color=grade, group=grade),
              data = dept_db, linetype ="dotted",
              #color = "red"
    ) 
  
  return(p)
}

