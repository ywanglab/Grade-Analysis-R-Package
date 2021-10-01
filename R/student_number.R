
########################################################################

# Plot and table student numbers 
#######################################################################
#######################################################
#Define a function to plot DFW of a group of courses
#######################################################
#' plot the total number of students for a group of courses compared to department total
#'
#' @param DFW_data by course DFW dataframe
#' @param avg_data Department aggregated dataframe
#' @param grp coure numbers of the group
#' @param grp_nm 
#' @param TYPE "total" or "r_total"
#' @param earliest_sem earliest semester to display
#' @param course_comp compare courses in the group or not. Default=FALSE.
#'
#' @return
#' @export
#'
#' @examples
plot_total_by_group <- function(DFW_data=by_course_DFW, 
                                avg_data=MATH_dept_total, 
                                grp=remedial_courses, 
                                grp_nm="remedial",  # Use "DEPT" for grp_name for dept plot
                                TYPE="total",    # total or r_total
                                earliest_sem =earliest_sem_in_plot, 
                                course_comp=FALSE
                                
){
  #imfile_nm <- paste("figs/", grp_nm, "_DFW_by_term.png", sep="")
  if (TYPE=="total") {
    leading_title_word <- ""  
    ylab_word <- "total"
  } 
  else {leading_title_word <- "Running "
  ylab_word <- "r_total"
  }
  imfile_nm <- paste("figs/", grp_nm, leading_title_word, "_total_by_term.png", sep="")
  DFW_data <- DFW_data %>% filter (Term >= earliest_sem)
  avg_data <- avg_data %>% filter (Term >= earliest_sem)
  
  if (grp_nm != "DEPT" & course_comp==TRUE) {
    imfile_nm <- paste("figs/", grp_nm, leading_title_word, "_total_by_course.png", sep="")
    DFW_data <- DFW_data %>% mutate(semester= case_when(
      Term %% 100 ==1 ~ "Fall",
      Term %% 100 == 2 ~ "Spring",
      TRUE             ~ "Summer"
    ))
    DFW_data %>% #mutate(Term = factor(Term)) %>%
      # filter(SubjectCode %in% c("MATH","STAT")) %>%  #excluding ENGR
      filter(CourseNumber %in% grp) %>%
      mutate (CourseNumber = factor(CourseNumber)) %>%
      ggplot(aes(factor(Term), total,label=total))  +
      geom_point(aes(group = CourseNumber, col = CourseNumber ))+
      #  geom_text(aes(label=total),nudge_x=0.0,nudge_y=50)+
      geom_text_repel() + 
      geom_line(aes(group = CourseNumber, col = CourseNumber )) +
      ylim(0,NA)+
      xlab("") + ylab(ylab_word) +
      ggtitle(paste(leading_title_word,"total number of students in",
                    grp_nm, 
                    " courses per semester")) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~semester,ncol=1)+
      ggsave(imfile_nm,width=fig_width)
  }
  else if   (grp_nm != "DEPT" & course_comp== FALSE) {
    imfile_nm <- paste("figs/", grp_nm, leading_title_word, "_total_by_term.png", sep="")
    DFW_data <- DFW_data %>% mutate(semester= case_when(
      Term %% 100 ==1 ~ "Fall",
      Term %% 100 == 2 ~ "Spring",
      TRUE             ~ "Summer"
    ))
    DFW_data %>% #mutate(Term = factor(Term)) %>%
      # filter(SubjectCode %in% c("MATH","STAT")) %>%  #excluding ENGR
      filter(CourseNumber %in% grp) %>%
      mutate (CourseNumber = factor(CourseNumber)) %>%
      ggplot(aes(factor(Term), total,label=total))  +
      geom_point(aes(group = semester, col = semester ))+
      # geom_text(aes(label=total),nudge_x=0.0,nudge_y=80)+
      geom_text_repel() + 
      geom_line(aes(group = semester, col = semester )) +
      ylim(0,NA)+
      xlab("") + ylab(ylab_word) +
      ggtitle(paste(leading_title_word,"total number of students in",
                    grp_nm, 
                    "courses per semester")) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~CourseNumber,ncol=1)+
      ggsave(imfile_nm,width=fig_width)
  }
  
  else {
    imfile_nm <- paste("figs/", grp_nm, leading_title_word, "_total_by_term.png", sep="")
    avg_data <- avg_data %>% mutate(semester= case_when(
      Term %% 100 ==1 ~ "Fall",
      Term %% 100 == 2 ~ "Spring",
      TRUE             ~ "Summer" ))
    avg_data %>% 
      ggplot(aes(factor(Term), total, label=total))  +
      xlab("") + ylab(ylab_word) +
      ggtitle(paste(leading_title_word,"total number of students per semester in the Department"
      )) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_point(
        mapping = aes(factor(Term), total,color=semester),
        data = avg_data,
        shape = 4
      ) +
      geom_text(aes(label=total),nudge_x=0.0,nudge_y=50)+
      # geom_text_repel() + 
      geom_line(mapping = aes(1:nrow(avg_data), total, color = semester),
                data = avg_data,
                linetype ="dotted",
      )  + 
      ylim(0,NA)+
      ggsave(imfile_nm,width=fig_width)
    
    
  }
  #print(imfile_nm)
}

#Define a function to display and plot DFW of a group of courses 
#' display table and plot DFW of a group of courses 
#'
#' @param DFW_data DFW data 
#' @param avg_data Aggregated data
#' @param grp course numbers of the group
#' @param grp_name 
#' @param input_table the table to be knitted
#' @param TYPE "total" or other
#' @param earliest_sem earlisest semester to display
#' @param course_comp compare courses in the group or not. Default=FALSE.
#'
#' @return
#' @export
#'
#' @examples
display_plot_total_by_course <- function(DFW_data=by_course_DFW, 
                                         avg_data=MATH_dept_total,
                                         grp=remedial_courses,
                                         grp_name="remedial",
                                         input_table=by_course_total_table,
                                         TYPE="total", 
                                         earliest_sem =earliest_sem_in_plot, course_comp=FALSE
) {
  index_analyzed <- input_table$Course %in% 
    c( grp, TYPE)
  
  t <-   input_table[index_analyzed,]
  #view(by_course_table[index_analyzed,])
  # plot the DFW of the  group
  group <- grp
  grp_name <- grp_name
  p<-  plot_total_by_group(DFW_data=DFW_data, 
                           avg_data=avg_data,
                           grp=group, grp_nm = grp_name, TYPE=TYPE, earliest_sem,course_comp ) 
  return(list(display=t, plot=p))
}
# Knit the table

#' Knit the total table by course
#'
#' @param ls a list contains the table in ls$display
#' @param number_sem_to_display number of semester to display
#' @param TYPE "total", "DFW" or others
#'
#' @return
#' @export
#'
#' @examples
total_table_by_course <- function(ls, number_sem_to_display=number_sem_display,
                                  TYPE="total") {
  t <- ls$display
  last_col_ind <- ncol(t)
  sel_col <- c(1:2, (last_col_ind-number_sem_display):last_col_ind)
  displayed_table <- t[,sel_col]
  
  colnames(displayed_table)[1:2] <- c("Subj","Course")
  
  options(knitr.kable.NA = "**")
  if (TYPE=="total") {leading_title_word <- ""  } 
  else {leading_title_word <- "Running "}
  displayed_table %>% knitr::kable(caption=paste(leading_title_word,"total number of students by course per semester")) %>%
    kable_styling(font_size = 7)
}




####################################################################
# Analysis by Colleges and Majors: number of students
###################################################################


#' Bargraph of percentage of student numbers by college
#'
#' @param combined_db college total data frame
#' @param comp_gp selected colleges
#' @param earliest_sem earlist semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_by_college_bargraph <- function(combined_db = by_college_total,
                                     comp_gp = all_colleges,
                                     earliest_sem = earliest_sem_in_plot
                                     
) {
  label_col <- ifelse(combined_db$percent > 15, "white", "black")
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  p <-  combined_db %>%
    filter(College %in% comp_gp) %>%
    ggplot(aes(College, percent, fill = 
                 College  )) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = TRUE) +
    geom_text(
      aes(label = round(percent, 1), 
          vjust = ifelse( percent >15, 1.3, -0.4)),
      color=label_col,
      position = position_dodge(width = 1),
      size = 2.5
      
    ) +
    ylim(0, 50) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("%") +
    facet_wrap(~ Term, ncol = 3)+
    ggtitle("Percent of students taking a math department course by college") 
  return(p)
}



# 
#' Plot line graph of percentages for student numbers by college
#'
#' @param combined_db by_college_total dataframe
#' @param comp_gp selected colleges
#' @param earliest_sem earlist semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_by_college_linegraph <- function (combined_db = by_college_total,
                                       comp_gp = all_colleges,
                                       earliest_sem = earliest_sem_in_plot) {
  combined_db <- combined_db %>% filter(Term >= earliest_sem)
  
  p<- combined_db %>% 
    filter(College %in% comp_gp) %>%
    ggplot(aes(factor(Term), percent,color=College)) +
    geom_point() +
    geom_line(aes(group=College)) +
    ylim(0,50)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("%") +
    ggtitle("Percentage of students taking a math department course by college") 
  
  return(p)
}





##########################
#' Plot bargraph of total students by college
#'
#' @param combined_db by_college_total dataframe
#' @param comp_gp selected colleges
#' @param earliest_sem earliest semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_total_by_college_bargraph <- function(combined_db = by_college_total,
                                           comp_gp = all_colleges,
                                           earliest_sem = earliest_sem_in_plot
                                           
) {
  label_col <- ifelse(combined_db$total > 150, "white", "black")
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  p <-  combined_db %>%
    filter(College %in% comp_gp) %>%
    ggplot(aes(College, total, fill = 
                 College  )) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = TRUE) +
    geom_text(
      aes(label = total, 
          vjust = ifelse( total >150, 1.3, -0.4)),
      color=label_col,
      position = position_dodge(width = 1),
      size = 2.5
      
    ) +
    ylim(0, 600) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("") +
    facet_wrap(~ Term, ncol = 3)+
    ggtitle("Total number of students taking a math department course by college") 
  return(p)
}



# 
#' Plot linegraph of student total by college
#'
#' @param combined_db by_college_total dataframe
#' @param comp_gp selected colleges
#' @param earliest_sem earlist semester to dispaly
#'
#' @return
#' @export
#'
#' @examples
plot_total_by_college_linegraph <- function (combined_db = by_college_total,
                                             comp_gp = all_colleges,
                                             earliest_sem = earliest_sem_in_plot) {
  combined_db <- combined_db %>% filter(Term >= earliest_sem)
  
  p<- combined_db %>% 
    filter(College %in% comp_gp) %>%
    ggplot(aes(factor(Term), total,color=College)) +
    geom_point() +
    geom_line(aes(group=College)) +
    ylim(0,600)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("") +
    ggtitle("Total number of students taking a math department course by college") 
  
  return(p)
}


#' Bargraph of student total of top 20 Majors
#'
#' @param colleges selected colleges
#' @param term the term for which the graph is created
#' @param DFW_db by_major_total data base
#'
#' @return
#' @export
#'
#' @examples
top_Major_by_total <- function (colleges=all_colleges,
                                term =current_term, DFW_db=by_major_total) {
  # if (TYPE== "avg") {leading_word=""}
  # else {leading_word="r_"}
  DFW_db <- DFW_db %>% 
    filter (College %in% colleges)
  
  
  
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    top_n(20,total) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    mutate(type=factor(type,levels=c("B","W"))) %>%
    mutate(Major = reorder(Major, total)) %>%
    ggplot(aes(Major,total, fill=College)) +
    geom_bar(stat="identity") +
    #geom_text(aes(label = total), hjust = hjst_value,color=text_col,size=3)+
    geom_text(aes(label = total,hjust = ifelse(total>10, 1.2,-0.4),
                  color=type     )  ,size=3, show.legend=FALSE)+
    scale_color_manual(values=c("black","white"))+
    ylim(0,250)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 8)) +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))+
    ylab(paste( "Number of students (",as.character(term),") of top 20 Majors")) +
    xlab("")
  return(list(plot=p))
}

#' Plot the barplots of the past semesters for student totals by major
#'
#' @param colleges selected colleges
#' @param DFW_db by_major_total database
#'
#' @return
#' @export
#'
#' @examples
barplot_by_Major_total <- function(colleges=all_colleges, DFW_db=by_major_total){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_Major_by_total(colleges,term, DFW_db)})
  return(ls)
}



#' bargraph of top 20 majors of student numbers in a course 
#'
#' @param colleges selected colleges
#' @param term term for which the graph is generated
#' @param DFW_db input database
#' @param course course number
#'
#' @return
#' @export
#'
#' @examples
top_Major_by_course_total <- function (colleges=all_colleges,
                                       term =current_term, DFW_db=by_course_major_total, course=703) {
  # if (TYPE== "avg") {leading_word=""}
  # else {leading_word="r_"}
  DFW_db <- DFW_db %>% 
    filter (College %in% colleges) %>%
    filter (CourseNumber %in% course)
  
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    top_n(20,total) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    mutate(type=factor(type,levels=c("B","W"))) %>%
    mutate(Major = reorder(Major, total)) %>%
    ggplot(aes(Major,total, fill=College)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = total,hjust = ifelse(total<10, -0.4, 1.2),
                  color=type     )  ,size=3, show.legend=FALSE)+
    scale_color_manual(values=c("black","white"))+
    ylim(0,150)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 8)) +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))+
    ylab(paste( "Number of students (",as.character(term),") of top 20 Majors in",paste(course,collapse="-"))) +
    xlab("")
  return(list(plot=p))
}

#' Plot the barplot in the past six semesters for student totals in a course by major
#'
#' @param colleges selected colleges
#' @param DFW_db input database
#' @param course course number
#'
#' @return
#' @export
#'
#' @examples
barplot_by_course_Major_total <- function(
  colleges=all_colleges, DFW_db=by_course_major_total,course=703){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_Major_by_course_total(colleges,term, DFW_db, course)})
  return(ls)
}

