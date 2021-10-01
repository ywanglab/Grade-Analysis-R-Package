#############################################################
#          Analysis of corequsites
############################################################


#' Display  table of a group of courses
#' Calculate and display (knit)  a table of a group fo courses
#' @param df input dataframe
#' @param TYPE "avg" or "r_avg"
#' @param analyzed_courses 
#' @param num_sem_to_display number of semesters to display in the table, 16 for letter size portrait format
#' @param FORM string of the name of the table to be displayed in the table
#' @param KEY column name: "DFW", "total", etc.
#' @param running_window running window size 
#'
#' @return
#' @export
#'
#' @examples
display_table <- function (df=df_Non_STEM_w_coreq, TYPE="avg",
                           analyzed_courses=Non_STEM, 
                           num_sem_to_display = 16,
                           FORM="coreq",KEY="DFW",
                           running_window=1
){
  # calculate DFW for NON_STEM with Coreq
  by_course_Non_STEM_w_coreq <- df  %>% group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    summarize(n = n())
  
  DFW_by_course_Non_STEM_w_coreq <- running_DFW(db=by_course_Non_STEM_w_coreq,
                                                running_window = running_window)
  #view(DFW_by_course_Non_STEM_w_coreq)
  
  DFW_dept_avg_Non_STEM_w_coreq <- Dept_running_DFW(db=by_course_Non_STEM_w_coreq,
                                                    running_window = running_window)
  #view(DFW_dept_avg_Non_STEM_w_coreq)
  
  # Display the table format
  by_course_DFW_table_Non_STEM_w_coreq <- DFW_table_by_course (
    Dept_DFW=DFW_dept_avg_Non_STEM_w_coreq, 
    by_course_DFW=DFW_by_course_Non_STEM_w_coreq,
    #Analyzed_courses =Analyzed_MATH_courses,
    TYPE=TYPE,
    FORM,
    KEY=KEY
    # number_sem_to_display=number_sem_display,
  )
  #view(by_course_DFW_table_Non_STEM_w_coreq)
  #view the table
  total_terms <- length(unique(df$Term))
  avg_table(TYPE=TYPE,
            by_course_DFW_table_Non_STEM_w_coreq,
            analyzed_course = analyzed_courses ,
            num_sem_to_display= min(total_terms-1, num_sem_to_display), FORM,KEY)
  
  #return(by_course_DFW_table_Non_STEM_w_coreq)
}







#################################################
# compute combined database
#################################################
###########################################
#' Calculate combined dataframe of groups and the group average
#' Calculate combined dataframe of a group of courses and the group average
#' @param ls_db a list of all input dataframes
#' @param gp_names 
#' @param running_window default to 1, not calculating the running average
#'
#' @return
#' @export
#'
#' @examples
combined_dept_avg_db <- function(ls_db,
                                 gp_names = c("p7p8", "n7p8", "p9", "n789"),
                                 running_window = 1) {
  DFW_dept_avg_combined <- NULL
  
  for (i in 1:length(gp_names)) {
    by_course_Non_STEM_w_coreq <-
      ls_db[[i]]  %>% group_by(Term, SubjectCode, CourseNumber, Grade) %>%
      summarize(n = n())
    DFW_dept_avg_Non_STEM_w_coreq <-
      Dept_running_DFW(db = by_course_Non_STEM_w_coreq,
                       running_window = running_window)
    
    DFW_dept_avg_Non_STEM_w_coreq <-
      DFW_dept_avg_Non_STEM_w_coreq %>%
      mutate(group = gp_names[i])
    
    DFW_dept_avg_combined <- bind_rows(DFW_dept_avg_combined,
                                       DFW_dept_avg_Non_STEM_w_coreq
    )
  }
  return(DFW_dept_avg_combined)
}



#' Plot bargraph
#' plot the bargraph of the DFW of compared groups
#' @param combined_db combined database of all groups to be plotted
#' @param earlist_sem earliest semester to display
#' @param TYPE  "avg" or "r_avg"
#'
#' @return
#' @export
#'
#' @examples
bargraph <- function(combined_db=DFW_dept_avg_Non_STEM_combined, earlist_sem= earliest_sem_in_plot, TYPE="avg")
{ 
  
  if (TYPE=="avg") {
    
    ylab_word <- "DFW %"
  } 
  else if  ( TYPE=="r_avg") {
    ylab_word <- " Running DFW %"
  }
  else {
    ylab_word <- "Cumulative DFW %"
  }
  
  combined_db <- combined_db %>% filter(Term >= earlist_sem)
  label_col <- ifelse(combined_db$DFW<20,"black","white")
  # use facet_wrap
  p <- combined_db %>% 
    #  mutate(Term = factor(Term)) %>%
    # mutate(labels=paste(Term,group)) %>%
    ggplot(aes(group, DFW, fill = group)) +
    geom_bar(stat="identity", show.legend = FALSE) +
    geom_text(aes(label = format(round(DFW,1)),
                  # vjust = ifelse( DFW >90, 1.4, -0.4)), color="black", size=3)+
                  vjust = ifelse( DFW < 20, -0.4, 1.4)), color=label_col, size=3)+
    ylim(0,100)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(ylab_word) +
    facet_wrap(~Term,ncol=3)
  
  return (p)
  
}



#' Bargraph of proportions
#'
#' @param combined_db 
#' @param earlist_sem 
#'
#' @return
#' @export
#'
#' @examples
bargraph_proportion <- function(combined_db=DFW_dept_avg_Non_STEM_combined, 
                                earlist_sem= earliest_sem_in_plot
)
{ 
  
  ylab_word <- "%"
  
  combined_db <- combined_db %>% filter(Term >= earlist_sem)
  label_col <- ifelse(combined_db$proportion<20,"black","white")
  # use facet_wrap
  p <- combined_db %>% 
       ggplot(aes(group, proportion, fill = group)) +
    geom_bar(stat="identity", show.legend = FALSE) +
    geom_text(aes(label = format(round(proportion,1)),
                  # vjust = ifelse( DFW >90, 1.4, -0.4)), color="black", size=3)+
                  vjust = ifelse( proportion < 20, -0.4, 1.4)), color=label_col, size=3)+
    ylim(0,100)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(ylab_word) +
    facet_wrap(~Term,ncol=3)
  
  return (p)
  
}




#' Line graph of compared groups
#' Plot a line graph of compare groups
#' @param db combined database of all groups
#' @param grp_nm 
#' @param TYPE "avg" or "r_avg"
#' @param FORM form name to be shown in the plot title
#' @param earlist_sem earliest semester to display in the figure
#'
#' @return
#' @export
#'
#' @examples
plot_line_compare_DFW <- function (db= DFW_dept_avg_Non_STEM_combined,
                                   grp_nm = "Non-STEM",
                                   TYPE="avg",
                                   FORM="coreq", earlist_sem=earliest_sem_in_plot
){
  db <- db %>% filter(Term >= earlist_sem)
  if (TYPE=="avg") {
    leading_title_word <- ""  
    ylab_word <- "DFW %"
    
  } 
  else if (TYPE=="c_avg") {
    leading_title_word <- "Cumulative "  
    ylab_word <- "Cumulative DFW %" }
  else  {leading_title_word <- "Running "
  ylab_word <- "Running DFW %"
  }
  
  if (FORM=="coreq") ending_word<- "with/without a co-requisite"
  else if (FORM=="800803") 
  {ending_word<- "with/without taking 700/703"}
  else if (FORM=="credit-bearing") 
  {ending_word<- "via different paths"}
  else if (FORM=="remediation") 
  {ending_word<- "via different paths"}
  else if (FORM=="Calculus") 
  {ending_word<- "via different paths"}
  else ending_word <- ""
  p<- db %>% 
    #mutate(Term = factor(Term)) %>%
    # mutate(labels=paste(Term,group)) %>%
    ggplot(aes(factor(Term), DFW, col = group)) +
    geom_point(aes(group=group,col=group)) +
    geom_line(aes(group=group,col=group)) +
    ylim(0,100)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(ylab_word) +
    ggtitle(paste("Aggregated ", leading_title_word, 
                  "DFW rates of ", grp_nm, 
                  " courses ", ending_word))
  
}

#' Plot the pie chart of proportions
#'
#' @param combined_db 
#' @param earlist_sem 
#' @param gtitle the title of the graph
#'
#' @return
#' @export
#'
#' @examples
piegraph_proportion <- function(combined_db=DFW_dept_avg_Non_STEM_combined, 
                                earlist_sem= earliest_sem_in_plot, gtitle=""
)
{ 
  
  ylab_word <- "%"
  
  combined_db <- combined_db %>% filter(Term >= earlist_sem)
  #label_col <- ifelse(combined_db$proportion<20,"black","white")
  # use facet_wrap
  combined_db <- combined_db %>%
    arrange(desc(group))%>%
    mutate(prop=proportion/sum(proportion)*100) %>%
    mutate(ypos=cumsum(prop)-0.6*prop)
  p <- combined_db %>% 
    ggplot(aes(x="", y=proportion, fill = group)) +
    geom_bar(stat="identity", width=1, show.legend = TRUE) +
    coord_polar("y",start=0)+
    geom_text(aes(y=ypos,label = format(round(proportion,1))),
              # vjust = ifelse( DFW >90, 1.4, -0.4)), color="black", size=3)+
              color="white", size=2.5)+
    #  ylim(0,100)+
    theme(axis.text.x = element_blank()) +
    # theme(legend.position="top")+
    #theme_void()+
    # theme_economist() +
    theme(legend.position="right")+
    xlab("") +
    ylab("") +
    facet_wrap(~Term,ncol=3)+
    ggtitle(gtitle)
  
  return (p)
  
}




####### Calculating for a sequence of two courses
# 
#' Running DFW of a seq of two courses
#' Calculating running DFW for a sequence of two courses, when windowsize=1, it just calculates averages
#' @param db1 
#' @param db2 
#' @param running_window running window size
#'
#' @return
#' @export
#'
#' @examples
running_DFW_seq_course <- function(db1=df_700_703,db2=df_p7_to_8, 
                                   running_window=running_DFW_terms)
{
  
  current_term <- max(db2$Term)
  by_course_db1 <- db1  %>% 
    filter (Term < current_term) %>%
    mutate(CourseNumber="800/803") %>%
    group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    #group_by(Term, SubjectCode,  Grade) %>%
    summarize(n = n())
  #view(by_course_db1)
  
  base_num_db1 <- by_course_db1 %>% 
    group_by(Term, SubjectCode, CourseNumber) %>%
    # group_by(Term, SubjectCode) %>%
    summarize(total= sum(n) ) %>%
    group_by(SubjectCode, CourseNumber) %>% arrange(Term) %>%
    #arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window)) 
  
  #view(base_num_db1)
  
  by_course_db2 <- db2  %>% 
    mutate(CourseNumber="800/803") %>%
    group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    #group_by(Term, SubjectCode, Grade) %>%
    summarize(n = n())
  
  by_course_running_DFW <- by_course_db2 %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, SubjectCode, CourseNumber) %>%
    # group_by(Term, SubjectCode) %>%
    summarize(total= sum(n),nPass = sum(n[Grade %in% passing_grades]) ) %>%
    group_by(SubjectCode, CourseNumber) %>% arrange(Term) %>%
    #arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),
           r_nPass=adapt_runSum(nPass,running_window)) 
  #view(by_course_running_DFW)
  seq_DFW <- 1-by_course_running_DFW$r_nPass / base_num_db1$r_total
  
  if (!("DFW" %in% colnames(by_course_running_DFW))) {
    by_course_running_DFW <-  
      bind_cols(by_course_running_DFW,data.frame(DFW = seq_DFW*100)) 
  }
  
  return (by_course_running_DFW)
}


#Department running average of only MATH courses (excluding ENGR)

#' Department running DFW for a sequence of two courses
#'
#' @param db1 
#' @param db2 
#' @param running_window running window size 
#'
#' @return
#' @export
#'
#' @examples
Dept_running_DFW_seq_course <- function (db1=df_700_703,db2=df_p7_to_8, 
                                         running_window=running_DFW_terms){
  current_term <- max(db2$Term)
  by_course_db1 <- db1  %>% 
    filter (Term < current_term) %>%
    group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    summarize(n = n())
  #view(by_course_db1)
  
  base_num_db1 <- by_course_db1 %>% 
    #filter(SubjectCode == "MATH") %>%
    group_by(Term) %>%
    summarize(total= sum(n) ) %>%
    #group_by(SubjectCode, CourseNumber) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window)) 
  
  #view(base_num_db1)
  
  by_course_db2 <- db2  %>% group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    summarize(n = n())
  
  MATH_dept_running_DFW <- by_course_db2 %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term) %>%
    summarize(total= sum(n),nPass =sum(n[Grade %in% passing_grades]) ) %>%
    # group_by(SubjectCode, CourseNumber) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),
           r_nPass=adapt_runSum(nPass,running_window)) 
  #view(by_course_running_DFW)
  seq_DFW <- 1- MATH_dept_running_DFW$r_nPass / base_num_db1$r_total
  
  if (!("DFW" %in% colnames(MATH_dept_running_DFW))) {
    MATH_dept_running_DFW <-  
      bind_cols(MATH_dept_running_DFW,data.frame(DFW = seq_DFW*100)) 
  }
  
  
  return(MATH_dept_running_DFW)
  
}

##################################
#' Display the table for a sequence of two courses
#' 
#' @param db1 
#' @param db2 
#' @param TYPE "avg" or "r_avg"
#' @param analyzed_courses 
#' @param num_sem_to_display in the table, 16 for a letter size portrait
#' @param FORM various form names, see the codes for details
#' @param KEY column name: "DFW", "total", etc
#' @param running_window running window size, default =15
#'
#' @return
#' @export
#'
#' @examples
display_seq_table <- function (db1=df_700_703,db2=df_p7_to_8, TYPE="avg",
                               analyzed_courses=c("800/803"), 
                               num_sem_to_display = 16,
                               FORM="7to8",KEY="DFW",
                               running_window=running_DFW_terms
){
  
  
  DFW_by_course_Non_STEM_w_coreq <- running_DFW_seq_course(db1,db2,
                                                           running_window)
  #view(DFW_by_course_Non_STEM_w_coreq)
  
  DFW_dept_avg_Non_STEM_w_coreq <- Dept_running_DFW_seq_course(db1,db2,
                                                               running_window )
  #view(DFW_dept_avg_Non_STEM_w_coreq)
  
  # Display the table format
  by_course_DFW_table_Non_STEM_w_coreq <- DFW_table_by_course (
    Dept_DFW=DFW_dept_avg_Non_STEM_w_coreq, 
    by_course_DFW=DFW_by_course_Non_STEM_w_coreq,
    #Analyzed_courses =Analyzed_MATH_courses,
    TYPE=TYPE,
    FORM,
    KEY=KEY
    # number_sem_to_display=number_sem_display,
  )
  #view(by_course_DFW_table_Non_STEM_w_coreq)
  #view the table
  total_terms <- length(unique(db2$Term))
  avg_table(TYPE=TYPE,
            by_course_DFW_table_Non_STEM_w_coreq,
            analyzed_course = analyzed_courses ,
            num_sem_to_display= min(total_terms-1, num_sem_to_display), FORM,KEY)
  
  #return(by_course_DFW_table_Non_STEM_w_coreq)
}



#' Compute combined dataframe of a sequence of two courses
#'
#' @param seq_db list of the sequence of two courses
#' @param db_compared compared database
#' @param gp_names 
#' @param running_window running window size
#'
#' @return
#' @export
#'
#' @examples
combined_dept_avg_db_seq <- function(seq_db=list(db1=df_700_703,db2=df_p7_to_8),
                                     db_compared=df_902_903,
                                     gp_names = c("p7to8",  "902/903"),
                                     running_window = running_DFW_terms) {
  # DFW_dept_avg_combined <- NULL
  DFW_dept_avg_seq_course<- 
    Dept_running_DFW_seq_course(db1=seq_db[[1]],db2=seq_db[[2]],
                                running_window=running_window )
  DFW_dept_avg_seq_course <- DFW_dept_avg_seq_course %>% 
    mutate(group = gp_names[1])
  
  
  by_course_Non_STEM_w_coreq <-
    db_compared  %>% group_by(Term, SubjectCode, CourseNumber, Grade) %>%
    summarize(n = n())
  DFW_dept_avg_Non_STEM_w_coreq <-
    Dept_running_DFW(db = by_course_Non_STEM_w_coreq,
                     running_window = running_window)
  
  DFW_dept_avg_Non_STEM_w_coreq <-
    DFW_dept_avg_Non_STEM_w_coreq %>%
    mutate(group = gp_names[2])
  
  DFW_dept_avg_combined <- bind_rows(DFW_dept_avg_seq_course,
                                     DFW_dept_avg_Non_STEM_w_coreq
  )
  
  return(DFW_dept_avg_combined)
}

