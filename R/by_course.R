

#' Running DFW
#' Calculating running DFW, when windowsize=1, it just calculates averages
#' @param db input dataframe
#' @param running_window running window size. Default to 1. 
#'
#' @return by_course_running_DFW
#' @export
#'
#' @examples
#' 
running_DFW <- function(db=by_course,running_window=1)
{
  
  by_course_running_DFW <- db %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, SubjectCode, CourseNumber) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    group_by(SubjectCode, CourseNumber) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100)  
  return (by_course_running_DFW)
}



#' Department running average
#' Department running average of only MATH courses (excluding ENGR)
#' @param db input dataframe
#' @param running_window running window size. Default =1
#'
#' @return Dept_running_DFW
#' @export
#'
#' @examples
#' 
Dept_running_DFW <- function (db=by_course, running_window=1){
  MATH_dept_running_DFW <- db %>%
    filter(SubjectCode %in% c("MATH", "STAT")) %>%
    group_by(Term) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100) 
  
  return(MATH_dept_running_DFW)
  
}

###################################################################
# Table Display DFW 
################################################################

#' DFW Table
#' Display DFW Table of selected courses with department average at the bottom
#' @param Dept_DFW Department DFW dataframe
#' @param by_course_DFW by_course_DFW data frame
#' @param TYPE can be "r_avg" for running average for "avg" for average
#' @param FORM can be "by_course" or some others
#' @param KEY column name: can be "DFW" or "total"
#'
#' @return DFW_table_by_course
#' @export
#'
#' @examples
DFW_table_by_course <- function(
  Dept_DFW=MATH_dept_running_DFW, 
  by_course_DFW=by_course_running_DFW,
  TYPE="r_avg",
  FORM="by_course",
  KEY="DFW"   # KEY: DFW or total
){
  # Table format for Dept ave DFW
  MATH_running_avg <- Dept_DFW %>% 
    select(Term, KEY) %>%
    spread(Term, KEY)
  
  if (FORM == "by_course" & KEY=="DFW"){
    MATH_running_avg_table <-
      data.frame(SubjectCode = "Dept", CourseNumber = TYPE) %>%
      bind_cols(MATH_running_avg) }
  else if (FORM == "by_course" & KEY=="total"){
    MATH_running_avg_table <-
      data.frame(SubjectCode = "Dept", CourseNumber = KEY) %>%
      bind_cols(MATH_running_avg) }
  
  else if (KEY == "total" & FORM != "by_course") {
    MATH_running_avg_table <-
      data.frame(SubjectCode = "Aggr", CourseNumber = KEY) %>%
      bind_cols(MATH_running_avg)
  }
  else {MATH_running_avg_table <-
    data.frame(SubjectCode = "Aggr", CourseNumber = TYPE) %>%
    bind_cols(MATH_running_avg)}
  
  # Display the table results  by course
  by_course_running_table <- by_course_DFW %>%
    select(Term,SubjectCode,CourseNumber,KEY) %>%
    spread(Term, KEY)
  
  # Attach the Dept ave in the last row
  if (!(TYPE %in% by_course_running_table$CourseNumber)) {
    # append MATH avg
    by_course_running_table <- by_course_running_table %>%
      rbind.data.frame(MATH_running_avg_table)
  }
  
  displayed_table <- by_course_running_table
  
  colnames(displayed_table)[1:2] <- c("Subj","Course")
  return(displayed_table)
}
# 
#' Average Table
#' Knit the DFW table
#' @param TYPE can be "avg", "r-avg", etc
#' @param display_table the table to be knitted
#' @param analyzed_course courses to be displayed
#' @param num_sem_to_display number of semesters to be displayed
#' @param FORM can be "by_course" or others, see the code for details
#' @param KEY column name: such as "DFW", "total"
#'
#' @return knitted table avg_table
#' @export
#'
#' @examples
avg_table <- function(TYPE="avg", display_table=by_course_DFW_table, 
                      analyzed_course =c(Analyzed_MATH_courses,advanced),
                      num_sem_to_display=number_sem_display,
                      FORM="by_course", KEY="DFW"
) 
{
    # Display only selected rows and columns
  if (KEY=="total")
    index_analyzed <- display_table$Course %in% c( analyzed_course, KEY)
  else 
    index_analyzed <- display_table$Course %in% c( analyzed_course, TYPE)
  
  last_col_ind <- ncol(display_table)
  
  sel_col <- c(1:2, (last_col_ind-min(num_sem_to_display,(ncol(display_table)-2) ) )
               :last_col_ind)
  display_table <- display_table[index_analyzed, 
                                 sel_col]
  options(knitr.kable.NA = "**")
  if (TYPE=="r_avg") {leading_title_word <- "Running " } 
  else if (TYPE=="c_avg") {leading_title_word <- "Cumulative " } 
  else {leading_title_word <- ""}
  
  if (FORM=="coreq") {end_word <- "with a corequisite"}
  else if (FORM=="wo-coreq") {end_word<- "without a corequisite"}
  else if (FORM =="7to8")  {end_word<- " for students who passed 700/703"}
  else if (FORM =="only8")  {end_word<- " for students without taking 700/703"}
  else if (FORM =="p7p8toc")  {end_word<- " for students who passed 700/703 and 800/803"}
  else if (FORM =="wo7p8toc")  {end_word<- " for students who passed 800/803 without taking 700/703"}
  else if (FORM =="p9toc")  {end_word<- " for students who passed 902/903"}
  else if (FORM =="wo789toc")  {end_word<- " for students without taking a remedial class"}
  else if (FORM =="902903")  {end_word<- " for students taking the sequence 902/903"}
  else if (FORM =="p1120_p1550")  {end_word<- " for students who passed 1120 and 1550"}
  else if (FORM =="p1150")  {end_word<- " for students who passed 1150"}
  else if (FORM =="p1050_p1550")  {end_word<- " for students who passed 1050 and 1550"}
  else if (FORM =="1610_only")  {end_word<- " for students who took 1610 directly"}
  else {end_word <- ""}
  
  if (KEY =="DFW") {table_title=paste(KEY, " rates ")}
  else {table_title= paste(KEY, " number of students ")}
  
  
  display_table %>% knitr::kable(
    caption=paste(leading_title_word,table_title, "by course per semester ",end_word)) %>%
    kable_styling(font_size = 7)
  
  
}


#################################################################################
# Plot department DFW

#' Plot Course DFW
#'
#' @param by_course_DFW_df by_course_DFW_df
#' @param MATH_dept_avg_df 
#' @param MATH_dept_running_DFW_df 
#' @param earliest_semester_in_plot 
#'
#' @return
#' @export
#'
#' @examples
plot_course_DFW <- function(by_course_DFW_df = by_course_DFW,
                            MATH_dept_avg_df =MATH_dept_avg,
                            MATH_dept_running_DFW_df =MATH_dept_running_DFW,
                            earliest_semester_in_plot = earliest_sem_in_plot
){
  by_course_DFW_df <- by_course_DFW_df %>%  filter (Term >= earliest_semester_in_plot)
  MATH_dept_avg_df <- MATH_dept_avg_df %>% filter (Term >= earliest_semester_in_plot)
  MATH_dept_running_DFW_df <- MATH_dept_running_DFW_df  %>%  filter (Term >=  earliest_semester_in_plot)
  
  f<- by_course_DFW_df %>%  # mutate(Term = factor(Term)) %>%
    # filter(SubjectCode %in% c("MATH","STAT")) %>%  #excluding ENGR
    ggplot(aes(factor(Term), DFW) ) +
    geom_boxplot() +  #color="black"
    #geom_point(alpha=0.2)+
    geom_jitter(width = 0.15, alpha = 0.2) +
    xlab("") + ylab("DFW %") +
    ggtitle("DFW rates of MATH courses per semester (dept avg indicated by x)") +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = MATH_dept_avg_df,
      color = "red",
      shape = 4
    ) +
    geom_line(mapping = aes(1:nrow(MATH_dept_avg_df), DFW),
              data = MATH_dept_avg_df, linetype ="dotted",
              color = "red") +
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = MATH_dept_running_DFW_df,
      color = "blue",
      shape = 1
    ) +
    geom_line(mapping = aes(1:nrow(MATH_dept_running_DFW_df), DFW),
              data = MATH_dept_running_DFW_df, linetype ="solid",
              color = "blue")
  
  
  ggsave("figs/MATH_DFW_by_term.png",width = fig_width)
  return(f)
}


#' Plot top DFW courses bargraph
#'
#' @param TYPE can be "avg", "r_avg"
#' @param term semester code, such as 202102
#' @param DFW_db DFW dataframe
#'
#' @return
#' @export
#'
#' @examples
top_DFW_courses <- function (TYPE="avg",
                             term =current_term, DFW_db=by_course_DFW) {
  if (TYPE== "avg") {leading_word=""}
  else {leading_word="Running "}
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    filter (DFW >= 25) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    #mutate(CourseNumber=factor(CourseNumber)) %>%
    mutate(CourseNumber = reorder(CourseNumber, DFW)) %>%
    ggplot(aes((CourseNumber),DFW)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = format(DFW,nsmall=1)), hjust = 1.2,color="white",size=3)+
    ylim(0,100)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 10)) +
    ylab(paste(leading_word, "DFW(>=25%) (",as.character(term),")")) +
    xlab("")
  return(list(plot=p))
}

#' Plot barplot for top courses
#'
#' @param DFW_db 
#' @param TYPE "avg" or "r_avg"
#'
#' @return
#' @export
#'
#' @examples
barplot <- function(DFW_db=by_course_DFW, TYPE="avg"){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_DFW_courses(TYPE,term, DFW_db)})
  return(ls)
}




#######################################################
#Define a function to plot DFW of a group of courses
#######################################################
#' Plot line graphs of DFW by group of courses, and saving the figure in a folder ./figs that needs to be created prior
#'
#' @param DFW_data 
#' @param avg_data average department data frame
#' @param grp course numbers of the group of courses. for example c(700,800)
#' @param grp_nm a string for the group name to shown in the figure title
#' @param TYPE can be "avg", "r_avg"
#' @param earliest_sem the earliest semester to show in the figure
#'
#' @return
#' @export
#'
#' @examples
plot_DFW_by_group <- function(DFW_data=by_course_DFW, 
                              avg_data=MATH_dept_avg, 
                              grp=remedial_courses, 
                              grp_nm="remedial",
                              TYPE="avg",
                              earliest_sem =earliest_sem_in_plot
){
  #imfile_nm <- paste("figs/", grp_nm, "_DFW_by_term.png", sep="")
  if (TYPE=="avg") {
    leading_title_word <- ""  
    ylab_word <- "DFW %"
  } 
  else {leading_title_word <- "Running "
  ylab_word <- "Running DFW %"
  }
  imfile_nm <- paste("figs/", grp_nm, leading_title_word, "_DFW_by_term.png", sep="")
  DFW_data <- DFW_data %>% filter (Term >= earliest_sem)
  avg_data <- avg_data %>% filter (Term >= earliest_sem)
  
  DFW_data %>% #mutate(Term = factor(Term)) %>%
    # filter(SubjectCode %in% c("MATH","STAT")) %>%  #excluding ENGR
    filter(CourseNumber %in% grp) %>%
    mutate (CourseNumber = factor(CourseNumber)) %>%
    ggplot(aes(factor(Term), DFW))  +
    geom_point(aes(group = CourseNumber, col = CourseNumber ))+
    geom_line(aes(group = CourseNumber, col = CourseNumber )) +
    ylim(0,100)+
    xlab("") + ylab(ylab_word) +
    ggtitle(paste(leading_title_word,"DFW rates of ",
                  grp_nm, 
                  " courses per semester (dept ",TYPE," indicated by x)")) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = avg_data,
      color = "red",
      shape = 4
    ) +
    geom_line(mapping = aes(1:nrow(avg_data), DFW),
              data = avg_data,
              linetype ="dotted",
              color = "red")  +
    ggsave(imfile_nm,width=fig_width)
  #print(imfile_nm)
}

#
#' Title
#' Display the DFW table and plot DFW of a group of courses 
#' @param DFW_data 
#' @param avg_data 
#' @param grp 
#' @param grp_name 
#' @param input_table the table in wide format to be knitted
#' @param TYPE "avg", or "r_avg"
#' @param earliest_sem earliest semester to display in the plot
#'
#' @return
#' @export
#'
#' @examples
display_plot_by_course <- function(DFW_data=by_course_DFW, 
                                   avg_data=MATH_dept_avg,
                                   grp=remedial_courses,
                                   grp_name="remedial",
                                   input_table=by_course_DFW_table,
                                   TYPE="avg", 
                                   earliest_sem =earliest_sem_in_plot
) {
  index_analyzed <- input_table$Course %in% 
    c( grp, TYPE)
  
  t <-   input_table[index_analyzed,]
  #view(by_course_table[index_analyzed,])
  # plot the DFW of the  group
  group <- grp
  grp_name <- grp_name
  p<-  plot_DFW_by_group(DFW_data=DFW_data, 
                         avg_data=avg_data,
                         grp=group, grp_nm = grp_name, TYPE=TYPE, earliest_sem) 
  return(list(display=t, plot=p))
}


# 
#' Knit the DFW Table
#' Knit the DFW table by course
#' @param ls a list contains the DFW table to be knitted. The table is given by ls$display
#' @param number_sem_to_display in the table
#' @param TYPE "avg" or "r_avg"
#'
#' @return
#' @export
#'
#' @examples
table_by_course <- function(ls, number_sem_to_display=number_sem_display,
                            TYPE="avg") {
  t <- ls$display
  last_col_ind <- ncol(t)
  sel_col <- c(1:2, (last_col_ind-number_sem_display):last_col_ind)
  displayed_table <- t[,sel_col]
  
  colnames(displayed_table)[1:2] <- c("Subj","Course")
  
  options(knitr.kable.NA = "**")
  if (TYPE=="avg") {leading_title_word <- ""  } 
  else {leading_title_word <- "Running "}
  displayed_table %>% knitr::kable(caption=paste(leading_title_word,"DFW rates by course per semester")) %>%
    kable_styling(font_size = 7)
}


