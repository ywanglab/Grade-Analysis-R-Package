######################################################
#                    Analysis by Faculty
######################################################


#' Compute faculty running DFW 
#'
#' @param db input database
#' @param running_window running window size;
#'
#' @return
#' @export
#'
#' @examples
faculty_running_DFW <- function(db=by_faculty,running_window=1)
{
  by_faculty_running_DFW <- db %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, Instructor) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    group_by(Instructor) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100)  
  return (by_faculty_running_DFW)
}




#
#' Department running average of all MATH faculty (excluding CSCI faculty)
#'
#' @param db input database
#' @param running_window running window size
#'
#' @return
#' @export
#'
#' @examples
Dept_faculty_running_DFW <- function (db=by_faculty, running_window=1){
  MATH_dept_faculty_running_DFW <- db %>%
    #  filter(SubjectCode == "MATH") %>%
    group_by(Term) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100) 
  
  return(MATH_dept_faculty_running_DFW)
  
}

###################################################################
# Table Display DFW 
################################################################

#' Generate the faculty DFW table
#'
#' @param Dept_DFW department aggregated DFW dataframe
#' @param by_faculty_DFW by faculty DFW dataframe
#' @param TYPE "Dept r_avg" or 
#'
#' @return
#' @export
#'
#' @examples
DFW_table_by_faculty <- function(
  Dept_DFW=MATH_dept_faculty_running_DFW, 
  by_faculty_DFW=by_faculty_running_DFW,
  TYPE="Dept r_avg"
){
  # Table format for Dept ave DFW
  MATH_running_avg <- Dept_DFW %>% 
    select(Term, DFW) %>%
    spread(Term, DFW)
  
  MATH_running_avg_table <-
    data.frame(Instructor = TYPE) %>%
    bind_cols(MATH_running_avg)
  
  # Display the table results  by faculty
  
  by_faculty_running_table <- by_faculty_DFW %>%
    select(Term,Instructor,DFW) %>%
    spread(Term, DFW)
  
  # Attach the Dept ave in the last row
  if (!(TYPE %in% by_faculty_running_table$Instructor)) {
    # append MATH avg
    by_faculty_running_table <- by_faculty_running_table %>%
      rbind.data.frame(MATH_running_avg_table)
  }
  
  displayed_table <- by_faculty_running_table
  
  return(displayed_table)
  
}

#' Knit the faculty DFW average table
#'
#' @param TYPE "Dept avg" or other
#' @param display_table displayed table to be knitted
#' @param analyzed_fac selected fac
#' @param number_sem_to_display 
#'
#' @return
#' @export
#'
#' @examples
fac_avg_table <- function(TYPE="Dept avg", display_table=by_fac_table,
                          analyzed_fac=Analyzed_fac,
                          number_sem_to_display=number_sem_display
)
{
  
  index_analyzed <- display_table$Instructor %in% c( analyzed_fac, TYPE)
  last_col_ind <- ncol(display_table)
  sel_col <- c(1, (last_col_ind-number_sem_to_display):last_col_ind)
  displayed_table <- display_table[index_analyzed, 
                                   sel_col]
  options(knitr.kable.NA = "**")
  if (TYPE=="Dept r_avg") {leading_title_word <- "Running " } 
  else {leading_title_word <- ""}
  displayed_table %>% knitr::kable(caption=paste(leading_title_word,"DFW rates by faculty per semester")) %>%
    kable_styling(font_size = 7)
}


#' Plot faculty DFW linegraph
#'
#' @param by_faculty_DFW_df 
#' @param MATH_dept_fac_avg_df 
#' @param MATH_dept_fac_running_avg_df 
#' @param earlist_sem earlist semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_fculty_DFW <- function(by_faculty_DFW_df = by_faculty_DFW,
                            MATH_dept_fac_avg_df = MATH_dept_fac_avg,
                            MATH_dept_fac_running_avg_df = MATH_dept_fac_running_avg,
                            earlist_sem = earliest_sem_in_plot
                            
){
  by_faculty_DFW_df <- by_faculty_DFW_df %>% filter (Term >= earlist_sem)
  MATH_dept_fac_avg_df <- MATH_dept_fac_avg_df %>% filter (Term >= earlist_sem)
  MATH_dept_fac_running_avg_df <- MATH_dept_fac_running_avg_df %>% filter (Term >= earlist_sem)
  
  
  p <- by_faculty_DFW_df %>% #mutate(Term = factor(Term)) %>%
    ggplot(aes(factor(Term), DFW)) +
    geom_boxplot(color = "black") +
    #geom_point(alpha=0.2)+
    geom_jitter(width = 0.15, alpha = 0.2) +
    xlab("") + ylab("DFW %") +
    ggtitle("DFW rates of MATH faculty per semester (avg indicated by x)") +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = MATH_dept_fac_avg_df,
      color = "red",
      shape = 4
    ) +
    geom_line(mapping = aes(1:nrow(MATH_dept_fac_avg_df), DFW),
              data = MATH_dept_fac_avg_df,linetype ="dotted",
              color = "red")+
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = MATH_dept_fac_running_avg_df,
      color = "blue",
      shape = 1
    ) +
    geom_line(mapping = aes(1:nrow(MATH_dept_fac_running_avg_df), DFW),
              data = MATH_dept_fac_running_avg_df, linetype ="solid",
              color = "blue")
  
  ggsave("figs/by_fac/MATH_fac_DFW_by_term.png",width = fig_width)
  
  return(p)
}


#' Plot bargraph of top 20 faculty without showing names
#'
#' @param term term to plot
#'
#' @return
#' @export
#'
#' @examples
top_DFW_faculty_no_name <- function (term =current_term) {
  p<- by_faculty_DFW %>% ungroup() %>%
    filter(Term == term) %>%
    #  filter (DFW >= 25) %>%
    arrange(desc(DFW)) %>% 
    mutate(Instructor=factor(Instructor)) %>%
    mutate(Instructor = reorder(Instructor, DFW)) %>%
    ggplot(aes(Instructor,DFW)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = round(DFW,1)),hjust = 1.2,color="white",size=3)+
    ylim(0,100)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 10)) +
    theme(
      #axis.text.x = element_blank(),
      axis.text.y = element_blank()) +
    ylab(paste("DFW of all MATH faculty (",as.character(term),")")) +
    xlab("")
  return(list(plot=p))
}



#' plot faculty DFW bargraph with names
#'
#' @param TYPE "avg" or "r_avg"
#' @param term term to display
#' @param DFW_db input data frame
#'
#' @return
#' @export
#'
#' @examples
top_DFW_faculty <- function (TYPE="avg",
                             term =current_term, 
                             DFW_db=by_faculty_DFW) {
  if (TYPE== "avg") {leading_word=""}
  else {leading_word="Running "}
  p<- DFW_db %>% ungroup()%>%
    filter(Term == term) %>%
    #filter (DFW >= 25) %>%
    arrange(desc(DFW)) %>% 
    mutate(Instructor=factor(Instructor)) %>%
    mutate(Instructor = reorder(Instructor, DFW)) %>%
    ggplot(aes(Instructor,DFW)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = format(round(DFW,1))), hjust = 1.2, color="white",size=3)+
    ylim(0,100)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 10)) +
    # theme(
    #   #axis.text.x = element_blank(),
    #   axis.text.y = element_blank()) +
    ylab(paste(leading_word,"DFW of faculty (",as.character(term),")")) +
    xlab("")
  return(list(plot=p))
}


#' Plot the bargraph of faculty DFW for the past six semesters with names
#'
#' @param DFW_db input database
#' @param TYPE "avg" or "r_avg"
#'
#' @return
#' @export
#'
#' @examples
barplot_fac <- function(DFW_db=by_faculty_DFW, TYPE="avg"){ 
  
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_DFW_faculty(TYPE,term, DFW_db)})
  
  return(ls)
}
#######################################################
#Define a function to plot DFW of a group of faculty or individual
#######################################################
#' plot DFW of a group of faculty or individual
#'
#' @param DFW_data input dataframe
#' @param avg_data input aggregated (department) dataframe
#' @param grp selected faculty members
#' @param grp_nm 
#' @param TYPE "Dept avg" or other
#' @param earliest_sem eariest semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_fac_DFW_by_group <- function(DFW_data=by_faculty_DFW, 
                                  avg_data=MATH_dept_fac_avg, 
                                  grp, 
                                  grp_nm=grp, 
                                  TYPE="Dept avg", earliest_sem =earliest_sem_in_plot
){
  
  DFW_data <- DFW_data %>%  filter( Term >= earliest_sem)
  avg_data <- avg_data %>%  filter( Term >= earliest_sem)
  
  #if (length(grp)==1) {grp_nm <- grp}
  if (TYPE=="Dept avg") {leading_title_word <- ""  
  ylab_word <- "DFW %"
  } 
  else {leading_title_word <- "Running "
  ylab_word <- "Running DFW %"
  }
  imfile_nm <- paste("figs/by_fac/", grp_nm, 
                     leading_title_word, "_DFW_by_term.png", sep="")
  DFW_data %>% #mutate(Term = factor(Term)) %>%
    filter(Instructor %in% grp) %>%
    mutate (Instructor = factor(Instructor)) %>%
    ggplot(aes(factor(Term), DFW))  +
    geom_point(aes(group = Instructor, col = Instructor))+
    geom_line(aes(group = Instructor, col = Instructor)) +
    xlab("") + ylab(ylab_word) +
    ylim(0,100)+
    ggtitle(paste(leading_title_word, 
                  "DFW rates of",grp_nm, "'s courses by semester (",TYPE," indicated by x)")) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point(
      mapping = aes(factor(Term), DFW),
      data = avg_data,
      color = "red",
      shape = 4
    ) +
    geom_line(mapping = aes(1:nrow(avg_data), DFW),
              data = avg_data, linetype ="dotted",
              color = "red")  +
    ggsave(imfile_nm,width=fig_width)
  #print(imfile_nm)
}


# 
#' Display the table and Plot DFW of individual faculty
#'
#' @param individual individual name
#' @param DFW_data input dataframe
#' @param avg_data input aggregated (department) dataframe
#' @param input_table input displayed table in wide format
#' @param TYPE "Dept avg" or other
#' @param earliest_sem earliest semester to display
#'
#' @return
#' @export
#'
#' @examples
display_plot_individual_DFW <- function (individual, 
                                         DFW_data=by_faculty_DFW, 
                                         avg_data=MATH_dept_fac_avg,
                                         input_table=by_fac_table,
                                         TYPE="Dept avg", earliest_sem =earliest_sem_in_plot){
  
  index_analyzed <- input_table$Instructor %in% 
    c( individual, TYPE)
  
  last_col_ind <- ncol(input_table)
  t <- input_table[index_analyzed, 
                   c(1, (last_col_ind-number_sem_display):last_col_ind)]
  
  #view(by_faculty_table[index_analyzed,])
  p<- plot_fac_DFW_by_group(
    DFW_data, 
    avg_data,
    individual,
    individual, 
    TYPE, earliest_sem  )
  
  return(list(display=t,plot=p))
}



#' knit the table for individual DFW
#'
#' @param i index for the individua in the group
#' @param list list that contains the displayed table
#' @param TYPE "avg" or "r_avg"
#'
#' @return
#' @export
#'
#' @examples
table_individual <- function (i=1,list, TYPE="avg"){
  if (TYPE=="r_avg"){ leading_title_word <-"Running " }
  else {leading_title_word<-""}
  t <- list[,i]$display
  options(knitr.kable.NA = "**")
  t %>% knitr::kable(caption=paste(leading_title_word,"DFW rates by faculty per semester")) %>%
    kable_styling(font_size = 7)
  
}

#########################################################
# Analysis by grade distribution for faculty
########################################################

#' plot faculty grade distribution compared to department avg
#'
#' @param combined_db input data frame combining the faculty and the department average
#' @param comp_gp compared groups
#' @param earliest_sem earliest semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_fac_grade_bargraph <- function(combined_db = by_fac_grades_combined,
                                    comp_gp = c(individual, "Dept"),
                                    earliest_sem = earliest_sem_in_plot
                                    
) {
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  p <-  combined_db %>%
    filter(Instructor %in% comp_gp) %>%
    ggplot(aes(grade, proportion, fill = 
                 Instructor  )) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = TRUE) +
    geom_text(
      aes(label = round(proportion, 1), 
          vjust = ifelse( proportion >90, 1.2, -0.4)),
      position = position_dodge(width = 1),
      size = 2.5
      
    ) +
    ylim(0, 100) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    xlab("") +
    ylab("%") +
    facet_wrap(~ Term, ncol = 3)
  return(p)
}


#' Plot faculty DFW grade distribution line graph
#'
#' @param combined_db combined dataframe of both the faculty and the departmetn avg
#' @param comp_gp compared groups
#' @param comp_grade selected grades to compare
#' @param earliest_sem earliest semester to display
#'
#' @return
#' @export
#'
#' @examples
plot_fac_DFW_distribution <- function (combined_db = by_fac_grades_combined,
                                       comp_gp = c(individual, "Dept"),
                                       comp_grade =c("D","F","W"),
                                       earliest_sem = earliest_sem_in_plot) {
  combined_db <- combined_db %>% filter(Term >= earliest_sem)
  dept_db <- combined_db %>%  
    filter(Instructor %in% comp_gp[2]) %>%
    filter(grade %in% c(comp_grade)) 
  
  p<- combined_db %>% 
    filter(Instructor %in% comp_gp[1]) %>%
    filter(grade %in% c(comp_grade)) %>%
    ggplot(aes(factor(Term), proportion,color=grade)) +
    geom_point() +
    geom_line(aes(group=grade)) +
    ylim(0,100)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("%") +
    ggtitle(paste("DFW grade distribution in ",comp_gp[1],"'s courses per semester (Dept avg by dotted)")) +
    
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



