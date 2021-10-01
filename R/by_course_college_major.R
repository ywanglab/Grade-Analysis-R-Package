

#' Ruunning DFW by collge
#'
#' @param db dataframe by college
#' @param running_window running window size 
#'
#' @return
#' @export
#'
#' @examples
college_running_DFW <- function(db=by_course_college,running_window=1)
{
  by_college_running_DFW <- db %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, College) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    group_by(College) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100)  
  return (by_college_running_DFW)
}



#' Plot DFW bargraph by college
#'
#' @param combined_db combined dataframe
#' @param comp_gp set of colleges
#' @param earliest_sem earliest semeser in the plot
#' @param TYPE   "DFW" or "rDFW"
#'
#' @return
#' @export
#'
#' @examples
plot_DFW_by_college_bargraph <- function(combined_db = by_college_total,
                                         comp_gp = all_colleges,
                                         earliest_sem = earliest_sem_in_plot,
                                         TYPE="DFW"
                                         
) {
  
  if (TYPE=="rDFW") 
  {leading_word="Running "
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  
  
  label_col <- ifelse(combined_db$DFW<20,"black","white")
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  p <-  combined_db %>%
    filter(College %in% comp_gp) %>%
    ggplot(aes(College, DFW, fill = 
                 College  )) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = TRUE) +
    geom_text(
      aes(label = round(DFW, 1),vjust = ifelse(DFW<20,  -0.4, 1.2)), 
      color=label_col,
      position = position_dodge(width = 1),
      size = 2.5
      
    ) +
    ylim(0, 100) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(paste(leading_word,"DFW %")) +
    facet_wrap(~ Term, ncol = 3) +
    ggtitle(paste(leading_title_word,"DFW rates of students taking a math department course by college"))
  return(p)
}




#' Plot stacked bargraphs for DFW by college
#'
#' @param combined_db 
#' @param comp_gp set of colleges
#' @param earliest_sem earliest semester to show in plot
#' @param TYPE "DFW" or "rDFW"
#'
#' @return the stacked barplot
#' @export
#'
#' @examples
plot_DFW_by_college_stacked_bar <- function (combined_db = by_college_total,
                                             comp_gp = all_colleges,
                                             earliest_sem = earliest_sem_in_plot,
                                             TYPE="DFW") {
  
  if (TYPE=="rDFW") 
  {leading_word="Running "
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  
  label_col <- ifelse(combined_db$DFW<20,"black","white")
  combined_db <- combined_db %>%
    filter (Term >= earliest_sem)
  p <-  combined_db %>%
    filter(College %in% comp_gp) %>%
    ggplot(aes(factor(Term), DFW,fill=College)) +
    geom_bar(#aes(group=College),
      stat = "identity",
      position = "stack",
      show.legend = TRUE
    ) +
    geom_text(
      aes(label = round(DFW, 1),vjust =1.2),# ifelse(DFW<20,  -0.4, 1.2)),
      color="white", #label_col,
      position = "stack",
      size = 2.5
      
    ) +
    # ylim(0, 100) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(paste(leading_word,"DFW %")) +
    # facet_wrap(~ Term, ncol = 3) +
    ggtitle(paste(leading_title_word,"DFW rates of students taking a math department course by college"))
  return(p)
}



# Plot DFW curves vs. Term
#' Plot DFW linegraph by college
#'
#' @param combined_db combined database
#' @param comp_gp set of colleges
#' @param earliest_sem earliest semester to display
#' @param TYPE  "DFW" or "rDFW"
#'
#' @return
#' @export
#'
#' @examples
plot_DFW_by_college_linegraph <- function (combined_db = by_college_total,
                                           comp_gp = all_colleges,
                                           earliest_sem = earliest_sem_in_plot,
                                           TYPE="DFW") {
  
  if (TYPE=="rDFW") 
  {leading_word="Running "
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  combined_db <- combined_db %>% filter(Term >= earliest_sem)
  
  p<- combined_db %>% 
    filter(College %in% comp_gp) %>%
    ggplot(aes(factor(Term), DFW,color=College)) +
    geom_point() +
    geom_line(aes(group=College)) +
    #  ylim(0,100)+
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab(paste(leading_word,"DFW %")) +
    ggtitle(paste(leading_title_word,"DFW rates of students taking a math department course by college")) 
  
  return(p)
}


########################################################
# DFW Analysis by Major
########################################################## 

#' Major running DFW
#' calculate running DFW by Major
#' @param db dataframe by course and by Major
#' @param running_window running window size
#'
#' @return
#' @export
#'
#' @examples
Major_running_DFW <- function(db=by_course_Major,running_window=1)
{
  by_Major_running_DFW <- db %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, College,Major) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    group_by(Major) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100)  %>%
    mutate(type=ifelse(DFW>20,"W","B"))
  
  
  return (by_Major_running_DFW)
}




#' top DFW majors (bargraph)
#' plot the bargraph of top 20 Majors of DFW for a college or all colleges
#' @param colleges selected colleges
#' @param term the term to plot the bargraph
#' @param DFW_db Major DFW dataframe
#' @param TYPE "DFW" or "rDFW"
#'
#' @return
#' @export
#'
#' @examples
top_Major_by_DFW <- function (colleges=all_colleges,
                              term =current_term, 
                              DFW_db=Major_DFW,
                              TYPE="rDFW") {
  if (TYPE=="rDFW") 
  {leading_word="r_"
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  
  DFW_db <- DFW_db %>% 
    filter (College %in% colleges)
  
  
  
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    top_n(20, DFW) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    mutate(type=factor(type,levels=c("B","W"))) %>%
    mutate(Major = reorder(Major, DFW)) %>%
    ggplot(aes(Major,DFW, fill=College)) +
    geom_bar(stat="identity") +
    #geom_text(aes(label = round(DFW,1)), hjust = hjst_value,color=text_col,size=3)+
    # geom_text(aes(label = round(DFW,1),hjust = ifelse(DFW<20, -0.4, 1.3),color=label_col), size=3)+
    geom_text(aes(label = round(DFW,1),hjust = ifelse(DFW>20, 1.4,-0.4),
                  color=type     )  ,size=3,show.legend = FALSE)+
    scale_color_manual(values=c("black","white"))+
    ylim(0,100)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 8)) +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))+
    ylab(paste( leading_title_word, "DFW (",as.character(term),") of top 20 Majors")) +
    xlab("")  #+
  # ylab(paste(leading_word,"DFW %")) +
  # ggtitle(paste("Top 20 majors with the highest ",leading_title_word,"DFW rates of students taking a math department course")) 
  # 
  
  return(list(plot=p))
}



#' Major DFW barplot of the past six semesters
#' plot the DFW barplot of the past six semesters 
#' @param colleges selected college
#' @param DFW_db DFW dataframe by Major
#'
#' @return
#' @export
#'
#' @examples
barplot_by_Major_DFW <- function(colleges=all_colleges, DFW_db=Major_DFW){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_Major_by_DFW(colleges,term, DFW_db)})
  return(ls)
}



###########################################################  
## Top 20 best majors
##########################################################
#' top 20 best majors for a semeser
#' plot the top 20 majors for a semester in a college (a group of colleges)
#' @param colleges selected colleges
#' @param term the term to plot the bargraph
#' @param DFW_db DFW dataframe by Major
#' @param TYPE "rDFW" or "DFW"
#'
#' @return
#' @export
#'
#' @examples
top_best_Major_by_DFW <- function (colleges=all_colleges,
                                   term =current_term, 
                                   DFW_db=Major_DFW,
                                   TYPE="rDFW") {
  if (TYPE=="rDFW") 
  {leading_word="r_"
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  
  DFW_db <- DFW_db %>% 
    filter (College %in% colleges)
  
  
  
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    top_n(20, desc(DFW)) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    mutate(type=factor(type,levels=c("W","B"))) %>%
    mutate(Major = reorder(Major, desc(DFW))) %>%
    ggplot(aes(Major,DFW, fill=College)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = round(DFW,1),hjust = ifelse(DFW>20, 1.4,-0.4),
                  color=type     )  ,size=3,show.legend = FALSE)+
    scale_color_manual(values=c("white","black"))+
    ylim(0,100)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 8)) +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))+
    ylab(paste( leading_title_word, "DFW (",as.character(term),") of top 20 BEST Majors")) +
    xlab("")
  # ylab(paste(leading_word,"DFW %")) +
  # ggtitle(paste(leading_title_word,"DFW rates of students taking a math department course by college")) 
  # 
  
  return(list(plot=p))
}



#' barplot by Major of the past six semesters
#'
#' @param colleges selected colleges
#' @param DFW_db DFW dataframe by Major
#'
#' @return
#' @export
#'
#' @examples
barplot_by_Major_best_DFW <- function(colleges=all_colleges, DFW_db=Major_DFW){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_best_Major_by_DFW(colleges,term, DFW_db)})
  return(ls)
}


########################################################################
# DFW by course and Major
########################################################################




#' Compute by course by Major running DFW
#'
#' @param db by_course_by_major dataframe
#' @param running_window running window size
#'
#' @return
#' @export
#'
#' @examples
by_course_Major_running_DFW <- function(db=by_course_by_Major,running_window=1)
{
  by_Major_running_DFW <- db %>% 
    # arrange(Term, SubjectCode,CourseNumber) %>% 
    group_by(Term, SubjectCode, CourseNumber, College,Major) %>%
    summarize(total= sum(n),nDFW =total- sum(n[Grade %in% passing_grades]) ) %>%
    group_by(SubjectCode, CourseNumber, Major) %>% arrange(Term) %>%
    mutate(r_total=adapt_runSum(total, running_window),r_nDFW=adapt_runSum(nDFW,running_window)) %>%
    mutate(DFW = r_nDFW/r_total*100)  %>%
    mutate(type=ifelse(DFW>20,"W","B"))
  return (by_Major_running_DFW)
}





#' Compute the top 20 Majors in a course
#'
#' @param colleges selected colleges
#' @param term term for the computation
#' @param DFW_db DFW data base by course and Major
#' @param course course number
#' @param TYPE "rDFW" or "DFW"
#'
#' @return
#' @export
#'
#' @examples
top_Major_by_course_DFW <- function (colleges=all_colleges,
                                     term =current_term, 
                                     DFW_db=course_Major_DFW, 
                                     course=703, TYPE="rDFW") {
  if (TYPE=="rDFW") 
  {leading_word="r_"
  leading_title_word ="Running "
  }
  else {
    leading_word=""
    leading_title_word=""
  }
  
  DFW_db <- DFW_db %>% 
    filter (College %in% colleges) %>%
    filter (CourseNumber %in% course)
  
  p<- DFW_db %>% ungroup() %>%
    filter(Term == term) %>%
    top_n(20,DFW) %>%
    # select(CourseNumber,DFW) %>%
    # arrange(desc(DFW)) %>% #%>% head(20) %>%
    mutate(type=factor(type,levels=c("B","W"))) %>%
    mutate(Major = reorder(Major, DFW)) %>%
    ggplot(aes(Major,DFW, fill=College)) +
    geom_bar(stat="identity") +
    # geom_text(aes(label = round(DFW,1),hjust = ifelse(DFW>90, 1.4, -0.4)),  color="black",size=3)+
    geom_text(aes(label = round(DFW,1),hjust = ifelse(DFW>20, 1.4,-0.4),
                  color=type     )  ,size=3, show.legend=FALSE)+
    scale_color_manual(values=c("black","white"))+
    ylim(0,100)+
    coord_flip() +
    theme_economist() +
    theme(axis.text.y = element_text(size = 8)) +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))+
    ylab(paste(leading_title_word, "DFW (",as.character(term),") of top 20 Majors in MATH",as.character(course))) +
    xlab("")
  return(list(plot=p))
}

#' Plot the barplots of top majors in a course for the past six semesters
#'
#' @param colleges selected colleges
#' @param DFW_db DFW dataframe 
#' @param course course number
#' @param TYPE "DFW" or "rDFW"
#'
#' @return
#' @export
#'
#' @examples
barplot_by_course_Major_DFW <- function(
  colleges=all_colleges, DFW_db=course_Major_DFW,course=703, TYPE="rDFW"){
  all_terms <- unique(DFW_db$Term)
  num_all_terms <- length(all_terms)
  #only show the most recent 6 semesters
  terms <- all_terms[(num_all_terms-5):num_all_terms]
  
  ls <- sapply(terms,   function(term){
    top_Major_by_course_DFW(colleges,term, DFW_db, course, TYPE)})
  return(ls)
}



