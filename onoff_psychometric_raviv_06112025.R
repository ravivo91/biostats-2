# 06/11/25
rm(list = ls()) 

library(R.matlab)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(stringr)
library(dplyr)
library(psyphy) # for mAFC fitting
library(modelfree) # for threshold computation
library(BayesFactor)
library(patchwork)
library(tidyr)

platform <- R.version$platform

# Set the working directory based on the platform
if (grepl("mingw32", platform, ignore.case = TRUE)) {
  # Windows platform
  setwd("E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/CSV-20250427T192454Z-001/CSV/Results/100 Trials")
  pcorr_resultsfolder <- "E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/CSV-20250427T192454Z-001/CSV/Results/pcorr//"
  dprime_resultsfolder <- "E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/CSV-20250427T192454Z-001/CSV/Results/dprime/"
  corr_resultsfolder <- "E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/CSV-20250427T192454Z-001/CSV/Results/Correlation Graphs/"
  ax_length <- read_csv('E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results/Study Results/Lenstar Data/axial_length.csv')
} else if (grepl("apple", platform, ignore.case = TRUE)) {
  # macOS platform
  setwd('~/Desktop/Rstudio/Psychophysics Results/New_Results/Study Results/CSV/')
  ax_length <- read.csv('~/Desktop/Rstudio/Psychophysics Results/New_Results/Study Results/Lenstar Data/axial_length.csv')
  pcorr_resultsfolder <- "~//Desktop//Rstudio//Psychophysics Results//Results//pcorr//pcorr_"
  dprime_resultsfolder <- "~//Desktop//Rstudio//Psychophysics Results//Results//dprime//" 
} else {
  # Default or unknown platform
  warning("Platform not recognized. Working directory not set.")
}


ax_length_ctr <- 1

###These are some variables that are set before doing a plot...although i can't find the. plot that uses these values. Also some of these showing that they are creating data frames when certain analysis are being performed###
# control the type of analysis ####
# plot psymets?
plot_psymet <- TRUE
# plot d' versus contrast?
plot_dprime <- TRUE

# d' versus slope analysis?
dprime_slope_analysis <- TRUE
if (dprime_slope_analysis) {
  df_dp <- data.frame()
  df_dprime_a_b <- data.frame()
}

pcorrect_analysis <- TRUE  #### TODO? A pcorr analysis
if (pcorrect_analysis) {
  df_a_b <- data.frame()
}

# TRIAL DEPENDENCE - look at trial dependence individual differences ####
trial_dependence_analysis <- TRUE
if (trial_dependence_analysis) {
  df_t_dep <- data.frame()
}

###this looks in the folder and finds all of the files and creates a dataframe of all of their names###
# get a list of the psychometric function files
psy_files <- list.files('./')

#### loop over psy_file names to build a data frame ####

# constant for number of trials
df_full <- data.frame()

#this is a way to get all of the information from each file, so you start with the first file and then continue until all the files have been gone over#
for (tmp in 1:length(psy_files)) {
   print(psy_files[tmp])

    print(ax_length[ax_length_ctr,1])

    
    d_file <- read.csv(psy_files[tmp])
    d_file <- data.frame(d_file,stringsAsFactors = FALSE)
    
    #total number of points for each contrast level
    total_points <- nrow(d_file)
    n_trials <-  total_points / 7
    message("Total points: ", total_points, ";  Trials per condition: ", n_trials)
    allowed_trials <- c(75, 100, 125)
    if (!(n_trials %in% allowed_trials)) {
      stop(sprintf(
        "n_trials must be one of %s; but got %s (total_points = %s)",
        paste(allowed_trials, collapse = ", "),
        n_trials,
        total_points
      ))
    }
    
    d_file <- d_file[!(d_file$trial == "trial" & seq_along(d_file$trial)!= 1), ]
    if (str_split(psy_files[tmp],"--") == psy_files[tmp]){
      f_str <- str_split(psy_files[tmp],"-")
      f_str[[1]][2] = "ON"
    }else {
      f_str <-str_split(psy_files[tmp],"-")
      f_str <- paste(f_str[[1]][1], f_str[[1]][3], f_str[[1]][4], f_str[[1]][5], f_str[[1]][6],f_str[[1]][7])
      f_str <- str_split(f_str," ")
      f_str[[1]][2] = "OFF"
    }
    print(f_str)
    
    # parse the filename into subject, condition, distance
    subject <- f_str[[1]][1]
    condition <- f_str[[1]][2]
    distance <- f_str[[1]][3] #1=near,0=far
    if (distance==0){
      distance = "Far"
    }else {
      distance = "Near"
    }
    # Axial Length Counter
    if (subject != ax_length[[1]][ax_length_ctr]) {
      
      # attempt to find the row index for this subject
      new_idx <- match(subject, ax_length[[1]])
      
      if (!is.na(new_idx)) {
        # found it → jump the counter there
        ax_length_ctr <- new_idx
      } else {
        # not found → force NAs and skip any further lookups
        re     <- NA
        ax_od  <- NA
        ax_os  <- NA
        ax_len <- NA
        
        # you can either 'next' to go on to the next subject in your loop,
        # or just wrap everything below in an `else{}` block
        next
      }
    }
    print(subject)
    print(ax_length[[1]][ax_length_ctr])
    
    # get average RE for OD and OS
    re <- (ax_length[ax_length_ctr,2] + ax_length[ax_length_ctr,3])/2
    ax_od <- ax_length[ax_length_ctr,4]
    ax_os <- ax_length[ax_length_ctr,5]
    ax_len <- (ax_od + ax_os)/2
    data <- data.frame(d_file$contrast)
    contrast <- as.numeric(unique(d_file$contrast))
    
    df_tmp <- d_file %>% group_by(contrast,correct) %>% tally() %>%  ungroup() %>% complete(contrast, correct, fill = list(n = 0))
    df_tmp$contrast <- as.numeric(df_tmp$contrast) 
    df_tmp$correct <- as.numeric(df_tmp$correct)
    df_tmp$n <- as.numeric(df_tmp$n)
    df_ncorrect <- subset(df_tmp, correct == 1)
    subj_str <- rep(subject,length(contrasts))
    condition_str <- rep(condition,length(contrasts))
    distance_str <- rep(distance,length(contrasts))
    incorrect <- n_trials - df_ncorrect$n
    p_correct <- (df_ncorrect$n + 0.5) / (n_trials+1)
    d_prime <- sapply(p_correct, FUN = dprime.mAFC, m = 2)
    df_matfile <- data.frame(subj_str,re,ax_len,condition_str,distance_str, df_ncorrect$contrast, df_ncorrect$n, incorrect, d_prime) #this creates a data frame with all of the information, including d_prime
    tmp_data <- df_matfile
    colnames(tmp_data) <- c('subj','re','axial length','stim','distance','contrast','correct','incorrect','dprime') # this gives columns their names
    
    df_full <- rbind(df_full,df_matfile)#this creates a new data frame by putting the two rows together
    
    # DPRIME SLOPE ANALYSIS -- dprime slope analysis ####
    if (dprime_slope_analysis) {
        dp.glm <- glm(formula = dprime ~ log10(abs(contrast)), family = gaussian(), data = tmp_data)
        #print(summary(dp.glm))
        print(dp_intercept <- 10^(-1*dp.glm$coefficients[1]))
        print(dp_slope <- 10^(-1*dp.glm$coefficients[2]))
        ax_od <- ax_length[ax_length_ctr,4]
        ax_os <- ax_length[ax_length_ctr,5]
        ax_len <- (ax_od + ax_os)/2
        df_dp <- rbind(df_dp,data.frame(subject,re,ax_len,condition,distance,dp_slope,dp_intercept))
        
        contrast <- abs(tmp_data$contrast)
        xseq <- seq(min(abs(contrast)), max(abs(contrast)), len = 1000)
        dprime.pred <- predict(dp.glm, data.frame(contrast = xseq), type = "response",  se.fit = TRUE)
        # make a data frame of the prediction and name the variables
        dprime_pred_df <- data.frame(xseq, dprime.pred$fit)
        colnames(dprime_pred_df) <- c('contrast','fit')
        
        # pick out a d-prime threshold level
        thresh_level = .5 # arbitrary d-prime as thresh_level
        
        # get threshold and slope using the modelfree library
        dprime_a_b <- threshold_slope(dprime_pred_df$fit, dprime_pred_df$contrast, thresh_level) 
        # make a dataframe of threshold slope
        thresh <- dprime_a_b$x_th
        slope <- dprime_a_b$slope
        ax_od <- ax_length[ax_length_ctr,4]
        ax_os <- ax_length[ax_length_ctr,5]
        ax_len <- (ax_od + ax_os)/2
        print(thresh)
        print(slope)
        
        df_dprime_a_b <- rbind(df_dprime_a_b,data.frame(subject,re,ax_len,condition,distance,thresh,slope))
    }
    
    # PCORR ANALYSIS -- using model free ####
    if (pcorrect_analysis) {
        
        #n_trials <- 100
        m_choice <- 2    
        tmp_data <- df_matfile
        colnames(tmp_data) <- c('subj','re','ax_length','condition','distance','contrast','correct','incorrect','dprime')
        result = tryCatch({
            # fit appropriate glm to contrast versus p(correct)
            model <- glm(data = tmp_data, cbind(correct, n_trials-correct) ~ abs(contrast), family = binomial(mafc.probit(m_choice)))
            # make a variable from the contrast data frame
            contrast <- abs(tmp_data$contrast)
            # make an upsampled number of points from min to max contrast
            xseq <- seq(min(abs(contrast)), max(abs(contrast)), len = 1000)
            # generate a prediction from the fitted glm
            psymet_fit.pred <- predict(model, data.frame(contrast = xseq), type = "response",  se.fit = TRUE)
            # make a data frame of the prediction and name the variables
            psymet_pred_df <- data.frame(xseq, psymet_fit.pred$fit)
            colnames(psymet_pred_df) <- c('contrast','fit')
            
            # pick out a p(correct) threshold level
            thresh_level = .75 # arbitrary thresh_level
            
            # get threshold and slope using the modelfree library
            a_b <- threshold_slope(psymet_pred_df$fit, psymet_pred_df$contrast, thresh_level) 
            # make a dataframe of threshold slope
            thresh <- a_b$x_th
            slope <- a_b$slope
            ax_od <- ax_length[ax_length_ctr,4]
            ax_os <- ax_length[ax_length_ctr,5]
            ax_len <- (ax_od + ax_os)/2
            print(thresh)
            print(slope)
            df_a_b <- rbind(df_a_b,data.frame(subject,re,ax_len,condition,distance,thresh,slope))
        }, warning = function(w) {
            print('warning')
        }, error = function(e) {
            print('error')
        }, finally = {
            print('')
        }
        )
    }
    
    # SEQ DEPENDCIES ANALYSIS - v 3/20/20 ####
    #if (trial_dependence_analysis) {
         #data_tmp <- data # make a copy of the data
          
          # init contingency table of responses
          # cont_tbl <- c(0,0,0,0)
           #for (ctr in 1:(length(data$resp_int)-1)) {
            #   if (data$resp_int[ctr] == 1 & data$resp_int[ctr+1] == 1) {
             #      cont_tbl[1] = cont_tbl[1] + 1
              # } else if (data$resp_int[ctr] == 1 & data$resp_int[ctr+1] == 0) {
               #    cont_tbl[2] = cont_tbl[2] + 1
               #} else if (data$resp_int[ctr] == 0 & data$resp_int[ctr+1] == 1) {
              #     cont_tbl[3] = cont_tbl[3] + 1
               #  } else if (data$resp_int[ctr] == 0 & data$resp_int[ctr+1] == 0) {
                #   cont_tbl[4] = cont_tbl[4] + 1
               #}
          # }    
    #}
        
    
    # PCORR plots ####    
    if (plot_psymet) {
        #n_trials <- 100
        m_choice <- 2    
        # need a try / catch structure to catch failed fits 
        result = tryCatch({
        model <- glm(data = tmp_data, cbind(correct, n_trials-correct) ~ log10(abs(contrast)), family = binomial(mafc.probit(m_choice)))
        pp_g <- ggplot(data = tmp_data, aes(y = correct/n_trials, x = log10(abs(contrast)))) + geom_point(size = 4)
        contrast <- abs(tmp_data$contrast)
        xseq <- seq(min(abs(contrast)), max(abs(contrast)), len = 1000)  #I used, for example, a 1000 points
        yseq <- predict(model, data.frame(contrast = xseq), type = "response")
        curve <- data.frame(xseq, yseq)
        pp_g <- pp_g + geom_line(data = curve, aes(x = log10(xseq), y = yseq), size = 3) + ylim(0,1) + theme(text = element_text(size=20),axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16)) +  ggtitle(paste0(subj_str[1], " ", condition_str[1], " ", distance_str[1] , " ", thresh_level[1])) + xlab('log10(contrast)') + ylab('p(correct)')
        
        # need to uncomment these lines to make or save percent correct plots ####
        # make plots
        print(pp_g)
        # save graphs
        graph_fname <- paste0(pcorr_resultsfolder,subj_str[1], "_", condition_str[1], "_", distance[1] , "_" , thresh_level[1],".png")
        ggsave(graph_fname)
        }, warning = function(w) {
            print('warning')
        }, error = function(e) {
            print('error')
        }, finally = {
            print('No data lost.')
        }
        )
    }
    
    # dprime plots ####
    if (plot_dprime) {
        Asym <- 3
        xmid <- .3 
        scal <- 1

        if (distance =="Far") {
          xseq <- seq(0.05, 0.2, len = 100)  
        }else
          xseq <- seq(0.01, 0.1, len = 100)
        
        dp.glm <- glm(formula = dprime ~ log10(abs(contrast)), family = gaussian(), data = tmp_data)
        #dp.glm <- nls(formula = dprime ~ SSlogis(log(abs(contrast)), Asym, xmid, scal), data = tmp_data)
        dp.pred <- predict(dp.glm, newdata = data.frame(contrast = xseq), type = "response", se.fit = TRUE) 
        yseq <- dp.pred$fit[1:100]
        fit_df <- data.frame(xseq,yseq)
        p2 <- ggplot(data = tmp_data, aes(x = abs(contrast), y = dprime)) + geom_point(size = 3) + scale_x_log10() + ylim(-1.5,3.6) + geom_line(data = fit_df, aes(x = xseq, y = yseq), size = 1.5) + theme(text = element_text(size=20),axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16)) +  ggtitle(paste0(subj_str[1], " ", condition_str[1], " ", distance [1], " ", thresh_level[1]))
        # need to uncomment these lines to make or save d' plots ####
        graph_fname <- paste0(dprime_resultsfolder,subj_str[1], "_", condition_str[1], "_", distance[1],"_", thresh_level[1],".png")
        ggsave(graph_fname)
        print(p2) #### change to ggsave in a plots directory
    }

}

colnames(df_full) <- c('subject','re','ax_len',"condition",'distance', 'contrast','correct','incorrect','dprime')

if (dprime_slope_analysis) {
    rownames(df_dp) <- c()
    df_dp <- rbind(df_dp,data.frame(subject,re,ax_len,condition,distance,dp_slope,dp_intercept))
    colnames(df_dp) <- c('subject','re','ax_len','condition', 'distance','dp_slope','dp_intercept')
}

if (pcorrect_analysis) {
    rownames(df_a_b) <- c()
    df_dp <- rbind(df_a_b,data.frame(subject,re,ax_len,condition,distance,thresh,slope))
    colnames(df_a_b) <- c('subject','re','ax_len','condition','distance','threshold','slope')
}

# Now the processing of the raw is done. At this point "sanity checks" are required. For example, do the subject numbers and conditions make sense. Are there any input errors. The more checks and the more eyes at this stage the better. Anomalies can then be tracked down in the raw data files, if they exist. The output here has been shaped somewhat, but these outputs still contain all the raw data. This script, data files, and the processing could be shared with an interested potential collaborator. 

# Now the batch processing of raw data files is done, we should have a few manageable .csv files that are amenable to statistical analysis.

# Those files are save in the following lines. Good practice dictates that in addition to the saving of the data, what scripts these data files are directed to as input. 

# Here we save the full data frame. This ought to be merely a restructured version of the input data, here .mat files from our psychophysics experiment in matlab. It does not currently go to any further analysis scripts.

setwd('E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/Results')
write_csv(df_full, 'on_off_full_psychophysical_data.csv')
write_csv(df_a_b, 'on_off_threshold_psychophysical_data.csv')

# GENERAL ANALYSIS -- separate from raw data ####
    # subset out data for simple analysis pcorr ####
    
    # NOTE: R silently fails on some threshold computations. It believes it is doing us a favor by picking up and moving on happily. This can lead to puzzling numbers of of observations in the different conditions. A more robust strategy is to do explicit error checking in the fitting code above which was tried via try/catch above, but did not lead to the expected result. As of 8/26/20 there are: 284 for dog on, 269 for dog off, 300 observations for letter on, and 310 obs for letter off.
do_analysis_pcorr <- TRUE
do_analysis_dprime <- TRUE
if (do_analysis_pcorr) {
    # subset on far/near, off far/near
    df_a_b_far_on <- subset(df_a_b, (df_a_b$distance == 'Far') & (df_a_b$condition == 'ON'))
    df_a_b_far_off <- subset(df_a_b, (df_a_b$distance == 'Far') & (df_a_b$condition == 'OFF'))
    
    # subset letter on, letter off
    df_a_b_near_on <- subset(df_a_b, (df_a_b$distance == 'Near') & (df_a_b$condition == 'ON'))
    df_a_b_near_off <- subset(df_a_b, (df_a_b$distance == 'Near') & (df_a_b$condition == 'OFF'))

    colnames(df_a_b_far_off) <- c('subject','re','ax_len','condition','distance','threshold','slope')
    colnames(df_a_b_far_on) <- c('subject','re','ax_len','condition','distance','threshold','slope')
    colnames(df_a_b_near_off) <- c('subject','re','ax_len','condition','distance','threshold','slope')
    colnames(df_a_b_near_on) <- c('subject','re','ax_len','condition','distance','threshold','slope')
    
###RK Making Graphs Test###
    setwd("E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/Correlation Graphs")
    far_off <-ggplot(df_a_b_far_off, aes(x = ax_len, y = log10(threshold))) + 
      geom_point() + # This adds points to the plot
      theme_minimal() + # This applies a minimal theme to the plot
      labs(title = "Far Off Plot", 
           x = "Axial Length(mm)", 
           y = "Log(threshold)") + ylim(-2,-.5) + stat_smooth(method = 'lm')
    print(far_off)
    # save graphs
    graph_fname <- paste0("far_off", ".png")
    ggsave(graph_fname)
    
    far_on <- ggplot(df_a_b_far_on, aes(x = ax_len, y = log10(threshold))) + 
      geom_point() + # This adds points to the plot
      theme_minimal() + # This applies a minimal theme to the plot
      labs(title = "Far On Plot", 
           x = "Axial Length(mm)", 
           y = "Log(threshold)") + ylim(-2,-.5) + stat_smooth(method = 'lm')
    print(far_on)
    # save graphs
    graph_fname <- paste0("far_on", ".png")
    ggsave(graph_fname)
    
    near_off <- ggplot(df_a_b_near_off, aes(x = ax_len, y = log10(threshold))) + 
      geom_point() + # This adds points to the plot
      theme_minimal() + # This applies a minimal theme to the plot
      labs(title = "Near Off Plot ", 
           x = "Axial Length (mm)", 
           y = "Log(threshold)") + ylim(-2,-.5) + stat_smooth(method = 'lm')
    print(near_off)
    # save graphs
    graph_fname <- paste0("near_off", ".png")
    ggsave(graph_fname)
    
    near_on <-ggplot(df_a_b_near_on, aes(x = ax_len, y = log10(threshold))) + 
      geom_point() + # This adds points to the plot
      theme_minimal() + # This applies a minimal theme to the plot
      labs(title = "Near On Plot", 
           x = "Axial Length(mm)", 
           y = "Log(threshold)") + ylim(-2,-.5) + stat_smooth(method = 'lm')
    print(near_on)
    # save graphs
    graph_fname <- paste0("near_on", ".png")
    ggsave(graph_fname)
    
    
    
    # NB Because of the varying number of rows this data frame fails    
#    df_threshs <- data.frame(df_a_b_far_off$subject, df_a_b_far_off$ax_od, df_a_b_near_on$thresh, df_a_b_near_off$thresh, df_a_b_far_on$thresh, df_a_b_far_off$thresh)
#    colnames(df_threshs) <- c('subject', 'ax_length', 'far_on', 'far_off', 'near_on', 'near_off')
    #regressionBF(ax_length ~ l, data = df_threshs, whichModels = "all")

    
    # Combine all data frames ensuring the number of rows match by using merge
#merged_df <- merge(df_a_b_far_off[, c('subject', 'ax_len', 'threshold')], df_a_b_far_on[, c('subject', 'threshold')], by = 'subject', all = TRUE, suffixes = c('_far_off', '_far_on'))
#merged_df <- merge(merged_df, df_a_b_near_off[, c('subject', 'threshold')], by = 'subject', all = TRUE, suffixes = c('', '_near_off'))
#merged_df <- merge(merged_df, df_a_b_near_on[, c('subject', 'threshold')], by = 'subject', all = TRUE, suffixes = c('', '_near_on'))

# Rename columns to match the original intent
#colnames(merged_df) <- c('subject', 'ax_length', 'far_off', 'far_on', 'near_off', 'near_on')
#df_threshs <- merged_df

# Check if df_threshs is valid
#if (nrow(df_threshs) == 0) {
#  warning("Unable to create df_threshs due to empty data frames.")
#}
    
    df_threshs <- df_a_b_far_off %>%
      select(subject,
             ax_length = ax_len,
             far_off   = threshold) %>%
      full_join(
        df_a_b_far_on %>%
          select(subject,
                 far_on = threshold),
        by = "subject"
      ) %>%
      full_join(
        df_a_b_near_off %>%
          select(subject,
                 near_off = threshold),
        by = "subject"
      ) %>%
      full_join(
        df_a_b_near_on %>%
          select(subject,
                 near_on = threshold),
        by = "subject"
      )
    
    # sanity‐check
    if (nrow(df_threshs) == 0) {
      warning("df_threshs came back empty—check your source data frames.")
    }
    
    
  
    
    }

if (do_analysis_dprime) {
    # subset out data for simple analysis dprime ####
    
    # NB. Unlike the pcorr analysis the d-prime analysis does no error checking
    
    
    # subset FAR ON, FAR OFF
    df_dprime_a_b_far_on <- subset(df_dprime_a_b, (df_dprime_a_b$distance == 'Far') & (df_dprime_a_b$condition == 'ON'))
    df_dprime_a_b_far_off <- subset(df_dprime_a_b, (df_dprime_a_b$distance == 'Far') & (df_dprime_a_b$condition == 'OFF'))
    
    # subset NEAR ON, NEAR OFF
    df_dprime_a_b_near_on <- subset(df_dprime_a_b, (df_dprime_a_b$distance == 'Near') & (df_dprime_a_b$condition == 'ON'))
    df_dprime_a_b_near_off <- subset(df_dprime_a_b, (df_dprime_a_b$distance == 'Near') & (df_dprime_a_b$condition == 'OFF'))
    
# the un-addressed bug above here cause the creation of the d-prime data.fame to fail if the script is run clean ####
        
#### edited this to retain RE ####    
    df_dprime_threshs <- data.frame(df_dprime_a_b_near_on$subject, df_dprime_a_b_near_on$ax_len, df_dprime_a_b_near_on$re,                    df_dprime_a_b_far_on$threshold, df_dprime_a_b_far_off$threshold, df_dprime_a_b_near_on$threshold, df_dprime_a_b_near_off$threshold)
    colnames(df_dprime_threshs) <- c('subject', 'ax_length', 're', 'far_on', 'far_off', 'near_on', 'near_off')

    
}    

# Remove the subject 'RL026' from each data frame
#df_dprime_a_b_near_on_filtered <- subset(df_dprime_a_b_near_on, subject != "RL026")
#df_dprime_a_b_far_on_filtered <- subset(df_dprime_a_b_far_on, subject != "RL026")
#df_dprime_a_b_far_off_filtered <- subset(df_dprime_a_b_far_off, subject != "RL026")
#df_dprime_a_b_near_off_filtered <- subset(df_dprime_a_b_near_off, subject != "RL026")

# Ensure all filtered data frames have the same number of rows before creating the new data frame
#min_rows <- min(nrow(df_dprime_a_b_near_on_filtered), nrow(df_dprime_a_b_far_on_filtered), nrow(df_dprime_a_b_far_off_filtered), nrow(df_dprime_a_b_near_off_filtered))

#df_dprime_a_b_near_on_filtered <- df_dprime_a_b_near_on_filtered[1:min_rows, ]
#df_dprime_a_b_far_on_filtered <- df_dprime_a_b_far_on_filtered[1:min_rows, ]
#df_dprime_a_b_far_off_filtered <- df_dprime_a_b_far_off_filtered[1:min_rows, ]
#df_dprime_a_b_near_off_filtered <- df_dprime_a_b_near_off_filtered[1:min_rows, ]

# Create the new data frame after filtering
df_dprime_threshs <- data.frame(
  df_dprime_a_b_near_on$subject,
  df_dprime_a_b_near_on$ax_OD,
  df_dprime_a_b_near_on$re_OD,
  df_dprime_a_b_far_on$thresh,
  df_dprime_a_b_far_off$thresh,
  df_dprime_a_b_near_on$thresh,
  df_dprime_a_b_near_off$thresh
)

# Set column names
colnames(df_dprime_threshs) <- c('subject', 'ax_length', 're', 'far_on', 'far_off', 'near_on', 'near_off')

dp_mean <- aggregate(. ~ subject, data = df_dprime_threshs, mean)
dp_sd <- aggregate(. ~ subject, data = df_dprime_threshs, sd)
colnames(dp_sd) <- c('subject','ax_length','letter_on_sd', 'letter_off_sd', 'dog_on_sd', 'dog_off_sd')


# The code below can throw on within subject standard error bars

# Remove the subject 'RL026' from each data frame
# Remove the subject 'RL026' from each data frame
df_near_on_filtered <- subset(df_dprime_a_b_near_on, subject != "RL026")
df_far_on_filtered <- subset(df_dprime_a_b_far_on, subject != "RL026")
df_far_off_filtered <- subset(df_dprime_a_b_far_off, subject != "RL026")
df_near_off_filtered <- subset(df_dprime_a_b_near_off, subject != "RL026")

# Ensure all filtered data frames have the same number of rows before creating the new data frame
min_rows <- min(nrow(df_near_on_filtered), nrow(df_far_on_filtered), nrow(df_far_off_filtered), nrow(df_near_off_filtered))

df_near_on_filtered <- df_near_on_filtered[1:min_rows, ]
df_far_on_filtered <- df_far_on_filtered[1:min_rows, ]
df_far_off_filtered <- df_far_off_filtered[1:min_rows, ]
df_near_off_filtered <- df_near_off_filtered[1:min_rows, ]

# Create the new data frame after filtering
df_analysis <- data.frame(
  subject = df_dprime_a_b_near_on$subject,
  ax_length = df_dprime_a_b_near_on$ax_OD,
  re = df_dprime_a_b_near_on$re_OD,
  near_on = df_dprime_a_b_near_on$thresh,
  near_off = df_dprime_a_b_near_off$thresh,
  far_on = df_dprime_a_b_far_on$thresh,
  far_off = df_dprime_a_b_far_off$thresh
)

# Set column names appropriately
colnames(df_analysis) <- c(
  'subject', 'ax_length', 're',
  'near_on', 'near_off', 'far_on', 'far_off'
)

# Load necessary libraries
library(ggplot2)
library(knitr)
library(rmarkdown)

# Set directory to save plots and results
dir_path <- "E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/New_Results_2/Results"
output_file <- file.path(dir_path, "Statistical_Results.txt")

# Start capturing console output
sink(output_file)

# Far ON
cat("### Far ON Correlation ###\n")
print(cor.test(df_analysis$ax_length, log10(df_analysis$far_on)))
cat("\n\n")

# Far OFF
cat("### Far OFF Correlation ###\n")
print(cor.test(df_analysis$ax_length, log10(df_analysis$far_off)))
cat("\n\n")

# Relative Sensitivity Analysis - Far ON/OFF
cat("### Far Relative Sensitivity Analysis ###\n")
far_on_off_contrast <- (df_analysis$far_on - df_analysis$far_off) / (df_analysis$far_on + df_analysis$far_off)
print(cor.test(df_analysis$ax_length, far_on_off_contrast))
cat("\n\n")

# Near ON
cat("### Near ON Correlation ###\n")
print(cor.test(df_analysis$ax_length, log10(df_analysis$near_on)))
cat("\n\n")

# Near OFF
cat("### Near OFF Correlation ###\n")
print(cor.test(df_analysis$ax_length, log10(df_analysis$near_off)))
cat("\n\n")

# Relative Sensitivity Analysis - Near ON/OFF
cat("### Near Relative Sensitivity Analysis ###\n")
near_on_off_contrast <- (df_analysis$near_on - df_analysis$near_off) / (df_analysis$near_on + df_analysis$near_off)
print(cor.test(df_analysis$ax_length, near_on_off_contrast))
cat("\n\n")

# Stop capturing console output
sink()

# Save the analysis data frame as a formatted table
analysis_table <- kable(df_analysis, format = "markdown", caption = "Filtered Analysis Data Frame")
write(analysis_table, file = file.path(dir_path, "Analysis_Data_Frame.md"))

# Far ON Plot
(g_far_on <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(far_on))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Far ON Correlation with Axial Length'))
ggsave(filename = file.path(dir_path, "Far_ON_Correlation.png"), plot = g_far_on)

# Far OFF Plot
(g_far_off <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(far_off))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Far OFF Correlation with Axial Length'))
ggsave(filename = file.path(dir_path, "Far_OFF_Correlation.png"), plot = g_far_off)

# Far ON/OFF Contrast Plot
(g_far_contrast <- ggplot(data = df_analysis, aes(x = ax_length, y = far_on_off_contrast)) +
    xlim(21, 28) + ylim(-0.3, 0.3) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab('OFF/ON Index') +
    ggtitle('Far Relative Sensitivity with Axial Length'))
ggsave(filename = file.path(dir_path, "Far_Relative_Sensitivity_Correlation.png"), plot = g_far_contrast)

# Near ON Plot
(g_near_on <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(near_on))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Near ON Correlation with Axial Length'))
ggsave(filename = file.path(dir_path, "Near_ON_Correlation.png"), plot = g_near_on)

# Near OFF Plot
(g_near_off <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(near_off))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Near OFF Correlation with Axial Length'))
ggsave(filename = file.path(dir_path, "Near_OFF_Correlation.png"), plot = g_near_off)

# Near ON/OFF Contrast Plot
(g_near_contrast <- ggplot(data = df_analysis, aes(x = ax_length, y = near_on_off_contrast)) +
    xlim(21, 28) + ylim(-0.3, 0.3) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab('OFF/ON Index') +
    ggtitle('Near Relative Sensitivity with Axial Length'))
ggsave(filename = file.path(dir_path, "Near_Relative_Sensitivity_Correlation.png"), plot = g_near_contrast)


# Analysis Tables
#Normality
# Create a summary table with detailed statistical information
summary_results <- data.frame(
  Analysis = c(
    "Far ON Correlation",
    "Far OFF Correlation",
    "Far Relative Sensitivity",
    "Near ON Correlation",
    "Near OFF Correlation",
    "Near Relative Sensitivity"
  ),
  Estimate = c(
    cor.test(df_analysis$ax_length, log10(df_analysis$far_on))$estimate,
    cor.test(df_analysis$ax_length, log10(df_analysis$far_off))$estimate,
    cor.test(df_analysis$ax_length, far_on_off_contrast)$estimate,
    cor.test(df_analysis$ax_length, log10(df_analysis$near_on))$estimate,
    cor.test(df_analysis$ax_length, log10(df_analysis$near_off))$estimate,
    cor.test(df_analysis$ax_length, near_on_off_contrast)$estimate,
  ),
  p_value = c(
    cor.test(df_analysis$ax_length, log10(df_analysis$far_on))$p.value,
    cor.test(df_analysis$ax_length, log10(df_analysis$far_off))$p.value,
    cor.test(df_analysis$ax_length, far_on_off_contrast)$p.value,
    cor.test(df_analysis$ax_length, log10(df_analysis$near_on))$p.value,
    cor.test(df_analysis$ax_length, log10(df_analysis$near_off))$p.value,
    cor.test(df_analysis$ax_length, near_on_off_contrast)$p.value
  ),
  Confidence_Interval_Lower = c(
    cor.test(df_analysis$ax_length, log10(df_analysis$far_on))$conf.int[1],
    cor.test(df_analysis$ax_length, log10(df_analysis$far_off))$conf.int[1],
    cor.test(df_analysis$ax_length, far_on_off_contrast)$conf.int[1],
    cor.test(df_analysis$ax_length, log10(df_analysis$near_on))$conf.int[1],
    cor.test(df_analysis$ax_length, log10(df_analysis$near_off))$conf.int[1],
    cor.test(df_analysis$ax_length, near_on_off_contrast)$conf.int[1]
  ),
  Confidence_Interval_Upper = c(
    cor.test(df_analysis$ax_length, log10(df_analysis$far_on))$conf.int[2],
    cor.test(df_analysis$ax_length, log10(df_analysis$far_off))$conf.int[2],
    cor.test(df_analysis$ax_length, far_on_off_contrast)$conf.int[2],
    cor.test(df_analysis$ax_length, log10(df_analysis$near_on))$conf.int[2],
    cor.test(df_analysis$ax_length, log10(df_analysis$near_off))$conf.int[2],
    cor.test(df_analysis$ax_length, near_on_off_contrast)$conf.int[2]
  )
)

# Save summary results as a CSV file
write.csv(summary_results, file = file.path(dir_path, "Summary_Results.csv"), row.names = FALSE)

# Save the analysis data frame as a formatted table
analysis_table <- kable(df_analysis, format = "markdown", caption = "Filtered Analysis Data Frame")
write(analysis_table, file = file.path(dir_path, "Analysis_Data_Frame.md"))
#############################Graphs with P Values#######################
# Load necessary libraries
library(ggplot2)
library(knitr)
library(rmarkdown)

# Example: Far ON Plot
cor_far_on <- cor.test(df_analysis$ax_length, log10(df_analysis$far_on))
r_value_far_on <- round(cor_far_on$estimate, 2)
p_value_far_on <- round(cor_far_on$p.value, 4)

(g_far_on <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(far_on))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Far ON Correlation with Axial Length') +
    annotate("text", x = 22, y = -1.9, label = paste("r =", r_value_far_on, "\n", "p =", p_value_far_on), hjust = 0))
ggsave(filename = file.path(dir_path, "Far_ON_Correlation.png"), plot = g_far_on)

# Example: Far OFF Plot
cor_far_off <- cor.test(df_analysis$ax_length, log10(df_analysis$far_off))
r_value_far_off <- round(cor_far_off$estimate, 2)
p_value_far_off <- round(cor_far_off$p.value, 4)

(g_far_off <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(far_off))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Far OFF Correlation with Axial Length') +
    annotate("text", x = 22, y = -1.9, label = paste("r =", r_value_far_off, "\n", "p =", p_value_far_off), hjust = 0))
ggsave(filename = file.path(dir_path, "Far_OFF_Correlation.png"), plot = g_far_off)

# Example: Far ON/OFF Contrast Plot
cor_far_on_off_contrast <- cor.test(df_analysis$ax_length, far_on_off_contrast)
r_value_far_on_off_contrast <- round(cor_far_on_off_contrast$estimate, 2)
p_value_far_on_off_contrast <- round(cor_far_on_off_contrast$p.value, 4)

(g_far_contrast <- ggplot(data = df_analysis, aes(x = ax_length, y = far_on_off_contrast)) +
    xlim(21, 28) + ylim(-0.3, 0.3) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab('OFF/ON Index') +
    ggtitle('Far Relative Sensitivity with Axial Length') +
    annotate("text", x = 22, y = 0.2, label = paste("r =", r_value_far_on_off_contrast, "\n", "p =", p_value_far_on_off_contrast), hjust = 0))
ggsave(filename = file.path(dir_path, "Far_Relative_Sensitivity_Correlation.png"), plot = g_far_contrast)

# Example: Near ON Plot
cor_near_on <- cor.test(df_analysis$ax_length, log10(df_analysis$near_on))
r_value_near_on <- round(cor_near_on$estimate, 2)
p_value_near_on <- round(cor_near_on$p.value, 4)

(g_near_on <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(near_on))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Near ON Correlation with Axial Length') +
    annotate("text", x = 22, y = -1.9, label = paste("r =", r_value_near_on, "\n", "p =", p_value_near_on), hjust = 0))
ggsave(filename = file.path(dir_path, "Near_ON_Correlation.png"), plot = g_near_on)

# Example: Near OFF Plot
cor_near_off <- cor.test(df_analysis$ax_length, log10(df_analysis$near_off))
r_value_near_off <- round(cor_near_off$estimate, 2)
p_value_near_off <- round(cor_near_off$p.value, 4)

(g_near_off <- ggplot(data = df_analysis, aes(x = ax_length, y = log10(near_off))) +
    ylim(-2, -1) + xlim(21, 28) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab(expression(log[10](threshold))) +
    ggtitle('Near OFF Correlation with Axial Length') +
    annotate("text", x = 22, y = -1.9, label = paste("r =", r_value_near_off, "\n", "p =", p_value_near_off), hjust = 0))
ggsave(filename = file.path(dir_path, "Near_OFF_Correlation.png"), plot = g_near_off)

# Calculate the correlation for Near ON/OFF Contrast
cor_near_on_off_contrast <- cor.test(df_analysis$ax_length, near_on_off_contrast)
r_value_near_on_off_contrast <- round(cor_near_on_off_contrast$estimate, 2)
p_value_near_on_off_contrast <- round(cor_near_on_off_contrast$p.value, 4)

# Create the Near ON/OFF Contrast Plot with Annotations
(g_near_contrast <- ggplot(data = df_analysis, aes(x = ax_length, y = near_on_off_contrast)) +
    xlim(21, 28) + ylim(-0.3, 0.3) + geom_point() +
    stat_smooth(method = 'lm', color = 'black') +
    xlab('Axial Length (mm)') + ylab('ON/OFF Sensitivity Index') +
    ggtitle('Near Relative Sensitivity with Axial Length') +
    annotate("text", x = 22, y = 0.2, label = paste("r =", r_value_near_on_off_contrast, "\n", "p =", p_value_near_on_off_contrast), hjust = 0))
ggsave(filename = file.path(dir_path, "Near_Relative_Sensitivity_Correlation.png"), plot = g_near_contrast)

# -------------------------------
# Build a correlation‐results table
# -------------------------------
# define each target series
conds <- list(
  Far_ON               = log10(df_analysis$far_on),
  Far_OFF              = log10(df_analysis$far_off),
  Far_ON_OFF_Contrast  = df_analysis$far_on_off_contrast,
  Near_ON              = log10(df_analysis$near_on),
  Near_OFF             = log10(df_analysis$near_off),
  Near_ON_OFF_Contrast = df_analysis$near_on_off_contrast
)

# run cor.test on each one
corr_list <- lapply(names(conds), function(cond_name) {
  x <- df_analysis$ax_length
  y <- conds[[cond_name]]
  res <- cor.test(x, y)
  data.frame(
    condition   = cond_name,
    r           = round(as.numeric(res$estimate),  2),
    p           = signif(res$p.value, 4),
    t_stat      = round(res$statistic,  2),
    df          = as.integer(res$parameter),
    ci_lower    = round(res$conf.int[1], 2),
    ci_upper    = round(res$conf.int[2], 2),
    row.names   = NULL,
    stringsAsFactors = FALSE
  )
})

# bind into one data.frame
corr_table <- do.call(rbind, corr_list)

# print as markdown table
library(knitr)
kable(
  corr_table,
  caption = "Correlation of Axial Length with Each Condition",
  col.names = c("Condition", "r", "p-value", "t", "df", "CI Lo", "CI Hi")
)

# optionally save to CSV
write.csv(
  corr_table,
  file.path(dir_path, "correlation_results_table.csv"),
  row.names = FALSE
)

