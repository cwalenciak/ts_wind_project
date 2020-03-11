library(ncdf4)
library(lubridate)


#******************************************************************************
# Creates a dataframe from the nc data
#******************************************************************************
# nrow:
nc_to_df <- function(nc_data, nrow, file_date = "1/1/1900"){
    
    # Extract Data from nc file
    time_offset <- ncvar_get(nc_data, "time_offset")
    h_windspeed <- ncvar_get(nc_data, "horizontal_wspd")
    h_wdir <- ncvar_get(nc_data, "horizontal_wdir")
    air_pressure <- ncvar_get(nc_data, "air_pressure")
    rel_humid <- ncvar_get(nc_data, "relative_humidity")
    
    df <- data.frame( time_offset
                      , h_windspeed[nrow,]
                      , h_wdir[nrow,]
                      , air_pressure
                      , rel_humid
    )
    
    # Change column names
    names(df) <- c( "time_offset"
                    , "horizontal_wspd"
                    , "horizontal_wdir"
                    , "air_pressure"
                    , "relative_humidity"
    )
    
    # Add date from file to df
    df$file_date <- file_date
    
    return(df)
    
}


#******************************************************************************
# Averages the data in to a given time measurement, ex. 1min, 5mins, 10mins...
#******************************************************************************
# time_col: Column that has the time you are basing your observations on
# time_span: Total observation time for a given set of data (e.x. LIDAR data is 1 day)
# time_interval: What you want to convert your time to. Units are ambigous?
time_measurment_change <- function(df, time_col, time_span, time_interval, keep_cols){
    
    df$group <- 0
    
    start_seq = 1
    end_seq = time_interval + 5
    loop_interval = time_span/time_interval
    
    for(i in 1:loop_interval){
        
        # Goes through index of Dataframe
        for(j in start_seq:end_seq) {
            
            minute_dif <- df[j, time_col] - (time_interval * i)
            
            minute_dif_next = 0
            
            if (j < end_seq){
                minute_dif_next <- df[j + 1, time_col] - (time_interval * i)  
            }
            
            if( (minute_dif < 1 & minute_dif > -1) | (minute_dif < -1 & minute_dif_next >= 1) ){
                df[c(seq(start_seq, j)), ]$group <- i
                start_seq = j + 1
                end_seq = start_seq + time_interval + 5
                break
            } 
        }
    }  
    
    x <- df[ , append(keep_cols, "group")]
    x <- aggregate(x, by=list(x$group), FUN=mean, na.rm = T)
    
    return(x)
    
}


#******************************************************************************
# Extracts date from time units
#******************************************************************************
extract_file_date <- function(nc){
    
    x <-nc$dim[1]$time$units
    x <- substr(x, 15, 24)
    x <- as.POSIXlt(x, format = "%Y-%m-%d")
    
    return(x)
}


#******************************************************************************
# Writes data to working directory in a text file
#******************************************************************************
write_to_txt <- function(data, file_name){
    sink(paste0(file_name, ".txt"))
    print(data)
    sink()
}


#******************************************************************************
# Writes data to working directory in a text file
#******************************************************************************

combine_files <- function(flist, fname, time_col, time_span, time_interval, nrow, keep_cols){
    
    file_date <- ""
    
    combined_df <- data.frame()
    combined_list  <- list()
    
    incr = 0
    
    for(i in flist){
        
        incr = incr + 1
        
        nc <- nc_open(paste0(fname, "/", i))
        
        df <- nc_to_df(nc, nrow)
        
        if(time_interval > 0){
            new_df <- time_measurment_change(df, time_col, time_span, time_interval, keep_cols)
        } else {
            new_df <- df
        }
        
        file_date <- extract_file_date(nc)
        
        combined_list[[as.character(file_date)]] <- new_df
        
        new_df$file_date <- file_date
        combined_df <- rbind(combined_df, new_df)
        
        nc_close(nc)
        
        print(paste0("File Index: ", i, "   complete: ", incr/length(flist) * 100, "%"))
        
    }
    
    return(list(combined_df, combined_list))
    
}




#******************************************************************************
# Calculate ASE Score Cross Validation
#******************************************************************************

wind_ase_score <- function(flist, s, d, horizon, p_max = 5, q_max = 2){
    
    df <- data.frame()
    
    for(i in 1:length(flist)){
        
        x_name <- names(flist[i])
        x <- flist[[i]]$horizontal_wspd
        
        est_data <- x
       
        if(d > 0){
            for(i in 1:d){
                est_data <- artrans.wge(est_data, phi.tr = 1)
            }
        }
        
        # Get p and q
        aic_type <- c('aic', 'bic')
        pq_list <- list()
        
        incr <- 1
        
        for(i in aic_type){
            aic_x <- aic5.wge(est_data, p = 0:p_max, q = 0:q_max, type = as.character(i))
            
            for(j in 1:5){
                pq_list[[incr]] <- c(aic_x[[1]][j], aic_x[[2]][j])
                incr <- incr + 1
            }
        }
        
        pq_list <- unique(pq_list)

        # Estimate   
        for(i in 1:length(pq_list)){
            
            # Train
            est_x <- est.arma.wge(est_data, p = pq_list[[i]][1], q = pq_list[[i]][2])
            
            # Test
            ase_sum <- 0
            
            for(j in flist){
                x_fore <- fore.aruma.wge(j[,2], phi = est_x$phi, theta = est_x$theta, d = d, limits = F, 
                                         n.ahead = horizon, lastn = T, plot = F)
                
                length(x_fore$f)
                length(j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])])
                err <- x_fore$f  - j[,2][((length(j[,2]) - horizon) + 1):length(j[,2])]
                ase_sum <- ase_sum + mean(err^2)
            }
            
            ase_sum <- ase_sum/length(flist)
            
            new_df <- data.frame(x_name, pq_list[[i]][1], pq_list[[i]][2], s, d, horizon, ase_sum)
  
            df <- rbind(df, new_df)
        }
    }
    
    names(df) <- c("train_data", "p", "q", "s", "d", "horizon", "Mean ASE")
    
    return(df)   
}



#******************************************************************************
# Splits a Data frame into multiple training sets. Returns a list
#******************************************************************************
split_to_train <- function(x, train_size, spacer = 1){
    
    final_df <- data.frame()
    new_list <- list()
    
    for(i in seq(1, (dim(x)[1] - train_size + 1), by = spacer)){
        
        end_pos <- (i + train_size - 1)
        
        if( end_pos > dim(x)[1]){
            end_pos <- dim(x)[1]
        }
        
        new_list[[ as.character(paste0(i,":",end_pos))]] <- x[(i:end_pos),]
        print(i)
    }
    return(new_list)
}
