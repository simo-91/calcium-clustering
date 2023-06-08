# Notes:
# Using Fast Fourier Analysis
# Plotting Power Density Spectrum (PSD) in function of frequency (max f = Nyquist frequency = fs/2)
# Acquiring at 0.5Hz; so fs = 0.5Hz so Nyquist frequency = 0.25
# Lower frequency is 1/T where T = span of the whole timeseries considered (371 seconds in our case)
# Range to plot on is 0.0027 - 0.25 Hz

# HRASV12 4dpf

# Initialize an empty data frame
all_PSD_mean <- data.frame()

# Loop over the all_lists
for (list_name in names(all_lists)) {
  
  # Get the current list
  curr_list <- all_lists[[list_name]]
  
  # Find the arrays that contain "psd.mean" for total type
  total_psd_mean_names <- grep("psd.mean$", names(curr_list), value = TRUE)
  
  # Find the arrays that contain "psd.RFP.mean" for RFP type
  RFP_psd_mean_names <- grep("psd.RFP.mean$", names(curr_list), value = TRUE)
  
  # Combine the arrays
  psd_mean_names <- c(total_psd_mean_names, RFP_psd_mean_names)
  
  # Add the arrays to the all_hind_PSD_mean data frame
  for (arr_name in psd_mean_names) {
    # Extract the Genotype and Age from the list_name
    split_name <- strsplit(list_name, "_")[[1]]
    Genotype <- split_name[1]
    Age <- split_name[2]
    
    # Extract the ID, Condition and Type from the arr_name
    Type <- ifelse(grepl("RFP", arr_name), "RFP", "total")
    split_ID <- strsplit(arr_name, "_")[[1]]
    Condition <- paste(Genotype, Age, Type, sep = " ")
    Condition <- gsub("ID[0-9]+", "", Condition)
    
    # Calculate mean for each array
    mean_psd = mean(unlist(curr_list[[arr_name]]))
    
    # Create a data frame from the current array
    curr_df <- data.frame(Genotype = Genotype, Age = Age, Condition = Condition, Type = Type, "mean_psd" = mean_psd)
    
    # Add the current data frame to the all_hind_PSD_mean data frame
    all_PSD_mean <- rbind(all_PSD_mean, curr_df)
  }
}


# List of IDs
IDs <- c("ID0031", "ID0032", "ID0033", "ID0034", "ID0035", 
         "ID0036", "ID0037", "ID0058", "ID0059", "ID0060", 
         "ID0061", "ID0062", "ID0063", "ID0112", "ID0113", 
         "ID0114", "ID0115")

# List of conditions
areas <- c("hind", "mid")

# Initialize an empty dataframe
HRASV12_4dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)

# Loop through each condition and ID
for (area in areas) {
  for (ID in IDs) {
    
    # For all areas
    if(paste("HRASV12_4dpf_",area, sep="") %in% names(all_lists) && 
       paste(ID,"psd.mean",sep=".") %in% names(all_lists[[paste("HRASV12_4dpf_",area, sep="")]]) &&
       "PSD mean" %in% names(all_lists[[paste("HRASV12_4dpf_",area, sep="")]][[paste(ID,"psd.mean",sep=".")]])) {
      
      HRASV12_4dpf_PSD_df[paste("HRASV12_4dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("HRASV12_4dpf_",area, sep="")]][[paste(ID,"psd.mean",sep=".")]][["PSD mean"]]
    }
    
    # For RFP condition
    if(paste("HRASV12_4dpf_",area, sep="") %in% names(all_lists) && 
       paste(ID, "psd.RFP.mean", sep=".") %in% names(all_lists[[paste("HRASV12_4dpf_",area, sep="")]]) &&
       "PSD mean" %in% names(all_lists[[paste("HRASV12_4dpf_",area, sep="")]][[paste(ID, "psd.RFP.mean", sep=".")]])) {
      
      HRASV12_4dpf_PSD_df[paste("HRASV12_4dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("HRASV12_4dpf_",area, sep="")]][[paste(ID, "psd.RFP.mean", sep=".")]][["PSD mean"]]
    }
  }
}


library(tidyverse)
# Reshape the dataframe
HRASV12_4dpf_PSD_long.df <- HRASV12_4dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 
temp2 <- str_split(HRASV12_4dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
        rbind(temp2[[i]]) )
}

HRASV12_4dpf_PSD_long.df <- cbind(HRASV12_4dpf_PSD_long.df, result)
names(HRASV12_4dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")



# HRASV12 5dpf
# Initialize an empty dataframe
HRASV12_5dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)

# List of additional IDs
additional_IDs <- c("ID0064", "ID0065", "ID0066", "ID0067", "ID0068", "ID0116", "ID0117", "ID0118")

# Loop through each condition and ID
for (area in areas) {
  for (ID in c(IDs, additional_IDs)) {
    
    # For all areas
    if (paste("HRASV12_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.mean", sep = ".") %in% names(all_lists[[paste("HRASV12_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("HRASV12_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]])) {
      
      HRASV12_5dpf_PSD_df[paste("HRASV12_5dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("HRASV12_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]][["PSD mean"]]
    }
    
    # For RFP condition
    if (paste("HRASV12_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.RFP.mean", sep = ".") %in% names(all_lists[[paste("HRASV12_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("HRASV12_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]])) {
      
      HRASV12_5dpf_PSD_df[paste("HRASV12_5dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("HRASV12_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]][["PSD mean"]]
    }
  }
}

library(tidyverse)
# Reshape the dataframe
HRASV12_5dpf_PSD_long.df <- HRASV12_5dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 
temp2 <- str_split(HRASV12_5dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
                  rbind(temp2[[i]]) )
}

HRASV12_5dpf_PSD_long.df <- cbind(HRASV12_5dpf_PSD_long.df, result)
names(HRASV12_5dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")


# CTRL (both 4 and 5dpf)
# Initialize empty dataframes
CTRL_4dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)
CTRL_5dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)

# List of additional IDs for 4dpf and 5dpf
additional_IDs_4dpf <- c("ID0040", "ID0041", "ID0042", "ID0043", "ID0044", "ID0045", "ID0046", "ID0047")
additional_IDs_5dpf <- c("ID0048", "ID0049", "ID0050", "ID0051", "ID0052", "ID0053", "ID0054", "ID0055", "ID0056", "ID0057")

# Loop through each condition and ID for 4dpf
for (area in areas) {
  for (ID in c(IDs, additional_IDs_4dpf)) {
    # For all areas
    if (paste("CTRL_4dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.mean", sep = ".") %in% names(all_lists[[paste("CTRL_4dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("CTRL_4dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]])) {
      CTRL_4dpf_PSD_df[paste("CTRL_4dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("CTRL_4dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]][["PSD mean"]]
    }
    # For RFP condition
    if (paste("CTRL_4dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.RFP.mean", sep = ".") %in% names(all_lists[[paste("CTRL_4dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("CTRL_4dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]])) {
      CTRL_4dpf_PSD_df[paste("CTRL_4dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("CTRL_4dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]][["PSD mean"]]
    }
  }
}

# Loop through each condition and ID for 5dpf
for (area in areas) {
  for (ID in c(IDs, additional_IDs_5dpf)) {
    # For all areas
    if (paste("CTRL_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.mean", sep = ".") %in% names(all_lists[[paste("CTRL_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("CTRL_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]])) {
      CTRL_5dpf_PSD_df[paste("CTRL_5dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("CTRL_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]][["PSD mean"]]
    }
    # For RFP condition
    if (paste("CTRL_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.RFP.mean", sep = ".") %in% names(all_lists[[paste("CTRL_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("CTRL_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]])) {
      CTRL_5dpf_PSD_df[paste("CTRL_5dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("CTRL_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]][["PSD mean"]]
    }
  }
}


library(tidyverse)

# Reshape the 4dpf dataframe
CTRL_4dpf_PSD_long.df <- CTRL_4dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 

temp2 <- str_split(CTRL_4dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
                  rbind(temp2[[i]]) )
}

CTRL_4dpf_PSD_long.df <- cbind(CTRL_4dpf_PSD_long.df, result)
names(CTRL_4dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")

# Reshape the 5dpf dataframe
CTRL_5dpf_PSD_long.df <- CTRL_5dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 

temp2 <- str_split(CTRL_5dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
                  rbind(temp2[[i]]) )
}

CTRL_5dpf_PSD_long.df <- cbind(CTRL_5dpf_PSD_long.df, result)
names(CTRL_5dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")


# AKT1 4 and 5dpf
# Initialize empty dataframes
AKT1_4dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)
AKT1_5dpf_PSD_df <- data.frame("Frequency_Hz" = frequencies_x)

# List of additional IDs for 4dpf and 5dpf
additional_IDs_4dpf <- c("ID0119", "ID0120", "ID0121", "ID0122", "ID0123", "ID0124", "ID0125", "ID0126", "ID0127", "ID0128", "ID0129", "ID0133", "ID0134", "ID0136", "ID0137", "ID0138")
additional_IDs_5dpf <- c("ID0130", "ID0131", "ID0132", "ID0140", "ID0141", "ID0142", "ID0143")

# Loop through each condition and ID for 4dpf
for (area in areas) {
  for (ID in c(IDs, additional_IDs_4dpf)) {
    # For all areas
    if (paste("AKT1_4dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.mean", sep = ".") %in% names(all_lists[[paste("AKT1_4dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("AKT1_4dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]])) {
      AKT1_4dpf_PSD_df[paste("AKT1_4dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("AKT1_4dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]][["PSD mean"]]
    }
    # For RFP condition
    if (paste("AKT1_4dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.RFP.mean", sep = ".") %in% names(all_lists[[paste("AKT1_4dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("AKT1_4dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]])) {
      AKT1_4dpf_PSD_df[paste("AKT1_4dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("AKT1_4dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]][["PSD mean"]]
    }
  }
}

# Loop through each condition and ID for 5dpf
for (area in areas) {
  for (ID in c(IDs, additional_IDs_5dpf)) {
    # For all areas
    if (paste("AKT1_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.mean", sep = ".") %in% names(all_lists[[paste("AKT1_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("AKT1_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]])) {
      AKT1_5dpf_PSD_df[paste("AKT1_5dpf_", area, "_", ID, "_total", sep = "")] <- all_lists[[paste("AKT1_5dpf_", area, sep = "")]][[paste(ID, "psd.mean", sep = ".")]][["PSD mean"]]
    }
    # For RFP condition
    if (paste("AKT1_5dpf_", area, sep = "") %in% names(all_lists) &&
        paste(ID, "psd.RFP.mean", sep = ".") %in% names(all_lists[[paste("AKT1_5dpf_", area, sep = "")]]) &&
        "PSD mean" %in% names(all_lists[[paste("AKT1_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]])) {
      AKT1_5dpf_PSD_df[paste("AKT1_5dpf_", area, "_", ID, "_RFP", sep = "")] <- all_lists[[paste("AKT1_5dpf_", area, sep = "")]][[paste(ID, "psd.RFP.mean", sep = ".")]][["PSD mean"]]
    }
  }
}


library(tidyverse)

# Reshape the 4dpf dataframe
AKT1_4dpf_PSD_long.df <- AKT1_4dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 

temp2 <- str_split(AKT1_4dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
                  rbind(temp2[[i]]) )
}

AKT1_4dpf_PSD_long.df <- cbind(AKT1_4dpf_PSD_long.df, result)
names(AKT1_4dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")

# Reshape the 5dpf dataframe
AKT1_5dpf_PSD_long.df <- AKT1_5dpf_PSD_df %>%
  pivot_longer(cols = -Frequency_Hz,
               names_to = "Column",
               values_to = "Value") 

temp2 <- str_split(AKT1_5dpf_PSD_long.df$Column, pattern = "_") 
result <- data.frame()
for (i in 1:length(temp2)) {
  result <- rbind(result, 
                  rbind(temp2[[i]]) )
}

AKT1_5dpf_PSD_long.df <- cbind(AKT1_5dpf_PSD_long.df, result)
names(AKT1_5dpf_PSD_long.df) <- c("Frequency_Hz", "Condition", "PSD", "Genotype", "Age", "Area", "ID", "Type")



# Relative PSDs
df_list <- list(HRASV12_4dpf_PSD_long.df, 
                HRASV12_5dpf_PSD_long.df, 
                CTRL_4dpf_PSD_long.df, 
                CTRL_5dpf_PSD_long.df, 
                AKT1_4dpf_PSD_long.df, 
                AKT1_5dpf_PSD_long.df)

# Loop over the list
for (i in seq_along(df_list)) {
  df_list[[i]] <- df_list[[i]] %>%
    group_by(ID, Type) %>%
    mutate(Sum_PSD = sum(PSD)) %>%
    mutate(Relative_PSD = PSD / Sum_PSD) %>%
    ungroup()
}
names(df_list) <- c("HRASV12_4dpf_PSD_long.df", 
                    "HRASV12_5dpf_PSD_long.df", 
                    "CTRL_4dpf_PSD_long.df", 
                    "CTRL_5dpf_PSD_long.df", 
                    "AKT1_4dpf_PSD_long.df", 
                    "AKT1_5dpf_PSD_long.df")

list2env(df_list, envir = .GlobalEnv)

# GGplot

ggplot(HRASV12_4dpf_PSD_long.df, aes(x = Frequency_Hz, y = PSD, color = Type)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PSD vs Frequency_Hz",
       x = "Frequency_Hz",
       y = "PSD",
       color = "Type") +
  theme(legend.position = "bottom")



# all conditions
all_conds_PSD_long.df <- bind_rows(HRASV12_4dpf_PSD_long.df, 
                                   HRASV12_5dpf_PSD_long.df, 
                                   CTRL_4dpf_PSD_long.df, 
                                   CTRL_5dpf_PSD_long.df, 
                                   AKT1_4dpf_PSD_long.df, 
                                   AKT1_5dpf_PSD_long.df)

ggplot(all_conds_PSD_long.df, aes(x = Frequency_Hz, y = PSD, color = Genotype)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PSD vs Frequency_Hz",
       x = "Frequency_Hz",
       y = "PSD",
       color = "Type") +
  theme(legend.position = "bottom")
