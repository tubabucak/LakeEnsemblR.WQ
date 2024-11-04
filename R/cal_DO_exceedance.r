#'@description
#' Calculates the percentage time exceeded curve for O2 (similar approach to Flow Duration Curve)

#' @name cal_DO_exceedance
#' @param oxy_data dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.
#' @param depth numeric; Specific depth that the data should be extracted. It should be the depths that are available in the oxy_data: Like 1.0, 1.5 etc
#' @return dataframe with DO (mgL) and Probability of exceedance in the second column.

#' Example usage cal_DO_exceedance(oxy_data, depth = 3)
#' @export

cal_DO_exceedance<- function(oxy_data, depth = 0){

    depth_col <- paste0("Depth_", depth)

# Check if the depth is existed in the data
if (!(depth_col %in% names(oxy_data))) {
    stop("The depth column is not found in the data.")
    }
# Select the specified depth
    oxy_data_sub <- oxy_data[[depth_col]]

# These steps for calculating exceedance probability    
    oxy_data_sub<-na.omit(oxy_data_sub)
    oxy_data_sub<-sort(oxy_data_sub,decreasing=T)

    oxy_exceedance<-data.frame(exceedance=100/length(oxy_data_sub[depth])*1:length(oxy_data_sub[depth]),
                           DO = oxy_data_sub)

    return(oxy_exceedance)
}

