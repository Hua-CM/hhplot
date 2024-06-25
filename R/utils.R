# tweak saturation and birghtness
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cols PARAM_DESCRIPTION
#' @param sat Reduce the saturation value to this percent, Default: 0.5
#' @param bri Reduce the birghtness value to this percent, Default: 0.5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col_desat
#' @export 
col_desat <- function(cols, sat=0.5, bri=0.5) {
       X <- diag(c(1, sat, bri)) %*% rgb2hsv(col2rgb(cols))
       hsv(X[1,], X[2,], X[3,])
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @param border PARAM_DESCRIPTION, Default: 'light gray'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col_show
#' @export 
col_show <- function(col, border = "light gray", ...) {
        n <- length(col)
        plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
                    axes = FALSE, xlab = "", ylab = "", ...)
        rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
        text((0:(n-1)/n)+(1/(2*n)), rep(0.5,n), labels=col)
}

#' @title findoutliner
#' @description Find outlier sample in a vector
#' @param x The input
#' @return a vector including TRUE or FALSE. TRUE is outliner
#' @noRd
findoutliner <- function(x) {
  return(ifelse(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
                  x > quantile(x, 0.75) + 1.5 * IQR(x), TRUE, FALSE))
}

#' @title findoutliner2
#' @description Find outlier technique replicate in qPCR
#' @param x cq values
#' @return a vector including TRUE or FALSE. TRUE is outliner
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[tibble]{enframe}}
#' @rdname findoutliner2
#' @export 
#' @importFrom dplyr mutate filter distinct select bind_rows group_by summarise
#' @importFrom tibble deframe
findoutliner2 <-function(x){
  if(sd(x) < 0.3){
    return(rep(FALSE, length(x)))
  }
  else{
    df_dist <- pivot_longer(dist(x) %>%
                   as.matrix() %>%
                   as.data.frame() %>%
                   dplyr::mutate(idx1=rownames(.)),
                 cols = -idx1,
                 names_to = 'idx2',
                 values_to  = "distance") %>%
      dplyr::filter(idx1 != idx2) %>%
      dplyr::mutate(idx3=ifelse(idx1<idx2, idx1, idx2),
             idx4=ifelse(idx1<idx2, idx2, idx1)) %>%
      dplyr::distinct(idx3, idx4, .keep_all = TRUE) %>%
      dplyr::select(idx1=idx3,
                    idx2=idx4,
                    distance)
    vec_dist <- df_dist %>%
      dplyr::select(idx=idx1, distance) %>%
      dplyr::bind_rows(
        df_dist %>%
          select(idx=idx2, distance) 
      ) %>%
      dplyr::group_by(idx) %>%
      dplyr::summarise(avg_dist=mean(distance)) %>%
      tibble::deframe()
    return(vec_dist==max(vec_dist))
  }
}

