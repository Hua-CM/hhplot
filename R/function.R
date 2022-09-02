# tweak saturation and birghtness
desat <- function(cols, sat=0.5, bri=0.5) {
       library(colorspace)
       X <- diag(c(1, sat, bri)) %*% rgb2hsv(col2rgb(cols))
       hsv(X[1,], X[2,], X[3,])
}
# show color
pal <- function(col, border = "light gray", ...) {
        n <- length(col)
        plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
                    axes = FALSE, xlab = "", ylab = "", ...)
        rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
   }