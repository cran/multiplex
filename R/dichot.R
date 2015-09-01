dichot <-
function (x, c = 1) 
{
    if (isTRUE(0 >= c) == TRUE) {
        x <- replace(x, x >= c, 1L)
        x <- replace(x, x < c, 0L)
    }
    else {
        x <- replace(x, x < c, 0L)
        x <- replace(x, x >= c, 1L)
    }
    x
}
