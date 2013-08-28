dichot <-
function(x, c = 1) 
{
    x <- replace(x, x < c, 0)
    x <- replace(x, x >= c, 1)
    return(x)
}
