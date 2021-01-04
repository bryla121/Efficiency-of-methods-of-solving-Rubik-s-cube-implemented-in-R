#####
# orientation
# CORNERS
# 0 - correct
# 1 - one turn clockwise
# 2 - two turns clockwise (one counter clockwise)
#po wrzuceniu na odpowiednie miesjce jaki obrot byl wykonany, aby osiagnac ten stan 
# EDGES
# 0 - correct
# 1 - turnd incorrectly
#MIDDLE - mozna ulepszyc za pomoca middle moves
# We use the most common definition for orientation, so a correctly oriented edge can be solved in <U
# R F2 D L B2> and a correctly oriented corner can be solved in <U R2 F2 D L2 B2>.
#####

library("cubing", lib.loc="~/R/win-library/3.6")

my <- getCubieCube("Solved")
plot(my)

####movinG
my <- move(my, moves = scramble1199)
plot(my)


check_counter <- CFOP(my,0)$counter


results_fully_opt_CFOP <- vector(length=3000)


for (i in 1:3000)
{
  x <- getCubieCube("Solved")
  sc <- eval(parse(text=paste("scramble",i,sep = "")))
  x <- move(x, moves = sc)
  
  results_fully_opt_CFOP[i] <- fully_opt_CFOP(x,0)$counter
  
  if (i %% 25 == 0)
    print(i)
  
  i = i + 1
  
}


write.csv(results_fully_opt_CFOP, file ="results_fully_opt_CFOP.csv", row.names=FALSE)




test_check_solve <- function()
{
  solved <- getCubieCube("Solved")
  for (i in 1:3000)
  {
    x <- getCubieCube("Solved")
    sc <- eval(parse(text=paste("scramble",i,sep = "")))
    x <- move(x, moves = sc)
    x <- fully_opt_cross(x,0)$cube
    x <- fully_opt_F2L(x,0)$cube
    x <- OLL(x,0)$cube
    x <- PLL(x,0,0)$cube
    if (!(x == solved))
    {
      print(i)
    }
  }
}




test_check_solve()



