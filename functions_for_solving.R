  library("cubing", lib.loc="~/R/win-library/3.6")
  
  
  WR_cross <- function(x,c)     ###WHITE-RED EDGE
  {
    if ( x$ep["UR"]== 5)      #1
    {
      if (x$eo["UR"]==0)
      {
        x <- x
        c <- c
        # dobre miejsce i dobra orientacja
      } 
      else
      {
        x <- move(x, moves = "R'UF'U'")
        c = c+4
        # dobre miejsce zla orientacja 
      }
    }
    else
    {
      if (x$ep["FR"]==5)      #2
      {
        if (x$eo["FR"]==0)
        {
          x <- move(x, moves = "R")
          c <- c+1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "UF'U'")
          c <- c+3
          #orientacja z³a
        }
      }
      else if (x$ep["FL"]==5) #3
      {
        if (x$eo["FL"]==0)
        {
          x <- move(x, moves = "F2RF2")
          c <- c+3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "UFU'")
          c <- c+3
        }
      }
      else if (x$ep["BL"]==5) #4
      {
        if (x$eo["BL"]==0)
        {
          x <- move(x, moves = "B2R'B2")
          c <- c+3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U'B'U")
          c <- c+3
        }
      }    
      else if (x$ep["BR"]==5) #5
      {
        if (x$eo["BR"]==0)
        {
          x <- move(x, moves = "R'")
          c <- c+1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U'BU")
          c <- c+3
        }
      }   
      else if (x$ep["UF"]==5) #6
      {
        if (x$eo["UF"]==0)
        {
          x <- move(x, moves = "F'UFU'")
          c <- c+4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "FR")
          c <- c+2
        }
      }   
      else if (x$ep["UL"]==5) #7
      {
        if (x$eo["UL"]==0)
        {
          x <- move(x, moves = "LU2L'U2")
          c <- c+4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "LUFU'")
          c <- c+4
        }
      }   
      else if (x$ep["UB"]==5) #8
      {
        if (x$eo["UB"]==0)
        {
          x <- move(x, moves = "BU'B'U")
          c <- c+4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "B'R'")
          c <- c+2
        }
      }   
      else if (x$ep["DR"]==5) #9
      {
        if (x$eo["DR"]==0)
        {
          x <- move(x, moves = "R2")
          c <- c+1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "D'F'RF")
          c <- c+4
        }
      }   
      else if (x$ep["DF"]==5) #10
      {
        if (x$eo["DF"]==0)
        {
          x <- move(x, moves = "DR2'")
          c <- c+2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "F'RF")
          c <- c+3
        }
      }   
      else if (x$ep["DL"]==5) #11
      {
        if (x$eo["DL"]==0)
        {
          x <- move(x, moves = "D2R2")
          c <- c+2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "DF'RF")
          c <- c+4
        }
      }   
      else if (x$ep["DB"]==5) #12
      {
        if (x$eo["DB"]==0)
        {
          x <- move(x, moves = "D'R2")
          c <- c+2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "BR'B'")
          c <- c+3
        }
      }   
    }
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  
  WG_cross <- function(x,c)     ###WHITE-GREEN EDGE
  {
    if ( x$ep["UF"]== 6)      #1
    {
      if (x$eo["UF"]==0)
      {
        x <- x
        c <- c
        # dobre miejsce i dobra orientacja
      } 
      else
      {
        x <- move(x, moves = "FU'RU")
        c <- c + 4
        # dobre miejsce zla orientacja 
      }
    }
    else
    {
      if (x$ep["UR"]==6)      #2
      {
        if (x$eo["UR"]==0)
        {
          x <- move(x, moves = "RU'R'U")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "R'F'")
          c <- c + 2
          #orientacja z³a
        }
      }
      else if (x$ep["UL"]==6) #3
      {
        if (x$eo["UL"]==0)
        {
          x <- move(x, moves = "L'ULU'")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "LF")
          c <- c + 2
        }
      }
      else if (x$ep["UB"]==6) #4
      {
        if (x$eo["UB"]==0)
        {
          x <- move(x, moves = "BU2B'U2")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "BULU'")
          c <- c + 4
        }
      } 
      else if (x$ep["FR"]==6) #5
      {
        if (x$eo["FR"]==0)
        {
          x <- move(x, moves = "U'RU")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "F'")
          c <- c + 1
        }
      }
      else if (x$ep["FL"]==6) #6
      {
        if (x$eo["FL"]==0)
        {
          x <- move(x, moves = "UL'U'")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "F")
          c <- c + 1
        }
      }
      else if (x$ep["BL"]==6) #7
      {
        if (x$eo["BL"]==0)
        {
          x <- move(x, moves = "ULU'")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U2B'U2")
          c <- c + 3
        }
      }
      else if (x$ep["BR"]==6) #8
      {
        if (x$eo["BR"]==0)
        {
          x <- move(x, moves = "U'R'U")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U2BU2")
          c <- c + 3
        }
      }
      else if (x$ep["DR"]==6) #9
      {
        if (x$eo["DR"]==0)
        {
          x <- move(x, moves = "D'F2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "RF'R'")
          c <- c + 3
        }
      }
      else if (x$ep["DF"]==6) #10
      {
        if (x$eo["DF"]==0)
        {
          x <- move(x, moves = "F2")
          c <- c + 1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "DRF'R'")
          c <- c + 4
        }
      }
      else if (x$ep["DL"]==6) #11
      {
        if (x$eo["DL"]==0)
        {
          x <- move(x, moves = "DF2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "L'FL")
          c <- c + 3
        }
      }
      else if (x$ep["DB"]==6) #12
      {
        if (x$eo["DB"]==0)
        {
          x <- move(x, moves = "D2F2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "DL'FL")
          c <- c + 4
        }
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  WO_cross <- function(x,c)     ###WHITE-ORANGE EDGE
  {
    if ( x$ep["UL"]== 7)      #1
    {
      if (x$eo["UL"]==0)
      {
        x <- x
        c <- c
        # dobre miejsce i dobra orientacja
      } 
      else
      {
        x <- move(x, moves = "LU'FU")
        c <- c + 4
        # dobre miejsce zla orientacja 
      }
    }
    else
    {
      if (x$ep["UR"]==7)      #2
      {
        if (x$eo["UR"]==0)
        {
          x <- move(x, moves = "U2LU2L'")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "UF'U'L'")
          c <- c + 4
          #orientacja z³a
        }
      }
      else if (x$ep["UF"]==7) #3
      {
        if (x$eo["UF"]==0)
        {
          x <- move(x, moves = "FU'F'U")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "F'L'")
          c <- c + 2
        }
      }
      else if (x$ep["UB"]==7) #4
      {
        if (x$eo["UB"]==0)
        {
          x <- move(x, moves = "U'LUL'")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "BL")
          c <- c + 2
        }
      } 
      else if (x$ep["FR"]==7) #5
      {
        if (x$eo["FR"]==0)
        {
          x <- move(x, moves = "U2RU2")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U'F'U")
          c <- c + 3
        }
      }
      else if (x$ep["FL"]==7) #6
      {
        if (x$eo["FL"]==0)
        {
          x <- move(x, moves = "L'")
          c <- c + 1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U'FU")
          c <- c + 3
        }
      }
      else if (x$ep["BL"]==7) #7
      {
        if (x$eo["BL"]==0)
        {
          x <- move(x, moves = "L")
          c <- c + 1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "UB'U'")
          c <- c + 3
        }
      }
      else if (x$ep["BR"]==7) #8
      {
        if (x$eo["BR"]==0)
        {
          x <- move(x, moves = "U2R'U2")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "UBU'")
          c <- c + 3
        }
      }
      else if (x$ep["DR"]==7) #9
      {
        if (x$eo["DR"]==0)
        {
          x <- move(x, moves = "D2L2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "D'FL'F'")
          c <- c + 4
        }
      }
      else if (x$ep["DF"]==7) #10
      {
        if (x$eo["DF"]==0)
        {
          x <- move(x, moves = "D'L2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "FL'F'")
          c <- c + 3
        }
      }
      else if (x$ep["DL"]==7) #11
      {
        if (x$eo["DL"]==0)
        {
          x <- move(x, moves = "L2")
          c <- c + 1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "L'U'FU")
          c <- c + 4
        }
      }
      else if (x$ep["DB"]==7) #12
      {
        if (x$eo["DB"]==0)
        {
          x <- move(x, moves = "DL2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "B'LB")
          c <- c + 3
        }
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  
  WB_cross <- function(x,c)     ###WHITE-BLUE EDGE
  {
    if ( x$ep["UB"]== 8)      #1
    {
      if (x$eo["UB"]==0)
      {
        x <- x
        c <- c
        # dobre miejsce i dobra orientacja
      } 
      else
      {
        x <- move(x, moves = "BU'LU")
        c <- c + 4
        # dobre miejsce zla orientacja 
      }
    }
    else
    {
      if (x$ep["UL"]==8)      #2
      {
        if (x$eo["UL"]==0)
        {
          x <- move(x, moves = "LU'L'U")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "L'B'")
          c <- c + 2
          #orientacja z³a
        }
      }
      else if (x$ep["UF"]==8) #3
      {
        if (x$eo["UF"]==0)
        {
          x <- move(x, moves = "FU2F'U2")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "FURU'")
          c <- c + 4
        }
      }
      else if (x$ep["UR"]==8) #4
      {
        if (x$eo["UR"]==0)
        {
          x <- move(x, moves = "R'URU'")
          c <- c + 4
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "RB")
          c <- c + 2
        }
      } 
      else if (x$ep["FR"]==8) #5
      {
        if (x$eo["FR"]==0)
        {
          x <- move(x, moves = "URU'")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U2F'U2")
          c <- c + 3
        }
      }
      else if (x$ep["FL"]==8) #6
      {
        if (x$eo["FL"]==0)
        {
          x <- move(x, moves = "U'L'U")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "U2FU2")
          c <- c + 3
        }
      }
      else if (x$ep["BL"]==8) #7
      {
        if (x$eo["BL"]==0)
        {
          x <- move(x, moves = "U'LU")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "B'")
          c <- c + 1
        }
      }
      else if (x$ep["BR"]==8) #8
      {
        if (x$eo["BR"]==0)
        {
          x <- move(x, moves = "UR'U'")
          c <- c + 3
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "B")
          c <- c + 1
        }
      }
      else if (x$ep["DR"]==8) #9
      {
        if (x$eo["DR"]==0)
        {
          x <- move(x, moves = "DB2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "R'BR")
          c <- c + 3
        }
      }
      else if (x$ep["DF"]==8) #10
      {
        if (x$eo["DF"]==0)
        {
          x <- move(x, moves = "D2B2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "DR'BR")
          c <- c + 4
        }
      }
      else if (x$ep["DL"]==8) #11
      {
        if (x$eo["DL"]==0)
        {
          x <- move(x, moves = "D'B2")
          c <- c + 2
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "LB'L'")
          c <- c + 3
        }
      }
      else if (x$ep["DB"]==8) #12
      {
        if (x$eo["DB"]==0)
        {
          x <- move(x, moves = "B2")
          c <- c + 1
          #orientacja dobra
        }
        else
        {
          x <- move(x, moves = "DLB'L'")
          c <- c + 4
        }
      }
    }
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  
  CROSS <- function(x,c)        ###WHITE CROSS FUNCTION
  {
    
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  URF_corner <- function(x,c)   ###WHITE-GREEN-RED CORNER
  {
    if (x$cp["URF"]==1)       #1
    {
      if (x$co["URF"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$co["URF"]==1)
      {
        x <- move(x, moves = "R'D'RDR'D'R")
        c <- c + 7
      }
      else if (x$co["URF"]==2)
      {
        x <- move(x, moves = "FDF'D'FDF'")
        c <- c + 7
      }
    }
    else if (x$cp["UFL"]==1)  #2
    {
      if (x$co["UFL"]==0)
      {
        x <- move(x, moves = "F'D'FR'D2R")
        c <- c + 6
      }
      else if (x$co["UFL"]==1)
      {
        x <- move(x, moves = "F'D'F2D2F'")
        c <- c + 5
      }
      else if (x$co["UFL"]==2)
      {
        x <- move(x, moves = "LR'DRL'")
        c <- c + 5
        #MIDDLE
      }
    }
    else if (x$cp["ULB"]==1)  #3
    {
      if (x$co["ULB"]==0)
      {
        x <- move(x, moves = "L'R'D2LR")
        c <- c + 5
      }
      else if (x$co["ULB"]==1)
      {
        x <- move(x, moves = "L'FD2F'L")
        c <- c + 5
      }
      else if (x$co["ULB"]==2)
      {
        x <- move(x, moves = "BR'D2RB'")
        c <- c + 5
      }
    }
    else if (x$cp["UBR"]==1)  #4
    {
      if (x$co["UBR"]==0)
      {
        x <- move(x, moves = "RDR'FD2F'")
        c <- c + 6
      }
      else if (x$co["UBR"]==1)
      {
        x <- move(x, moves = "FB'D'F'B")
        c <- c + 5
      }
      else if (x$co["UBR"]==2)
      {
        x <- move(x, moves = "RDR2D2R")
        c <- c + 5
      }
    }
    else if (x$cp["DFR"]==1)  #5
    {
      if (x$co["DFR"]==0)
      {
        x <- move(x, moves = "R'DRFD2F'")
        c <- c + 6
      }
      else if (x$co["DFR"]==1)
      {
        x <- move(x, moves = "FDF'")
        c <- c + 3
      }
      else if (x$co["DFR"]==2)
      {
        x <- move(x, moves = "R'D'R")
        c <- c + 3
      }
    }
    else if (x$cp["DLF"]==1)  #6
    {
      if (x$co["DLF"]==0)
      {
        x <- move(x, moves = "DR'DRFD2F'")
        c <- c + 7
      }
      else if (x$co["DLF"]==1)
      {
        x <- move(x, moves = "R'DR")
        c <- c + 3
      }
      else if (x$co["DLF"]==2)
      {
        x <- move(x, moves = "D'FD2F'")
        c <- c + 4
      }
    }
    else if (x$cp["DBL"]==1)  #7
    {
      if (x$co["DBL"]==0)
      {
        x <- move(x, moves = "D2R'DRFD2F'")
        c <- c + 7
      }
      else if (x$co["DBL"]==1)
      {
        x <- move(x, moves = "R'D2R")
        c <- c + 3
      }
      else if (x$co["DBL"]==2)
      {
        x <- move(x, moves = "FD2F'")
        c <- c + 3
      }
    }
    else if (x$cp["DRB"]==1)  #8
    {
      if (x$co["DRB"]==0)
      {
        x <- move(x, moves = "D'R'DRFD2F'")
        c <- c + 7
      }
      else if (x$co["DRB"]==1)
      {
        x <- move(x, moves = "DR'D2R")
        c <- c + 4
      }
      else if (x$co["DRB"]==2)
      {
        x <- move(x, moves = "FD'F'")
        c <- c + 3
      }
    }
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  
  UFL_corner <- function(x,c)   ###WHITE-GREEN-ORANGE CORNER
  {
    if (x$cp["UFL"]==2)       #1
    {
      if (x$co["UFL"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$co["UFL"]==1)
      {
        x <- move(x, moves = "F'D'FDF'D'F")
        c <- c + 7
      }
      else if (x$co["UFL"]==2)
      {
        x <- move(x, moves = "LDL'D'LDL'")
        c <- c + 7
      }
    }
    else if (x$cp["ULB"]==2)  #2
    {
      if (x$co["ULB"]==0)
      {
        x <- move(x, moves = "L'D'LF'D2F")
        c <- c + 6
      }
      else if (x$co["ULB"]==1)
      {
        x <- move(x, moves = "L'D'L2D2L'")
        c <- c + 5
      }
      else if (x$co["ULB"]==2)
      {
        x <- move(x, moves = "BF'DFB'")
        c <- c + 5
        #MIDDLE
      }
    }
    else if (x$cp["UBR"]==2)  #3
    {
      if (x$co["UBR"]==0)
      {
        x <- move(x, moves = "B'F'D2BF")
        c <- c + 5
      }
      else if (x$co["UBR"]==1)
      {
        x <- move(x, moves = "B'LD2L'B")
        c <- c + 5
      }
      else if (x$co["UBR"]==2)
      {
        x <- move(x, moves = "RF'D2FR'")
        c <- c + 5
      }
    }
    else if (x$cp["URF"]==2)  #4
    {
      if (x$co["URF"]==0)
      {
        x <- move(x, moves = "FDF'LD2L'")
        c <- c + 6
      }
      else if (x$co["URF"]==1)
      {
        x <- move(x, moves = "LR'D'L'R")
        c <- c + 5
      }
      else if (x$co["URF"]==2)
      {
        x <- move(x, moves = "FDF2D2F")
        c <- c + 5
      }
    }
    else if (x$cp["DLF"]==2)  #5
    {
      if (x$co["DLF"]==0)
      {
        x <- move(x, moves = "F'DFLD2L'")
        c <- c + 6
      }
      else if (x$co["DLF"]==1)
      {
        x <- move(x, moves = "LDL'")
        c <- c + 3
      }
      else if (x$co["DLF"]==2)
      {
        x <- move(x, moves = "F'D'F")
        c <- c + 3
      }
    }
    else if (x$cp["DBL"]==2)  #6
    {
      if (x$co["DBL"]==0)
      {
        x <- move(x, moves = "DF'DFLD2L'")
        c <- c + 7
      }
      else if (x$co["DBL"]==1)
      {
        x <- move(x, moves = "F'DF")
        c <- c + 3
      }
      else if (x$co["DBL"]==2)
      {
        x <- move(x, moves = "D'LD2L'")
        c <- c + 4
      }
    }
    else if (x$cp["DRB"]==2)  #7
    {
      if (x$co["DRB"]==0)
      {
        x <- move(x, moves = "D2F'DFLD2L'")
        c <- c + 7
      }
      else if (x$co["DRB"]==1)
      {
        x <- move(x, moves = "F'D2F")
        c <- c + 3
      }
      else if (x$co["DRB"]==2)
      {
        x <- move(x, moves = "LD2L'")
        c <- c + 3
      }
    }
    else if (x$cp["DFR"]==2)  #8
    {
      if (x$co["DFR"]==0)
      {
        x <- move(x, moves = "D'F'DFLD2L'")
        c <- c + 7
      }
      else if (x$co["DFR"]==1)
      {
        x <- move(x, moves = "DF'D2F")
        c <- c + 4
      }
      else if (x$co["DFR"]==2)
      {
        x <- move(x, moves = "LD'L'")
        c <- c + 3
      }
    }
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  ULB_corner <- function(x,c)   ###WHITE-BLUE-ORANGE CORNER
  {
    if (x$cp["ULB"]==3)       #1
    {
      if (x$co["ULB"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$co["ULB"]==1)
      {
        x <- move(x, moves = "L'D'LDL'D'L")
        c <- c + 7
      }
      else if (x$co["ULB"]==2)
      {
        x <- move(x, moves = "BDB'D'BDB'")
        c <- c + 7
      }
    }
    else if (x$cp["UBR"]==3)  #2
    {
      if (x$co["UBR"]==0)
      {
        x <- move(x, moves = "B'D'BL'D2L")
        c <- c + 6
      }
      else if (x$co["UBR"]==1)
      {
        x <- move(x, moves = "B'D'B2D2B'")
        c <- c + 5
      }
      else if (x$co["UBR"]==2)
      {
        x <- move(x, moves = "RL'DLR'")
        c <- c + 5
        #MIDDLE
      }
    }
    else if (x$cp["URF"]==3)  #3
    {
      if (x$co["URF"]==0)
      {
        x <- move(x, moves = "R'L'D2RL")
        c <- c + 5
      }
      else if (x$co["URF"]==1)
      {
        x <- move(x, moves = "R'BD2B'R")
        c <- c + 5
      }
      else if (x$co["URF"]==2)
      {
        x <- move(x, moves = "FL'D2LF'")
        c <- c + 5
      }
    }
    else if (x$cp["UFL"]==3)  #4
    {
      if (x$co["UFL"]==0)
      {
        x <- move(x, moves = "LDL'BD2B'")
        c <- c + 6
      }
      else if (x$co["UFL"]==1)
      {
        x <- move(x, moves = "BF'D'B'F")
        c <- c + 5
      }
      else if (x$co["UFL"]==2)
      {
        x <- move(x, moves = "LDL2D2L")
        c <- c + 5
      }
    }
    else if (x$cp["DBL"]==3)  #5
    {
      if (x$co["DBL"]==0)
      {
        x <- move(x, moves = "L'DLBD2B'")
        c <- c + 6
      }
      else if (x$co["DBL"]==1)
      {
        x <- move(x, moves = "BDB'")
        c <- c + 3
      }
      else if (x$co["DBL"]==2)
      {
        x <- move(x, moves = "L'D'L")
        c <- c + 3
      }
    }
    else if (x$cp["DRB"]==3)  #6
    {
      if (x$co["DRB"]==0)
      {
        x <- move(x, moves = "DL'DLBD2B'")
        c <- c + 7
      }
      else if (x$co["DRB"]==1)
      {
        x <- move(x, moves = "L'DL")
        c <- c + 3
      }
      else if (x$co["DRB"]==2)
      {
        x <- move(x, moves = "D'BD2B'")
        c <- c + 4
      }
    }
    else if (x$cp["DFR"]==3)  #7
    {
      if (x$co["DFR"]==0)
      {
        x <- move(x, moves = "D2L'DLBD2B'")
        c <- c + 7
      }
      else if (x$co["DFR"]==1)
      {
        x <- move(x, moves = "L'D2L")
        c <- c + 3
      }
      else if (x$co["DFR"]==2)
      {
        x <- move(x, moves = "BD2B'")
        c <- c + 3
      }
    }
    else if (x$cp["DLF"]==3)  #8
    {
      if (x$co["DLF"]==0)
      {
        x <- move(x, moves = "D'L'DLBD2B'")
        c <- c + 7
      }
      else if (x$co["DLF"]==1)
      {
        x <- move(x, moves = "DL'D2L")
        c <- c + 4
      }
      else if (x$co["DLF"]==2)
      {
        x <- move(x, moves = "BD'B'")
        c <- c + 3
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  UBR_corner <- function(x,c)   ###WHITE-BLUE-RED CORNER
  {
    if (x$cp["UBR"]==4)       #1
    {
      if (x$co["UBR"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$co["UBR"]==1)
      {
        x <- move(x, moves = "B'D'BDB'D'B")
        c <- c + 7
      }
      else if (x$co["UBR"]==2)
      {
        x <- move(x, moves = "RDR'D'RDR'")
        c <- c + 7
      }
    }
    else if (x$cp["URF"]==4)  #2
    {
      if (x$co["URF"]==0)
      {
        x <- move(x, moves = "R'D'RB'D2B")
        c <- c + 6
      }
      else if (x$co["URF"]==1)
      {
        x <- move(x, moves = "R'D'R2D2R'")
        c <- c + 5
      }
      else if (x$co["URF"]==2)
      {
        x <- move(x, moves = "FB'DBF'")
        c <- c + 5
        #MIDDLE
      }
    }
    else if (x$cp["UFL"]==4)  #3
    {
      if (x$co["UFL"]==0)
      {
        x <- move(x, moves = "F'B'D2FB")
        c <- c + 5
      }
      else if (x$co["UFL"]==1)
      {
        x <- move(x, moves = "F'RD2R'F")
        c <- c + 5
      }
      else if (x$co["UFL"]==2)
      {
        x <- move(x, moves = "LB'D2BL'")
        c <- c + 5
      }
    }
    else if (x$cp["ULB"]==4)  #4
    {
      if (x$co["ULB"]==0)
      {
        x <- move(x, moves = "BDB'RD2R'")
        c <- c + 6
      }
      else if (x$co["ULB"]==1)
      {
        x <- move(x, moves = "RL'D'R'L")
        c <- c + 5
      }
      else if (x$co["ULB"]==2)
      {
        x <- move(x, moves = "BDB2D2B")
        c <- c + 5
      }
    }
    else if (x$cp["DRB"]==4)  #5
    {
      if (x$co["DRB"]==0)
      {
        x <- move(x, moves = "B'DBRD2R'")
        c <- c + 6
      }
      else if (x$co["DRB"]==1)
      {
        x <- move(x, moves = "RDR'")
        c <- c + 3
      }
      else if (x$co["DRB"]==2)
      {
        x <- move(x, moves = "B'D'B")
        c <- c + 3
      }
    }
    else if (x$cp["DFR"]==4)  #6
    {
      if (x$co["DFR"]==0)
      {
        x <- move(x, moves = "DB'DBRD2R'")
        c <- c + 7
      }
      else if (x$co["DFR"]==1)
      {
        x <- move(x, moves = "B'DB")
        c <- c + 3
      }
      else if (x$co["DFR"]==2)
      {
        x <- move(x, moves = "D'RD2R'")
        c <- c + 4
      }
    }
    else if (x$cp["DLF"]==4)  #7
    {
      if (x$co["DLF"]==0)
      {
        x <- move(x, moves = "D2B'DBRD2R'")
        c <- c + 7
      }
      else if (x$co["DLF"]==1)
      {
        x <- move(x, moves = "B'D2B")
        c <- c + 3
      }
      else if (x$co["DLF"]==2)
      {
        x <- move(x, moves = "RD2R'")
        c <- c + 3
      }
    }
    else if (x$cp["DBL"]==4)  #8
    {
      if (x$co["DBL"]==0)
      {
        x <- move(x, moves = "D'B'DBRD2R'")
        c <- c + 7
      }
      else if (x$co["DBL"]==1)
      {
        x <- move(x, moves = "DB'D2B")
        c <- c + 4
      }
      else if (x$co["DBL"]==2)
      {
        x <- move(x, moves = "RD'R'")
        c <- c + 3
      }
    }
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  white_corners <- function(x,c)  ###WHITE-CORNERS
  {
    
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  first_layer <- function(x,c)    ###WHITE FACE
  {
    c <- CROSS(x,c)$counter
    x <- CROSS(x,c)$cube
    
    c <- white_corners(x,c)$counter
    x <- white_corners(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_RED_GREEN <- function(x,c)
  {
    x <- move(x, moves = "FD'F'D'R'DR")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_GREEN_RED <- function(x,c)
  {
    x <- move(x, moves = "R'DRDFD'F'")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_RED_BLUE <- function(x,c)
  {
    x <- move(x, moves = "B'DBDRD'R'")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_BLUE_RED <- function(x,c)
  {
    x <- move(x, moves = "RD'R'D'B'DB")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_ORANGE_GREEN <- function(x,c)
  {
    x <- move(x, moves = "F'DFDLD'L'")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_GREEN_ORANGE <- function(x,c)
  {
    x <- move(x, moves = "LD'L'D'F'DF")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_ORANGE_BLUE <- function(x,c)
  {
    x <- move(x, moves = "BD'B'D'L'DL")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  put_BLUE_ORANGE <- function(x,c)
  {
    x <- move(x, moves = "L'DLDBD'B'")
    c <- c+7
    return(list("cube"=x,"counter"=c))
  }
  
  
  RED_GREEN_EDGE <- function(x,c)       #SECOND LAYER RED-GREEN EDGE
  {
    if (x$ep["DR"]==1)          #1
    {
      if (x$eo["DR"]==0)
      {
        x <- move(x, moves = "D")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$eo["DR"]==1)
      {
        x <- move(x, moves = "D2")
        c <- put_GREEN_RED(x,c)$counter + 1
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["DF"]==1)     #2
    {
      if (x$eo["DF"]==0)
      {
        x <- move(x, moves = "D2")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$eo["DF"]==1)
      {
        x <- move(x, moves = "D'")
        c <-put_GREEN_RED(x,c)$counter + 1
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["DL"]==1)     #3
    {
      if (x$eo["DL"]==0)
      {
        x <- move(x, moves = "D'")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$eo["DL"]==1)
      {
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["DB"]==1)     #4
    {
      if (x$eo["DB"]==0)
      {
        c <- put_RED_GREEN(x,c)$counter
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$eo["DB"]==1)
      {
        x <- move(x, moves = "D")
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["FR"]==1)     #5
    {
      if (x$eo["FR"]==0)
      {
        x <- x
      }
      else if (x$eo["FR"]==1)
      {
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_RED_GREEN(x,c)$counter
        x <- put_RED_GREEN(x,c)$cube
      }
    }
    else if (x$ep["FL"]==1)     #6
    {
      if (x$eo["FL"]==0)
      {
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
        c <- put_RED_GREEN(x,c)$counter
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$eo["FL"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- put_GREEN_ORANGE(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["BL"]==1)     #7
    {
      if (x$eo["BL"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter + 1
        x <- put_ORANGE_BLUE(x,c)$cube
        x <- move(x, moves = "D2")
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
      else if (x$eo["BL"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- put_BLUE_ORANGE(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$ep["BR"]==1)     #8
    {
      if (x$eo["BR"]==0)
      {
        c <- put_RED_BLUE(x,c)$counter
        x <- put_RED_BLUE(x,c)$cube
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
      else if (x$eo["BR"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- put_BLUE_RED(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  GREEN_ORANGE_EDGE <- function(x, c)    #SECOND LAYER GREEN-ORANGE EDGE
  {
    if (x$ep["FL"]==2)          #1
    {
      if (x$eo["FL"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$eo["FL"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- put_GREEN_ORANGE(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["BL"]==2)     #2
    {
      if (x$eo["BL"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
      }
      else if (x$eo["BL"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- put_BLUE_ORANGE(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["BR"]==2)     #3
    {
      if (x$eo["BR"]==0)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- put_BLUE_RED(x,c)$cube
        x <- move(x, moves = "D2")
        c <- put_ORANGE_GREEN(x,c)$counter
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["BR"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- put_BLUE_RED(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["FR"]==2)     #4
    {
      if (x$eo["FR"]==0)
      {
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
        c <- put_ORANGE_GREEN(x,c)$counter
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["FR"]==1)
      {
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_ORANGE_GREEN(x,c)$counter
        x <- put_ORANGE_GREEN(x,c)$cube
      }
    }
    else if (x$ep["DR"]==2)     #5
    {
      if (x$eo["DR"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["DR"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DF"]==2)     #6
    {
      if (x$eo["DF"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["DF"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DL"]==2)     #7
    {
      if (x$eo["DL"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["DL"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DB"]==2)     #8
    {
      if (x$eo["DB"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter
        x <- put_ORANGE_GREEN(x,c)$cube
      }
      else if (x$eo["DB"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_GREEN_ORANGE(x,c)$cube
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  ORANGE_BLUE_EDGE <- function(x,c)     #SECOND LAYER ORANGE-BLUE EDGE
  {
    if (x$ep["BL"]==3)            #1
    {
      if (x$eo["BL"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$eo["BL"]==1)
      {
        c <- put_ORANGE_BLUE(x,c)$counter + 1
        x <- put_ORANGE_BLUE(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
      }
    }
    else if (x$ep["BR"]==3)       #2
    {
      if (x$eo["BR"]==0)
      {
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
      }
      else if (x$eo["BR"]==1)
      {
        c <- put_RED_BLUE(x,c)$counter + 1
        x <- put_RED_BLUE(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
      }
    }
    else if (x$ep["FR"]==3)       #3
    {
      if (x$eo["FR"]==0)
      {
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
        x <- move(x, moves = "D2")
        c <- put_BLUE_ORANGE(x,c)$counter
        x <- put_BLUE_ORANGE(x,c)$cube
      }
      else if (x$eo["FR"]==1)
      {
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
      }
    }
    else if (x$ep["FL"]==3)       #4
    {
      if (x$eo["FL"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter
        x <- put_ORANGE_GREEN(x,c)$cube
        c <- put_BLUE_ORANGE(x,c)$counter
        x <- put_BLUE_ORANGE(x,c)$cube
      }
      else if (x$eo["FL"]==1)
      {
        c <- put_GREEN_ORANGE(x,c)$counter + 1
        x <- put_GREEN_ORANGE(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_BLUE_ORANGE(x,c)$counter
        x <- put_BLUE_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DR"]==3)       #5
    {
      if (x$eo["DR"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_ORANGE_BLUE(x,c)$cube
      }
      else if (x$eo["DR"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter
        x <- put_BLUE_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DF"]==3)       #6
    {
      if (x$eo["DF"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter
        x <- put_ORANGE_BLUE(x,c)$cube
      }
      else if (x$eo["DF"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_BLUE_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DL"]==3)       #7
    {
      if (x$eo["DL"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_ORANGE_BLUE(x,c)$cube
      }
      else if (x$eo["DL"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_BLUE_ORANGE(x,c)$cube
      }
    }
    else if (x$ep["DB"]==3)       #8
    {
      if (x$eo["DB"]==0)
      {
        c <- put_ORANGE_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_ORANGE_BLUE(x,c)$cube
      }
      else if (x$eo["DB"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_BLUE_ORANGE(x,c)$cube
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  BLUE_RED_EDGE <- function(x,c)     #SECOND LAYER BLUE-RED EDGE
  {
    if (x$ep["BR"]==4)            #1
    {
      if (x$eo["BR"]==0)
      {
        x <- x
        c <- c
      }
      else if (x$eo["BR"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- put_BLUE_RED(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    else if (x$ep["FR"]==4)       #2
    {
      if (x$eo["FR"]==0)
      {
        c <- put_RED_GREEN(x,c)$counter
        x <- put_RED_GREEN(x,c)$cube
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
      }
      else if (x$eo["FR"]==1)
      {
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
        x <- move(x, moves = "D")
        c <- put_RED_BLUE(x,c)$counter
        x <- put_RED_BLUE(x,c)$cube
      }
    }
    else if (x$ep["FL"]==4)       #3
    {
      if (x$eo["FL"]==0)
      {
        c <- put_ORANGE_GREEN(x,c)$counter + 1
        x <- put_ORANGE_GREEN(x,c)$cube
        x <- move(x, moves = "D2")
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
      }
      else if (x$eo["FL"]==1)
      {
        c <- put_ORANGE_GREEN(x,c)$counter + 1
        x <- put_ORANGE_GREEN(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_RED_BLUE(x,c)$counter
        x <- put_RED_BLUE(x,c)$cube
      }
    }
    else if (x$ep["BL"]==4)       #4
    {
      if (x$eo["BL"]==0)
      {
        c <- put_BLUE_ORANGE(x,c)$counter
        x <- put_BLUE_ORANGE(x,c)$cube
        c <- put_RED_BLUE(x,c)$counter
        x <- put_RED_BLUE(x,c)$cube
      }
      else if (x$eo["BL"]==1)
      {
        c <- put_BLUE_ORANGE(x,c)$counter + 1
        x <- put_BLUE_ORANGE(x,c)$cube
        x <- move(x, moves = "D'")
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    else if (x$ep["DR"]==4)       #5
    {
      if (x$eo["DR"]==0)
      {
        c <- put_RED_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_RED_BLUE(x,c)$cube
      }
      else if (x$eo["DR"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    else if (x$ep["DF"]==4)       #6
    {
      if (x$eo["DF"]==0)
      {
        c <- put_RED_BLUE(x,c)$counter
        x <- put_RED_BLUE(x,c)$cube
      }
      else if (x$eo["DF"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- move(x, moves = "D'")
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    else if (x$ep["DL"]==4)       #7
    {
      if (x$eo["DL"]==0)
      {
        c <- put_RED_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_RED_BLUE(x,c)$cube
      }
      else if (x$eo["DL"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    else if (x$ep["DB"]==4)       #8
    {
      if (x$eo["DB"]==0)
      {
        c <- put_RED_BLUE(x,c)$counter + 1
        x <- move(x, moves = "D2")
        x <- put_RED_BLUE(x,c)$cube
      }
      else if (x$eo["DB"]==1)
      {
        c <- put_BLUE_RED(x,c)$counter + 1
        x <- move(x, moves = "D")
        x <- put_BLUE_RED(x,c)$cube
      }
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  second_layer <- function(x,c)
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  first_two_layers <- function(x,c)
  {
    c <- first_layer(x,c)$counter
    x <- first_layer(x,c)$cube
    
    c <- second_layer(x,c)$counter
    x <- second_layer(x,c)$cube
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  yellow_edges_orientation <- function(x,c)       ###ORIENTATION OF YELLOW CROSS
  {
    if (x$eo["DR"]==0 && x$eo["DF"]==0 && x$eo["DL"]==0 && x$eo["DB"]==0)         #1
    {
      x <- x
      c <- c
    }
    else if (x$eo["DR"]==1 && x$eo["DF"]==1 && x$eo["DL"]==1 && x$eo["DB"]==1)    #2
    {
      x <- move(x, moves = "FLDL'D'F'BDRD'R'B'")
      c <- c + 12
    }
    else if (x$eo["DR"]==1 && x$eo["DF"]==1 && x$eo["DL"]==0 && x$eo["DB"]==0)    #3
    {
      x <- move(x, moves = "RDFD'F'R'")
      c <- c + 6
    }
    else if (x$eo["DR"]==1 && x$eo["DF"]==0 && x$eo["DL"]==1 && x$eo["DB"]==0)    #4
    {
      x <- move(x, moves = "RFDF'D'R'")
      c <- c + 6
    }
    else if (x$eo["DR"]==1 && x$eo["DF"]==0 && x$eo["DL"]==0 && x$eo["DB"]==1)    #5
    {
      x <- move(x, moves = "BDRD'R'B'")
      c <- c + 6
    }
    else if (x$eo["DR"]==0 && x$eo["DF"]==0 && x$eo["DL"]==1 && x$eo["DB"]==1)    #6
    {
      x <- move(x, moves = "LDBD'B'L'")
      c <- c + 6
    }
    else if (x$eo["DR"]==0 && x$eo["DF"]==1 && x$eo["DL"]==0 && x$eo["DB"]==1)    #7
    {
      x <- move(x, moves = "FLDL'D'F'")
      c <- c + 6
    }
    else if (x$eo["DR"]==0 && x$eo["DF"]==1 && x$eo["DL"]==1 && x$eo["DB"]==0)    #8
    {
      x <- move(x, moves = "FDLD'L'F'")
      c <- c + 6
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  change_right_back <- function(x)
  {
    x <- move(x, moves = "FDF'DFD2F'D")
    return(x)
  }
  
  
  change_left_back <- function(x)
  {
    x <- move(x, moves = "RDR'DRD2R'D")
    return(x)
  }
  
  
  change_right_front <- function(x)
  {
    x <- move(x, moves = "LDL'DLD2L'D")
    return(x)
  }
  
  
  change_left_front <- function(x)
  {
    x <- move(x, moves = "BDB'DBD2B'D")
    return(x)
  }
  
  
  yellow_edges_permutation <- function(x,c)
  {
    if (x$ep["DR"]==9 && x$ep["DF"]==10 && x$ep["DL"]==11 && x$ep["DB"]==12)        #1
    {
      x <- x
      c <- c
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==9 && x$ep["DL"]==10 && x$ep["DB"]==11)   #2
    {
      x <- move(x, moves = "D")
      c <- c + 1
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==12 && x$ep["DL"]==9 && x$ep["DB"]==10)   #3
    {
      x <- move(x, moves = "D2")
      c <- c + 1
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==11 && x$ep["DL"]==12 && x$ep["DB"]==9)   #4
    {
      x <- move(x, moves = "D'")
      c <- c + 1
    }
    else if (x$ep["DR"]==9 && x$ep["DF"]==10 && x$ep["DL"]==12 && x$ep["DB"]==11)   #5
    {
      x <- change_left_back(x)
      c <- c + 8
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==9 && x$ep["DL"]==10 && x$ep["DB"]==12)   #6
    {
      x <- move(x, moves = "D")
      x <- change_left_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==11 && x$ep["DL"]==9 && x$ep["DB"]==10)   #7
    {
      x <- move(x, moves = "D2")
      x <- change_left_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==12 && x$ep["DL"]==11 && x$ep["DB"]==9)   #8
    {
      x <- move(x, moves = "D'")
      x <- change_left_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==10 && x$ep["DL"]==11 && x$ep["DB"]==9)   #9
    {
      x <- change_right_back(x)
      c <- c + 8
    }
    else if (x$ep["DR"]==9 && x$ep["DF"]==12 && x$ep["DL"]==10 && x$ep["DB"]==11)   #10
    {
      x <- move(x, moves = "D")
      x <- change_right_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==9 && x$ep["DL"]==12 && x$ep["DB"]==10)   #11
    {
      x <- move(x, moves = "D2")
      x <- change_right_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==11 && x$ep["DL"]==9 && x$ep["DB"]==12)   #12
    {
      x <- move(x, moves = "D'")
      x <- change_right_back(x)
      c <- c + 9
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==10 && x$ep["DL"]==9 && x$ep["DB"]==12)   #13
    {
      x <- change_left_front(x)
      x <- change_right_back(x)
      x <- move(x, moves = "D")
      c <- c + 17
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==11 && x$ep["DL"]==10 && x$ep["DB"]==9)   #14
    {
      x <- change_left_back(x)
      x <- change_right_front(x)
      x <- move(x, moves = "D2")
      c <- c + 17
    }
    else if (x$ep["DR"]==9 && x$ep["DF"]==12 && x$ep["DL"]==11 && x$ep["DB"]==10)   #15
    {
      x <- change_right_back(x)
      x <- change_left_front(x)
      x <- move(x, moves = "D'")
      c <- c + 17
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==9 && x$ep["DL"]==12 && x$ep["DB"]==11)   #16
    {
      x <- change_right_front(x)
      x <- change_left_back(x)
      c <- c + 16
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==10 && x$ep["DL"]==12 && x$ep["DB"]==9)   #17
    {
      x <- change_right_front(x)
      x <- move(x, moves = "D'")
      c <- c + 9
    }
    else if (x$ep["DR"]==9 && x$ep["DF"]==11 && x$ep["DL"]==10 && x$ep["DB"]==12)   #18
    {
      x <- change_left_front(x)
      c <- c + 8
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==9 && x$ep["DL"]==11 && x$ep["DB"]==10)   #19
    {
      x <- change_left_back(x)
      x <- move(x, moves = "D")
      c <- c + 9
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==12 && x$ep["DL"]==9 && x$ep["DB"]==11)   #20
    {
      x <- change_right_back(x)
      x <- move(x, moves = "D2")
      c <- c + 9
    }
    else if (x$ep["DR"]==12 && x$ep["DF"]==10 && x$ep["DL"]==9 && x$ep["DB"]==11)   #21
    {
      x <- change_left_front(x)
      x <- move(x, moves = "D")
      c <- c + 9
    }
    else if (x$ep["DR"]==11 && x$ep["DF"]==12 && x$ep["DL"]==10 && x$ep["DB"]==9)   #22
    {
      x <- change_left_back(x)
      x <- move(x, moves = "D2")
      c <- c + 9
    }
    else if (x$ep["DR"]==9 && x$ep["DF"]==11 && x$ep["DL"]==12 && x$ep["DB"]==10)   #23
    {
      x <- change_right_back(x)
      x <- move(x, moves = "D'")
      c <- c + 9
    }
    else if (x$ep["DR"]==10 && x$ep["DF"]==9 && x$ep["DL"]==11 && x$ep["DB"]==12)   #24
    {
      x <- change_right_front(x)
      c <- c + 8
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  yellow_cross <- function(x,c)
  {
    c <- yellow_edges_orientation(x,c)$counter
    x <- yellow_edges_orientation(x,c)$cube
    
    c <- yellow_edges_permutation(x,c)$counter
    x <- yellow_edges_permutation(x,c)$cube
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  yellow_corners_permutations <- function(x,c)
  {
    if (x$cp["DFR"]==5 && x$cp["DLF"]==6 && x$cp["DBL"]==7 && x$cp["DRB"]==8)         #1
    {
      x <- x
      c <- c
    }
    else if (x$cp["DFR"]==5 && x$cp["DLF"]==8 && x$cp["DBL"]==6 && x$cp["DRB"]==7)    #2
    {
      x <- move(x, moves = "D'R'DLD'RDL'")
      c <- c + 8
    }
    else if (x$cp["DFR"]==5 && x$cp["DLF"]==7 && x$cp["DBL"]==8 && x$cp["DRB"]==6)    #3
    {
      x <- move(x, moves = "DFD'B'DF'D'B")
      c <- c + 8
    }
    else if (x$cp["DFR"]==8 && x$cp["DLF"]==6 && x$cp["DBL"]==5 && x$cp["DRB"]==7)    #4
    {
      x <- move(x, moves = "D'F'DBD'FDB'")
      c <- c + 8
    }
    else if (x$cp["DFR"]==7 && x$cp["DLF"]==6 && x$cp["DBL"]==8 && x$cp["DRB"]==5)    #5
    {
      x <- move(x, moves = "DLD'R'DL'D'R")
      c <- c + 8
    }
    else if (x$cp["DFR"]==8 && x$cp["DLF"]==5 && x$cp["DBL"]==7 && x$cp["DRB"]==6)    #6
    {
      x <- move(x, moves = "D'L'DRD'LDR'")
      c <- c + 8
    }
    else if (x$cp["DFR"]==6 && x$cp["DLF"]==8 && x$cp["DBL"]==7 && x$cp["DRB"]==5)    #7
    {
      x <- move(x, moves = "DBD'F'DB'D'F")
      c <- c + 8
    }
    else if (x$cp["DFR"]==6 && x$cp["DLF"]==7 && x$cp["DBL"]==5 && x$cp["DRB"]==8)    #8
    {
      x <- move(x, moves = "DRD'L'DR'D'L")
      c <- c + 8
    }
    else if (x$cp["DFR"]==7 && x$cp["DLF"]==5 && x$cp["DBL"]==6 && x$cp["DRB"]==8)    #9
    {
      x <- move(x, moves = "D'B'DFD'BDF'")
      c <- c + 8
    }
    else if (x$cp["DFR"]==6 && x$cp["DLF"]==5 && x$cp["DBL"]==8 && x$cp["DRB"]==7)    #10
    {
      x <- move(x, moves = "D'B'DFD'BDF'D'R'DLD'RDL'")
      c <- c + 16
    }
    else if (x$cp["DFR"]==7 && x$cp["DLF"]==8 && x$cp["DBL"]==5 && x$cp["DRB"]==6)    #11
    {
      x <- move(x, moves = "D'B'DFD'BDF'D'L'DRD'LDR'")
      c <- c + 16
    }
    else if (x$cp["DFR"]==8 && x$cp["DLF"]==7 && x$cp["DBL"]==6 && x$cp["DRB"]==5)    #12
    {
      x <- move(x, moves = "D'B'DFD'BDF'DLD'R'DL'D'R")
      c <- c + 16
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
  }
  
  
  sexy_move <- function(x,c)
  {
    x <- move(x, moves = "RUR'U'RUR'U'")
    c <- c + 8
    return(list("cube"=x,"counter"=c))
  }
  
  
  yellow_corners_orientations <- function(x,c)
  {
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      c <- c - 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      x <- move(x, moves = "D")
      c <- c + 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      c <- c - 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      x <- move(x, moves = "D")
      c <- c + 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      c <- c - 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      x <- move(x, moves = "D")
      c <- c + 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      c <- c - 1
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    if (x$co["DFR"]!=0)
    {
      c <- sexy_move(x,c)$counter
      x <- sexy_move(x,c)$cube
    }
    else
    {
      x <- move(x, moves = "D")
      c <- c + 1
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
    
    
  }
  
  
  yellow_face <- function(x,c)
  {
    c <- yellow_cross(x,c)$counter
    x <- yellow_cross(x,c)$cube
    
    c <- yellow_corners_permutations(x,c)$counter
    x <- yellow_corners_permutations(x,c)$cube
    
    c <- yellow_corners_orientations(x,c)$counter
    x <- yellow_corners_orientations(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  LBL <- function(x,c)
  {
    c <- CROSS(x,c)$counter
    x <- CROSS(x,c)$cube
    
    c <- white_corners(x,c)$counter
    x <- white_corners(x,c)$cube
    
    c <- second_layer(x,c)$counter
    x <- second_layer(x,c)$cube
    
    c <- yellow_edges_orientation(x,c)$counter
    x <- yellow_edges_orientation(x,c)$cube
    
    c <- yellow_edges_permutation(x,c)$counter
    x <- yellow_edges_permutation(x,c)$cube
    
    c <- yellow_corners_permutations(x,c)$counter
    x <- yellow_corners_permutations(x,c)$cube
    
    c <- yellow_corners_orientations(x,c)$counter
    x <- yellow_corners_orientations(x,c)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
##################################################################################### CFOP
  
  green_red_block <- function(x,c)
  {
    if (x$cp["URF"]==1 && x$co["URF"]==0)           #1st corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #1.1a
      {
        x <- x
        c <- c
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #1.1b
      {
        x <- move(x, moves = "R'DRD'FD2F'D'FD2F'")
        c <- c + 11
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #1.2a
      {
        x <- move(x, moves = "F2D'F2DF2")
        c <- c + 5
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #1.2b
      {
        x <- move(x, moves = "UF'D'FU'R'D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #1.3a
      {
        x <- move(x, moves = "L2F2D'F2DF2L2")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #1.3b
      {
        x <- move(x, moves = "BDB'U2R'D2RU2")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #1.4a
      {
        x <- move(x, moves = "R2DR2D'R2")
        c <- c + 5
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #1.4b
      {
        x <- move(x, moves = "U'RDR'UFD2F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #1.5a
      {
        x <- move(x, moves = "D")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #1.5b
      {
        x <- move(x, moves = "D2")
        c <- put_GREEN_RED(x,c)$counter + 1
        x <- put_GREEN_RED(x,c)$cube
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #1.6a
      {
        x <- move(x, moves = "D2")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #1.6b
      {
        x <- move(x, moves = "D'")
        c <- put_GREEN_RED(x,c)$counter + 1
        x <- put_GREEN_RED(x,c)$cube
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #1.7a
      {
        x <- move(x, moves = "D'")
        c <- put_RED_GREEN(x,c)$counter + 1
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #1.7b
      {
        c <- put_GREEN_RED(x,c)$counter
        x <- put_GREEN_RED(x,c)$cube
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #1.8a
      {
        c <- put_RED_GREEN(x,c)$counter
        x <- put_RED_GREEN(x,c)$cube
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #1.8b
      {
        x <- move(x, moves = "D")
        c <- put_GREEN_RED(x,c)$counter + 1
        x <- put_GREEN_RED(x,c)$cube
      }
    }
    else if (x$cp["URF"]==1 && x$co["URF"]==1)      #1st corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #1.1a
      {
        x <- move(x, moves = "FD'F'D'FDF'D'FD2F'")
        c <- c + 11
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #1.1b
      {
        x <- move(x, moves = "FD'F'D'FD'F'DR'D'R")
        c <- c + 11
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #1.2a
      {
        x <- move(x, moves = "RF'R2D2RF2D2F'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #1.2b
      {
        x <- move(x, moves = "LDL'R'D2RFD2F'")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #1.3a
      {
        x <- move(x, moves = "BD2B'R'D2RFD2F'")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #1.3b
      {
        x <- move(x, moves = "L'DLR'D2RFD2F'")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #1.4a
      {
        x <- move(x, moves = "B'D2BR'D2RFD2F'")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #1.4b
      {
        x <- move(x, moves = "RD'R2D2RFD2F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #1.5a
      {
        x <- move(x, moves = "D2FD'F'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #1.5b
      {
        x <- move(x, moves = "D'R'D2RFD2F'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #1.6a
      {
        x <- move(x, moves = "D'FD'F'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #1.6b
      {
        x <- move(x, moves = "R'D2RFD2F'")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #1.7a
      {
        x <- move(x, moves = "FD'F'R'D'R")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #1.7b
      {
        x <- move(x, moves = "DR'D2RFD2F'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #1.8a
      {
        x <- move(x, moves = "DFD'F'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #1.8b
      {
        x <- move(x, moves = "D2R'D2RFD2F'")
        c <- c + 7
      }
    }
    else if (x$cp["URF"]==1 && x$co["URF"]==2)      #1st corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #1.1a
      {
        x <- move(x, moves = "R'DRDR'D'RDR'D2R")
        c <- c + 11
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #1.1b
      {
        x <- move(x, moves = "R'DRD'FDF'DFDF'")
        c <- c + 11
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #1.2a
      {
        x <- move(x, moves = "LD2L'FD2F'R'D2R")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #1.2b
      {
        x <- move(x, moves = "F'DF2D2F'R'D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #1.3a
      {
        x <- move(x, moves = "L'D2LFD2F'R'D2R")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #1.3b
      {
        x <- move(x, moves = "BD'B'FD2F'R'D2R")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #1.4a
      {
        x <- move(x, moves = "F'RF2D2F'R2D2R")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #1.4b
      {
        x <- move(x, moves = "B'D'BFD2F'R'D2R")
        c <- c + 9
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #1.5a
      {
        x <- move(x, moves = "FD2F'R'D2R")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #1.5b
      {
        x <- move(x, moves = "DR'DRFDF'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #1.6a
      {
        x <- move(x, moves = "DFD2F'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #1.6b
      {
        x <- move(x, moves = "D2R'DRFDF'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #1.7a
      {
        x <- move(x, moves = "D2FD2F'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #1.7b
      {
        x <- move(x, moves = "D'R'DRFDF'")
        c <- c + 7 
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #1.8a
      {
        x <- move(x, moves = "D'FD2F'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #1.8b
      {
        x <- move(x, moves = "R'DRFDF'")
        c <- c + 6 
      }
    }
    
    
    if (x$cp["UFL"]==1 && x$co["UFL"]==0)           #2nd corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #2.1a
      {
        x <- move(x, moves = "U'R'D'RUR'DR")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #2.1b
      {
        x <- move(x, moves = "F2DF2D'FD'FR'D2R")
        c <- c + 10
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #2.2a
      {
        x <- move(x, moves = "F'DF2D2F'D'FD2F'")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #2.2b
      {
        x <- move(x, moves = "F'D'FR'D2R")
        c <- c + 6
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #2.3a
      {
        x <- move(x, moves = "UL'D'LU'DR'D2R")
        c <- c + 8
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #2.3b
      {
        x <- move(x, moves = "L2D'L2DL'D2L'FD'F'")
        c <- c + 10
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #2.4a
      {
        x <- move(x, moves = "R2U'R'D'RURDR")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #2.4b
      {
        x <- move(x, moves = "U2B'D2BU2R'DR")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #2.5a
      {
        x <- move(x, moves = "LDL'R'D'R")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #2.5b
      {
        x <- move(x, moves = "D'F'D2FD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #2.6a
      {
        x <- move(x, moves = "D'LD2L'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #2.6b
      {
        x <- move(x, moves = "F'D2FD'FDF'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #2.7a
      {
        x <- move(x, moves = "LD2L'D'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #2.7b
      {
        x <- move(x, moves = "DF'D2FD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #2.8a
      {
        x <- move(x, moves = "DLD2L'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #2.8b
      {
        x <- move(x, moves = "F'DFD'FD2F'")
        c <- c + 7
      }
    }
    else if (x$cp["UFL"]==1 && x$co["UFL"]==1)      #2nd corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #2.1a
      {
        x <- move(x, moves = "FD'F2D'F2D2F'")
        c <- c + 7
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #2.1b
      {
        x <- move(x, moves = "R'D2RF'D'F2D2F'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #2.2a
      {
        x <- move(x, moves = "FDF'LD'L'D'FDF'")
        c <- c + 10
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #2.2b
      {
        x <- move(x, moves = "LD'L'FDF'D'FD2F'")
        c <- c + 10
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #2.3a
      {
        x <- move(x, moves = "BDB'F'D'F2D2F'")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #2.3b
      {
        x <- move(x, moves = "FL'F2D'F2D2F'L")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #2.4a
      {
        x <- move(x, moves = "B'DBF'D'F2D2F'")
        c <- c + 7
        #middle
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #2.4b
      {
        x <- move(x, moves = "RD2R'F'D'F2D2F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #2.5a
      {
        x <- move(x, moves = "DLD'L'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #2.5b
      {
        x <- move(x, moves = "D2F'D'F2D2F'")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #2.6a
      {
        x <- move(x, moves = "F'D'FD2R'D'R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #2.6b
      {
        x <- move(x, moves = "D'F'D'F2D2F'")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #2.7a
      {
        x <- move(x, moves = "D'LD'L'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #2.7b
      {
        x <- move(x, moves = "F'D'F2D2F'")
        c <- c + 5
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #2.8a
      {
        x <- move(x, moves = "LD'L'DR'D'R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #2.8b
      {
        x <- move(x, moves = "DF'D'F2D2F'")
        c <- c + 6
      }
    }
    else if (x$cp["UFL"]==1 && x$co["UFL"]==2)      #2nd corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #2.1a
      {
        x <- move(x, moves = "R'D'LDRL'")
        c <- c + 5
        #middle
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #2.1b
      {
        x <- move(x, moves = "FDF'D'LR'DL'R")
        c <- c + 7
        #middle
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #2.2a
      {
        x <- move(x, moves = "F'DFDF'DF2DF'")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #2.2b
      {
        x <- move(x, moves = "F'DFDF'D'FD'R'DR")
        c <- c + 11
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #2.3a
      {
        x <- move(x, moves = "L'DL2R'DRL'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #2.3b
      {
        x <- move(x, moves = "BD2B'LR'DRL'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #2.4a
      {
        x <- move(x, moves = "RD'R2LDRL'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #2.4b
      {
        x <- move(x, moves = "B'D2BLR'DRL'")
        c <- c + 6
        #middle
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #2.5a
      {
        x <- move(x, moves = "D'LR'DRL'")
        c <- c + 4
        #middle
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #2.5b
      {
        x <- move(x, moves = "F'DFDFDF'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #2.6a
      {
        x <- move(x, moves = "LR'DL'R")
        c <- c + 3
        #middle
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #2.6b
      {
        x <- move(x, moves = "DF'DFDFDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #2.7a
      {
        x <- move(x, moves = "DLR'DRL'")
        c <- c + 4
        #middle
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #2.7b
      {
        x <- move(x, moves = "D2F'DFDFDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #2.8a
      {
        x <- move(x, moves = "D2LR'DRL'")
        c <- c + 4
        #middle
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #2.8b
      {
        x <- move(x, moves = "D'F'DFDFDF'")
        c <- c + 8 
      }
    }
    
    
    if (x$cp["ULB"]==1 && x$co["ULB"]==0)           #3rd corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #3.1a
      {
        x <- move(x, moves = "U2R'D'RU2R'DR")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #3.1b
      {
        x <- move(x, moves = "L'DLFD'F'D'R'D'R")
        c <- c + 10
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #3.2a
      {
        x <- move(x, moves = "L2DL2D'LR'D2RL")
        c <- c + 8
        #middle
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #3.2b
      {
        x <- move(x, moves = "U'F'D'FUR'D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #3.3a
      {
        x <- move(x, moves = "BFD2B'F'")
        c <- c + 5
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #3.3b
      {
        x <- move(x, moves = "L'DLD'BD2B'FD'F'")
        c <- c + 10
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #3.4a
      {
        x <- move(x, moves = "B2D'B2DB'FD2F'B'")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #3.4b
      {
        x <- move(x, moves = "URDR'U'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #3.5a
      {
        x <- move(x, moves = "D'BD'B'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #3.5b
      {
        x <- move(x, moves = "L'DLFD2F'")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #3.6a
      {
        x <- move(x, moves = "BD'B'R'D2R")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #3.6b
      {
        x <- move(x, moves = "DL'DLFD2F'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #3.7a
      {
        x <- move(x, moves = "DBD'B'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #3.7b
      {
        x <- move(x, moves = "D2L'DLFD2F'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #3.8a
      {
        x <- move(x, moves = "D2BD'B'R'D2R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #3.8b
      {
        x <- move(x, moves = "D'L'DLFD2F'")
        c <- c + 7
      }
    }
    else if (x$cp["ULB"]==1 && x$co["ULB"]==1)      #3rd corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #3.1a
      {
        x <- move(x, moves = "FD2L'D2LF'")
        c <- c + 6
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #3.1b
      {
        x <- move(x, moves = "R'DRL'FD2F'L")
        c <- c + 7
        #middle
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #3.2a
      {
        x <- move(x, moves = "F'D2F2L'D2LF'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #3.2b
      {
        x <- move(x, moves = "LD'L2FD2F'L")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #3.3a
      {
        x <- move(x, moves = "BD'B'DFDF'D'FD2F'")
        c <- c + 11
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #3.3b
      {
        x <- move(x, moves = "BD'B'DFD'F'DR'D'R")
        c <- c + 11
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #3.4a
      {
        x <- move(x, moves = "LB'L2FD2F'LB")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #3.4b
      {
        x <- move(x, moves = "RDR'L'FD2F'L")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #3.5a
      {
        x <- move(x, moves = "BD'B'D2R'D'R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #3.5b
      {
        x <- move(x, moves = "DL'FD2F'L")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #3.6a
      {
        x <- move(x, moves = "DBD'B'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #3.6b
      {
        x <- move(x, moves = "D2L'FD2F'L")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #3.7a
      {
        x <- move(x, moves = "D2BD'B'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #3.7b
      {
        x <- move(x, moves = "D'L'FD2F'L")
        c <- c + 6
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #3.8a
      {
        x <- move(x, moves = "D'BD'B'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #3.8b
      {
        x <- move(x, moves = "L'FD2F'L")
        c <- c + 5
      }
    }
    else if (x$cp["ULB"]==1 && x$co["ULB"]==2)      #3rd corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #3.1a
      {
        x <- move(x, moves = "R'D2RBR'D2RB'")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #3.1b
      {
        x <- move(x, moves = "FD'F'BR'D2RB'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #3.2a
      {
        x <- move(x, moves = "B'LB2R'D2RB'L'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #3.2b
      {
        x <- move(x, moves = "F'D'FBR'D2RB'")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #3.3a
      {
        x <- move(x, moves = "L'DLD'R'D'RDR'D2R")
        c <- c + 11
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #3.3b
      {
        x <- move(x, moves = "L'DLDFDF'DFDF'")
        c <- c + 11
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #3.4a
      {
        x <- move(x, moves = "L'D'LRD'R2DR")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #3.4b
      {
        x <- move(x, moves = "B'DB2R'D2RB'")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #3.5a
      {
        x <- move(x, moves = "D2BR'D2RB'")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #3.5b
      {
        x <- move(x, moves = "D'L'DLD2FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #3.6a
      {
        x <- move(x, moves = "D'BR'D2RB'")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #3.6b
      {
        x <- move(x, moves = "L'DLD2FDF'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #3.7a
      {
        x <- move(x, moves = "BR'D2RB'")
        c <- c + 5
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #3.7b
      {
        x <- move(x, moves = "DL'DLD2FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #3.8a
      {
        x <- move(x, moves = "DBR'D2RB'")
        c <- c + 6
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #3.8b
      {
        x <- move(x, moves = "D2L'DLD2FDF'")
        c <- c + 8
      }
    }
    
    
    
    if (x$cp["UBR"]==1 && x$co["UBR"]==0)           #4th corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #4.1a
      {
        x <- move(x, moves = "UR'D'RU'R'DR")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #4.1b
      {
        x <- move(x, moves = "R2D'R2DR'DR'FD2F'")
        c <- c + 10
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #4.2a
      {
        x <- move(x, moves = "F2UFDF2D'FU'F2")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #4.2b
      {
        x <- move(x, moves = "U2F'D'FU2R'D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #4.3a
      {
        x <- move(x, moves = "U'L'D'LUDR'D2R")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #4.3b
      {
        x <- move(x, moves = "B2DB2D'BD2BR'DR")
        c <- c + 10
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #4.4a
      {
        x <- move(x, moves = "RD'R2D2RDR'D2R")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #4.4b
      {
        x <- move(x, moves = "B'D2BR'DR")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #4.5a
      {
        x <- move(x, moves = "D2RD'R'DR'D2R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #4.5b
      {
        x <- move(x, moves = "D'B'DBFD'F'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #4.6a
      {
        x <- move(x, moves = "D'RD'R'DR'D2R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #4.6b
      {
        x <- move(x, moves = "B'DBFD'F'")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #4.7a
      {
        x <- move(x, moves = "RD'R'DR'D2R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #4.7b
      {
        x <- move(x, moves = "DB'DBFD'F'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #4.8a
      {
        x <- move(x, moves = "DRD'R'DR'D2R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #4.8b
      {
        x <- move(x, moves = "D2B'DBFD'F'")
        c <- c + 7
      }
    }
    else if (x$cp["UBR"]==1 && x$co["UBR"]==1)      #4th corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #4.1a
      {
        x <- move(x, moves = "FDB'D'F'B")
        c <- c + 5
        #middle
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #4.1b
      {
        x <- move(x, moves = "BR'B2D2BRD2FD'F'")
        c <- c + 10
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #4.2a
      {
        x <- move(x, moves = "F'DF2B'D'F'B")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #4.2b
      {
        x <- move(x, moves = "LD2L'FB'D'F'B")
        c <- c + 6
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #4.3a
      {
        x <- move(x, moves = "BD'B2FD'F'B")
        c <- c + 6
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #4.3b
      {
        x <- move(x, moves = "L'D2LFB'D'F'B")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #4.4a
      {
        x <- move(x, moves = "RD'R2D'RD'R'D'R")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #4.4b
      {
        x <- move(x, moves = "RD'R'D'RDR'DFD'F'")
        c <- c + 11
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #4.5a
      {
        x <- move(x, moves = "D'RD'R'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #4.5b
      {
        x <- move(x, moves = "FB'D'F'B")
        c <- c + 3
        #middle
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #4.6a
      {
        x <- move(x, moves = "RD'R'D'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #4.6b
      {
        x <- move(x, moves = "DFB'D'F'B")
        c <- c + 4
        #middle
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #4.7a
      {
        x <- move(x, moves = "DRD'R'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #4.7b
      {
        x <- move(x, moves = "D2FB'D'F'B")
        c <- c + 4
        #middle
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #4.8a
      {
        x <- move(x, moves = "D2RD'R'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #4.8b
      {
        x <- move(x, moves = "D'FB'D'F'B")
        c <- c + 4
        #middle
      }
    }
    else if (x$cp["UBR"]==1 && x$co["UBR"]==2)      #4th corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #4.1a
      {
        x <- move(x, moves = "R'DR2DR2D2R")
        c <- c + 7
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #4.1b
      {
        x <- move(x, moves = "FD2F'RDR2D2R")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #4.2a
      {
        x <- move(x, moves = "LD'L'RDR2D2R")
        c <- c + 7
        #middle
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #4.2b
      {
        x <- move(x, moves = "F'D2FRDR2D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #4.3a
      {
        x <- move(x, moves = "L'D'LRDR2D2R")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #4.3b
      {
        x <- move(x, moves = "R'BR2D2R2B'DR")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #4.4a
      {
        x <- move(x, moves = "B'DBD'RDR'FDF'")
        c <- c + 10
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #4.4b
      {
        x <- move(x, moves = "B'DBR'D'RDR'D2R")
        c <- c + 10
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #4.5a
      {
        x <- move(x, moves = "DRDR2D2R")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #4.5b
      {
        x <- move(x, moves = "D2B'DBD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #4.6a
      {
        x <- move(x, moves = "D2RDR2D2R")
        c <- c + 6
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #4.6b
      {
        x <- move(x, moves = "D'B'DBD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #4.7a
      {
        x <- move(x, moves = "D'RDR2D2R")
        c <- c + 6
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #4.7b
      {
        x <- move(x, moves = "B'DBD'FDF'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #4.8a
      {
        x <- move(x, moves = "RDR2D2R")
        c <- c + 5
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #4.8b
      {
        x <- move(x, moves = "DB'DBD'FDF'")
        c <- c + 8
      }
    }
    
    
    if (x$cp["DFR"]==1 && x$co["DFR"]==0)           #5th corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #5.1a
      {
        x <- move(x, moves = "R'D2RFD2F'DR'D'R")
        c <- c + 10
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #5.1b
      {
        x <- move(x, moves = "'FD'F'R'D2R")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #5.2a
      {
        x <- move(x, moves = "D'F'DF2D'F'")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #5.2b
      {
        x <- move(x, moves = "DF2D'FR'DR2F2R'")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #5.3a
      {
        x <- move(x, moves = "FL'DLD'F'R'D2R")
        c <- c + 9
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #5.3b
      {
        x <- move(x, moves = "D2FL'DLF'")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #5.4a
      {
        x <- move(x, moves = "DRD'R2DR")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #5.4b
      {
        x <- move(x, moves = "FD'B'DBF'R'D2R")
        c <- c + 9
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #5.5a
      {
        x <- move(x, moves = "R'D2RDR'D'R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #5.5b
      {
        x <- move(x, moves = "R'D'RD'R'D2RFD2F'")
        c <- c + 10
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #5.6a
      {
        x <- move(x, moves = "FD2F'R'D2RDR'D2R")
        c <- c + 10
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #5.6b
      {
        x <- move(x, moves = "FD2F'D'FDF'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #5.7a
      {
        x <- move(x, moves = "R'DRD2R'D'R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #5.7b
      {
        x <- move(x, moves = "DFD2F'DFD'F'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #5.8a
      {
        x <- move(x, moves = "D'R'D2RD'R'DR")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #5.8b
      {
        x <- move(x, moves = "FD'F'D2FDF'")
        c <- c + 7
      }
    }
    else if (x$cp["DFR"]==1 && x$co["DFR"]==1)      #5th corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #5.1a
      {
        x <- move(x, moves = "DR'DRD2R'DR")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #5.1b
      {
        x <- move(x, moves = "D2R'DRDFDF'")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #5.2a
      {
        x <- move(x, moves = "R'D2RF'DF2D'F'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #5.2b
      {
        x <- move(x, moves = "F'DFD'R'DR")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #5.3a
      {
        x <- move(x, moves = "D'L'DLR'DR")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #5.3b
      {
        x <- move(x, moves = "R'D'BD'B'R")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #5.4a
      {
        x <- move(x, moves = "D'B'DBFDF'")
        c <- c + 7
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #5.4b
      {
        x <- move(x, moves = "RD2R'FDF'")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #5.5a
      {
        x <- move(x, moves = "D'R'DR")
        c <- c + 4
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #5.5b
      {
        x <- move(x, moves = "DR'D2RD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #5.6a
      {
        x <- move(x, moves = "FD'F'D2R'D'R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #5.6b
      {
        x <- move(x, moves = "D'FD'F'DFDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #5.7a
      {
        x <- move(x, moves = "DR'D2RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #5.7b
      {
        x <- move(x, moves = "FDF'")
        c <- c + 3
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #5.8a
      {
        x <- move(x, moves = "DR'D'RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #5.8b
      {
        x <- move(x, moves = "D'FDF'DFDF'")
        c <- c + 8
      }
    }
    else if (x$cp["DFR"]==1 && x$co["DFR"]==2)      #5th corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #5.1a
      {
        x <- move(x, moves = "D'FD'F'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #5.1b
      {
        x <- move(x, moves = "D2FD'F'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #5.2a
      {
        x <- move(x, moves = "DLD'L'R'D'R")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #5.2b
      {
        x <- move(x, moves = "D2LD'L'FD2F'")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #5.3a
      {
        x <- move(x, moves = "DBD'B'FD'F'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #5.3b
      {
        x <- move(x, moves = "BR'D'RB'")
        c <- c + 5
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #5.4a
      {
        x <- move(x, moves = "D'RD'R'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #5.4b
      {
        x <- move(x, moves = "RD'R'DFD'F'")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #5.5a
      {
        x <- move(x, moves = "DR'DRD'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #5.5b
      {
        x <- move(x, moves = "R'DRD2FDF'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #5.6a
      {
        x <- move(x, moves = "D'FD2F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #5.6b
      {
        x <- move(x, moves = "DFD'F'")
        c <- c + 4
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #5.7a
      {
        x <- move(x, moves = "D'FD'F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #5.7b
      {
        x <- move(x, moves = "D'FDF'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #5.8a
      {
        x <- move(x, moves = "R'D'R")
        c <- c + 3
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #5.8b
      {
        x <- move(x, moves = "D'FD2F'D'FD2F'")
        c <- c + 8
      }
    }
    
    
    if (x$cp["DLF"]==1 && x$co["DLF"]==0)           #6th corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #6.1a
      {
        x <- move(x, moves = "DR'D2RFD2F'DR'D'R")
        c <- c + 11
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #6.1b
      {
        x <- move(x, moves = "DR'DRFD2F'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #6.2a
      {
        x <- move(x, moves = "F'DF2D'F'")
        c <- c + 5
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #6.2b
      {
        x <- move(x, moves = "DR'DRF'D'FD'R'D'R")
        c <- c + 11
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #6.3a
      {
        x <- move(x, moves = "DFL'DLD'F'R'D2R")
        c <- c + 10
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #6.3b
      {
        x <- move(x, moves = "D'FL'DLF'")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #6.4a
      {
        x <- move(x, moves = "DRD'R2DR")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #6.4b
      {
        x <- move(x, moves = "DRDR'FD'F'D2FDF'")
        c <- c + 11
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #6.5a
      {
        x <- move(x, moves = "R'D2RD'R'DR")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #6.5b
      {
        x <- move(x, moves = "DFD'F'D2FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #6.6a
      {
        x <- move(x, moves = "DR'D2RDR'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #6.6b
      {
        x <- move(x, moves = "DR'D'RD'R'D2RFD2F'")
        c <- c + 11
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #6.7a
      {
        x <- move(x, moves = "DFDF'DFD2F'R'D2R")
        c <- c + 11
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #6.7b
      {
        x <- move(x, moves = "DFD2F'D'FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #6.8a
      {
        x <- move(x, moves = "DR'DRD2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #6.8b
      {
        x <- move(x, moves = "D2FD2F'DFD'F'")
        c <- c + 8
      }
    }
    else if (x$cp["DLF"]==1 && x$co["DLF"]==1)      #6th corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #6.1a
      {
        x <- move(x, moves = "D2R'DRDR'D2R")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #6.1b
      {
        x <- move(x, moves = "D'R'DRDFDF'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #6.2a
      {
        x <- move(x, moves = "DR'D2RF'DF2D'F'")
        c <- c + 9
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #6.2b
      {
        x <- move(x, moves = "DF'DFD'R'DR")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #6.3a
      {
        x <- move(x, moves = "L'DLR'DR")
        c <- c + 5
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #6.3b
      {
        x <- move(x, moves = "DR'DBD'B'R")
        c <- c + 7
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #6.4a
      {
        x <- move(x, moves = "DR'DRD'RD'R2DR")
        c <- c + 10
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #6.4b
      {
        x <- move(x, moves = "D'B'DBR'D2R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #6.5a
      {
        x <- move(x, moves = "D2R'D'RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #6.5b
      {
        x <- move(x, moves = "FDF'DFDF'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #6.6a
      {
        x <- move(x, moves = "R'DR")
        c <- c + 3
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #6.6b
      {
        x <- move(x, moves = "D2R'D2RD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #6.7a
      {
        x <- move(x, moves = "DFD'F'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #6.7b
      {
        x <- move(x, moves = "FD'F'DFDF'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #6.8a
      {
        x <- move(x, moves = "D2R'D2RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #6.8b
      {
        x <- move(x, moves = "DFDF'")
        c <- c + 4
      }
    }
    else if (x$cp["DLF"]==1 && x$co["DLF"]==2)      #6th corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #6.1a
      {
        x <- move(x, moves = "FD'F'D'FD2F'")
        c <- c + 7
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #6.1b
      {
        x <- move(x, moves = "D'FD'F'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #6.2a
      {
        x <- move(x, moves = "LD'L'R'D'R")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #6.2b
      {
        x <- move(x, moves = "D'LD'L'FD2F'")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #6.3a
      {
        x <- move(x, moves = "D2BD'B'FD'F'")
        c <- c + 6
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #6.3b
      {
        x <- move(x, moves = "DFD'L'DLF'")
        c <- c + 7
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #6.4a
      {
        x <- move(x, moves = "DFD2F'RD'R2DR")
        c <- c + 9
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #6.4b
      {
        x <- move(x, moves = "DRD'R'DFD'F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #6.5a
      {
        x <- move(x, moves = "DR'D'R")
        c <- c + 4
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #6.5b
      {
        x <- move(x, moves = "FD2F'D'FD2F'")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #6.6a
      {
        x <- move(x, moves = "D2R'DRD'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #6.6b
      {
        x <- move(x, moves = "DR'DRD2FDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #6.7a
      {
        x <- move(x, moves = "FD2F'DR'D'R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #6.7b
      {
        x <- move(x, moves = "D'FD2F'")
        c <- c + 4
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #6.8a
      {
        x <- move(x, moves = "FD'F'DR'D'R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #6.8b
      {
        x <- move(x, moves = "FDF'D'FD2F'")
        c <- c + 7
      }
    }
    
    
    
    if (x$cp["DBL"]==1 && x$co["DBL"]==0)           #7th corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #7.1a
      {
        x <- move(x, moves = "FD'F'DFDF'DFD'F'")
        c <- c + 11
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #7.1b
      {
        x <- move(x, moves = "D2R'DRFD2F'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #7.2a
      {
        x <- move(x, moves = "DF'DF2D'F'")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #7.2b
      {
        x <- move(x, moves = "LDL'FDF'DFD'F'")
        c <- c + 10
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #7.3a
      {
        x <- move(x, moves = "L'D2LBD2B'D'R'D'R")
        c <- c + 10
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #7.3b
      {
        x <- move(x, moves = "FL'DLF'")
        c <- c + 5
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #7.4a
      {
        x <- move(x, moves = "D'RD'R2DR")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #7.4b
      {
        x <- move(x, moves = "D2FD'F'B'DBR'D2R")
        c <- c + 10
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #7.5a
      {
        x <- move(x, moves = "R'D'RD'R'DR")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #7.5b
      {
        x <- move(x, moves = "D'FD2F'DFD'F'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #7.6a
      {
        x <- move(x, moves = "DR'D2RD'R'DR")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #7.6b
      {
        x <- move(x, moves = "FDF'DFD'F'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #7.7a
      {
        x <- move(x, moves = "D2R'D2RDR'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #7.7b
      {
        x <- move(x, moves = "D2R'D'RD'R'D2RFD2F'")
        c <- c + 11
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #7.8a
      {
        x <- move(x, moves = "D2FDF'DFD2F'R'D2R")
        c <- c + 11
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #7.8b
      {
        x <- move(x, moves = "D2FD2F'D'FDF'")
        c <- c + 8
      }
    }
    else if (x$cp["DBL"]==1 && x$co["DBL"]==1)      #7th corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #7.1a
      {
        x <- move(x, moves = "D'R'DRDR'D2R")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #7.1b
      {
        x <- move(x, moves = "R'DRDFDF'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #7.2a
      {
        x <- move(x, moves = "D'F'DFD2FDF'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #7.2b
      {
        x <- move(x, moves = "D2F'DFD'R'DR")
        c <- c + 8
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #7.3a
      {
        x <- move(x, moves = "DL'DLR'DR")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #7.3b
      {
        x <- move(x, moves = "D2L'FDF'L")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #7.4a
      {
        x <- move(x, moves = "DB'DBFDF'")
        c <- c + 7
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #7.4b
      {
        x <- move(x, moves = "B'DBR'D2R")
        c <- c + 6
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #7.5a
      {
        x <- move(x, moves = "D'R'D2RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #7.5b
      {
        x <- move(x, moves = "D2FDF'")
        c <- c + 4
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #7.6a
      {
        x <- move(x, moves = "D'R'D'RDR'D2R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #7.6b
      {
        x <- move(x, moves = "D'R'DRD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #7.7a
      {
        x <- move(x, moves = "R'D2R")
        c <- c + 3
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #7.7b
      {
        x <- move(x, moves = "D'R'D2RD'FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #7.8a
      {
        x <- move(x, moves = "D2FD'F'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #7.8b
      {
        x <- move(x, moves = "DFD'F'DFDF'")
        c <- c + 8
      }
    }
    else if (x$cp["DBL"]==1 && x$co["DBL"]==2)      #7th corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #7.1a
      {
        x <- move(x, moves = "DFD'F'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #7.1b
      {
        x <- move(x, moves = "FD'F'D'R'D'R")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #7.2a
      {
        x <- move(x, moves = "D'LD'L'R'D'R")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #7.2b
      {
        x <- move(x, moves = "LD'L'FD2F'")
        c <- c + 6
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #7.3a
      {
        x <- move(x, moves = "D'BD'B'FD'F'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #7.3b
      {
        x <- move(x, moves = "D2BR'D'RB'")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #7.4a
      {
        x <- move(x, moves = "DRD'R'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #7.4b
      {
        x <- move(x, moves = "D2RD'R'DFD'F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #7.5a
      {
        x <- move(x, moves = "DFD'F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #7.5b
      {
        x <- move(x, moves = "DFDF'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #7.6a
      {
        x <- move(x, moves = "D2R'D'R")
        c <- c + 4
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #7.6b
      {
        x <- move(x, moves = "DFD2F'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #7.7a
      {
        x <- move(x, moves = "D'R'DRD'R'D'R")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #7.7b
      {
        x <- move(x, moves = "D2R'DRD2FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #7.8a
      {
        x <- move(x, moves = "DFD2F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #7.8b
      {
        x <- move(x, moves = "FD2F'")
        c <- c + 3
      }
    }
    
    
    
    if (x$cp["DRB"]==1 && x$co["DRB"]==0)           #8th corner
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #8.1a
      {
        x <- move(x, moves = "D'R'D2RFD2F'DR'D'R")
        c <- c + 11
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #8.1b
      {
        x <- move(x, moves = "D'FD'F'R'D2R")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #8.2a
      {
        x <- move(x, moves = "D2F'DF2D'F'")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #8.2b
      {
        x <- move(x, moves = "D'F'D'FR'DRD2R'D'R")
        c <- c + 11
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #8.3a
      {
        x <- move(x, moves = "D'R'BD'B'DRFD2F'")
        c <- c + 10
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #8.3b
      {
        x <- move(x, moves = "DR'BD'B'R")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #8.4a
      {
        x <- move(x, moves = "RD'R2DR")
        c <- c + 5
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #8.4b
      {
        x <- move(x, moves = "D'FD'F'B'DBR'D2R")
        c <- c + 10
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #8.5a
      {
        x <- move(x, moves = "D'FDF'DFD2F'R'D2R")
        c <- c + 11
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #8.5b
      {
        x <- move(x, moves = "D'FD2F'D'FDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #8.6a
      {
        x <- move(x, moves = "D'R'DRD2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #8.6b
      {
        x <- move(x, moves = "FD2F'DFD'F'")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #8.7a
      {
        x <- move(x, moves = "D2R'D2RD'R'DR")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #8.7b
      {
        x <- move(x, moves = "D'FD'F'D2FDF'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #8.8a
      {
        x <- move(x, moves = "D'R'D2RDR'D'R")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #8.8b
      {
        x <- move(x, moves = "D'R'D'RD'R'D2RFD2F'")
        c <- c + 11
      }
    }
    else if (x$cp["DRB"]==1 && x$co["DRB"]==1)      #8th corner turned clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #8.1a
      {
        x <- move(x, moves = "R'DRDR'D2R")
        c <- c + 7
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #8.1b
      {
        x <- move(x, moves = "DR'DRDFDF'")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #8.2a
      {
        x <- move(x, moves = "F'DFD2FDF'")
        c <- c + 7
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #8.2b
      {
        x <- move(x, moves = "DR'D2F'DFR")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #8.3a
      {
        x <- move(x, moves = "D2L'DLR'DR")
        c <- c + 6
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #8.3b
      {
        x <- move(x, moves = "D'L'FDF'L")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #8.4a
      {
        x <- move(x, moves = "D2B'DFDBF'")
        c <- c + 6
        #middle
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #8.4b
      {
        x <- move(x, moves = "DB'DBR'D2R")
        c <- c + 7
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #8.5a
      {
        x <- move(x, moves = "D'FD'F'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #8.5b
      {
        x <- move(x, moves = "D2FD'F'DFDF'")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #8.6a
      {
        x <- move(x, moves = "R'D2RDR'D2R")
        c <- c + 7
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #8.6b
      {
        x <- move(x, moves = "D'FDF'")
        c <- c + 4
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #8.7a
      {
        x <- move(x, moves = "R'D'RDR'D2R")
        c <- c + 7
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #8.7b
      {
        x <- move(x, moves = "R'DRD'FDF'")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #8.8a
      {
        x <- move(x, moves = "DR'D2R")
        c <- c + 4
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #8.8b
      {
        x <- move(x, moves = "R'D2RD'FDF'")
        c <- c + 7
      }
    }
    else if (x$cp["DRB"]==1 && x$co["DRB"]==2)      #8th corner turned anti-clockwise
    {
      if (x$ep["FR"]==1 && x$eo["FR"]==0)            #8.1a
      {
        x <- move(x, moves = "D2FD'F'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["FR"]==1 && x$eo["FR"]==1)       #8.1b
      {
        x <- move(x, moves = "DFD'F'D'R'D'R")
        c <- c + 8
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==0)       #8.2a
      {
        x <- move(x, moves = "LD'L'R'D'R")
        c <- c + 6
      }
      else if (x$ep["FL"]==1 && x$eo["FL"]==1)       #8.2b
      {
        x <- move(x, moves = "DLD'L'FD2F'")
        c <- c + 7
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==0)       #8.3a
      {
        x <- move(x, moves = "BD'B'FD'F'")
        c <- c + 5
        #middle
      }
      else if (x$ep["BL"]==1 && x$eo["BL"]==1)       #8.3b
      {
        x <- move(x, moves = "D'BR'D'RB'")
        c <- c + 6
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==0)       #8.4a
      {
        x <- move(x, moves = "D2RD'R'D2R'D'R")
        c <- c + 8
      }
      else if (x$ep["BR"]==1 && x$eo["BR"]==1)       #8.4b
      {
        x <- move(x, moves = "D'RD'R'DFD'F'")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==0)       #8.5a
      {
        x <- move(x, moves = "D2FD2F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DR"]==1 && x$eo["DR"]==1)       #8.5b
      {
        x <- move(x, moves = "FD'F'")
        c <- c + 3
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==0)       #8.6a
      {
        x <- move(x, moves = "D2FD'F'DR'D'R")
        c <- c + 8
      }
      else if (x$ep["DF"]==1 && x$eo["DF"]==1)       #8.6b
      {
        x <- move(x, moves = "D2FDF'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==0)       #8.7a
      {
        x <- move(x, moves = "D'R'D'R")
        c <- c + 4
      }
      else if (x$ep["DL"]==1 && x$eo["DL"]==1)       #8.7b
      {
        x <- move(x, moves = "D2FD2F'D'FD2F'")
        c <- c + 8
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==0)       #8.8a
      {
        x <- move(x, moves = "R'DRD'R'D'R")
        c <- c + 7
      }
      else if (x$ep["DB"]==1 && x$eo["DB"]==1)       #8.8b
      {
        x <- move(x, moves = "D'R'DRD2FDF'")
        c <- c + 8
      }
    }
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  red_blue_block <- function(x,c)
  {
    x <- move(x, moves = "y")
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    x <- move(x, moves = "y'")
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  blue_orange_block <- function(x,c)
  {
    x <- move(x, moves = "y2")
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    x <- move(x, moves = "y2")
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  orange_green_block <- function(x,c)
  {
    x <- move(x, moves = "y'")
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    x <- move(x, moves = "y")
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  F2L <- function(x,c)
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    
    
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    
    
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    
    
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  OLL_perms <- function(x,c)
  {
    if (x$eo["UR"]==1 && x$eo["UF"]==1 && x$eo["UL"]==1 && x$eo["UB"]==1)                 #4 EDGES ORIENTED INCORRECTLY
    {
      if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==2)           #8
      {
        x <- move(x, moves = "RU2R2FRF'U2R'FRF'")
        c <- c + 11
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==1)      #9
      {
        x <- move(x, moves = "FRUR'U'F'UL'U'LFRUR'F'")
        c <- c + 15
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==0 && x$co["UBR"]==2)      #10
      {
        x <- move(x, moves = "LF'L'FU2FU'RU'R'F'")
        c <- c + 11
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==0)      #11
      {
        x <- move(x, moves = "RU2R2FRF'U2M'URU'L'x'")
        c <- c + 12
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==0)      #12
      {
        x <- move(x, moves = "R'U2FRUR'U'F2U2FR")
        c <- c + 12
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==2)      #13
      {
        x <- move(x, moves = "MUR'F2RUL'ULM'")
        c <- c + 9
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==1 && x$co["UBR"]==1)      #14
      {
        x <- move(x, moves = "MU'LF2L'U'RU'R'M'")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==0)      #15
      {
        x <- move(x, moves = "MURUR'U'M2URU'L'x'")
        c <- c + 11
      }
    }
    else if (x$eo["UR"]==0 && x$eo["UF"]==1 && x$eo["UL"]==1 && x$eo["UB"]==0)            #2 EDGES ORIENTED INCORRECTLY "L-shape"
    {
      if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==0)           #16
      {
        x <- move(x, moves = "R'U'FURU'R'F'R")
        c <- c + 9
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==0)      #19
      {
        x <- move(x, moves = "F'U'L'ULF")
        c <- c + 6
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==1)      #20
      {
        x <- move(x, moves = "L'U'LU'L'ULULF'L'F")
        c <- c + 12
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==2)      #23
      {
        x <- move(x, moves = "LFR'FRF'R'FRF2L'")
        c <- c + 11
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==2)      #24
      {
        x <- move(x, moves = "F'L'U'LUL'U'LUF")
        c <- c + 10
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==1 && x$co["UBR"]==0)      #39
      {
        x <- move(x, moves = "LF2R'F'RF'L'")
        c <- c + 7
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==0)      #40
      {
        x <- move(x, moves = "x'R'F2R2U'R'UR'F2Rx")
        c <- c + 9
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==1 && x$co["UBR"]==1)      #43
      {
        x <- move(x, moves = "R'F'LF'L'F2R")
        c <- c + 7
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==0 && x$co["UBR"]==1)      #45
      {
        x <- move(x, moves = "LR2F'RF'R'F2RF'Mx")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==1 && x$co["UBR"]==2)      #46
      {
        x <- move(x, moves = "F'LF'L2ULUL'U'LF2")
        c <- c + 11
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==2)      #49
      {
        x <- move(x, moves = "(LF'L'FLF'L'FL'U'LUL'U'L")
        c <- c + 15
      }
    }
    else if (x$eo["UR"]==1 && x$eo["UF"]==1 && x$eo["UL"]==0 && x$eo["UB"]==0)            #2 EDGES ORIENTED INCORRECTLY "L-shape"
    {
      if (x$co["URF"]==2 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==1)           #17
      {
        x <- move(x, moves = "LUF'U'L'ULFL'")
        c <- c + 9
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==2)      #18
      {
        x <- move(x, moves = "FURU'R'F'")
        c <- c + 6
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==0)      #21
      {
        x <- move(x, moves = "RUR'URU'R'U'R'FRF'")
        c <- c + 12
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==2)      #22
      {
        x <- move(x, moves = "R'F'LF'L'FLF'L'F2R")
        c <- c + 11
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==1)      #25
      {
        x <- move(x, moves = "FRUR'U'RUR'U'F'")
        c <- c + 10
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==2)      #38
      {
        x <- move(x, moves = "R'F2LFL'FR")
        c <- c + 7
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==0 && x$co["UBR"]==2)      #41
      {
        x <- move(x, moves = "FR'F'RURU'R'")
        c <- c + 8
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==2)      #42
      {
        x <- move(x, moves = "LFR'FRF2L'")
        c <- c + 7
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==2 && x$co["UBR"]==0)      #44
      {
        x <- move(x, moves = "R'L2FL'FLF2L'FMx")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==1 && x$co["UBR"]==2)      #47
      {
        x <- move(x, moves = "FR'FR2U'R'U'RUR'F2")
        c <- c + 11
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==0)      #48
      {
        x <- move(x, moves = "R'FRF'R'FRF'RUR'U'RUR'")
        c <- c + 15
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==0)      #56
      {
        x <- move(x, moves = "RL'BLR'U2RL'BLR'")
        c <- c + 7
      }
    }
    else if (x$eo["UR"]==0 && x$eo["UF"]==0 && x$eo["UL"]==1 && x$eo["UB"]==1)            #2 EDGES ORIENTED INCORRECTLY "L-shape"
    {
      if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==1)           #26
      {
        x <- move(x, moves = "L'BL2F'L2B'L2FL'")
        c <- c + 9
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==0 && x$co["UBR"]==1)      #28
      {
        x <- move(x, moves = "L'U'Ly'LF'L'ULFL'y")
        c <- c + 10
      }
    }
    else if (x$eo["UR"]==1 && x$eo["UF"]==0 && x$eo["UL"]==0 && x$eo["UB"]==1)            #2 EDGES ORIENTED INCORRECTLY "L-shape"
    {
      if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==2)           #27
      {
        x <- move(x, moves = "RB'R2FR2BR2F'R")
        c <- c + 9
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==2 && x$co["UBR"]==0)      #29
      {
        x <- move(x, moves = "RUR'yR'FRU'R'F'Ry'")
        c <- c + 10
      }
    }
    else if (x$eo["UR"]==0 && x$eo["UF"]==1 && x$eo["UL"]==0 && x$eo["UB"]==1)            #2 EDGES ORIENTED INCORRECTLY HORIZONTAL "LINE"
    {
      if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==0)           #30
      {
        x <- move(x, moves = "R'U'RUFx'RU'R'Eyx")
        c <- c + 9
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==0)      #32
      {
        x <- move(x, moves = "RUR'U'R'FRF'")
        c <- c + 8
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==0)      #33
      {
        x <- move(x, moves = "FRUR'U'F'")
        c <- c + 6
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==2)      #35
      {
        x <- move(x, moves = "FURU'R'URU'R'F'")
        c <- c + 10
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==2)      #36
      {
        x <- move(x, moves = "L'B'LU'R'URU'R'URL'BL")
        c <- c + 14
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==0)      #50
      {
        x <- move(x, moves = "LF'L'U'LUFU'L'")
        c <- c + 9
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==1)      #51
      {
        x <- move(x, moves = "R'FRUR'U'F'UR")
        c <- c + 9
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==2)      #52
      {
        x <- move(x, moves = "LF'L'U'LFL'F'UF")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==1 && x$co["UBR"]==1)      #53
      {
        x <- move(x, moves = "R'FRUR'F'RFU'F'")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==2 && x$co["ULB"]==2 && x$co["UBR"]==2)      #54
      {
        x <- move(x, moves = "L'B'LR'U'RUL'BL")
        c <- c + 10
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==1 && x$co["UBR"]==1)      #55
      {
        x <- move(x, moves = "RBR'LUL'U'RB'R'")
        c <- c + 10
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==0)      #57
      {
        x <- move(x, moves = "RUR'U'LR'FRF'L'")
        c <- c + 10
      }
    }
    else if (x$eo["UR"]==1 && x$eo["UF"]==0 && x$eo["UL"]==1 && x$eo["UB"]==0)            #2 EDGES ORIENTED INCORRECTLY VERTICAL "LINE"
    {
      if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==2)           #31
      {
        x <- move(x, moves = "R'U'R'FRF'UR")
        c <- c + 8
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==2)      #34
      {
        x <- move(x, moves = "RU2R2U'RU'R'U2FRF'")
        c <- c + 11
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==2)      #37
      {
        x <- move(x, moves = "R'U'RU'R'UF'UFR")
        c <- c + 10
      }
    }
    else if (x$eo["UR"]==0 && x$eo["UF"]==0 && x$eo["UL"]==0 && x$eo["UB"]==0)            #4 EDGES ORIENTED CORRECTLY
    {
      if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==0 && x$co["UBR"]==0)           #0
      {
        x <- x
        c <- c
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==1 && x$co["ULB"]==2 && x$co["UBR"]==1)      #1
      {
        x <- move(x, moves = "RU2R'U'RUR'U'RU'R'")
        c <- c + 11
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==2 && x$co["ULB"]==1 && x$co["UBR"]==1)      #2
      {
        x <- move(x, moves = "(RU2R2U'R2U'R2U2R")
        c <- c + 9
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==0)      #3
      {
        x <- move(x, moves = "x'RU'R'DRUR'D'x")
        c <- c + 8
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==1)      #4
      {
        x <- move(x, moves = "R2D'RU2R'DRU2R")
        c <- c + 9
      }
      else if (x$co["URF"]==1 && x$co["UFL"]==2 && x$co["ULB"]==0 && x$co["UBR"]==0)      #5
      {
        x <- move(x, moves = "x'RUR'DRU'R'D'x")
        c <- c + 8
      }
      else if (x$co["URF"]==0 && x$co["UFL"]==1 && x$co["ULB"]==1 && x$co["UBR"]==1)      #6
      {
        x <- move(x, moves = "L'U'LU'L'U2L")
        c <- c + 7
      }
      else if (x$co["URF"]==2 && x$co["UFL"]==0 && x$co["ULB"]==2 && x$co["UBR"]==2)      #7
      {
        x <- move(x, moves = "RUR'URU2R'")
        c <- c + 7
      }
    }
    else
    {
      x <- x
      c <- c
    }
    
    
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  OLL <- function(x,c)
  {
    x <- move(x, moves = "z2")
    plot(x)
    
    for (i in 1:4)
    {
      c <- OLL_perms(x,c)$counter
      x <- OLL_perms(x,c)$cube
      
      x <- move(x, moves = "y")
    }
    
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  A_perm <- function(x,c)
  {
    x <- move(x, moves = "xR'UR'D2RU'R'D2R2x'")
    c <- c + 9
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  A_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "xLU'LD2L'ULD2L2x'")
    c <- c + 9
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  E_perm <- function(x,c)
  {
    x <- move(x, moves = "x'RU'R'DRUR'D'RUR'DRU'R'D'x")
    c <- c + 16
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  F_perm <- function(x,c)
  {
    x <- move(x, moves = "LUFL'U'LULF'L2ULUL'U'LU'L'")
    c <- c + 18
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  G1_perm <- function(x,c)
  {
    x <- move(x, moves = "R'U'RB2DL'ULU'LD'B2")
    c <- c + 12
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  G1_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "LUL'B2D'RU'R'UR'DB2")
    c <- c + 12
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  G2_perm <- function(x,c)
  {
    x <- move(x, moves = "z'U2RB'LB'L'BR'U2F'LFz")
    c <- c + 12
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  G2_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "zU2L'BR'BRB'LU2FR'F'z'")
    c <- c + 12
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  H_perm <- function(x,c)
  {
    x <- move(x, moves = "M2U'M2U2M2U'M2")
    c <- c + 7
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  J_perm <- function(x,c)
  {
    x <- move(x, moves = "RUR'F'RUR'U'R'FR2U'R'U'")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  J_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "L'U'LFL'U'LULF'L2ULU")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  N_perm <- function(x,c)
  {
    x <- move(x, moves = "R'UL'U2RU'LR'UL'U2RU'LU")
    c <- c + 15
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  N_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "LU'RU2L'UR'LU'RU2L'UR'U'")
    c <- c + 15
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  R_perm <- function(x,c)
  {
    x <- move(x, moves = "R'U2RU2R'FRUR'U'R'F'R2U'")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  R_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "LU2L'U2LF'L'U'LULFL2U")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  T_perm <- function(x,c)
  {
    x <- move(x, moves = "RUR'U'R'FR2U'R'U'RUR'F'")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  U_perm <- function(x,c)
  {
    x <- move(x, moves = "R'UR'U'R'U'R'URUR2")
    c <- c + 11
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  U_perm_mirror <- function(x,c)
  {
    x <- move(x, moves = "R2U'R'U'RURURU'R")
    c <- c + 11
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  V_perm <- function(x,c)
  {
    x <- move(x, moves = "L'UL'U'y'R'F'R2U'R'UR'FRFy")
    c <- c + 14
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  Y_perm <- function(x,c)
  {
    x <- move(x, moves = "FRU'R'U'RUR'F'RUR'U'R'FRF'")
    c <- c + 17
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  Z_perm <- function(x,c)
  {
    x <- move(x, moves = "UR'U'RU'RURU'R'URUR2U'R'U")
    c <- c + 17
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
  PLL_perms <- function(x,c,test_check)
  {
    test_check <- 0
    if (x$ep["UR"]==5 && x$ep["UF"]==6 && x$ep["UL"]==7 && x$ep["UB"]==8 && !(x$cp["URF"]==3 && x$cp["UFL"]==4 && x$cp["ULB"]==1 && x$cp["UBR"]==2))                 # edges correctly permuted
    {
      if (x$cp["URF"]==1 && x$cp["UFL"]==2 && x$cp["ULB"]==3 && x$cp["UBR"]==4)
      {
        x <- x
        c <- c
      }
      else if (x$cp["URF"]==3 && x$cp["UFL"]==2 && x$cp["ULB"]==4 && x$cp["UBR"]==1)
      {
        c <- A_perm(x,c)$counter
        x <- A_perm(x,c)$cube
      }
      else if (x$cp["URF"]==1 && x$cp["UFL"]==4 && x$cp["ULB"]==2 && x$cp["UBR"]==3)
      {
        c <- A_perm_mirror(x,c)$counter
        x <- A_perm_mirror(x,c)$cube
      }
      else if (x$cp["URF"]==4 && x$cp["UFL"]==3 && x$cp["ULB"]==2 && x$cp["UBR"]==1)
      {
        c <- E_perm(x,c)$counter
        x <- E_perm(x,c)$cube
      }
    }
    else if (x$cp["URF"]==1 && x$cp["UFL"]==2 && x$cp["ULB"]==3 && x$cp["UBR"]==4)        # corners correctly permuted             
    {
      if (x$ep["UR"]==7 && x$ep["UF"]==8 && x$ep["UL"]==5 && x$ep["UB"]==6)
      {
        c <- H_perm(x,c)$counter
        x <- H_perm(x,c)$cube
      }
      else if (x$ep["UR"]==7 && x$ep["UF"]==6 && x$ep["UL"]==8 && x$ep["UB"]==5)
      {
        c <- U_perm(x,c)$counter
        x <- U_perm(x,c)$cube
      }
      else if (x$ep["UR"]==8 && x$ep["UF"]==6 && x$ep["UL"]==5 && x$ep["UB"]==7)
      {
        c <- U_perm_mirror(x,c)$counter
        x <- U_perm_mirror(x,c)$cube
      }
      else if (x$ep["UR"]==6 && x$ep["UF"]==5 && x$ep["UL"]==8 && x$ep["UB"]==7)
      {
        c <- Z_perm(x,c)$counter
        x <- Z_perm(x,c)$cube
      }
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==8 && x$ep["UL"]==7 && x$ep["UB"]==6 && x$cp["URF"]==1 && x$cp["UFL"]==3 && x$cp["ULB"]==2 && x$cp["UBR"]==4)
    {
      c <- F_perm(x,c)$counter
      x <- F_perm(x,c)$cube
    }
    else if (x$ep["UR"]==6 && x$ep["UF"]==5 && x$ep["UL"]==7 && x$ep["UB"]==8 && x$cp["URF"]==4 && x$cp["UFL"]==2 && x$cp["ULB"]==3 && x$cp["UBR"]==1)
    {
      c <- J_perm(x,c)$counter
      x <- J_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==7 && x$ep["UL"]==6 && x$ep["UB"]==8 && x$cp["URF"]==1 && x$cp["UFL"]==3 && x$cp["ULB"]==2 && x$cp["UBR"]==4)
    {
      c <- J_perm_mirror(x,c)$counter
      x <- J_perm_mirror(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==8 && x$ep["UL"]==7 && x$ep["UB"]==6 && x$cp["URF"]==1 && x$cp["UFL"]==4 && x$cp["ULB"]==3 && x$cp["UBR"]==2)
    {
      c <- N_perm(x,c)$counter
      x <- N_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==8 && x$ep["UL"]==7 && x$ep["UB"]==6 && x$cp["URF"]==3 && x$cp["UFL"]==2 && x$cp["ULB"]==1 && x$cp["UBR"]==4)
    {
      c <- N_perm_mirror(x,c)$counter
      x <- N_perm_mirror(x,c)$cube
    }
    else if (x$ep["UR"]==6 && x$ep["UF"]==5 && x$ep["UL"]==7 && x$ep["UB"]==8 && x$cp["URF"]==1 && x$cp["UFL"]==2 && x$cp["ULB"]==4 && x$cp["UBR"]==3)
    {
      c <- R_perm(x,c)$counter
      x <- R_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==7 && x$ep["UL"]==6 && x$ep["UB"]==8 && x$cp["URF"]==1 && x$cp["UFL"]==2 && x$cp["ULB"]==4 && x$cp["UBR"]==3)
    {
      c <- R_perm_mirror(x,c)$counter
      x <- R_perm_mirror(x,c)$cube
    }
    else if (x$ep["UR"]==7 && x$ep["UF"]==6 && x$ep["UL"]==5 && x$ep["UB"]==8 && x$cp["URF"]==4 && x$cp["UFL"]==2 && x$cp["ULB"]==3 && x$cp["UBR"]==1)
    {
      c <- T_perm(x,c)$counter
      x <- T_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==7 && x$ep["UL"]==6 && x$ep["UB"]==8 && x$cp["URF"]==3 && x$cp["UFL"]==2 && x$cp["ULB"]==1 && x$cp["UBR"]==4)
    {
      c <- V_perm(x,c)$counter
      x <- V_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==6 && x$ep["UL"]==8 && x$ep["UB"]==7 && x$cp["URF"]==3 && x$cp["UFL"]==2 && x$cp["ULB"]==1 && x$cp["UBR"]==4)
    {
      c <- Y_perm(x,c)$counter
      x <- Y_perm(x,c)$cube
    }
    else if (x$ep["UR"]==5 && x$ep["UF"]==7 && x$ep["UL"]==8 && x$ep["UB"]==6 && x$cp["URF"]==3 && x$cp["UFL"]==1 && x$cp["ULB"]==2 && x$cp["UBR"]==4)
    {
      c <- G1_perm(x,c)$counter
      x <- G1_perm(x,c)$cube
    }
    else if (x$ep["UR"]==8 && x$ep["UF"]==5 && x$ep["UL"]==7 && x$ep["UB"]==6 && x$cp["URF"]==2 && x$cp["UFL"]==4 && x$cp["ULB"]==3 && x$cp["UBR"]==1)
    {
      c <- G1_perm_mirror(x,c)$counter
      x <- G1_perm_mirror(x,c)$cube
    }
    else if (x$ep["UR"]==8 && x$ep["UF"]==6 && x$ep["UL"]==5 && x$ep["UB"]==7 && x$cp["URF"]==1 && x$cp["UFL"]==3 && x$cp["ULB"]==4 && x$cp["UBR"]==2)
    {
      c <- G2_perm(x,c)$counter
      x <- G2_perm(x,c)$cube
    }
    else if (x$ep["UR"]==7 && x$ep["UF"]==6 && x$ep["UL"]==8 && x$ep["UB"]==5 && x$cp["URF"]==4 && x$cp["UFL"]==2 && x$cp["ULB"]==1 && x$cp["UBR"]==3)
    {
      c <- G2_perm_mirror(x,c)$counter
      x <- G2_perm_mirror(x,c)$cube
    }
    else
    {
      test_check <- 1
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c, "test_check"=test_check))
  }
  
  PLL_perms_repeated <- function(x,c,test_check)
  {
    for (i in 1:4)
    {
      test_check <- PLL_perms(x,c,test_check)$test_check
      c <- PLL_perms(x,c,test_check)$counter
      x <- PLL_perms(x,c,test_check)$cube
      
      x <- move(x, moves = "y")
      
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c, "test_check"=test_check))
  }
  
  
  PLL <- function(x,c,test_check = 0)
  {
    test_check <- PLL_perms_repeated(x,c,test_check)$test_check
    c <- PLL_perms_repeated(x,c,test_check)$counter
    x <- PLL_perms_repeated(x,c,test_check)$cube
    
    if (test_check == 1)
    {
      x <- move(x, moves = "U")
      test_check <- PLL_perms_repeated(x,c,test_check)$test_check
      c <- PLL_perms_repeated(x,c,test_check)$counter + 1
      x <- PLL_perms_repeated(x,c,test_check)$cube
    }
    if (test_check == 1)
    {
      x <- move(x, moves = "U")
      test_check <- PLL_perms_repeated(x,c,test_check)$test_check
      c <- PLL_perms_repeated(x,c,test_check)$counter
      x <- PLL_perms_repeated(x,c,test_check)$cube
    }
    if (test_check == 1)
    {
      x <- move(x, moves = "U")
      test_check <- PLL_perms_repeated(x,c,test_check)$test_check
      c <- PLL_perms_repeated(x,c,test_check)$counter
      x <- PLL_perms_repeated(x,c,test_check)$cube
    }
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  CFOP <- function(x,c)
  {
    c <- CROSS(x,c)$counter
    x <- CROSS(x,c)$cube
    
    c <- F2L(x,c)$counter
    x <- F2L(x,c)$cube
    
    c <- OLL(x,c)$counter
    x <- OLL(x,c)$cube
    
    c <- PLL(x,c,0)$counter
    x <- PLL(x,c,0)$cube
    
    plot(x)
    #Sys.sleep(0.1)
    return(list("cube"=x,"counter"=c))
  }
  
  
#####################################################################################
  
  
  check_solve <- function()
  {
    solved <- getCubieCube("Solved")
    for (i in 801:1000)
    {
      x <- getCubieCube("Solved")
      sc <- eval(parse(text=paste("scramble",i,sep = "")))
      x <- move(x, moves = sc)
      x <- solve_LBL(x,0)$cube
      if (!(x == solved))
      {
        print(i)
      }
    }
  }
  