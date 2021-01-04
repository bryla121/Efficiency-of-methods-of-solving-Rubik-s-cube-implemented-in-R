semi_opt_cross <- function(x,c)
{
  c_test1 <- 0
  c_test2 <- 0
  best_color <- "green"
  c_test1 <- WG_cross(x,c_test1)$counter
  c_test2 <- WO_cross(x,c_test2)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "orange"
  }
  c_test2 <- WB_cross(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "blue"
  }
  c_test2 <- WR_cross(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "red"
  }
  
  
  if (best_color == "green")                      #first green
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red"
    c_test1 <- WR_cross(x,c_test1)$counter
    c_test2 <- WO_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange"
    }
    c_test2 <- WB_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue"
    }
    
    if (best_color == "red")                      #second red
    {
      c <- WR_cross(x,c)$counter
      x <- WR_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange"
      c_test1 <- WO_cross(x,c_test1)$counter
      c_test2 <- WB_cross(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue"
      }
      if (best_color == "orange")                 #third orange and fourth blue
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
      else if (best_color == "blue")              #third blue and fourth orange
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
    }
    else if (best_color == "orange")              #second orange
    {
      c <- WO_cross(x,c)$counter
      x <- WO_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WB_cross(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue"
      }
      
      if (best_color == "red")                    #third red and fourth blue
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
      else if (best_color == "blue")              #third blue and fourth red
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
    else if (best_color == "blue")                #second blue
    {
      c <- WB_cross(x,c)$counter
      x <- WB_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WO_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange"
      }
      
      if (best_color == "red")                    #third red and fourth orange
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
      else if (best_color == "orange")            #third orange and fourth red
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
  }
  else if (best_color == "orange")                #first orange
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red"
    c_test1 <- WR_cross(x,c_test1)$counter
    c_test2 <- WG_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green"
    }
    c_test2 <- WB_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue"
    }
    
    if (best_color == "red")                      #second red
    {
      c <- WR_cross(x,c)$counter
      x <- WR_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green"
      c_test1 <- WG_cross(x,c_test1)$counter
      c_test2 <- WB_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue"
      }
      
      if (best_color == "green")                  #third green and fourt blue
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
      else if (best_color == "blue")              #third blue and fourt green
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
    }
    else if (best_color == "green")               #second green
    {
      c <- WG_cross(x,c)$counter
      x <- WG_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WB_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue"
      }
      
      if (best_color == "red")                    #third red and fourth blue
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
      else if (best_color == "blue")              #third blue and fourth red
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
    else if (best_color == "blue")                #second blue
    {
      c <- WB_cross(x,c)$counter
      x <- WB_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WG_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green"
      }
      
      if (best_color == "red")                    #third red and fourth green
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
      else if (best_color == "green")             #third green and fourth red
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
  }
  else if (best_color == "blue")                  #first blue
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red"
    c_test1 <- WR_cross(x,c_test1)$counter
    c_test2 <- WG_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green"
    }
    c_test2 <- WO_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange"
    }
    
    if (best_color == "red")                      #second red
    {
      c <- WR_cross(x,c)$counter
      x <- WR_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange"
      c_test1 <- WO_cross(x,c_test1)$counter
      c_test2 <- WG_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green"
      }
      
      if (best_color == "orange")                 #third orange and fourth green
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
      else if (best_color == "green")             #third green and fourth orange
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
    }
    else if (best_color == "orange")              #second orange
    {
      c <- WO_cross(x,c)$counter
      x <- WO_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WG_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green"
      }
      
      if (best_color == "red")                    #third red and fourth green
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
      else if (best_color == "green")             #third green and fourth red
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
    else if (best_color == "green")               #second green
    {
      c <- WG_cross(x,c)$counter
      x <- WG_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red"
      c_test1 <- WR_cross(x,c_test1)$counter
      c_test2 <- WO_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange"
      }
      
      if (best_color == "red")                  #third red and fourth orange
      {
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
      else if (best_color == "orange")          #third orange and fourth red
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WR_cross(x,c)$counter
        x <- WR_cross(x,c)$cube
      }
    }
  }
  else if (best_color == "red")                   #first red
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "blue"
    c_test1 <- WB_cross(x,c_test1)$counter
    c_test2 <- WG_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green"
    }
    c_test2 <- WO_cross(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange"
    }
    
    if (best_color == "blue")                     #second blue
    {
      c <- WB_cross(x,c)$counter
      x <- WB_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange"
      c_test1 <- WO_cross(x,c_test1)$counter
      c_test2 <- WG_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green"
      }
      
      if (best_color == "orange")                 #third orange and fourth green
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
      else if (best_color == "green")             #third green and fourth orange
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
    }
    else if (best_color == "orange")              #second orange
    {
      c <- WO_cross(x,c)$counter
      x <- WO_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue"
      c_test1 <- WB_cross(x,c_test1)$counter
      c_test2 <- WG_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green"
      }
      
      if (best_color == "blue")                   #third blue and fourth green
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
      }
      else if (best_color == "green")             #third green and fourth blue
      {
        c <- WG_cross(x,c)$counter
        x <- WG_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
    }
    else if (best_color == "green")               #second green
    {
      c <- WG_cross(x,c)$counter
      x <- WG_cross(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue"
      c_test1 <- WB_cross(x,c_test1)$counter
      c_test2 <- WO_cross(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange"
      }
      
      if (best_color == "blue")                   #third blue and fourth orange
      {
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
        
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
      }
      else if (best_color == "orange")            #third orange and fourth blue
      {
        c <- WO_cross(x,c)$counter
        x <- WO_cross(x,c)$cube
        
        c <- WB_cross(x,c)$counter
        x <- WB_cross(x,c)$cube
      }
    }
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


semi_opt_white_corners <- function(x,c)
{
  c_test1 <- 0
  c_test2 <- 0
  best_color <- "green_red"
  c_test1 <- URF_corner(x,c_test1)$counter
  c_test2 <- UFL_corner(x,c_test2)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "orange_green"
  }
  c_test2 <- ULB_corner(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "blue_orange"
  }
  c_test2 <- UBR_corner(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "red_blue"
  }
  
  
  if (best_color == "green_red")                          #first green_red
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_blue"
    c_test1 <- UBR_corner(x,c_test1)$counter
    c_test2 <- UFL_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    c_test2 <- ULB_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_orange"
    }
    
    if (best_color == "red_blue")                         #second red_blue
    {
      c <- UBR_corner(x,c)$counter
      x <- UBR_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_green"
      c_test1 <- UFL_corner(x,c_test1)$counter
      c_test2 <- ULB_corner(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      if (best_color == "orange_green")                   #third orange_green and fourth blue_orange
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third blue_orange and fourth orange_green
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second orange_green
    {
      c <- UFL_corner(x,c)$counter
      x <- UFL_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- ULB_corner(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth blue_orange
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third blue_orange and fourth red_blue
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
    else if (best_color == "blue_orange")                 #second blue_orange
    {
      c <- ULB_corner(x,c)$counter
      x <- ULB_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- UFL_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth orange_green
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
      else if (best_color == "orange_green")              #third orange_green and fourth red_blue
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
  }
  else if (best_color == "orange_green")                  #first orange_green
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_blue"
    c_test1 <- UBR_corner(x,c_test1)$counter
    c_test2 <- URF_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_red"
    }
    c_test2 <- ULB_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_orange"
    }
    
    if (best_color == "red_blue")                         #second red_blue
    {
      c <- UBR_corner(x,c)$counter
      x <- UBR_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- URF_corner(x,c_test1)$counter
      c_test2 <- ULB_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "green_red")                      #third green_red and fourt blue_orange
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third blue_orange and fourt green_red
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
    }
    else if (best_color == "green_red")                   #second green_red
    {
      c <- URF_corner(x,c)$counter
      x <- URF_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- ULB_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth blue_orange
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third blue_orange and fourth red_blue
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
    else if (best_color == "blue_orange")                 #second blue_orange
    {
      c <- ULB_corner(x,c)$counter
      x <- ULB_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- URF_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_red"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth green_red
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
      else if (best_color == "green_red")                 #third green_red and fourth red_blue
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
  }
  else if (best_color == "blue_orange")                   #first blue_orange
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_blue"
    c_test1 <- UBR_corner(x,c_test1)$counter
    c_test2 <- URF_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_red"
    }
    c_test2 <- UFL_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    
    if (best_color == "red_blue")                         #second red_blue
    {
      c <- UBR_corner(x,c)$counter
      x <- UBR_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_green"
      c_test1 <- UFL_corner(x,c_test1)$counter
      c_test2 <- URF_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_red"
      }
      
      if (best_color == "orange_green")                   #third orange_green and fourth green_red
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
      else if (best_color == "green_red")                 #third green_red and fourth orange_green
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second orange_green
    {
      c <- UFL_corner(x,c)$counter
      x <- UFL_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- URF_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_red"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth green_red
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
      else if (best_color == "green_red")                 #third green_red and fourth red_blue
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
    else if (best_color == "green_red")                   #second green_red
    {
      c <- URF_corner(x,c)$counter
      x <- URF_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- UBR_corner(x,c_test1)$counter
      c_test2 <- UFL_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "red_blue")                       #third red_blue and fourth orange_green
      {
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
      else if (best_color == "orange_green")              #third orange_green and fourth red_blue
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- UBR_corner(x,c)$counter
        x <- UBR_corner(x,c)$cube
      }
    }
  }
  else if (best_color == "red_blue")                      #first red_blue
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "blue_orange"
    c_test1 <- ULB_corner(x,c_test1)$counter
    c_test2 <- URF_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_red"
    }
    c_test2 <- UFL_corner(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    
    if (best_color == "blue_orange")                      #second blue_orange
    {
      c <- ULB_corner(x,c)$counter
      x <- ULB_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_green"
      c_test1 <- UFL_corner(x,c_test1)$counter
      c_test2 <- URF_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_red"
      }
      
      if (best_color == "orange_green")                   #third orange_green and fourth green_red
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
      else if (best_color == "green_red")                 #third green_red and fourth orange_green
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second orange_green
    {
      c <- UFL_corner(x,c)$counter
      x <- UFL_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_orange"
      c_test1 <- ULB_corner(x,c_test1)$counter
      c_test2 <- URF_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_red"
      }
      
      if (best_color == "blue_orange")                    #third blue_orange and fourth green_red
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
      }
      else if (best_color == "green_red")                 #third green_red and fourth blue_orange
      {
        c <- URF_corner(x,c)$counter
        x <- URF_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
    }
    else if (best_color == "green_red")                   #second green_red
    {
      c <- URF_corner(x,c)$counter
      x <- URF_corner(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_orange"
      c_test1 <- ULB_corner(x,c_test1)$counter
      c_test2 <- UFL_corner(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "blue_orange")                    #third blue_orange and fourth orange_green
      {
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
        
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
      }
      else if (best_color == "orange_green")              #third orange_green and fourth blue_orange
      {
        c <- UFL_corner(x,c)$counter
        x <- UFL_corner(x,c)$cube
        
        c <- ULB_corner(x,c)$counter
        x <- ULB_corner(x,c)$cube
      }
    }
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


semi_opt_second_layer <- function(x,c)
{
  c_test1 <- 0
  c_test2 <- 0
  best_color <- "green_orange"
  c_test1 <- GREEN_ORANGE_EDGE(x,c_test1)$counter
  c_test2 <- ORANGE_BLUE_EDGE(x,c_test2)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "orange_blue"
  }
  c_test2 <- BLUE_RED_EDGE(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "blue_red"
  }
  c_test2 <- RED_GREEN_EDGE(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "red_green"
  }
  
  
  if (best_color == "green_orange")                       #first green_orange
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_green"
    c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
    c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_blue"
    }
    c_test2 <- BLUE_RED_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_red"
    }
    
    if (best_color == "red_green")                        #second red_green
    {
      c <- RED_GREEN_EDGE(x,c)$counter
      x <- RED_GREEN_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_blue"
      c_test1 <- ORANGE_BLUE_EDGE(x,c_test1)$counter
      c_test2 <- BLUE_RED_EDGE(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_red"
      }
      if (best_color == "orange_blue")                    #third orange_blue and fourth blue_red
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
      else if (best_color == "blue_red")                  #third blue_red and fourth orange_blue
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
    }
    else if (best_color == "orange_blue")                 #second orange_blue
    {
      c <- ORANGE_BLUE_EDGE(x,c)$counter
      x <- ORANGE_BLUE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- BLUE_RED_EDGE(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_red"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth blue_red
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
      else if (best_color == "blue_red")                  #third blue_red and fourth red_green
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
    else if (best_color == "blue_red")                    #second blue_red
    {
      c <- BLUE_RED_EDGE(x,c)$counter
      x <- BLUE_RED_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_blue"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth orange_blue
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
      else if (best_color == "orange_blue")               #third orange_blue and fourth red_green
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
  }
  else if (best_color == "orange_blue")                   #first orange_blue
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_green"
    c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
    c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_orange"
    }
    c_test2 <- BLUE_RED_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_red"
    }
    
    if (best_color == "red_green")                        #second red_green
    {
      c <- RED_GREEN_EDGE(x,c)$counter
      x <- RED_GREEN_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_orange"
      c_test1 <- GREEN_ORANGE_EDGE(x,c_test1)$counter
      c_test2 <- BLUE_RED_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_red"
      }
      
      if (best_color == "green_orange")                   #third green_orange and fourth blue_red
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
      else if (best_color == "blue_red")                  #third blue_red and fourt green_orange
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
    }
    else if (best_color == "green_orange")                #second green_orange
    {
      c <- GREEN_ORANGE_EDGE(x,c)$counter
      x <- GREEN_ORANGE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- BLUE_RED_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_red"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth blue_red
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
      else if (best_color == "blue_red")                  #third blue_red and fourth red_green
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
    else if (best_color == "blue_red")                    #second blue_red
    {
      c <- BLUE_RED_EDGE(x,c)$counter
      x <- BLUE_RED_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_orange"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth green_orange
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
      else if (best_color == "green_orange")              #third green_orange and fourth red_green
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
  }
  else if (best_color == "blue_red")                      #first blue_red
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_green"
    c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
    c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_orange"
    }
    c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_blue"
    }
    
    if (best_color == "red_green")                        #second red_green
    {
      c <- RED_GREEN_EDGE(x,c)$counter
      x <- RED_GREEN_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_blue"
      c_test1 <- ORANGE_BLUE_EDGE(x,c_test1)$counter
      c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_orange"
      }
      
      if (best_color == "orange_blue")                    #third orange_blue and fourth green_orange
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
      else if (best_color == "green_orange")              #third green_orange and fourth orange_blue
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
    }
    else if (best_color == "orange_blue")                 #second orange_blue
    {
      c <- ORANGE_BLUE_EDGE(x,c)$counter
      x <- ORANGE_BLUE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_orange"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth green_orange
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
      else if (best_color == "green_orange")              #third green_orange and fourth red_green
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
    else if (best_color == "green_orange")                #second green_orange
    {
      c <- GREEN_ORANGE_EDGE(x,c)$counter
      x <- GREEN_ORANGE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_green"
      c_test1 <- RED_GREEN_EDGE(x,c_test1)$counter
      c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_blue"
      }
      
      if (best_color == "red_green")                      #third red_green and fourth orange_blue
      {
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
      else if (best_color == "orange_blue")               #third orange_blue and fourth red_green
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- RED_GREEN_EDGE(x,c)$counter
        x <- RED_GREEN_EDGE(x,c)$cube
      }
    }
  }
  else if (best_color == "red_green")                     #first red_green
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "blue_red"
    c_test1 <- BLUE_RED_EDGE(x,c_test1)$counter
    c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "green_orange"
    }
    c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_blue"
    }
    
    if (best_color == "blue_red")                         #second blue_red
    {
      c <- BLUE_RED_EDGE(x,c)$counter
      x <- BLUE_RED_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_blue"
      c_test1 <- ORANGE_BLUE_EDGE(x,c_test1)$counter
      c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_orange"
      }
      
      if (best_color == "orange_blue")                    #third orange_blue and fourth green_orange
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
      else if (best_color == "green_orange")              #third green_orange and fourth orange_blue
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
    }
    else if (best_color == "orange_blue")                 #second orange_blue
    {
      c <- ORANGE_BLUE_EDGE(x,c)$counter
      x <- ORANGE_BLUE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_red"
      c_test1 <- BLUE_RED_EDGE(x,c_test1)$counter
      c_test2 <- GREEN_ORANGE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "green_orange"
      }
      
      if (best_color == "blue_red")                       #third blue_red and fourth green_orange
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
      }
      else if (best_color == "green_orange")              #third green_orange and fourth blue_red
      {
        c <- GREEN_ORANGE_EDGE(x,c)$counter
        x <- GREEN_ORANGE_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
    }
    else if (best_color == "green_orange")                #second green_orange
    {
      c <- GREEN_ORANGE_EDGE(x,c)$counter
      x <- GREEN_ORANGE_EDGE(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_red"
      c_test1 <- BLUE_RED_EDGE(x,c_test1)$counter
      c_test2 <- ORANGE_BLUE_EDGE(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_blue"
      }
      
      if (best_color == "blue_red")                       #third blue_red and fourth orange_blue
      {
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
        
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
      }
      else if (best_color == "orange_blue")               #third orange_blue and fourth blue_red
      {
        c <- ORANGE_BLUE_EDGE(x,c)$counter
        x <- ORANGE_BLUE_EDGE(x,c)$cube
        
        c <- BLUE_RED_EDGE(x,c)$counter
        x <- BLUE_RED_EDGE(x,c)$cube
      }
    }
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


semi_opt_F2L <- function(x,c)
{
  c_test1 <- 0
  c_test2 <- 0
  best_color <- "orange_green"
  c_test1 <- orange_green_block(x,c_test1)$counter
  c_test2 <- blue_orange_block(x,c_test2)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "blue_orange"
  }
  c_test2 <- red_blue_block(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "red_blue"
  }
  c_test2 <- green_red_block(x,0)$counter
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_color <- "green_red"
  }
  
  
  if (best_color == "orange_green")                       #first green_orange
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "green_red"
    c_test1 <- green_red_block(x,c_test1)$counter
    c_test2 <- blue_orange_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_orange"
    }
    c_test2 <- red_blue_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "red_blue"
    }
    
    if (best_color == "green_red")                        #second red_green
    {
      c <- green_red_block(x,c)$counter
      x <- green_red_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_orange"
      c_test1 <- blue_orange_block(x,c_test1)$counter
      c_test2 <- red_blue_block(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "red_blue"
      }
      if (best_color == "blue_orange")                    #third orange_blue and fourth blue_red
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
      else if (best_color == "red_blue")                  #third blue_red and fourth orange_blue
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
    }
    else if (best_color == "blue_orange")                 #second orange_blue
    {
      c <- blue_orange_block(x,c)$counter
      x <- blue_orange_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- red_blue_block(x,0)$counter
      
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "red_blue"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth blue_red
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
      else if (best_color == "red_blue")                  #third blue_red and fourth red_green
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
    else if (best_color == "red_blue")                    #second blue_red
    {
      c <- red_blue_block(x,c)$counter
      x <- red_blue_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- blue_orange_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth orange_blue
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third orange_blue and fourth red_green
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
  }
  else if (best_color == "blue_orange")                   #first orange_blue
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "green_red"
    c_test1 <- green_red_block(x,c_test1)$counter
    c_test2 <- orange_green_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    c_test2 <- red_blue_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "red_blue"
    }
    
    if (best_color == "green_red")                        #second red_green
    {
      c <- green_red_block(x,c)$counter
      x <- green_red_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "orange_green"
      c_test1 <- orange_green_block(x,c_test1)$counter
      c_test2 <- red_blue_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "red_blue"
      }
      
      if (best_color == "orange_green")                   #third green_orange and fourth blue_red
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
      else if (best_color == "red_blue")                  #third blue_red and fourt green_orange
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second green_orange
    {
      c <- orange_green_block(x,c)$counter
      x <- orange_green_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- red_blue_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "red_blue"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth blue_red
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
      else if (best_color == "red_blue")                  #third blue_red and fourth red_green
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
    else if (best_color == "red_blue")                    #second blue_red
    {
      c <- red_blue_block(x,c)$counter
      x <- red_blue_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- orange_green_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth green_orange
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
      else if (best_color == "orange_green")              #third green_orange and fourth red_green
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
  }
  else if (best_color == "red_blue")                      #first blue_red
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "green_red"
    c_test1 <- green_red_block(x,c_test1)$counter
    c_test2 <- orange_green_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    c_test2 <- blue_orange_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_orange"
    }
    
    if (best_color == "green_red")                        #second red_green
    {
      c <- green_red_block(x,c)$counter
      x <- green_red_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_orange"
      c_test1 <- blue_orange_block(x,c_test1)$counter
      c_test2 <- orange_green_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "blue_orange")                    #third orange_blue and fourth green_orange
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
      else if (best_color == "orange_green")              #third green_orange and fourth orange_blue
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
    }
    else if (best_color == "blue_orange")                 #second orange_blue
    {
      c <- blue_orange_block(x,c)$counter
      x <- blue_orange_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- orange_green_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth green_orange
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
      else if (best_color == "orange_green")              #third green_orange and fourth red_green
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second green_orange
    {
      c <- orange_green_block(x,c)$counter
      x <- orange_green_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "green_red"
      c_test1 <- green_red_block(x,c_test1)$counter
      c_test2 <- blue_orange_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "green_red")                      #third red_green and fourth orange_blue
      {
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third orange_blue and fourth red_green
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- green_red_block(x,c)$counter
        x <- green_red_block(x,c)$cube
      }
    }
  }
  else if (best_color == "green_red")                     #first red_green
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    
    c_test1 <- 0
    c_test2 <- 0
    best_color <- "red_blue"
    c_test1 <- red_blue_block(x,c_test1)$counter
    c_test2 <- orange_green_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "orange_green"
    }
    c_test2 <- blue_orange_block(x,0)$counter
    if (c_test2 < c_test1)
    {
      c_test1 <- c_test2
      best_color <- "blue_orange"
    }
    
    if (best_color == "red_blue")                         #second blue_red
    {
      c <- red_blue_block(x,c)$counter
      x <- red_blue_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "blue_orange"
      c_test1 <- blue_orange_block(x,c_test1)$counter
      c_test2 <- orange_green_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "blue_orange")                    #third orange_blue and fourth green_orange
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
      else if (best_color == "orange_green")              #third green_orange and fourth orange_blue
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
    }
    else if (best_color == "blue_orange")                 #second orange_blue
    {
      c <- blue_orange_block(x,c)$counter
      x <- blue_orange_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- red_blue_block(x,c_test1)$counter
      c_test2 <- orange_green_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "orange_green"
      }
      
      if (best_color == "red_blue")                       #third blue_red and fourth green_orange
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
      }
      else if (best_color == "orange_green")              #third green_orange and fourth blue_red
      {
        c <- orange_green_block(x,c)$counter
        x <- orange_green_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
    }
    else if (best_color == "orange_green")                #second green_orange
    {
      c <- orange_green_block(x,c)$counter
      x <- orange_green_block(x,c)$cube
      
      c_test1 <- 0
      c_test2 <- 0
      best_color <- "red_blue"
      c_test1 <- red_blue_block(x,c_test1)$counter
      c_test2 <- blue_orange_block(x,0)$counter
      if (c_test2 < c_test1)
      {
        c_test1 <- c_test2
        best_color <- "blue_orange"
      }
      
      if (best_color == "red_blue")                       #third blue_red and fourth orange_blue
      {
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
        
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
      }
      else if (best_color == "blue_orange")               #third orange_blue and fourth blue_red
      {
        c <- blue_orange_block(x,c)$counter
        x <- blue_orange_block(x,c)$cube
        
        c <- red_blue_block(x,c)$counter
        x <- red_blue_block(x,c)$cube
      }
    }
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


semi_opt_LBL <- function(x,c)
{
  c <- semi_opt_cross(x,c)$counter
  x <- semi_opt_cross(x,c)$cube
  
  c <- semi_opt_white_corners(x,c)$counter
  x <- semi_opt_white_corners(x,c)$cube
  
  c <- semi_opt_second_layer(x,c)$counter
  x <- semi_opt_second_layer(x,c)$cube
  
  c <- yellow_face(x,c)$counter
  x <- yellow_face(x,c)$cube
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


semi_opt_CFOP <- function(x,c)
{
  c <- semi_opt_cross(x,c)$counter
  x <- semi_opt_cross(x,c)$cube
  
  c <- semi_opt_F2L(x,c)$counter
  x <- semi_opt_F2L(x,c)$cube
  
  c <- OLL(x,c)$counter
  x <- OLL(x,c)$cube
  
  c <- PLL(x,c,0)$counter
  x <- PLL(x,c,0)$cube
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}

################################################################## fully optimized


fully_opt_cross <- function(x,c)
{
  x_copy <- x
  c_test1 <- 0
  c_test2 <- 0
  best_case <- "case1"
  
  ### case1
  c_test1 <-WR_cross(x_copy,c_test1)$counter
  x_copy <- WR_cross(x_copy,c_test1)$cube
  c_test1 <-WO_cross(x_copy,c_test1)$counter
  x_copy <- WO_cross(x_copy,c_test1)$cube
  c_test1 <-WG_cross(x_copy,c_test1)$counter
  x_copy <- WG_cross(x_copy,c_test1)$cube
  c_test1 <-WB_cross(x_copy,c_test1)$counter
  x_copy <- WB_cross(x_copy,c_test1)$cube
  
  ### case2
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case2"
  }
  
  ### case3
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case3"
  }
  
  ### case4
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case4"
  }
  
  ### case5
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case5"
  }
  
  ### case6
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case6"
  }
  
  ### case7
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case7"
  }
  
  ### case8
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case8"
  }
  
  ### case9
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case9"
  }
  
  ### case10
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case10"
  }
  
  ### case11
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case11"
  }
  
  ### case12
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case12"
  }
  
  ### case13
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case13"
  }
  
  ### case14
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case14"
  }
  
  ### case15
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case15"
  }
  
  ### case16
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case16"
  }
  
  ### case17
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case17"
  }
  
  ### case18
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case18"
  }
  
  ### case19
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case19"
  }
  
  ### case20
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case20"
  }
  
  ### case21
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case21"
  }
  
  ### case22
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case22"
  }
  
  ### case23
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case23"
  }
  
  ### case24
  x_copy <- x
  c_test2 <- 0
  c_test2 <-WB_cross(x_copy,c_test2)$counter
  x_copy <- WB_cross(x_copy,c_test2)$cube
  c_test2 <-WG_cross(x_copy,c_test2)$counter
  x_copy <- WG_cross(x_copy,c_test2)$cube
  c_test2 <-WO_cross(x_copy,c_test2)$counter
  x_copy <- WO_cross(x_copy,c_test2)$cube
  c_test2 <-WR_cross(x_copy,c_test2)$counter
  x_copy <- WR_cross(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case24"
  }
  
  
  if (best_case == "case1")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case2")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case3")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case4")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case5")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case6")
  {
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case7")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case8")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case9")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case10")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  else if (best_case == "case11")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case12")
  {
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  else if (best_case == "case13")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case14")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case15")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
  }
  else if (best_case == "case16")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  else if (best_case == "case17")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case18")
  {
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  else if (best_case == "case19")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case20")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case21")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
  }
  else if (best_case == "case22")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  else if (best_case == "case23")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
  }
  else if (best_case == "case24")
  {
    c <- WB_cross(x,c)$counter
    x <- WB_cross(x,c)$cube
    c <- WG_cross(x,c)$counter
    x <- WG_cross(x,c)$cube
    c <- WO_cross(x,c)$counter
    x <- WO_cross(x,c)$cube
    c <- WR_cross(x,c)$counter
    x <- WR_cross(x,c)$cube
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


fully_opt_white_corners <- function(x,c)
{
  x_copy <- x
  c_test1 <- 0
  c_test2 <- 0
  best_case <- "case1"
  
  ### case1
  c_test1 <-URF_corner(x_copy,c_test1)$counter
  x_copy <- URF_corner(x_copy,c_test1)$cube
  c_test1 <-UFL_corner(x_copy,c_test1)$counter
  x_copy <- UFL_corner(x_copy,c_test1)$cube
  c_test1 <-ULB_corner(x_copy,c_test1)$counter
  x_copy <- ULB_corner(x_copy,c_test1)$cube
  c_test1 <-UBR_corner(x_copy,c_test1)$counter
  x_copy <- UBR_corner(x_copy,c_test1)$cube
  
  ### case2
  x_copy <- x
  c_test2 <- 0
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case2"
  }
  
  ### case3
  x_copy <- x
  c_test2 <- 0
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case3"
  }
  
  ### case4
  x_copy <- x
  c_test2 <- 0
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case4"
  }
  
  ### case5
  x_copy <- x
  c_test2 <- 0
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case5"
  }
  
  ### case6
  x_copy <- x
  c_test2 <- 0
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case6"
  }
  
  ### case7
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case7"
  }
  
  ### case8
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case8"
  }
  
  ### case9
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case9"
  }
  
  ### case10
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case10"
  }
  
  ### case11
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case11"
  }
  
  ### case12
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case12"
  }
  
  ### case13
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case13"
  }
  
  ### case14
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case14"
  }
  
  ### case15
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case15"
  }
  
  ### case16
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case16"
  }
  
  ### case17
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case17"
  }
  
  ### case18
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case18"
  }
  
  ### case19
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case19"
  }
  
  ### case20
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case20"
  }
  
  ### case21
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case21"
  }
  
  ### case22
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case22"
  }
  
  ### case23
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case23"
  }
  
  ### case24
  x_copy <- x
  c_test2 <- 0
  c_test2 <-UBR_corner(x_copy,c_test2)$counter
  x_copy <- UBR_corner(x_copy,c_test2)$cube
  c_test2 <-ULB_corner(x_copy,c_test2)$counter
  x_copy <- ULB_corner(x_copy,c_test2)$cube
  c_test2 <-UFL_corner(x_copy,c_test2)$counter
  x_copy <- UFL_corner(x_copy,c_test2)$cube
  c_test2 <-URF_corner(x_copy,c_test2)$counter
  x_copy <- URF_corner(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case24"
  }
  
  
  if (best_case == "case1")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case2")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case3")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case4")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case5")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case6")
  {
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case7")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case8")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case9")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case10")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  else if (best_case == "case11")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case12")
  {
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  else if (best_case == "case13")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case14")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case15")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
  }
  else if (best_case == "case16")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  else if (best_case == "case17")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case18")
  {
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  else if (best_case == "case19")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case20")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case21")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
  }
  else if (best_case == "case22")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  else if (best_case == "case23")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
  }
  else if (best_case == "case24")
  {
    c <- UBR_corner(x,c)$counter
    x <- UBR_corner(x,c)$cube
    c <- ULB_corner(x,c)$counter
    x <- ULB_corner(x,c)$cube
    c <- UFL_corner(x,c)$counter
    x <- UFL_corner(x,c)$cube
    c <- URF_corner(x,c)$counter
    x <- URF_corner(x,c)$cube
  }
  
  
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


fully_opt_second_layer <- function(x,c)
{
  x_copy <- x
  c_test1 <- 0
  c_test2 <- 0
  best_case <- "case1"
  
  ### case1
  c_test1 <-RED_GREEN_EDGE(x_copy,c_test1)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test1)$cube
  c_test1 <-GREEN_ORANGE_EDGE(x_copy,c_test1)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test1)$cube
  c_test1 <-ORANGE_BLUE_EDGE(x_copy,c_test1)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test1)$cube
  c_test1 <-BLUE_RED_EDGE(x_copy,c_test1)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test1)$cube
  
  ### case2
  x_copy <- x
  c_test2 <- 0
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case2"
  }
  
  ### case3
  x_copy <- x
  c_test2 <- 0
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case3"
  }
  
  ### case4
  x_copy <- x
  c_test2 <- 0
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case4"
  }
  
  ### case5
  x_copy <- x
  c_test2 <- 0
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case5"
  }
  
  ### case6
  x_copy <- x
  c_test2 <- 0
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case6"
  }
  
  ### case7
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case7"
  }
  
  ### case8
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case8"
  }
  
  ### case9
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case9"
  }
  
  ### case10
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case10"
  }
  
  ### case11
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case11"
  }
  
  ### case12
  x_copy <- x
  c_test2 <- 0
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case12"
  }
  
  ### case13
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case13"
  }
  
  ### case14
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case14"
  }
  
  ### case15
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case15"
  }
  
  ### case16
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case16"
  }
  
  ### case17
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case17"
  }
  
  ### case18
  x_copy <- x
  c_test2 <- 0
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case18"
  }
  
  ### case19
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case19"
  }
  
  ### case20
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case20"
  }
  
  ### case21
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case21"
  }
  
  ### case22
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case22"
  }
  
  ### case23
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case23"
  }
  
  ### case24
  x_copy <- x
  c_test2 <- 0
  c_test2 <-BLUE_RED_EDGE(x_copy,c_test2)$counter
  x_copy <- BLUE_RED_EDGE(x_copy,c_test2)$cube
  c_test2 <-ORANGE_BLUE_EDGE(x_copy,c_test2)$counter
  x_copy <- ORANGE_BLUE_EDGE(x_copy,c_test2)$cube
  c_test2 <-GREEN_ORANGE_EDGE(x_copy,c_test2)$counter
  x_copy <- GREEN_ORANGE_EDGE(x_copy,c_test2)$cube
  c_test2 <-RED_GREEN_EDGE(x_copy,c_test2)$counter
  x_copy <- RED_GREEN_EDGE(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case24"
  }
  
  
  if (best_case == "case1")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case2")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case3")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case4")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case5")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case6")
  {
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case7")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case8")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case9")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case10")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  else if (best_case == "case11")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case12")
  {
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  else if (best_case == "case13")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case14")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case15")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
  }
  else if (best_case == "case16")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  else if (best_case == "case17")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case18")
  {
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  else if (best_case == "case19")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case20")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case21")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
  }
  else if (best_case == "case22")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  else if (best_case == "case23")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
  }
  else if (best_case == "case24")
  {
    c <- BLUE_RED_EDGE(x,c)$counter
    x <- BLUE_RED_EDGE(x,c)$cube
    c <- ORANGE_BLUE_EDGE(x,c)$counter
    x <- ORANGE_BLUE_EDGE(x,c)$cube
    c <- GREEN_ORANGE_EDGE(x,c)$counter
    x <- GREEN_ORANGE_EDGE(x,c)$cube
    c <- RED_GREEN_EDGE(x,c)$counter
    x <- RED_GREEN_EDGE(x,c)$cube
  }
  
  #print(best_case)
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


fully_opt_F2L <- function(x,c)
{
  x_copy <- x
  c_test1 <- 0
  c_test2 <- 0
  best_case <- "case1"
  
  ### case1
  c_test1 <-green_red_block(x_copy,c_test1)$counter
  x_copy <- green_red_block(x_copy,c_test1)$cube
  c_test1 <-red_blue_block(x_copy,c_test1)$counter
  x_copy <- red_blue_block(x_copy,c_test1)$cube
  c_test1 <-blue_orange_block(x_copy,c_test1)$counter
  x_copy <- blue_orange_block(x_copy,c_test1)$cube
  c_test1 <-orange_green_block(x_copy,c_test1)$counter
  x_copy <- orange_green_block(x_copy,c_test1)$cube
  
  ### case2
  x_copy <- x
  c_test2 <- 0
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case2"
  }
  
  ### case3
  x_copy <- x
  c_test2 <- 0
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case3"
  }
  
  ### case4
  x_copy <- x
  c_test2 <- 0
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case4"
  }
  
  ### case5
  x_copy <- x
  c_test2 <- 0
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case5"
  }
  
  ### case6
  x_copy <- x
  c_test2 <- 0
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case6"
  }
  
  ### case7
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case7"
  }
  
  ### case8
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case8"
  }
  
  ### case9
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case9"
  }
  
  ### case10
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case10"
  }
  
  ### case11
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case11"
  }
  
  ### case12
  x_copy <- x
  c_test2 <- 0
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case12"
  }
  
  ### case13
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case13"
  }
  
  ### case14
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case14"
  }
  
  ### case15
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case15"
  }
  
  ### case16
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case16"
  }
  
  ### case17
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case17"
  }
  
  ### case18
  x_copy <- x
  c_test2 <- 0
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case18"
  }
  
  ### case19
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case19"
  }
  
  ### case20
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case20"
  }
  
  ### case21
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case21"
  }
  
  ### case22
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case22"
  }
  
  ### case23
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case23"
  }
  
  ### case24
  x_copy <- x
  c_test2 <- 0
  c_test2 <-orange_green_block(x_copy,c_test2)$counter
  x_copy <- orange_green_block(x_copy,c_test2)$cube
  c_test2 <-blue_orange_block(x_copy,c_test2)$counter
  x_copy <- blue_orange_block(x_copy,c_test2)$cube
  c_test2 <-red_blue_block(x_copy,c_test2)$counter
  x_copy <- red_blue_block(x_copy,c_test2)$cube
  c_test2 <-green_red_block(x_copy,c_test2)$counter
  x_copy <- green_red_block(x_copy,c_test2)$cube
  
  if (c_test2 < c_test1)
  {
    c_test1 <- c_test2
    best_case <- "case24"
  }
  
  
  if (best_case == "case1")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case2")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case3")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case4")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case5")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case6")
  {
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case7")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case8")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case9")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case10")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  else if (best_case == "case11")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case12")
  {
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  else if (best_case == "case13")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case14")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case15")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
  }
  else if (best_case == "case16")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  else if (best_case == "case17")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case18")
  {
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  else if (best_case == "case19")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case20")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case21")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
  }
  else if (best_case == "case22")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  else if (best_case == "case23")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
  }
  else if (best_case == "case24")
  {
    c <- orange_green_block(x,c)$counter
    x <- orange_green_block(x,c)$cube
    c <- blue_orange_block(x,c)$counter
    x <- blue_orange_block(x,c)$cube
    c <- red_blue_block(x,c)$counter
    x <- red_blue_block(x,c)$cube
    c <- green_red_block(x,c)$counter
    x <- green_red_block(x,c)$cube
  }
  
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}



fully_opt_LBL <- function(x,c)
{
  c <- fully_opt_cross(x,c)$counter
  x <- fully_opt_cross(x,c)$cube
  
  c <- fully_opt_white_corners(x,c)$counter
  x <- fully_opt_white_corners(x,c)$cube
  
  c <- fully_opt_second_layer(x,c)$counter
  x <- fully_opt_second_layer(x,c)$cube
  
  c <- yellow_face(x,c)$counter
  x <- yellow_face(x,c)$cube
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}


fully_opt_CFOP <- function(x,c)
{
  c <- fully_opt_cross(x,c)$counter
  x <- fully_opt_cross(x,c)$cube
  
  c <- fully_opt_F2L(x,c)$counter
  x <- fully_opt_F2L(x,c)$cube
  
  c <- OLL(x,c)$counter
  x <- OLL(x,c)$cube
  
  c <- PLL(x,c,0)$counter
  x <- PLL(x,c,0)$cube
  
  plot(x)
  #Sys.sleep(0.1)
  return(list("cube"=x,"counter"=c))
}

