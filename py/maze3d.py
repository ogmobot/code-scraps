import random

def make_raw_maze(length, breadth, depth, start=(0.5,0.5,0.5)):
  """Creates a maze with the given number of cells in each dimension.
  "start" should be of the form (x,y,z), where 0<=x,y,z<=1, showing how far along each side the starting cell should be.
  3 = BORDER
  2 = UNVISITED
  1 = WALL
  0 = VISITED"""
  #make maze full of walls and unvisited cells (this works, don't touch)
  maze = [[[1+((i%2)*(j%2)*(k%2)) for i in range(2*length+1)] for j in range(2*breadth+1)] for k in range(2*depth+1)]

  #make top and bottom borders
  for i in range(len(maze[0])):
    for j in range(len(maze[0][0])):
      maze[0][i][j]=3
      maze[-1][i][j]=3
  #make front and back borders
  for i in range(len(maze)):
    for j in range(len(maze[0][0])):
      maze[i][0][j]=3
      maze[i][-1][j]=3
  #make right and left borders
  for i in range(len(maze)):
    for j in range(len(maze[0])):
      maze[i][j][0]=3
      maze[i][j][-1]=3

  wall_list=[]
  finished=0
  
  #mark start cell as visited
  start_depth=2*int(start[2]*(depth-1))+1
  start_breadth=2*int(start[1]*(breadth-1))+1
  start_length=2*int(start[0]*(length-1))+1
  maze[start_depth][start_breadth][start_length] = 0

  while finished==0:
    #append the six walls of start cell to wall list
    #top
    if maze[start_depth+1][start_breadth][start_length]==1:
      wall_list.append((start_depth+1,start_breadth,start_length))
    #bottom
    if maze[start_depth-1][start_breadth][start_length]==1:
      wall_list.append((start_depth-1,start_breadth,start_length))
    #front
    if maze[start_depth][start_breadth+1][start_length]==1:
      wall_list.append((start_depth,start_breadth+1,start_length))
    #back
    if maze[start_depth][start_breadth-1][start_length]==1:
      wall_list.append((start_depth,start_breadth-1,start_length))
    #right
    if maze[start_depth][start_breadth][start_length+1]==1:
      wall_list.append((start_depth,start_breadth,start_length+1))
    #left
    if maze[start_depth][start_breadth][start_length-1]==1:
      wall_list.append((start_depth,start_breadth,start_length-1))

    #randomly select wall to check
    active_wall = random.choice(wall_list)
    wall_list.remove(active_wall)

    #look for adjacent, unvisited cells
    adjacent = (-1,-1,-1)
    if maze[active_wall[0]+1][active_wall[1]][active_wall[2]]==2:
      adjacent = (active_wall[0]+1,active_wall[1],active_wall[2])
    if maze[active_wall[0]-1][active_wall[1]][active_wall[2]]==2:
      adjacent = (active_wall[0]-1,active_wall[1],active_wall[2])
    if maze[active_wall[0]][active_wall[1]+1][active_wall[2]]==2:
      adjacent = (active_wall[0],active_wall[1]+1,active_wall[2])
    if maze[active_wall[0]][active_wall[1]-1][active_wall[2]]==2:
      adjacent = (active_wall[0],active_wall[1]-1,active_wall[2])
    if maze[active_wall[0]][active_wall[1]][active_wall[2]+1]==2:
      adjacent = (active_wall[0],active_wall[1],active_wall[2]+1)
    if maze[active_wall[0]][active_wall[1]][active_wall[2]-1]==2:
      adjacent = (active_wall[0],active_wall[1],active_wall[2]-1)

    #if there are any adjacent unvisited cells, knock the wall down and treat the unvisited cell as a new cell
    if adjacent != (-1,-1,-1):
      maze[active_wall[0]][active_wall[1]][active_wall[2]]=0
      start_depth=adjacent[0]
      start_breadth=adjacent[1]
      start_length=adjacent[2]
      maze[start_depth][start_breadth][start_length]=0

    #check if finished by looking for unvisited cells
    finished=1
    for i in range(len(maze)):
      for j in range(len(maze[0])):
        if 2 in maze[i][j]:
          finished=0

  #rewrite top and bottom borders
  for i in range(len(maze[0])):
    for j in range(len(maze[0][0])):
      maze[0][i][j]=1
      maze[-1][i][j]=1
  #rewrite front and back borders
  for i in range(len(maze)):
    for j in range(len(maze[0][0])):
      maze[i][0][j]=1
      maze[i][-1][j]=1
  #rewrite right and left borders
  for i in range(len(maze)):
    for j in range(len(maze[0])):
      maze[i][j][0]=1
      maze[i][j][-1]=1

 # for floor in maze:
 #   for row in floor:
 #     print row
 #   print
  print("Maze generated.")
  return maze

def maze_to_string(raw_maze,EMPTY=".",WALL="O",UP="<",DOWN=">",UPDOWN="X",NEWLINE="\r\n"):
  """Takes the "raw" maze and writes it to a string"""
  #make mazef (maze formatted)
  mazef = [[[EMPTY for i in range(len(raw_maze[0][0]))] for j in range(len(raw_maze[0]))] for k in range((len(raw_maze)-1)//2)]

  #go through raw_maze floor by floor, bottom to top (skipping the solid top and bottom floors)
  for floor_num in range(1,len(raw_maze)-1):
    if floor_num%2 == 1:
      #write layout to mazef
      for row_num in range(len(raw_maze[0])):
        for element_num in range(len(raw_maze[0][0])):
          if raw_maze[floor_num][row_num][element_num]==1:
            mazef[(floor_num-1)//2][row_num][element_num]=WALL
    else:
      #add stairs to mazef levels above and below
      for row_num in range(len(raw_maze[0])):
        for element_num in range(len(raw_maze[0][0])):
          if raw_maze[floor_num][row_num][element_num]==0:
            mazef[(floor_num//2)][row_num][element_num]=DOWN
            if mazef[(floor_num//2)-1][row_num][element_num]==EMPTY:
              mazef[(floor_num//2)-1][row_num][element_num]=UP
            else:
              mazef[(floor_num//2)-1][row_num][element_num]=UPDOWN

  result = ""
  #print mazef to file  
  for i in range(len(mazef)):
    for j in range(len(mazef[0])):
      for k in range(len(mazef[0][0])):
        result = result + mazef[i][j][k]
      result = result + NEWLINE
    result = result + NEWLINE
  print("Completed.")
  return result

def save_maze(raw_maze,fileout):
  """Stores the maze as a comma-separated list of numbers.
  The first line of the file displays the length, breadth, and depth of the maze.
  The next line contains the bottom floor of the maze, and each subsequent line contains the floor above.
  (Note that every odd floor will consist almost entirely of walls; these are floors/ceilings.)
  0 = empty space
  1 = wall"""
  return

