import sys
try:
  map = sys.argv[1][0:110].split()
except:
  sys.exit(1)

# Locate us and the opponent.
#
for y in range(0,10):
  for x in range(0, 10):
    if 'Y' == map[y][x]:
      me_y = y
      me_x = x
    elif 'X' == map[y][x]:
      him_y = y
      him_x = x

# If we're on a direct line with the opponent, fire a missile.
#
if me_y == him_y or me_x == him_x or abs(me_y - him_y) == abs(me_x - him_x):
  if   him_y < me_y and him_x < me_x:
    sys.stdout.write('M NW')
  elif him_y < me_y and him_x == me_x:
    sys.stdout.write('M N')
  elif him_y < me_y and him_x > me_x:
    sys.stdout.write('M NE')
  elif him_y == me_y and him_x < me_x:
    sys.stdout.write('M W')
  elif him_y == me_y and him_x > me_x:
    sys.stdout.write('M E')
  elif him_y > me_y and him_x < me_x:
    sys.stdout.write('M SW')
  elif him_y > me_y and him_x == me_x:
    sys.stdout.write('M S')
  elif him_y > me_y and him_x > me_x:
    sys.stdout.write('M SE')

# Otherwise, move randomly.
#
else:
  import random
  sys.stdout.write(random.choice(['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']))
