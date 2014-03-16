

--TODO add EMP to grid state

statesPerCell :: Int
statesPerCell = let
    bot = 11
    empty = 1
    mine = 1
    missle = 8
    bullet = 8
    in bot + bot + empty + mine + missle + bullet


gridSize :: Int
gridSize = 10 * 10


empStates :: Int
empStates = let
    noEmp = 1
    empDuration = 2
    in noEmp + empDuration


statesPerGrid :: Int
statesPerGrid = gridSize * statesPerCell


maxTurns :: Int
maxTurns = 1000


statesPerGame :: Int
statesPerGame = empStates + statesPerGrid + maxTurns


actions :: Int
actions = let
    nop = 1
    move = 8
    bullet = 8
    missle = 8
    mine = 8
    emp = 1
    in nop + move + bullet + missle + mine + emp


qSpace :: Int
qSpace = actions * statesPerGame








