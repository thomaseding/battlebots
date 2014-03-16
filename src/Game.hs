module Game where


import Data.Either
import Data.Map (Map)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import GameData
import Values


--------------------------------------------------------------------------------


inBounds :: Coords -> Bool
inBounds coords = case coords of
    Coords x y -> 0 <= x && x < 10 && 0 <= y && y < 10


getCell :: Coords -> Arena -> Cell
getCell coords = case inBounds coords of
    False -> error "getCell out of bounds"
    True -> Map.findWithDefault emptyCell coords . unArena


rawPutCell :: Coords -> Cell -> Arena -> Arena
rawPutCell coords cell = case inBounds coords of
    False -> error "rawPutCell out of bounds"
    True -> Arena . Map.insert coords cell . unArena


newtype Id a = Id { runId :: a }
    deriving (Show, Eq, Ord)


class PutCell a b | a -> b where
    putCell :: Coords -> a -> Arena -> b Arena


instance PutCell Bot Maybe where
    putCell coords bot arena = let
        cell = getCell coords arena
        in case _bot cell of
            Just _ -> Nothing
            Nothing -> let
                cell' = cell { _bot = Just bot }
                in Just $ rawPutCell coords cell' arena


instance PutCell LandMine Id where
    putCell coords landMine arena = let
        cell = getCell coords arena
        landMines = landMine : _landMines cell
        cell' = cell { _landMines = landMines }
        in Id $ rawPutCell coords cell' arena


instance PutCell Bullet Id where
    putCell coords bullet arena = let
        cell = getCell coords arena
        bullets = bullet : _bullets cell
        cell' = cell { _bullets = bullets }
        in Id $ rawPutCell coords cell' arena


instance PutCell Missile Id where
    putCell coords missile arena = let
        cell = getCell coords arena
        missiles = missile : _missiles cell
        cell' = cell { _missiles = missiles }
        in Id $ rawPutCell coords cell' arena


--------------------------------------------------------------------------------


arenaCoords :: [Coords]
arenaCoords = do
    x <- [0 .. 9]
    y <- [0 .. 9]
    return $ Coords x y


arenaCells :: Arena -> [(Coords, Cell)]
arenaCells = Map.assocs . unArena


cellMap :: (Cell -> Cell) -> Arena -> Arena
cellMap f = Arena . Map.map f . unArena


mkBots :: [Bot]
mkBots = [Bot P1 E10, Bot P2 E10]



class LoseEnergy a where
    loseEnergy :: Int -> a -> a


instance LoseEnergy Energy where
    loseEnergy n e = if n < 0
        then error "loseEnergy supplied negative number"
        else let
            en = fromEnum e
            en' = max 0 $ en - n
            in toEnum en'


instance LoseEnergy Bot where
    loseEnergy n (Bot c e) = Bot c $ loseEnergy n e


moveDir :: Coords -> Dir -> Either Coords Coords
moveDir coords dir = let
    Coords x y = coords
    coords' = case dir of
        N -> Coords x (y - 1)
        NE -> Coords (x + 1) (y - 1)
        E -> Coords (x + 1) y
        SE -> Coords (x + 1) (y + 1)
        S -> Coords x (y + 1)
        SW -> Coords (x - 1) (y + 1)
        W -> Coords (x - 1) y
        NW -> Coords (x - 1) (y - 1)
    in case inBounds coords' of
        False -> Left coords'
        True -> Right coords'


newtype CollisionCoords = CollisionCoords { _collisionCoords :: Coords }
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


gatherBots :: Arena -> [(Bot, Coords)]
gatherBots arena = let
    cellInfos = arenaCells arena
    getBots coords cell = let
        f bot = (bot, coords)
        in map f $ maybeToList $ _bot cell
    in concatMap (uncurry getBots) cellInfos


removeBots :: Arena -> Arena
removeBots = cellMap $ \cell -> cell { _bot = Nothing }


--------------------------------------------------------------------------------


gatherMissiles :: Arena -> [(Missile, Coords)]
gatherMissiles arena = let
    cellInfos = arenaCells arena
    getMissiles coords cell = let
        f missile = (missile, coords)
        in map f $ _missiles cell
    in concatMap (uncurry getMissiles) cellInfos


removeMissiles :: Arena -> Arena
removeMissiles = cellMap $ \cell -> cell { _missiles = [] }


tickMissiles :: Arena -> Arena
tickMissiles arena = let
    ms = gatherMissiles arena
    arena' = removeMissiles arena
    in foldr (uncurry tickMissile) arena' ms


tickMissile :: Missile -> Coords -> Arena -> Arena
tickMissile m coords arena = let
    Missile dir = m
    in case moveMissile coords dir of
        Left boomCoords -> explodeMissile boomCoords arena
        Right coords' -> let
            cell = getCell coords' arena
            in case _bot cell of
                Nothing -> runId $ putCell coords' m arena
                Just _ -> let
                    boomCoords = CollisionCoords coords'
                    in explodeMissile boomCoords arena


moveMissile :: Coords -> Dir -> Either CollisionCoords Coords
moveMissile coords dir = let
    move = flip moveDir dir
    in case move coords >>= move of
        Left coords' -> Left $ CollisionCoords coords'
        Right coords' -> Right coords'


explodeMissile :: CollisionCoords -> Arena -> Arena
explodeMissile = explodeWeapon (ImpactDamage 3) (SplashDamage 1)


--------------------------------------------------------------------------------


neighbors :: Coords -> [Coords]
neighbors coords = rights $ map (moveDir coords) allValues


newtype ImpactDamage = ImpactDamage { _impactDamage :: Int }
    deriving (Show, Eq, Ord)


newtype SplashDamage = SplashDamage { _splashDamage :: Int }


feedDamage :: Int -> Coords -> Arena -> Arena
feedDamage amount coords arena = let
    botInfos = gatherBots arena
    damageBot (bot, botCoords) = case coords == botCoords of
        False -> bot
        True -> loseEnergy amount bot
    updateBotInfo botInfo = (damageBot botInfo, snd botInfo)
    botInfos' = map updateBotInfo botInfos
    arena' = removeBots arena
    putBotInfo (bot, coords) = let
        err = error "feedDamage: The impossible just happened."
        in maybe err id . putCell coords bot
    in foldr putBotInfo arena' botInfos'


explodeWeapon :: ImpactDamage -> SplashDamage -> CollisionCoords -> Arena -> Arena
explodeWeapon impact splash loc = let
    center = _collisionCoords loc
    ns = neighbors center
    impactAmount = _impactDamage impact
    splashAmount = _splashDamage splash
    damageCenter = feedDamage impactAmount center
    damageNeighbors = \arena -> foldr (\coords -> feedDamage splashAmount coords) arena ns
    in damageNeighbors . damageCenter































