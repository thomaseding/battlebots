module Game where


import Control.Monad
import Data.Either
import Data.Map (Map)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import Data.Proxy
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


gatherBullets :: Arena -> [(Bullet, Coords)]
gatherBullets arena = let
    cellInfos = arenaCells arena
    getBullets coords cell = let
        f bullet = (bullet, coords)
        in map f $ _bullets cell
    in concatMap (uncurry getBullets) cellInfos


removeBullets :: Arena -> Arena
removeBullets = cellMap $ \cell -> cell { _bullets = [] }


gatherMissiles :: Arena -> [(Missile, Coords)]
gatherMissiles arena = let
    cellInfos = arenaCells arena
    getMissiles coords cell = let
        f missile = (missile, coords)
        in map f $ _missiles cell
    in concatMap (uncurry getMissiles) cellInfos


removeMissiles :: Arena -> Arena
removeMissiles = cellMap $ \cell -> cell { _missiles = [] }


class (PutCell a b) => GatherAll a b where
    gatherAll :: Proxy a -> Arena -> [(a, Coords)]
    removeAll :: Proxy a -> Arena -> Arena


instance GatherAll Bullet Id where
    gatherAll _ = gatherBullets
    removeAll _ = removeBullets


instance GatherAll Missile Id where
    gatherAll _ = gatherMissiles
    removeAll _ = removeMissiles


--------------------------------------------------------------------------------


mkProxy :: a -> Proxy a
mkProxy _ = Proxy


newtype Speed = Speed { unSpeed :: Int }


--------------------------------------------------------------------------------


class (GatherAll a Id) => Projectile a where
    projectileDir :: a -> Dir
    projectileSpeed :: Proxy a -> Speed
    projectileImpactDamage :: Proxy a -> ImpactDamage
    projectileSplashDamage :: Proxy a -> SplashDamage


instance Projectile Bullet where
    projectileDir (Bullet dir) = dir
    projectileSpeed _ = Speed 3
    projectileImpactDamage _ = ImpactDamage 1
    projectileSplashDamage _ = SplashDamage 0


instance Projectile Missile where
    projectileDir (Missile dir) = dir
    projectileSpeed _ = Speed 2
    projectileImpactDamage _ = ImpactDamage 3
    projectileSplashDamage _ = SplashDamage 1


--------------------------------------------------------------------------------


tickBullets :: Arena -> Arena
tickBullets = tickProjectiles (Proxy :: Proxy Bullet)


tickMissiles :: Arena -> Arena
tickMissiles = tickProjectiles (Proxy :: Proxy Missile)


tickProjectiles :: (Projectile a) => Proxy a -> Arena -> Arena
tickProjectiles proxy arena = let
    ps = gatherAll proxy arena
    arena' = removeAll proxy arena
    in foldr (uncurry tickProjectile) arena' ps


tickProjectile :: (Projectile a) => a -> Coords -> Arena -> Arena
tickProjectile p coords arena = let
    dir = projectileDir p
    proxy = mkProxy p
    in case moveProjectile proxy coords dir of
        Left collision -> explodeProjectile proxy collision arena
        Right coords' -> let
            cell = getCell coords' arena
            in case _bot cell of
                Nothing -> runId $ putCell coords' p arena
                Just _ -> let
                    collision = CollisionCoords coords'
                    in explodeProjectile proxy collision arena


repeatM :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatM n f x = let
    g = const . f
    in foldM g x $ replicate n ()


moveProjectile :: (Projectile a) => Proxy a -> Coords -> Dir -> Either CollisionCoords Coords
moveProjectile proxy coords dir = let
    move = flip moveDir dir
    in case return coords >>= repeatM (unSpeed $ projectileSpeed proxy) move of
        Left coords' -> Left $ CollisionCoords coords'
        Right coords' -> Right coords'


explodeProjectile :: (Projectile a) => Proxy a => CollisionCoords -> Arena -> Arena
explodeProjectile proxy = let
    impact = projectileImpactDamage proxy
    splash = projectileSplashDamage proxy
    in explodeWeapon impact splash


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































