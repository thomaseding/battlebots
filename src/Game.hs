{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game where


import Control.Monad
import Control.Monad.State.Strict
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, isJust, fromJust)
import Data.Proxy
import GameData
import System.Random
import Values


--------------------------------------------------------------------------------


mkProxy :: a -> Proxy a
mkProxy _ = Proxy


newtype Id a = Id { runId :: a }
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


newArena :: Arena
newArena = Arena Map.empty


cellMap :: (Cell -> Cell) -> Arena -> Arena
cellMap f = Arena . Map.map f . unArena


--------------------------------------------------------------------------------


getCell :: Coords -> Arena -> Cell
getCell coords = case inBounds coords of
    False -> error "getCell out of bounds"
    True -> Map.findWithDefault emptyCell coords . unArena


rawPutCell :: Coords -> Cell -> Arena -> Arena
rawPutCell coords cell = case inBounds coords of
    False -> error "rawPutCell out of bounds"
    True -> Arena . Map.insert coords cell . unArena


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


--------------------------------------------------------------------------------


inBounds :: Coords -> Bool
inBounds coords = case coords of
    Coords x y -> 0 <= x && x < 10 && 0 <= y && y < 10


arenaCoords :: [Coords]
arenaCoords = do
    x <- [0 .. 9]
    y <- [0 .. 9]
    return $ Coords x y


arenaCells :: Arena -> [(Coords, Cell)]
arenaCells = Map.assocs . unArena


neighbors :: Coords -> [Coords]
neighbors coords = rights $ map (moveDir coords) allValues


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


--------------------------------------------------------------------------------


newtype Speed = Speed { unSpeed :: Int }
    deriving (Show, Eq, Ord)


newtype CollisionCoords = CollisionCoords { _collisionCoords :: Coords }
    deriving (Show, Eq, Ord)


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


gatherLandMines :: Arena -> [(LandMine, Coords)]
gatherLandMines arena = let
    cellInfos = arenaCells arena
    getLandMines coords cell = let
        f landMine = (landMine, coords)
        in map f $ _landMines cell
    in concatMap (uncurry getLandMines) cellInfos


removeLandMines :: Arena -> Arena
removeLandMines = cellMap $ \cell -> cell { _landMines = [] }


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


tickLandMines :: Arena -> Arena
tickLandMines arena = let
    ls = gatherLandMines arena
    arena' = removeLandMines arena
    in foldr (uncurry tickLandMine) arena' ls


tickLandMine :: LandMine -> Coords -> Arena -> Arena
tickLandMine mine coords arena = let
    cell = getCell coords arena
    mineCount = length $ _landMines cell
    mineExists = mineCount > 0
    botExists = isJust $ _bot cell
    in case mineExists || botExists of
        False -> runId $ putCell coords mine arena
        True -> case (mineExists && botExists) || (mineCount > 1) of
            False -> error "tickLandMine: The impossible just happened."
            True -> let
                collision = CollisionCoords coords
                in explodeMine collision arena


explodeMine :: CollisionCoords -> Arena -> Arena
explodeMine = let
    impact = ImpactDamage 2
    splash = SplashDamage 1
    in explodeWeapon impact splash


--------------------------------------------------------------------------------


explodeWeapon :: ImpactDamage -> SplashDamage -> CollisionCoords -> Arena -> Arena
explodeWeapon impact splash loc = let
    center = _collisionCoords loc
    ns = neighbors center
    impactAmount = _impactDamage impact
    splashAmount = _splashDamage splash
    damageCenter = feedDamage impactAmount center
    damageNeighbors = \arena -> foldr (\coords -> feedDamage splashAmount coords) arena ns
    in damageNeighbors . damageCenter


--------------------------------------------------------------------------------


newtype Winner = Winner { unWinner :: Player }
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


class (Monad m) => BoutMonad m


data BoutState = BoutState {
    _arena :: Arena,
    _time :: Int,
    _emp :: ()
} deriving (Show, Read, Eq, Ord)


newtype BoutEngine m a = BoutEngine { unBoutEngine :: StateT BoutState m a }
    deriving (Functor, Monad, MonadIO, MonadState BoutState)


setupBout :: StdGen -> Player -> Player -> BoutState
setupBout gen p0 p1 = let
    vals = randomRs (0, 9) gen
    bot0 = Bot p0 E10
    bot1 = Bot p1 E10
    x0 = 0
    x1 = 9
    y0 = vals !! 0
    y1 = vals !! 1
    coords0 = Coords x0 y0
    coords1 = Coords x1 y1
    putBot c b = fromJust . putCell c b
    arena = putBot coords0 bot0 $ putBot coords1 bot1 $ newArena
    in BoutState {
        _arena = arena,
        _time = 0,
        _emp = () }


runBout :: (BoutMonad m) => StdGen -> m (Maybe Winner)
runBout gen = let
    (gen1, gen2) = split gen
    ps = case fst $ random gen1 of
        True -> [P1, P2]
        False -> [P2, P1]
    st = setupBout gen2 (ps !! 0) (ps !! 1)
    go = do
        mWinner <- getWinner
        case mWinner of
            Just _ -> return mWinner
            Nothing -> do
                time <- gets _time
                case time < 1000 of
                    False -> return Nothing
                    True -> tickBout >> go
    in evalStateT (unBoutEngine go) st


getWinner :: (BoutMonad m) => BoutEngine m (Maybe Winner)
getWinner = do
    arena <- gets _arena
    let bots = map fst $ gatherBots arena
        isAlive bot@(Bot _ e) = case e of
            E0 -> True
            _ -> False
        aliveBots = filter isAlive bots
    return $ case aliveBots of
        [Bot p _] -> Just $ Winner p
        _ -> Nothing
    


tickBout :: (BoutMonad m) => BoutEngine m ()
tickBout = error "fwefweg93eb"













































