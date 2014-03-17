{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game (
    GatherAll(gatherAll),
    MonadBattleBots(..),
    Command(..),
    Winner(..),
    runMatch,
    getCell,
    moveDir,
    getBotCoords,
) where


import Control.Monad (foldM, unless, replicateM, liftM)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.State.Strict (gets, modify, evalStateT, StateT, MonadState)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Either (rights)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, isJust, fromJust)
import Data.Ord (comparing)
import Data.Proxy (Proxy(Proxy))
import GameData
import Values (allValues)


--------------------------------------------------------------------------------


mkProxy :: a -> Proxy a
mkProxy _ = Proxy


pick :: (MonadRandom m) => [a] -> m a
pick xs = case length xs of
    0 -> error "pick: empty list"
    n -> do
        idx <- getRandomR (0, n - 1)
        return $ xs !! idx


newtype Id a = Id { runId :: a }
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


newArena :: Arena
newArena = Arena Map.empty


cellMap :: (Cell -> Cell) -> Arena -> Arena
cellMap f = Arena . Map.map f . unArena


tickArena :: Arena -> Arena
tickArena = tickLandMines . tickBullets . tickMissiles


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
    putBotInfo (bot, botCoords) = let
        err = error "feedDamage: The impossible just happened."
        in maybe err id . putCell botCoords bot
    in foldr putBotInfo arena' botInfos'


--------------------------------------------------------------------------------


gatherBots :: Arena -> [(Bot, Coords)]
gatherBots arena = let
    cellInfos = arenaCells arena
    getBots coords cell = let
        f bot = (bot, coords)
        in map f $ maybeToList $ _bot cell
    orderByPlayer = sortBy $ comparing $ \(Bot p _, _) -> p
    in orderByPlayer $ concatMap (uncurry getBots) cellInfos


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


instance GatherAll Bot Maybe where
    gatherAll _ = gatherBots
    removeAll _ = removeBots


instance GatherAll Bullet Id where
    gatherAll _ = gatherBullets
    removeAll _ = removeBullets


instance GatherAll Missile Id where
    gatherAll _ = gatherMissiles
    removeAll _ = removeMissiles


instance GatherAll LandMine Id where
    gatherAll _ = gatherLandMines
    removeAll _ = removeLandMines


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
            True -> error "tickLandMine: The impossible just happened."
            False -> let
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


data Command
    = DoNothing
    | Move Dir
    | FireBullet Dir
    | FireMissile Dir
    | DropLandMine Dir
    | FireEmp
    | UnknownCommand
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


class (MonadRandom m) => MonadBattleBots m where
    getCommand :: Player -> Arena -> m Command
    tellArena :: Arena -> m ()


data BoutState = BoutState {
    _arena :: Arena,
    _time :: Int,
    _emp :: Maybe EmpDuration
} deriving (Show, Read, Eq, Ord)


newtype BoutEngine m a = BoutEngine { unBoutEngine :: StateT BoutState m a }
    deriving (Functor, Monad, MonadIO, MonadState BoutState, MonadTrans)


instance (MonadRandom m) => MonadRandom (BoutEngine m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs


instance (MonadBattleBots m) => MonadBattleBots (BoutEngine m) where
    getCommand p = lift . getCommand p
    tellArena = lift . tellArena


setupBoutM :: (MonadRandom m) => m BoutState
setupBoutM = do
    coordsLeft <- liftM (Coords 0) $ getRandomR (0, 9)
    coordsRight <- liftM (Coords 9) $ getRandomR (0, 9)
    (playerLeft, playerRight) <- pick [(P1, P2), (P2, P1)]
    let infoLeft = (playerLeft, coordsLeft)
        infoRight = (playerRight, coordsRight)
    return $ setupBout infoLeft infoRight


setupBout :: (Player, Coords) -> (Player, Coords) -> BoutState
setupBout (p0, coords0) (p1, coords1) = let
    bot0 = Bot p0 E10
    bot1 = Bot p1 E10
    putBot c b = fromJust . putCell c b
    arena = putBot coords0 bot0 $ putBot coords1 bot1 $ newArena
    in BoutState {
        _arena = arena,
        _time = 0,
        _emp = Nothing }


runBout :: (MonadBattleBots m) => m (Maybe Winner)
runBout = do
    st <- setupBoutM
    evalStateT (unBoutEngine go) st
    where
        go = do
            gameOver <- getGameOver
            case gameOver of
                True -> do
                    tellArena =<< gets _arena
                    getWinner
                False -> do
                    tellArena =<< gets _arena
                    tickBout
                    go


isAlive :: Bot -> Bool
isAlive b = case b of
    Bot _ E0 -> False
    _ -> True


getGameOver :: (MonadBattleBots m) => BoutEngine m Bool
getGameOver = do
    arena <- gets _arena
    time <- gets _time
    let bots = map fst $ gatherBots arena
        aliveBots = filter isAlive bots
    return $ bots /= aliveBots && time < 1000


getWinner :: (MonadBattleBots m) => BoutEngine m (Maybe Winner)
getWinner = do
    arena <- gets _arena
    let bots = map fst $ gatherBots arena
        aliveBots = filter isAlive bots
    return $ case aliveBots of
        [Bot p _] -> Just $ Winner p
        _ -> Nothing


tickBout :: (MonadBattleBots m) => BoutEngine m ()
tickBout = do
    runCommands
    modify $ \st -> let
        emp = case _emp st of
            Nothing -> Nothing
            Just EmpTwoRounds -> Just EmpOneRound
            Just EmpOneRound -> Nothing
        in st {
            _arena = tickArena $ _arena st,
            _time = _time st + 1,
            _emp = emp }


runCommands :: (MonadBattleBots m) => BoutEngine m ()
runCommands = do
    arena <- gets _arena
    let bots = map fst $ gatherBots arena
        (bot0, bot1) = case bots of
            [b0, b1] -> (b0, b1)
            _ -> error "tickBout: The impossible just happened."
        Bot p0 _ = bot0
        Bot p1 _ = bot1
        reissueMove prevMoveSuccess cmd bot = case prevMoveSuccess of
            Success -> return ()
            Failure -> issueMove cmd bot >> return ()
    cmd0 <- getCommand p0 arena
    cmd1 <- getCommand p1 arena
    empActive <- gets $ isJust . _emp
    unless empActive $ do
        moveSuccess0 <- issueMove cmd0 bot0
        moveSuccess1 <- issueMove cmd1 bot1
        reissueMove moveSuccess0 cmd0 bot0
        reissueMove moveSuccess1 cmd1 bot1
        issueNonMove cmd0 bot0
        issueNonMove cmd1 bot1


data Success = Failure | Success


issueMove :: (MonadBattleBots m) => Command -> Bot -> BoutEngine m Success
issueMove cmd bot = case cmd of
    Move dir -> moveBot bot dir
    _ -> return Failure


issueNonMove :: (MonadBattleBots m) => Command -> Bot -> BoutEngine m ()
issueNonMove cmd bot = case cmd of
    DoNothing -> return ()
    Move _ -> return ()
    FireBullet dir -> fireBullet bot dir
    FireMissile dir -> fireMissile bot dir
    DropLandMine dir -> dropLandMine bot dir
    FireEmp -> fireEmp bot
    UnknownCommand -> return ()


fireProjectile :: (MonadBattleBots m, Projectile a) => Bot -> a -> BoutEngine m ()
fireProjectile bot p = modify $ \st -> let
    arena = _arena st
    coords = getBotCoords bot arena
    arena' = runId $ putCell coords p arena
    in st { _arena = arena' }


fireBullet :: (MonadBattleBots m) => Bot -> Dir -> BoutEngine m ()
fireBullet bot = fireProjectile bot . Bullet


fireMissile :: (MonadBattleBots m) => Bot -> Dir -> BoutEngine m ()
fireMissile bot = fireProjectile bot . Missile


dropLandMine :: (MonadBattleBots m) => Bot -> Dir -> BoutEngine m ()
dropLandMine bot dir = modify $ \st -> let
    arena = _arena st
    coords = getBotCoords bot arena
    in case moveDir coords dir of
        Left _ -> st
        Right coords' -> case _bot $ getCell coords' arena of
            Just _ -> st
            Nothing -> let
                arena' = runId $ putCell coords' LandMine arena
                in st { _arena = arena' }


fireEmp :: (MonadBattleBots m) => Bot -> BoutEngine m ()
fireEmp bot = do
    arena <- gets _arena
    let bot' = loseEnergy 1 bot
        coords = getBotCoords bot arena
        arena' = removeBot bot arena
        arena'' = case putCell coords bot' arena' of
            Nothing -> error "fireEmp: The impossible just happened."
            Just a -> a
    modify $ \st -> st { _arena = arena'', _emp = Just EmpTwoRounds }


getBotCoords :: Bot -> Arena -> Coords
getBotCoords bot arena = let
    botInfos = gatherBots arena
    in case botInfos of
        [(bot0, coords0), (bot1, coords1)] -> case bot == bot0 of
            True -> coords0
            False -> case bot == bot1 of
                True -> coords1
                False -> error "getBotCoords: The impossible just happened."
        _ -> error "getBotCoords: The impossible just happened."


removeBot :: Bot -> Arena -> Arena
removeBot bot arena = let
    coords = getBotCoords bot arena
    cell = getCell coords arena
    cell' = cell { _bot = Nothing }
    in rawPutCell coords cell' arena


moveBot :: (MonadBattleBots m) => Bot -> Dir -> BoutEngine m Success
moveBot bot dir = do
    arena <- gets _arena
    let coords = getBotCoords bot arena
        mArena' = case moveDir coords dir of
            Left _ -> Nothing
            Right coords' -> putCell coords' bot $ removeBot bot arena
    case mArena' of
        Nothing -> return Failure
        Just arena' -> do
            modify $ \st -> st { _arena = arena' }
            return Success


getMatchWinner :: [Maybe Winner] -> Maybe Winner
getMatchWinner ws = let
    count p = length . filter (== (Just $ Winner p))
    countP1 = count P1 ws
    countP2 = count P2 ws
    in fmap Winner $ case compare countP1 countP2 of
        EQ -> Nothing
        GT -> Just P1
        LT -> Just P2


runMatch :: (MonadBattleBots m) => Int -> m (Maybe Winner)
runMatch numBouts = liftM getMatchWinner $ replicateM numBouts runBout
































