module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

scrdimsI@(scrwI, scrhI) = (500, 500) :: (Int, Int)
scrdims@(scrw, scrh) = (fromIntegral scrwI, fromIntegral scrhI) :: (Float, Float)
defaultPaddleSize@(defaultPaddleWidth, defaultPaddleHeight) = (20, 100) :: (Float, Float)
defaultBallRadius = 10 :: Float




-- Game state

-- Initialisation 

paddleDragFactor = (-0.00025) :: Float
ballDragFactor = 0 :: Float

data Paddle = Paddle { paddlePosition :: Vector, paddleVelocity :: Vector, paddleColor :: Color, keyDown :: Bool }
data Ball = Ball { ballPosition :: Vector, ballVelocity :: Vector, ballColor :: Color }
data Pong = Pong { player1 :: Paddle, player2 :: Paddle, ball :: Ball, size :: (Float, Float), paused :: Bool, score :: (Int, Int) }

initialState :: Pong
initialState = Pong
    ( Paddle ( scrw/2, 0) (0,   0) black False )
    ( Paddle (-scrw/2, 0) (0,   0) black False )
    ( Ball   (      0, 0) (200, 100) blue  )
    scrdims
    True
    (0, 0)

    
-- Collisions
    
ballWallCollision :: Pong -> Ball -> Bool
ballWallCollision game ball = 
    let (sizeX, sizeY) = size game
        (ballX, ballY) = ballPosition ball
        ballRadius = defaultBallRadius
    in ballY + ballRadius > sizeY/2 || ballY - ballRadius < -sizeY/2
    
ballPaddleCollision :: [Paddle] -> Ball -> Bool
ballPaddleCollision [] _ = False
ballPaddleCollision (paddle:rest) ball =
    let paddlePos = paddlePosition paddle
        halfPaddleSize = mulSV 0.5 defaultPaddleSize
        ballPos = ballPosition ball
        ballRadius = defaultBallRadius
    in collideSphereRectangle (ballPos, ballRadius) (paddlePos + halfPaddleSize, paddlePos - halfPaddleSize) 
       || ballPaddleCollision rest ball
       
bounceBall :: Pong -> Ball -> Vector -> Vector
bounceBall game ball (vx, vy) = (vx', vy') where
    vx' = if ballPaddleCollision [player1 game, player2 game] ball 
            then (-vx) else vx
    vy' = if ballWallCollision game ball 
            then (-vy) else vy
       
    
clipPaddle :: Pong -> Paddle -> Paddle
clipPaddle game paddle = 
    let (paddleX, paddleY) = paddlePosition paddle
        halfHeight = defaultPaddleHeight / 2
        (sizeX, sizeY) = size game
        collidedTop = paddleY + halfHeight > sizeY / 2
        collidedBottom = paddleY - halfHeight < (-sizeY) / 2
        
        newPos = if collidedTop    then (paddleX,  sizeY/2   - halfHeight) 
            else if collidedBottom then (paddleX, (-sizeY)/2 + halfHeight)
            else paddlePosition paddle
            
        newVel = if collidedTop || collidedBottom then (0, 0)
            else paddleVelocity paddle
            
    in paddle {paddlePosition = newPos, paddleVelocity = newVel}   


-- Movement
    
updatePos :: Float -> Vector -> Vector -> Vector
updatePos timeStep vel pos = pos + mulSV timeStep vel

updateVel :: Float -> Float -> Vector -> Vector
updateVel timeStep dragFactor vel = vel + mulSV (timeStep * dragFactor * (magV vel)^2) vel
            
stepPaddle :: Float -> Paddle -> Paddle
stepPaddle timeStep paddle = paddle 
    { paddlePosition = updatePos timeStep vel pos
    , paddleVelocity = updateVel timeStep drag vel
    }
    where pos = paddlePosition paddle
          vel = paddleVelocity paddle
          drag = if keyDown paddle then 0 else paddleDragFactor
    
stepPaddles :: Float -> Pong -> Pong
stepPaddles timeStep game = game { player1 = updatePaddle . player1 $ game, 
                                   player2 = updatePaddle . player2 $ game }
    where updatePaddle = clipPaddle game . stepPaddle timeStep

    
stepBall :: Float -> Pong -> Pong
stepBall timeStep game = game { ball = Ball newPos newVel newColor } where
    oldBall = ball game
    newColor = ballColor oldBall
    newVel = bounceBall game oldBall . 
                updateVel timeStep ballDragFactor . ballVelocity $ oldBall
    newPos = updatePos timeStep newVel . ballPosition $ oldBall
    
    
stepGame :: Float -> Pong -> Pong
stepGame timeStep game = 
    if paused game then game else
        checkScore . stepBall timeStep . stepPaddles timeStep $ game


-- Scoring

resetBall :: Pong -> Pong
resetBall game =
    let oldScore = score game
        newGame = initialState { score = oldScore }
    in setKeys game newGame
        
checkScore :: Pong -> Pong
checkScore game = 
    let (gw, gh) = size game 
        (bx, by) = ballPosition . ball $ game
        (p1, p2) = score game
        outOfBoundsLeft  = bx < -gw/2
        outOfBoundsRight = bx >  gw/2
        newScore = if outOfBoundsLeft then (p1+1, p2) else if outOfBoundsRight then (p1, p2+1) else (p1, p2)
        
    in if outOfBoundsLeft || outOfBoundsRight then resetBall $ game {score = newScore} else game




-- Graphics

window :: Display
window = InWindow "Pong" scrdimsI (100, 100)

background :: Color
background = white
              
showPaddle :: Paddle -> Picture
showPaddle paddle = translate x y . color col $ rectangleSolid defaultPaddleWidth defaultPaddleHeight
    where (x, y) = paddlePosition paddle
          col    = paddleColor    paddle

showBall :: Ball -> Picture
showBall ball = translate x y . color col $ circleSolid defaultBallRadius
    where (x, y) = ballPosition ball
          col    = ballColor    ball

showArena :: (Float, Float) -> Picture
showArena (w, h) = rectangleWire w h
    
showScore :: (Int, Int) -> Picture
showScore (p1, p2) = translate (-35) (scrh/2-30) . scale 0.2 0.2 . text $ show p2 ++ " | " ++ show p1

showGame :: Pong -> Picture
showGame game = pictures
    [ showPaddle . player1 $ game
    , showPaddle . player2 $ game
    , showBall . ball $ game
    , showArena . size $ game
    , showScore . score $ game
    ]


    
    

-- IO

keyUD :: KeyState -> Bool
keyUD state = case state of Down -> True
                            Up   -> False

eventHandler :: Event -> Pong -> Pong
eventHandler event game = case event of 
    
    EventKey (Char 'w') state modifiers _           -> game { player2 = (player2 game) { paddleVelocity = (0,  100), keyDown = keyUD state } }
    EventKey (Char 's') state modifiers _           -> game { player2 = (player2 game) { paddleVelocity = (0, -100), keyDown = keyUD state } }
    EventKey (SpecialKey KeyUp) state modifiers _   -> game { player1 = (player1 game) { paddleVelocity = (0,  100), keyDown = keyUD state } }
    EventKey (SpecialKey KeyDown) state modifiers _ -> game { player1 = (player1 game) { paddleVelocity = (0, -100), keyDown = keyUD state } }
    
    EventKey (Char 'p') state modifiers _  -> game { paused = if keyUD state then not . paused $ game else paused game }


    otherwise -> game
    
    
main :: IO ()
main = play window background 60 initialState showGame eventHandler stepGame





-- Helper

setKeys :: Pong -> Pong -> Pong
setKeys old new =
    let k1 = keyDown . player1 $ old
        k2 = keyDown . player2 $ old
    in new { player1 = (player1 new) { keyDown = k1 }, player2 = (player2 new) { keyDown = k2 } }



collideSphereRectangle :: (Point, Float) -> (Point, Point) -> Bool
collideSphereRectangle (cPos, cRad) dims@((minx, miny), (maxx, maxy)) =
    pointInRectangle cPos dims || 
    foldl (\acc seg -> acc || pointLineDistance cPos seg < cRad) False
        [(a,b), (b,c), (c,d), (d,a)]
    where a = (minx, miny)
          b = (minx, maxy)
          c = (maxx, maxy)
          d = (maxx, miny)
          
pointInRectangle :: Point -> (Point, Point) -> Bool
pointInRectangle (px, py) ((minx, miny), (maxx, maxy)) = minx < px && px < maxx  &&  miny < py && py < maxy

pointLineDistance :: Point -> (Point, Point) -> Float
pointLineDistance p (v, w) = 
    let l2 = (magV $ w - v)^2
        t = max 0 . min 1 . (/l2) $ dotV (p - v) (w - v)
        projection = v + mulSV t (w - v)
    in if l2 == 0 then magV $ v - p else magV $ projection - p
