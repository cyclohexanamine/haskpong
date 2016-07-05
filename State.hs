module State where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector



-- Global Variables

scrdimsI@(scrwI, scrhI) = (500, 500) :: (Int, Int)
scrdims@(scrw, scrh) = (fromIntegral scrwI, fromIntegral scrhI) :: (Float, Float)
defaultPaddleSize@(defaultPaddleWidth, defaultPaddleHeight) = (20, 100) :: (Float, Float)
defaultBallRadius = 10 :: Float



-- Initialisation

paddleDragFactor = (-0.00025) :: Float
ballDragFactor = 0 :: Float

data Paddle = Paddle { paddlePosition :: Point, paddleVelocity :: Vector, paddleColor :: Color, keyDown :: Bool }
data Ball = Ball { ballPosition :: Point, ballVelocity :: Vector, ballColor :: Color }
data Pong = Pong { player1 :: Paddle, player2 :: Paddle, ball :: Ball, size :: (Float, Float), paused :: Bool, score :: (Int, Int) }

initialState :: Pong
initialState = Pong
    ( Paddle ( scrw/2, 0) (0,   0) black False )
    ( Paddle (-scrw/2, 0) (0,   0) black False )
    ( Ball   (      0, 0) (200, 100) blue  )
    scrdims
    True
    (0, 0)



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



-- Helpers

setKeys :: Pong -> Pong -> Pong
setKeys old new =
    let k1 = keyDown . player1 $ old
        k2 = keyDown . player2 $ old
    in new { player1 = (player1 new) { keyDown = k1 }, player2 = (player2 new) { keyDown = k2 } }