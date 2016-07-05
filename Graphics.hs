module Graphics where

import State
import Graphics.Gloss



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