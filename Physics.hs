module Physics where

import State
import Graphics.Gloss
import Graphics.Gloss.Data.Vector



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



-- Collision helpers

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