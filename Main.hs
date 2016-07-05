module Main where

import State
import Physics
import Graphics

import Graphics.Gloss.Interface.Pure.Game



-- Game state

stepGame :: Float -> Pong -> Pong
stepGame timeStep game =
    if paused game then game else
        checkScore . stepBall timeStep . stepPaddles timeStep $ game



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