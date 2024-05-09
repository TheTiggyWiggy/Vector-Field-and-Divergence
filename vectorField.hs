-- Final Project Due May 10th
-- Tanner Wagner
-- Professor Haugh
-- CS 357
-- 8 May 2024

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO.Unsafe

-- Data Model Definition
-- Defines the Model data type, representing the state of the application.
-- Contains fields for vector field functions, grid points, last clicked position,
-- whether the last click was handled, current field index, and output text display.

data Model = Model
    { field :: VectorField
    , fieldPoints :: [(Float, Float)]
    , lastClick :: Maybe (Float, Float)
    , clickHandled :: Bool
    , fieldIndex :: Int
    , outputText :: String
    }

-- Type for Vector Field
-- Represents a vector field as a pair of functions, each taking two Floats
-- and returning a Float, corresponding to the x and y components of the field at any point.

type VectorField = (Float -> Float -> Float, Float -> Float -> Float)

-- Main Function
-- Entry point of the application. Initializes the model with a default vector field,
-- sets up the window properties, and runs the game simulation.

main :: IO ()
main = do
    let fields = [(\x y -> x, \x y -> y), (\x y -> -x, \x y -> -y), (\x _ -> 5, \_ y -> 10)]
    let model = Model (fields !! 0) fieldGrid Nothing False 0 ""
    play (InWindow "Divergence Simulation" (1600, 800) (10, 10)) black 30 model drawModel handleEvent updateModel

-- Draw Model Function
-- Combines all visual components of the model into a single picture.
-- It includes translating the vector field to the center and adding educational text to the side.

drawModel :: Model -> Picture
drawModel model = pictures [
    translate (-400) 0 $ vectorField model,
    translate 800 0 $ educationalText model
    ]

-- Vector Field Rendering Function
-- Generates a visual representation of a vector field from the model's state.
-- This function is responsible for drawing each vector at specified grid points,
-- handling the coloring based on vector magnitude and direction, and responding to user clicks.
-- The main components of this function include:
-- 1. Drawing vectors: Each vector is drawn from the origin (0, 0) to its end point,
--    which is determined based on the current vector field selected by `fieldIndex`.
--    Vectors are translated to their respective positions in the grid by scaling the coordinates.
-- 2. Coloring vectors: Vector color varies based on the vector's magnitude, calculated using the Euclidean norm
--    (sqrt(x^2 + y^2)), normalized by sqrt(2) for scaling. The color intensity changes with magnitude,
--    highlighting vector strength and direction.
-- 3. Click response: If a click occurs within the vector field, a red circle and the current output text
--    are displayed at the click location, indicating interaction.
-- 4. Mathematical transformations:
--    a. Vector endpoints calculation (`vectorEnd`): Depending on the `fieldIndex`, vectors are scaled and directed
--       accordingly. For example, a positive divergence field has vectors pointing outward, negative divergence
--       inward, and a uniform field has constant vectors.
--    b. Arrowheads drawing (`arrowHead`): Arrowheads are added to vectors to indicate direction. The direction
--       (angle) of the vector is calculated using `atan2`, and the arrowhead is constructed using basic trigonometric
--       transformations to position the tips at the calculated angles.

vectorField :: Model -> Picture
vectorField model = pictures $
    [ translate (400 * x) (400 * y) $ color (vectorColor x y) $ vectorLine (x, y) model
    | (x, y) <- fieldPoints model, not (x == 0 && y == 0) ]
    ++ [ case lastClick model of
         Just (lx, ly) -> pictures [
             translate lx ly $ color red $ circleSolid 5,
             translate (lx + 10) (ly + 10) $ scale 0.2 0.2 $ color white $ text (outputText model)]
         Nothing -> Blank ]
    where
        vectorColor x y
          | fieldIndex model == 2 = white
          | otherwise = let norm = sqrt (x * x + y * y) / sqrt 2
                        in makeColor norm norm (1 - norm) 1

        vectorLine (x, y) model = pictures [line [(0,0), vectorEnd (x, y) model], arrowHead (vectorEnd (x, y) model) (x, y) model]

        vectorEnd (x, y) model
          | fieldIndex model == 0 = (50 * x, 50 * y)
          | fieldIndex model == 1 = (50 * (-x), 50 * (-y))
          | fieldIndex model == 2 = (50 * 5 / 15, 50 * 10 / 15)

        arrowHead (px, py) (vx, vy) model = let
            size = 5
            angle = 20 * (pi / 180)
            dir = atan2 vy vx
            leftTip = rotateV (dir + angle) (px, py) size
            rightTip = rotateV (dir - angle) (px, py) size
            in polygon [leftTip, (px, py), rightTip]

-- Vector Rotation Function
-- Performs a rotation transformation on a point in the 2D plane.
-- This function is used to compute new coordinates for a point after rotating it around the origin
-- by a specified angle, commonly used for drawing arrowheads in vector representations.
-- Parameters:
-- angle: The rotation angle in radians. Positive values rotate counterclockwise.
-- (px, py): The initial point coordinates to be rotated.
-- len: The length of the line from the origin to the point, used to maintain the vector's magnitude.
-- Returns:
-- A tuple representing the new coordinates of the point after rotation.
-- Mathematical Explanation:
-- The function applies the standard rotation matrix transformation in 2D:
-- [ cos(θ) -sin(θ) ]
-- [ sin(θ)  cos(θ) ]
-- where θ is the rotation angle. This transformation is applied to the point (px, py),
-- treated as a vector from the origin (0,0) to (px, py). The new x-coordinate is calculated as
-- (px + len * cos(angle)) and the new y-coordinate as (py + len * sin(angle)),
-- using the length (len) directly to position the endpoint after rotation.

rotateV :: Float -> (Float, Float) -> Float -> (Float, Float)
rotateV angle (px, py) len = (px + len * cos angle, py + len * sin angle)

-- The 'educationalText' function takes a 'Model' and returns a 'Picture'.
-- This function is designed to display educational text on the screen using the 'Picture' type in a specified format.
-- It operates as follows:
-- - 'explanationText' extracts the explanation text from the given 'Model'.
-- - 'lines' breaks down the explanation text into a list of lines.
-- - 'zipWith' pairs each line of text with its corresponding y-coordinate.
-- - Within the 'zipWith' function:
-- - 'translate' shifts each text line to the left by 700 units and up by y * 50 units from the top.
-- - 'scale' resizes each text line to 0.2 times its original size.
-- - 'color' sets the color of each text line to white.
-- - 'text' creates a 'Picture' from each line of text.
-- - 'pictures' combines all the individual text 'Picture's into a single 'Picture'.

educationalText :: Model -> Picture
educationalText model = pictures $
    zipWith (\line y -> translate (-700) (300 - y * 50) $ scale 0.2 0.2 $ color white $ text line)
    (lines $ explanationText model)
    [0..]

-- The 'explanationText' function takes a 'Model' and returns a formatted explanation text as a single string.
-- This function serves to provide detailed descriptions of various vector fields based on the index specified in the 'Model'.
-- It operates as follows:
-- - It uses a 'case' expression to match the 'fieldIndex' of the 'Model'.
-- - Depending on the index value:
-- - If the index is 0, it describes a vector field with positive divergence.
-- - If the index is 1, it describes a vector field with negative divergence.
-- - If the index is 2, it describes a vector field with zero divergence.
-- - If the index doesn't match any of the specified cases, it returns an empty string.
-- - Each case provides a detailed explanation of the vector field, 
-- -including its characteristics, description, and details about divergence.

explanationText :: Model -> String
explanationText model = unlines $ case fieldIndex model of
    0 -> ["Field (x, y): Positive Divergence",
          "Description:",
          "  - Vectors radiate outward",
          "  - Represents a source field",
          "  - Example: Increasing temperature",
          "Detail (div(F) > 0):",
          "  - The magnitude of the vectors",
          "    increase as you go outwards.",
          "  - This shows that while the",
          "    divergence remains constant,",
          "    the strength of the field",
          "    increases radially."]
    1 -> ["Field (-x, -y): Negative Divergence",
          "Description:",
          "  - Vectors converge inward",
          "  - Represents a sink field",
          "  - Example: Cooling region",
          "Detail (div(F) < 0):",
          "  - Similarly to the uniform",
          "    field, the magnitude of",
          "    the vectors increase as",
          "    you go out radially.",
          "  - Direction is reversed",
          "    due to sign of div(F)."]
    2 -> ["Field (5, 10): Zero Divergence",
          "Description:",
          "  - Constant vector field",
          "  - Vectors have the same magnitude",
          "    and direction",
          "  - Represents steady flow",
          "Detail (div(F) = 0):",
          "  - Zero divergence indicates that",
          "    our vector field has no",
          "    dependence on the input",
          "    and therefore our vectors",
          "    are equal everywhere."]
    _ -> [""]

-- The 'handleEvent' function takes an 'Event' and a 'Model' and returns an updated 'Model' based on the event.
-- This function is responsible for handling user events such as mouse clicks and keyboard inputs.
-- It operates as follows:
-- - It uses a 'case' expression to match the type of event.
-- - If the event is a mouse click with the left button ('MouseButton LeftButton'):
-- - It checks if the click falls within the bounds of the vector field.
-- - If so, it updates the 'lastClick' field of the 'Model' with the adjusted 
-- - coordinates of the click and sets 'clickHandled' to 'False'.
-- - If the event is a key press ('Char 's'):
-- - It calculates the index of the next vector field.
-- - It updates the 'Model' with the new vector field and its index.
-- - It calculates the divergence value for the new vector field and updates the 
-- - 'outputText' field of the 'Model' accordingly.
-- - For any other event, it leaves the 'Model' unchanged.

handleEvent :: Event -> Model -> Model
handleEvent event model = case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
        let (x, y) = mousePos
            adjustedX = x + 400
            vectorFieldWidth = 800
            vectorFieldHeight = 800
            fieldLeftBound = -400
            fieldRightBound = 430
            fieldTopBound = 400
            fieldBottomBound = -400
        in if adjustedX >= fieldLeftBound && adjustedX <= fieldRightBound &&
              y >= fieldBottomBound && y <= fieldTopBound
           then model { lastClick = Just (adjustedX, y), clickHandled = False }
           else model
    EventKey (Char 's') Down _ _ ->
        let newIndex = (fieldIndex model + 1) `mod` 3
            fields = [(\x y -> x, \x y -> y), (\x y -> -x, \x y -> -y), (\x _ -> 5, \_ y -> 10)]
            newModel = model { field = fields !! newIndex, fieldIndex = newIndex }
            newDivValue = calculateDivergence newModel
        in newModel { outputText = "div(F) = " ++ show newDivValue }
    _ -> model

-- The 'updateModel' function takes a time delta ('Float') and a 'Model' and returns an updated 'Model'.
-- This function is responsible for updating the 'Model' based on elapsed time and internal state.
-- It operates as follows:
-- - It ignores the time delta and directly uses the 'Model'.
-- - It uses a 'case' expression to match the 'lastClick' field of the 'Model'.
-- - If there was a previous click ('Just (lx, ly)'):
-- - And if the click has not been handled yet:
-- - It calculates the divergence value for the current model.
-- - It prints a message to the console indicating the coordinates of the click and the divergence value.
-- - It updates the 'Model' to mark the click as handled and sets the 'outputText' with the divergence value.
-- - Otherwise, it leaves the 'Model' unchanged.
-- - If there was no previous click or it has already been handled:
-- - It leaves the 'Model' unchanged.

updateModel :: Float -> Model -> Model
updateModel _ model = case lastClick model of
    Just (lx, ly) | not (clickHandled model) -> unsafePerformIO $ do
        let divValue = calculateDivergence model
        putStrLn $ "Click at: (" ++ show lx ++ ", " ++ show ly ++ "), div(F) = " ++ show divValue
        return model { clickHandled = True, outputText = "div(F) = " ++ show divValue }
    _ -> model

-- The 'calculateDivergence' function takes a 'Model' and returns the divergence value as a 'Float'.
-- This function calculates the divergence based on the index of the vector field in the 'Model'.
-- It operates as follows:
-- - It uses pattern matching to check the 'fieldIndex' of the 'Model'.
-- - Depending on the index value:
-- - If the index is 0, it returns a divergence value of 2.
-- - If the index is 1, it returns a divergence value of -2.
-- - If the index is 2, it returns a divergence value of 0.
-- - For any other index value, it returns 0.

calculateDivergence :: Model -> Float
calculateDivergence model
    | fieldIndex model == 0 = 2
    | fieldIndex model == 1 = -2
    | fieldIndex model == 2 = 0
    | otherwise = 0

-- The 'fieldGrid' variable represents a grid of coordinates in the plane.
-- Each coordinate is represented as a tuple of two 'Float' values (x, y).
-- It operates as follows:
-- - It uses list comprehension to generate a grid of coordinates within the range [-1, 1] in both x and y directions.
-- - The step size between consecutive coordinates is 0.1 in both dimensions, resulting in a dense grid.

fieldGrid :: [(Float, Float)]
fieldGrid = [(x, y) | x <- [-1, -0.9 .. 1], y <- [-1, -0.9 .. 1]]
