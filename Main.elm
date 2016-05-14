module Main (..) where

import Graphics.Element exposing (Element, layers, container, Position, midLeftAt, absolute, relative, leftAligned)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3, getY, toTuple, add, toRecord, i, j, k, scale)
import Math.Matrix4 exposing (makePerspective, mul, makeLookAt, makeRotate, transform, Mat4)
import Task
import Text
import Time
import WebGL
import Keyboard
import Window
import StartApp
import Effects
import Html


-- MODEL


{-| This is the application's Person data structure
-}
type alias Person =
  { position : Vec3
  , velocity : Vec3
  }


{-| This is the applications's Model data structure
-}
type alias Model =
  { person : Person
  , window : Maybe ( Int, Int )
  , texture : Maybe WebGL.Texture
  }


{-| Constant definition of eye level, i.e. where the viewer is looking from relative to the 'ground'
-}
eyeLevel : Float
eyeLevel =
  2



-- INIT


{-| When the application first starts, this is initial state of the Model
-}
initModel : Model
initModel =
  { person =
      { position = vec3 0 eyeLevel -10
      , velocity = vec3 0 0 0
      }
  , window = Nothing
  , texture = Nothing
  }


{-| initial `Effect`s to be processed
-}
initTextures : Effects.Effects Action
initTextures =
  WebGL.loadTexture "woodCrate.jpg"
    |> Task.toMaybe
    |> Task.map TextureLoaded
    |> Effects.task



-- ACTIONS


{-| Every half a second there's an event coming through;
these are all the valid actions we could receive.
# Move - the user is trying to jump using the space key, move using the arrow keys,
or the window is being resized
# TextureLoaded - a texture has been loaded across the wire
-}
type Action
  = Move { dimensions : ( Int, Int ), isJumping : Bool, direction : { x : Int, y : Int }, dt : Float }
  | TextureLoaded (Maybe WebGL.Texture)



-- INPUTS


{-| Translate raw incoming events into `Action`s.

The approach taken here is to sample the different streams regularly and act accordingly.

This is an easy approach to take, but could result in lost signals,
if the key presses are very short and sharp, or if the system is lagging.

A better approach might be to do that sampling, but also accept other inputs
when they come in. This would probably require some interpolation to work out
the correct response.

-}
{-| A signal of Move actions, derived from relevant keyboard signals and window resize signals
-}
keyboard : Signal Action
keyboard =
  let
    dt =
      Signal.map (\t -> t / 500) (Time.fps 25)
  in
    Signal.map4 (\s a w d -> Move { isJumping = s, direction = a, dimensions = w, dt = d }) Keyboard.space Keyboard.arrows Window.dimensions dt
      |> Signal.sampleOn dt



-- UPDATE


{-| update the Model from incoming Actions
-}
update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  --(model, Effects.none)
  let
    newModel =
      case action of
        Move { isJumping, direction, dimensions, dt } ->
          let
            newPerson =
              model.person
                |> walk direction
                |> jump isJumping
                |> gravity dt
                |> physics dt
          in
            { model | person = newPerson, window = Just dimensions }

        TextureLoaded texture ->
          { model | texture = texture }
  in
    ( newModel, Effects.none )


{-| Work out where a person has moved to, and how fast they're now moving, after walking in a given direction
-}
walk : { x : Int, y : Int } -> Person -> Person
walk directions person =
  if getY person.position > eyeLevel then
    person
  else
    let
      vx =
        toFloat -directions.x

      vz =
        toFloat directions.y
    in
      { person
        | velocity = vec3 vx (getY person.velocity) vz
      }


{-| Work out how high a person is, and how fast they're currently jumping (or falling)
-}
jump : Bool -> Person -> Person
jump isJumping person =
  if not isJumping || getY person.position > eyeLevel then
    person
  else
    let
      ( vx, _, vz ) =
        toTuple person.velocity
    in
      { person
        | velocity = vec3 vx 2 vz
      }


{-| Apply the effects of gravity
-}
gravity : Float -> Person -> Person
gravity dt person =
  if getY person.position <= eyeLevel then
    person
  else
    let
      v =
        toRecord person.velocity
    in
      { person
        | velocity = vec3 v.x (v.y - 2 * dt) v.z
      }


{-| Apply the effects of acceleration or deceleration
-}
physics : Float -> Person -> Person
physics dt person =
  let
    position =
      person.position `add` scale dt person.velocity

    ( x, y, z ) =
      toTuple position
  in
    { person
      | position =
          if y < eyeLevel then
            vec3 x eyeLevel z
          else
            position
    }



-- VIEW


{-| Define the mesh for a crate
-}
type alias Vertex =
  { position : Vec3
  , coord : Vec3
  }


{-| generate a View from a Model
-}
view : Signal.Address Action -> Model -> Html.Html
view address { person, window, texture } =
  Html.fromElement
    <| case ( person, window, texture ) of
        ( _, Nothing, _ ) ->
          message

        ( _, _, Nothing ) ->
          message

        ( person, Just ( w, h ), Just texture ) ->
          let
            entity =
              world texture (perspective ( w, h ) person)
          in
            layers
              [ WebGL.webgl ( w, h ) [ entity ]
              , container w 100 position message
              ]


{-| Render the visible world
-}
world : WebGL.Texture -> Mat4 -> WebGL.Renderable
world tex perspective =
  WebGL.render vertexShader fragmentShader crate { crate = tex, perspective = perspective }


{-| Calculate the viewers viewpoint
-}
perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
  mul
    (makePerspective 45 (toFloat w / toFloat h) 1.0e-2 100)
    (makeLookAt person.position (person.position `add` k) j)


{-| Describes the initial (actually static) state of the cube in the scene
-}
crate : WebGL.Drawable Vertex
crate =
  WebGL.Triangle (List.concatMap rotatedFace [ ( 0, 0 ), ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, -90 ) ])


{-| Constant function describing the initial position of the viewer
-}
position : Position
position =
  midLeftAt (absolute 40) (relative 0.5)


{-| Rotate a cube face
-}
rotatedFace : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedFace ( angleXZ, angleYZ ) =
  let
    x =
      makeRotate (degrees angleXZ) j

    y =
      makeRotate (degrees angleYZ) i

    t =
      x `mul` y

    each f ( a, b, c ) =
      ( f a, f b, f c )
  in
    List.map (each (\v -> { v | position = transform t v.position })) face


{-| Constant function describing the faces of a generic cube
-}
face : List ( Vertex, Vertex, Vertex )
face =
  let
    topLeft =
      Vertex (vec3 -1 1 1) (vec3 0 1 0)

    topRight =
      Vertex (vec3 1 1 1) (vec3 1 1 0)

    bottomLeft =
      Vertex (vec3 -1 -1 1) (vec3 0 0 0)

    bottomRight =
      Vertex (vec3 1 -1 1) (vec3 1 0 0)
  in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]


{-| Helper text displayed at the top left of the window
-}
message : Element
message =
  leftAligned
    <| Text.monospace
    <| Text.fromString
    <| "Walk around with a first person perspective.\n"
    ++ "Arrows keys to move, space bar to jump."



-- Shaders


{-| Vertex shader
-}
vertexShader : WebGL.Shader { position : Vec3, coord : Vec3 } { u | perspective : Mat4 } { vcoord : Vec2 }
vertexShader =
  [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]


{-| Fragment shader
-}
fragmentShader : WebGL.Shader {} { u | crate : WebGL.Texture } { vcoord : Vec2 }
fragmentShader =
  [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]



-- MAIN


{-| The StartApp `app` function
-}
app : StartApp.App Model
app =
  StartApp.start
    { init = ( initModel, initTextures )
    , update = update
    , view = view
    , inputs = [ keyboard ]
    }


{-| The Elm required `main` function
It's a bit unfortunate that you can only have Html.Html returned from the
StartApp.App record, and therefore from our view function.
-}
main : Signal Html.Html
main =
  app.html


{-| Port for processing `Task`s. The only tasks being generated in this app
are from the initial fetch of the crate texture.
-}
port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
