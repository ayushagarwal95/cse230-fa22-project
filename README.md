# cse230-fa22-project

Team Members: 
1. Brian Li
2. Aditya Jeswani
3. Prerna Bhavsar
4. Ayush Agarwal

## Project Proposal

### Proposal
For our project, we're proposing to implement a networked two-player game: Battleship. For simplicity, we'll be following standard rules and layout of the game:

1) 10 x 10 game board 
2) 5 ships: 1 (1x5), 1 (1x4), 2 (1x3), 1 (1x2)
3) Players takes turns guessing the positions of the opponents ships
4) First player to destroy all of the opponent's ships wins 

### Core Components
As a preliminary overview of the project, we decomposed the the project into 4 core components:

1) State Charts
2) Networking
3) Game Logic
4) Brick TUI

In the following section, we'll discuss a simplified overview of each component and their purpose.

### State Charts
To facilitate SRP (single-responsibility principle), the core of our program will be a state chart / state machine with the responsibility of ensuring consistency across both player as well as integrity.

On the top level, there are four states: 
```
<Start>, <Attacking>, <Waiting>, <Terminal>
```

Each top-level state is composed of its own state machine, driving the progression of the game. For example, if the player is in an attacking state, the state machine might look as follows:
```
<Refresh display (Ask for input)> -> 
<Choose valid coordinate> -> 
<Wait for response from opponent> -> 
<Update game state> ->
<Refresh display (Update grid)> -> 
<Terminal State (Here, it'll be asking game logic if we won)>
```
Each one of these nodes trigger an "action" of another component when entered. For example, choosing a valid coordinate can be handled by the game logic. The one exception to this rule are the terminal states, which needs to decide which top-level state to transition to.

### Networking
GRPC will probably be the easiest way to handle this and there should be libraries that are readily available.

### Game Logic
At its core, this component's responsibility is to assure the integrity of moves and to manage the game state. To fulfill this, it need to remember 5 pieces of information:

1) Where our ships are placed
2) Ship coordinates that have been attacked
3) All coordinates that we have chosen 
4) Which coordinates that we've chosen were hits
5) Which ships were completely destroyed

Everything else can be computed as the function of the information above.

### Brick TUI
We'll be using Brick for the TUI. Currently, our plan is to have the following layout:
```
 __________  __________
|          ||          |
|          ||          |
|          ||          |
|__________||__________|
|__________||__________|
``` 

More specifically, it'll be 2 10x10 grids where one grid represents the player's board and the other represents the coordinates the player has guessed. On the bottom, we'll have a prompt that notifies the player if we need any inputs from them.

If there's time, then we'll extended the prompt to also support chatting or reduce the area of the prompt and add support for data about the state of the game (e.g. the status of ships).

## Project Updates (11/23/2022)

### Update
For our update, we focused on constructing a simple test program that utilizes Brick UI, Brick events, State Management, and Networking. We intend to use the lessons and structures created as the framework on which our Battleship application will be built on. 

Our sample application consists of two states: A and B. Each of which will have a different rendering. To transition between the states, the process must receive a "flip" signal from another paired process running the same application. In other words, a user interacting with the application controls another user's state on a different terminal. As a visualization, this is the corresponding state machine:

```
State Machine:
┌───┐            ┌───┐ 
│ A │  <──────>  │ B │  
└───┘            └───┘

Events:
A `EnterKey` -> A (Send Flip to Paired Process)
B `EnterKey` -> B (Send Flip to Paired Process)
A `Flip`     -> B 
B `Flip`     -> A 
```

Lastly, since it can be implemented independently of all other modules, we've also completed the Game Logic portion of the project (i.e. tracking targeted coordinates, ...).

### Learnings
#### 1) Networking
In order to implement the state transitions, we needed some support for networking. Namely, each process should be able to listen in the background for incoming requests. Likewise, our event handling should support sending networked requests to other processes. 

To allow for listening in the background, we discovered `forkIO` which can be treated semantically as similar to a go-routine. Next, perform impure actions (e.g. network requests) outside of main, we need can leverage `liftIO` from types that are instances of `MonadIO`. 

Lastly, to easily implement a TCP channel between two processes, we leveraged the `Network.TCP.Simple` module.

#### 2) State Machine
For the most part, implementing a simple State Machine is relatively easy. However, due to properties of Haskell as well as how Brick works, it became apparent that states should be broken up by "incoming" events. In other words, each important processing of an event should have an associated state. 

As an example, we'll looking into the Attacking state. As mentioned in our proposal, it can be broken down into the following operations:
1) Validate coordinate
2) Notify opponent (Send Network Event)
3) Process response from opponent (Hit/Miss)
4) Transition to Waiting

Here, we can see that there's an incoming event in this chain of actions. As such, we should split 3) and 4) into a separate state that 1) and 2) transitions into. 

#### 3) Brick
The primary learning for Brick was simply getting a feel for how the Brick module works. This includes understanding the layout of Widgets, combining Widgets into a "complex" display, and associating attributes to widgets for styling.

### Challenges

The main challenges for the update were:
1) Understanding how to execute impure functions outside of `main`
2) Designing an architecture that has enough abstractions to avoid tight coupling of modules

### Redefined Goals

No changes to our proposed project have been planned based on our progress on the project.

### Architecture
For our architecture, we've arranged our application into a layered stack where each layer can only depend on modules in layers below. The first layer is comprised of solely `Main.hs`. The second layer contains all of the relevant Brick logics: `UI.hs` and `Control.hs`. The third layer contains state management logic: `State.hs` and `Transition.hs`. Lastly, the fourth layer contains game logic and networking: `Game.hs` and `Networking.hs`. A visualization of the layer is shown in the following:
```
   ┌─────────┐ 
1) │ Main.hs │ 
   └─────────┘ 
   ┌───────┐ ┌────────────┐  
2) │ UI.hs │ │ Control.hs │ 
   └───────┘ └────────────┘    
   ┌──────────┐ ┌───────────────┐
3) │ State.hs │ │ Transition.hs │
   └──────────┘ └───────────────┘ 
   ┌─────────┐ ┌───────────────┐ 
4) │ Game.hs │ │ Networking.hs │ 
   └─────────┘ └───────────────┘ 
```

By following this architecture, we can parallelize contributions while limiting discussions to only the exposed interface between layers. This idea of layered ownership is also why we isolated state management from Brick (i.e. `handleEvent` is simply a mapping function).

We have also modelled the game as a state machine with events causing transitions between the state. The state diagram can be seen as below:

```
Player 1:

┌────────┐          
│ Setup1 │          
└────────┘          
    │
    ▽
┌───────┐          
│ Start │          
└───────┘          
    │
    ▽
┌───────────┐                         ┌─────────┐          
│ Attacking │ ◁────────────────────── │ Waiting │          
└───────────┘                         └─────────┘          
       │                               △   │
       │       ┌────────────┐          │   │ 
       └─────▷ │ AttackWait │ ─────────┘   │
               └────────────┘              │ 
                     │                     │
                     ▽                     ▽
                  ┌─────┐               ┌──────┐          
                  │ Win │               │ Lose │          
                  └─────┘               └──────┘          
```

```
Player 2:

┌────────┐          
│ Setup2 │          
└────────┘          
    │
    ▽
┌───────────┐          
│ SetupWait │          
└───────────┘          
    │
    ▽
┌─────────┐                         ┌───────────┐          
│ Waiting │ ──────────────────────▷ │ Attacking │          
└─────────┘                         └───────────┘
    │  △                               │   
    │  │       ┌────────────┐          │    
    │  └────── │ AttackWait │ ◁────────┘   
    │          └────────────┘               
    │                │                     
    ▽                ▽                     
 ┌──────┐         ┌─────┐               
 │ Lose │         │ Win │               
 └──────┘         └─────┘               

```

A more in-depth diagram with edge labels can be found at: https://lucid.app/lucidchart/d4f8f0cc-cc6b-45c4-9198-483d9e65fb62/edit?viewport_loc=-106%2C14%2C2303%2C1518%2C0_0&invitationId=inv_b122c06a-b8c8-4661-9ba8-552a0eb94c85


┌────────────────┐                 ┌────────────────┐            
│ ┌─────┐┌─────┐ │                 │ ┌─────┐┌─────┐ │  
│ │     ││     │ │                 │ │     ││     │ │
│ └─────┘└─────┘ │   <─────────>   │ └─────┘└─────┘ │      
│ ┌─────┐┌─────┐ │                 │ ┌─────┐┌─────┐ │   
│ └─────┘└─────┘ │                 │ └─────┘└─────┘ │       
└────────────────┘                 └────────────────┘
        P1                                 P2
