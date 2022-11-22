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
|______________________|
``` 

More specifically, it'll be 2 10x10 grids where one grid represents the player's board and the other represents the coordinates the player has guessed. On the bottom, we'll have a prompt that notifies the player if we need any inputs from them.

If there's time, then we'll extended the prompt to also support chatting or reduce the area of the prompt and add support for data about the state of the game (e.g. the status of ships).

## Project Updates (11/23/2022)

### Updates

1. Networking: We began with learning and implementing a simple server-client program in Haskell. Specifically, we referred to [this article](https://wiki.haskell.org/Implement_a_chat_server) to understand network and socket programming. Since we decided to not have a centralized server which saves the game state for both the players and interacts with the players machines to determine the progression of the game, we modified the implementation such that both ends would act as servers and clients. On any event, a message is sent to the opponent - awaits a response and updates the state locally.

2. State Upates with Network Events: Our project involves updating the UI and state based on events received from the network. In order to learn further on this, we decided to build a simple networked application - which has a small colored square rendered on the terminal, pressing the return/enter button should send a message to the other node and a node receiving a message would toggle the color of the square displayed. We plan to build on this simple application further to complete our project.

### Challenges

TODO

### Redefined Goals

No changes to our proposed project have been planned based on our progress on the project.

### Architecture

We have modelled the game as a state machine with events causing transitions between the state. The state diagram can be seen as below:

TODO - add image.