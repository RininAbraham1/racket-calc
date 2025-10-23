\#Dev log

\##10/23/25

12:41 am: First I would need to create a prefix-notation calculator in racket with two modes, one interactive and the other batch using the mode.rkt. I would need to build a recursive parser that processes the expressions left to right and keep the history. For the first session try the create the project structure and history.

12:51 am: I have set up the repository, now i need to start working on the calculator, The main problem will be trying to parse prefix notation and managing the history. Plan right now is to create main.rkt and set up the loop and history. 

1:11 am: So i have implemented the basic structure and ran test cases for interactive mode and quitting. The next step is to start building the expression parser and handle really simple numbers like 7, Then add history references and then implement operators. I am currently testing directly from the ide's terminal but probably need to switch over to cmd for the final mode testing. 

1:33 am: Implemented the parse number function and tested, going to start working on the history and value functions. 

