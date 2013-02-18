==========================================
= Konane Scaffolding Code Implementation =
==========================================
Copyright 2010, James Gettinger
All rights reserved.


--------------------------------------------------------------------------------
1. Introduction & Usage Notes
--------------------------------------------------------------------------------

This scaffolding code was created for the Artificial Intelligence class
(EN.600.335/435) at Johns Hopkins University by James Gettinger.  It is
provided to students of that class to be used in their Konane minimax
implementations, and provides a framework that will allow students to
concentrate on the details of the minimax search algorithms, rather than the
rules of Konane.

This codebase is not complete; the implementation of a main() function is left
to students; it should contain user I/O code, as well as the main game loop.
The various gameplaying agents should extend the abstract Player class.

The Board, Move and Tile classes fully implement the rules of Konane; the
majority of the game logic is in the Borard class.  It should be sufficient to
directly provide a successor function and goal test to the search agents, as
well as check arbitrary moves for legality.  It also keep track of the board,
and maintain the current state of play for all agents.  The toString()
functions can be used for displaying the current state of play.


--------------------------------------------------------------------------------
2. Disclaimer
--------------------------------------------------------------------------------

This software was originally written only for the purpose of analyzing the
software engineering problems involved in producing an application of its
type.  It is not guaranteed in any sense to be suitable for any purpose and no
warranty, express or implied, is provided.  Use at your own risk.


--------------------------------------------------------------------------------
3. Terms of Use
--------------------------------------------------------------------------------

This software is only intended to be used by the members of the JHU Artificial
Intelligence class taught by Ben Mitchell.  Ben Mitchell has the right to
distribute and utilize the application for purposes related to teaching this
class.  Individuals to whom he distributes this software are permitted to use
it only for the purpose of generating data for the scientific paper
assignments provided in this class.  Any other use is expressly prohibited
without written consent of the author, James Gettinger

