package edu.jhu.jgettinger.konane.player;

import java.util.ArrayList;

import edu.jhu.jgettinger.konane.Board;
import edu.jhu.jgettinger.konane.Move;

/**
 * Player <br>
 * A generic player of the konane game
 * 
 * @author James Gettinger
 * 
 */
public abstract class Player {

	private boolean color;
	/**
	 * Default constructor
	 * 
	 * @param color
	 *            the player's color
	 */
	public Player(boolean color) {
		this.color = color;
	}

	/**
	 * Function to prompt the player to make the first move of the game
	 * 
	 * @param b
	 *            the board to parse
	 * @return the first move to be made
	 */
	public Move getFirstMove(Board b) {
		return null;
	}

	/**
	 * Function to prompt the player to make the second move of the game
	 * 
	 * @param b
	 *            the board to parse
	 * @param x
	 *            the x-coordinate of black's first move
	 * @param y
	 *            the y-coordinate of black's first move
	 * 
	 * @return the second move to be made
	 */
	public Move getSecondMove(Board b, int x, int y) {
		return null;
	}

	/**
	 * Function to prompt the player to make a move after the first move has
	 * already been made
	 * 
	 * @param b
	 *            the board to parse
	 * @return the selected move
	 */
	public Move getNextMove(Board b) {
		return null;
	}

	public boolean getColor() {
		return color;
	}

	public void setColor(boolean color) {
		this.color = color;
	}
}
