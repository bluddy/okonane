package edu.jhu.jgettinger.konane;

/**
 * Board
 * <br>
 * Representation of a konane game board.
 * <br>
 * Defaults to a 4x4 board. 
 * <br>
 * 
 * @author James Gettinger
 *
 */

import edu.jhu.jgettinger.konane.player.Player;
import java.util.ArrayList;

public class Board {
	protected static final int MIN_SIZE = 4;
	protected static final int MAX_SIZE = 8;
	
	protected Move last = null;
	protected Tile[][] tiles;
	
	private int size;
	private int moveNum = 1;


	/**
	 * Creates a board with the inputed tiles.
	 * 
	 * @param size
	 *            the size of the board
	 * 
	 */
	public Board(int size) {
		this.size = size;
		tiles = new Tile[size][size];

		if (size >= MIN_SIZE && size <= MAX_SIZE && size % 2 == 0) {
			for (int y = 0; y < size; y++) {
				for (int x = 0; x < size; x++) {
					if ((x % 2 == 0 && y % 2 == 0)
							|| (x % 2 != 0 && y % 2 != 0)) {
						tiles[x][y] = new Tile(Tile.BLACK, true);
					} else {
						tiles[x][y] = new Tile(Tile.WHITE, true);
					}
				}
			}

		} else {
			System.out.println("Board size must be an even number between "
					+ MIN_SIZE + " and " + MAX_SIZE + " inclusively");
		}
	}

	/**
	 * Returns a specific tile based on its x,y coordinates
	 * 
	 * @param x
	 *            the x-coordinate
	 * @param y
	 *            the y-coordinate
	 * @return tile
	 */
	public Tile getTile(int x, int y) {
		return tiles[x][y];
	}

	/**
	 * Returns a string representation of the board
	 * 
	 * @return a string representation
	 */
	public String toString() {
		String toret = " ";

		for (int x = 0; x < size; x++) {
			toret += "|" + x;

			if (x == size - 1) {
				toret += "|";
			}
		}
		toret += "\n";

		for (int y = 0; y < size; y++) {
			toret += y;
			for (int x = 0; x < size; x++) {
				toret += "|" + tiles[x][y];

				if (x == size - 1) {
					toret += "|";
				}
			}

			toret += "\n";
		}

		return toret;
	}

	/**
	 * checks to see if the board has an valid moves
	 * 
	 * @return true if the game is over
	 */
	public boolean endCase(Player mover) {
		ArrayList<Move> allMoves = getMoves(mover.getColor(), moveNum);

		boolean retval = false;
		if (allMoves.size() == 0) {
			retval = true;
		}

		return retval;
	}

	/**
	 * get all the possible moves after a defined number of moves are made
	 * 
	 *  @param color
	 *            the player to move's color
	 * @param moves
	 *            the moves to make
	 * @return the moves
	 */
	public ArrayList<Move> getMovesAfter(boolean color, int moveNumber,
			ArrayList<Move> moves) {
		Move tempMove = last;
		int tempMoveNum = moveNum;
		
		Tile[][] temp = new Tile[size][size];
		for (int y = 0; y < size; y++) {
			for (int x = 0; x < size; x++) {
				temp[x][y] = new Tile(tiles[x][y].getColor(), tiles[x][y].isOccupied());
			}
		}

		resetOccupied();
		
		if(moves.size() > 0){
			last = moves.get(moves.size() -1);
		}
		
		boolean simColor = color;
		if(moves.size() % 2 != 0){
			simColor = !simColor;
		}
		
		for (int i = 0; i < moves.size(); i++) {
			makeMove(moves.get(i), (moveNumber - moves.size()) + i, simColor, true);
			simColor = !simColor;
		}
		


		ArrayList<Move> toret = getMoves(color, moveNumber);

		for (int y = 0; y < size; y++) {
			for (int x = 0; x < size; x++) {
				tiles[x][y] = temp[x][y];
			}
		}

		last = tempMove;
		moveNum = tempMoveNum;
		restoreOccupied();
		
		return toret;
	}
	
	/**
	 * gets all possible moves
	 * 
	 * @return the moves
	 */
	public ArrayList<Move> getMoves(boolean color, int moveNumber) {
		ArrayList<Move> moves = new ArrayList<Move>();
		if (moveNumber <= 2) {
			for (int y2 = 0; y2 < size; y2++) {
				for (int x2 = 0; x2 < size; x2++) {
					Move temp = new Move(-1, x2, -1, y2);
					if (makeMove(temp, moveNumber, color, false)) {
						moves.add(temp);
					}
				}
			}
		} else {
			for (int y1 = 0; y1 < size; y1++) {
				for (int x1 = 0; x1 < size; x1++) {
					for (int y2 = 0; y2 < size; y2++) {
						for (int x2 = 0; x2 < size; x2++) {
							Move temp = new Move(x1, x2, y1, y2);
							if (makeMove(temp, moveNumber, color, false)) {
								moves.add(temp);
							}
						}
					}
				}
			}
		}

		return moves;
	}

	/**
	 * Checks to see if a move is valid.
	 * 
	 * @param m
	 *            the move to make
	 * @param mover
	 *            the player who is moving
	 * @param make
	 *            if true the move is made
	 * @return true if valid
	 */
	public boolean makeMove(Move m, int moveNumber, boolean color, boolean make) {
		int x1 = m.getX1();
		int x2 = m.getX2();
		int y1 = m.getY1();
		int y2 = m.getY2();

		boolean valid = true;
		boolean toret = false;
		boolean foundLanding = false;

		if (moveNumber <= 2) {
			if (y2 < size && y2 >= 0 && x2 < size && x2 >= 0
					&& tiles[x2][y2].getColor() == color) {
				if (moveNumber == 1) {
					if (x2 == 0 && y2 == 0) {
						toret = true;
					} else if (x2 == 0 && y2 == size - 1) {
						toret = true;
					} else if (x2 == (size / 2) - 1 && y2 == (size / 2) - 1) {
						toret = true;
					} else if (x2 == (size / 2) && y2 == (size / 2) - 1) {
						toret = true;
					} else if (x2 == (size / 2) - 1 && y2 == (size / 2)) {
						toret = true;
					} else if (x2 == (size / 2) && y2 == (size / 2)) {
						toret = true;
					} else if (x2 == size - 1 && y2 == 0) {
						toret = true;
					} else if (x2 == size - 1 && y2 == size - 1) {
						toret = true;
					}
				} else {

					int x = last.getX2();
					int y = last.getY2();

					if (x2 == x && y2 == y - 1) {
						toret = true;
					} else if (x2 == x - 1 && y2 == y) {
						toret = true;
					} else if (x2 == x + 1 && y2 == y) {
						toret = true;
					} else if (x2 == x && y2 == y + 1) {
						toret = true;
					}
				}
			}

			if (toret && make) {
				setOccupied(x2, y2, false);
			}
		} else {
			if (x1 < size && y1 < size && x2 < size && y2 < size && x1 >= 0
					&& y1 >= 0 && x2 >= 0 && y2 >= 0) {
				if (tiles[x1][y1].isOccupied()
						&& tiles[x1][y1].getColor() == color) {
					if (x1 == x2) {
						if (y2 < y1) {
							for (int i = 1; i <= y1 - y2; i += 1) {
								if (i % 2 == 0) {
									if (tiles[x1][y1 - i].isOccupied()) {
										valid = false;
										break;
									} else {
										foundLanding = true;
									}
								} else {
									if (!tiles[x1][y1 - i].isOccupied()
											|| tiles[x1][y1 - i].getColor() == color) {
										valid = false;
										break;
									}
								}
							}

							if (valid && foundLanding && (y1 - y2) % 2 == 0) {
								toret = true;

								if (make) {
									for (int i = 1; i <= y1 - y2; i += 2) {
										setOccupied(x1, y1-i, false);
									}
								}
							}
						} else if (y2 > y1) {
							for (int i = 1; i <= y2 - y1; i += 1) {
								if (i % 2 == 0) {
									if (tiles[x1][y1 + i].isOccupied()) {
										valid = false;
										break;
									} else {
										foundLanding = true;
									}
								} else {
									if (!tiles[x1][y1 + i].isOccupied()
											|| tiles[x1][y1 + i].getColor() == color) {
										valid = false;
										break;
									}
								}
							}

							if (valid && foundLanding && (y2 - y1) % 2 == 0) {
								toret = true;

								if (make) {
									for (int i = 1; i <= y2 - y1; i += 2) {
										setOccupied(x1, y1+i, false);
									}
								}
							}
						} else {
							valid = false;
						}
					} else if (y1 == y2) {
						if (x2 < x1) {
							for (int i = 1; i <= x1 - x2; i += 1) {
								if (i % 2 == 0) {
									if (tiles[x1 - i][y1].isOccupied()) {
										valid = false;
										break;
									} else {
										foundLanding = true;
									}
								} else {
									if (!tiles[x1 - i][y1].isOccupied()
											|| tiles[x1 - i][y1].getColor() == color) {
										valid = false;
										break;
									}
								}
							}

							if (valid && foundLanding && (x1 - x2) % 2 == 0) {
								toret = true;

								if (make) {
									for (int i = 1; i <= x1 - x2; i += 2) {
										setOccupied(x1-i, y1, false);
									}
								}
							}
						} else if (x2 > x1) {
							for (int i = 1; i <= x2 - x1; i += 1) {
								if (i % 2 == 0) {
									if (tiles[x1 + i][y1].isOccupied()) {
										valid = false;
										break;
									} else {
										foundLanding = true;
									}
								} else {
									if (!tiles[x1 + i][y1].isOccupied()
											|| tiles[x1 + i][y1].getColor() == color) {
										valid = false;
										break;
									}
								}
							}

							if (valid && foundLanding && (x2 - x1) % 2 == 0) {
								toret = true;

								if (make) {
									for (int i = 1; i <= x2 - x1; i += 2) {
										setOccupied(x1+i, y1, false);
									}
								}
							}
						} else {
							valid = false;
						}
					}
				}
			}

			if (toret && make) {
				setOccupied(x1, y1, false);
				tiles[x2][y2].setColor(tiles[x1][y1].getColor());
				setOccupied(x2, y2, true);
				//System.out.println(x2 + " " + y2);
			}
		}

		if (toret && make) {
			last = m;
			moveNum++;
		}

		return toret;
	}

	/**
	 * Sets a square occupied
	 * 
	 * @param x the x coordinate
	 * @param y the y coordinate
	 * @param occ occupied or not
	 */
	protected void setOccupied(int x, int y, boolean occ){
		tiles[x][y].setOccupied(occ);
	}
	
	/**
	 * Reset the occupied list
	 */
	protected void resetOccupied(){
		return;
	}
	
	/**
	 * Restore the occupied list
	 */
	protected void restoreOccupied(){
		return;
	}
	
	public static void main(String args[]) {
		Board b = new Board(4);
		System.out.println(b);
		b = new Board(6);
		System.out.println(b);
		b = new Board(5);
	}

	public int getSize() {
		return size;
	}

	public void setSize(int size) {
		this.size = size;
	}

	public int getMoveNum() {
		return moveNum;
	}
}
