package edu.jhu.jgettinger.konane;
/**
 * Tile
 * <br>
 * Representation of a konane tile. 
 * A tile is simply a container class for 
 * to tell if a board location contains a 
 * black, white or no rock. 

 * @author James Gettinger
 *
 */


public class Tile {
	public static final boolean BLACK = true;
	public static final boolean WHITE = false;
	
	private boolean color = BLACK;
	private boolean occupied = false;
	
	/**
   	 * Creates a tile
   	 *
   	 * @param color color to use
   	 * @param occupied if its occupied or not
   	*/
	public Tile(boolean color, boolean occupied){
		this.color = color;
		this.occupied = occupied;
	}

	public boolean getColor() {
		return color;
	}

	public void setColor(boolean color) {
		this.color = color;
	}

	public boolean isOccupied() {
		return occupied;
	}

	public void setOccupied(boolean occupied) {
		this.occupied = occupied;
	}
	
	public String toString(){
		String toret = ".";
		
		if(color == BLACK && occupied){
			toret = "b";
		}
		else if(color == WHITE && occupied){
			toret = "w";
		}
	
		return toret;
	}
}
