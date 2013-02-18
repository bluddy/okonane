package edu.jhu.jgettinger.konane;


/**
 * Move
 * <br>
 * A generic konane move, from (x1,y1) to (x2,y2)
 *
 * @author James Gettinger
 *
 */
public class Move {
	private int x1, x2, y1, y2;
	
	public Move(int x1, int x2, int y1, int y2){
		this.x1 = x1;
		this.x2 = x2;
		this.y1 = y1;
		this.y2 = y2;
	}

	public int getX1() {
		return x1;
	}

	public void setX1(int x1) {
		this.x1 = x1;
	}

	public int getX2() {
		return x2;
	}

	public void setX2(int x2) {
		this.x2 = x2;
	}

	public int getY1() {
		return y1;
	}

	public void setY1(int y1) {
		this.y1 = y1;
	}

	public int getY2() {
		return y2;
	}

	public void setY2(int y2) {
		this.y2 = y2;
	}
	
	public String toString(){
		return x1 + " " + y1 + " " + x2 + " " + y2;
	}
	
	public boolean equals(Object o){
		Move op = (Move) o;
		boolean retval = false;
		
		if(op.getX1() == x1 && op.getY1() == y1 && op.getX2() == x2 && op.getY2() == y2){
			retval = true;
		}
		
		return retval;
	}
}
