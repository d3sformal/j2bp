package dillig;

import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class Singleton
{
	private Set<Point1> points;
	
	
	public Singleton()
	{
		points = new HashSet<Point1>();
	}

	public Point1 getShared(Point1 p)
	{
		Point1 existing = null;
		
		Iterator<Point1> it = points.iterator();
		
		Point1 cur;
		
		while (it.hasNext())
		{
			cur = it.next();
			
			if (cur.equals(p))
			{
				existing = cur;
				break;
			}
		}
			
		if (existing == null)
		{
			points.add(p);
			return p;
		}
		else
		{
			return existing;
		}
	}

	public void checkCorrectness(int x, int y)
	{
		Point1 p = new Point1(x, y);
		Point1 shared_p = getShared(p);
		
		int x2;
		int y2;
		
		if (shared_p != p) 
		{
			x2 = shared_p.getX();
			y2 = shared_p.getY();
			
			if (x2 != x) Verify.assertTrue("shared_p.getX() != x", false);
			if (y2 != y) Verify.assertTrue("shared_p.getY() != y", false);
		}
	}

	public static void main(String[] args)
	{
		Singleton sig = new Singleton();
	
		sig.checkCorrectness(1,2);
	}	
}


class Point1
{
	private int x;
	private int y;
	

	public Point1(int _x, int _y)
	{
		this.x = _x;
		this.y = _y;
	}
	
	public int getX()
	{
		return x;
	}
	
	public int getY()
	{
		return y;
	}

	public boolean equals(Object obj)
	{
		if (obj == null) return false;
		
		if ( ! (obj instanceof Point1) ) return false;
		
		Point1 p = (Point1) obj;
		
		if ((this.x == p.x) && (this.y == p.y)) return true;
		else return false;		
	}
}

