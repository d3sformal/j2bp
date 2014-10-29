package paper;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;


public class Image
{
	public List<Rectangle> rectangles;
	public int[][] pixels;

	
	public static void main(String[] args)
	{
		Image img = new Image();

		img.load();
		img.render();
		
		// displayScreen();		
	}
	
	public void load()
	{
		rectangles = new ArrayList<Rectangle>();
		pixels = new int[50][50];
		
		Rectangle r = null;
		
		r = new Rectangle();
		r.top = 10;
		r.left = 10;
		r.right = 30;
		r.bottom = 15;
		r.color = 2;
		rectangles.add(r);
		
		r = new Rectangle();
		r.top = 5;
		r.left = 5;
		r.right = 25;
		r.bottom = 25;
		r.color = 3;
		rectangles.add(r);
	}

	public void render()
	{
		Rectangle rec = null;		
		int i,j;

		Iterator<Rectangle> recIt = rectangles.iterator();
		
		// loop over all rectangles and draw them
		while (recIt.hasNext())
		{
			rec = recIt.next();
		
			// change relevant pixels to rectangle color
			for (i = rec.left; i <= rec.right; i++)
			{
				for (j = rec.top; j <= rec.bottom; j++)
				{
					pixels[i][j] = rec.color;
				}
			}
		}
	}
}

class Rectangle
{
	public int top,left,right,bottom;
	public int color;
}

