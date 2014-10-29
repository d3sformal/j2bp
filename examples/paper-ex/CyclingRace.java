package paper;

import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Collection;


public class CyclingRace
{
	public static void main(String[] args)
	{
		List<Cyclist> cyclists = new ArrayList<Cyclist>();
		
		Cyclist cl = null;
		
		// insert raw data for cyclists
		
		cl = new Cyclist();
		cl.idnum = 2;
		cl.time = 3725;
		cl.bonus = 5;
		cyclists.add(cl);
		
		cl = new Cyclist();
		cl.idnum = 56;
		cl.time = 3569;
		cl.bonus = 10;
		cyclists.add(cl);
		
		cl = new Cyclist();
		cl.idnum = 123;
		cl.time = 3766;
		cl.bonus = 50;
		cyclists.add(cl);
		
		
		TreeMap<Integer,Cyclist> results = new TreeMap<Integer,Cyclist>();
		
		int diff = 0;
		
		// compute results of the competition
		Iterator<Cyclist> cycIt = cyclists.iterator();
		while (cycIt.hasNext())
		{
			cl = cycIt.next();
			diff = cl.time - cl.bonus;
			results.put(diff, cl);
		}
		
		Collection<Cyclist> resCyclists = results.values();

		// print the best time and differences
		Iterator<Cyclist> resIt = resCyclists.iterator();
		Cyclist bestCL = resIt.next();
		int bestTime = bestCL.time - bestCL.bonus;
		// print(bestCL.id + " " + bestTime);		
		while (resIt.hasNext())
		{
			cl = resIt.next();
			diff = cl.time - cl.bonus - bestTime;
			// print(cl.id + " " + diff)
		}
	}
}

class Cyclist 
{
	public int idnum;
	public int time;
	public int bonus;
}
