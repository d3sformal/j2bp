package paper;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;


public class Scheduler
{
	public static void main(String[] args)
	{
		Map<Integer,ThreadInfo> id2thread = new HashMap<Integer,ThreadInfo>();

		Set<Integer> active = new HashSet<Integer>();


		// initialize map with data for several threads
		
		ThreadInfo th1 = new ThreadInfo(1,5);
		id2thread.put(1, th1);
		
		ThreadInfo th2 = new ThreadInfo(2,18);
		id2thread.put(2, th2);

		ThreadInfo th3 = new ThreadInfo(3,10);
		id2thread.put(3, th3);


		// some threads are put into the active state
		
		active.add(2);
		active.add(3);


		// compute order in which threads should run
		
		List<Integer> schedule = new LinkedList<Integer>();

		int actID = 0;
		ThreadInfo actTh = null;

		int i = 0;
		ThreadInfo schTh = null;

		// set of active threads is iterated when to make scheduling decisions
		Iterator<Integer> actIt = active.iterator();
		while (actIt.hasNext())
		{
			actID = actIt.next();

			// the info object is retrieved for each active thread
			actTh = id2thread.get(actID);

			// read the field "priority"
			// property: variable "actTh" cannot be null for any active thread ID
			// property: variable "schTh" cannot be null
			for (i = 0; i < schedule.size(); i++)
			{
				schTh = id2thread.get(schedule.get(i));
				if (actTh.priority > schTh.priority)
				{
					schedule.add(i, actID);
					break;
				}
			}
		}
	}
}

class ThreadInfo
{
	public int id;
	public int priority;

	public ThreadInfo(int id_, int prio_)
	{
		id = id_;
		priority = prio_;
	}
}
