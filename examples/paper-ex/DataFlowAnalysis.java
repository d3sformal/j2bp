package paper;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;


public class DataFlowAnalysis
{
	public static void main(String[] args)
	{
		// map from cfg node id to successor ids
		Map<Integer, int[]> cfg = new HashMap<Integer, int[]>();

		// initialize control flow graph
		
		int[] succ = null;

		succ = new int[1];
		succ[0] = 2;
		cfg.put(1, succ);
	
		succ = new int[2];
		succ[0] = 3;
		succ[1] = 4;
		cfg.put(2, succ);
		
		succ = new int[1];
		succ[0] = 5;
		cfg.put(3, succ);

		succ = new int[1];
		succ[0] = 5;
		cfg.put(4, succ);

		succ = new int[0];
		cfg.put(5, succ);


		Map<Integer, Set<Integer>> cfnode2facts = new HashMap<Integer, Set<Integer>>();

		int cfnodeID = 0;

		Set<Integer> cfgNodes = cfg.keySet();		
		Iterator<Integer> cfgIt = cfgNodes.iterator();
		while (cfgIt.hasNext())
		{
			cfnodeID = cfgIt.next();
			cfnode2facts.put(cfnodeID, new HashSet<Integer>());
		}

		List<Integer> queue = new LinkedList<Integer>();

		// start with the entry node
		queue.add(1);

		Set<Integer> oldFacts = null;
		Set<Integer> newFacts = null;
		int i = 0;
		Iterator<Integer> it = null;

		while (queue.size() > 0)
		{
			cfnodeID = queue.get(0);
			queue.remove(0);
			
			oldFacts = cfnode2facts.get(cfnodeID);

			// update facts and store in the newFacts variable
			newFacts = new HashSet<Integer>();
			for (it = oldFacts.iterator(); it.hasNext(); ) newFacts.add(it.next());
			newFacts.add(queue.size() + 1);
			
			cfnode2facts.put(cfnodeID, newFacts);

			if ( ! oldFacts.equals(newFacts) ) 
			{
				// update queue based on CFG
				succ = cfg.get(cfnodeID);
				for (i = 0; i < succ.length; i++) queue.add(succ[i]);
			}
		}
	}
}

