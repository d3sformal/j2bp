package jpf;

import gov.nasa.jpf.JPF;
import gov.nasa.jpf.Config;
import gov.nasa.jpf.ListenerAdapter;
import gov.nasa.jpf.jvm.JVM;
import gov.nasa.jpf.search.Search;


/**
 * a listener to gracefully stop JPF after a specified amount of time 
 * and yet allow for the printing of statistics at the end of the search
 * use the property jpf.time_limit <time_in_seconds> to configure
 */
public class TimeConstrainedJPF extends ListenerAdapter 
{
	static long maxDuration = 0;
	static long startTime = 0;

	public TimeConstrainedJPF(Config cfg, JPF jpf)
	{
	}
	
	public void searchStarted(Search search)
	{
		JVM vm = search.getVM();
		Config config = vm.getConfig();
		
		this.startTime = System.currentTimeMillis();
		
		this.maxDuration = config.getInt("jpf.time_limit", -1);
		
		System.out.println("**** TIME BOUNDED SEARCH - LIMIT SET TO (SECONDS): " + maxDuration + " ****");
		
		this.maxDuration = this.maxDuration * 1000; // convert to milliseconds
	}
	
	public void stateAdvanced(Search search) 
	{
		long duration = System.currentTimeMillis() - this.startTime;
		
		if (duration >= maxDuration)
		{
			duration = duration / 1000;
			
			System.out.println("**** TIME BOUNDED SEARCH - TOTAL TIME (SECONDS): " + duration + " ****");
			
			search.terminate();
		}
	}	
}
