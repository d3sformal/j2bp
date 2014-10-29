package paper;

import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;


public class Ex5
{
	public static Map<Integer,Customer> customers;
	public static Set<Integer> vip;
	public static List<Order> orders;

	public static void main(String[] args)
	{
		// remove these lines in the paper
		customers = new HashMap<Integer,Customer>();
		vip = new HashSet<Integer>();
		orders = new ArrayList<Order>();

		customers.put(1, new Customer(1, 60000));
		customers.put(2, new Customer(2, 35000));

		orders.add(new Order(1, 60000, 2008));
		orders.add(new Order(2, 35000, 2006));

		processOrder(1, 100000, 2009);
		processOrder(2, 80000, 2010);

		pruneVIPs(2011);
		giveBenefits();
	}

	public static void processOrder(int cid, int price, int year)
	{
		orders.add(new Order(cid, price, year));

		Customer c = customers.get(cid);
		c.balance = c.balance + price;

		// make customer VIP if its balance > 100000
		if (c.balance > 100000) vip.add(cid);
	}

	public static void pruneVIPs(int currentYear)
	{
		Order o;
		Customer c;

		Iterator<Order> it = orders.iterator();
		while (it.hasNext())
		{
			o = it.next();

			// old orders do not count into the VIP balance
			if (currentYear - 5 > o.year)
			{
				c = customers.get(o.cid);
				
				c.balance = c.balance - o.price;
				if (c.balance <= 100000) vip.remove(c.id);

				it.remove();
			}
		}
	}

	public static void giveBenefits()
	{
		Integer vid;
		Customer vc;

		Iterator<Integer> it;
		for (it = vip.iterator(); it.hasNext(); )
		{
			vid = it.next();
			vc = customers.get(vid);

			// property: vc.balance cannot be smaller than zero because it would make discount negative (i.e., more expensive orders)
			vc.discount = (vc.balance - 100000) / 20;
		}
	}
}

class Customer
{
	public int id;
	public int balance;
	public int discount;

	public Customer(int i, int b)
	{
		id = i;
		balance = b;
		discount = 0;
	}
}

class Order
{
	public int cid;
	public int price;
	public int year;

	public Order(int c, int p, int y)
	{
		cid = c;
		price = p;
		year = y;
	}
}
