package vutbr;

import java.util.LinkedList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;

public class RewriteList
{
    public static void main(String[] args)
    {
        LinkedList<Integer> l1 = new LinkedList<Integer>();
        int size = 3;
        int i = 0;
        while (i < size) 
        {
            l1.add(0);
            i = i + 1;
        }
        i = 0;
        while (i < size) 
        {
            l1.set(i, 10);
            i = i + 1;
        }
        Iterator<Integer> it = l1.iterator();
        int cur;
        while (it.hasNext()) 
        {
            cur = it.next();
            if (cur != 10) Verify.assertTrue("cur != 10", false);
        }
    }
}

