package vutbr;

import java.util.ArrayList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;

public class CheckSorted
{
    public static void main(String[] args)
    {
        ArrayList<Integer> l1 = new ArrayList<Integer>();
        int i = 0;
        while (i < 3) 
        {
            i = i + 1;
            l1.add(i);
        }
        Iterator<Integer> it = l1.iterator();
        int ins = 2;
        int cur;
        int pos = 0;
        while (it.hasNext()) 
        {
            cur = it.next();
            if (cur > ins) break;
            pos = pos + 1;
        }
        l1.add(pos,ins);
        it = l1.iterator();
        int pre;
        int t1 = it.next();
        pre = t1;
        int t2;
        while (it.hasNext()) 
        {
            t2 = it.next();
            cur = t2;
            if (pre > cur) Verify.assertTrue("pre > cur", false);
            pre = cur;
        }
    }
}

