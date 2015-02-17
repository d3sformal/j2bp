package vutbr;

import java.util.LinkedList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;

public class ListContents
{
    public static void main(String[] args)
    {
        LinkedList<Integer> l1 = new LinkedList<Integer>();
        int size = 3;
        int i = 0;
        while (i < size) 
        {
            i = i + 1;
            l1.add(i);
        }
        i = 0;
        Iterator<Integer> it = l1.iterator();
        int cur;
        while (it.hasNext()) 
        {
            i = i + 1;
            cur = it.next();
            if (cur != i) Verify.assertTrue("cur != i", false);
        }
    }
}

