package vutbr;

import java.util.LinkedList;
import java.util.Iterator;

import gov.nasa.jpf.jvm.Verify;

public class ListLength
{
    public static void main(String[]args) 
    {
        LinkedList<Integer> l1 = new LinkedList<Integer>();
        int cur = 0;
        while (cur < 2) 
        {
            l1.add(10);
            cur = cur + 1;
        }
        Iterator<Integer> it = l1.iterator();
        int len = 0;
        while (it.hasNext()) 
        {
            cur = it.next();
            len = len + 1;
        }
        l1.add(30);
        it = l1.iterator();
        int len2 = 0;
        while (it.hasNext()) 
        {
            cur = it.next();
            len2 = len2 + 1;
        }
        if (len + 1 != len2) Verify.assertTrue("len+1 != len2", false);
    }
} 

