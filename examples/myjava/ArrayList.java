package myjava;


public class ArrayList
{
	private Object[] data = new Object[0];
	private int size = 0;
  
	public Object get(int index) 
	{
	    return data[index];
  	}

	public void add(Object o) 
	{
		if (size == data.length) 
		{
			Object[] ndata = new Object[size * 2 + 1];
			System.arraycopy(data, 0, ndata, 0, size);
			data = ndata;
		}
		
		data[size] = o;
		size = size + 1;
	}
	
	public Object remove(int idx) 
	{
		if (this.size <= idx) return null;

		Object v = data[idx];

		for (int i = idx + 1; i < size; i++) data[i-1] = data[i];
		size = size - 1;

		return v;
	}

	public int size() 
	{
		return this.size;
	}
}
