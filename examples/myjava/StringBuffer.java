package myjava;


public class StringBuffer 
{
	char[] value;
	int count;


	public StringBuffer()
	{
		value = new char[8];
		count = 0;
	}

	public int length() 
	{
		return count;
    }

	public void setLength(int newLength) 
	{
		if (newLength < 0) return;

		if (newLength > value.length) expandCapacity(newLength);
	}

	public char charAt(int index) 
	{
		if ((index < 0) || (index >= count)) return (char) -1;
		
		return value[index];
	}
 
	public void setCharAt(int index, char ch) 
	{
		if ((index < 0) || (index >= count)) return;

		value[index] = ch;
	}

	public StringBuffer append(char c) 
	{
		synchronized (this)
		{
			int newCount = count + 1;

			if (newCount > value.length) expandCapacity(newCount);

			value[count] = c;
		
			count = newCount;
		}
		
		return this;
	}

	public synchronized StringBuffer insert(int offset, char c)
	{
		if ((offset < 0) || (offset > count)) return this;

		int newCount = count + 1;
		
		if (newCount > value.length) expandCapacity(newCount);

		System.arraycopy(value, offset, value, offset + 1, count - offset);

		value[offset] = c;
		
		count = newCount;
		
		return this;
	}


	public void expandCapacity(int newLength)
	{
		char[] newValue = new char[newLength];

		for (int i = 0; i < count; i++) newValue[i] = value[i];

		value = newValue;
	}

}
