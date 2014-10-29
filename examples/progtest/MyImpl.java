package progtest;


public class MyImpl implements MyItf
{
	public int num = 0;

	public void doSmth(int p)
	{
		if (p > 0) num = p + 2;
	}

	public static void main(String[] args)
	{
		MyItf m = new MyImpl();

		m.doSmth(3);
	}
}
