import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.BufferedReader;
import java.io.FileReader;

public class USAddressJ 
{
	private static final String REGEX = "^(.*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$";
	public static void main(String[] args) 
	{
		Pattern p = Pattern.compile(REGEX);
		try (BufferedReader br = new BufferedReader(new FileReader("/tmp/addr.txt"))) 
		{
			String line;
			while ((line = br.readLine()) != null) 
			{
				Matcher m = p.matcher(line);
				if (m.matches())
				{
					for (int i = 0; i < m.groupCount() ; i++)
					{
						System.out.print(m.group(i));
						System.out.print(",");
					}
					System.out.println();
				} else 
				{
					System.out.println("not matched.");
				}
			}
		}
		catch (Exception e) 
		{
			System.out.println(e);
		}
	}
}