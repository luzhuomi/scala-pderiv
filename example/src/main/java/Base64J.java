import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.BufferedReader;
import java.io.FileReader;

public class Base64J 
{
	private static final String REGEX = "^\\s*(?:(?:[a-z0-9+/]\\s*){4})*(?:(?:[a-z0-9+/]\\s*){2}\\s*[a-z0-9+/=]\\s*=)?\\s*$";
	public static void main(String[] args) 
	{
		Pattern p = Pattern.compile(REGEX,Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);
		try (BufferedReader br = new BufferedReader(new FileReader("/tmp/base64.txt"))) 
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