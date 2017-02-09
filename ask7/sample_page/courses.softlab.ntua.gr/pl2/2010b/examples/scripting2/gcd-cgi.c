#include <stdio.h>
#include <stdlib.h>

unsigned long gcd (unsigned long x, unsigned long y)
{
  while (x > 0 && y > 0)
    if (x > y) x %= y; else y %= x;
  return x + y;
}

int main ()
{
  printf("Content-type: text/html\n\n");
  printf("<html>\n"
         "<head>\n"
         "<title>CGI script for the GCD</title>\n"
         "</head>\n"
         "<body>\n"
         "<h1>Result</h1>\n");

  char *data = getenv("QUERY_STRING");
  unsigned long x, y;

  if (data == NULL)
    printf("<p>Error! Error in passing data from form to script.</p>");
  else if (sscanf(data, "x=%lu&y=%lu", &x, &y) != 2)
    printf("<p>Error! Invalid parameters. Data must be numeric.</p>");
  else
    printf("<p>The GCD of %lu and %lu is %lu.</p>\n", x, y, gcd(x, y));

  printf("</body>\n"
         "</html>\n");
  return 0;
}
