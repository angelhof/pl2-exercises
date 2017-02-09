#include <stdio.h>

int main ()
{
  printf("Content-type: text/html\n\n");
  printf("<html>\n"
         "<head>\n"
         "<title>My first CGI script</title>\n"
         "</head>\n"
         "<body>\n"
         "<h1>Hello world!</h1>\n"
         "<p>This is my first CGI script in C.</p>\n"
         "</body>\n"
         "</html>\n");
  return 0;
}
