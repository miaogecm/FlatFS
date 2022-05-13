#include "args.h"
#include <stdio.h>
#include <string.h>

Args::Args(const char *fname)
{
  FILE *f = fopen(fname, "r");
  if (!f) return;

  bool skip = false;
  char first[200],second[200];
  unsigned i = 0, j = 0;
  char c;

  while ((c = getc(f)) != EOF) {
    if (c == '#') {
      skip = true;
    }else if (c == '\n') {
      first[i] = '\0';
      second[j] = '\0';
      if (!skip)  {
        _args.push_back(make_pair(string(first),string(second)));
      }
      memset (first, 1, 200);
      memset (second, 1, 200);
      i = j = 0;
      skip = false;
    }else if (!skip) {
      if (c == '=') {
        if (first[i]!='\0')
          first[i++] = '\0';
        continue;
      }
      if (i >= 200 || j >= 200)
        continue;

      if (i == 0 || first[i-1] != '\0')
        first[i++] = c;
      else
        second[j++] = c;
    }
  }

  sort(_args.begin(),_args.end());
}
