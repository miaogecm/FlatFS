#include "between.h"

#include <algorithm>
#include <iostream>
#include <assert.h>

Between::~Between () {
  delete _in;
  delete _sep;
}

Match
Between::next ()
{
  Match m;
  Match s;
  while (1) {
    m = _in->next ();
    if (!_in->eof ())
      _sep->advance (m.s);
    s = _sep->curr ();
    if (_sep->eof () || _in->eof ()) {
      m.s.dn = m.e.dn = 0;
      _curr_m = m;
      _eof = true;
      return m;
    }
    assert (s.e >= m.s );
    if ((s.s < m.s) && (s.e > m.e)) 
      break;
  }
  _curr_m = m;
  return m;
}

Match
Between::advance (PostIt to)
{
  Match m;
  PostIt t = to;
  do {
    m = _in->advance (t);
    if (!_in->eof ())
      _sep->advance (m.s);

    if (_in->eof () || _sep->eof ()) {
      m.s.dn = m.e.dn = 0;
      _curr_m = m;
      _eof = true;
      return m;
    }
    t = _sep->curr ().s;
  } while (m.e > t);

  _curr_m = m;
  return m;
}

void
Between::printstats()
{
  fprintf(stderr, "queryop: between\n");
}
