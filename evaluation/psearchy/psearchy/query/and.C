#include "and.h"
#include <iostream>
#include <algorithm>
#include <math.h>
#include <assert.h>
#include <map>

And::And (vector<QueryOp *> in) : QueryOp ()
{
  _in = in;
  _curr_m.s.dn = _curr_m.e.dn = 0;

  if (_in.size () < 1) {
    cerr << "And::And less than one word in And" << endl;
    return;
  }

  if (_in.size () > sizeof (unsigned)*8) {
    _in.resize (sizeof (unsigned)*8);
    cerr << "warning: too many terms, only accepting " <<  _in.size() << endl;
  }

  _max_mdn.dn = 0;
  _max_mdn.wc = 0;

  //sort input streams so a stream with the less postings appear in front
  sort (_in.begin (), _in.end (), QueryOp::maxcmp);
  init ();
}

And::~And () 
{
   map<QueryOp *, int> dup;
   for (unsigned i = 0; i < _in.size (); i++) {
      if (dup.find(_in[i]) == dup.end()) {
	 delete _in[i];
	 dup[_in[i]] = 1;
      }
  }
  _in.clear();

}

void
And::init ()
{
  _score = 0;
  _max_mdn.dn = _max_mdn.wc = 0;
  _min_mdn.dn = (DID)-1;
  _min_mdn.wc =  (word_pos)-1;
#if 0
  inword qw;
  for (unsigned i = 0; i < _in.size (); i++) {
    qw.m = _in[i]->next ();
    if (_in[i]->eof ()) 
      return;
    qw.i = i;
    _v.push (qw);
    _score += qw.m.score;
    if (qw.m.e > _max_mdn) 
      _max_mdn = qw.m.e;
  }
  assert (_max_mdn > _v.top ().m.s);
#endif
}

void
And::printstats ()
{
  printf("And: nexts %u advanaces %u\n", _next_stats, _advance_stats);
  for (unsigned i = 0; i < _in.size(); i++) 
    _in[i]->printstats ();
}

Match
And::next ()
{
  inword x;
  PostIt to;
  to.wc = 0;
  Match m, cm;

  _next_stats++;
  _curr_m.s.dn = _curr_m.e.dn = 0;
  //input list is sorted from rarest to more common
  //keep _max_mdn.dn, the biggest document number seen so far among all input lists
  //always try to advance a more rare list to be/past _max_mdn.dn
  //updating _max_mdn.dn 
  //when all lists have _max_mdn.dn, we find a match
  int i = 0, pi = -1;
  while (i < (int)_in.size()) {

    if (pi == i) {
      i++;
      continue;
    }
    cm = _in[i]->curr();
    if (_in[i]->curr().e.dn < _max_mdn.dn) {
      to.dn = _max_mdn.dn;
      m = _in[i]->advance (to);
    } else if (_in[i]->curr().e < _max_mdn) {
      m = _in[i]->advance (_max_mdn);
    } else 
      m = _in[i]->next ();

    if (_in[i]->eof()) {
      _eof = true;
      return _curr_m;
    }

    if (m.s.dn > _min_mdn.dn || m.s < _min_mdn)
      _min_mdn = m.s;

    if (m.e.dn == _max_mdn.dn) {
      i++;
      if (m.e > _max_mdn)
        _max_mdn = m.e;
    } else {
      pi = i;
      i = 0;
      _max_mdn = m.e;
    } 
  }
  _curr_m.s = _min_mdn;
  _curr_m.e = _max_mdn;
  _curr_m.score = 0;

  if (!_curr_m.s.dn || !_curr_m.e.dn) 
    _eof = true;
  else {
    for (i = 0; i < (int) _in.size(); i++) 
      _curr_m.score += _in[i]->curr().score;
    _curr_m.score = score(_curr_m);
    _min_mdn = _max_mdn;
  }

  return _curr_m;
}

Match
And::advance (PostIt to)
{
  _advance_stats++;
  _max_mdn.dn = to.dn;
  return next ();
}

//give words that occur close together higher scores
unsigned 
And::score (Match m)
{
  unsigned s;
  int d = m.e.wc - m.s.wc;
  d = d << 16;
  int i = 16;
  while (!(d & 0x80000000) && (i >0)) {
    d = d << 1;
    i--;
  }
  s =  m.score/(i+1);
  return s;
}


//This function is closely related to score(), ONE MUST CHANGE BOTH
//returns true if there will be no more documents with scores
//higher than min
bool
And::stopearly (int min)
{
  if (_in[0]->stopearly (min))
    return true;
  else
    return false;
}


void
DistAnd::init ()
{
   _score = 0;
  inword qw;
  for (unsigned i = 0; i < _in.size (); i++) {
    qw.m = _in[i]->next ();
    if (_in[i]->eof ()) 
      return;
    qw.i = i;
    insertlist (qw);
    _score += qw.m.score;
    if (qw.m.e > _max_mdn) 
      _max_mdn = qw.m.e;
  }
  assert (_max_mdn > _v.front ().m.s);
}

void
DistAnd::insertlist (inword x)
{
  list<inword>::iterator j;
  for (j = _v.begin (); j!= _v.end (); ++j) {
    if (x == (*j)) {
      Match xn = _in[x.i]->peeknext ();
      inword pp;
      pp.m = _in[j->i]->peeknext ();
      pp.i = j->i;
      if (xn.s > pp.m.s) {
	//swap positions
	_score -= j->m.score;
	j->i = x.i;
	j->m = x.m;
	_score += x.m.score;
	x = pp;
	_in[pp.i]->next ();
      }else{
	//xn and pp.m might still be the same
	//resulting in arbitary advancement
	//XXX -- jy'll fix it later
	x.m = xn;
	_in[x.i]->next ();
      }
    }else if (x > (*j))
      break;
  }
  _v.insert (j, x);
}

Match
DistAnd::next ()
{
  inword x;
  PostIt to;
  to.wc = 0;
  Match m;

  _curr_m.s.dn = _curr_m.e.dn = 0;
  if (_v.size () < _in.size ()) 
    return _curr_m;

  do {
    if (_v.front ().m.s.dn == _max_mdn.dn) {
      _curr_m.s = _v.front ().m.s;
      _curr_m.e = _max_mdn;
      _curr_m.score = _score;
      _curr_m.score = score (_curr_m);
    }
    to.dn = _max_mdn.dn;
    x = _v.front ();

    m = _in[x.i]->curr ();
    x.m = _in[x.i]->advance (to);
    if (x.m.s == m.s)
      x.m = _in[x.i]->next ();

    _score -= _v.front ().m.score;
    _v.pop_front ();

    if (_in[x.i]->eof ()) 
      return _curr_m;

    insertlist (x);

    _score += x.m.score;
    if (x.m.e > _max_mdn)
      _max_mdn = x.m.e;
    assert (_max_mdn >= _v.front ().m.s);
  }while (!_curr_m.s.dn);

  return _curr_m;
}
