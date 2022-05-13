#ifndef __QUERYOP_H__
#define __QUERYOP_H__

#include <stdio.h>

#include <db.h>
#include <sys/types.h>
#include <fcntl.h>
#include <limits.h>

#include <string>
#include <vector>
#include <map>
#include <queue>

#include <list>
#include "../mkdb/mkdb.h"

using namespace std;

struct Match {
  PostIt s; //start of the match
  PostIt e; //end of the match
  unsigned score;
  static bool scorecmp (Match a, Match b) { return (a.score >= b.score); }
};

class QueryOp {

  public:
    QueryOp () : _eof(false), _next_stats(0), _advance_stats(0),_max(0) {}
    virtual ~QueryOp () {}
    virtual Match next () = 0;
    virtual Match peeknext () { return curr ();}
    virtual Match advance (PostIt to) = 0 ;
    virtual Match curr () = 0;
    virtual string name () { return "QueryOp";}
    bool eof () { return _eof; }
    virtual double progress () { return 0.0;}
    virtual unsigned max () { return 0;}
    virtual QueryOp *sep () { return NULL;}
    virtual bool stopearly (int min) { return false;}
    static bool maxcmp (QueryOp* a, QueryOp* b) { return (a->max () <= b->max ()); }
    virtual void printstats () = 0;

  protected:
    bool _eof;
    unsigned _next_stats;
    unsigned _advance_stats;
    unsigned _max; 
};

class Word : public QueryOp {
  public:
    Word(string w, FILE *f, DB *db, map<string, ind_offset>& w2p, unsigned maxlen);
    void init ();
    virtual ~Word();
    virtual string name () { return "Word";}
    unsigned max () { return _max;}
    Match curr () { return _curr_m;}
    double progress () { return _max>0?(double)_in_core_p/(double)_max:0.0;}
    virtual Match peeknext () 
    {
      Match m;
      if (_in_core_p < _max) 
	m.s = m.e = _in_core[_in_core_p];
      else 
	m.s.dn = m.e.dn = 0;
      return m;
    }
    virtual unsigned score (Match p);
    virtual bool stopearly(int min);
    static Word *getrarest () { return rarest;}
    static void setrarest (Word *w) { rarest = w;}
    static void settags(unsigned num, unsigned *tags) 
    { 
      numtags = num; 
      tagmask = 0;
      for (unsigned i = 0; i < (sizeof(word_pos)*8 - numtags);i++) 
	tagmask = (tagmask << 1) | 0x1;
      tagsmulti = tags;
    }
    virtual Match next();
    virtual Match advance(PostIt to);

    /* statistics collection functions*/
    void printstats();
    int get_pg() { return _n_pg;}
    int get_corepg() { return _n_corepg;}


    static unsigned numtags;
    static word_pos tagmask;
    static unsigned *tagsmulti; //pointer to an array of multipliers for each tag

  protected:
    void memory_resident(unsigned p);
    static Word *rarest;

    PostIt *_in_core;
    unsigned _in_core_p;
    void *_in_core_p_real;
    size_t _in_core_p_sz;
    Match _curr_m;
    string _w;
    unsigned _s;
    unsigned _s_sqrt;
    DID _s_dn;

  
  private:
#if linux
   unsigned char *_incore_vec;
#else
   char *_incore_vec;
#endif
   int _n_pg;
   int _n_corepg;
   int _vec_start;
   map<int, int> _pg_checked;
   int _last_checked;
   double _read_time;
   double _read_db_time;
   double _read_max_time;

};

class SepWord : public Word {
  public:
    SepWord (string w, FILE *f, DB *db, map<string, ind_offset>& w2p, unsigned maxlen, bool delim = true) 
      : Word (w, f, db, w2p, maxlen), _delim (delim) {}
    ~SepWord () {}
    virtual string name () { return "SepWord";}
    Match next ();
    Match advance (PostIt to);

  protected:
    bool _delim;
};

class SimpleWord : public Word {
  public:
    SimpleWord (string w, FILE *f, DB *db,  map<string, ind_offset>& w2p, unsigned maxlen, 
		bool delim = true) : Word (w, f, db, w2p, maxlen), _delim (delim) {}
    virtual ~SimpleWord () {} 
    virtual string name () { return "SimpleWord";}
    virtual unsigned score (Match p);
    virtual bool stopearly (int min);
  private:
    bool _delim;
};

class PositionalWord : public Word {
public:
    PositionalWord(string w, FILE *f, DB *db,  map<string, ind_offset>& w2p, unsigned maxlen)
      : Word(w, f, db, w2p, maxlen) {}
    virtual ~PositionalWord() {} 
    virtual string name () { return "PositionalWord";}
    virtual unsigned score(Match p);
    virtual bool stopearly(int min);
};

class Aggregate : public QueryOp {
  public:
    Aggregate (QueryOp *in, unsigned n = 1);
    ~Aggregate ();
    virtual string name () { return "Aggregate";}
    double progress () { return _in->progress();}
    bool stopearly(int s);
    unsigned max ();
    void printstats ();
    Match curr () 
    {
      Match m;
      if (!_eof && _cs < _n)
	m = _curr_m[_cs];
      else
	m.s.dn = m.e.dn = 0;
      return m;
    }
    Match next ();
    Match advance (PostIt);

  protected:
    QueryOp *_in;
    QueryOp *_sep;
    Match _curr_sep;
    unsigned _n;
    Match *_curr_m;
    unsigned _cs;
    unsigned _ce;
    static int maxcount;
};

#endif
