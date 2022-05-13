#ifndef __AND_H__
#define __AND_H__

#include "queryop.h"

class And : public QueryOp {
  public:
    struct inword {
      unsigned i; //which of the _in stream is this
      Match m;
      inword () {}
      bool operator> (const inword& p) const { return (m.s > p.m.s); }
      bool operator== (const inword& p) const { return (m.s == p.m.s); }
      bool operator< (const inword& p) const { return (m.s < p.m.s); }
    };
    And (vector<QueryOp *> in);
    ~And();
    virtual string name () { return "And";}
    virtual void init ();
    unsigned score (Match m);
    virtual bool stopearly (int min);
    unsigned max () { return _in[0]->max ();} //maximum possible matching places, i.e. the min of individual words' max
    Match curr () { return _curr_m;}
    virtual Match next ();
    Match advance (PostIt to);
    void  printstats();
    virtual double progress () { return _in[0]->progress();}

  protected: 

    vector<QueryOp *> _in;
    Match _curr_m;
    PostIt _max_mdn;
    PostIt _min_mdn;
    int _score;
  private:
    priority_queue< inword, vector<inword>, greater<inword> > _v;
};

//instead of using heap/priority_queue as in normal And,
//DistAnd uses a normal linked list which is slower, but allows 
//it to filter out Match that have multiple keywords/matches
//matched at the same word position
class DistAnd : public And {
  public:
    DistAnd (vector<QueryOp *> in) : And (in) {}
    //~DistAnd () { And::~And(); }
    virtual string name () { return "DistAnd";}
    virtual void init ();
    Match next ();
  private:
    void insertlist (inword x);
    list<inword> _v;
};

#endif
