#ifndef __BETWEEN_H__
#define __BETWEEN_H__

#include "queryop.h"

class Between : public QueryOp {
  public:
    Between (QueryOp *in, QueryOp *sep) :  QueryOp (), _sep (sep), _in (in) {}
    ~Between ();
    virtual string name () { return "Between";}
    unsigned max () { return _in->max ();}
    QueryOp *sep () { return _sep;}
    Match curr () { return _curr_m;}

    Match next ();
    Match advance (PostIt to);

    void printstats() ;

  private:
    QueryOp *_sep;
    QueryOp *_in;
    Match _curr_m;
};

#endif
