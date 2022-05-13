#include "query.h"
#include "../mkdb/mkdb.h"

class BoolOperator {
  public:
    BoolOperator(vector<string> w) { words = w;}
    virtual PostIt next_doc();
  private:
    vector<string> words;
};

class And : BoolOperator {
  public:
    PostIt next_doc();
};

class Or : BoolOperator {
  public:
    PostIt next_doc();
};
