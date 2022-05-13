#ifndef _DOC_NOMANAGER_H_
#define _DOC_NOMANAGER_H_

#include "../mkdb/mkdb.h"
#include <db.h>
#include <string.h>
using namespace std;

class DocNoManager {
  public :
    DocNoManager(string no2name, string name2no, bool update=false);
    ~DocNoManager();
    static DocNoManager* Instance();
    void init();

    DID getDocNo(string);
    DID getmaxfo() { return _maxfo; }
    static int compare_DID(DB *db, const DBT *a, const DBT *b) { 
      DID ai, bi; /* * Returns: * < 0 if a < b * = 0 if a = b * > 0 if a > b */ 
      memcpy(&ai, a->data, sizeof(DID)); 
      memcpy(&bi, b->data, sizeof(DID)); 
      return (ai - bi); 
    }


  private:
    void cleanup();

    string _no2name;
    string _name2no;

    DB *_no2name_db;
    DB *_name2no_db;

    DID _maxfo;
    ind_offset _curr_off;

    bool _update;

    static DocNoManager *_instance;
};
#endif
