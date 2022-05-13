#ifndef __ARGS_H
#define __ARGS_H

#include <string>
#include <vector>
#include <algorithm>

using namespace std;

// arguments
class Args {

  public:
  Args(const char *fname);

  template<class T>
  T nget(string s, T defaultv = (T)0) {
    pair<string,string> pp = make_pair(s,string(""));
    unsigned p = upper_bound(_args.begin(),_args.end(), pp) - _args.begin();
    if (p >= _args.size()||_args[p].first!=s) return defaultv;
    return (T) atoi(_args[p].second.c_str());
  }

  string sget(string s, string defaultv) {
    pair<string,string> pp = make_pair(s,string(""));
    unsigned p = upper_bound(_args.begin(),_args.end(),pp) - _args.begin();
    if (p >= _args.size() || _args[p].first!=s) return defaultv;
    return _args[p].second;
  }

  protected:
  vector<pair<string,string> > _args;
};

#endif // __ARGS_H
