#ifndef __MKDB_H_
#define __MKDB_H_

#define WORDCHAR(c) (isalnum(c))
#define SENTENCESTOP(c) (c == '.' || c=='?' || c=='!')
typedef long long DID; //determines max. docs

#if defined(__i386__)
typedef unsigned short word_pos;
#else
typedef unsigned word_pos;
#endif

typedef unsigned long long ind_offset; //the index file size is max. unsigned long long

struct PostIt { 
  DID dn; 
  word_pos wc;
  bool operator< (const PostIt &p) const {
    return ((dn < p.dn) || (dn == p.dn && wc < p.wc));
  }
  bool operator== (const PostIt &p) const {
    return ((dn == p.dn) && (wc == p.wc));
  }
  bool operator!= (const PostIt &p) const {
    return ((dn != p.dn) || (wc != p.wc));
  }
  bool operator>= (const PostIt &p) const {
    return ((dn > p.dn) || (dn == p.dn && wc >= p.wc));
  }
  bool operator> (const PostIt &p) const {
    return ((dn > p.dn) || (dn == p.dn && wc > p.wc));
  }
  int operator- (const PostIt &p) const {
    return (dn - p.dn);
  }
  static bool cmp (const PostIt& a, const PostIt& b) {
    return ((a.dn < b.dn) || (a.dn == b.dn && a.dn <= b.dn));
  }
#if defined(__i386__)
} __attribute__ ((packed));
#else
}; //god knows what ((packed)) does on other architectures
#endif

#endif //__MKDB_H_
