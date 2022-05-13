#include <sys/types.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <errno.h>
#include <unistd.h>

#include "queryop.h"
#include "math.h"
#include "time.h"
#include <sys/time.h>


#include <algorithm>
#include <iostream>
#include <assert.h>
#include <string.h>

extern int verbose;
extern int page_size;
extern int page_shift;
unsigned Word::numtags = 0;
unsigned *Word::tagsmulti = NULL;
word_pos Word::tagmask = (word_pos)-1;
Word *Word::rarest = NULL;
int Aggregate::maxcount = 5;

extern double gettime();

/*
 * mmap() doesn't like offsets not divisable by the page size.
 */
char *
xmmap(size_t len, int fd, off_t offset, void *& realp, size_t& reallen)
{
  void *p;
  size_t nlen;
  off_t noffset;
  int pagesize = getpagesize();

  noffset = offset & ~(pagesize-1);
  nlen = len + (offset - noffset);

  p = mmap(0, nlen, PROT_READ, MAP_SHARED, fd, noffset);
  if(p == (void *)-1){
    fprintf(stderr, "queryop: xmmap %ld bytes at %ld failed: %s\n",
	    (long)nlen,
	    (long)noffset,
	    strerror(errno));
    exit(1);
  }
  realp = p;
  reallen = nlen;
  return((char*)p + (offset - noffset));
}

Word::Word(string w, FILE *f, DB *db, map<string, ind_offset>& w2p, unsigned maxlen) : QueryOp()
{

  FILE *fp = f;
  double ts1, ts2;

  ind_offset offset;
  DBT key, data;
  bzero(&key,sizeof(key));
  bzero(&data,sizeof(data));
  key.data = (void *)w.c_str();
  key.size = w.size() + 1;
  data.data = &offset;
  data.size = sizeof(offset); 

  _incore_vec = NULL;
  _in_core_p = 0;
  _s = 0;
  _w = w;
  _s_dn = 0;
   _n_pg = _n_corepg = 0;
  _vec_start = -1024;
  _last_checked = -1;

  if (verbose)
    ts1 = gettime ();
  if (db) {
     if ((db->get(db, NULL, &key, &data, 0) != 0) 
	   || (data.size != sizeof(offset))) {
	_max = _in_core_p = 0;
	cout << w << ": no such word found in database" << endl;
	 return;
     }
     memcpy(&offset,data.data,sizeof(offset));
  } else {
     if (w2p.find(w) == w2p.end()) {
	_max = _in_core_p = 0;
	cout << w << ": no such word found in database" << endl;
	return;
      }
     offset = w2p[w];
  }
  if (verbose) {
     ts2 = gettime ();
     _read_db_time = ts2-ts1;
  }
  if (fseeko(fp,(off_t)offset,SEEK_SET) != 0) {
    fprintf(stderr,"seek error\n");
    _max = _in_core_p = 0;
    return;
  }

  char wordbuf[maxlen+2+sizeof(_max)];
  unsigned r = fread(wordbuf,1,w.size()+1+sizeof(_max),fp);
  if ((r!= (w.size()+1+sizeof(_max))) || (strcmp(w.c_str(),wordbuf)!=0)) {
    fprintf(stderr,"read error! read %d char (%s) opposed to %s\
	end of file? %u\n", r, wordbuf,w.c_str(),feof(fp)?1:0);
    _max = _in_core_p = 0;
    return;
  }
  offset += (w.size()+1 + sizeof(_max));
  _max = *((unsigned *)(wordbuf+w.size()+1));

  _in_core = (PostIt *)xmmap(_max*sizeof(PostIt),fileno(fp),(off_t)offset, _in_core_p_real, _in_core_p_sz);
  _in_core_p = 0;
  _curr_m.s.dn = _curr_m.s.wc = 0;
  _curr_m.e.dn = _curr_m.e.wc = 0;
  if (_max > 0)
    _s = (DID)-1/_max;

  
  
  if (verbose) {
     ts1 = gettime ();
     _read_max_time = ts1-ts2;

#if linux
     _incore_vec = (unsigned char *)malloc(1+(_in_core_p_sz>>page_shift));
#else
     _incore_vec = (char *)malloc(1+(_in_core_p_sz>>page_shift));
#endif
   }

  if (verbose) {
     if (mincore((void *)_in_core_p_real, _in_core_p_sz, _incore_vec)!=0) {
	perror("mincore error: ");
     }else {
	_n_pg = _in_core_p_sz >>page_shift;
	for (int i = 0; i < _n_pg; i++) {
	   if (_incore_vec[i] % 2 > 0)
	      _n_corepg++;
	}
     }
  }
  r = madvise(_in_core_p_real, _in_core_p_sz, MADV_SEQUENTIAL);
  assert(r == 0);
// #if linux
//   r =  mlock(_in_core_p_real, _in_core_p_sz);
//   if (r!= 0) 
//      perror("mlock:");
//   assert(r == 0);
// #endif	
  PostIt p; 
  for (unsigned i = 0; i < _max; i++) 
    p = _in_core[i];

  if (verbose) {
     ts2 = gettime ();
     _read_time = ts2-ts1;
   }

  /*
    m.s =  p;
    score (m);
    m.s.wc &= tagmask;
    m.e = m.s;
    if (i > 0)
      assert (m.s.dn >= _in_core[i-1].dn);
    next ();

  }
  exit (0);
  */
}

Word::~Word () 
{
  if (_max && munmap (_in_core_p_real, _in_core_p_sz) != 0) {
     fprintf(stderr, "error trying to munmap %p\n", _in_core_p_real);
     perror("queryop: munmap");
   }
  if (_incore_vec)
     free(_incore_vec);
}

void
Word::memory_resident(unsigned p)
{
   return;
   int pg = (p * sizeof(PostIt)) >> page_shift; 
   //if (_pg_checked.find(pg) == _pg_checked.end()) { //quite slow
   if (pg != _last_checked) {
      if ((pg - _vec_start) >= 1024){
	 int sz = ((_max*sizeof(PostIt))>>page_shift) - pg + 1;
	 if (sz > 1024)
	    sz = 1024;
	 _vec_start = pg;
	 if (mincore((void *)(((long long)_in_core + p)&(~(page_size-1))), sz << page_shift, _incore_vec)!=0)
	    perror("queryop.C: Word::next mincore:");
      }
      if (_incore_vec[pg -_vec_start] % 2 > 0) 
	 _n_corepg++;
      _n_pg++;
      _last_checked = pg;
      _pg_checked[pg] = 1;
   }
}

Match
Word::next ()
{
  _next_stats++;
  Match m;
  m.s.dn = m.e.dn = 0;
  if (_in_core_p >= _max) {
    _eof = true;
    _curr_m = m;
    return m;
  }

  /* measure the residence of a page */
  if (verbose > 0) 
     memory_resident(_in_core_p);

  m.s = _in_core[_in_core_p];
  if (_in_core_p > 0)
    assert(m.s.dn >= _in_core[_in_core_p-1].dn);
  m.score = score (m);
  m.s.wc &= tagmask;
  m.e = m.s;
  _curr_m = m;
  _in_core_p++;
  return m;
}

//advance the list to return the first posting equal or after to
Match
Word::advance (PostIt to)
{
  Match m;
  m.s.dn = m.e.dn = 0;
  unsigned end = _max - 1;
  unsigned mid;

  if ((_in_core_p >= _max) 
      || (_in_core[end] < to)) {
    _eof = true;
    _curr_m = m;
    return m;
  }

  if (_in_core_p > 0 
      && _in_core[_in_core_p-1].dn > to.dn) {
    m = _curr_m;
    return m;
  }
  if (_curr_m.s.dn == to.dn) {
    while (_in_core[_in_core_p].dn == to.dn && (_in_core[_in_core_p].wc &tagmask) < to.wc) {
      _in_core_p++;
      if (verbose)
	 memory_resident(_in_core_p);
    }
    _curr_m.s = _in_core[_in_core_p++];
    _curr_m.score = score(_curr_m);
    _curr_m.s.wc &= tagmask;
    _curr_m.e = _curr_m.s;
    _next_stats++;
    return _curr_m;
  }
  _advance_stats++;
#if 0
  if (_in_core[_in_core_p] < to) {
    while (_in_core_p < end ) {
      mid = (_in_core_p+end)/2;
      if (_in_core[mid] < to) {
	_in_core_p = mid+1;
	if (_in_core[_in_core_p] > to) 
	  break;
      }else if (_in_core[mid] >= to) {
	end = mid;
      }
    }
  }
#else
  if (_in_core[_in_core_p] < to) {
    _in_core_p = 0;
    while (_in_core_p < end ) {
      mid = (_in_core_p+end)/2;
      if (verbose > 0)
	 memory_resident(mid);
      if (_in_core[mid] < to) {
	_in_core_p = mid+1;
	if (_in_core[_in_core_p] > to) 
	  break;
      }else if (_in_core[mid] >= to) {
	end = mid;
      }
    }
  }
#endif 
  m.s = _in_core[_in_core_p++];
  m.score = score(m);
  m.s.wc &= tagmask;
  m.e = m.s;
  _curr_m = m;
  return m;
}

unsigned 
Word::score (Match p)
{
  int s;
  s =  _s; 

  //scale the score by the log of the position of each word in the
  //corresponding document, so words appear in the beginning of the
  //text score higher.
  //a gross approximation of log, s = s/(int) log ((int)(tagmask & p.s.wc)+2);
  //count the first occurance of 1 in a digit's binrary representation
  //to speed things up.
  long long t = tagmask & p.s.wc;
  t = t << 16;
  int i = 16;
  while (!(t & 0x80000000) && i >0) {
    t = t << 1;
    i--;
  }
  s = s/(i+1);
  //s = s/((int) log ((int)(tagmask & p.s.wc)+2)+1);

  //scale the score by the square root of document ID
  //e.g. if document ID has been arranged such as 
  //more important document has smaller ID, this score prefers
  //results with smaller document ID.
  //save previous computation...
  if (_s_dn != p.s.dn) {
    _s_dn = p.s.dn;
    _s_sqrt = (int)sqrt((double)(_s_dn));
  }
  s = s/_s_sqrt;

  //multiply the score based on tag values
  word_pos mask = 0x1 << (sizeof (word_pos)*8-Word::numtags);
  for (unsigned i = 0; i < Word::numtags; i++) {
    if (mask & p.s.wc) 
      s *= Word::tagsmulti[i];
    mask = mask >> 1;
  }
  return s;
}

//This function is closely related to score(), ONE MUST CHANGE BOTH
//returns true if there will be no more documents with scores
//higher than min
bool
Word::stopearly (int min)
{
  if (((long long) _s/((long long) sqrt((double)_s_dn))) < min)
    return true;
  else
    return false;
}

void
Word::printstats ()
{
  fprintf (stderr, "queryop: %s nexts %d advances %d max %d pointer %d \
      readtime %.1f,%.1f,%.1f (us) pg %u corepg %u\n", 
      _w.c_str(), _next_stats, _advance_stats, _max, _in_core_p, 
      _read_db_time, _read_max_time, _read_time, _n_pg, _n_corepg);
}

Match 
SepWord::next ()
{
  Match m, n; 
  if (_delim) {
    m = this->curr ();
    n = Word::next ();
    if (n.s.dn)
      m.e = n.s;
    else
      m.s.dn = m.e.dn = 0;
  }else{
    m = Word::next ();
    n = Word::next ();
    if (m.s.dn && n.s.dn)
      m.e = n.s;
    else
      m.s.dn = m.e.dn = 0;
  }
  _curr_m = n;
  return n;
}

//advance to and return the seperator that 
//SPAN or AFTER to
Match
SepWord::advance (PostIt to)
{
  Match m = Word::advance (to);
  if (_delim || ((_in_core_p % 2)==0)) {
    if (_in_core_p > 1)
      m.s = _in_core[_in_core_p-2];
    else
      m.s.dn = m.s.wc = 0;
    assert (m.s < to);
  }else {
    Match n = Word::next ();
    m.e = n.s;
  }
  _curr_m = m;
  assert (_curr_m.e > to);
  return m;
}

unsigned 
SimpleWord::score (Match p)
{
  unsigned s;
  s = _s;
  
  //multiply the score based on tag values
  word_pos mask = 0x1 << (sizeof (word_pos)*8-Word::numtags);
  for (unsigned i = 0; i < Word::numtags; i++) {
    if (mask & p.s.wc) 
      s *= Word::tagsmulti[i];
    mask = mask >> 1;
  }
  return s;
}

//This function is closely related to score(), ONE MUST CHANGE BOTH
//returns false
bool
SimpleWord::stopearly (int min)
{
  return false;
}

unsigned 
PositionalWord::score (Match p)
{
  unsigned s;
  s = _s;
  
  //scale the score by the log of the position of each word in the
  //corresponding document, so words appear in the beginning of the
  //text score higher.
  s = s/(int)log((double)((tagmask & p.s.wc)+2));

  //multiply the score based on tag values
  word_pos mask = 0x1 << (sizeof(word_pos)*8-Word::numtags);
  for (unsigned i = 0; i < Word::numtags; i++) {
    if (mask & p.s.wc) 
      s *= Word::tagsmulti[i];
    mask = mask >> 1;
  }
  return s;
}

//This function is closely related to score(), ONE MUST CHANGE BOTH
//returns false
bool
PositionalWord::stopearly(int min)
{
  return false;
}

Aggregate::Aggregate (QueryOp *in, unsigned n) : 
  _in (in), 
  _sep (in->sep ()), 
  _n(n)
{
  _curr_m = (Match *) malloc (sizeof(Match) * n);
  _cs = _ce = 0;
  _curr_m[0].s.dn = 0;
  assert(this!=in);
}

Aggregate::~Aggregate() {
  delete _in;
  delete [] _curr_m;
}

unsigned
Aggregate::max()
{
  if (_eof)
    return _max;
  else {
    unsigned m = _max;
    if (_in->progress() > 0.0) 
       m = (unsigned) (m/_in->progress());
    return m;
  }
}

Match
Aggregate::advance (PostIt to)
{
  vector<Match> v;
  v.clear ();

  Match m;
  m.s.dn = m.e.dn = 0;
  _advance_stats++;
  if ((to.dn || to.wc ) 
    || (_sep && to > _sep->curr().e) 
    || (!_sep && to.dn > _in->curr ().s.dn))
    m = _in->advance (to);
  if (_in->eof()) {
    _eof = true;
    m.s.dn = m.e.dn = 0;
  }
  return m;
}

Match
Aggregate::next ()
{
  _next_stats++;
  if (_cs < _ce) 
    return _curr_m[++_cs];

  Match m;
  _cs = _ce = 0;
  if (!_eof && !_curr_m[0].s.dn) { //it is a fresh start
    m = _in->next ();
    _curr_m[_ce++] = m;
  } else
    m = _in->curr ();

  if (_in->eof ()) {
    _eof = true;
    m.s.dn = m.e.dn = 0;
    return m;
  }

  Match cr = Word::getrarest ()->curr ();
  if (_sep)
    _curr_sep = _sep->curr ();

  int s = 0; 
  int maxs = Aggregate::maxcount;
  int i = 0;
  int best = 0;
  Match p, next_sep;
  while (1) {
    p = _in->next ();
    if (maxs > 0) {
      s += m.score;
      maxs--;
    }
    if (_sep)
      next_sep = _sep->curr ();
    if (p.s.dn && 
        ((_sep && _curr_sep.s == next_sep.s)
         || (!_sep && (p.s.dn == m.s.dn)))) {
      if ((p.s != p.e) && cr.s != Word::getrarest ()->curr ().s) {
        for (i = best-1; i >= 0; i--) {
          if (_curr_m[i].score > m.score)
            break;
          if (i < ((int)_n-1))
            _curr_m[i+1] = _curr_m[i];
        }
        i++;
        if (i < (int)_n) {
          _curr_m[i] = m;
          if (i==best) {
            best++;
            if (best > (int)_ce)
              _ce = best;
          }
        }
        m = p;
        cr = Word::getrarest ()->curr ();
      } else {
        if (p.score > m.score) 
          m = p;
        if (_ce < _n)
          _curr_m[_ce++] = p;
      }
    } else {
      if ((m.s.dn) && (_ce < _n))
        _curr_m[_ce++] = m;
      _ce--;
      for (i = 0; i <= (int)_ce; i++)
        _curr_m[i].score = s;
      return _curr_m[_cs];
    }
  }
}

void
Aggregate::printstats()
{
  fprintf(stderr, "queryop: Aggregate nexts %u advances %u\n", _next_stats, _advance_stats);
  _in->printstats();
}

bool
Aggregate::stopearly (int s)
{
  return _in->stopearly (s / Aggregate::maxcount);
}

