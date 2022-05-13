#include <assert.h>
#include <iostream>
#include <unistd.h>
#include <map>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <pthread.h>

#include "and.h"
#include "between.h"
#include "../mkdb/args.h"
#include "docnomanager.h"

#define CPUS 48

#define LEADSPECIAL(x) (x==':' || x=='_')
#define PARENT(x) (x == '[' || x == ']' || x == '(' || x == ')')
#define CONTEXTSENTENCESTOP(c) (c == '.' || c=='?' || c=='!' || c == ';' || c == ':')

const char *tmpdir;
extern char * xmmap(size_t len, int fd, off_t offset, void *&realp, size_t& realsz);
Args *a;
int ncore = 1;
int verbose = 0;
string prefix;
bool translate = false;
bool interactive = true;
int stopearly;
int page_size = 1<<12;
int page_shift = 12;
bool distinct = true;
int output_offset;
unsigned num_context = 1;
unsigned num_context_words = 5;
unsigned maxmax = 0;
QueryOp *test;
bool first_run = true;
pthread_t tha[CPUS];
unsigned int start = 0;
unsigned int topnum = 20;
ind_offset *ff_index = NULL;
DID max_dn = 0;
int ffd = -1;
char *qs;
map<string, bool> stoplist;
pthread_mutex_t output_lock;
map<string, ind_offset> w2p; 
DB *n2f_db = NULL;

struct doccontext {
  DID dn;
  vector<word_pos> v;
  int s;
};

typedef struct {
   double total;
   double construct_word;
   double search;
   double display_context;
   
   int n_pg;
   int n_corepg;
}statistics;

double gettime()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (((double)tv.tv_sec)*1000000.0+(double)tv.tv_usec);
}


int
read_context(struct doccontext dc, char *buf, unsigned maxc, statistics &stats)
{
   char *fbuf;
   unsigned fsz;
   int ret = 0;
   
   //read in all
   if (dc.dn < max_dn) 
      fsz = ff_index[dc.dn]-ff_index[dc.dn-1];
   else {
      fsz = (lseek(ffd, 0L, SEEK_END) - ff_index[dc.dn-1]);
   }
  
   unsigned max_pg = (fsz >> page_shift)+1;
   int fbuf_offset;
#if linux
   unsigned char vec[max_pg];
#else
   char vec[max_pg];
#endif

   void *fbufreal;
   size_t fbufsz;
   fbuf = (char *)xmmap(fsz, ffd, ff_index[dc.dn-1], fbufreal, fbufsz);
   if (fbuf == MAP_FAILED) {
      fprintf(stderr, "mmap sz %d fd %d offset %llu %lu\n", fsz, ffd, ff_index[dc.dn-1], sizeof(off_t));
      perror("qe.read_context: mmap failed: ");
      return ret;
   }

   fbuf_offset = (long long)fbuf % page_size;
   if (verbose > 0 && 
	 (mincore(fbuf-fbuf_offset, fsz+fbuf_offset, vec))!= 0) 
      perror("qe.read_context: mincore");

   unsigned bi = 0, j = 0, k = 0, w = 0;
   bool inword = false;
   /* i don't like duplicates in dc.v */
   vector<word_pos> v;
   v.clear();
   for (unsigned i = 0; i < dc.v.size(); i++) {
     if (dc.v[i] < fbufsz && (v.empty() || v.back()!= dc.v[i]))
       v.push_back(dc.v[i]);
   }

   unsigned startj;
   for (unsigned i = 0; i < v.size(); i++) {

     if (j < v[i])
       j = v[i];
     else
       continue;

      w = 0;
      inword = false;
      while (j > 0 && w <= num_context_words) {
        if (WORDCHAR(fbuf[j])) 
          inword = true;
        else if (inword) {
          w++;
          inword = false;
        }
        j--;
      }

      startj= j;
      w = 0;
      inword = false;
      j++;
      while (j < fsz && w <= (2*num_context_words )) {
        buf[bi++] = fbuf[j++];
        if ((bi+1) >= maxc || j >= fsz) {
          ret = 1;
          goto done;
        }
        if (WORDCHAR(fbuf[j]))
          inword = true;
        else if (inword) {
          w++;
          inword = false;
        }
      }

      for (k = 0; k < 3; k++) {
        buf[bi++] = '.';
        if ((bi+1) == maxc) {
          ret = 1;
          goto done;
        }
      }

      if (verbose > 0) {
        startj = (startj  + fbuf_offset)>> page_shift;
        while (startj <= ((j + fbuf_offset)>>page_shift)) {
          if ((vec[startj] % 2) > 0)
            stats.n_corepg++;
          startj ++;
          stats.n_pg++;
        }
      }
   }
done:
   buf[bi] = '\0';
   assert (bi < maxc);
   if (munmap(fbufreal, fbufsz)!=0)
     perror("qe.read_context: munmap");
   return ret;
}

QueryOp*
alloc_word (string which, string w, FILE *f, DB *db, unsigned maxlen, 
	    map<string, QueryOp * > &words2qo)
{
  if (distinct) {
    map<string, QueryOp *>::iterator i;
    i = words2qo.find (w);
    if (i != words2qo.end ()) {
      if (verbose > 1)
	fprintf(stderr, "%s is duplicate\n", w.c_str());
      return words2qo [w];
    }
  }

  map<string, bool>::iterator si;
  si = stoplist.find(w);
  if (si != stoplist.end()) {
    if (verbose > 1)
      fprintf (stderr, "%s stopped!\n", w.c_str());
    return NULL;
  }else if (stoplist.size ()> 0 && w.length() < 2)
    return NULL;

  Word *o;
  if (which == "SepWord") {
    o = new SepWord (w, f, db, w2p, maxlen);
  }else {
    if (which == "SimpleWord")
      o = new SimpleWord (w, f, db, w2p, maxlen);
    else if (which == "PositionalWord")
      o = new PositionalWord (w, f, db, w2p, maxlen);
    else {
      o = new Word (w, f, db, w2p, maxlen);
    }
    if (o->max () > maxmax) 
      maxmax = o->max ();
    if (!Word::getrarest () || o->max () < Word::getrarest ()->max ())
      Word::setrarest (o);
  }

  //  if (w == "a") 
  // test= o;

  if (distinct) 
    words2qo[w] = o;

  //  allwords.push_back(o);
  return o;
}

And *
create_and (string which, vector<QueryOp *> in) 
{
  if (which == "DistAnd") 
    return new DistAnd (in);
  else 
    return new And (in);
}

string
get_name_from_no(DB *db, DID dn)
{
  DBT key,data;
  bzero(&key,sizeof(key));
  bzero(&data,sizeof(data));
  key.data = &dn;
  key.size = sizeof(dn);
  int err = db->get(db, NULL, &key,&data,0);
  if (err != 0) {
    char tmpbuf[100];
    sprintf(tmpbuf,"no such doc num %lld ", dn);
    return string(tmpbuf);
  } else
    return string((char *)data.data);
}

vector<string>
tokenize_query (char *q)
{
  char buf[100];
  int i = 0;
  char c;
  vector<string> v;

  while (1) {
    if (q) {
      c = *q;
      q++;
    }else{
      c = getchar ();
      if (c == EOF)
	c = '\0';
    }
    if (WORDCHAR(c)) 
      buf[i++] = tolower(c);
    else if (((!i) && LEADSPECIAL(c))
	     || ((i && LEADSPECIAL (buf[0]) && c && c != ' ' && !PARENT(c))))
      buf[i++] = c;
    else {
      if (i) {
	buf[i] = '\0';
	assert(i < 100);
	v.push_back (string (buf));
	i = 0;
      }
      if (PARENT(c)) {
	buf[i++] = c;
	buf[i] = '\0';
	v.push_back (string(buf));
	i = 0;
      }
      if (!c)
	return v;
    }
  }
}

//the query string must be in postorder form?
QueryOp *
parse_query (char *q, string wordtype, 
	     string andtype, FILE *f, DB *w2p_db, unsigned maxwordlen,
	     map<string, QueryOp * > &words2qo)
{
  QueryOp *sep = NULL;
  string w;
  vector<string> tokens;
  vector<string> parent;
  vector<QueryOp *> in;
  QueryOp *out;
  vector<QueryOp *> parsed;

  tokens = tokenize_query (q);
  parsed.clear();

  if (verbose > 1) {
    for (unsigned i = 0; i < tokens.size (); i++) 
      fprintf(stderr,"%s ", tokens[i].c_str());
    fprintf(stderr, "\n");
  }

  for (unsigned i = 0; i < tokens.size (); i++) {
    w = tokens[i];
    if (w[0] == ':' || w == "]" || w == ")") {
      if (w[0] == ':')
	sep = alloc_word (string ("SepWord"), w, f, w2p_db, maxwordlen, 
			  words2qo);
      else
	sep = NULL;
      in.clear ();
      out = NULL;
      while (!parsed.empty()) {
	out = parsed.back ();
	parsed.pop_back ();
	if (out) 
	  in.push_back (out);
	else
	  break;
      }
      if (in.size () == 0) {
	continue; // parens with no words inside
      } else if (in.size () > 1) 
	out = create_and (andtype, in);
      else
	out = in[0];
      if (sep) {
	 out = new Between (out, sep);
	 //default is to aggregate
	 if ((i == (tokens.size ()-1)) 
	     || ((tokens[i+1] != "]") && (tokens[i+1] != ")"))) {
	   out = new Aggregate (out, num_context);
	 }
      } else {
	if (w[0] == ']') 
	  assert (parent.back () == "[");
	else {
	  assert (parent.back () == "(");
	  out = new Aggregate (out, num_context);
	}
	parent.pop_back ();
      }
      if (out) {
	parsed.push_back (out);
      }
    } else if (w == "[" || w == "(") {
      parent.push_back (w);
      parsed.push_back (NULL); //use NULL to delimeter a parenthesis
    } else {
      out = alloc_word (wordtype, w, f, w2p_db, maxwordlen, words2qo);
      if (out) {
	parsed.push_back (out);
      }
    }
  } 

  if (parsed.size () > 1)  {
    return (new Aggregate (create_and (andtype, parsed), num_context)); //default is to aggregate
  } else if (parsed.size () == 1) {
    if ((tokens.size () && tokens.back () == "]")
	|| parsed.back ()->name () == "Aggregate")
      return parsed.back ();
    else
      return (new Aggregate (parsed.back (), num_context));
  } else
    return NULL; 
}

list<doccontext>
resolve_one_query (int &totalmatch, QueryOp *out)
{
   list<doccontext>::iterator iter; 
   list<doccontext> tops;
   doccontext dc;
   
   tops.clear();
   dc.dn = 0;

   while (1) {
      
      Match m = out->next ();

      if (stopearly && (tops.size() == start+topnum)
	    && out->stopearly(tops.front().s)) {
	 if (verbose > 0) 
	    fprintf (stderr, "stopearly: doc %lld,%u no need to search beyond %lld\n", 
		  tops.front().dn, tops.front().s, m.s.dn);
	 break;
      }

      if (dc.dn != m.s.dn) {
	 if (dc.dn) {
	    for (iter = tops.begin (); iter != tops.end (); ++iter) {
	       if (dc.s < (*iter).s)
		  break;
	    }
	    
	    if (iter != tops.begin () || tops.size () < start + topnum) 
	       tops.insert (iter, dc);
	    
	    if (tops.size () > start+topnum) 
	       tops.pop_front ();
	    totalmatch++;
	 }
	 
	 if (m.s.dn) {
	    dc.v.clear ();
	    dc.dn = m.s.dn;
	    dc.s = m.score;
	    dc.v.push_back (m.s.wc);
	    dc.v.push_back (m.e.wc);
	 }
      } else if (m.s.dn) {
	dc.v.push_back (m.s.wc);
	dc.v.push_back (m.e.wc);
      }
    
      if (!m.s.dn || out->eof ()) {
	 if (verbose > 0)
	    fprintf (stderr, "stopearly: all\n");
	 break;
      }
   }
   return tops;
}

unsigned long long 
load_db(DB *w2p_db)
{
   DBT key, data;
   DBC *dbc;
   ind_offset offset;
   int r;

   r = w2p_db->cursor(w2p_db, NULL, &dbc, 0);
   assert(r == 0);

   unsigned long long n  = 0;

   while (1) {
      bzero(&key,sizeof(key));
      bzero(&data,sizeof(data));
      r = dbc->c_get(dbc, &key, &data, DB_NEXT);
      if (r!= 0)
	 break;
      offset = *(ind_offset *) data.data;
      w2p[string((char *)key.data)] = offset;
      n++;
   }
   return n;
}

void
parse(char *querystring)
{
  char *c;

  qs = querystring;
  while (1) {
    if (strcasestr(qs, "topnum:") == qs) {
      qs += strlen("topnum:");
      if ((c = strchr(qs, ','))) { 
	*c = '\0';
	start = atoi(qs);
	qs = c + 1;
	if (((c = strchr(qs, ' ')) != NULL 
	     || (c = strchr(qs, '\n')) != NULL)) {
	  *c = '\0';
	  topnum = atoi(qs);
	  qs = c + 1;
	}
      }
    }else if (strcasestr(qs, "context:") == qs) {
      qs += strlen("context:");
      if (((c = strchr(qs, ' ')) != NULL
	   || (c = strchr(qs, '\n')) != NULL)) {
	*c = '\0';
	num_context = atoi(qs);
	qs = c + 1;
      }
    }else
      break;
  }
}

void
display(list<doccontext> tops, statistics &stats, int totalmatch, QueryOp *out)
{

  pthread_mutex_lock(&output_lock);

  printf("total match %lu %u %d %.2f\n", (tops.size ()>(start+topnum))?topnum:(tops.size ()-start),
	 totalmatch, stopearly?(unsigned)(totalmatch/out->progress()):totalmatch, out->progress());

  if (tops.size() > 0) {
    unsigned i;
    list<doccontext>::reverse_iterator iter; 
    for ( iter = tops.rbegin(), i = 0; 
	  (i < (start + topnum) && iter != tops.rend ()); 
	  i++, ++iter) {
      if (i >= start) {
	if (translate) 
	  printf("%u:%s:", (*iter).s, get_name_from_no(n2f_db, (*iter).dn).c_str());
	else
	  printf("%u:%lld:", (*iter).s, (*iter).dn);
	sort ((*iter).v.begin (), (*iter).v.end ());
	if (verbose || !output_offset) {
	  unsigned vsz = (*iter).v.size ();
	  for (unsigned x = 0; x < vsz; x++) {
	    if (!x || (*iter).v[x] != (*iter).v[x-1]) {
	      printf("%u", (*iter).v[x]);
	      if ((*iter).v[x]!=(*iter).v[vsz-1] || verbose)
		printf(",");
	    }
	  }
	}

	if (output_offset) {
	  char tmpbuf[10000];
	  int r = read_context((*iter), tmpbuf, 10000, stats);
	  if (verbose)
	    printf("(%lu %s)", strlen(tmpbuf), r?"incomp":"");
	  printf("%s", tmpbuf);
	} 

	printf("\n");
      }
    }
  }
  fflush(stdout);
  pthread_mutex_unlock(&output_lock);
}


void *
doqueryfile(void *arg)
{
  char dbname[100];
  char filename[100];
  int cid = (long long) arg;
  DB *w2p_db = NULL;
  FILE *f;
  int err;
  double ts_begin, ts1, ts2;
  statistics stats;
  int totalmatch = 0;
  map<string, QueryOp * > words2qo;

  if (verbose > 0)  {
    ts_begin = gettime();
    stats.n_pg = 0;
    stats.n_corepg = 0;
    stats.display_context = 0;
  }

  sprintf(filename, "%s%d/%s-f-%d", tmpdir, cid, prefix.c_str(), cid);
  printf("%s\n", filename);

  f = fopen(filename,"r");
  if (!f) {
    fprintf(stderr, "error opening %s\n", filename);
    perror("qe.C: open ");
    exit(1);
  }

  sprintf(dbname, "%s%d/%s-w2p.db-%d.0", tmpdir, cid, prefix.c_str(), cid);
  err = db_create(&w2p_db, NULL, 0);
  assert(!err);
  err = w2p_db->open(w2p_db, NULL, dbname, NULL, DB_BTREE, DB_RDONLY,  0666);
  if (err) {
    fprintf(stderr, "failed to open %s\n", dbname);
    exit(1);
  }


  printf("%d: run query with %s and %s\n", cid, filename, dbname);

  QueryOp *out = parse_query (qs, a->sget("word", "Word"), 
			      a->sget("and", "And"), f, w2p_db, 
			      a->nget("maxwordlen",100), words2qo);

  //  printf("%d: parse_query with %s and %s done\n", cid, filename, dbname);
  if (w2p_db) 
    w2p_db->close(w2p_db,0);

  if (verbose > 0) {
    ts2 = gettime ();
    stats.construct_word = ts2 - ts_begin;
  }

  if (verbose > 1)
    fprintf (stderr, "posting list has max len %d\n", maxmax);

  if (!out) {
    fprintf (stderr, "no keywords found\n");
    if( interactive ) {
      cout << "searchydone" << endl;
    }
    return NULL;
  }

  list<doccontext> tops = resolve_one_query (totalmatch, out);
  if (verbose > 0) {
    ts1 = gettime ();
    stats.search = ts1 - ts2;
  }

  display(tops, stats, totalmatch, out);

  if (verbose > 0) {
    ts2 = gettime ();
    stats.display_context = (ts2-ts1); 

    fprintf(stderr, "qs: %s ", qs);
    fprintf(stderr, "matches: %lu %u %u %.2f\n", 
	    (tops.size() > (start+topnum))?topnum:(tops.size()-start), 
	    totalmatch, 
	    stopearly?(unsigned)(totalmatch/out->progress()):totalmatch, 
	    out->progress());
    if (out)
      out->printstats();
    stats.total =  ts2 - ts_begin; 
    int qe_pg = 0, qe_corepg = 0;
    //    for (unsigned xx = 0; xx < allwords.size(); xx++) {
    //qe_pg += allwords[xx]->get_pg();
    //qe_corepg += allwords[xx]->get_corepg();
    //}
    fprintf(stderr, "pagehits: %d %d %d %d\n", 
            qe_pg, qe_corepg, stats.n_pg, stats.n_corepg);
    fprintf(stderr, "timing: %.1f, %.1f %.1f %.1f (in us)\n",stats.total, 
            stats.construct_word, stats.search, stats.display_context);

    fflush(stderr);
  }

  if (verbose > 1)  {
    fprintf (stderr, "found %u matches \n", totalmatch);
    if (out)
      out->printstats();
  }

  words2qo.clear();
  fclose(f);
  return NULL;
}

void
doquery(char *querystring)
{
  parse(querystring);

  if (verbose > 1)
    fprintf(stderr, "start %d num %d context %d qs %s\n", start, topnum, num_context, qs);

  void *value;
  for(int i = 0; i < ncore; i++)
    pthread_create(&(tha[i]), NULL, &doqueryfile, (void *) i);

  for(int i = 0; i < ncore; i++)
    assert(pthread_join(tha[i], &value) == 0);
}


int
main(int argc, char *argv[])
{
   const char *config = "mkdb.config";
   char querystring[1001];
   querystring[1000] = '\0';
   char ch;
   char *sep;
   FILE *sf;

   pthread_mutex_init(&output_lock, NULL);

   unsigned maxwordlen;
   string filename;
   unsigned numtags;
   unsigned *tags;
   unsigned default_context = num_context;

  tmpdir = "/tmp";
  if(getenv("TMPDIR"))
    tmpdir = getenv("TMPDIR");

   page_size = getpagesize();
   page_shift = 0;
   int i = page_size;
   while (i > 1) {
     i = i >> 1;
     page_shift++;
   }

   while ((ch = getopt(argc, argv, "f:q:n:luv:dc:w:c:t:")) != -1) {
     switch (ch) {
     case 'v':
       verbose = atoi(optarg);
       break;
     case 'd':
       distinct = false;
       break;
     case 'f':
       config = optarg;
       break;
     case 'q':
       strncpy (querystring, optarg, 1000);
       interactive = false;
       break;
     case 'n':
       start = atoi(optarg);
       sep = strchr(optarg,',');
       if (sep) 
	 topnum = atoi(sep+1);
       break;
     case 'l':
       translate = true;
       break;
     case 'x':
       num_context = default_context = atoi(optarg);
       break;
     case 'w':
       num_context_words = atoi(optarg);
       break;
     case 'c':
       ncore = atoi(optarg);
       break;
     case 't':
	tmpdir = optarg;
	break;
     default:
       cerr << "usage: qe [-t tmpdir] [-n topnum] [-q querystring] [-f configfile] [-l translate=true] -[c ncore]" << endl;
       exit(1);
     }
   }

   a = new Args(config);
   maxwordlen = a->nget("maxwordlen",100);
   prefix = a->sget("prefix","ind");

   //read in stopword file
   char line[1000];
   int n;
   string stopf = a->sget("stopword","stopword");
   sf = fopen(stopf.c_str(), "r");
   if (sf) {
      while (!feof(sf)) {
	 n = fscanf(sf, "%s\n", line);
	 stoplist[string(line)] = true;
      }
      fclose(sf);
   }

   //read in context file offsets if possible
   output_offset =  a->nget("outputoffset",0);
   if (output_offset) {
      string ffname = prefix;
      ffname.append("-ffi");
      FILE *ff = fopen(ffname.c_str(), "r");
      fseek(ff, 0L, SEEK_END);
      ind_offset fsz = ftello(ff);
      max_dn = fsz/sizeof(ind_offset);
      if (interactive) 
	 ff_index = (ind_offset *) malloc(sizeof(ind_offset) * max_dn);
      else 
	 ff_index = (ind_offset *) mmap(0, fsz, PROT_READ, MAP_SHARED, fileno(ff), 0);
      if (!ff_index) {
	 fprintf(stderr, "not enough memory for %lld context index\n", max_dn);
	 exit(1);
      }
      if (interactive) {
	 rewind(ff);
	 size_t r = fread (ff_index, sizeof(ind_offset), max_dn, ff);
	 if (r != max_dn) {
	    fprintf(stderr, "read in ff_index wrong %lu %lld\n", r, max_dn);
	    exit(1);
	 }
      }
      fclose (ff);
      ffname = prefix;
      ffname.append("-ff");
#if linux
      ffd = open(ffname.c_str(), O_RDONLY|O_LARGEFILE);
#else
      ffd = open(ffname.c_str(), O_RDONLY);
#endif
   }


   if (translate) {
     char n2f_dbname[100];
     int err;
     sprintf(n2f_dbname, "%s%d/%s-n2f.db", tmpdir, 0, prefix.c_str());
     err = db_create(&n2f_db, NULL, 0);
     assert(!err);
     err = n2f_db->open(n2f_db, NULL, n2f_dbname, NULL, DB_BTREE, DB_RDONLY, 0666);
     if (err) {
       fprintf(stderr, " %s failed to open\n", n2f_dbname);
       exit(1);
     }
   }

   stopearly = a->nget("stopearly",0);
   numtags = a->nget("numtags",0);
   tags = (unsigned *) malloc( sizeof(unsigned)*numtags );
   char tname[100];
   for (unsigned i = 0; i < numtags; i++) {
      sprintf(tname,"tag%u",i);
      tags[i] = a->nget(string(tname),1);
   }
   Word::settags(numtags, tags);

   if( interactive ) {
     cout << "searchydone" << endl;
   }

   do {
      if (interactive && !fgets(querystring, 1000, stdin))
	 break;
      
      doquery(querystring);

      if( interactive ) {
        cout << "searchydone" << endl;
      }

      //clean up state, reset all query parameters to default
      //      allwords.clear();

      Word::setrarest(NULL);
      start = 0;
      topnum = 20;
      num_context = default_context;

   } while (interactive && !feof(stdin));

  if (n2f_db)
      n2f_db->close(n2f_db,0);
  if (ffd > 0)
      close(ffd);
}
