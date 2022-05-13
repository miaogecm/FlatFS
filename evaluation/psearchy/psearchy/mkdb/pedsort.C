/*
 * parallel version of edsort
 */

#define LINUX

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <db.h>
#include <pthread.h>
#include <sched.h>
#ifdef LINUX
#include <wait.h>
#include <linux/unistd.h>
#endif
#include "args.h"
#include "mkdb.h"


#include <set>
#include <vector>

// #define COUNTER

const char *tmpdir;
const char *config = "mkdb.config";
pthread_mutex_t input_lock;
int *cpuseq;
extern int errno;
unsigned maxwordlen;
string prefix;
int ncore = 1;
long long maxmem = 256*1024*1024;
DID did = 1;
DID max_did = 1;
int first = 1;
int order = 0;
int threaded = 1;
int dblim = 0;

extern int nprimes, primes[];

#define NFILES 1000000
#define MAXFILENAME 1024
#define MaxFDS 800

char files[NFILES][MAXFILENAME];

#define BLOCKSIZE 128
struct Block {
  struct Block *next;
  int n; //number of groups of Tags
  PostIt* p[BLOCKSIZE];
};

struct Bucket {
  char *word;
  struct Block *b0;
  struct Block *bN;
  unsigned n; //number of blocks
};

#define NBYTES   (maxmem/4)  // number of bytes per data structure

struct pass0_state {
  int maxinfo;
  int maxword;
  int maxhash;
  char *wordbytes;
  PostIt *infobuf;
  struct Bucket *table;
  int wordi;
  int infoi;
  int maxblocks;
  struct Block *blocks;
  int blocki;
  int buckets_used;
  int nstrcmp;
};

int hash_primes[] = { 0, 0, 0, 0, 0, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741};

bool update_only;
void *dofiles(void *arg);
void cleanup(int cid, int exit_status, int pass0files);
int pass0(int cid, FILE *input, DID did, int *pass0files,  struct pass0_state *ps);
void passN(int cid, char *outfile, char *oldoutfile, int pass0files);
float printrusage(int init);
void flushwords(int cid, struct pass0_state *ps, int *pass0files);

static struct sharedmem {
  volatile int run;
  volatile int first;
  volatile int did;
  volatile uint64_t tot;
} *shared;

#ifdef LINUX

#ifdef COUNTER

#define NPMC 1
static uint64_t pmccount[NPMC];

static inline uint64_t read_pmc(uint32_t ecx)
{
  uint32_t a, d;
  __asm __volatile("rdpmc" : "=a" (a), "=d" (d) : "c" (ecx));
  return ((uint64_t) a) | (((uint64_t) d) << 32);
}

static void read_counters(int cid)
{
  static uint64_t (*pmclast)[NPMC];   // [core][num counters]
  if (!pmclast) {
    pmclast = new uint64_t[ncore][NPMC];
    memset(pmclast, 0, ncore * sizeof *pmclast);
  }

  for (int i = 0; i < NPMC; i++) {
    // if (SOCKETCORE(cid) == 0) {
      uint64_t pmc = read_pmc(i);
      if (pmclast[cid][i])
        pmccount[i] += pmc - pmclast[cid][i];
      pmclast[cid][i] = pmc;
      // }
  }
}

#endif

__thread int thread_cpu = 0;

#define NCPU	8	

static inline void atomic_inc(volatile int *n)
{
  asm volatile("lock; incl %0" : "+m" (*n));
}


static inline int atomic_add_return(int i, volatile int *n)
{
  int __i = i;
  asm volatile("lock; xaddl %0, %1"
               : "+r" (i), "+m" (*n)
               : : "memory");
  return i + __i;
}

static inline int atomic_add64_return(uint64_t i, volatile uint64_t *n)
{
  int __i = i;
  asm volatile("lock; xaddq %0, %1"
               : "+r" (i), "+m" (*n)
               : : "memory");
  return i + __i;
}

static pid_t gettid(void)
{
  return syscall(__NR_gettid);
}

void
set_affinity(int cpu_id)
{
 int tid = gettid();
 cpu_set_t mask;
 CPU_ZERO(&mask);
 //CPU_SET(((cpu_id<<2)|(cpu_id>>2)) & 15, &mask);
 CPU_SET(cpu_id, &mask);

 // printf("set_affinity: %d %d\n", tid, cpu_id);

 int r = sched_setaffinity(tid, sizeof(mask), &mask);
 if (r < 0) {
   fprintf(stderr, "couldn't set affinity for %d\n", cpu_id);
   exit(1);
 }
}

static int get_affinity() {
	cpu_set_t mask;
	int i;
	
	if (sched_getaffinity(0, sizeof(mask), &mask) == -1){
		printf("get_cpu failed\n");
    }
	
	for(i = 0; i < NCPU; i++) {
		if (CPU_ISSET(i, &mask))
			return i;
	}
	return -1;
}

static int __attribute__((unused))
get_npfs(void)
{
  char fn[128];
  snprintf(fn, sizeof(fn), "/proc/%u/stat", getpid());
  FILE *proc = fopen(fn, "r");
  assert(proc);

  char buf[4096];
  size_t r = fread(buf, 1, sizeof(buf) - 1, proc);
  assert(r > 0);
  buf[r] = 0;

  char *s = buf;
  for (int k = 0; k < 9; k++) {
    s = strchr(s, ' ');
    s++;
  }

  return atoi(s);
}

#endif

static int find_prime_recurse(int l, int u, int n)
{
  int i = (u - l) / 2 + l;

  if (primes[i] <= n && n <= primes[i+1])
    return primes[i+1];

  if (primes[i] > n) 
    return find_prime_recurse(l, i, n);
  else
    return find_prime_recurse(i, u, n);
}

static int find_prime(int n)
{
  return find_prime_recurse(0, nprimes-1, n);
}

struct cpuinfo
{
  int proc, phys;
};

static void get_cpu_sequence(int order, int *seq)
{
  if (getenv("CPUSEQ")) {
    char *cpuseq = strdup(getenv("CPUSEQ"));
    char *tok, *pos = cpuseq;
    int n = 0;
    while ((tok = strsep(&pos, ",")) && n < ncore) {
      seq[n++] = atoi(tok);
    }
    free(cpuseq);

    if (n < ncore) {
      fprintf(stderr, "Number of cores requested %d > CPUSEQ %d",
              ncore, n);
      exit(-1);
    }
    return;
  }

  // Parse cpuinfo file
  std::vector<cpuinfo> cpus;
  
  FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
  if (cpuinfo == NULL) {
    perror("failed to open /proc/cpuinfo");
    exit(-1);
  }

  char line[1024];
  struct cpuinfo cur;
  while (fgets(line, sizeof line, cpuinfo)) {
    int *val = NULL;
    if (strncmp(line, "processor\t", 10) == 0)
      val = &cur.proc;
    else if (strncmp(line, "physical id\t", 12) == 0)
      val = &cur.phys;
    if (val)
      *val = atoi(strchr(line, ':')+1);

    if (line[0] == '\n')
      cpus.push_back(cur);
  }

  fclose(cpuinfo);

  if (ncore > (int)cpus.size()) {
    fprintf(stderr, "Number of cores requested %d > available cores %d\n",
            ncore, (int)cpus.size());
    exit(-1);
  }

  if (order == 0) {
    // Sequential
    for (int i = 0; i < ncore; ++i)
      seq[i] = cpus.at(i).proc;
  } else {
    // Round-robin
    int maxphys = 0;
    for (unsigned int i = 0; i < cpus.size(); ++i)
      if (cpus[i].phys > maxphys)
        maxphys = cpus[i].phys;

    int i = 0;
    while (true) {
      // Take one processor from each physical chip
      assert(!cpus.empty());
      std::set<int> phys;
      std::vector<struct cpuinfo>::iterator it;
      for (it = cpus.begin(); it != cpus.end();) {
        if (!phys.count(it->phys)) {
          phys.insert(it->phys);
          seq[i++] = it->proc;
          if (i == ncore)
            return;
          it = cpus.erase(it);
        } else {
          ++it;
        }
      }
    }
  }
}

static void 
initshared(void)
{
  if (threaded) {
    shared = (struct sharedmem *) malloc(sizeof(struct sharedmem));
    assert(shared);
  } else {
    shared = (struct sharedmem *) mmap(0, sizeof(struct sharedmem), PROT_READ|PROT_WRITE, 
                                       MAP_SHARED|MAP_ANONYMOUS, 0, 0);
    if (shared == MAP_FAILED) {
      perror("mmap failed");
      exit(-1);
    }
  }
  shared->did = 1;
  shared->first = 1;
}

int
main(int argc, char *argv[])
{
  char ch;
  char *path;

  printrusage(1);

  tmpdir = "/tmp";
  if(getenv("TMPDIR"))
    tmpdir = getenv("TMPDIR");

  update_only = false;

  while ((ch = getopt(argc, argv, "t:f:u:c:m:s:x:pl:")) != -1) {
    switch (ch) {
      case 't':
        tmpdir = optarg;
        break;
      case 'f':
        config = optarg;
        break;
      case 'u':
        update_only = true;
        break;
      case 'c':
        ncore = atoi (optarg);
        break;
      case 'm':
        maxmem = atoi (optarg);
        maxmem = maxmem * 1024*1024;
        break;
      case 's':
        order = atoi(optarg);
        break;
      case 'x':
 		path = optarg;
		break;
      case 'p':
        threaded = 0;
        break;
      case 'l':
        dblim = atoi(optarg);
        break;
      default:
        break;
    }
  }
  argc -= optind;
  argv += optind;
  if (argc != 0) {
    fprintf(stderr,"./pedsort [-t tmpdir] [-u (update)] [-f config_file] [-c ncore] [-s sched] [-x path] [-p] [-l dblim]\n");
    exit(1);
  }
  Args *a = new Args(config);
  maxwordlen = a->nget<unsigned>("maxwordlen", 100);
  prefix = (a->sget("prefix","ind")).c_str();

  pthread_mutex_init(&input_lock, NULL);

  //  printf("npfs: %d\n", get_npfs());

  cpuseq = new int[ncore];
  get_cpu_sequence(order, cpuseq);

  // Increase my FD limit as much as possible
  struct rlimit fdlim;
  if (getrlimit(RLIMIT_NOFILE, &fdlim) < 0) {
    perror("getrlimit failed");
    exit(-1);
  }
  fdlim.rlim_cur = fdlim.rlim_max;
  if (setrlimit(RLIMIT_NOFILE, &fdlim) < 0) {
    perror("setrlimit failed");
    exit(-1);
  }

  char n2f_dbname[100];
  DB *n2f_db;
  int err;
  sprintf(n2f_dbname, "%s%d/%s-n2f.db", tmpdir, 0, prefix.c_str());
  err = db_create(&n2f_db, NULL, 0);
  if (err) {
    fprintf(stderr, "failed to create db %s\n", strerror(errno));
    exit(2);
  }
  err = n2f_db->open(n2f_db, NULL, n2f_dbname, NULL, DB_BTREE, DB_TRUNCATE|DB_CREATE, 0666);
  if (err) {
    fprintf(stderr, " %s failed to open\n", n2f_dbname);
    exit(2);
  }
  
  FILE *fp;
  fp = fopen(path, "r");
  if (fp == NULL) {
	fprintf(stderr, "fopen error\n");
	exit(0);
  }
  while (fgets(files[max_did], MAXFILENAME, fp) != NULL) {
    DBT key, data;
    assert(strlen(files[max_did]) < MAXFILENAME);
    assert(files[max_did][strlen(files[max_did])-1] == '\n');

    files[max_did][strlen(files[max_did])-1] = '\0';

    bzero(&key,sizeof(key));
    bzero(&data,sizeof(data));
    key.data = &max_did;
    key.size = sizeof(max_did);
    data.data = files[max_did];
    data.size = strlen(files[max_did]) + 1;
	
    if((err = n2f_db->put(n2f_db, NULL, &key, &data, DB_NOOVERWRITE)) != 0){
      fprintf(stderr, "mkdb: db->put failed %s\n", db_strerror(err));
      exit(1);
    }

    max_did++;

    assert(max_did < NFILES);
  }
  fclose(fp);

  if(n2f_db->close(n2f_db, 0) != 0){
    fprintf(stderr, "pedsort: db lose failed %s\n", strerror(errno));
    exit(1);
  }

   printf("max_did: %lld\n", max_did);

  fprintf(stderr, "Building index\n");
  fflush(stdout);

  initshared();
  if (threaded) {
    pthread_t *tha = new pthread_t[ncore];
    void *value;
	thread_cpu = get_affinity();
    for(int i = 0; i < ncore; i++)
      pthread_create(&(tha[i]), NULL, &dofiles, (void *) i);
    // int j = 0;
    // dofiles((void *) j);
    for(int i = 0; i < ncore; i++)
      assert(pthread_join(tha[i], &value) == 0);
    delete[] tha;
  } else {
    for (int i = 1; i < ncore; i++) {
      pid_t p;

      p = fork();
      if (p < 0) {
        perror("fork failed");
        exit(-1);
      } else if (p == 0) {  // child
        shared->run++;
        while (shared->run < ncore) ;
        dofiles((void *) i);
        return 0;
      }
    }
    shared->run++;
    while (shared->run < ncore);
    int i = 0;
    dofiles((void *) i);

    // wait until all children are done
    int stat;
    int p;
    for (i = 1; i < ncore; i++) {
      p = wait(&stat);
    }
  }
#ifdef COUNTER
  printf("tot = %llu\n", (unsigned long long)shared->tot);
#endif

  fprintf(stdout, "%d: ", ncore);
  float r = printrusage(0);
  fprintf(stdout, " time %f\n", r);
  fprintf(stdout, "aver throughput: %.2f jobs/hour/core", ((60*60)  / r) / ncore);
  fprintf(stdout, "\n");
  fprintf(stdout, "total throughput: %.2f jobs/hour", (60*60)  / r);
  fprintf(stdout, "\n");
  // printf("npfs: %d\n", get_npfs());
  exit(0);
}

struct w2p_db
{
  int cid;                      // Core id
  int sid;                      // Shard id
  DB *db;
  int count;
};

void w2p_open(struct w2p_db *w2p)
{
  char dbfile[MAXFILENAME];
  DB *db;

  assert(!w2p->db);

  //fprintf(stderr, "Core %d shard %d\n", w2p->cid, w2p->sid);

  sprintf(dbfile, "%s%d/%s-w2p.db-%d.%d", tmpdir, w2p->cid, prefix.c_str(), w2p->cid, w2p->sid);
  int err = db_create(&db, NULL, 0);
  if (err) {
    fprintf(stderr,"failed to create db %s\n", strerror(errno));
    exit(2);
  }
  err = db->open(db, NULL, dbfile, NULL, DB_BTREE, DB_TRUNCATE|DB_CREATE, 0666);
  if (err) {
    fprintf(stderr,"failed to open db %s %s\n", dbfile, strerror(errno));
    exit(2);
  }

  w2p->db = db;
  w2p->count = 0;
}

void w2p_close(struct w2p_db *w2p)
{
  assert(w2p->db);

  if(w2p->db->close(w2p->db, 0) != 0){
    fprintf(stderr, "pedsort: db %d close failed %s\n", w2p->cid, strerror(errno));
    exit(1);
  }

  w2p->db = NULL;
  w2p->sid++;
}

void *dofiles(void *arg)
{
  int cid = (long long) arg;
  char outfile[MAXFILENAME];
  char oldoutfile[MAXFILENAME];
  int pass0files = 0;
  struct pass0_state ps;
  int nfile = 0;
  int nword = 0;
  int nwordlast = 0;

#ifdef LINUX
  int c = cpuseq[cid];
  set_affinity(c);
  thread_cpu = c;
   printf("%d assigned to core %d\n", cid, c);
#endif

  sprintf(outfile, "%s%d/%s-f-%d", tmpdir, cid, prefix.c_str(), cid);
  strcpy(oldoutfile, outfile);
  strcpy(outfile+strlen(outfile),".tmp");

  ps.maxinfo = NBYTES/sizeof(PostIt);
  ps.maxword = NBYTES;
  ps.wordbytes = (char *)malloc(ps.maxword);
  ps.infobuf = (PostIt *)malloc(sizeof(PostIt)*ps.maxinfo);
  ps.maxhash = NBYTES/sizeof(struct Bucket);
  
//   for (int j = 0; j < 31; j++) {
//     if (hash_primes[j] >= ps.maxhash) {
//       ps.maxhash = hash_primes[j];
//       break;
//     }
//   }

    //fprintf(stderr, "maxhash %d\n", ps.maxhash);
  ps.maxhash = find_prime(ps.maxhash);
    //fprintf(stderr, "prime %d\n", ps.maxhash);

  ps.table = (struct Bucket *)malloc(sizeof(struct Bucket) * ps.maxhash);
  memset(ps.table, 0, sizeof(struct Bucket) * ps.maxhash);
  ps.wordi = 0;
  ps.infoi = 0;
  ps.maxblocks = NBYTES/sizeof(struct Block);
  ps.blocks = (struct Block *) malloc(ps.maxblocks * sizeof(struct Block));
  memset(ps.blocks, 0, ps.maxblocks * sizeof(struct Block));
  ps.blocki = 0;
  
  ps.nstrcmp = 0;

   //fprintf(stderr, "%d: allocate maxmem %lld info %d word %d hash %d(%d) blocks %d\n", cid, maxmem, (int) ps.maxinfo*sizeof(PostIt), ps.maxword, (int) ps.maxhash*sizeof(struct Bucket), ps.maxhash, (int) ps.maxblocks * sizeof(struct Block));

  assert(ps.wordbytes && ps.infobuf && ps.blocks && ps.table);

#ifdef COUNTER
  read_counters(cid);
#endif

  //  pthread_mutex_lock(&input_lock);
  while (1) {
    DID d = atomic_add_return(1, &(shared->did)); 
    if (shared->did >= max_did)
      break;
	
    // pthread_mutex_unlock(&input_lock);
    FILE *input = fopen((const char *) files[d], "r");
    if (input == 0) {
      fprintf(stderr, "dofiles: couldn't open %lld: %s %s\n", d, files[d], strerror(errno));
      cleanup(cid, 1, pass0files);
    }
	
    //nwordlast = pass0(cid, input, d, &pass0files, &ps);
    //nword += nwordlast;
    fclose(input);
    nfile++;
    // pthread_mutex_lock(&input_lock);
  }
  // pthread_mutex_unlock(&input_lock);
	//fprintf(stderr, "flushwords\n");
  flushwords(cid, &ps, &pass0files);

  delete ps.wordbytes;
  delete ps.infobuf;
  delete ps.blocks;
  delete ps.table;
  passN(cid, outfile, oldoutfile, pass0files);

#ifdef COUNTER
  read_counters(cid);
  for (int i = 0; i < NPMC; i++) {
    atomic_add64_return(pmccount[i], &(shared->tot));
    printf("%d: pmc %llu:\n", cid, (unsigned long long)pmccount[i]);
  }
#endif

  if (shared->first) {
    shared->first = 0;
    fprintf(stdout, "pedsort %d: nfile %d, nword %d,  hashsize %d (nstrcmp %d), pass0files %d,nwordlast %d, total: ", cid, nfile, nword, ps.maxhash, ps.nstrcmp, pass0files, nwordlast);
    float r = printrusage(0);
    fprintf(stdout, " time %f\n", r);
    fprintf(stdout, " throughput: %.2f jobs/hour/core", ((60*60)  / r) / ncore);
    fprintf(stdout, "\n");
  }
  return 0;
}

/*
 * OpenBSD fwrite() is pretty slow for short writes.
 */
int
xwrite(const void *xptr, int n, FILE *fp)
{
  int i;
  const char *ptr = (const char *) xptr;

//   if ((i = fwrite(ptr, 1, n, fp)) == EOF)
//     return(EOF);
//   return i;
  for(i = 0; i < n; i++)
    if(putc(ptr[i], fp) == EOF)
      return(EOF);
  return(i);
}

void
sorttmpname(int cid, int pass, int fn, char *buf)
{
  sprintf(buf, "%s%d/nis-%d-%d-%d-%lld", tmpdir, cid, getpid(), pass, fn, 
          (long long) pthread_self());
}

/*
 * First pass (in-core sort):
 */


int
lookup(struct pass0_state *ps, char *word)
{
  int i;
  unsigned int h;
  unsigned int k = 0;
  unsigned int o;

  for(i = 0; word[i]; i++)
    k = (k * 33) + word[i];
  h = k % ps->maxhash;
  o = 1 + (k % (ps->maxhash - 1));
  for(i = 0; i < ps->maxhash; i++){
    if(ps->table[h].word == 0)
      return(h);
    if(strcmp(ps->table[h].word, word) == 0)
      return(h);
    h += o;
    ps->nstrcmp++;
    if(h >= (unsigned int)ps->maxhash)
      h = h - ps->maxhash;
  }
  fprintf(stderr, "pedsort: hash table full\n");
  exit(1);
}

int
tablecompar(const void *xa, const void *xb)
{
  struct Bucket *a = (struct Bucket *) xa;
  struct Bucket *b = (struct Bucket *) xb;

  
  if(a->word && b->word)
    return(strcmp(a->word, b->word));
  if (a->word == 0 && b->word == 0)
    return (0);
  if(a->word == 0)
    return(1);
  if(b->word == 0)
    return(-1);
  assert (0);
}


void
flushwords(int cid, struct pass0_state *ps, int *pass0files)
{
  int i, pi;
  struct Block *bl;
  char tn[512];
  FILE *fp;

  bzero(tn, 512);
  sorttmpname(cid, 0, *pass0files, tn);
  *pass0files = *pass0files + 1;
  fp = fopen(tn, "w");
  if(fp == 0){
    fprintf(stderr, "ni: cannot write %s %s\n", tn, strerror(errno));
    cleanup(cid, 1, *pass0files);
  }

#ifdef HAVE_MERGESORT
  mergesort(ps->table, ps->maxhash, sizeof(ps->table[0]), tablecompar);
#else
  qsort(ps->table, ps->maxhash, sizeof(ps->table[0]), tablecompar);
# endif // HAVE_MERGESORT

  for(i = 0; i < ps->maxhash; i++){
    struct Bucket *bu = &ps->table[i];
    if(bu->word == 0)
      break;
    xwrite(bu->word, strlen(bu->word) + 1, fp);
    xwrite(&(bu->n), sizeof(bu->n), fp);

    // printf("flush: %s %d\n", bu->word, bu->n);

    for(bl = bu->b0; bl; bl = bl->next){
      for(pi = 0 ; pi < bl->n; pi++){
        if(xwrite(bl->p[pi], sizeof(PostIt), fp) <= 0){
          fprintf(stderr, "pedsort: xwrite failed %s\n", strerror(errno));
          cleanup(cid, 1, *pass0files);
        }
      }
    }

    bu->word = 0;
  }

  if(fclose(fp) != 0){
    fprintf(stderr, "pedsort: fclose failed %s\n", strerror(errno));
    cleanup(cid, 1, *pass0files);
  }

  // fprintf(stderr, "%x: pedsort: pass 0: bucket_used %u: ", (int) pthread_self(),
  // ps->buckets_used);

  ps->wordi = 0;
  ps->infoi = 0;
  ps->blocki = 0;
  ps->buckets_used = 0;
}


int
pass0(int cid, FILE *input, DID did, int *pass0files, struct pass0_state *ps)
{
  char *p;
  struct Bucket *bu;
  struct Block *bl;
  PostIt *infop;
  int len, c, skip;
  unsigned h;
  unsigned wc = 0;

  while(1){
    if((ps->wordi + (signed)maxwordlen + 1) > ps->maxword||
       (ps->infoi + 1) > ps->maxinfo ||
       ps->blocki + 1 >= ps->maxblocks ||
       ps->buckets_used >= ps->maxhash) {
      flushwords(cid, ps, pass0files);
    }

    len = 0;
    skip = 0;
    p = ps->wordbytes + ps->wordi;
    while((c = getc(input)) != EOF) {
      if (WORDCHAR(c)) {
        p[len] = tolower(c);
        len++;
      } else {
        p[len] = '\0';
        if (len == 0) {
          p++;
          skip++;
          continue;
        } else
          len++;
          break;
      }
    }
    wc++;
    if(len == 0) {
      assert(c == EOF);
      break;
    }
    assert(len>0);

    // printf("word %s %d %d %d\n", p, len, skip, wc);

    infop = ps->infobuf + ps->infoi;
    infop->dn = did;
    infop->wc = wc;
    ps->infoi++;
    
    h = lookup(ps, p);
    bu = &ps->table[h];
    
    if(bu->word == 0){
      ps->buckets_used++;
      bu->word = p;
      ps->wordi += len + skip;
      bu->n = 0;
      bl = bu->b0 = bu->bN = &ps->blocks[ps->blocki++];
      bl->n = 0;
      bl->next = 0;
    } else if(bu->bN->n >= BLOCKSIZE){
      bl = bu->bN->next = &ps->blocks[ps->blocki++];
      bu->bN = bl;
      bl->next = 0;
      bl->n = 0;
    } else {
      bl = bu->bN;
    }

    bl->p[bl->n] = infop;
    bl->n += 1;
    bu->n += 1;
  }
  return wc;
}

/*
 * Read a '\0'-terminated word.
 * Return 0 on success, EOF on EOF.
 */
int
zword(FILE *fp, char buf[], int bufsize)
{
  int i, c;

  i = 0;
  while((c = getc(fp)) != EOF){
    if(i < bufsize)
      buf[i++] = c;
    if(c == '\0')
      break;
  }
  if (i == bufsize)
    buf[bufsize-1] = '\0';
  if(i > 0)
    return(0);
  return(EOF);
}

/*
 * Read an unsigned int
 */
inline unsigned
znum(FILE *fp)
{
  unsigned pos;
  fread(&pos, sizeof(pos), 1, fp);
  return(pos);
}

/*
 * Copy bytes from ifp to ofp.
 */
void
zcopy(int cid, FILE *ifp, FILE *ofp, unsigned n, int pass0files)
{
  int c;
  unsigned i;

  for(i = 0; i < n && (c = getc(ifp)) != EOF; i++){
    if(putc(c, ofp) == EOF){
      fprintf(stderr, "pedsort: putc: %s\n", strerror(errno));
      cleanup(cid, 1, pass0files);
    }
  }
}

void
passN(int cid, char *outfile, char *oldoutfile, int pass0files)
{
  struct w2p_db w2p = {cid};
  int opass = 0;
  int onf = pass0files;
  int nnf = 0;
  FILE *ofp;
  FILE *fps[MaxFDS];
  int ifn;
  char iname[256], oname[256];
  char word[MaxFDS][maxwordlen+1];
  int eof[MaxFDS];
  unsigned numwords = 0;
  bool done = false;
  ind_offset offset = 0;
  char *minmask = NULL;
  int err;

  while(1){
    nnf = 0;
    ifn = 0;
    while(ifn < onf){
      int xni, fpn, startfpn;

      if(ifn == 0 && (ifn + MaxFDS + 1) >= onf) {
        strcpy(oname, outfile);
        ofp = fopen(outfile,"w");
        done = true;
        if (!ofp) {
          cleanup(cid, 1, pass0files);
          exit(1);
        }
        nnf++;
      } else {
        sorttmpname(cid, opass + 1, nnf++, oname);
        ofp = fopen(oname, "w");
        if(ofp == 0){
          fprintf(stderr, "pedsort: cannot write %s\n", oname);
          cleanup(cid, 1, pass0files);
        }
      }

      xni = onf - ifn;
      if(xni > MaxFDS)
        xni = MaxFDS;

      if (update_only && done) {
        fps[0] = fopen(oldoutfile, "r");
        if (fps[0] == 0) {
          fprintf(stderr,"pedsort: WARNING cannot open old index %s\n", oldoutfile);
          update_only = false;
        }else {
          eof[0] = zword(fps[0], word[0], sizeof(word[0]));
        }
        startfpn = 1;
        xni++;
      }else
        startfpn = 0;

      for (fpn = startfpn; fpn < xni; fpn++) {
        sorttmpname(cid, opass, ifn + fpn - startfpn, iname);
        fps[fpn] = fopen(iname, "r");
        if(fps[fpn] == 0){
          fprintf(stderr, "pedsort: cannot open %s %s\n", iname, 
                  strerror(errno));
          cleanup(cid, 1, pass0files);
        }
        eof[fpn] = zword(fps[fpn], word[fpn], sizeof(word[fpn]));
      }

      /*
       * Merge sort on the MaxFDS inputs.
       */
      while(1){
        /*
         * Find the minimum word, and find all the temporaries
         * with that word. We need them all to pre-compute the
         * total number of positions.
         */
        unsigned n[MaxFDS], nsum;
        const char *minword = 0;

        minmask = (char *)realloc(minmask,sizeof(char)*xni);
        assert(minmask);
        bzero(minmask, sizeof(char)*xni);

        for(fpn = 0; fpn < xni; fpn++){
          if(eof[fpn] == 0){
            if(minword == 0){
              minword = word[fpn];
              minmask[fpn] = 1;
            } else {
              int x = strcmp(word[fpn], minword);
              if(x < 0){
                minword = word[fpn];
                bzero(minmask,sizeof(char)*xni);
                minmask[fpn] = 1;
              } else if(x == 0){
                minmask[fpn] = 1;
              }
            }
          }
        }

        if(minword == 0)
          break;

        nsum = 0;
        for(fpn = 0; fpn < xni; fpn++){
          if(minmask[fpn] == 1) {
            n[fpn] = znum(fps[fpn]);
            nsum += n[fpn];
          }
        }

        fputs(minword, ofp);
        putc('\0', ofp);
        xwrite(&nsum, sizeof(nsum), ofp);

        if (done) {
          numwords++;

          if (dblim && ++w2p.count > dblim)
            w2p_close(&w2p);

          if (!w2p.db)
            w2p_open(&w2p);

          DBT key, data;
          bzero(&key,sizeof(key));
          bzero(&data,sizeof(data));

          key.data = (void *) minword;
          key.size = strlen(minword) + 1;
          data.data = &offset;
          data.size = sizeof(ind_offset);
          if((err = w2p.db->put(w2p.db, NULL, &key, &data, DB_NOOVERWRITE)) != 0){
             fprintf(stderr, "mkdb: db->put failed %s\n", db_strerror(err));
          }
          offset += strlen(minword) + 1 + sizeof(nsum) + nsum * sizeof(PostIt);
          w2p.count++;
        }

        /*
         * Spit out the position lists, first temporary first.
         * Advance to the next word.
         */
        for(fpn = 0; fpn < xni; fpn++){
          if(minmask[fpn]==1) {
            unsigned bytes = n[fpn] * sizeof(PostIt);
            zcopy(cid, fps[fpn], ofp, bytes, pass0files);
            eof[fpn] = zword(fps[fpn], word[fpn], sizeof(word[fpn]));
          }
        }
      }

      for(fpn = 0; fpn < xni; fpn++){
        fclose(fps[fpn]);
        if (startfpn && fpn == 0)
          continue;
        sorttmpname(cid, opass, ifn + fpn - startfpn, iname);
        if(unlink(iname) != 0){
          fprintf(stderr, "pedsort: unlink %s failed %s\n", iname,
                  strerror(errno));
          cleanup(cid, 1, pass0files);
        }
      }
      ifn += MaxFDS;
      
      if(fclose(ofp) != 0){
        fprintf(stderr, "pedsort: write to %s failed %s\n", oname, 
                strerror(errno));
        cleanup(cid, 1, pass0files);
      }
      if(done){
        // fprintf(stderr, "pedsort: merged %d files, %u words, ", onf, numwords);
        //fprintf(stderr, "\n");
        rename(outfile, oldoutfile);

        if (w2p.db)
          w2p_close(&w2p);

        return;
      }
    }

    // fprintf(stderr, "pedsort: merged %d files: ", onf);
    // fprintf(stderr, "\n");

    opass++;
    onf = nnf;
  }
}

void
cleanup(int cid, int exit_status, int pass0files)
{
  int pass, passfiles, i;

  pass = 0;
  passfiles = pass0files;

  while(pass == 0 || passfiles > 1){
    for(i = 0; i < passfiles; i++){
      char name[128];
      sorttmpname(cid, pass, i, name);
      unlink(name);
    }
    passfiles = (passfiles + MaxFDS - 1) / MaxFDS;
    pass++;
  }
  // unlink(outfile[n]);
  exit(exit_status);
}

float
printrusage(int init)
{
  static struct rusage oru;
  static double _or;
  struct rusage ru;
  struct timeval tv;
  double u, s, r;
  long i, o;

  gettimeofday(&tv, (struct timezone *) 0);
  if(getrusage(RUSAGE_SELF, &ru) < 0){
    perror("pedsort: getrusage");
    return 0.0;
  }

  if (init) {
    oru = ru;
    _or = tv.tv_sec + (tv.tv_usec / 1000000.0);
    return 0.0;
  }

  u = ru.ru_utime.tv_sec + (ru.ru_utime.tv_usec / 1000000.0);
  u -= oru.ru_utime.tv_sec + (oru.ru_utime.tv_usec / 1000000.0);
  s = ru.ru_stime.tv_sec + (ru.ru_stime.tv_usec / 1000000.0);
  s -= oru.ru_stime.tv_sec + (oru.ru_stime.tv_usec / 1000000.0);
  r = tv.tv_sec + (tv.tv_usec / 1000000.0);
  r -= _or;
  i = ru.ru_inblock;
  i -= oru.ru_inblock;
  o = ru.ru_oublock;
  o -= oru.ru_oublock;

  fprintf(stdout, "%.2fu %.2fs %.2fr %ldi %ldo", u, s, r, i, o);
  return r;
}

