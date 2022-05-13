# How to run psearchy

0. Install `libdb` : `sudo apt-get install libdb-dev`

1. Create directories to store the per-core dbs and files.  You may want to mount store each directory on a separately mounted file system.

    `mkdir /tmp/db/0`

    `mkdir /tmp/db/1`

    `mkdir /tmp/db/2`

    ...
    
    run script as a root user `sudo ./mkdir.sh`
    
2. Prepare file path strings to be indexed, run script `mkfiles` to generate file path locate in a file directory, e.g., a Linux kernel source tree directory.

    `./mkfiles ${path of files to be indexed} > path`
    
3. Change into directory `mkdb/` and modify `pedsort.C`line 628 from

    `FILE *input = fopen((const char *) files[d], "r");` to
    
    `FILE *input = fopen((const char *) files[d], "rm");`
    
    specifically, mode`m` of `fopen` of GNU C library access the file using `mmap` rather than I/O          
    system calls (e.g., `read`). Currently, use of `mmap` is only for a file opened for reading

    modify GNUmakefile line 14 from

    `g++ -o $@ $(LDFLAGS) $^ -ldb -lpthread` to

    `g++ -o $@ $(LDFLAGS) $^ -ldb -pthread`
    
    then, run `make`
    
4. Change into directory `query/` and modify qe.C line 590 from

    `sprintf(dbname, "%s%d/%s-w2p.db-%d", tmpdir, cid, prefix.c_str(), cid);` to
    
    `sprintf(dbname, "%s%d/%s-w2p.db-%d.0", tmpdir, cid, prefix.c_str(), cid);`

    modify GNUmakefile line 9 from

    `g++ -o qe -L$(LDB) $(qe_objects) -ldb` to
    
    `g++ -o qe -L$(LDB) $(qe_objects) -ldb -pthread`
    
    then, run `make`
    
5. Build Berkeley database to store file names and contents in each file. e.g., To build the indices using two cores (-c 2) with a 2 MB hash table per core (-m 2), run:

    `sudo ./mkdb/pedsort -t /tmp/db/ -c 2 -m 2 -x path`
    
6. To query the indices "main" with two cores in database, run

     `sudo ./query/qe -t /tmp/db/ -q "main" -c 2  -l`
