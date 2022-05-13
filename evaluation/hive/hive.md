### Hive Install

1. `mkdir /usr/local/hive`, `mkdir /user/local/hadoop`, `mkdir /usr/local/java`, 
2. Download hive-2.3.9 hadoop-2.10.1, jre-1.8.0, decompress `apache-hive-2.3.9-bin.tar.gz`, `hadoop-2.10.1.tar.gz`, `jre1.8.0.tar.gz` and copy into associated directory in `/usr/local`.

2. Update `.bashrc`，setup environment for Hive, Hadoop, Java.

`export JAVA_HOME=/usr/local/java/jre1.8.0_271`
`export PATH=$JAVA_HOME"/bin:$PATH"`

`HIVE_VERSION="2.3.9"`
`export PATH=/usr/local/hive/apache-hive-2.3.9-bin/bin:$PATH`

`HADOOP_VERSION="2.10.1"`
`export HADOOP_HOME=/usr/local/hadoop/hadoop-2.10.1`
`export PATH=/usr/local/hadoop/hadoop-2.10.1/bin:$PATH`

`export HIVE_OPTS="-hiveconf mapred.job.tracker=local \
   -hiveconf fs.default.name=file:///home/flatfs/hive \
   -hiveconf hive.metastore.warehouse.dir=file:///home/flatfs/hive/warehouse \
   -hiveconf javax.jdo.option.ConnectionURL=jdbc:derby:;databaseName=/home/flatfs/hive/metastore_db";create=true`

3. Create a Hive working directory `mkdir /home/flatfs/hive`.
4. Initialize derby database `schematool -initSchema -dbType derby`.
5. Configure JVM heap size `export HADOOP_HEAPSIZE=10240` in `/usr/local/hadoop/hadoop-2.10.1/etc/hadoop/hadoop-env.sh`
6. Run `hive`.
