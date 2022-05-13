from mparts.manager import Task
from mparts.host import HostInfo, CHECKED, UNCHECKED
from mparts.util import Progress
from support import ResultsProvider, SetCPUs, PrefetchList, FileSystem, \
    SystemMonitor

import os

__all__ = []

__all__.append("PsearchyLoad")
class PsearchyLoad(Task, ResultsProvider):
    MODE_THREAD = intern("thread")
    MODE_PROCESS = intern("process")

    ORDER_SEQ = intern("seq")
    ORDER_RR = intern("rr")

    __info__ = ["host", "psearchyPath", "filesPath", "dbPath",
                "mode", "order", "mem", "dblim", "trial", "*sysmonOut"]

    def __init__(self, host, trial, psearchyPath, filesPath, dbPath, cores,
                 mode, order, mem, dblim, setcpus, sysmon):
        assert mode in [PsearchyLoad.MODE_THREAD, PsearchyLoad.MODE_PROCESS], \
            "Invalid mode %s" % mode
        assert order in [PsearchyLoad.ORDER_SEQ, PsearchyLoad.ORDER_RR], \
            "Invalid order %s" % order

        Task.__init__(self, host = host, trial = trial)
        ResultsProvider.__init__(self, cores)
        self.host = host
        self.trial = trial
        self.psearchyPath = psearchyPath
        self.filesPath = filesPath
        self.dbPath = dbPath
        self.mode = mode
        self.order = order
        self.mem = mem
        self.dblim = dblim
        self.setcpus = setcpus
        self.sysmon = sysmon

    def wait(self, m):
        # Construct command
        cmd = [os.path.join(self.psearchyPath, "mkdb", "pedsort"),
               "-t", self.dbPath,
               "-c", str(self.cores),
               "-m", str(self.mem)]
        if self.mode == PsearchyLoad.MODE_PROCESS:
            cmd.append("-p")
        if self.dblim:
            cmd.extend(["-l", str(self.dblim)])
        # For the submission, we measured the entire time, including
        # file list loading.  This is probably a more natural
        # definition of "job", even though that part is sequential.
        cmd = self.sysmon.wrap(cmd)
        # We could instead only start measuring once we start
        # indexing.
        #cmd = self.sysmon.wrap(cmd, "Building index")

        # Run
        logPath = self.host.getLogPath(self)
        self.host.r.run(cmd, stdin = self.filesPath, stdout = logPath,
                        addEnv = {"CPUSEQ" : self.setcpus.getSeqStr()},
                        wait = CHECKED)

        # Get result
        log = self.host.r.readFile(logPath)
        self.sysmonOut = self.sysmon.parseLog(log)
        self.setResults(1, "job", "jobs", self.sysmonOut["time.real"])

__all__.append("Mkfiles")
class Mkfiles(Task):
    __info__ = ["host", "psearchyPath", "baseDir"]

    def __init__(self, host, psearchyPath, baseDir):
        Task.__init__(self, host = host)
        self.host = host
        self.psearchyPath = psearchyPath
        self.baseDir = baseDir

        self.filesPath = \
            os.path.join(self.psearchyPath, "files-%08x" % abs(hash(baseDir)))

    def start(self):
        # Do I need to construct the files list?
        test = self.host.r.run(["test", "-f", self.filesPath],
                               wait = UNCHECKED)
        if test.getCode() != 0:
            # Yes
            with Progress("Generating files list"):
                self.host.r.run([os.path.join(self.psearchyPath, "mkfiles"),
                                 self.baseDir],
                                stdout = self.filesPath + ".tmp",
                                noCheck = True)
                self.host.r.run(["mv", self.filesPath + ".tmp",
                                 self.filesPath])

class PsearchyRunner(object):
    def __str__(self):
        return "psearchy"

    @staticmethod
    def run(m, cfg):
        host = cfg.primaryHost
        m += host
        m += HostInfo(host)
        fs = FileSystem(host, cfg.fs, clean = True)
        m += fs
        psearchyPath = os.path.join(cfg.benchRoot, "psearchy")
        files = Mkfiles(host, psearchyPath, cfg.textRoot)
        m += files
        m += PrefetchList(host, files.filesPath, reuse = True)
        setcpus = SetCPUs(host = host, num = cfg.cores, hotplug = cfg.hotplug,
                          seq = cfg.order)
        m += setcpus
        sysmon = SystemMonitor(host)
        m += sysmon
        for trial in range(cfg.trials):
            m += PsearchyLoad(host, trial, psearchyPath, files.filesPath,
                              fs.path, cfg.cores, cfg.mode, cfg.order, cfg.mem,
                              cfg.dblim, setcpus, sysmon)
        # XXX
        # m += cfg.monitors
        m.run()

__all__.append("runner")
runner = PsearchyRunner()
