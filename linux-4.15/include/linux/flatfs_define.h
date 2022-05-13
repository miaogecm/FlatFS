/*
 * FlatFS: A Metadata-Optimized NVM File System
 * 
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */
#ifndef FLATFS_DEFINE_H
#define FLATFS_DEFINE_H

#define CONFIG_FLATFS_DEBUG_ALL             0

#define CONFIG_FLATFS_DEBUG_VFS             (CONFIG_FLATFS_DEBUG_ALL || 0)
#define CONFIG_FLATFS_DEBUG_FS              (CONFIG_FLATFS_DEBUG_ALL || 0)
#define CONFIG_FLATFS_DEBUG_BRTREE          (CONFIG_FLATFS_DEBUG_ALL || 0)

#define CONFIG_FLATFS_DEBUG_VFS_FENTRY      (CONFIG_FLATFS_DEBUG_VFS || 0)
#define CONFIG_FLATFS_DEBUG_VFS_PATH_WALK   (CONFIG_FLATFS_DEBUG_VFS || 0)
#define CONFIG_FLATFS_DEBUG_VFS_MOUNT       (CONFIG_FLATFS_DEBUG_VFS || 0)
#define CONFIG_FLATFS_DEBUG_VFS_PERM_CHECK  (CONFIG_FLATFS_DEBUG_VFS || 0)

#define CONFIG_FLATFS_DEBUG_FS_SYMLINK      (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_LINK         (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_UNLINK       (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_MKDIR        (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_RMDIR        (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_READDIR      (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_RENAME       (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_RMDIR_RECUR  (CONFIG_FLATFS_DEBUG_FS || 0)
#define CONFIG_FLATFS_DEBUG_FS_CPDIR_RECUR  (CONFIG_FLATFS_DEBUG_FS || 0)

#define CONFIG_FLATFS_DEBUG_BRTREE_ADD      (CONFIG_FLATFS_DEBUG_BRTREE || 0)
#define CONFIG_FLATFS_DEBUG_BRTREE_UPD      (CONFIG_FLATFS_DEBUG_BRTREE || 0)
#define CONFIG_FLATFS_DEBUG_BRTREE_DEL      (CONFIG_FLATFS_DEBUG_BRTREE || 0)
#define CONFIG_FLATFS_DEBUG_BRTREE_GET      (CONFIG_FLATFS_DEBUG_BRTREE || 0)

//#define CONFIG_FLATFS_BPLUS_VERIFY
//#define CONFIG_FASTSTR_VERIFY
//#define CONFIG_INOCACHE_STATISTICS
//#define CONFIG_NDCACHE_STATISTICS
//#define CONFIG_FLATFS_BPLUS_UNIT_TEST
//#define CONFIG_FLATFS_PROFILING
//#define CONFIG_FLATFS_BRBAG
#define CONFIG_FLATFS_LEX_PATHNAME

#define FLATFS_NCPU				48

#endif
