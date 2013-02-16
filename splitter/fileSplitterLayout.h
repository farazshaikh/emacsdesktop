/*
 * fileSplitterLayout.h --
 *
 *      OnDisk FileSplitter meta data layout
 *
 */

#ifndef _SPLITTER_LAYOUT
#define _SPLITTER_LAYOUT
#include <stdint.h>


#define MAX_SHARDINFO_VER 1
#define CHECKSUM_BOOTSTRAP 1
#define SHARD_MAGIC 0xC0FFEE

#ifndef MAX_PATH
#define MAX_PATH 256
#endif

#ifndef SECTOR_SIZE
#define SECTOR_SIZE 512
#endif

#define ROUND_UP(x,y)   (((x) + (y) -1) / (y))
#define ROUND_DOWN(x,y) ((x) / (y) * (y))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

struct shardHeader {
    uint64_t magic;
    uint64_t version;
    uint64_t offset;
    uint64_t length;
    uint64_t checkSum;
} __attribute__ ((packed));

typedef struct shardHeader shardHeader;

/*
 * each shard is named as <offset>.p
 * FORMAT struct shardHeader
 *        [shardData]
 */
#define SHARD_SUFFIX ".p"

/*
 * the shard meta file holds the file metadata
 * FORMAT struct fileMeta
 */
#define SHARD_META_FILE ".fileInfo"
struct FileMeta {
    uint64_t magic;
    uint64_t version;
    uint8_t fileName[256];
    uint64_t length;
    uint64_t shardSize;
} __attribute__ ((packed));
typedef struct FileMeta FileMeta;

/*
 * the reconcile file meta includes information needed for reconciliation of shared
 * FORMAT struct reconcileMeta [one sector]
 *        [shardBitmap]
 */
#define SHARD_RECONCILE_FILE  ".reconcileInfo"
struct __ReconcileMeta {
    uint64_t magic;
    uint64_t version;
    uint64_t reconciledShards;
    uint64_t totalShards;
    uint8_t  shardBitMap[0];
} __attribute__ ((packed));

union ReconcileMeta {
    struct __ReconcileMeta meta;
    uint8_t   filler[512];
}  __attribute__ ((packed));
typedef union ReconcileMeta ReconcileMeta;

/*
 * It is understood that following function is not cryptographic hash
 * Also is understood the one should not attempt writing a cryptographic
 * hash over the weekend. And I don't want to copy an implementation off
 * the net
 */
static uint64_t
rollingAdderCRC(uint8_t *buffer, uint64_t length, uint64_t calculatedCRC)
{
    uint64_t i;
    /*
     * if length is not 64bit aligned make it aligned for the purpose of CRC checking
     * A max of 3 bytes in the tail can fall of CRC protection by this
     */
    length = ROUND_DOWN(length, sizeof(uint64_t));
    for (i = 0; i < length / sizeof(uint64_t); i++) {
	calculatedCRC += ((uint64_t *) buffer)[i];
    }
    return calculatedCRC;
}

static void
getShardHeader(uint8_t  *buffer,
               uint64_t version,
               uint64_t offset,
               uint64_t length,
               shardHeader *hdr)
{
    uint64_t calculatedCRC;

    memset(hdr, 0, sizeof(*hdr));
    hdr->magic = SHARD_MAGIC;
    hdr->version = version;
    hdr->offset = offset;
    hdr->length = length;
    hdr->checkSum = CHECKSUM_BOOTSTRAP;

    calculatedCRC = 0;
    calculatedCRC = rollingAdderCRC((uint8_t *) hdr, sizeof(*hdr), calculatedCRC);
    calculatedCRC = rollingAdderCRC((uint8_t *) buffer, length, calculatedCRC);
    hdr->checkSum = calculatedCRC;
    return;
}

#endif //_SPLITTER_LAYOUT
