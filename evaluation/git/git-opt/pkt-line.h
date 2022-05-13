#ifndef PKTLINE_H
#define PKTLINE_H

#include "git-compat-util.h"
#include "strbuf.h"

/*
 * Write a packetized stream, where each line is preceded by
 * its length (including the header) as a 4-byte hex number.
 * A length of 'zero' means end of stream (and a length of 1-3
 * would be an error).
 *
 * This is all pretty stupid, but we use this packetized line
 * format to make a streaming format possible without ever
 * over-running the read buffers. That way we'll never read
 * into what might be the pack data (which should go to another
 * process entirely).
 *
 * The writing side could use stdio, but since the reading
 * side can't, we stay with pure read/write interfaces.
 */
void packet_flush(int fd);
void packet_write(int fd, const char *fmt, ...) __attribute__((format (printf, 2, 3)));
void packet_buf_flush(struct strbuf *buf);
void packet_buf_write(struct strbuf *buf, const char *fmt, ...) __attribute__((format (printf, 2, 3)));

/*
 * Read a packetized line into the buffer, which must be at least size bytes
 * long. The return value specifies the number of bytes read into the buffer.
 *
 * If src_buffer is not NULL (and nor is *src_buffer), it should point to a
 * buffer containing the packet data to parse, of at least *src_len bytes.
 * After the function returns, src_buf will be incremented and src_len
 * decremented by the number of bytes consumed.
 *
 * If src_buffer (or *src_buffer) is NULL, then data is read from the
 * descriptor "fd".
 *
 * If options does not contain PACKET_READ_GENTLE_ON_EOF, we will die under any
 * of the following conditions:
 *
 *   1. Read error from descriptor.
 *
 *   2. Protocol error from the remote (e.g., bogus length characters).
 *
 *   3. Receiving a packet larger than "size" bytes.
 *
 *   4. Truncated output from the remote (e.g., we expected a packet but got
 *      EOF, or we got a partial packet followed by EOF).
 *
 * If options does contain PACKET_READ_GENTLE_ON_EOF, we will not die on
 * condition 4 (truncated input), but instead return -1. However, we will still
 * die for the other 3 conditions.
 *
 * If options contains PACKET_READ_CHOMP_NEWLINE, a trailing newline (if
 * present) is removed from the buffer before returning.
 */
#define PACKET_READ_GENTLE_ON_EOF (1u<<0)
#define PACKET_READ_CHOMP_NEWLINE (1u<<1)
int packet_read(int fd, char **src_buffer, size_t *src_len, char
		*buffer, unsigned size, int options);

/*
 * Convenience wrapper for packet_read that is not gentle, and sets the
 * CHOMP_NEWLINE option. The return value is NULL for a flush packet,
 * and otherwise points to a static buffer (that may be overwritten by
 * subsequent calls). If the size parameter is not NULL, the length of the
 * packet is written to it.
 */
char *packet_read_line(int fd, int *size);

/*
 * Same as packet_read_line, but read from a buf rather than a descriptor;
 * see packet_read for details on how src_* is used.
 */
char *packet_read_line_buf(char **src_buf, size_t *src_len, int *size);

#define DEFAULT_PACKET_MAX 1000
#define LARGE_PACKET_MAX 65520
extern char packet_buffer[LARGE_PACKET_MAX];

#endif
