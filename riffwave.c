/******************************************************************************
*                                                                             *
*       Copyright (C) 1997 Tony Robinson and SoftSound Limited                *
*                                                                             *
*       See the file LICENSE for conditions on distribution and usage         *
*                                                                             *
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "shorten.h"

#ifndef min
# define min(a,b) ((a)<(b)?(a):(b))
#endif

#define BLOCKSIZE 256
#define NBLOCKS_INIT 16

#define RIFF_SIGNATURE 0x46464952   /* "RIFF" */
#define WAVE_SIGNATURE 0x45564157   /* "WAVE" */
#define fmt_SIGNATURE  0x20746D66   /* "fmt " */
#define data_SIGNATURE 0x61746164   /* "data" */

#define WAVE_FORMAT_PCM	      0x0001
#define WAVE_FORMAT_ALAW      0x0006
#define WAVE_FORMAT_MULAW     0x0007

/*
 * This structure contains an array of `char *' pointers to blocks
 * of data to be coded verbatim. The structure itself will be
 * dynamically allocated, as will the array of pointers, as will
 * the blocks themselves.
 */
struct Riff_Wave_Header {
  int nblocks, nblk_alloc;
  unsigned char **blocks;
  unsigned *blklen;
};

/*
 * Portably read a little-endian long integer from a file.
 */
static long fread_long (fp) FILE *fp; {
  long result;
  result = fgetc(fp);
  result |= ((long)fgetc(fp) << 8);
  result |= ((long)fgetc(fp) << 16);
  result |= ((long)fgetc(fp) << 24);
  return result;
}

/*
 * Retrieve a little-endian short integer from a char array, again
 * portably.
 */
static short short_at (p) unsigned char *p; {
  return ((short)p[1] << 8) | p[0];
}

/*
 * Write a load of data into a header chunk.
 */
static void write_hdr(buf, len, hdr)
unsigned char *buf; int len; Riff_Wave_Header *hdr;
{
  while (len--) {
    if (hdr->nblocks == 0 ||
	hdr->blklen[hdr->nblocks-1] == VERBATIM_CHUNK_MAX) {
      hdr->nblocks++;
      if (hdr->nblocks > hdr->nblk_alloc) {
	long size;

	hdr->nblk_alloc += NBLOCKS_INIT;

	size = hdr->nblk_alloc * sizeof(char *);
	hdr->blocks = hdr->blocks ? realloc(hdr->blocks, size) : malloc(size);

	if (!hdr->blocks)
	  perror_exit("malloc or realloc(%ld)", size);

	size = hdr->nblk_alloc * sizeof(unsigned);
	hdr->blklen = hdr->blklen ? realloc(hdr->blklen, size) : malloc(size);

	if (!hdr->blklen)
	  perror_exit("malloc or realloc(%ld)", size);
      }
      hdr->blklen[hdr->nblocks-1] = 0;
      hdr->blocks[hdr->nblocks-1] = pmalloc(sizeof(unsigned char) *
					    VERBATIM_CHUNK_MAX);
    }
    hdr->blocks[hdr->nblocks-1][hdr->blklen[hdr->nblocks-1]++] = *buf++;
  }
}

/*
 * Write a little-endian long integer into a header chunk.
 */
static void write_hdr_long(data, hdr)
unsigned long data; Riff_Wave_Header *hdr;
{
  unsigned char realdata[4];
  realdata[0] = data & 0xFF;
  realdata[1] = (data >> 8) & 0xFF;
  realdata[2] = (data >> 16) & 0xFF;
  realdata[3] = (data >> 24) & 0xFF;
  write_hdr (realdata, 4, hdr);
}

Riff_Wave_Header *riff_wave_prochdr (filei, ftype, nchan, datalen, wtype)
FILE *filei; int *ftype, *nchan; long *datalen; int *wtype;
{
  Riff_Wave_Header *hdr;
  long len, ckhdr;
  int seen_fmt = 0;

  /* Initialize the returned wave type */
  if (wtype)
	  *wtype = 0;

  /* Reserve space for the header structure. */
  hdr = pmalloc(sizeof(*hdr));
  hdr->nblocks = hdr->nblk_alloc = 0;
  hdr->blocks = NULL;
  hdr->blklen = NULL;

  /* Read the RIFF signature. */
  if (fread_long(filei) != RIFF_SIGNATURE ||
      (len = fread_long(filei)) < 12 ||
      fread_long(filei) != WAVE_SIGNATURE) {
    free_header(hdr);
    return NULL;
  }

  /* Write the RIFF signature into the header buffer. */
  write_hdr_long(RIFF_SIGNATURE, hdr);
  write_hdr_long(len, hdr);
  write_hdr_long(WAVE_SIGNATURE, hdr);

  /* Now begin reading chunks. Give special processing to the "fmt"
   * chunk, end header processing on the "data" chunk, and skip
   * everything else. */
  while (!feof(filei) && (ckhdr = fread_long(filei)) != data_SIGNATURE) {
    long cklen = fread_long(filei);
    int len, blklen;
    unsigned char buf[256];

    write_hdr_long(ckhdr, hdr);
    write_hdr_long(cklen, hdr);

    if (ckhdr == fmt_SIGNATURE) {
      unsigned char buf[16];
      if (cklen < 16 || fread(buf, 1, 16, filei) < 16) {
	/* We don't understand WAVE files with a header that's too short. */
	free_header(hdr);
	return NULL;
      }
      cklen -= 16;
      write_hdr(buf, 16, hdr);
      switch(short_at(buf)) { 	      /* WAVE_FORMAT */
      case WAVE_FORMAT_PCM:
	switch (short_at(buf+14)) {   /* bits per sample */
	case 8:
	  *ftype = TYPE_U8;
	  break;
	case 16:
	  *ftype = TYPE_S16LH;
	  break;
	default:
	  free_header(hdr);
	  return NULL;
	}
	break;
      case WAVE_FORMAT_MULAW:
	*ftype = TYPE_GENERIC_ULAW;
	break;
      case WAVE_FORMAT_ALAW:
	*ftype = TYPE_GENERIC_ALAW;
	break;
      default:
	/* We don't understand this format */
	free_header(hdr);
	if (wtype)
		*wtype = short_at(buf);
	return NULL;
      }

      if ((*nchan = short_at(buf+2)) < 1) { /* at least one channel */
	free_header(hdr);
	return NULL;
      }

      seen_fmt = 1;
    }

    while (cklen > 0 && (len = fread(buf, 1,
				     (blklen = min(cklen,256)),
				     filei)) > 0) {
      write_hdr (buf, blklen, hdr);
	  cklen -= blklen;
    }
  }
  if (!seen_fmt) {		/* no fmt chunk before data chunk */
    free_header(hdr);
    return NULL;
  }
  write_hdr_long (data_SIGNATURE, hdr);
  write_hdr_long (*datalen = fread_long(filei), hdr);
  return hdr;
}

static void write_verbatim_chunk (data, len, fileo)
unsigned char *data; int len; FILE *fileo;
{
  uvar_put((ulong) FN_VERBATIM, FNSIZE, fileo);
  uvar_put((ulong) len, VERBATIM_CKSIZE_SIZE, fileo);
  while (len--) {
    uvar_put ((ulong) *data, VERBATIM_BYTE_SIZE, fileo);
    data++;
  }
}

void write_header (hdr, fileo)
Riff_Wave_Header *hdr; FILE *fileo;
{
  unsigned char **blocks;
  unsigned *blklen;
  int nblocks;

  nblocks = hdr->nblocks;
  blocks = hdr->blocks;
  blklen = hdr->blklen;
  while (nblocks--)
    write_verbatim_chunk (*blocks++, *blklen++, fileo);
}

void verbatim_file (filei, fileo)
FILE *filei, *fileo;
{
  unsigned char buf[VERBATIM_CHUNK_MAX];
  int len;

  while ( (len = fread(buf, 1, VERBATIM_CHUNK_MAX, filei)) > 0)
    write_verbatim_chunk (buf, len, fileo);
}

void free_header (hdr)
Riff_Wave_Header *hdr;
{
  unsigned char **blocks;
  int nblocks;

  if (hdr) {
    if (hdr->blocks) {
      nblocks = hdr->nblocks;
      blocks = hdr->blocks;

      while (nblocks--)
	free (*blocks++);

      free (hdr->blocks);
    }
    if (hdr->blklen)
      free (hdr->blklen);
    free (hdr);
  }
}
