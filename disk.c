/* disk.c: Routines for handling disk images
   Copyright (c) 2007-2017 Gergely Szasz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

   Author contact information:

   Philip: philip-fuse@shadowmagic.org.uk

*/

#include <config.h>

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "internals.h"

/* every track data:
TRACK_LEN TYPE TRACK......DATA CLOCK..MARKS MF..MARKS WEAK..MARKS
               ^               ^            ^         ^
               |__ track       |__ clocks   |__ mf    |__ weak
  so, track[-1] = TYPE
  TLEN = track[-3] + tarck 256 * track[-2]
  TYPE is Track type as in UDI spec (0x00, 0x01, 0x02, 0x80, 0x81, 0x82) after
       update_tracks_mode() !!!
*/

#define BUFF_ALLOC 32768

#define DISK_CLEN( bpt ) ( ( bpt ) / 8 + ( ( bpt ) % 8 ? 1 : 0 ) )

#define DISK_SET_TRACK_IDX( d, idx ) \
   d->track = d->data + 3 + ( idx ) * d->tlen; \
   d->clocks = d->track  + d->bpt; \
   d->fm     = d->clocks + DISK_CLEN( d->bpt ); \
   d->weak   = d->fm     + DISK_CLEN( d->bpt )

#define DISK_SET_TRACK( d, head, cyl ) \
   DISK_SET_TRACK_IDX( (d), (d)->sides * cyl + head )



/* The ordering of these strings must match the order of the
 * libspectrum_disk_error_t enumeration in libspectrum.h.in */

static const char * const disk_error[] = {
  "OK",				/* LIBSPECTRUM_DISK_OK */
  "Feature not implemented",	/* LIBSPECTRUM_DISK_IMPL */
  "Out of memory",		/* LIBSPECTRUM_DISK_MEM */
  "Invalid disk geometry",	/* LIBSPECTRUM_DISK_GEOM */
  "Cannot open disk image",	/* LIBSPECTRUM_DISK_OPEN */
  "Unsupported file feature",	/* LIBSPECTRUM_DISK_UNSUP */
  "Read only disk",		/* LIBSPECTRUM_DISK_RDONLY */
  "Cannot close file",		/* LIBSPECTRUM_DISK_CLOSE */
  "Cannot write disk image",	/* LIBSPECTRUM_DISK_WRFILE */
  "Partially written file",	/* LIBSPECTRUM_DISK_WRPART */
  "Unknown image type",		/* LIBSPECTRUM_DISK_UNKNOWN */
  "Sector not found",		/* LIBSPECTRUM_DISK_SNFOUND */
  "Invalid parameters",		/* LIBSPECTRUM_DISK_BADPARAM */

  "Unknown error code"		/* LIBSPECTRUM_DISK_LAST_ERROR */
};

static const libspectrum_disk_desc_t disk_desc[] = {
  { "<Unknown>", "", "", 0 },
  { "UDI", "Ultra Disk Image", "https://faqwiki.zxnet.co.uk/wiki/UDI_format", 0 },
  { "FDI", "UKV Spectrum Debugger's Full Disk Image", "http://www.worldofspectrum.org/faq/reference/formats.htm#FDI", 0 },
  { "TD0", "Teledisk image", "http://www.classiccmp.org/dunfield/img54306/td0notes.txt", 1 },
  { "MGT", "Disciple/+D disk image", "https://faqwiki.zxnet.co.uk/wiki/MGT_format", 0 },
  { "IMG", "Disciple/+D disk image", "https://faqwiki.zxnet.co.uk/wiki/MGT_format", 0 },
  { "SAD", "SAm Disk (SAM Coupe)", "", 0 },
  { "DSK/CPC", "", "http://cpctech.cpc-live.com/docs/dsk.html", 0 },
  { "DSK/ECPC", "", "http://cpctech.cpc-live.com/docs/extdsk.html", 1 },
  { "TRD", "", "https://faqwiki.zxnet.co.uk/wiki/TRD_format", 0 },
  { "SCL", "", "https://faqwiki.zxnet.co.uk/wiki/TRD_format#SCL_Format", 0 },
  { "OPD/OPU", "Opus Discovery disk image", "https://faqwiki.zxnet.co.uk/wiki/OPD_format", 0 },
  { "D40", "Didaktik 40 disk image", "", 0 },
  { "D80", "Didaktik 80 disk image", "", 0 },
  { "LOG", "Human readable file of a disk", "", 0 },
};

static const int disk_bpt[] = {
  6250,				/* AUTO assumes DD */
  5208,				/* 8" SD */
  10416,			/* 8" DD */
  3125,				/* SD */
  6250,				/* DD */
  6500,				/* DD+ e.g. Coin Op Hits */
  12500,			/* HD */
};

typedef struct disk_gap_t {
  int gap;			/* gap byte */
  int sync;			/* sync byte */
  int sync_len;
  int mark;			/* mark byte 0xa1 for MFM -1 for MF */
  int len[4];
} disk_gap_t;

static disk_gap_t gaps[] = {
  { 0x4e, 0x00, 12, 0xa1, {  0, 60, 22, 24 } },			/* MGT MFM */
  { 0x4e, 0x00, 12, 0xa1, {  0, 10, 22, 60 } },			/* TRD MFM */
  { 0xff, 0x00,  6, -1,   { 40, 26, 11, 27 } },			/* IBM3740 FM */
  { 0x4e, 0x00, 12, 0xa1, { 80, 50, 22, 54 } },			/* IBM34 MFM */
  { 0xff, 0x00,  6, -1,   {  0, 16, 11, 10 } },			/* MINIMAL FM */
  { 0x4e, 0x00, 12, 0xa1, {  0, 32, 22, 24 } },			/* MINIMAL MFM */
};

#define GAP_MGT_PLUSD	0
#define GAP_TRDOS	1
#define GAP_IBM3740	2
#define GAP_IBM34	3
#define GAP_MINIMAL_FM  4
#define GAP_MINIMAL_MFM 5

static libspectrum_word crc_fdc_table[] = {
  0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7,
  0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,
  0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6,
  0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
  0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485,
  0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d,
  0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4,
  0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc,
  0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823,
  0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b,
  0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12,
  0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a,
  0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
  0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49,
  0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70,
  0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78,
  0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f,
  0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067,
  0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e,
  0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256,
  0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d,
  0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
  0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c,
  0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634,
  0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab,
  0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3,
  0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a,
  0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92,
  0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9,
  0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1,
  0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
  0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0
};

typedef struct buffer_t {		/* to store buffer data */
  unsigned char *data;
  size_t len;
  size_t idx;
} buffer_t;

#define buffavail( buffer ) \
  ( buffer->idx < buffer->len ? buffer->len - buffer->idx : 0 )
/* data buffer */
#define buff ( buffer->data + buffer->idx )

void disk_update_tlens( libspectrum_disk_t *d );

#define guess_track_geom( d, head, cyl, sector_base, sectors, seclen, mfm) \
  libspectrum_guess_track_geom( d, head, cyl, sector_base, sectors, seclen, mfm, NULL )

libspectrum_disk_error_t
libspectrum_disk_set_track( libspectrum_disk_t *d, int head, int cyl )
{
  if( !d ) return LIBSPECTRUM_DISK_BADPARAM;

  if( head >= d->sides || cyl >= d->cylinders )
    return LIBSPECTRUM_DISK_GEOM;

  DISK_SET_TRACK( d, head, cyl );

  return LIBSPECTRUM_DISK_OK;
}

const char *
libspectrum_disk_strerror( libspectrum_disk_error_t error )
{
  if( error > LIBSPECTRUM_DISK_LAST_ERROR )
    error = LIBSPECTRUM_DISK_LAST_ERROR;
  return disk_error[ error ];
}

/*
    automatically grow the buffer if needed
*/
static void
buffwrite( const void *data, size_t len, buffer_t *buffer )
{
  if( len > buffer->len - buffer->idx ) {
    buffer->data =
        libspectrum_realloc( buffer->data, buffer->len + BUFF_ALLOC );
    buffer->len += BUFF_ALLOC;
  }
  memcpy( buffer->data + buffer->idx, data, len );
  buffer->idx += len;
}

#define buffprintf( buffer, format... ) \
  snprintf( head, 256, format ); \
  buffwrite( head, strlen( head ), buffer )

static int
buffread( void *data, size_t len, buffer_t *buffer )
{
  if( len > buffer->len - buffer->idx )
    return 0;
  memcpy( data, buffer->data + buffer->idx, len );
  buffer->idx += len;
  return 1;
}

static int
buffseek( buffer_t *buffer, long offset, int whence )
{
  if( whence == SEEK_CUR )
    offset += buffer->idx;
  if( offset >= buffer->len )
    return -1;
  buffer->idx = offset;
  return 0;
}

/* CRC-16-CCITT: G(x) = x^16 + x^12 + x^5 + 1  */
libspectrum_word
libspectrum_disk_crc( libspectrum_word crc, libspectrum_byte data )
{
  return ( ( crc << 8 ) ^ crc_fdc_table[( ( crc >> 8 ) ^ data ) & 0xff] ) &
         0xffff;
}

static libspectrum_signed_dword
crc_udi( libspectrum_signed_dword crc, libspectrum_byte data )
{
  int i;
  libspectrum_signed_dword temp;

  crc ^= (libspectrum_signed_dword)(-1) ^ data;
  for( i = 8; i > 0; i-- ) {
    temp = -( crc & 1 );
    crc >>= 1;
    crc ^= (libspectrum_dword)0xedb88320 & temp;
  }
  crc ^= (libspectrum_signed_dword)(-1);
  return crc;
}


/*
  read next id from track (d->track, d->i)
  return with `head', `track', `secnum', `length' *code*
  recorded in sector header.
  d->i point after the 2 CRC byte
*/
#define id_read( d, h, t, s, l ) libspectrum_disk_id_read( d, h, t, s, l )
int
libspectrum_disk_id_read( libspectrum_disk_t *d, int *head, int *track,
                          int *sector, int *length )
{
  int a1mark = 0;

  if( !d || !head || !track || !sector || !length )
    return 0;

  while( d->i < d->bpt ) {
    if( d->track[ d->i ] == 0xa1 &&
      libspectrum_bitmap_test( d->clocks, d->i ) ) {		/* 0xa1 with clock */
      a1mark = 1;
    } else if( d->track[ d->i ] == 0xfe &&
      ( libspectrum_bitmap_test( d->clocks, d->i ) ||		/* 0xfe with clock */
        a1mark ) ) {						/* or 0xfe with 0xa1 */
      d->i++;
      *track  = d->track[ d->i++ ];
      *head   = d->track[ d->i++ ];
      *sector = d->track[ d->i++ ];
      *length = d->track[ d->i++ ];
      d->i += 2;	/* skip CRC */
      return 1;
    } else {
      a1mark = 0;
    }
    d->i++;
  }
  return 0;
}

/*
  read next datamark from track (d->track, d->i)
*/
#define datamark_read( d, deleted, fmf ) \
  libspectrum_disk_datamark_read( d, deleted, fmf )
int
libspectrum_disk_datamark_read( libspectrum_disk_t *d, int *deleted, int *fmf )
{
  int a1mark = 0;

  if( !d || !deleted ) return 0;

  while( d->i < d->bpt ) {
    if( d->track[ d->i ] == 0xa1 &&
        libspectrum_bitmap_test( d->clocks, d->i ) ) { /* 0xa1 with clock */
      a1mark = 1;
    } else if( d->track[ d->i ] >= 0xf8 && d->track[ d->i ] <= 0xfe &&
               ( libspectrum_bitmap_test( d->clocks, d->i ) || a1mark ) ) {
      /* 0xfe with clock or 0xfe after 0xa1 mark */
      *deleted = d->track[ d->i ] == 0xf8 ? 1 : 0;
      if( fmf != NULL )
        *fmf = a1mark ? 1 : 0;
      d->i++;
      return 1;
    } else {
      a1mark = 0;
    }
    d->i++;
  }
  return 0;
}

/*
  seek to `sector' sector in current track
  return with sector length *code*: `len'
  d->i pont after the 2 CRC byte (the first GAP byte)
*/
#define id_seek( d, s ) libspectrum_disk_id_seek( d, s, NULL )
int
libspectrum_disk_id_seek( libspectrum_disk_t *d, int sector, int *len )
{
  int h, t, s, l;

  if( !d ) return 0;

  if( len == NULL ) len = &l;
  d->i = 0;	/* start of the track */
  while( id_read( d, &h, &t, &s, len ) ) {
    if( s == sector )
      return 1;
  }
  return 0;
}

/*
    Seek to a head, cyl, sector data position
    return with sector length *code*: `len'  and
    data mark properies: `deleted', `mfm' data
    d->i point to the first data byte in sector
*/
libspectrum_disk_error_t
libspectrum_disk_seek( libspectrum_disk_t *d, int head, int cyl, int sector,
                       int *len, int *del, int *fmf )
{
  if( !d || !del )
    return LIBSPECTRUM_DISK_BADPARAM;

  if( head >= d->sides || cyl >= d->cylinders )
    return LIBSPECTRUM_DISK_GEOM;

  DISK_SET_TRACK( d, head, cyl );
  if( !libspectrum_disk_id_seek( d, sector, len ) ||
      !datamark_read( d, del, fmf ) )
    return LIBSPECTRUM_DISK_SNFOUND;

  return LIBSPECTRUM_DISK_OK;
}

/*
  copy data from a sector, handle track length and wraparound...
*/
static void
read_sector_data( libspectrum_disk_t *d, libspectrum_byte *buffer, int len )
{
  while( len ) {
    if( d->i >= d->bpt ) {
      d->i = 0;
    }
    *buffer = d->track[d->i];
    buffer++;
    d->i++;
    len--;
  }
}

/*
    Read `snum' sector's data from head, cyl,
    sector into a buffer. if *del != 0 then read
    deleted sector data, else skip deleted sectors.
    return with buffer pointer, or NULL if data not found,
      buffer length, readed/skipped sector num in *snum (overwrite),
      skipped/readed deleted sector num in *del (overwrite),
      if prop != NULL than track properties in `prop' and
        if sprop != NULL sector_base & 256*(num sectors) in `sprop'
    buffer must be freed if not NULL
    Note: *snum and *del is input and output parameter, so
          the given values are overwritten!
*/
libspectrum_disk_error_t
libspectrum_disk_read_sectors( libspectrum_disk_t *d, int head, int cyl,
                               int sector, int *snum, int *del,
                               libspectrum_byte **buffer, size_t *length,
                               int *prop, int *sprop )
{
  int n = *snum;
  int dd = *del;
  int len, dam;
  buffer_t b;

  if( !d || !snum || !del || !buffer || !length )
    return LIBSPECTRUM_DISK_BADPARAM;

  if( head >= d->sides || cyl >= d->cylinders )
    return LIBSPECTRUM_DISK_GEOM;
  DISK_SET_TRACK( d, head, cyl );

  b.data = NULL;
  b.len = b.idx = 0;
  *snum = *del = 0;
  *buffer = NULL; *length = 0;

  if( sector < 0 || sector > 255 )  /* max sector num is 0xff */
    return LIBSPECTRUM_DISK_SNFOUND;

  if( prop != NULL ) {
    int sector_base;
    int sectors;
    int seclen;
    int mfm;
    *prop = guess_track_geom( d, head, cyl, &sector_base, &sectors, &seclen,
                              &mfm );
    if( sprop != NULL )
      *sprop = sector_base + 256 * sectors;
  }

  if( n < 1 ) /* nothing to do */
    return LIBSPECTRUM_DISK_OK;
  if( sector + n > 256 ) n = 256 - sector;  /* calculate max sector num. */
  while( n && sector < 256 ) { /* max sector num is 0xff */
    if( libspectrum_disk_id_seek( d, sector, &len ) &&
        datamark_read( d, &dam, NULL ) ) {
      if( dam ) (*del)++;
      if( !dam || dd ) { /* read if not deleted or deleted, but user wants deleted data too */
/* There are two questions about sector length:
  1. the data length is determined by a code with an exponential function:
     00 -> 128, 01 -> 256, 02 -> 512, 03 -> 1024... and specifications did not
     say anything about 04, 05, 06... etc code.
     How different chips calculate the size? With uPD765 we have sector length
     greater than 1024 (code = 0x03), e.g. on `coin op hits side a' DSK we have
     a lot of 8192 byte length (code = 0x06) sector...
  2. some WD chips have a L flag (Length code), a bit that change the meaning of
     the length code: if L = 0 then 00 -> 256, 01 -> 512, 02 -> 1024, 03 -> 128
     what about code 04, 05? how this family of FDCs interpret codes greater
     than 0x03?
*/
        len = 0x80 << len; /* calculate length from code... */
        if( b.len == 0 ) libspectrum_malloc( BUFF_ALLOC );
        if( b.len - b.idx < len )
          b.data = libspectrum_realloc( b.data, b.len + BUFF_ALLOC );
        b.len += BUFF_ALLOC;
        read_sector_data( d, b.data + b.idx, len );
        b.idx += len;
        (*snum)++;		/* hmm.. ++ has greater precedence than * ?... */
      }
    }
    sector++; /* next sector */
    n--;
  }
  *buffer = b.data;
  *length = b.idx;

  return LIBSPECTRUM_DISK_OK;
}

/* seclen 00 -> 128, 01 -> 256 ... byte */
static int
data_write_file( libspectrum_disk_t *d, buffer_t *b, int seclen )
{
  int len = 0x80 << seclen;
  /* TODO: check len? */

  buffwrite( &d->track[ d->i ], len, b );

  return 0;
}

/*
    write `sectors' num of sector data from sector `sector_base' from `track'
     e.g.: sector 1,2,3,4,5,6,7,...
*/
static int
savetrack( libspectrum_disk_t *d, buffer_t *b, int head, int track,
           int sector_base, int sectors, int seclen )
{
  int s;
  int del;

  DISK_SET_TRACK( d, head, track );
  d->i = 0;
  for( s = sector_base; s < sector_base + sectors; s++ ) {
    if( id_seek( d, s ) ) {
      if( datamark_read( d, &del, NULL ) ) {		/* write data if we have data */
        if( data_write_file( d, b, seclen ) )
          return 1;
      }
    } else {
      return 1;
    }
  }
  return 0;
}

/*
    write all sector data from `track' as laid in track
*/
static int
saverawtrack( libspectrum_disk_t *d, buffer_t *b, int head, int track )
{
  int h, t, s, seclen;
  int del;

  DISK_SET_TRACK( d, head, track );
  d->i = 0;
  while( id_read( d, &h, &t, &s, &seclen ) ) {
    if( datamark_read( d, &del, NULL ) ) {		/* write data if we have data */
      if( data_write_file( d, b, seclen ) )
        return 1;
    }
  }
  return 0;
}

/*
#define DISK_ID_NOTMATCH 1
#define DISK_SECLEN_VARI 2
#define DISK_SPT_VARI 4
#define DISK_SBASE_VARI 8
#define DISK_MFM_VARI 16
#define DISK_DDAM 32
#define DISK_CORRUPT_SECTOR 64
#define DISK_UNFORMATTED_TRACK 128
#define DISK_FM_DATA 256
#define DISK_WEAK_DATA 512
*/

int
libspectrum_guess_track_geom( libspectrum_disk_t *d, int head, int track,
                              int *sector_base, int *sectors, int *seclen,
                              int *mfm, int *interl )
{
  int r = 0;
  int h, t, s, sl, xmfm;
  int del = 0;
  int smax = 0, sidx = -1;
  *sector_base = -1;
  *sectors = 0;
  *seclen = -1;
  *mfm = -1;

  DISK_SET_TRACK( d, head, track );
  if( d->track[-1] & 0x80 )
    r |= LIBSPECTRUM_DISK_WEAK_DATA;
  d->i = 0;
  while( id_read( d, &h, &t, &s, &sl ) ) {
    if( *sector_base == -1 )
      *sector_base = s;
    if( *seclen == -1 )
      *seclen = sl;
      /* not so robust */
/*    if( *mfm == -1 )
      *mfm = d->track[ d->i ] == 0x4e ? 1 : 0;
*/
    if( !datamark_read( d, &del, &xmfm ) )
      r |= LIBSPECTRUM_DISK_CORRUPT_SECTOR;
    if( *mfm != -1 && *mfm != xmfm ) {
      r |= LIBSPECTRUM_DISK_MFM_VARI;
      *mfm = 2;
    } else if( *mfm == -1 ) {
      *mfm = xmfm;
    }
    if( t != track )
      r |= LIBSPECTRUM_DISK_ID_NOTMATCH;
    if( s < *sector_base )
      *sector_base = s;
    if( sl != *seclen ) {
      r |= LIBSPECTRUM_DISK_SECLEN_VARI;
      if( sl > *seclen )
        *seclen = sl;
    }
    if( del )
      r |= LIBSPECTRUM_DISK_DDAM;
    *sectors += 1;
    if( s > smax ) {
      smax = s;
      sidx = *sectors;
    }
  }
  if( *sector_base < 0 )
    r |= LIBSPECTRUM_DISK_UNFORMATTED_TRACK;

  /* calculate sector interleave if any... */
  if( interl ) {
    if( *sectors > 0 ) {
      *interl = *sectors - sidx;
    } else {
      *interl = -1;
    }
  }
  return r;
}

static void
update_tracks_mode( libspectrum_disk_t *d )
{
  int i, j, bpt;
  int mfm, fm, weak;

  for( i = 0; i < d->cylinders * d->sides; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    mfm = 0, fm = 0, weak = 0;
    bpt = d->track[-3] + 256 * d->track[-2];
    for( j = DISK_CLEN( bpt ) - 1; j >= 0; j-- ) {
      mfm  |= ~d->fm[j];
      fm   |= d->fm[j];
      weak |= d->weak[j];
    }
    if( mfm && !fm ) d->track[-1] = 0x00;
    if( !mfm && fm ) d->track[-1] = 0x01;
    if( mfm &&  fm ) d->track[-1] = 0x02;
    if( weak ) {
      d->track[-1] |= 0x80;
      d->have_weak = 1;
    }
  }
}

static int
check_disk_geom( libspectrum_disk_t *d, int *sector_base, int *sectors,
                 int *seclen, int *mfm, int *unf )
{
  int h, t, s, slen, sbase, m;
  int r = 0;

  DISK_SET_TRACK_IDX( d, 0 );
  d->i = 0;
  *sector_base = -1;
  *sectors = -1;
  *seclen = -1;
  *mfm = -1;
  *unf = -1;

  for( t = 0; t < d->cylinders; t++ ) {
    for( h = 0; h < d->sides; h++ ) {
      r |= ( d->track[-1] & 0x80 ) ? LIBSPECTRUM_DISK_WEAK_DATA : 0;
      r |= ( d->track[-1] & 0x03 ) == 0x02 ? LIBSPECTRUM_DISK_MFM_VARI : 0;
      r |= ( d->track[-1] & 0x03 ) == 0x01 ? LIBSPECTRUM_DISK_FM_DATA : 0;
      /* we have our special unformatted guessing ...*/
      r |= ( guess_track_geom( d, h, t, &sbase, &s, &slen, &m ) &
             ~LIBSPECTRUM_DISK_UNFORMATTED_TRACK );
      if( *sector_base == -1 )
        *sector_base = sbase;
      if( *sectors == -1 )
        *sectors = s;
      if( *seclen == -1 )
        *seclen = slen;
      if( *mfm == -1 )
        *mfm = m;
      if( sbase == -1 ) {		/* unformatted */
        if( *unf == -1 && h > 0 ) *unf = -2;
        if( *unf == -1 ) *unf = t;
        continue;
      }
      if( *unf > -1 ) *unf = -2;
      if( sbase != *sector_base ) {
        r |= LIBSPECTRUM_DISK_SBASE_VARI;
        if( sbase < *sector_base )
          *sector_base = sbase;
      }
      if( s != *sectors ) {
        r |= LIBSPECTRUM_DISK_SPT_VARI;
        if( s > *sectors )
          *sectors = s;
      }
      if( slen != *seclen ) {
        r |= LIBSPECTRUM_DISK_SECLEN_VARI;
        if( slen > *seclen )
          *seclen = slen;
      }
      if( m != *mfm ) {
        r |= LIBSPECTRUM_DISK_MFM_VARI;
        *mfm = 1;
      }
    }
  }

  if( *unf == -2 ) {
    r |= LIBSPECTRUM_DISK_UNFORMATTED_TRACK;
    *unf = -1;
  }

  return r;
}

static int
gap_add( libspectrum_disk_t *d, int gap, int gaptype )
{
  disk_gap_t *g = &gaps[ gaptype ];
  if( d->i + g->len[gap]  >= d->bpt )  /* too many data bytes */
    return 1;
/*-------------------------------- given gap --------------------------------*/
  memset( d->track + d->i, g->gap,  g->len[gap] ); d->i += g->len[gap];
  return 0;
}

static int
preindex_len( int gaptype )		/* preindex gap and index mark */
{
  disk_gap_t *g = &gaps[ gaptype ];
  return g->len[0] + g->sync_len + ( g->mark >= 0 ? 3 : 0 ) + 1;
}

/*
  [ ....GAP.... ] [ ... SYNC ... ] [ . MARK . ]
  |------------------------------------------->
                Preindex
*/
static int
preindex_add( libspectrum_disk_t *d, int gaptype )		/* preindex gap and index mark */
{
  disk_gap_t *g = &gaps[ gaptype ];
  if( d->i + preindex_len( gaptype ) >= d->bpt )
    return 1;
/*------------------------------ pre-index gap -------------------------------*/
  if( gap_add( d, 0, gaptype ) )
    return 1;
/*------------------------------   sync    ---------------------------*/
  memset( d->track + d->i, g->sync, g->sync_len ); d->i += g->sync_len;
  if( g->mark >= 0 ) {
    memset( d->track + d->i , g->mark, 3 );
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
  }
/*------------------------------     mark     ------------------------------*/
  if( g->mark < 0 )				/* FM */
    libspectrum_bitmap_set( d->clocks, d->i ); 		/* set clock mark */
  d->track[ d->i++ ] = 0xfc;			/* index mark */
  return 0;
}

static int
postindex_len( int gaptype )		/* preindex gap and index mark */
{
  disk_gap_t *g = &gaps[ gaptype ];
  return g->len[1];
}

static int
postindex_add( libspectrum_disk_t *d, int gaptype )		/* postindex gap */
{
  return gap_add( d, 1, gaptype );
}

static int
gap4_add( libspectrum_disk_t *d, int gaptype )
{
  int len = d->bpt - d->i;
  disk_gap_t *g = &gaps[ gaptype ];

  if( len < 0 ) {
    return 1;
  }
/*------------------------------     GAP IV     ------------------------------*/
  memset( d->track + d->i, g->gap, len ); /* GAP IV fill until end of track */
  d->i = d->bpt;
  return 0;
}

#define SECLEN_128 0x00
#define SECLEN_256 0x01
#define SECLEN_512 0x02
#define SECLEN_1024 0x03
#define CRC_OK 0
#define CRC_ERROR 1

/*
  [ ....GAP.... ] [ ... SYNC ... ] [ . MARK . ] [ .. DATA .. ] [ . CRC . ]
                 |------------------------------------------------------->
                                    ID
*/
static int
id_add( libspectrum_disk_t *d, int h, int t, int s, int l, int gaptype,
        int crc_error )
{
  libspectrum_word crc = 0xffff;
  disk_gap_t *g = &gaps[ gaptype ];
  if( d->i + g->sync_len + ( g->mark >= 0 ? 3 : 0 ) + 7 >= d->bpt )
    return 1;
/*------------------------------   sync    ---------------------------*/
  memset( d->track + d->i, g->sync, g->sync_len ); d->i += g->sync_len;
  if( g->mark >= 0 ) {
    memset( d->track + d->i , g->mark, 3 );
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    crc = libspectrum_disk_crc( crc, g->mark );
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    crc = libspectrum_disk_crc( crc, g->mark );
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    crc = libspectrum_disk_crc( crc, g->mark );
  }
/*------------------------------     mark     ------------------------------*/
  if( g->mark < 0 )			/* FM */
    libspectrum_bitmap_set( d->clocks, d->i ); 	/* set clock mark */
  d->track[ d->i++ ] = 0xfe;		/* ID mark */
  crc = libspectrum_disk_crc( crc, 0xfe );
/*------------------------------     header     ------------------------------*/
  d->track[ d->i++ ] = t; crc = libspectrum_disk_crc( crc, t );
  d->track[ d->i++ ] = h; crc = libspectrum_disk_crc( crc, h );
  d->track[ d->i++ ] = s; crc = libspectrum_disk_crc( crc, s );
  d->track[ d->i++ ] = l; crc = libspectrum_disk_crc( crc, l );
  d->track[ d->i++ ] = crc >> 8;
  if( crc_error ) {
    d->track[ d->i++ ] = (~crc) & 0xff;	/* record a CRC error */
  } else {
    d->track[ d->i++ ] = crc & 0xff;	/* CRC */
  }
/*------------------------------     GAP II     ------------------------------*/
  return gap_add( d, 2, gaptype );
}

/*
  [ ....GAP.... ] [ ... SYNC ... ] [ . MARK . ] [ .. DATA .. ] [ . CRC . ]
 |-------------------------------------------->
                  datamark
*/
static int
datamark_add( libspectrum_disk_t *d, int ddam, int gaptype )
{
  disk_gap_t *g = &gaps[ gaptype ];
  if( d->i + g->len[2] + g->sync_len + ( g->mark >= 0 ? 3 : 0 ) + 1 >= d->bpt )
    return 1;
/*------------------------------   sync    ---------------------------*/
  memset( d->track + d->i, g->sync, g->sync_len ); d->i += g->sync_len;
  if( g->mark >= 0 ) {
    memset( d->track + d->i , g->mark, 3 );
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
    libspectrum_bitmap_set( d->clocks, d->i ); d->i++;
  }
/*------------------------------     mark     ------------------------------*/
  if( g->mark < 0 )			/* FM */
    libspectrum_bitmap_set( d->clocks, d->i ); 	/* set clock mark */
  d->track[ d->i++ ] = ddam ? 0xf8 : 0xfb;	/* DATA mark 0xf8 -> deleted data */
  return 0;
}

#define NO_DDAM 0
#define DDAM 1
#define NO_AUTOFILL -1
/*
   copy data from *buffer and update *buffer->idx
   if 'buffer' == NULL, then copy data bytes from 'data'
   if gaptype == -1, then READ DAM/DDAM if ddam not correct fail...
     except if ddam & 0x80 == 0x80, then overwrite dam according to
     ddam & 0x7f, or ddam == -1 -> ignore ddam and overwrite the
     first found data...
*/
static int
data_add( libspectrum_disk_t *d, buffer_t *buffer, unsigned char *data,
          int len, int ddam, int gaptype, int crc_error, int autofill,
          int *start_data )
{
  int length, dam, fmf = 0;
  libspectrum_word crc = 0xffff;
  disk_gap_t *g = &gaps[ gaptype ];

  if( gaptype == -1 ) {
    int del;
    datamark_read( d, &del, &fmf );
/* overwrite dam with the given value */
    if( ddam == -1 ) {
      ;
    } else if( ddam & 0x80 ) {
      ddam = !!( ddam & 0x7f );
      d->track[d->i - 1] = ddam ? 0xf8 : 0xfb;
    } else if( !del != !ddam ) {
      return 2;
    }
  } else {
    if( datamark_add( d, ddam, gaptype ) )
      return 1;
    fmf = g->mark >= 0 ? 1 : 0;
  }
/* read back the real DAM (theoretically it can be other than fb/f8 [0xf8...0xfe] ) */
  dam = d->track[d->i - 1];
/* MFM crc += 3x 0xa1 */
  if( fmf ) {
    crc = libspectrum_disk_crc( crc, 0xa1 );
    crc = libspectrum_disk_crc( crc, 0xa1 );
    crc = libspectrum_disk_crc( crc, 0xa1 );
  }
  crc = libspectrum_disk_crc( crc, dam );	/* deleted or normal */
  if( len < 0 )
    goto header_crc_error;			/* CRC error */
  if( d->i + len + 2 >= d->bpt )  		/* too many data bytes */
    return 1;
/*------------------------------      data      ------------------------------*/
  if( start_data != NULL ) *start_data = d->i;	/* record data start position */
  if( buffer == NULL ) {
    memcpy( d->track + d->i, data, len );
    length = len;
  } else {
    length = buffavail( buffer );
    if( length > len ) length = len;
    buffread( d->track + d->i, length, buffer );
  }
  if( length < len ) {	/* autofill with 'autofill' */
    if( autofill < 0 )
      return 1;
    while( length < len ) {
      d->track[ d->i + length ] = autofill;
      length++;
    }
  }
  length = 0;
  while( length < len ) {	/* calculate CRC */
    crc = libspectrum_disk_crc( crc, d->track[ d->i ] );
    d->i++;
    length++;
  }
  if( crc_error ) crc ^= 1;	/* mess up CRC */
  d->track[ d->i++ ] = crc >> 8; d->track[ d->i++ ] = crc & 0xff;    /* CRC */
/*------------------------------     GAP III    ------------------------------*/
header_crc_error:
  return ( gaptype < 0 ? 0 : gap_add( d, 3, gaptype ) );
}

/*
  d    -> disk (d->track, d->i)
  data -> data bytes
  len  -> length of data (sector len)
  ddam -> deleted (!0) or normal (0) data
          if gaptype == -1 and ddam & 0x80 == 0x80 then overwrite dam as well
            (ddam & 0x7f -> 0: normal, !0 : deleted),
            if ddam & 0x80 == 0 then seek to first normal and/or deleted data,
            except if ddam = -1 -> than the first data overwritten
  gaptype -> if -1 do not add gap and data mark, just seek to next data mark and
             overwrite data, else write `gaptype' gap, datamark end "after gap"
  crc_error -> if !0 write messed up CRC
  start_data -> if !NULL the index of data pos (d->i)
*/
int
libspectrum_disk_data_add( libspectrum_disk_t *d, unsigned char *data, int len,
                           int ddam, int gaptype, int crc_error,
                           int *start_data )
{
  if( !d ) return 1;

  return data_add( d, NULL, data, len, ddam, gaptype, crc_error, -1,
                   start_data );
}

static int
calc_sectorlen( int mfm, int sector_length, int gaptype )
{
  int len = 0;
  disk_gap_t *g = &gaps[ gaptype ];

/*------------------------------     ID        ------------------------------*/
  len += g->sync_len + ( g->mark >= 0 ? 3 : 0 ) + 7;
/*------------------------------     GAP II    ------------------------------*/
  len += g->len[2];
/*---------------------------------  data   ---------------------------------*/
  len += g->sync_len + ( g->mark >= 0 ? 3 : 0 ) + 1;		/* DAM */
  len += sector_length;
  len += 2;		/* CRC */
/*------------------------------    GAP III    ------------------------------*/
  len += g->len[3];
  return len;
}

static int
calc_lenid( int sector_length )
{
  int id = 0;

  while( sector_length > 0x80 ) {
    id++;
    sector_length >>= 1;
  }

  return id;
}

#define NO_INTERLEAVE 1
#define INTERLEAVE_2 2
#define INTERLEAVE_OPUS 13
#define NO_PREINDEX 0
#define PREINDEX 1

static int
trackgen( libspectrum_disk_t *d, buffer_t *buffer, int head, int track,
          int sector_base, int sectors, int sector_length, int preindex,
          int gap, int interleave, int autofill )
{
  int i, s, pos;
  int slen = calc_sectorlen( ( d->density != LIBSPECTRUM_DISK_SD &&
                               d->density != LIBSPECTRUM_DISK_8_SD ),
                             sector_length, gap );
  int idx;

  d->i = 0;
  DISK_SET_TRACK( d, head, track );
  if( preindex && preindex_add( d, gap ) )
    return 1;
  if( postindex_add( d, gap ) )
    return 1;

  idx = d->i;
  pos = i = 0;
  for( s = sector_base; s < sector_base + sectors; s++ ) {
    d->i = idx + pos * slen;
    if( id_add( d, head, track, s, calc_lenid( sector_length ), gap, CRC_OK ) )
      return 1;
    if( data_add( d, buffer, NULL, sector_length, NO_DDAM, gap, CRC_OK,
                  autofill, NULL ) )
      return 1;
    pos += interleave;
    if( pos >= sectors ) {	/* wrap around */
      pos -= sectors;
      if( pos <= i ) {		/* we fill this pos already */
        pos++;			/* skip one more pos */
        i++;
      }
    }
  }
  d->i = idx + sectors * slen;
  return gap4_add( d, gap );
}

/* close and destroy a disk structure and data */
void
libspectrum_disk_close( libspectrum_disk_t *d )
{
  if( !d ) return;

  if( d->data != NULL ) {
    libspectrum_free( d->data );
    d->data = NULL;
  }

  if( d->filename != NULL ) {
    libspectrum_free( d->filename );
    d->filename = NULL;
  }

  d->type = LIBSPECTRUM_DISK_TYPE_NONE;
}

/*
 *  if d->density == LIBSPECTRUM_DISK_DENS_AUTO =>
 *                            use d->tlen if d->bpt == 0
 *                             or use d->bpt to determine d->density
 *  or use d->density
 */
static int
disk_alloc( libspectrum_disk_t *d )
{
  size_t dlen;

  if( d->density != LIBSPECTRUM_DISK_DENS_AUTO ) {
    d->bpt = disk_bpt[ d->density ];
  } else if( d->bpt > 12500 ) {
    return d->status = LIBSPECTRUM_DISK_UNSUP;
  } else if( d->bpt > 10416 ) {
    d->density = LIBSPECTRUM_DISK_HD;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_HD ];
  } else if( d->bpt > 6500 ) {
    d->density = LIBSPECTRUM_DISK_8_DD;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_8_DD ];
  } else if( d->bpt > 6250 ) {
    d->density = LIBSPECTRUM_DISK_DD_PLUS;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_DD_PLUS ];
  } else if( d->bpt > 5208 ) {
    d->density = LIBSPECTRUM_DISK_DD;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_DD ];
  } else if( d->bpt > 3125 ) {
    d->density = LIBSPECTRUM_DISK_8_SD;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_8_SD ];
  } else if( d->bpt > 0 ) {
    d->density = LIBSPECTRUM_DISK_SD;
    d->bpt = disk_bpt[ LIBSPECTRUM_DISK_SD ];
  }

  if( d->bpt > 0 )
    d->tlen = 4 + d->bpt + 3 * DISK_CLEN( d->bpt );

  dlen = d->sides * d->cylinders * d->tlen;	/* track len with clock and other marks */
  if( dlen == 0 ) return d->status = LIBSPECTRUM_DISK_GEOM;

  d->data = libspectrum_new0( libspectrum_byte, dlen );

  return d->status = LIBSPECTRUM_DISK_OK;
}

/* create a new unformatted disk  */
libspectrum_disk_error_t
libspectrum_disk_new( libspectrum_disk_t *d, int sides, int cylinders,
                      libspectrum_disk_dens_t density,
                      libspectrum_disk_type_t type )
{
  if( !d ) return LIBSPECTRUM_DISK_BADPARAM;
  d->filename = NULL;

  if( density < LIBSPECTRUM_DISK_DENS_AUTO || density > LIBSPECTRUM_DISK_HD ||	/* unknown density */
      type <= LIBSPECTRUM_DISK_TYPE_NONE || type >= LIBSPECTRUM_DISK_TYPE_LAST || /* unknown type */
      sides < 1 || sides > 2 ||				/* 1 or 2 side */
      cylinders < 35 || cylinders > 83 )		/* 35 .. 83 cylinder */
    return d->status = LIBSPECTRUM_DISK_GEOM;

  d->type = type;
  d->density =
    density == LIBSPECTRUM_DISK_DENS_AUTO ? LIBSPECTRUM_DISK_DD : density;
  d->sides = sides;
  d->cylinders = cylinders;

  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  d->wrprot = 0;
  d->dirty = 1;
  disk_update_tlens( d );

  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
alloc_uncompress_buffer( unsigned char **buffer, int length )
{
  unsigned char *b;

  if( *buffer != NULL )				/* return if allocated */
    return 0;

  b = libspectrum_new0( unsigned char, length );
  if( b == NULL )
    return 1;
  *buffer = b;

  return 0;
}

libspectrum_disk_error_t
libspectrum_disk_preformat( libspectrum_disk_t *d )
{
  buffer_t buffer;

  if( !d ) return LIBSPECTRUM_DISK_BADPARAM;

  buffer.len = 0;
  buffer.idx = 0;

  if( trackgen( d, &buffer, 0, 0, 0xff, 1, 128,
                NO_PREINDEX, GAP_MINIMAL_MFM, NO_INTERLEAVE, 0xff ) )
    return LIBSPECTRUM_DISK_GEOM;

  if( trackgen( d, &buffer, 0, 2, 0xfe, 1, 128,
                NO_PREINDEX, GAP_MINIMAL_MFM, NO_INTERLEAVE, 0xff ) )
    return LIBSPECTRUM_DISK_GEOM;

  return LIBSPECTRUM_DISK_OK;
}

/* open a disk image */
#define GEOM_CHECK \
  if( d->sides < 1 || d->sides > 2 || d->cylinders < 1 || d->cylinders > 85 ) \
      return d->status = LIBSPECTRUM_DISK_GEOM

#ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
static int
udi_read_compressed( const libspectrum_byte *buffer,
                     size_t compr_size, size_t uncompr_size,
                     libspectrum_byte **data, size_t *data_size )
{
  libspectrum_error error;
  libspectrum_byte *tmp;
  size_t olength = uncompr_size;

  tmp = NULL;

  error = libspectrum_zlib_inflate( buffer, compr_size, &tmp, &olength );
  if( error ) return error;

  if( *data_size < uncompr_size ) {
    *data = libspectrum_renew( libspectrum_byte, *data, uncompr_size );
    *data_size = uncompr_size;
  }
  memcpy( *data, tmp, uncompr_size );
  libspectrum_free( tmp );

  return 0;
}

static int
udi_write_compressed( const libspectrum_byte *buffer,
                      size_t uncompr_size, size_t *compr_size,
                      libspectrum_byte **data, size_t *data_size )
{
  libspectrum_error error;
  libspectrum_byte *tmp;

  tmp = NULL;
  error = libspectrum_zlib_compress( buffer, uncompr_size,
                                     &tmp, compr_size );
  if( error ) return error;

  if( *data_size < *compr_size ) {
    *data = libspectrum_renew( libspectrum_byte, *data, *compr_size );
    *data_size = *compr_size;
  }
  memcpy( *data, tmp, *compr_size );
  libspectrum_free( tmp );

  return LIBSPECTRUM_ERROR_NONE;
}
#endif			/* #ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */

static void
udi_pack_tracks( libspectrum_disk_t *d )
{
  int i, tlen, clen, ttyp;
  libspectrum_byte *tmp;

  for( i = 0; i < d->sides * d->cylinders; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    tmp = d->track;
    ttyp = tmp[-1];
    tlen = tmp[-3] + 256 * tmp[-2];
    clen = DISK_CLEN( tlen );
    tmp += tlen;
    /* copy clock if needed */
    if( tmp != d->clocks )
      memcpy( tmp, d->clocks, clen );
    if( ttyp == 0x00 || ttyp == 0x01 ) continue;
    tmp += clen;
    if( ttyp & 0x02 ) {		/* copy FM marks */
      if( tmp != d->fm )
        memcpy( tmp, d->fm, clen );
      tmp += clen;
    }
    if( ! ( ttyp & 0x80 ) ) continue;
    if( tmp != d->weak )		/* copy WEAK marks*/
      memcpy( tmp, d->weak, clen );
  }
}

static void
udi_unpack_tracks( libspectrum_disk_t *d )
{
  int i, tlen, clen, ttyp;
  libspectrum_byte *tmp;
  libspectrum_byte mask[] = { 0xff, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe };

  for( i = 0; i < d->sides * d->cylinders; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    tmp = d->track;
    ttyp = tmp[-1];
    tlen = tmp[-3] + 256 * tmp[-2];
    clen = DISK_CLEN( tlen );
    tmp += tlen;
    if( ttyp & 0x80 ) tmp += clen;
    if( ttyp & 0x02 ) tmp += clen;
    if( ( ttyp & 0x80 ) ) {	/* copy WEAK marks*/
      if( tmp != d->weak )
        memcpy( d->weak, tmp, clen );
      tmp -= clen;
    } else {			/* clear WEAK marks*/
      memset( d->weak, 0, clen );
    }
    if( ttyp & 0x02 ) {		/* copy FM marks */
      if( tmp != d->fm )
        memcpy( d->fm, tmp, clen );
      tmp -= clen;
    } else {			/* set/clear FM marks*/
      memset( d->fm, ttyp & 0x01 ? 0xff : 0, clen );
      if( tlen % 8 ) {		/* adjust last byte */
        d->fm[clen - 1] &= mask[ tlen % 8 ];
      }
    }
    /* copy clock if needed */
    if( tmp != d->clocks )
      memcpy( d->clocks, tmp, clen );
  }
}

/* calculate track len from type, if type eq. 0x00/0x01/0x02/0x80/0x81/0x82
   !!! not for 0x83 nor 0xf0 !!!
*/
#define UDI_TLEN( type, bpt ) ( ( bpt ) + DISK_CLEN( bpt ) * ( 1 + \
                                        ( type & 0x02 ? 1 : 0 ) + \
                                        ( type & 0x80 ? 1 : 0 ) ) )

static int
udi_uncompress_tracks( libspectrum_disk_t *d )
{
  int i;
  libspectrum_byte *data = NULL;
#ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
  size_t data_size = 0;
  int bpt, tlen, clen, ttyp;
#endif			/* #ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */

  for( i = 0; i < d->sides * d->cylinders; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    if( d->track[-1] != 0xf0 ) continue;	/* if not compressed */

#ifndef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
    /* if libspectrum cannot support */
    return d->status = LIBSPECTRUM_DISK_UNSUP;
#else 			/* #ifndef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */
    clen = d->track[-3] + 256 * d->track[-2] + 1;
    ttyp = d->track[0];				/* compressed track type   */
    bpt = d->track[1] + 256 * d->track[2];	/* compressed track len... */
    tlen = UDI_TLEN( ttyp, bpt );
    d->track[-1] = ttyp;
    d->track[-3] = d->track[1];
    d->track[-2] = d->track[2];
    if( udi_read_compressed( d->track + 3, clen, tlen, &data, &data_size ) ) {
      if( data ) libspectrum_free( data );
      return d->status = LIBSPECTRUM_DISK_UNSUP;
    }
    memcpy( d->track, data, tlen );		/* read track */
#endif			/* #ifndef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */
  }

  if( data ) libspectrum_free( data );

  return LIBSPECTRUM_DISK_OK;
}

#ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
static int
udi_compress_tracks( libspectrum_disk_t *d )
{
  int i, tlen;
  libspectrum_byte *data = NULL;
  size_t clen, data_size = 0;

  for( i = 0; i < d->sides * d->cylinders; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    if( d->track[-1] == 0xf0 ) continue;	/* already compressed??? */

    tlen = UDI_TLEN( d->track[-1], d->track[-3] + 256 * d->track[-2] );

    /* if fail to compress, skip ... */
    if( udi_write_compressed( d->track, tlen, &clen, &data, &data_size ) ||
        clen < 1 ) continue;

    /* if compression too large, skip... */
    if( clen > 65535 || clen >= tlen ) continue;

    d->track[0] = d->track[-1];			/* track type... */
    d->track[1] = d->track[-3];			/* compressed track len... */
    d->track[2] = d->track[-2];			/* compressed track len... */
    memcpy( d->track + 3, data, clen );		/* read track */
    clen--;
    d->track[-1] = 0xf0;
    d->track[-3] = clen & 0xff;
    d->track[-2] = ( clen >> 8 ) & 0xff;
  }
  if( data ) libspectrum_free( data );
  return LIBSPECTRUM_DISK_OK;
}
#endif			/* #ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */

static int
open_udi( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, bpt, ttyp, tlen, error;
  size_t eof;
  libspectrum_dword crc;

  crc = ~(libspectrum_dword) 0;

  /* check file length */
  eof = buff[4] + 256 * buff[5] + 65536 * buff[6] + 16777216 * buff[7];
  if( eof != buffer->len - 4 )
    return d->status = LIBSPECTRUM_DISK_OPEN;

  /* check CRC32 */
  for( i = 0; i < eof; i++ )
    crc = crc_udi( crc, buff[i] );
  if( crc != buff[eof] + 256 * buff[eof + 1] + 65536 * buff[eof + 2] +
             16777216 * buff[eof + 3] )
    return d->status = LIBSPECTRUM_DISK_OPEN;

  d->sides = buff[10] + 1;
  d->cylinders = buff[9] + 1;
  GEOM_CHECK;
  d->density = LIBSPECTRUM_DISK_DENS_AUTO;
  buffer->idx = 16;
  d->bpt = 0;

  /* scan file for the longest track */
  for( i = 0; buffer->idx < eof; i++ ) {
    if( buffavail( buffer ) < 3 )
      return d->status = LIBSPECTRUM_DISK_OPEN;
    ttyp = buff[0];
    if( ttyp != 0x00 && ttyp != 0x01 && ttyp != 0x02 && ttyp != 0x80 &&
        ttyp != 0x81 && ttyp != 0x82 && ttyp != 0x83 && ttyp != 0xf0 )
      return d->status = LIBSPECTRUM_DISK_UNSUP;

    /* if libspectrum cannot support */
#ifndef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
    if( ttyp == 0xf0 ) d->status = LIBSPECTRUM_DISK_UNSUP;
#endif			/* #ifndef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */
    if( ttyp == 0x83 ) {			/* multiple read */
      if( i == 0 ) return d->status = LIBSPECTRUM_DISK_GEOM;	/* cannot be first track */
      i--; bpt = 0;					/* not a real track */
      tlen = buff[1] + 256 * buff[2];		/* current track len... */
      tlen = ( tlen & 0xfff8 ) * ( tlen & 0x07 );
    } else if( ttyp == 0xf0 ) {			/* compressed track */
      if( buffavail( buffer ) < 7 )
        return d->status = LIBSPECTRUM_DISK_OPEN;
      bpt = buff[4] + 256 * buff[5];
      tlen = 7 + buff[1] + 256 * buff[2];
    } else {
      bpt = buff[1] + 256 * buff[2];		/* current track len... */
      tlen = 3 + UDI_TLEN( ttyp, bpt );
    }
    if( bpt > d->bpt )
      d->bpt = bpt;
    if( buffseek( buffer, tlen, SEEK_CUR ) == -1 )
      return d->status = LIBSPECTRUM_DISK_OPEN;
  }

  if( d->bpt == 0 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  bpt = d->bpt;		/* save the maximal value */
  d->tlen = 3 + bpt + 3 * DISK_CLEN( bpt );
  d->bpt = 0;		/* we know exactly the track len... */
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;
  d->bpt = bpt;		/* restore the maximal byte per track */
  buffer->idx = 16;

  for( i = 0; buffer->idx < eof; i++ ) {
    DISK_SET_TRACK_IDX( d, i );
    ttyp = buff[0];
    bpt = buff[1] + 256 * buff[2];		/* current track len... */

    memset( d->track, 0x4e, d->bpt );		/* fillup */
    /* read track + clocks */
    if( ttyp == 0x83 ) {			/* multiple read */
      i--;					/* not a real track */
      DISK_SET_TRACK_IDX( d, i );		/* back to previous track */
      d->weak += buff[3] + 256 * buff[4];	/* add offset to weak */
      tlen = ( buff[1] + 256 * buff[2] ) >> 3;	/* weak len in bytes */
      for( tlen--; tlen >= 0; tlen-- )
        d->weak[tlen] = 0xff;
      tlen = buff[1] + 256 * buff[2];		/* current track len... */
      tlen = ( tlen & 0xfff8 ) * ( tlen & 0x07 );
      buffseek( buffer, tlen, SEEK_CUR );
    } else {
      if( ttyp == 0xf0 )			/* compressed */
        tlen = bpt + 4;
      else
        tlen = UDI_TLEN( ttyp, bpt );
      d->track[-1] = ttyp;
      d->track[-3] = buff[1];
      d->track[-2] = buff[2];
      buffer->idx += 3;
      buffread( d->track, tlen, buffer );	/* first read data */
    }
  }
  error = udi_uncompress_tracks( d );
  if( error ) return error;
  udi_unpack_tracks( d );

  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
open_img_mgt_opd( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, sectors, seclen;

  buffer->idx = 0;

  /* guess geometry of disk:
   * 2*80*10*512, 1*80*10*512, 1*40*10*512, 1*40*18*256, 1*80*18*256,
   * 2*80*18*256
   */
  if( buffer->len == 2*80*10*512 ) {
    d->sides = 2; d->cylinders = 80; sectors = 10; seclen = 512;
  } else if( buffer->len == 1*80*10*512 ) {
    /* we cannot distinguish between a single sided 80 track image
     * and a double sided 40 track image (2*40*10*512) */
    d->sides = 1; d->cylinders = 80; sectors = 10; seclen = 512;
  } else if( buffer->len == 1*40*10*512 ) {
    d->sides = 1; d->cylinders = 40; sectors = 10; seclen = 512;
  } else if( buffer->len == 1*40*18*256 ) {
    d->sides = 1; d->cylinders = 40; sectors = 18; seclen = 256;
  } else if( buffer->len == 1*80*18*256 ) {
    /* we cannot distinguish between a single sided 80 track image
     * and a double sided 40 track image (2*40*18*256) */
    d->sides = 1; d->cylinders = 80; sectors = 18; seclen = 256;
  } else if( buffer->len == 2*80*18*256 ) {
    d->sides = 2; d->cylinders = 80; sectors = 18; seclen = 256;
  } else {
    return d->status = LIBSPECTRUM_DISK_GEOM;
  }

  /* create a DD disk */
  d->density = LIBSPECTRUM_DISK_DD;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  if( d->type == LIBSPECTRUM_DISK_IMG ) {	/* IMG out-out */
    for( j = 0; j < d->sides; j++ ) {
      for( i = 0; i < d->cylinders; i++ ) {
        if( trackgen( d, buffer, j, i, 1, sectors, seclen,
                      NO_PREINDEX, GAP_MGT_PLUSD, NO_INTERLEAVE, NO_AUTOFILL ) )
          return d->status = LIBSPECTRUM_DISK_GEOM;
      }
    }
  } else {			/* MGT / OPD alt */
    for( i = 0; i < d->cylinders; i++ ) {
      for( j = 0; j < d->sides; j++ ) {
        if( trackgen( d, buffer, j, i, d->type == LIBSPECTRUM_DISK_MGT ? 1 : 0,
                      sectors, seclen, NO_PREINDEX, GAP_MGT_PLUSD,
                      d->type == LIBSPECTRUM_DISK_MGT ? NO_INTERLEAVE :
                                                        INTERLEAVE_OPUS,
                      NO_AUTOFILL ) )
          return d->status = LIBSPECTRUM_DISK_GEOM;
      }
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
open_d40_d80( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, sectors, seclen;

  if( buffavail( buffer ) < 180 )
    return d->status = LIBSPECTRUM_DISK_OPEN;

  /* guess geometry of disk */
  d->sides =     buff[0xb1] & 0x10 ? 2 : 1;
  d->cylinders = buff[0xb2];
  sectors =      buff[0xb3];

  if( d->sides < 1 || d->sides > 2 || d->cylinders > 83 || sectors > 127 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  seclen = 512;

  buffer->idx = 0;

  /* create a DD disk */
  d->density = LIBSPECTRUM_DISK_DD;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  for( i = 0; i < d->cylinders; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      if( trackgen( d, buffer, j, i, 1, sectors, seclen,
                    NO_PREINDEX, GAP_MGT_PLUSD, NO_INTERLEAVE, NO_AUTOFILL ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/* preindex
 *
 */
static int
open_sad( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, sectors, seclen, preindex;

  preindex = d->flag & LIBSPECTRUM_DISK_FLAG_PREIDX ? 1 : 0;
  d->sides = buff[18];
  d->cylinders = buff[19];
  GEOM_CHECK;
  sectors = buff[20];
  seclen = buff[21] * 64;
  buffer->idx = 22;

  /* create a DD disk */
  d->density = LIBSPECTRUM_DISK_DD;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  for( j = 0; j < d->sides; j++ ) {
    for( i = 0; i < d->cylinders; i++ ) {
      if( trackgen( d, buffer, j, i, 1, sectors, seclen, preindex,
                    GAP_MGT_PLUSD, NO_INTERLEAVE, NO_AUTOFILL ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
open_trd( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, sectors, seclen;

  if( buffseek( buffer, 8*256, SEEK_CUR ) == -1 )
    return d->status = LIBSPECTRUM_DISK_OPEN;
  if( buffavail( buffer ) < 256 )
    return d->status = LIBSPECTRUM_DISK_OPEN;

  if( buff[231] != 0x10 || buff[227] < 0x16 || buff[227] > 0x19 )
    return d->status = LIBSPECTRUM_DISK_OPEN;	/*?*/

  /* guess geometry of disk */
  d->sides =  buff[227] & 0x08 ? 1 : 2;
  d->cylinders = buff[227] & 0x01 ? 40 : 80;
  /* we have more tracks then on a standard disk... */
  if( buffer->len > d->sides * d->cylinders * 16 * 256 ) {
    for( i = d->cylinders + 1; i < 83; i++ ) {
      if( d->sides * i * 16 * 256 >= buffer->len ) break;
    }
    d->cylinders = i;
  }
  sectors = 16; seclen = 256;

  /* create a DD disk */
  d->density = LIBSPECTRUM_DISK_DD;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  buffer->idx = 0;
  for( i = 0; i < d->cylinders; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      if( trackgen( d, buffer, j, i, 1, sectors, seclen,
                    NO_PREINDEX, GAP_TRDOS, INTERLEAVE_2, 0x00 ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

/* FIXME: move back to fuse//disk.c
  if( settings_current.auto_load ) {
    position_context_save( d, &context );
    trdos_insert_boot_loader( d );
    position_context_restore( d, &context );
  }
*/
  return d->status = LIBSPECTRUM_DISK_OK;
}

/* preindex
 *
 */
static int
open_fdi( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, h, gap, preindex;
  int bpt, bpt_fm, max_bpt = 0, max_bpt_fm = 0;
  int data_offset, track_offset, head_offset, sector_offset;
  libspectrum_byte head[256];

  preindex = d->flag & LIBSPECTRUM_DISK_FLAG_PREIDX ? 1 : 0;
  d->wrprot = buff[0x03] == 1 ? 1 : 0;
  d->sides = buff[0x06] + 256 * buff[0x07];
  d->cylinders = buff[0x04] + 256 * buff[0x05];
  GEOM_CHECK;
  data_offset = buff[0x0a] + 256 * buff[0x0b];
  h = 0x0e + buff[0x0c] + 256 * buff[0x0d];		/* save head start */
  head_offset = h;

  /* first determine the longest track */
  d->bpt = 0;
  for( i = 0; i < d->cylinders * d->sides; i++ ) {	/* ALT */
    buffer->idx = head_offset;
    if( buffread( head, 7, buffer ) != 1 )	/* 7 := track head  */
      return d->status = LIBSPECTRUM_DISK_OPEN;
    bpt = postindex_len( GAP_MINIMAL_MFM ) +
          ( preindex ? preindex_len( GAP_MINIMAL_MFM ) : 0 ) + 6; /* +gap4 */
    bpt_fm = postindex_len( GAP_MINIMAL_FM ) +
             ( preindex ? preindex_len( GAP_MINIMAL_FM ) : 0 ) + 3;  /* +gap4 */
    for( j = 0; j < head[0x06]; j++ ) {		/* calculate track len */
      if( j % 35 == 0 ) {				/* 35-sector header */
        if( buffread( head + 7, 245, buffer ) != 1 )	/* 7*35 := max 35 sector head */
          return d->status = LIBSPECTRUM_DISK_OPEN;
      }
      if( ( head[ 0x0b + 7 * ( j % 35 ) ] & 0x3f ) != 0 ) {
        bpt += calc_sectorlen( 1, 0x80 << head[ 0x0a + 7 * ( j % 35 ) ],
                               GAP_MINIMAL_MFM );
        bpt_fm += calc_sectorlen( 0, 0x80 << head[ 0x0a + 7 * ( j % 35 ) ],
                                  GAP_MINIMAL_FM );
      }
    }
    if( bpt > max_bpt )
      max_bpt = bpt;
    if( bpt_fm > max_bpt_fm )
      max_bpt_fm = bpt_fm;

    head_offset += 7 + 7 * head[ 0x06 ];
  }

  if( max_bpt == 0 || max_bpt_fm == 0 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  d->density = LIBSPECTRUM_DISK_DENS_AUTO;		/* disk_alloc use d->bpt */
  if( max_bpt_fm < 3000 ) {		/* we choose an SD disk with FM */
    d->bpt = max_bpt_fm;
    gap = GAP_MINIMAL_FM;
  } else {
    d->bpt = max_bpt;
    gap = GAP_MINIMAL_MFM;
  }
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;
    /* start reading the tracks */

  head_offset = h;			/* restore head start */
  for( i = 0; i < d->cylinders * d->sides; i++ ) {	/* ALT */
    buffer->idx = head_offset;
    buffread( head, 7, buffer );	/* 7 = track head */
    track_offset = head[0x00] + 256 * head[0x01] +
                   65536 * head[0x02] + 16777216 * head[0x03];
    DISK_SET_TRACK_IDX( d, i );
    d->i = 0;
    if( preindex )
      preindex_add( d, gap );
    postindex_add( d, gap );
    for( j = 0; j < head[0x06]; j++ ) {
      if( j % 35 == 0 ) {  /* if we have more than 35 sector in a track,
                              we have to seek back to the next sector
                              headers and read it ( max 35 sector header */
        buffer->idx = head_offset + 7 *( j + 1 );
        buffread( head + 7, 245, buffer );	/* 7*35 := max 35 sector head */
      }
      id_add( d, head[ 0x08 + 7 * ( j % 35 ) ], head[ 0x07 + 7*( j % 35 ) ],
              head[ 0x09 + 7*( j % 35 ) ], head[ 0x0a + 7*( j % 35 ) ], gap,
              ( head[ 0x0b + 7*( j % 35 ) ] & 0x3f ) ? CRC_OK : CRC_ERROR );
      sector_offset = head[ 0x0c + 7 * ( j % 35 ) ] +
                      256 * head[ 0x0d + 7 * ( j % 35 ) ];
      buffer->idx = data_offset + track_offset + sector_offset;
      data_add( d, buffer, NULL, ( head[ 0x0b + 7 * ( j % 35 ) ] & 0x3f ) == 0 ?
                                 -1 : 0x80 << head[ 0x0a + 7 * ( j % 35 ) ],
                head[ 0x0b + 7 * ( j % 35 ) ] & 0x80 ? DDAM : NO_DDAM,
                gap, CRC_OK, NO_AUTOFILL, NULL );
    }
    head_offset += 7 + 7 * head[0x06];
    gap4_add( d, gap );
  }
  return d->status = LIBSPECTRUM_DISK_OK;
}

static void
cpc_set_weak_range( libspectrum_disk_t *d, int idx, buffer_t *buffer, int n,
                    int len )
{
  int i, j, first = -1, last = -1;
  libspectrum_byte *t, *w;

  t = d->track + idx;
  w = buffer->data + buffer->idx;

  for( i = 0; i < len; i++, t++, w++ ) {
    for( j = 0; j < n - 1; j++ ) {
      if( *t != w[j * len] ) {
        if( first == -1 ) first = idx + i;
        last = idx + i;
      }
    }
  }
  if( first == -1 || last == -1 ) {
    return;
  }
  for( ; first <= last; first++ ) {
    libspectrum_bitmap_set( d->weak, first );
  }
}

/* preindex
 *
 */
static int
open_cpc( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, seclen, idlen, gap, preindex, idx;
  int bpt, max_bpt = 0, trlen;
  int fix[84], plus3_fix;
  unsigned char *hdrb;

  preindex = d->flag & LIBSPECTRUM_DISK_FLAG_PREIDX ? 1 : 0;
  d->sides = buff[0x31];
  d->cylinders = buff[0x30];			/* maximum number of tracks */
  GEOM_CHECK;
/* first scan for the longest track */
  if( *buffer->data == 'M' ) {
    /* All track sizes in the standard disk image must be the same */
    trlen = buff[0x32] + 256 * buff[0x33];
  } else {
    trlen = -1;
  }
  buffer->idx = 256;
  for( i = 0; i < d->sides*d->cylinders; i++ ) {
    /* ignore Sector Offset block */
    if( buffavail( buffer ) >= 13 && !memcmp( buff, "Offset-Info\r\n", 13 ) ) {
      buffer->idx = buffer->len;
    }

      /* sometimes in the header there are more track than in the file */
    if( buffavail( buffer ) == 0 ) {			/* no more data */
      if( d->flag & LIBSPECTRUM_DISK_FLAG_NOADD_MISSING )
        d->cylinders = i / d->sides + i % d->sides;	/* the real cylinder number */
      break;
    }
    if( buffavail( buffer ) < 256 ||
        memcmp( buff, "Track-Info", 10 ) )		/* check track header */
      return d->status = LIBSPECTRUM_DISK_OPEN;

    gap = (unsigned char)buff[0x16] == 0xff ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM;
    plus3_fix = 0;
    while( i < buff[0x10] * d->sides + buff[0x11] ) {
      if( i < 84 ) fix[i] = 0;
      i++;
    }
    if( i >= d->sides*d->cylinders )
      return d->status = LIBSPECTRUM_DISK_OPEN;

    if( i != buff[0x10] * d->sides + buff[0x11] )	/* problem with track idx */
      fprintf( stderr, "Track/head info mismatch! (exp:%d got:%d /max:%d/)\n",
               i, buff[0x10] * d->sides, d->sides*d->cylinders );

    bpt = postindex_len( gap ) +
          ( preindex ? preindex_len( gap ) : 0 ) +
          ( gap == GAP_MINIMAL_MFM ? 6 : 3 );	/* gap4 */

    for( j = 0; j < buff[0x15]; j++ ) {			/* each sector */
      seclen = d->type == LIBSPECTRUM_DISK_ECPC ? buff[ 0x1e + 8 * j ] +
                                                  256 * buff[ 0x1f + 8 * j ]
                                                : 0x80 << buff[ 0x1b + 8 * j ];
      idlen = 0x80 << buff[ 0x1b + 8 * j ];	/* sector length from ID */
      if( idlen != 0 && idlen <= ( 0x80 << 0x08 ) && 		/* idlen is o.k. */
          seclen > idlen && seclen % idlen )			/* seclen != N * len */
        return d->status = LIBSPECTRUM_DISK_OPEN;

      bpt += calc_sectorlen( gap == GAP_MINIMAL_MFM ? 1 : 0,
                             seclen > idlen ? idlen : seclen, gap );
      if( i < 84 && d->flag & LIBSPECTRUM_DISK_FLAG_PLUS3_CPC ) {
        if( j == 0 && buff[ 0x1b + 8 * j ] == 6 && seclen > 6144 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_4;
        else if( j == 0 && buff[ 0x1b + 8 * j ] == 6 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_1;
        else if( j == 0 &&
                 buff[ 0x18 + 8 * j ] == j && buff[ 0x19 + 8 * j ] == j &&
                 buff[ 0x1a + 8 * j ] == j && buff[ 0x1b + 8 * j ] == j )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_3;
        else if( j == 0 &&
                 buff[ 0x18 + 8 * j ] == 1 && buff[ 0x19 + 8 * j ] == 1 &&
                 buff[ 0x1a + 8 * j ] == 1 && buff[ 0x1b + 8 * j ] == 1 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_3a;
        else if( j == 1 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_1 &&
                 buff[ 0x1b + 8 * j ] == 2 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_2;
        else if( i == 38 && j == 0 && buff[ 0x1b + 8 * j ] == 2 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_CPC_5;
        else if( j > 1 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_2 &&
                 buff[ 0x1b + 8 * j ] != 2 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_NONE;
        else if( j > 0 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_3 &&
                 ( buff[ 0x18 + 8 * j ] != j || buff[ 0x19 + 8 * j ] != j ||
                   buff[ 0x1a + 8 * j ] != j || buff[ 0x1b + 8 * j ] != j ) )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_NONE;
        else if( j > 0 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_3a &&
                 ( buff[ 0x18 + 8 * j ] != j + 1 || buff[ 0x19 + 8 * j ] != j + 1 ||
                   buff[ 0x1a + 8 * j ] != j + 1 || buff[ 0x1b + 8 * j ] != j + 1 ) )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_NONE;
        else if( j > 10 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_2 )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_NONE;
        else if( i == 38 && j > 0 && plus3_fix == LIBSPECTRUM_DISK_OFLAG_CPC_5 &&
                 buff[ 0x1b + 8 * j ] != 2 - ( j & 1 ) )
          plus3_fix = LIBSPECTRUM_DISK_OFLAG_NONE;
      }
    }
    if( i < 84 ) {
      fix[i] = plus3_fix;
      d->open_flag |= plus3_fix;
      if( fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_4 )
        bpt = 6500; /* Type 1 variant DD+ (e.g. Coin Op Hits) */
      else if( fix[i] != LIBSPECTRUM_DISK_OFLAG_NONE )
        bpt = 6250; /* we assume a standard DD track */
    }
/* extended DSK image uses track size table */
    buffer->idx += ( trlen < 0 ? 256 * buffer->data[0x34 + i] : trlen );
    if( bpt > max_bpt )
      max_bpt = bpt;
  }
  if( max_bpt == 0 ) max_bpt = 6250;

  d->density = LIBSPECTRUM_DISK_DENS_AUTO;			/* disk_alloc use d->bpt */
  d->bpt = max_bpt;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  DISK_SET_TRACK_IDX( d, 0 );
  buffer->idx = 256;				/* rewind to first track */
  for( i = 0; i < d->sides*d->cylinders; i++ ) {
    int ii;					/* track number from Track Info */
    int idx_save;

    hdrb = buff;
    idx_save = buffer->idx;
    buffer->idx += 256;		/* skip to data */

    if( buffavail( buffer ) < 256 ) break;	/* End of image */

    gap = (unsigned char)hdrb[0x16] == 0xff ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM;

    ii = hdrb[0x10] * d->sides + hdrb[0x11];		/* adjust track No. */
    if( ii > i ) i = ii;
    DISK_SET_TRACK_IDX( d, i );
    d->i = 0;
    if( preindex)
      preindex_add( d, gap );
    postindex_add( d, gap );

    for( j = 0; j < hdrb[0x15]; j++ ) {			/* each sector */
      seclen = d->type == LIBSPECTRUM_DISK_ECPC ? hdrb[ 0x1e + 8 * j ] +	/* data length in sector */
                              256 * hdrb[ 0x1f + 8 * j ]
                            : 0x80 << hdrb[ 0x1b + 8 * j ];
      idlen = 0x80 << hdrb[ 0x1b + 8 * j ];		/* sector length from ID */

      if( idlen == 0 || idlen > ( 0x80 << 0x08 ) )      /* error in sector length code -> ignore */
        idlen = seclen;

      if( i < 84 && fix[i] == 2 && j == 0 ) {	/* repositionate the dummy track  */
        d->i = 8;
      }
      id_add( d, hdrb[ 0x19 + 8 * j ], hdrb[ 0x18 + 8 * j ],
              hdrb[ 0x1a + 8 * j ], hdrb[ 0x1b + 8 * j ], gap,
              hdrb[ 0x1c + 8 * j ] & 0x20 && !( hdrb[ 0x1d + 8 * j ] & 0x20 ) ?
              CRC_ERROR : CRC_OK );

      if( i < 84 && fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_1 && j == 0 ) {	/* 6144 */
        data_add( d, buffer, NULL, seclen,
                  hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                  hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                  CRC_ERROR : CRC_OK, 0x00, NULL );
      } else if( i < 84 && fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_2 && j == 0 ) {	/* 6144, 10x512 */
        datamark_add( d, hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap );
        gap_add( d, 2, gap );
        buffer->idx += seclen;
      } else if( i < 84 && ( fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_3 ||
                             fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_3a ) ) {	/* 128, 256, 512, ... 4096k */
        data_add( d, buffer, NULL, 128,
                  hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                  hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                  CRC_ERROR : CRC_OK, 0x00, NULL );
        buffer->idx += seclen - 128;
      } else if( i < 84 && fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_4 ) {	/* Nx8192 (max 6384 byte ) */
        data_add( d, buffer, NULL, 6384,
                  hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                  hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                  CRC_ERROR : CRC_OK, 0x00, NULL );
        buffer->idx += seclen - 6384;
      } else if( i < 84 && fix[i] == LIBSPECTRUM_DISK_OFLAG_CPC_5 ) {	/* 9x512 */
      /* 512 256 512 256 512 256 512 256 512 */
        if( idlen == 256 ) {
          data_add( d, NULL, buff, 512,
                    hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                    hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                    CRC_ERROR : CRC_OK, 0x00, NULL );
          buffer->idx += idlen;
        } else {
          data_add( d, buffer, NULL, idlen,
                    hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                    hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                    CRC_ERROR : CRC_OK, 0x00, NULL );
        }
      } else {
        data_add( d, buffer, NULL, seclen > idlen ? idlen : seclen,
                  hdrb[ 0x1d + 8 * j ] & 0x40 ? DDAM : NO_DDAM, gap,
                  hdrb[ 0x1c + 8 * j ] & 0x20 && hdrb[ 0x1d + 8 * j ] & 0x20 ?
                  CRC_ERROR : CRC_OK, 0x00, &idx );
        if( seclen > idlen ) {		/* weak sector with multiple copy  */
          cpc_set_weak_range( d, idx, buffer, seclen / idlen, idlen );
          buffer->idx += ( seclen / idlen - 1 ) * idlen;
                                        /* ( ( N * len ) / len - 1 ) * len */
        }
      }
    }
    gap4_add( d, gap );
/* extended DSK image uses track size table */
    buffer->idx = idx_save + ( trlen < 0 ? 256 * buffer->data[0x34 + i] : trlen );
  }
  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
open_scl( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, s, sectors, seclen;
  int scl_deleted, scl_files, scl_i;
  libspectrum_byte head[256];

  d->sides = 2;
  d->cylinders = 80;
  d->density = LIBSPECTRUM_DISK_DD;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

/*
 TR-DOS:
- Root directory is 8 sectors long starting from track 0, sector 1
- Max root entries are 128
- Root entry dimension is 16 bytes (16*128 = 2048 => 8 sector
- Logical sector(start from 0) 8th (9th physical) holds disc info
- Logical sectors from 9 to 15 are unused
- Files are *NOT* fragmented, and start on track 1, sector 1...

*/

  if( ( scl_files = buff[8] ) > 128 || scl_files < 1 ) 	/* number of files */
    return d->status = LIBSPECTRUM_DISK_GEOM;	/* too many file */
  buffer->idx = 9;		/* read SCL entries */

  DISK_SET_TRACK_IDX( d, 0 );
  d->i = 0;
  postindex_add( d, GAP_TRDOS );
  scl_i = d->i;			/* the position of first sector */
  s = 1;			/* start sector number */
  scl_deleted = 0;		/* deleted files */
  sectors = 0;			/* allocated sectors */
  /* we use 'head[]' to build TR-DOS directory */
  j = 0;			/* index for head[] */
  memset( head, 0, 256 );
  seclen = calc_sectorlen( 1, 256, GAP_TRDOS );	/* one sector raw length */
  for( i = 0; i < scl_files; i++ ) {	/* read all entry and build TR-DOS dir */
    if( buffread( head + j, 14, buffer ) != 1 )
      return d->status = LIBSPECTRUM_DISK_OPEN;
    head[ j + 14 ] = sectors % 16; /* ( sectors + 16 ) % 16 := sectors % 16
                                      starting sector */
    head[ j + 15 ] = sectors / 16 + 1; /* ( sectors + 16 ) / 16 := sectors / 16 + 1
                                          starting track */
    sectors += head[ j + 13 ];
    if( d->data[j] == 0x01 )		/* deleted file */
      scl_deleted++;
    if( sectors > 16 * 159 ) 	/* too many sectors needed */
      return d->status = LIBSPECTRUM_DISK_MEM;	/* or LIBSPECTRUM_DISK_GEOM??? */

    j += 16;
    if( j == 256 ) {			/* one sector ready */
      d->i = scl_i + ( ( s - 1 ) % 8 * 2 + ( s - 1 ) / 8 ) * seclen;	/* 1 9 2 10 3 ... */
      id_add( d, 0, 0, s, SECLEN_256, GAP_TRDOS, CRC_OK );
      data_add( d, NULL, head, 256, NO_DDAM, GAP_TRDOS, CRC_OK, NO_AUTOFILL, NULL );
      memset( head, 0, 256 );
      s++;
      j = 0;
    }
  }

  if( j != 0 ) {	/* we have to add this sector  */
    d->i = scl_i + ( ( s - 1 ) % 8 * 2 + ( s - 1 ) / 8 ) * seclen;	/* 1 9 2 10 3 ... */
    id_add( d, 0, 0, s, SECLEN_256, GAP_TRDOS, CRC_OK );
    data_add( d, NULL, head, 256, NO_DDAM, GAP_TRDOS, CRC_OK, NO_AUTOFILL, NULL );
    s++;
  }
  /* and add empty sectors up to No. 16 */
  memset( head, 0, 256 );
  for( ; s <= 16; s++ ) {			/* finish the first track */
    d->i = scl_i + ( ( s - 1 ) % 8 * 2 + ( s - 1 ) / 8 ) * seclen;	/* 1 9 2 10 3 ... */
    id_add( d, 0, 0, s, SECLEN_256, GAP_TRDOS, CRC_OK );
    if( s == 9 ) {			/* TR-DOS descriptor */
      head[225] = sectors % 16;	/* first free sector */
      head[226] = sectors / 16 + 1;	/* first free track */
      head[227] = 0x16;		/* 80 track 2 side disk */
      head[228] = scl_files;		/* number of files */
      head[229] = ( 2544 - sectors ) % 256;	/* free sectors */
      head[230] = ( 2544 - sectors ) / 256;
      head[231] = 0x10;		/* TR-DOS ID byte */
      memset( head + 234, 32, 9 );
      head[244] = scl_deleted;	/* number of deleted files */
      memcpy( head + 245, "FUSE-SCL", 8 );
    }
    data_add( d, NULL, head, 256, NO_DDAM, GAP_TRDOS, CRC_OK, NO_AUTOFILL, NULL );
    if( s == 9 )
      memset( head, 0, 256 );		/* clear sector data... */
  }
  gap4_add( d, GAP_TRDOS );

  /* now we continue with the data */
  for( i = 1; i < d->sides * d->cylinders; i++ ) {
    if( trackgen( d, buffer, i % 2, i / 2, 1, 16, 256,
                  NO_PREINDEX, GAP_TRDOS, INTERLEAVE_2, 0x00 ) )
      return d->status = LIBSPECTRUM_DISK_OPEN;
  }

/* FIXME: move back to fuse//disk.c
  if( settings_current.auto_load ) {
    position_context_save( d, &context );
    trdos_insert_boot_loader( d );
    position_context_restore( d, &context );
  }
*/
  return d->status = LIBSPECTRUM_DISK_OK;
}

/* preindex
 *
 */
static int
open_td0( buffer_t *buffer, libspectrum_disk_t *d )
{
  int i, j, s, sectors, seclen, bpt, gap, mfm, mfm_old;
  int data_offset, track_offset, sector_offset, preindex;
  unsigned char *uncomp_buff, *hdrb;

  if( buff[0] == 't' )		/* signature "td" -> advanced compression */
    return d->status = LIBSPECTRUM_DISK_IMPL;	/* not implemented */

  preindex = d->flag & LIBSPECTRUM_DISK_FLAG_PREIDX ? 1 : 0;
  uncomp_buff = NULL;			/* we may use this buffer */
  mfm_old = buff[5] & 0x80 ? 0 : 1; /* td0notes say: may older teledisk
                                       indicate the SD on high bit of
                                       data rate */
  d->sides = buff[9];			/* 1 or 2 */
  if( d->sides < 1 || d->sides > 2 )
    return d->status = LIBSPECTRUM_DISK_GEOM;
  /* skip comment block if any */
  data_offset = track_offset = 12 + ( buff[7] & 0x80 ?
                                        10 + buff[14] + 256 * buff[15] : 0 );

  /* determine the greatest track length */
  d->bpt = 0;
  d->cylinders = 0;
  seclen = 0;
  while( 1 ) {
    buffer->idx = track_offset;
    if( buffavail( buffer ) < 1 )
      return d->status = LIBSPECTRUM_DISK_OPEN;
    if( ( sectors = buff[0] ) == 255 ) /* sector number 255 => end of tracks */
      break;
    if( buffavail( buffer ) < 4 )	/* check track header is avail. */
      return d->status = LIBSPECTRUM_DISK_OPEN;
    if( buff[1] + 1 > d->cylinders )	/* find the biggest cylinder number */
      d->cylinders = buff[1] + 1;
    sector_offset = track_offset + 4;
    mfm = buff[2] & 0x80 ? 0 : 1;	/* 0x80 == 1 => SD track */
    bpt = postindex_len( mfm_old || mfm ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM ) +
          ( preindex ?
            preindex_len( mfm_old || mfm ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM ) :
            0 ) +
          mfm_old || mfm ? 6 : 3;
    for( s = 0; s < sectors; s++ ) {
      buffer->idx = sector_offset;
      if( buffavail( buffer ) < 6 )		/* check sector header is avail. */
        return d->status = LIBSPECTRUM_DISK_OPEN;
      if( !( buff[4] & 0x30 ) ) {		/* only if we have data */
        if( buffavail( buffer ) < 9 )		/* check data header is avail. */
          return d->status = LIBSPECTRUM_DISK_OPEN;

        bpt += calc_sectorlen( mfm_old || mfm, 0x80 << buff[3],
                               mfm_old || mfm ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM );
        if( buff[3] > seclen )
          seclen = buff[3];			/* biggest sector */
        sector_offset += buff[6] + 256 * buff[7] - 1;
      }
      sector_offset += 9;
    }
    if( bpt > d->bpt )
      d->bpt = bpt;
    track_offset = sector_offset;
  }

  if( d->bpt == 0 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  d->density = LIBSPECTRUM_DISK_DENS_AUTO;
  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  DISK_SET_TRACK_IDX( d, 0 );

  buffer->idx = data_offset;		/* first track header */
  while( 1 ) {
    if( ( sectors = buff[0] ) == 255 ) /* sector number 255 => end of tracks */
      break;

    DISK_SET_TRACK( d, ( buff[2] & 0x01 ), buff[1] );
    d->i = 0;
    /* later teledisk -> if buff[2] & 0x80 -> FM track */
    gap = mfm_old || buff[2] & 0x80 ? GAP_MINIMAL_FM : GAP_MINIMAL_MFM;
    postindex_add( d, gap );

    buffer->idx += 4;		/* sector header*/
    for( s = 0; s < sectors; s++ ) {
      hdrb = buff;
      buffer->idx += 9;		/* skip to data */
      if( !( hdrb[4] & 0x40 ) )		/* if we have id we add */
        id_add( d, hdrb[1], hdrb[0], hdrb[2], hdrb[3], gap,
                hdrb[4] & 0x02 ? CRC_ERROR : CRC_OK );
      if( hdrb[4] & 0x40 ) {		/* if we have _no_ id we drop data... */
        buffer->idx += hdrb[6] + 256 * hdrb[7] - 1;
        continue;		/* next sector */
      }
      if( !( hdrb[4] & 0x30 ) ) {		/* only if we have data */
        seclen = 0x80 << hdrb[3];

        switch( hdrb[8] ) {
        case 0:				/* raw sector data */
          if( hdrb[6] + 256 * hdrb[7] - 1 != seclen ) {
            if( uncomp_buff )
              libspectrum_free( uncomp_buff );
            return d->status = LIBSPECTRUM_DISK_OPEN;
          }
          if( data_add( d, buffer, NULL, hdrb[6] + 256 * hdrb[7] - 1,
                        hdrb[4] & 0x04 ? DDAM : NO_DDAM, gap, CRC_OK,
                        NO_AUTOFILL, NULL ) ) {
            if( uncomp_buff )
              libspectrum_free( uncomp_buff );
            return d->status = LIBSPECTRUM_DISK_OPEN;
          }
          break;
        case 1:				/* Repeated 2-byte pattern */
          if( uncomp_buff == NULL && alloc_uncompress_buffer( &uncomp_buff, 8192 ) )
            return d->status = LIBSPECTRUM_DISK_MEM;
          for( i = 0; i < seclen; ) {			/* fill buffer */
            if( buffavail( buffer ) < 13 ) { 		/* check block header is avail. */
              libspectrum_free( uncomp_buff );
              return d->status = LIBSPECTRUM_DISK_OPEN;
            }
            if( i + 2 * ( hdrb[9] + 256*hdrb[10] ) > seclen ) {
                                                  /* too many data bytes */
              libspectrum_free( uncomp_buff );
              return d->status = LIBSPECTRUM_DISK_OPEN;
            }
            /* ab ab ab ab ab ab ab ab ab ab ab ... */
            for( j = 1; j < hdrb[9] + 256 * hdrb[10]; j++ )
              memcpy( uncomp_buff + i + j * 2, &hdrb[11], 2 );
            i += 2 * ( hdrb[9] + 256 * hdrb[10] );
          }
          if( data_add( d, NULL, uncomp_buff, hdrb[6] + 256 * hdrb[7] - 1,
                        hdrb[4] & 0x04 ? DDAM : NO_DDAM, gap, CRC_OK,
                        NO_AUTOFILL, NULL ) ) {
            libspectrum_free( uncomp_buff );
            return d->status = LIBSPECTRUM_DISK_OPEN;
          }
          break;
        case 2:				/* Run Length Encoded data */
          if( uncomp_buff == NULL && alloc_uncompress_buffer( &uncomp_buff, 8192 ) )
            return d->status = LIBSPECTRUM_DISK_MEM;
          for( i = 0; i < seclen; ) {			/* fill buffer */
            if( buffavail( buffer ) < 11 ) {		/* check block header is avail */
              libspectrum_free( uncomp_buff );
              return d->status = LIBSPECTRUM_DISK_OPEN;
            }
            if( hdrb[9] == 0 ) {		/* raw bytes */
              if( i + hdrb[10] > seclen ||	/* too many data bytes */
                      buffread( uncomp_buff + i, hdrb[10], buffer ) != 1 ) {
                libspectrum_free( uncomp_buff );
                return d->status = LIBSPECTRUM_DISK_OPEN;
              }
              i += hdrb[10];
            } else {				/* repeated samples */
              if( i + 2 * hdrb[9] * hdrb[10] > seclen || /* too many data bytes */
                  buffread( uncomp_buff + i, 2 * hdrb[9], buffer ) != 1 ) {
                libspectrum_free( uncomp_buff );
                return d->status = LIBSPECTRUM_DISK_OPEN;
              }
              /*
                 abcdefgh abcdefg abcdefg abcdefg ...
                 \--v---/
                  2*hdrb[9]
                 |        |       |       |           |
                 +- 0     +- 1    +- 2    +- 3    ... +- hdrb[10]-1
              */
              for( j = 1; j < hdrb[10]; j++ ) /* repeat 'n' times */
                memcpy( uncomp_buff + i + j * 2 * hdrb[9], uncomp_buff + i,
                        2 * hdrb[9] );
              i += 2 * hdrb[9] * hdrb[10];
            }
          }
          if( data_add( d, NULL, uncomp_buff, hdrb[6] + 256 * hdrb[7] - 1,
                        hdrb[4] & 0x04 ? DDAM : NO_DDAM, gap, CRC_OK,
                        NO_AUTOFILL, NULL ) ) {
            libspectrum_free( uncomp_buff );
            return d->status = LIBSPECTRUM_DISK_OPEN;
          }
          break;
        default:
          if( uncomp_buff )
            libspectrum_free( uncomp_buff );
          return d->status = LIBSPECTRUM_DISK_OPEN;
          break;
        }
      }
    }
    gap4_add( d, gap );
  }

  if( uncomp_buff )
    libspectrum_free( uncomp_buff );
  return d->status = LIBSPECTRUM_DISK_OK;
}

/* update tracks TLEN */
void
disk_update_tlens( libspectrum_disk_t *d )
{
  int i;

  for( i = 0; i < d->sides * d->cylinders; i++ ) {	/* check tracks */
    DISK_SET_TRACK_IDX( d, i );
    if( d->track[-3] + 256 * d->track[-2] == 0 ) {
      d->track[-3] = d->bpt & 0xff;
      d->track[-2] = ( d->bpt >> 8 ) & 0xff;
    }
  }
}

/* open a disk image file, read and convert to our format
 * input:
 *   d->filename | d->type ->
 *   d->wrprot -> file write protect
 *   d->flag   -> flags
 *   buffer    -> filled with data
 *   length    -> data length
 */
libspectrum_disk_error_t
libspectrum_disk_open( libspectrum_disk_t *d, libspectrum_byte *buffer,
                       size_t length )
{
  buffer_t b;
  libspectrum_id_t type;
  int error;

  if( !d || !buffer ) return LIBSPECTRUM_DISK_BADPARAM;

  b.idx = 0;
  b.data = buffer;
  b.len = length;

  /* TODO: open_log() */
  if( ( d->type == LIBSPECTRUM_DISK_TYPE_NONE && d->filename == NULL ) ||
        d->type >= LIBSPECTRUM_DISK_LOG )
    return d->status = LIBSPECTRUM_DISK_UNKNOWN;

  if( d->type == LIBSPECTRUM_DISK_TYPE_NONE ) {
    error = libspectrum_identify_file_raw( &type, d->filename, b.data, b.len );
    if( error ) return d->status = LIBSPECTRUM_DISK_UNKNOWN;
    d->type = LIBSPECTRUM_DISK_TYPE_NONE;

    switch( type ) {
    case LIBSPECTRUM_ID_DISK_UDI:
      d->type = LIBSPECTRUM_DISK_UDI; break;
    case LIBSPECTRUM_ID_DISK_OPD:
      d->type = LIBSPECTRUM_DISK_OPD; break;
    case LIBSPECTRUM_ID_DISK_MGT:
      d->type = LIBSPECTRUM_DISK_MGT; break;
    case LIBSPECTRUM_ID_DISK_IMG:
      d->type = LIBSPECTRUM_DISK_IMG; break;
    case LIBSPECTRUM_ID_DISK_SAD:
      d->type = LIBSPECTRUM_DISK_SAD; break;
    case LIBSPECTRUM_ID_DISK_TRD:
      d->type = LIBSPECTRUM_DISK_TRD; break;
    case LIBSPECTRUM_ID_DISK_FDI:
      d->type = LIBSPECTRUM_DISK_FDI; break;
    case LIBSPECTRUM_ID_DISK_CPC:
      d->type = LIBSPECTRUM_DISK_CPC; break;
    case LIBSPECTRUM_ID_DISK_ECPC:
      d->type = LIBSPECTRUM_DISK_ECPC; break;
    case LIBSPECTRUM_ID_DISK_SCL:
      d->type = LIBSPECTRUM_DISK_SCL; break;
    case LIBSPECTRUM_ID_DISK_TD0:
      d->type = LIBSPECTRUM_DISK_TD0; break;
    case LIBSPECTRUM_ID_DISK_D80:
      d->type = LIBSPECTRUM_DISK_D80; break;
    default:
      return d->status = LIBSPECTRUM_DISK_UNKNOWN;
    }
  }

  d->open_flag = 0; /* reset open flags */

  switch( d->type ) {
  case LIBSPECTRUM_DISK_UDI:
    open_udi( &b, d );
    break;
  case LIBSPECTRUM_DISK_OPD:
  case LIBSPECTRUM_DISK_MGT:
  case LIBSPECTRUM_DISK_IMG:
    open_img_mgt_opd( &b, d );
    break;
  case LIBSPECTRUM_DISK_SAD:
    open_sad( &b, d );
    break;
  case LIBSPECTRUM_DISK_TRD:
    open_trd( &b, d );
    break;
  case LIBSPECTRUM_DISK_FDI:
    open_fdi( &b, d );
    break;
  case LIBSPECTRUM_DISK_CPC:
  case LIBSPECTRUM_DISK_ECPC:
    open_cpc( &b, d );
    break;
  case LIBSPECTRUM_DISK_SCL:
    open_scl( &b, d );
    break;
  case LIBSPECTRUM_DISK_TD0:
    open_td0( &b, d );
    break;
  case LIBSPECTRUM_DISK_D40:
  case LIBSPECTRUM_DISK_D80:
    open_d40_d80( &b, d );
    break;
  default:
    return d->status = LIBSPECTRUM_DISK_UNKNOWN;
  }

  if( d->status != LIBSPECTRUM_DISK_OK ) {
    if( d->data != NULL ) {
      libspectrum_free( d->data );
      d->data = NULL;
    }
    return d->status;
  }

  d->dirty = 0;
  disk_update_tlens( d );
  update_tracks_mode( d );

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*--------------------- other functions -----------------------*/

/* create a two sided disk (d) from two one sided (d1 and d2) */
libspectrum_disk_error_t
libspectrum_disk_merge_sides( libspectrum_disk_t *d, libspectrum_disk_t *d1,
                              libspectrum_disk_t *d2, int autofill )
{
  int i;
  int clen;

  if( !d || !d1 || !d2 )
    return LIBSPECTRUM_DISK_BADPARAM;

  if( d1->sides != 1 || d2->sides != 1 ||
      d1->bpt != d2->bpt ||
      ( autofill < 0 && d1->cylinders != d2->cylinders ) )
    return LIBSPECTRUM_DISK_GEOM;

  d->wrprot = 0;
  d->dirty = 0;
  d->sides = 2;
  d->type = d1->type;
  d->cylinders = d2->cylinders > d1->cylinders ? d2->cylinders : d1->cylinders;
  d->bpt = d1->bpt;
  d->density = LIBSPECTRUM_DISK_DENS_AUTO;

  if( disk_alloc( d ) != LIBSPECTRUM_DISK_OK )
    return d->status;

  clen = DISK_CLEN( d->bpt );
  d->track = d->data;
  d1->track = d1->data;
  d2->track = d2->data;
  for( i = 0; i < d->cylinders; i++ ) {
    if( i < d1->cylinders )
      memcpy( d->track, d1->track, d->tlen );
    else {
      d->track[0] = d->bpt & 0xff;
      d->track[1] = ( d->bpt >> 8 ) & 0xff;
      d->track[2] = 0x00;
      memset( d->track + 3, autofill & 0xff, d->bpt );		/* fill data */
      memset( d->track + 3 + d->bpt, 0x00, 3 * clen );		/* no clock and other marks */
    }
    d->track += d->tlen;
    d1->track += d1->tlen;
    if( i < d2->cylinders )
      memcpy( d->track, d2->track, d->tlen );
    else {
      d->track[0] = d->bpt & 0xff;
      d->track[1] = ( d->bpt >> 8 ) & 0xff;
      d->track[2] = 0x00;
      memset( d->track + 1, autofill & 0xff, d->bpt );		/* fill data */
      memset( d->track + 1 + d->bpt, 0x00, 3 * clen );		/* no clock and other marks */
    }
    d->track += d->tlen;
    d2->track += d2->tlen;
  }
  libspectrum_disk_close( d1 );
  libspectrum_disk_close( d2 );
  return d->status = LIBSPECTRUM_DISK_OK;
}

/*--------------------- start of write section ----------------*/

static int
write_udi( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, error;
  size_t len;
  libspectrum_dword crc;
  libspectrum_byte head[256];

  udi_pack_tracks( d );
#ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
  if( !( d->flag & LIBSPECTRUM_DISK_FLAG_SAVE_UNCOMPR ) )
    udi_compress_tracks( d );
#endif			/* #ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */

  crc = ~( libspectrum_dword ) 0;
  len = 16;

  for( i = 0; i < d->sides * d->cylinders; i++ ) {	/* check tracks */
    DISK_SET_TRACK_IDX( d, i );
    if( d->track[-1] == 0xf0 )
      len += 7 + d->track[-3] + 256 * d->track[-2];
    else
      len += 3 + UDI_TLEN( d->track[-1], d->track[-3] + 256 * d->track[-2] );
  }
  head[0] = 'U';
  head[1] = 'D';
  head[2] = 'I';
  head[3] = '!';
  head[4] = len & 0xff;
  head[5] = ( len >> 8 ) & 0xff;
  head[6] = ( len >> 16 ) & 0xff;
  head[7] = ( len >> 24 ) & 0xff;
  head[8] = 0x00;
  head[9] = d->cylinders - 1;
  head[10] = d->sides - 1;
  head[11] = head[12] = head[13] = head[14] = head[15] = 0;
  buffwrite( head, 16, b );

  for( j = 0; j < 16; j++ )
    crc = crc_udi( crc, head[j] );

  for( i = 0; i < d->sides * d->cylinders; i++ ) {	/* write tracks */
    DISK_SET_TRACK_IDX( d, i );
    head[0] = d->track[-1];		/* track type */
    head[1] = d->track[-3];		/* track len  */
    head[2] = d->track[-2];		/* track len2 */
    buffwrite( head, 3, b );

    for( j = 0; j < 3; j++ )
      crc = crc_udi( crc, head[j] );

    if( d->track[-1] == 0xf0 )
      len = 4 + d->track[-3] + 256 * d->track[-2];
    else
      len = UDI_TLEN( d->track[-1], d->track[-3] + 256 * d->track[-2] );
    buffwrite( d->track, len, b );

    for( j = len; j > 0; j-- ) {
      crc = crc_udi( crc, *d->track );
      d->track++;
    }
  }
  head[0] = crc & 0xff;
  head[1] = ( crc >> 8 ) & 0xff;
  head[2] = ( crc >> 16 ) & 0xff;
  head[3] = ( crc >> 24 ) & 0xff;
  buffwrite( head, 4, b );

#ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION
  /* Keep tracks uncompressed in memory */
  error = udi_uncompress_tracks( d );
  if( error ) return error;
#endif			/* #ifdef LIBSPECTRUM_SUPPORTS_ZLIB_COMPRESSION */

  udi_unpack_tracks( d );
  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
savetrack()
*/
static int
write_img_mgt_opd( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, sbase, sectors, seclen, mfm, cyl;

  if( check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl ) ||
      ( d->type != LIBSPECTRUM_DISK_OPD && ( sbase != 1 || seclen != 2 || sectors != 10 ) ) ||
      ( d->type == LIBSPECTRUM_DISK_OPD && ( sbase != 0 || seclen != 1 || sectors != 18 ) ) )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( cyl == -1 ) cyl = d->cylinders;
  if( cyl != 40 && cyl != 80 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( d->type == LIBSPECTRUM_DISK_IMG ) {	/* out-out */
    for( j = 0; j < d->sides; j++ ) {
      for( i = 0; i < cyl; i++ ) {
        if( savetrack( d, b, j, i, 1, sectors, seclen ) )
          return d->status = LIBSPECTRUM_DISK_GEOM;
      }
    }
  } else {			/* alt */
    for( i = 0; i < cyl; i++ ) {	/* MGT */
      for( j = 0; j < d->sides; j++ ) {
        if( savetrack( d, b, j, i, d->type == LIBSPECTRUM_DISK_MGT ? 1 : 0,
            sectors, seclen ) )
          return d->status = LIBSPECTRUM_DISK_GEOM;
      }
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
savetrack()
*/
static int
write_d40_d80( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, sbase, sectors, seclen, mfm, cyl;

  if( check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl ) ||
      ( sbase != 1 ) )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( cyl == -1 ) cyl = d->cylinders;

  if( ( d->type == LIBSPECTRUM_DISK_D40 && cyl > 43 ) ||
      ( d->type == LIBSPECTRUM_DISK_D80 && cyl > 83 ) )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  for( i = 0; i < cyl; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      if( savetrack( d, b, j, i, 1, sectors, seclen ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
savetrack()
*/
static int
write_trd( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, sbase, sectors, seclen, mfm, cyl;

  if( check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl ) ||
      sbase != 1 || seclen != 1 || sectors != 16 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( cyl == -1 ) cyl = d->cylinders;

  for( i = 0; i < cyl; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      if( savetrack( d, b, j, i, 1, sectors, seclen ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
savetrack()
*/
static int
write_sad( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, sbase, sectors, seclen, mfm, cyl;
  libspectrum_byte head[256];

  if( check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl ) ||
      sbase != 1 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( cyl == -1 ) cyl = d->cylinders;
  memcpy( head, "Aley's disk backup", 18 );
  head[18] = d->sides;
  head[19] = cyl;
  head[20] = sectors;
  head[21] = seclen * 4;
  buffwrite( head, 22, b );

  for( j = 0; j < d->sides; j++ ) {	/* OUT-OUT */
    for( i = 0; i < cyl; i++ ) {
      if( savetrack( d, b, j, i, 1, sectors, seclen ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
saverawtrack()
*/
static int
write_fdi( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, k, sbase, sectors, seclen, mfm, del;
  int h, t, s, l;
  int toff, soff;
  libspectrum_byte head[256];

  memset( head, 0, 14 );
  memcpy( head, "FDI", 3 );
  head[0x03] = d->wrprot = 1;
  head[0x04] = d->cylinders & 0xff;
  head[0x05] = d->cylinders >> 8;
  head[0x06] = d->sides & 0xff;
  head[0x07] = d->sides >> 8;
  sectors = 0;
  for( j = 0; j < d->cylinders; j++ ) {	/* count sectors */
    for( i = 0; i < d->sides; i++ ) {
      guess_track_geom( d, i, j, &sbase, &s, &seclen, &mfm );
      sectors += s;
    }
  }
  h = ( sectors + d->cylinders * d->sides ) * 7;	/* track header len */
  head[0x08] = ( h + 0x0e ) & 0xff;	/* description offset */
  head[0x09] = ( h + 0x0e ) >> 8; /* "http://fuse-emulator.sourceforge.net" */
  head[0x0a] = ( h + 0x33 ) & 0xff;	/* data offset */
  head[0x0b] = ( h + 0x33 ) >> 8;
  buffwrite( head, 14, b );

  /* write track headers */
  toff = 0;			/* offset of track data */
  for( i = 0; i < d->cylinders; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      DISK_SET_TRACK( d, j, i );
      d->i = 0;
      head[0x00] = toff & 0xff;
      head[0x01] = ( toff >> 8 ) & 0xff;	/* track offset */
      head[0x02] = ( toff >> 16 ) & 0xff;
      head[0x03] = ( toff >> 24 ) & 0xff;
      head[0x04] = 0;
      head[0x05] = 0;
      guess_track_geom( d, j, i, &sbase, &sectors, &seclen, &mfm );
      head[0x06] = sectors;
      buffwrite( head, 7, b ); /* track header */

      DISK_SET_TRACK( d, j, i );
      d->i = 0;
      k = 0;
      soff = 0;
      while( sectors > 0 ) {
        while( k < 35 && id_read( d, &h, &t, &s, &l ) ) {
          head[ 0x00 + k * 7 ] = t;
          head[ 0x01 + k * 7 ] = h;
          head[ 0x02 + k * 7 ] = s;
          head[ 0x03 + k * 7 ] = l;
          head[ 0x05 + k * 7 ] = soff & 0xff;
          head[ 0x06 + k * 7 ] = ( soff >> 8 ) & 0xff;
          if( !datamark_read( d, &del, NULL ) ) {
            head[ 0x04 + k * 7 ] = 0;	/* corrupt sector data */
          } else {
            head[ 0x04 + k * 7 ] = ( 1 << l ) | ( del ? 0x80 : 0x00 );
            soff += 0x80 << l;
          }
          k++;
        }			/* Sector header */
        buffwrite( head, 7 * k, b );

        sectors -= k;
        k = 0;
      }
      toff += soff;
    }
  }
  buffwrite( "http://fuse-emulator.sourceforge.net", 37, b );

  /* write data */
  for( i = 0; i < d->cylinders; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      if( saverawtrack( d, b, j, i ) )
        return d->status = LIBSPECTRUM_DISK_WRPART;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
saverawtrack()
*/
static int
write_cpc( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, k, sbase, sectors, seclen, mfm, cyl;
  int h, t, s, l;
  size_t len;
  libspectrum_byte head[256];

  i = check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl );
  if( i & LIBSPECTRUM_DISK_SECLEN_VARI ||
      i & LIBSPECTRUM_DISK_SPT_VARI ||
      i & LIBSPECTRUM_DISK_WEAK_DATA )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  if( i & LIBSPECTRUM_DISK_MFM_VARI )
    mfm = -1;
  if( cyl == -1 ) cyl = d->cylinders;

  memset( head, 0, 256 );
  memcpy( head, "MV - CPCEMU Disk-File\r\nDisk-Info\r\n", 34 );
  head[0x30] = cyl;
  head[0x31] = d->sides;
  len = sectors * ( 0x80 << seclen ) + 256;
  head[0x32] = len & 0xff;
  head[0x33] = len >> 8;
  buffwrite( head, 256, b );

  memset( head, 0, 256 );
  memcpy( head, "Track-Info\r\n", 12 );
  for( i = 0; i < cyl; i++ ) {
    for( j = 0; j < d->sides; j++ ) {
      DISK_SET_TRACK( d, j, i );
      d->i = 0;
      head[0x10] = i;
      head[0x11] = j;
      head[0x14] = seclen;
      head[0x15] = sectors;
      if( mfm != -1 )
        head[0x16] = mfm ? 0x4e : 0xff;
      head[0x17] = 0xe5;
      k = 0;
      while( id_read( d, &h, &t, &s, &l ) ) {
        head[ 0x18 + k * 8 ] = t;
        head[ 0x19 + k * 8 ] = h;
        head[ 0x1a + k * 8 ] = s;
        head[ 0x1b + k * 8 ] = l;
        if( k == 0 && mfm == -1 ) {	/* if mixed MFM/FM tracks */
          head[0x16] = d->track[ d->i ] == 0x4e ? 0x4e : 0xff;
        }
        k++;
      }
      buffwrite( head, 256, b );  /* track header */

      if( saverawtrack( d, b, j, i ) )
        return d->status = LIBSPECTRUM_DISK_WRPART;
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

static int
write_scl( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, k, l, t, s, sbase, sectors, seclen, mfm, del, cyl;
  int entries;
  libspectrum_dword sum = 597;		/* sum of "SINCLAIR" */
  libspectrum_byte head[256];

  if( check_disk_geom( d, &sbase, &sectors, &seclen, &mfm, &cyl ) ||
      sbase != 1 || seclen != 1 || sectors != 16 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  DISK_SET_TRACK_IDX( d, 0 );

  /* TR-DOS descriptor */
  if( !id_seek( d, 9 ) || !datamark_read( d, &del, NULL ) )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  entries = head[8] = d->track[ d->i + 228 ];	/* number of files */

  if( entries > 128 || d->track[ d->i + 231 ] != 0x10 ||
      ( d->track[ d->i + 227 ] != 0x16 && d->track[ d->i + 227 ] != 0x17 &&
        d->track[ d->i + 227 ] != 0x18 && d->track[ d->i + 227 ] != 0x19 ) ||
      d->track[ d->i ] != 0 )
    return d->status = LIBSPECTRUM_DISK_GEOM;

  memcpy( head, "SINCLAIR", 8 );
  sum += entries;
  buffwrite( head, 9, b );

  /* save SCL entries */
  j = 1;			/* sector number */
  k = 0;
  sectors = 0;
  for( i = 0; i < entries; i++ ) {	/* read TR-DOS dir */
    if( j > 8 )		/* TR-DOS dir max 8 sector len */
      return d->status = LIBSPECTRUM_DISK_GEOM;

    if( k == 0 && ( !id_seek( d, j ) || !datamark_read( d, &del, NULL ) ) )
      return d->status = LIBSPECTRUM_DISK_GEOM;

    buffwrite( d->track + d->i + k, 14, b );
    sectors += d->track[ d->i + k + 13 ];	/* file length in sectors */
    for( s = 0; s < 14; s++ ) {
      sum += d->track[ d->i + k ];
      k++;
    }
    k += 2;
    if( k >= 256 ) {		/* end of sector */
      j++;
      k = 0;
    }
  }

  /* save data */
  /* we have to 'defragment' the disk :) */
  j = 1;			/* sector number */
  k = 0;			/* byte offset */
  for( i = 0; i < entries; i++ ) {	/* read TR-DOS dir */
    DISK_SET_TRACK_IDX( d, 0 );
    if( k == 0 ) {
      if ( !id_seek( d, j ) || !datamark_read( d, &del, NULL ) )
        return d->status = LIBSPECTRUM_DISK_GEOM;
      memcpy( head, d->track + d->i, 256 );
    }

    s = head[ k + 14 ];	/* starting sector */
    t = head[ k + 15 ];	/* starting track */
    sectors = head[ k + 13 ] + s;	/* last sector */
    k += 16;
    if( k == 256 ) {
      k = 0;
      j++;
    }
    if( t >= d->sides * d->cylinders )
      return d->status = LIBSPECTRUM_DISK_GEOM;

    if( s % 16 == 0 )
      t--;
    DISK_SET_TRACK_IDX( d, t );

    for( ; s < sectors; s++ ) {		/* save all sectors */
      if( s % 16 == 0 ) {
        t++;
        if( t >= d->sides * d->cylinders )
          return d->status = LIBSPECTRUM_DISK_GEOM;
        DISK_SET_TRACK_IDX( d, t );
      }
      if( id_seek( d, s % 16 + 1 ) ) {
        if( datamark_read( d, &del, NULL ) ) {	/* write data if we have data */
          if( data_write_file( d, b, 1 ) ) {
            return d->status = LIBSPECTRUM_DISK_GEOM;
          } else {
            for( l = 0; l < 256; l++ )
              sum += d->track[ d->i + l ];
          }
        }
      } else {
        return LIBSPECTRUM_DISK_GEOM;
      }
    }
  }
  head[0] = sum & 0xff;
  head[1] = ( sum >> 8 ) & 0xff;
  head[2] = ( sum >> 16 ) & 0xff;
  head[3] = ( sum >> 24 ) & 0xff;

  buffwrite( head, 4, b );

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
  A log file is a little bigger than a disk image ~7M, but nowadays it is not
  a problem
*/
static int
write_log( buffer_t *b, libspectrum_disk_t *d )
{
  int i, j, k, del, fmf, rev;
  int h, t, s, l;
  char str[17];
  char head[257];

  str[16] = '\0';
  buffprintf( b, "DISK tracks log!\n" );
  buffprintf( b, "Sides: %d, cylinders: %d\n", d->sides, d->cylinders );

  for( j = 0; j < d->cylinders; j++ ) {	/* ALT :) */
    for( i = 0; i < d->sides; i++ ) {
      DISK_SET_TRACK( d, i, j );
      d->i = 0;
      buffprintf( b, "\n*********\nSide: %d, cylinder: %d type: 0x%02x tlen: %5u\n",
                  i, j, d->track[-1], d->track[-3] + 256 * d->track[-2] );
      while( id_read( d, &h, &t, &s, &l ) ) {
        buffprintf( b, "  h:%d t:%d s:%d l:%d(%d)", h, t, s, l, 0x80 << l );
        if( datamark_read( d, &del, &fmf ) ) {
          buffprintf( b, " %s(%sFM)\n", del ? "deleted" : "normal",
                      fmf ? "M" : "" );
        } else {
          buffprintf( b, " noDAM\n" );
        }
      }
    }
  }

  buffprintf( b, "\n***************************\nSector Data Dump:\n" );

  for( j = 0; j < d->cylinders; j++ ) {	/* ALT :) */
    for( i = 0; i < d->sides; i++ ) {
      DISK_SET_TRACK( d, i, j );
      d->i = 0;
      buffprintf( b, "\n*********\nSide: %d, cylinder: %d type: 0x%02x tlen: %5u\n",
                  i, j, d->track[-1], d->track[-3] + 256 * d->track[-2] );
      rev = k = 0;
      while( id_read( d, &h, &t, &s, &l ) ) {
        l = 0x80 << l;
        if( l > 16384 ) l = 16384; /* max HD track len is 12650 byte */
        if( datamark_read( d, &del, &fmf ) ) {
          buffprintf( b, "  h:%d t:%d s:%d l:%d (%s/%sFM)\n", h, t, s, l,
                   del ? "deleted" : "normal", fmf ? "M" : "" );
        } else {
          buffprintf( b, "  h:%d t:%d s:%d l:%d (missing data)\n", h,
                   t, s, l );
        }

        k = 0;
        while( k < l ) {
          if( !( k % 16 ) ) {
            buffprintf( b, "0x%08x:", k );
          }
          buffprintf( b, " 0x%02x", d->track[ d->i ] );
          str[ k & 0x0f ] = d->track[ d->i ] >= 32 &&
                            d->track[ d->i ] < 127 ? d->track[ d->i ] : '.';
          k++;
          if( !( k % 16 ) ) {
            buffprintf( b, " | %s\n", str );
          }
          d->i++;
          if( d->i >= d->bpt ) {
            d->i = 0;
            rev++;
            if( rev == 6 )
              break;
          }
        }
      }
    }
  }

  buffprintf( b, "\n***************************\n**Full Dump:\n" );

  for( j = 0; j < d->cylinders; j++ ) {	/* ALT :) */
    for( i = 0; i < d->sides; i++ ) {
      DISK_SET_TRACK( d, i, j );
      d->i = 0;
      buffprintf( b, "\n*********\nSide: %d, cylinder: %d type: 0x%02x tlen: %5u\n",
                  i, j, d->track[-1], d->track[-3] + 256 * d->track[-2] );
      k = 0;
      while( d->i < d->bpt ) {
        if( !( k % 8 ) ) {
          buffprintf( b, "0x%08x:", d->i );
        }

        buffprintf( b, " 0x%04x", d->track[ d->i ] |
          ( libspectrum_bitmap_test( d->clocks, d->i ) ? 0x0c00 : 0x0000 ) |
          ( libspectrum_bitmap_test( d->fm, d->i ) ? 0x1000 : 0x0000 ) |
          ( libspectrum_bitmap_test( d->weak, d->i ) ? 0x8000 : 0x0000 ) );
        k++;
        if( !( k % 8 ) ) {
          buffprintf( b, "\n" );
        }
        d->i++;
      }
    }
  }

  return d->status = LIBSPECTRUM_DISK_OK;
}

/*
    User must free *buffer!
*/
libspectrum_disk_error_t
libspectrum_disk_write( libspectrum_disk_t *d1, libspectrum_byte **buffer,
                        size_t *length, const char *filename )
{
  buffer_t b;
  size_t namelen;
  libspectrum_disk_t *d = d1;

  if( !d || !buffer || !length )
    return LIBSPECTRUM_DISK_BADPARAM;

  b.data = libspectrum_malloc( BUFF_ALLOC );
  b.len = BUFF_ALLOC;
  b.idx = 0;

  if( d->type == LIBSPECTRUM_DISK_TYPE_NONE ) {
    const char *ext;

    ext = "";
    if( filename != NULL ) {
      namelen = strlen( filename );
      if( namelen >= 4 )
        ext = filename + namelen - 4;
    }

    if( !strcasecmp( ext, ".udi" ) )
      d->type = LIBSPECTRUM_DISK_UDI;				/* ALT side */
    else if( !strcasecmp( ext, ".dsk" ) )
      d->type = LIBSPECTRUM_DISK_CPC;				/* ALT side */
    else if( !strcasecmp( ext, ".mgt" ) )
      d->type = LIBSPECTRUM_DISK_MGT;				/* ALT side */
    else if( !strcasecmp( ext, ".opd" ) || !strcasecmp( ext, ".opu" ) )
      d->type = LIBSPECTRUM_DISK_OPD;				/* ALT side */
    else if( !strcasecmp( ext, ".img" ) )		/* out-out */
      d->type = LIBSPECTRUM_DISK_IMG;
    else if( !strcasecmp( ext, ".trd" ) )		/* ALT */
      d->type = LIBSPECTRUM_DISK_TRD;
    else if( !strcasecmp( ext, ".sad" ) )		/* ALT */
      d->type = LIBSPECTRUM_DISK_SAD;
    else if( !strcasecmp( ext, ".fdi" ) )		/* ALT */
      d->type = LIBSPECTRUM_DISK_FDI;
    else if( !strcasecmp( ext, ".d40" ) )		/* ALT side */
      d->type = LIBSPECTRUM_DISK_D40;
    else if( !strcasecmp( ext, ".d80" ) )		/* ALT side */
      d->type = LIBSPECTRUM_DISK_D80;
    else if( !strcasecmp( ext, ".scl" ) )		/* not really a disk image */
      d->type = LIBSPECTRUM_DISK_SCL;
    else if( !strcasecmp( ext, ".td0" ) )		/* not supported */
      d->type = LIBSPECTRUM_DISK_TD0;
    else if( !strcasecmp( ext, ".log" ) )		/* ALT */
      d->type = LIBSPECTRUM_DISK_LOG;
    else
      d->type = LIBSPECTRUM_DISK_UDI;			/* ALT side */
  }

/* we need it for save */
  update_tracks_mode( d );
  switch( d->type ) {
  case LIBSPECTRUM_DISK_UDI:
    write_udi( &b, d );
    break;
  case LIBSPECTRUM_DISK_IMG:
  case LIBSPECTRUM_DISK_MGT:
  case LIBSPECTRUM_DISK_OPD:
    write_img_mgt_opd( &b, d );
    break;
  case LIBSPECTRUM_DISK_D40:
  case LIBSPECTRUM_DISK_D80:
    write_d40_d80( &b, d );
    break;
  case LIBSPECTRUM_DISK_TRD:
    write_trd( &b, d );
    break;
  case LIBSPECTRUM_DISK_SAD:
    write_sad( &b, d );
    break;
  case LIBSPECTRUM_DISK_FDI:
    write_fdi( &b, d );
    break;
  case LIBSPECTRUM_DISK_SCL:
    write_scl( &b, d );
    break;
  case LIBSPECTRUM_DISK_CPC:
    write_cpc( &b, d );
    break;
  case LIBSPECTRUM_DISK_LOG:
    write_log( &b, d );
    break;
  default:
    d->status = LIBSPECTRUM_DISK_WRFILE;
    break;
  }

  if( d->status != LIBSPECTRUM_DISK_OK ) {
    *buffer = NULL;
    *length = 0;
    return d->status;
  }

  *buffer = b.data;
  *length = b.idx;

  return d->status = LIBSPECTRUM_DISK_OK;
}

const libspectrum_disk_desc_t *
libspectrum_disk_type_description( libspectrum_disk_type_t dt )
{
  if( dt <= 0 || dt >= LIBSPECTRUM_DISK_TYPE_LAST )
    return NULL;

  return &disk_desc[ dt ];
}
