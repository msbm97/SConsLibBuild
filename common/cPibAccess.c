/**
* CPibAccess.c
*
* Interface routines to read & write PIB files.
*
*   File Header
*   For each data block:
*     Block Name (24 bytes)
*     Size of block in bytes (int)
*     Size of compression buffer (int)
*     Version (int)
*     Data Block
*    ---------------
*     40 bytes for header + size of datablock
* History:
*   Created:   6/01  K. Jones
**/
#define _INCLUDE_POSIX_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>

#include "cPibAccess.h"

#ifdef WIN32
int __cdecl _fseeki64(FILE *, __int64, int);
__int64 __cdecl _ftelli64(FILE *);
#endif

#define MAXNUMPIBFILES 10

/** size of of the header block **/
uInt pibBlockHdrSize = 40;

/** File handles and pointers for each open file. */
PibFile fData[MAXNUMPIBFILES];

/* The maximum file size supported by the OS. */
off_t maxPibFileSize = 0x7fffffff;

off_t getposPib(xdrs)
   XDR *xdrs;
{
#ifdef WIN32
    return _ftelli64((FILE *)xdrs->x_private);
#else
    return ftello((FILE *)xdrs->x_private);
#endif
}

bool_t
setposPib(xdrs, pos)
   XDR *xdrs;
   off_t pos;
{
#ifdef WIN32
    return ((_fseeki64((FILE *)xdrs->x_private, pos, 0) < 0) ?
      FALSE : TRUE);
#else
    return ((fseeko((FILE *)xdrs->x_private, pos, 0) < 0) ?
      FALSE : TRUE);
#endif
}


int
initFileData(int *fIndex)
{
   if(*fIndex < 0 || *fIndex >= MAXNUMPIBFILES) {
       return -1;
   }
   if(sizeof(off_t) == 8) {
        maxPibFileSize = 0x7fffffffffffffff;
    }
   fData[*fIndex].pibPtr_BlockStart = -1;
    fData[*fIndex].pibPtr_DataStart = -1;
    fData[*fIndex].block_compression = -1;
   return *fIndex;
}

/**
 * Retrieve the current file position.
 *
**/
off_t getXdrPos(XDR* xdrs)
{
   return (off_t)getposPib(xdrs);
}

/**
 * Set the current file position.
 *
**/
bool_t setXdrPos(XDR* xdrs, off_t pos)
{
   return setposPib(xdrs, pos);
}

/**
* Open a PIB file for Reading.
*
* @param file_xdr The OS file name of the file to open.
*
* @param ioflag A pointer to an integer flag,
*               returns 0 on success, 1 on failure.
*
**/
void OpenPibImportFile(sInt *err,
                       sInt *fIndex,
                       char *file_xdr,
                       char *pibFileType,
                       char *version,
                       char *description)
{
   bool_t rc;
   uInt sizep = 80;
   *err = 0;

   /* open input file */
   if(initFileData(fIndex) != *fIndex) {
      printf("*PibOpenImportFile* Can't open file %s for reading, Invalid file number %d\n",file_xdr,*fIndex);
      *err = 1;
      return;
   }
   fData[*fIndex].xPib = fopen(file_xdr, "rb");
   if(fData[*fIndex].xPib == NULL) {
      printf("*PibOpenImportFile* Can't open file %s for reading.\n",file_xdr);
      *err = 1;
      return;
   } else {
      /* associate xdr stream with file */
      fData[*fIndex].xdrPib.x_ops=NULL;
      xdrstdio_create(&(fData[*fIndex].xdrPib), fData[*fIndex].xPib, XDR_DECODE);
      if(fData[*fIndex].xdrPib.x_ops == NULL) {
         printf("*PibOpenImportFile* Can't create xdr stream for decoding\n");
         *err = 1;
         return;
      }
   }

   rc = xdr_string(&(fData[*fIndex].xdrPib), &pibFileType, sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &version,     sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &description, sizep);

   fData[*fIndex].pibPtr_DataStart = getXdrPos(&(fData[*fIndex].xdrPib));

   *err = !(rc);

   return;
}

/**
* Open a PIB file for writing.
*
* @param file_xdr The OS file name of the file to open.
*
* @param ioflag A pointer to an integer flag,
*               returns 0 on success, 1 on failure.
*
**/
void OpenPibExportFile(sInt *err,
                       sInt *fIndex,
                       char *file_xdr,
                       char *pibFileType,
                       char *version,
                       char *description)
{
   bool_t rc;
   uInt sizep = 80;
   *err = 0;

   /* open output file */
   if(initFileData(fIndex) != *fIndex) {
      printf("*PibOpenExportFile* Can't open file %s for writing, Invalid file number %d\n",file_xdr,*fIndex);
      *err = 1;
      return;
   }
   fData[*fIndex].xPib = fopen(file_xdr, "wb");
   if(fData[*fIndex].xPib == NULL) {
      printf("*PibOpenExportFile* Can't open file %s for writing\n",file_xdr);
      *err = 1;
   } else {
      /* associate xdr stream with file */
      fData[*fIndex].xdrPib.x_ops=NULL;
      xdrstdio_create(&(fData[*fIndex].xdrPib), fData[*fIndex].xPib, XDR_ENCODE);
      if(fData[*fIndex].xdrPib.x_ops == NULL) {
         printf("*PibOpenExportFile* Can't create xdr stream for encoding\n");
         *err = 1;
      }
   }

   rc = xdr_string(&(fData[*fIndex].xdrPib), &pibFileType, sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &version,     sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &description, sizep);

   fData[*fIndex].pibPtr_DataStart = getXdrPos(&(fData[*fIndex].xdrPib));

   *err = !(rc);

   return;
}

/**
* Open a PIB file to append data.
*
* @param file_xdr The OS file name of the file to open.
*
* @param ioflag A pointer to an integer flag,
*               returns 0 on success, 1 on failure.
*
**/
void OpenPibAppendFile(sInt *err,
                       sInt *fIndex,
                       char *file_xdr,
                       char *pibFileType,
                       char *version,
                       char *description,
                       off_t *pibPtr_appendPos)
{
   bool_t rc;
   uInt sizep = 80;
   off_t truncPos = *pibPtr_appendPos;

   *err = 0;

   /* open input file */
   if(initFileData(fIndex) != *fIndex) {
      printf("*OpenPibAppendFile* Can't open file %s for appending, Invalid file number %d\n",file_xdr,*fIndex);
      *err = 1;
      return;
   }
   fData[*fIndex].xPib = fopen(file_xdr, "r+b");
   if(fData[*fIndex].xPib == NULL) {
      printf("*OpenPibAppendFile* Can't open file %s for appending.\n",file_xdr);
      *err = 1;
      return;
   } else {
      /* associate xdr stream with file */
      fData[*fIndex].xdrPib.x_ops=NULL;
      xdrstdio_create(&(fData[*fIndex].xdrPib), fData[*fIndex].xPib, XDR_DECODE);
      if(fData[*fIndex].xdrPib.x_ops == NULL) {
         printf("*OpenPibAppendFile* Can't create xdr stream for decoding\n");
         *err = 1;
         return;
      }
   }

   rc = xdr_string(&(fData[*fIndex].xdrPib), &pibFileType, sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &version,     sizep) &&
        xdr_string(&(fData[*fIndex].xdrPib), &description, sizep);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
     return;
   }
   fData[*fIndex].pibPtr_DataStart = getXdrPos(&(fData[*fIndex].xdrPib));

   if(*pibPtr_appendPos > 0 ) {
#ifdef WIN32
       rc = setXdrPos(&(fData[*fIndex].xdrPib), *pibPtr_appendPos);
       if(rc) {
         *err = 0;
       } else {
         *err = 1;
       }
       SetEndOfFile(fData[*fIndex].xPib);
#else
       rc = ftruncate(fileno(fData[*fIndex].xPib),truncPos);
       if(rc) {
         *err = 1;
         return;
       }
#endif
   }
   xdr_destroy(&(fData[*fIndex].xdrPib));
   fclose(fData[*fIndex].xPib);
   fData[*fIndex].xPib = NULL;

   /* open output file in append mode */
   fData[*fIndex].xPib = fopen(file_xdr, "r+b");
   rc = setXdrPos(&(fData[*fIndex].xdrPib), *pibPtr_appendPos);
   if(fData[*fIndex].xPib == NULL) {
      printf("*OpenPibAppendFile* Can't open file %s for append\n",file_xdr);
      *err = 1;
   } else {
      /* associate xdr stream with file */
      fData[*fIndex].xdrPib.x_ops=NULL;
      xdrstdio_create(&(fData[*fIndex].xdrPib), fData[*fIndex].xPib, XDR_ENCODE);
      if(fData[*fIndex].xdrPib.x_ops == NULL) {
         printf("*OpenPibAppendFile* Can't create xdr stream for encoding\n");
         *err = 1;
      }
   }
   return;
}

/**
* Close the xdr file.
* If the file is being written, write an end of file marker.
*
**/
void ClosePibFile(sInt *fIndex)
{
   sInt err;
   sInt version =0;
   if(fData[*fIndex].xPib != NULL) {
      if(fData[*fIndex].xdrPib.x_op == XDR_ENCODE) {
         openPibBlock(&err, fIndex, "EOF", &version);
         closePibBlock(&err, fIndex);
      }
      xdr_destroy(&(fData[*fIndex].xdrPib));
      fclose(fData[*fIndex].xPib);
      fData[*fIndex].xPib = NULL;
   }
}

/**
* Rewind the file to the start of the data blocks.
*
**/
void RewindPibFile(sInt *err, sInt *fIndex)
{
   bool_t rc = 0;
   if(fData[*fIndex].pibPtr_DataStart > 0) {
     rc = setXdrPos(&(fData[*fIndex].xdrPib), fData[*fIndex].pibPtr_DataStart);
   }
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}


/**
* Open a data block, write the block label and a placeholder for the
* block size and compression fields.
*
* @param blocktype A 24 character label used to identify the block,
*                  will be padded with nulls.
*
* @param revision  The revision number of the block for version support.
*
* @return true -  success,
*         false - failure.
*
**/
void openPibBlock(sInt *err, sInt *fIndex, char *blocktype, sInt *revision)
{
   bool_t rc;
   char bt[24];
   uInt sizep = 24;
   sInt blocksize = -1;
   sInt block_compression = -1;
   char *btPtr = bt;

   strncpy(bt,blocktype,24);
   fData[*fIndex].pibPtr_BlockStart = getXdrPos(&(fData[*fIndex].xdrPib));

   rc =  (xdr_bytes(&(fData[*fIndex].xdrPib), &btPtr, &sizep, 24) &&
          xdr_int  (&(fData[*fIndex].xdrPib), &blocksize)              && /* blocksize placeholder */
          xdr_int  (&(fData[*fIndex].xdrPib), &block_compression)      && /* compression placeholder */
          xdr_int  (&(fData[*fIndex].xdrPib), revision));                 /* block version */

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
* Close the current data block, write the block size to the file and then
* reposition the file pointer to the end of the block.
*
* @return true -  success,
*         false - failure.
*
**/
void closePibBlock(sInt *err, sInt *fIndex)
{
   bool_t rc;
   sInt blocksize;
   off_t locBlocksize;
   off_t filePtr_End;

   filePtr_End = getXdrPos(&(fData[*fIndex].xdrPib));
   blocksize = (sInt)(filePtr_End-fData[*fIndex].pibPtr_BlockStart);
   locBlocksize = fData[*fIndex].pibPtr_BlockStart + 28;

   rc =  (setXdrPos(&(fData[*fIndex].xdrPib), locBlocksize )  &&
      xdr_int   (&(fData[*fIndex].xdrPib), &blocksize)         &&
      xdr_int   (&(fData[*fIndex].xdrPib), &(fData[*fIndex].block_compression)) &&
      setXdrPos(&(fData[*fIndex].xdrPib), filePtr_End));

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
* Read the label and size of the next block.
*
* @param blocktype - location to place the block label.
*
* @param blockparm - The block parameters.
*
* @return true - success,
*         false - faliure.
*
**/
void getNextPibBlock(sInt *err, sInt *fIndex, char* blocktype, sInt blockparm[])
{
   bool_t rc;
   uInt sizep = 24;
   fData[*fIndex].pibPtr_BlockStart = getXdrPos(&(fData[*fIndex].xdrPib));

   //fprintf(stderr,"*getNextPibBlock*");

   rc = xdr_bytes(&(fData[*fIndex].xdrPib), &blocktype, &sizep,24) &&
        xdr_int (&(fData[*fIndex].xdrPib), &(blockparm[0])) &&
        xdr_int (&(fData[*fIndex].xdrPib), &(blockparm[1])) &&
        xdr_int (&(fData[*fIndex].xdrPib), &(blockparm[2]));

   //fprintf(stderr," rc=%d  %s [%d,%d,%d]\n",rc,blocktype,blockparm[0],blockparm[1],blockparm[2]);

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
* Read the label and size of the next block.
*
* @param blocktype - location to place the block label.
*
* @param blocksize - The block size.
*
* @param blockCompression - The block compression.
*
* @param version   - The block version.
*
* @return true - success,
*         false - failure.
*
**/
void getNextPibBlockLong(sInt *err, sInt *fIndex, char* blocktype, sInt *blocksize,
                   sInt *blockCompression, sInt* version)
{
   bool_t rc;
   uInt sizep = 24;
   fData[*fIndex].pibPtr_BlockStart = getXdrPos(&(fData[*fIndex].xdrPib));

   //fprintf(stderr,"*getNextPibBlock* fData[*fIndex].pibPtr_BlockStart=%ld\n",fData[*fIndex].pibPtr_BlockStart);

   rc = xdr_bytes(&(fData[*fIndex].xdrPib), &blocktype,&sizep, sizep)   &&
          xdr_int (&(fData[*fIndex].xdrPib), blocksize)        &&
          xdr_int (&(fData[*fIndex].xdrPib), blockCompression) &&
          xdr_int (&(fData[*fIndex].xdrPib), version);

   //fprintf(stderr," rc=%d  %s [%d,%d,%d]\n",rc,blocktype,*blocksize, *blockCompression,*version);

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Skip over a block after opening it with getNextBlock.
 *
 * @return true - success,
 *         false - faliure.
 *
**/
void skipPibBlock(sInt *err, sInt *fIndex, sInt *blocksize)
{
   bool_t rc;
   rc = setXdrPos(&(fData[*fIndex].xdrPib), fData[*fIndex].pibPtr_BlockStart + (off_t)(*blocksize));
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Retrieve the current file position.
 *
**/
void getPibPos(sInt *fIndex, off_t *pos)
{
   *pos = (off_t)getXdrPos(&(fData[*fIndex].xdrPib));
}

/**
 * Set the current file position.
 *
**/
void setPibPos(sInt *err, sInt *fIndex, off_t *pos)
{
   bool_t rc;
   rc = setXdrPos(&(fData[*fIndex].xdrPib), (off_t)(*pos));
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Flush IO buffer contents to the file.
 *
**/
void flushPibFile(sInt *fIndex)
{
   fflush(fData[*fIndex].xPib);
}

/**
 * Read an integer value from the PIB file.
 */
void CReadXdrInt(sInt *err, sInt *fIndex, sInt *val)
{
   bool_t rc;

   rc = xdr_int (&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read an int64 value from the PIB file.
 */
void CReadXdrInt64(sInt *err, sInt *fIndex, sInt64 *val)
{
   bool_t rc;

   rc = xdr_hyper (&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read a float value from the PIB file.
 */
void CReadXdrFloat(sInt *err, sInt *fIndex, float *val)
{
   bool_t rc;
   rc = xdr_float(&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read a double value from the PIB file.
 */
void CReadXdrDouble(sInt *err, sInt *fIndex, double *val)
{
   bool_t rc;
   rc = xdr_double(&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read a string from the PIB file.
 */
void CReadXdrString(sInt *err, sInt *fIndex, char *val, sInt *maxlen)
{
   bool_t rc;
   sInt i;
   uInt csize;

   off_t filePtr_Cur = getXdrPos(&(fData[*fIndex].xdrPib));

   rc = xdr_u_int(&(fData[*fIndex].xdrPib), &csize) &&
       setXdrPos(&(fData[*fIndex].xdrPib), filePtr_Cur) &&
       xdr_string(&(fData[*fIndex].xdrPib), &val, (uInt)*maxlen);

   if(csize >= 0u && csize < (uInt)*maxlen) {
      for(i=csize; i<*maxlen; i++) {
         val[i] = '\0';
      }
   }

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read a raw byte array from the PIB file.
 */
void CReadXdrBytes(sInt *err, sInt *fIndex, char *val, sInt *maxlen)
{
   bool_t rc;
   sInt i;
   uInt csize;

   rc = xdr_bytes(&(fData[*fIndex].xdrPib), &val, &csize, (uInt)*maxlen);

   if(csize >= 0u && csize < (uInt)*maxlen) {
      for(i=csize; i<*maxlen; i++) {
         val[i] = '\0';
      }
   }

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write an int to the PIB file.
 */
void CWriteXdrInt(sInt *err, sInt *fIndex, sInt *val)
{
   bool_t rc;

   rc = xdr_int (&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write an int64 to the PIB file.
 */
void CWriteXdrInt64(sInt *err, sInt *fIndex, sInt64 *val)
{
   bool_t rc;

   rc = xdr_hyper (&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write a float to the PIB file.
 */
void CWriteXdrFloat(sInt *err, sInt *fIndex, float *val)
{
   bool_t rc;
   rc = xdr_float(&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write an double to the PIB file.
 */
void CWriteXdrDouble(sInt *err, sInt *fIndex, double *val)
{
   bool_t rc;
   rc = xdr_double(&(fData[*fIndex].xdrPib), val);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write a string to the PIB file.
 */
void CWriteXdrString(sInt *err, sInt *fIndex, char *val, sInt *maxlen)
{
   bool_t rc;
   uInt sizep = (uInt)*maxlen;
   rc = xdr_string(&(fData[*fIndex].xdrPib), &val, sizep);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Write a raw byte array to the PIB file.
 */
void CWriteXdrBytes(sInt *err, sInt *fIndex, char *val, sInt *maxlen)
{
   bool_t rc;
   uInt sizep = (uInt)*maxlen;
   rc = xdr_bytes(&(fData[*fIndex].xdrPib), &val, &sizep, sizep);
   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

void CReadXdrArrSize(sInt *err, sInt *fIndex, sInt *len)
{
   bool_t rc;

   off_t filePtr_Cur = getXdrPos(&(fData[*fIndex].xdrPib));

   rc = xdr_int (&(fData[*fIndex].xdrPib), len);

   if(rc && (*len > 0))
      rc = setXdrPos(&(fData[*fIndex].xdrPib), filePtr_Cur);

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read an integer array from the PIB file.
 */
void CReadXdrIntArr(sInt *err, sInt *fIndex, sInt *len, sInt **val)
{
   bool_t rc;
   uInt csize = (uInt)len;
   rc = xdr_array(&(fData[*fIndex].xdrPib), (caddr_t *)val, &csize, csize, sizeof(sInt),(xdrproc_t)xdr_int );

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read an float array from the PIB file.
 */
void CReadXdrFloatArr(sInt *err, sInt *fIndex, sInt *len, float **val)
{
   bool_t rc;
   uInt csize = (uInt)len;

   rc = xdr_array(&(fData[*fIndex].xdrPib), (caddr_t *)val, &csize, csize, sizeof(float),(xdrproc_t)xdr_float);

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}

/**
 * Read an double array from the PIB file.
 */
void CReadXdrDoubleArr(sInt *err, sInt *fIndex, sInt *len, double **val)
{
   bool_t rc;
   uInt csize = (uInt)len;

   rc = xdr_array(&(fData[*fIndex].xdrPib), (caddr_t *)val, &csize, csize, sizeof(double),(xdrproc_t)xdr_double);

   if(rc) {
     *err = 0;
   } else {
     *err = 1;
   }
}



