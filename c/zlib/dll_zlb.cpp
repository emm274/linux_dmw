#include <string.h>
#include "zlib.h"

void __attribute__ ((constructor)) __load(void);
void __attribute__ ((destructor)) __unload(void);

void __load(void)
{
}

void __unload(void)
{
}

extern "C" {
    int zlibCompress(Bytef* InBuf, int InBytes,
	       			 Bytef* OutBuf, int OutBytes);

    int zlibDecompress(Bytef* InBuf, int InBytes,
	    	     	   Bytef* OutBuf, int OutBytes);
}

int zlibCompress(Bytef* InBuf, int InBytes,
 				 Bytef* OutBuf, int OutBytes)
{
	int rc=0;

	z_stream_s strm;
	memset(&strm,0,sizeof(strm));

	strm.next_in  =InBuf;
	strm.avail_in =InBytes;
  strm.next_out =OutBuf;
  strm.avail_out=OutBytes;

	int rc1=deflateInit(&strm,Z_DEFAULT_COMPRESSION);

	if (rc1 == Z_OK) {
		rc1=deflate(&strm,Z_FINISH);
		if (rc1 == Z_STREAM_END) rc=strm.total_out;
		deflateEnd(&strm);
	}

	return rc;
}

int zlibDecompress(Bytef* InBuf, int InBytes,
   	     		   Bytef* OutBuf, int OutBytes)
{
	int rc=0;
	z_stream_s strm;
	memset(&strm,0,sizeof(strm));

	strm.next_in  =InBuf;
	strm.avail_in =InBytes;
  strm.next_out =OutBuf;
  strm.avail_out=OutBytes;

  int rc1=inflateInit(&strm);
	if (rc1 != Z_OK)
		rc=-1;
	else {
		rc1=inflate(&strm, Z_FINISH);
		if (rc1 != Z_STREAM_END)
			rc=-2;
		else
			rc=strm.total_out;

		rc1=inflateEnd(&strm);
  	if ((rc1 != Z_OK) && (rc >= 0)) rc=-3;
	}

	return rc;
}

