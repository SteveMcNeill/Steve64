#ifdef DEPENDENCY_ZLIB
  #include "download\zlib.h"
  qbs *func__deflate(qbs *text){
    uLongf filesize = (uint32)text->len;  //length of the text
    uLongf compsize = compressBound(filesize);
    unsigned char *dest = (unsigned char *)malloc(compsize);
    int32 result = compress(dest, &compsize, text->chr, filesize);
    qbs *ret = qbs_new(compsize,1);
    memcpy(ret->chr, dest, compsize);
    free(dest);
    return ret;
  }

  qbs *func__inflate(qbs *text, int64 originalsize){
    uLongf uncompsize = originalsize;
    unsigned char *dest = (unsigned char *)malloc(originalsize);
    int32 result = uncompress(dest, &uncompsize, text->chr, text->len);
    qbs *ret = qbs_new(uncompsize,1);
    memcpy(ret->chr, dest, uncompsize);
    free(dest);
    return ret;
  }
#endif