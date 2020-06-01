/************************************************************************
* "jlp_string.h"
* String utilities
*
* JLP 
* Version 05/05/2013
*************************************************************************/
#ifndef _jlp_compact_string_
#define _jlp_compact_string_

#ifdef __cplusplus
extern "C" {
#endif

int jlp_remove_ext_string(char *str1, int len1);
int jlp_trim_string(char *str1, int len1);
int jlp_compact_string(char *str1, int len1);
int jlp_cleanup_string(char *str1, int len1);

#ifdef __cplusplus
}
#endif

#endif
