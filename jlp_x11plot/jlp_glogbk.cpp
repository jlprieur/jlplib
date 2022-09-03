/*************************************************************************
* jlp_glogbk.cpp
* JLP_GLogbk Class: routine to create a popup window
*  in order to display the logbook (with  the values of some parameters)
*
* C++ version with classes
*
* JLP
* Version 04/12/2008 
**************************************************************************/
#include "jlp_glogbk.h"

/*
#define DEBUG
*/

/***************************************************************************
*
* INPUT:
***************************************************************************/
int JLP_GLogbk::UpdateLogbookBox(char *logbook_fname)
{
int status = -1;
char buffer[80];
FILE *fp;
register int i, j;

// Open logbook file:
  if((fp = fopen(logbook_fname,"r")) == NULL) {
  fprintf(stderr,"JLP_LogBook::UpdateLogBookBox/Error opening logfile >%s<\n",
          logbook_fname);
  } else {
    for(i = 0; i < nLines_max && !feof(fp); i++) {
/* Maximum length for a line will be 80 characters: */
      if(fgets(buffer,80,fp)) {
          buffer[79] = '\0';
          for(j = 0; j < 80 && isprint(buffer[j]); j++) 
                 Logbook_label[i][j] = buffer[j];
          Logbook_label[i][j] = '\0';
        } else {
          break;
        }
      }   
    nLines = i;
#ifdef DEBUG
    printf("UpdateLogbookBox/ %d lines successfully read (nLines_max = %d)\n",
            nLines, nLines_max);
#endif
    status = 0;
    fclose(fp);
  }

return(status);
}
