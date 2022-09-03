/***********************************************************************
* jlp_logbook
* C interface with JLP_Logbook class
*
* (prototypes are defined in "jlp_dialog.h")
*
* JLP
* Version 29/12/2008
***********************************************************************/
#include "jlp_dialog.h"
#include "jlp_glogbk.h"
#if JLP_USE_X11             /* To be able to disable X11 if necessary */
#include "jlp_glogbk_x11.h"
#endif

#define NN 5

static JLP_GLogbk *JLogbook[5];
static int nJLogbook = 0;

/**********************************************************************
*
* INPUT:
* title
*
**********************************************************************/
int JLP_Logbook_display(char *logbook_fname, int *process_data) 
{
JLP_GLogbk *JLogb0;
char title[40];
int status = -1;

// Open new logbook window: 
 strncpy(title, logbook_fname, 40);
 title[39] = '\0';

#if JLP_USE_X11
 JLogb0 = new JLP_GLogbk_X11(title);
#endif

 JLogb0->UpdateLogbookBox(logbook_fname);

 JLogb0->WaitForClose(process_data); 

 JLogb0->WaitForClose(process_data); 

 JLogb0->CloseWindow(); 

 delete JLogb0;

return(status);
}
/**********************************************************************
*
* INPUT:
* title
*
* OUTPUT:
* idv_logbook
**********************************************************************/
int JLP_Logbook_open(char *title, int *idv_logbook) 
{
int status = -1;
*idv_logbook = -1;

if(nJLogbook == 0) {
 *idv_logbook = 0;
 nJLogbook = 1;
// Open new logbook window: 
#if JLP_USE_X11
 JLogbook[*idv_logbook] = new JLP_GLogbk_X11(title);
#endif
 status = 0;
} else {
  fprintf(stderr,"JLP_Logbook_open/Error logbook already opened: nJLogbook=%d\n", 
          nJLogbook);
}

return(status);
}
/**********************************************************************
*
**********************************************************************/
int JLP_Logbook_update(char *logbook_fname, int idv_logbook) 
{
int status = -1;

if(idv_logbook >= 0 && idv_logbook < nJLogbook) {
  status = JLogbook[idv_logbook]->UpdateLogbookBox(logbook_fname);
} else {
  fprintf(stderr,"JLP_Logbook_update/Error: idv_logbook=%d nJLogbook=%d\n",
          idv_logbook, nJLogbook);
}
return(status);
}
/**********************************************************************
*
**********************************************************************/
int JLP_Logbook_close(int idv_logbook) 
{
int status = -1;

if(idv_logbook >= 0 && idv_logbook < nJLogbook) {
  delete JLogbook[idv_logbook];
  status = 0;
} else {
  fprintf(stderr,"JLP_Logbook_close/Error: idv_logbook=%d nJLogbook=%d\n",
          idv_logbook, nJLogbook);
}
return(status);
}
