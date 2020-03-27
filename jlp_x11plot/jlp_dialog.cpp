/***********************************************************************
* jlp_dialog
* C interface with JLP_Dialog class
*
* JLP
* Version 16/11/2006
***********************************************************************/
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <ctype.h>   // isdigit()

#include "jlp_dialog.h"
#include "jlp_gdlg.h"

#if JLP_USE_X11           /* New flag to disable X11 if necessary */
#include "jlp_gdlg_x11.h"
#endif

#define NN 20

static int init_dialog(char label[][80], char svalue[][80], char *val_type,
                       int *widget_type, int *radio_group, int *is_checked);

/*************************************************************************
* Test of Dialog Box
* (from "fit_moffat.cpp")
*************************************************************************/
int JLP_Dlg_Test(char *title, char *prompt, char *sval, int *is_OK)
{
int is_ok, nwidgets, i;
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];

for(i = 0; i < NN; i++) {
  widget_type[i] = 0;
  radio_group[i] = -1;
  label[i][0] = '\0';
  svalue[i][0] = '\0';
  is_checked[i] = 0;
  val_type[i] = 'c';
  }

// Static label:
i = 0;
widget_type[i] = 1;
strcpy(label[i], "Static Label for test\n(with something added)\n(as you can see)");

// Check Button:
i = 1;
widget_type[i] = 2;
strcpy(label[i], "Check Button for test");
is_checked[i] = 1;

// Radio Buttons:
i = 2;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(label[i], "1st choice");
strcpy(svalue[i], "Test of Radio Buttons");
is_checked[i] = 0;

i = 3;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(label[i], "2nd choice");
is_checked[i] = 1;

// Edit button: 
i = 4;
widget_type[i] = 4;
strcpy(label[i], "Enter filename:");
val_type[i] = 'c';
strcpy(svalue[i],"ttt.gra");

// Edit button: 
i = 5;
widget_type[i] = 4;
strcpy(label[i], "Enter integer value:");
val_type[i] = 'd';
sprintf(svalue[i],"%d",235);

// Edit button: 
i = 6;
widget_type[i] = 4;
strcpy(label[i], "Enter double value:");
val_type[i] = 'f';
sprintf(svalue[i],"%.2e",23.5e+05);

nwidgets = i+1;

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11(title, small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

for(i = 0; i < nwidgets; i++) {
  printf("Output from Dialog Box: svalue[%d]=%s, is_checked=%d\n",
         i,svalue[i], is_checked[i]); 
  }

*is_OK = is_ok;
strcpy(sval, svalue[4]);

printf("Output from Dialog Box: sval=%s, is_OK=%d\n",
        sval, *is_OK); 

return(0);
}
/************************************************************************
* Routine to prompt the options for hardcopy of curves 
*
* INPUT/OUTPUT:
*  title: title to be dislayed on top of the graph
*
* OUTPUT:
*  TeX_fonts: flag set to 1 if TeX fonts are wanted 
*  pst_filename: filename of output postscript file 
*  pst_plotdev: name of the plotting device (e.g. "square/pst.tmp")
************************************************************************/
int JLP_Dlg_CurveHardcopyOptions(char *pst_filename, char *pst_plotdev,
                                 char *title, int *TeX_fonts, int *is_OK)
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int is_ok, nwidgets, i;
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];
int ipost, iland, isqua;

for(i = 0; i < NN; i++) {
  widget_type[i] = 0;
  radio_group[i] = -1;
  label[i][0] = '\0';
  svalue[i][0] = '\0';
  is_checked[i] = 0;
  val_type[i] = 'c';
  }

// Edit button: 
i = 0;
widget_type[i] = 4;
strcpy(label[i], "Ouput filename:");
val_type[i] = 'c';
strcpy(svalue[i],"pst.tmp");
strcpy(pst_filename,"pst.tmp");
sprintf(pst_plotdev,"square/%s",pst_filename);

// Edit button: 
i = 1;
widget_type[i] = 4;
strcpy(label[i], "Title: ");
val_type[i] = 'c';
strncpy(svalue[i], title,80);
svalue[i][79] = '\0';

// Static label:
i = 2;
widget_type[i] = 1;
strcpy(label[i], "_______________________________________");
strcpy(label[i],  "======================================");

// Check Button:
i = 3;
widget_type[i] = 2;
strcpy(label[i], "TeX fonts");
is_checked[i] = 0;
*TeX_fonts = 0;

// Radio Buttons:
i = 4;
iland = 4;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(svalue[i], "_________ Postscript device: _________");
strcpy(svalue[i], "========= Postscript device: =========");
strcpy(label[i], "Landscape");
is_checked[i] = 0;

i = 5;
isqua = 5;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(label[i], "Square");
is_checked[i] = 1;

i = 6;
ipost = 6;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(label[i], "Portrait");
is_checked[i] = 0;

// Static label:
i = 7;
widget_type[i] = 1;
strcpy(label[i],  "______________________________________");

nwidgets = i+1;

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11("Options for postscript hardcopy", small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

*is_OK = is_ok;
if(!is_ok) return(-1);

*TeX_fonts = is_checked[3] ? 1 : 0;
strcpy(pst_filename, svalue[0]);
strcpy(title, svalue[1]);

  if(is_checked[iland] && !(*TeX_fonts)) 
     sprintf(pst_plotdev,"landscape/%s",pst_filename);
  else if(is_checked[iland] && (*TeX_fonts)) 
     sprintf(pst_plotdev,"LANDSCAPE/%s",pst_filename);
  else if(is_checked[ipost] && !(*TeX_fonts)) 
     sprintf(pst_plotdev,"postscript/%s",pst_filename);
  else if(is_checked[ipost] && (*TeX_fonts)) 
     sprintf(pst_plotdev,"POSTSCRIPT/%s",pst_filename);
  else if(is_checked[isqua] && (*TeX_fonts)) 
     sprintf(pst_plotdev,"SQUARE/%s",pst_filename);
  else 
     sprintf(pst_plotdev,"square/%s",pst_filename);


printf("Output from CurveHarcopyOptions: pst_plotdev=%s, pst_filename=%s, \
TeX fonts=%d \n title=%s\n", pst_plotdev, pst_filename, *TeX_fonts, title); 

return(0);
}
/************************************************************************
* Routine to prompt the options for hardcopy of images 
*
* OUTPUT:
*  TeX_fonts: flag set to 1 if TeX fonts are wanted 
*  lut_scale : flag set to 1 if lut scale is to be displayed 
*  pst_filename: filename of output postscript file 
*  pst_plotdev: name of the plotting device (e.g. "square/pst.tmp")
*
* INPUT/OUTPUT:
*  black_and_white : flag set to 1 for B&W images
************************************************************************/
int JLP_Dlg_ImageHardcopyOptions(char *pst_filename, char *pst_plotdev,
                                 int *TeX_fonts, int *lut_scale, 
                                 int *black_and_white, int *high_resolution, 
                                 int *is_OK)
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int is_ok, nwidgets, i;
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];
int iland, isqua;

for(i = 0; i < NN; i++) {
  widget_type[i] = 0;
  radio_group[i] = -1;
  label[i][0] = '\0';
  svalue[i][0] = '\0';
  is_checked[i] = 0;
  val_type[i] = 'c';
  }

// Edit button: 
i = 0;
widget_type[i] = 4;
strcpy(label[i], "Ouput filename:");
val_type[i] = 'c';
strcpy(svalue[i],"pst.tmp");
strcpy(pst_filename,"pst.tmp");
sprintf(pst_plotdev,"square/%s",pst_filename);

// Static label:
i = 1;
widget_type[i] = 1;
strcpy(label[i],  "======================================");

// Check Button:
i = 2;
widget_type[i] = 2;
strcpy(label[i], "TeX fonts");
is_checked[i] = 0;
*TeX_fonts = 0;

// Check Button:
i = 3;
widget_type[i] = 2;
strcpy(label[i], "LUT scale");
is_checked[i] = 0;
*lut_scale = 0;

// Check Button:
i = 4;
widget_type[i] = 2;
strcpy(label[i], "Black and white");
if(*black_and_white == 1) {
  is_checked[i] = 1;
} else {
  is_checked[i] = 0;
  *black_and_white = 0;
}

// Check Button:
i = 5;
widget_type[i] = 2;
strcpy(label[i], "High resolution (no compression)");
if(*high_resolution == 1) {
  is_checked[i] = 1;
} else {
  is_checked[i] = 0;
  *high_resolution = 0;
}

// Radio Buttons:
i = 6;
iland = 6;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(svalue[i], "========= Postscript device: =========");
strcpy(label[i], "Landscape");
is_checked[i] = 0;

// For images only two options: landscape or portrait (= square here)
i = 7;
isqua = 7;
widget_type[i] = 3;
radio_group[i] = 0;
strcpy(label[i], "Portrait");
is_checked[i] = 1;

// Static label:
i = 8;
widget_type[i] = 1;
strcpy(label[i],  "______________________________________");

nwidgets = i+1;

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11("Options for postscript hardcopy", small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

*is_OK = is_ok;
if(!is_ok) return(-1);

*TeX_fonts = is_checked[2] ? 1 : 0;
*lut_scale = is_checked[3] ? 1 : 0;
*black_and_white = is_checked[4] ? 1 : 0;
*high_resolution = is_checked[5] ? 1 : 0;
strcpy(pst_filename, svalue[0]);

  if(is_checked[iland] && !(*TeX_fonts)) 
     sprintf(pst_plotdev,"landscape/%s",pst_filename);
  else if(is_checked[iland] && (*TeX_fonts)) 
     sprintf(pst_plotdev,"LANDSCAPE/%s",pst_filename);
  else if(is_checked[isqua] && (*TeX_fonts)) 
     sprintf(pst_plotdev,"SQUARE/%s",pst_filename);
  else 
     sprintf(pst_plotdev,"square/%s",pst_filename);

printf("Output from ImageHarcopyOptions: plotdev=%s, filename=%s, \
TeX=%d B&W=%d lut_scale=%d high_resolution=%d\n", 
        pst_plotdev, pst_filename, *TeX_fonts, *black_and_white, *lut_scale,
        *high_resolution); 

return(0);
}
/************************************************************************
* Routine to prompt for a character string with a Dialog Box
*
* INPUT/OUTPUT:
* sval: character string
************************************************************************/
int JLP_Dlg_GetString(char *title, char *explanations, char *prompt, 
                         char *sval, int *is_OK)
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int is_ok, nwidgets, i;
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];

init_dialog(label, svalue, val_type, widget_type, radio_group, is_checked);

// Static label:
i = 0;
widget_type[i] = 1;
strcpy(label[i], explanations);

// Edit button: 
i = 1;
widget_type[i] = 4;
strcpy(label[i], prompt);
val_type[i] = 'c';
strcpy(svalue[i], sval);

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11(title, small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
nwidgets = 2;
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

*is_OK = is_ok;
if(is_ok) strcpy(sval, svalue[1]);

#ifdef DEBUG
printf("GetString/Output from Dialog Box: sval=%s, is_OK=%d\n",
        sval, *is_OK); 
#endif

return(0);
}
/************************************************************************
* Routine to prompt for an integer value with a Dialog Box
*
* INPUT/OUTPUT:
* ival: integer 
************************************************************************/
int JLP_Dlg_GetInt(char *title, char *explanations, char *prompt, 
                      int *ival, int *is_OK)
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int is_ok, nwidgets, i;
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];

init_dialog(label, svalue, val_type, widget_type, radio_group, is_checked);

// Static label:
i = 0;
widget_type[i] = 1;
strcpy(label[i], explanations);

// Edit button: 
i = 1;
widget_type[i] = 4;
strcpy(label[i], prompt);
val_type[i] = 'd';
sprintf(svalue[i],"%d", *ival);

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11(title, small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
nwidgets = 2;
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

*is_OK = is_ok;
if(is_ok) sscanf(svalue[1],"%d", ival);

#ifdef DEBUG
printf("Output from Dialog Box: ival=%d, is_OK=%d\n",
        *ival, *is_OK); 
#endif

return(0);
}
/************************************************************************
* Routine to prompt for a double value with a Dialog Box
*
* INPUT/OUTPUT:
* fval: double 
************************************************************************/
int JLP_Dlg_GetFloat(char *title, char *explanations, char *prompt, 
                        double *fval, int *is_OK)
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int is_ok, nwidgets, i;
JLP_GDlg *JDlg0;
char label[NN][80], svalue[NN][80], val_type[NN];
int widget_type[NN], radio_group[NN], is_checked[NN];

init_dialog(label, svalue, val_type, widget_type, radio_group, is_checked);

// Static label:
i = 0;
widget_type[i] = 1;
strcpy(label[i], explanations);

// Edit button: 
i = 1;
widget_type[i] = 4;
strcpy(label[i], prompt);
val_type[i] = 'f';
sprintf(svalue[i],"%f", *fval);

// Open new DialogBox:
#if JLP_USE_X11
JDlg0 = new JLP_GDlg_X11(title, small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif

// Create the widgets:
nwidgets = 2;
JDlg0->CreateWidgets(label, widget_type, radio_group, val_type, 
                     svalue, is_checked, nwidgets);

JDlg0->WaitForOK(svalue, is_checked, nwidgets, is_ok);

delete JDlg0;

*is_OK = is_ok;
if(is_ok) {
  sscanf(svalue[1],"%lf", fval);
  }

#ifdef DEBUG
printf("Output from Dialog Box: fval=%f, is_OK=%d\n",
        *fval, *is_OK); 
#endif

return(0);
}
/************************************************************************
* Routine to initialize all arrays needed by JLP_Dlg_Get.. routines
************************************************************************/
static int init_dialog(char label[][80], char svalue[][80], char *val_type,
                       int *widget_type, int *radio_group, int *is_checked)
{
register int i;
for(i = 0; i < NN; i++) {
  widget_type[i] = 0;
  radio_group[i] = -1;
  label[i][0] = '\0';
  svalue[i][0] = '\0';
  is_checked[i] = 0;
  val_type[i] = 'c';
  }
return(0);
}
static JLP_GDlg *JDlg[5];
static int nJDlg = 0;
/**********************************************************************
*
**********************************************************************/
int JLP_Dlg_open_logbook(char *title, int *idv_logbook) 
{
#ifdef JLP_USE_X11
int  small_size = 1;
#endif
int status = -1;
*idv_logbook = -1;

if(nJDlg == 0) {
 *idv_logbook = 0;
 nJDlg = 1;
// Open new DialogBox:
#if JLP_USE_X11
 JDlg[*idv_logbook] = new JLP_GDlg_X11(title, small_size);
#else
if(1) {
  fprintf(stderr,"Fatal error: this routine needs X11 library!\n");
  exit(-1);
  }
#endif
 status = 0;
} else {
  fprintf(stderr,"JLP_Dlg_open_logbook/Error logbook already opened: nJDlg=%d\n", 
          nJDlg);
}

return(status);
}
/**********************************************************************
*
**********************************************************************/
int JLP_Dlg_close_logbook(int idv_logbook) 
{
int status = -1;

if(idv_logbook >= 0 && idv_logbook < nJDlg) {
  delete JDlg[idv_logbook];
  status = 0;
} else {
  fprintf(stderr,"JLP_Dlg_close_logbook/Error: idv_logbook=%d nJDlg=%d\n",
          idv_logbook, nJDlg);
}
return(status);
}
