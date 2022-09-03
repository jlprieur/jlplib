/*************************************************************************
* jlp_gdlg.cpp
* JLP_GDlg Class: routine to create a popup window
*  in order to obtain the values of some parameters
*
* C++ version with classes
*
* JLP
* Version 04/12/2008 
**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>    // isdigit()

#include "jlp_macros.h"               // For definitions of macros: MAXI
#include "jlp_gdlg.h"
#include "time.h"

/*
#define DEBUG
*/
/* Contains:
int JLP_GDlg::FinalizeWidgets()
int JLP_GDlg::AddRadioButton(char *label0, int widget_type0, int radio_group0,
                            char val_type0, char *svalue0, int is_checked0, 
                            int x_ul, int y_ul, int& idata, int height0,
                            int &n_created_lines, int OnlyEvaluation)
int JLP_GDlg::AddWidget(char *label0, int widget_type0, int radio_group0,
                       char val_type0, char *svalue0, int is_checked0, 
                       int x_ul, int y_ul, int& idata, int height0,
                       int &n_created_lines, int OnlyEvaluation)
int JLP_GDlg::IsGoodValue(char cc, char value_type)
*/

/************************************************************************
* Check if radio buttons are OK
* and initialize cursor positions
*************************************************************************/
int JLP_GDlg::FinalizeWidgets()
{
register int i,j;
int found, k;

// Check that RadioButtons are coherent:
 for(i = 0; i < nJRadio; i++) {
   found = 0;
   for(j = 0; j < JRadio[i].nmembers; j++) {
     k = JRadio[i].iJWid[j];
// If value is not null, set it to one:
     if(!found && JWid[k].is_checked != 0) {
       JWid[k].is_checked = 1;
       found = k+1; 
       } else { 
       JWid[k].is_checked = 0;
       }
     }  // EOF loop on j
// If not found, set first value to one:
   if(!found) {
     k = JRadio[i].iJWid[0];
     JWid[k].is_checked = 1;
   }
 }  // EOF loop on i

// Initialize cursor position in Edit buttons:
 for(i = 0; i < nJEdit; i++) {
  j = JEdit_iJWid[i]; 
  JWid[j].svalue_icur = 0;
  }

return(0);
}
/************************************************************************
* Definition of widget structure (in "jlp_gdialog.h"):
* typedef struct {
* char label[80];                  // string of static label (input)
* int is_checked;                  // flag (input/output)
* char svalue[80];                 // char value (input/output)
* char val_type;                   // value type (input)
*                                  // 'd','f', 'c' (decimal, double, character)
* int widget_type;                 // widget type (input)
*                                  // 1=static 2=check 3=radio 4=edit
* int radio_group;                 // group number if radio button (input)
* int string_size;                 // size of label and svalue strings (80 here)
* } JLP_WIDGET;
*
* INPUT:
* x_ul, y_ul: location in device coordinates of the upper-left corner
* idata: index in input data array, corresponding to the added widget
* OnlyEvaluation: if set to one, do not create Widgets
*                 only evaluate the number of lines to be created
*************************************************************************/
int JLP_GDlg::AddRadioButton(char *label0, int widget_type0, int radio_group0,
                            char val_type0, char *svalue0, int is_checked0, 
                            int x_ul, int y_ul, int& idata, int height0,
                            int &n_created_lines, int OnlyEvaluation)
{
int i, j, irad, status = -1;
char title[80];

n_created_lines = 0;

if(widget_type0 != 3) {
 fprintf(stderr,"AddRadioButton/Fatal error, widget_type = %d\n", widget_type0);
 exit(-1);
 }

// Need at least two free lines (for the title or bottom line)
if(nJWid >= NWIDGET_MAX - 1) {
  fprintf(stderr,"AddRadioButton/Error, maximum number of widgets: %d \
has been reached\n", nJWid);
  return(-1);
  }

// Index of new item of JWid is nJWid:
j = nJWid; 

// Create title (stored in svalue) if first member of RadioButton group:
irad = radio_group0;
if(JRadio[irad].nmembers == -1) {
  sprintf(title,"%s",svalue0);
  status = CreateStaticLabel(title, x_ul, y_ul, j, height0, n_created_lines);
  if(status) {
    fprintf(stderr,"AddRadioButton/Fatal error creating title\n");
    exit(-1);
    }
   JRadio[irad].nmembers = 0;
  nJWid++;
  y_ul += height0;
} 

// Index of new item of JWid is nJWid:
j = nJWid; 

/********************************************************************/
// Now Create Radio Button

// Copy label up to the first '\0' or first '\n' encountered:
  for(i = 0; i < 79 && label0[i] && label0[i] != '\n'; i++) 
                                              JWid[j].label[i] = label0[i];
  JWid[j].label[i] = '\0';
  JWid[j].widget_type = widget_type0; 
  JWid[j].radio_group = radio_group0; 
  JWid[j].val_type = val_type0; 
  JWid[j].idata = idata; 
  JWid[j].svalue[0] = '\0';
  JWid[j].svalue_len = 0;
  JWid[j].is_checked = is_checked0;

// RadioButton: 
    if(JWid[j].radio_group < 0 || JWid[j].radio_group > NRADIO_MAX) {
       fprintf(stderr,"AddWidget/Error: invalid radio_group number: %d (max=%d)\n",
                JWid[j].radio_group, NRADIO_MAX);
       status = -1;
       }
    else {
// Store index of gadget among the indices of the radio group:
         irad = JWid[j].radio_group;
         JRadio[irad].iJWid[(JRadio[irad].nmembers)] = j;
         JRadio[irad].nmembers++;
         nJRadio = MAXI(irad+1, nJRadio);
       if(OnlyEvaluation)
         status = 0;
       else { 
         JWid[j].JWid_id = nJWid;
         status = CreateRadioButton(JWid[j], x_ul, y_ul);
         }
// Increase number of widgets:
       if(!status) {
          nJWid++;
          n_created_lines++;
         }
       }


return(status);
}
/************************************************************************
* Definition of widget structure (in "jlp_gdialog.h"):
* typedef struct {
* char label[80];                  // string of static label (input)
* int is_checked;                  // flag (input/output)
* char svalue[80];                 // char value (input/output)
* char val_type;                   // value type (input)
*                                  // 'd','f', 'c' (decimal, double, character)
* int widget_type;                 // widget type (input)
*                                  // 1=static 2=check 3=radio 4=edit
* int radio_group;                 // group number if radio button (input)
* int string_size;                 // size of label and svalue strings (80 here)
* } JLP_WIDGET;
*
* INPUT:
* x_ul, y_ul: location in device coordinates of the upper-left corner
* idata: index in input data array, corresponding to the added widget
* OnlyEvaluation: if set to one, do not create Widgets
*                 only evaluate the number of lines to be created
*************************************************************************/
int JLP_GDlg::AddWidget(char *label0, int widget_type0, int radio_group0,
                       char val_type0, char *svalue0, int is_checked0, 
                       int x_ul, int y_ul, int& idata, int height0,
                       int &n_created_lines, int OnlyEvaluation)
{
int i, j, status = -1;

n_created_lines = 0;

if(nJWid >= NWIDGET_MAX) {
  fprintf(stderr,"AddWidget/Error, maximum number of widgets: %d has been reached\n",
         nJWid);
  return(-1);
  }

// Index of new item of JWid is nJWid:
  j = nJWid; 

// Initialization:

// Input values:
// Copy label up to the first '\0' or first '\n' encountered:
  for(i = 0; i < 79 && label0[i] && isprint(label0[i]) 
       && label0[i] != '\n'; i++) JWid[j].label[i] = label0[i];
  JWid[j].label[i] = '\0';
  JWid[j].widget_type = widget_type0; 
  JWid[j].radio_group = radio_group0; 
  JWid[j].val_type = val_type0; 
  JWid[j].idata = idata; 

// Input/output values:
  JWid[j].is_checked = is_checked0;
  strcpy(JWid[j].svalue, svalue0);
  JWid[j].svalue_len = strlen(JWid[j].svalue);

// Check if new structure is OK:
switch (JWid[j].widget_type) {
// Static label:
  case 1:
    status = CreateStaticLabel(label0, x_ul, y_ul, j, height0, n_created_lines);
    break;
// CheckButton: 
  case 2:
    if(OnlyEvaluation) 
      status = 0;
    else {
      JWid[j].JWid_id = nJWid;
      status = CreateCheckButton(JWid[j], x_ul, y_ul);
      }
    if(!status) {
      nJWid++;
      n_created_lines++;
      }
    break;
// EditButton: 
  case 4:
    if(OnlyEvaluation)
       status = 0;
    else {
       JWid[j].JWid_id = nJWid;
       status = CreateEditButton(JWid[j], x_ul, y_ul);
       }
// Set the value of JEdit_iJWid to the current widget index 
// and increase the counter of Edit Buttons:
    if(!status){
        nJWid++;
        n_created_lines++;
        JEdit_iJWid[nJEdit] = j;
        nJEdit++;
      }
    break;
  default:
    fprintf(stderr,"AddWidget: j=%d unknown widget_type: %d\n", j, JWid[j].widget_type);
    break;
  }

return(status);
}
/*****************************************************************
* Check if the new character cc is compatible to value_type 
*
* INPUT:
* cc: new character to be tested
* value_type: 'c', 'i', or 'f' 
*
* OUTPUT:
* return 1 if OK, 0 otherwise
*****************************************************************/
int JLP_GDlg::IsGoodValue(char cc, char value_type)
{
int good = 0;

switch (value_type) {
   case 'c':
     if(isprint(cc)) good = 1;
     break;
// For decimal (=integer) values: sign and digits are allowed 
   case 'd' :
     if(isdigit(cc) || cc == '+' || cc == '-') good = 1;
     break;
// For double values: exponent, dot, sign and digits are allowed: 
   case 'f' :
     if(isdigit(cc) || cc == '.' || cc == '+' || cc == '-'
        || cc == 'E' || cc == 'e') good = 1;
     break;
   default:
     fprintf(stderr, "IsGoodValue/Fatal error: type=%c is unknown!\n", value_type);
     exit(-1);
   }

return(good);
}
