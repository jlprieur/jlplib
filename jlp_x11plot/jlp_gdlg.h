/*************************************************************************
* jlp_gdlg.h
* JLP_GDlg Class: routines to create a popup window 
*  in order to obtain the values of some parameters
*
* C++ version with classes
*
* JLP
* Version 02/12/2008 
**************************************************************************/
#ifndef __jlp_gdialog_h                /* sentry */
#define __jlp_gdialog_h

// Maximum number of widgets:
#define NWIDGET_MAX 30
#define NRADIO_MAX 30

// Definition of widget structure
typedef struct {
char label[80];                 // string of static label (input)
int is_checked;                 // = 1 if button is checked (input/output)
char svalue[80];                // string value of EditButton (input/output) 
char svalue_icur;               // cursor position in svalue (EditButton) 
char svalue_len;                // length of string value (including '\0') 
char val_type;                  // value type (input)
int widget_type;                // widget type (input)
                                // 1=static 2=check 3=radio 4=edit
int radio_group;                // group number if radio button (input)
int x_label, y_label;           // Location of label in the DialogBox 
                                // (in device coordinates)
int JWid_id;                    // Index of button (to identify it)
int win_width, win_height;      // size of button (device coordinates) 
int idata;                      // Correspondence with input data array
int string_size;                // Size of label and svalue  strings (80 here)
} JLP_WIDGET;

// RadioButtons:
typedef struct {
int iJWid[NWIDGET_MAX];          // Index of Widget linked to this button
int nmembers;
} JLP_RADIO;

// Definition of JLP_GDlg class:
class JLP_GDlg {

public:
  
// Pure virtual functions (should be followed by "=0"):
virtual int OpenDialogBox(const char *title, const int small_size) = 0;

virtual int CreateOKCancel(int y_ul) = 0;

virtual int WaitForOK(char svalues[][80], int *is_checked, 
            int nvalues, int& is_OK) = 0;

/* Not used ??
virtual int LoadWidgets(JLP_WIDGET *JWid0, int nwidgets) = 0;
*/
virtual int CreateWidgets(char label0[][80], int *widget_type0, 
                          int *radio_group0, char *val_type0, 
                          char svalue0[][80], int *is_checked0, 
                          int nwidgets0) = 0;

virtual int CreateStaticLabel(char *label0, int x_ul, int y_ul, int iJWid,
                              int height0, int &n_created_lines) = 0;
virtual int CreateCheckButton(JLP_WIDGET &JWid0, int x_ul, int y_ul) = 0;
virtual int CreateRadioButton(JLP_WIDGET &JWid0, int x_ul, int y_ul) = 0;
virtual int CreateEditButton(JLP_WIDGET &JWid0, int x_ul, int y_ul) = 0;

protected:

// Defined in jlp_gdialog.cpp: 
 int AddWidget(char *label0, int widget_type0, int radio_group0,
                char val_type0, char *svalue0, int is_checked0, 
                int x_ul, int y_ul, int& idata, int height0,
                int &n_created_lines, int OnlyEvaluation);
 int AddRadioButton(char *label0, int widget_type0, int radio_group0,
                     char val_type0, char *svalue0, int is_checked0, 
                     int x_ul, int y_ul, int& idata, int height0,
                     int &n_created_lines, int OnlyEvaluation);
 int FinalizeWidgets();
 int IsGoodValue(char cc, char value_type);

// Defined here:
/**********************************************************************
* JLP_DGDlg::Setup
* Initialize common variables
**********************************************************************/
void Setup() {
register int i, j;

// Reset the number of widgets:
   nJWid = 0; 
   nJRadio = 0; 
   nJEdit = 0;

// labels:
 strcpy(OK_label,"OK");
 strcpy(Cancel_label,"Cancel");

// Widgets 
 for(i = 0; i < NWIDGET_MAX; i++) {
   JWid[i].JWid_id = 0;
   JWid[i].string_size = 80;
   JWid[i].svalue_icur = 0;
   JWid[i].svalue_len = 0;
   JWid[i].svalue[0] = '\0';
   JWid[i].label[0] = '\0';
   JWid[i].idata = -1;
   JWid[i].widget_type = 0;
   }

// Radio Buttons:
 for(i = 0; i < NRADIO_MAX; i++) {
   JRadio[i].nmembers = -1;
   for(j = 0; j < NWIDGET_MAX; j++) JRadio[i].iJWid[j] = 0;
   }
}

// Private variables (in fact protected):
int window_is_open, nJWid, nJRadio, nJEdit;
int OK_x, OK_y, Cancel_x, Cancel_y;
int Dialog_screen;
int Dialog_width, Dialog_height;
char OK_label[3], Cancel_label[7];
int nRadio;
unsigned long Gray[256];
unsigned long int Black, White;

JLP_WIDGET JWid[NWIDGET_MAX];

JLP_RADIO JRadio[NRADIO_MAX];

int JEdit_iJWid[NWIDGET_MAX];
};

#endif    /* __jlp_gdialog_h sentry */
