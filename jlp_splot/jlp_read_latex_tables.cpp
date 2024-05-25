/**********************************************************************
* jlp_read_latex_tables.cpp 
* to read files containing latex tables
*
*  JLP
* Version 31-03-2020
**********************************************************************/
#include <stdio.h>
#include <string.h>

#include "jlp_read_latex_tables.h" 

/* Maximum length for a line will be 170 characters: */
#define BUFFER_MAX 360

/* Positive number (to be able to compute sqrt in compute_mean) */
#define NO_DATA 10000.

#define DEBUG

/************************************************************************
* Scan an input file containing a latex table
*
* INPUT:
*  latex_fname0: filename of the input file containing the latex table
*  icol_x : number of the column containing xplot0
*  icol_y : number of the column containing yplot0
*  icol_errorx : number of the column containing errorx0
*  icol_errory : number of the column containing errory0
*
* OUTPUT:
*  xplot0: array of the X data to be plotted
*  yplot0: array of the Y data to be plotted
*  errorx0: array of the X error data to be plotted
*  errory0: array of the Y error data to be plotted
*  npts_size0 : size used to create the output arrays 
*  npts0 : number of points (curve xplot0, yplot0)
*  error_bars0 : flag set to one if error_bars
*
*************************************************************************/
int jlp_read_latex_table_to_double(char *latex_fname0, int icol_x, int icol_y,
                          int icol_errorx, int icol_errory, double **xplot0,
                          double **yplot0, double **errorx0, double **errory0,
                          int *npts_size0, int *npts0, int *error_bars0)
{
char in_line[256];
int iline, ipts, stat_x, stat_y, stat_xerr, stat_yerr, nlines, iverbose = 1;
double ww_x = 0., ww_y = 0., ww_xerr = 0., ww_yerr = 0.;
FILE *fp_in_latex;

#ifdef DEBUG
char out_fname[64];
FILE *fp_out;
  strcpy(out_fname, "out_file_debug.txt");
  if((fp_out = fopen(out_fname, "w")) == NULL) {
    fprintf(stderr, "read_latex_table/DEBUG/Fatal error opening output ascii file: %s\n",
           out_fname);
    return(-1);
   }
#endif

// First reading of the file to determine for the total number of lines
// (i.e. maximum size of output arrays)
/* Open the input latex table: */
  if((fp_in_latex = fopen(latex_fname0, "r")) == NULL) {
    fprintf(stderr, "plot_latex/Fatal error opening in latex file: %s\n",
           latex_fname0);
    return(-1);
   }

iline = 0;
while(!feof(fp_in_latex)) {
  if(fgets(in_line, 256, fp_in_latex)) {
    iline++;
  }
}
nlines = iline;
#ifdef DEBUG
 printf("jlp_read_latex_table/First step: latex file succesfully read: nlines = ", nlines);
#endif

// Close file (since all lines have been read and counted)
fclose(fp_in_latex);

*xplot0 = new double[nlines];
*yplot0 = new double[nlines];
*errorx0 = new double[nlines];
*errory0 = new double[nlines];
*error_bars0 = 0;
*npts_size0 = nlines;

// Initialize error values to zero:
for(ipts = 0; ipts < nlines; ipts++) {
  (*errorx0)[ipts] = 0.;
  (*errory0)[ipts] = 0.;
 }
// Second reading of the file to read the data:
/* Re-open the input latex table: */
  if((fp_in_latex = fopen(latex_fname0, "r")) == NULL) {
    fprintf(stderr, "plot_latex/Fatal error opening in latex file: %s\n",
           latex_fname0);
    return(-1);
   }
iline = 0;
ipts = 0;
while(!feof(fp_in_latex)) {
  if(fgets(in_line, 256, fp_in_latex)) {
// Read the x and y values in columns icol_x and icol_y:
      stat_x = latex_read_fvalue(in_line, &ww_x, icol_x, iverbose);
      stat_y = latex_read_fvalue(in_line, &ww_y, icol_y, iverbose);
// Read the x and y error values in columns icol_errorx and icol_errory:
      stat_xerr = latex_read_fvalue(in_line, &ww_xerr, icol_errorx, iverbose);
      stat_yerr = latex_read_fvalue(in_line, &ww_yerr, icol_errory, iverbose);
      if((stat_x == 0) && (stat_y == 0)){
        (*xplot0)[ipts] = ww_x;
        (*yplot0)[ipts] = ww_y;
        if(stat_xerr == 0) {
           *error_bars0 = 1;
           (*errorx0)[ipts] = ww_xerr;
           }
        if(stat_yerr == 0) {
           *error_bars0 = 1;
           (*errory0)[ipts] = ww_yerr;
           }
        ipts++;
#ifdef DEBUG
        fprintf(fp_out,"%f %f %f %f %.30s\n", ww_x, ww_y, ww_xerr, ww_yerr, in_line);
#endif
      } else {
/*** DEBUG
        fprintf(fp_out,"Warning: error reading value: %s (line=%d, icol_x=%d)\n",
                in_line, iline, icol_x);
*/
      } // If status
    iline++;
   } // If fgets
 } /* EOF while ... */
#ifdef DEBUG
 printf("jlp_read_latex_table/Second step: from input_latex, %d lines sucessfully read\n", iline);
#endif

*npts0 = ipts;

fclose(fp_in_latex);
#ifdef DEBUG
fclose(fp_out);
printf("jlp_read_latex_table/output debug file %s successfuly written\n", out_fname);
#endif
return(0);
}

/************************************************************************
* Scan an input file containing a latex table
*
* INPUT:
*  latex_fname0: filename of the input file containing the latex table
*  icol_x : number of the column containing xplot0
*  icol_y : number of the column containing yplot0
*  icol_errorx : number of the column containing errorx0
*  icol_errory : number of the column containing errory0
*
* OUTPUT:
*  xplot0: array of the X data to be plotted
*  yplot0: array of the Y data to be plotted
*  errorx0: array of the X error data to be plotted
*  errory0: array of the Y error data to be plotted
*  npts_size0: size used to create the output arrays 
*  npts0 : number of points (curve xplot0, yplot0)
*  error_bars0 : flag set to one if error_bars
*
*************************************************************************/
int jlp_read_latex_table_to_float(char *latex_fname0, int icol_x, int icol_y,
                          int icol_errorx, int icol_errory, float **xplot0,
                          float **yplot0, float **errorx0, float **errory0,
                          int *npts_size0, int *npts0, int *error_bars0)
{
int status, i;
double *xxplot0, *yyplot0, *errorxx0, *erroryy0;

status = jlp_read_latex_table_to_double(latex_fname0, icol_x, icol_y,
                          icol_errorx, icol_errory, &xxplot0,
                          &yyplot0, &errorxx0, &erroryy0, 
                          npts_size0, npts0, error_bars0);
if(status == 0) {
  *xplot0 = new float[*npts_size0];
  *yplot0 = new float[*npts_size0];
  *errorx0 = new float[*npts_size0];
  *errory0 = new float[*npts_size0];
  for(i = 0; i < *npts_size0; i++) {
    *xplot0[i] = (float)(xxplot0[i]);
    *yplot0[i] = (float)(yyplot0[i]);
    *errorx0[i] = (float)(errorxx0[i]);
    *errory0[i] = (float)(erroryy0[i]);
    } // EOF i loop
  delete[] xxplot0;
  delete[] yyplot0;
  delete[] errorxx0;
  delete[] erroryy0;
  } // EOF status==0 case 
return(status);
}
/**************************************************************************
* Read integer value in column #icol from b_data string
*
**************************************************************************/
int latex_read_ivalue(char *b_data, int *value, int icol) 
{
int ival, status;
char buff[60];

*value = 0.;
status = latex_read_svalue(b_data, buff, icol); 
if(!status) { 
   ival = sscanf(buff, "%d", value);
/*
printf("latex_read_ivalue/buff=>%s< value=%d ival=%d\n", buff, *value, ival);
*/
   if(ival <= 0) status = 1;
  }

return(status);
}
/**************************************************************************
* Read double value in column #icol from b_data string
*
* INPUT:
* iverbose : (i = 1 verbose if error)
*           (i > 1 verbose if error and even if no error)
**************************************************************************/
int latex_read_fvalue(char *b_data, double *value, int icol, int iverbose) 
{
int ival, status;
char buff[60], nodata[60];

*value = 0.;
status = latex_read_svalue(b_data, buff, icol); 
if(status == 0) { 
   sscanf(buff, "%s", nodata);
   if(!strncmp(nodata,"\\nodata",7)) { 
     if(iverbose > 2) printf("%s latex_read_fvalue: nodata found! \n", b_data);
     *value = NO_DATA;
     status = 1;
     }
   else {
   ival = sscanf(buff, "%lf", value);
   if(ival <= 0) { 
      if(iverbose > 1) printf("latex_read_fvalue/buff=>%s< value=%.2f ival=%d\n", buff, *value, ival);
      status = 1;
      }
   }
  }
return(status);
}
/**************************************************************************
* Read string value in column #icol from b_data string
*
**************************************************************************/
int latex_read_svalue(char *b_data, char *value, int icol) 
{
int ic, status, column_is_found;
char buff[BUFFER_MAX], data[BUFFER_MAX], *pc;

*value = '\0';

// Comments (line starting with % or #):
if(b_data[0] == '%' || b_data[0] == '#') {
 return(-1);
 }

strcpy(data, b_data);

pc = data;
data[BUFFER_MAX-1] = '\0';
column_is_found = 0;
ic = 1;
buff[0] = '\0';
while(*pc && strncmp(pc,"\\\\",2)) {
  if(ic == icol) {
    column_is_found = 1; 
    strcpy(buff,pc);
    break;
    }
  if(*pc == '&') { 
    ic++;
    }
  pc++;
  }
*pc = '\0';
/* Return if column not found, or empty */
if(!buff[0]) return(-1); 

/* Otherwise go on analysis: */
status = 1;
buff[BUFFER_MAX-1] = '\0';
pc = buff;
while(*pc) {
  if(*pc == '&' || !strncmp(pc,"\\\\",2)) {
    *pc = '\0'; 
    strcpy(value,buff);
    if(*value) status = 0;
    break;
    }
  pc++;
  }

/* Removes '\r' (Carriage Return) if present: */
if(status == 0) {
pc = value;
while(*pc) {
  if(*pc == '\r') *pc = ' ';
  pc++;
  }
}

return(status);
}
/**************************************************************************
* Write a double value in column #icol to b_out string
*
* INPUT:
* b_data: current value of the line
* value: value to be written in b_data
* nber_of_decimal: number of decimals for the output format
*
* OUTPUT:
* b_out: updated value of the line b_data with the input value in Col.#icol
*
**************************************************************************/
int latex_write_fvalue(char *b_data, char *b_out, double value, int icol, 
                       int nber_of_decimals) 
{
int i, ic, column_is_found, istart, iend;
char data[360], *pc;

strcpy(data, b_data);

pc = data;
column_is_found = 0;
ic = 1;
i = 0;
while(*pc) {
  if(ic == icol && !column_is_found) {
    column_is_found = 1; 
    istart = i;
    }
  else if(ic == icol+1) {
    iend = i-1;
    break;
    }
  if(*pc == '&') { 
    ic++;
    }
  pc++;
  i++;
  }
/* Return if column not found, or empty */
if(istart == 0 || iend == istart) return(-1); 

strcpy(b_out, b_data);

switch(nber_of_decimals) {
  case 1:
    sprintf(&b_out[istart],"%8.1f ",value);
    break;
  case 2:
    sprintf(&b_out[istart],"%8.2f ",value);
    break;
  case 3:
  default:
    sprintf(&b_out[istart],"%8.3f ",value);
    break;
  case 4:
    sprintf(&b_out[istart],"%9.4f ",value);
    break;
  }
strcpy(&b_out[istart+9],&b_data[iend]);

/*
printf("update_value/from >%s< to >%s< (value=%.2f)\n", b_data, b_out, value);
*/

return(0);
}
