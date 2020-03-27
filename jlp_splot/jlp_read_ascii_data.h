/**********************************************************************
* jlp_read_ascii_data.h 
*
*  JLP
* Version 09-04-2018
**********************************************************************/
#ifndef _jlp_read_ascii_data
#define _jlp_read_ascii_data

int jlp_ascii_get_ncolumns(char *filename0, long *ncols0, long *ndatalines0,
                           long *nlines0);
int jlp_ascii_read_two_columns(char *filename0, int icol_x, int icol_y,
                               double **xplot0, double **yplot0, int *npts0);
int jlp_ascii_read_four_columns(char *filename0, int icol_x, int icol_y,
                                int icol_errorx, int icol_errory,
                                double **xplot0, double **yplot0, 
                                double **errorx0, double **errory0, 
                                int *npts0);

#endif
