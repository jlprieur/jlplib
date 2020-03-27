/**********************************************************************
* jlp_read_latex_tables.h 
*
*  JLP
* Version 09-04-2018
**********************************************************************/
#ifndef _jlp_read_latex_tables   /* BOF sentry */
#define _jlp_read_latex_tables

int jlp_read_latex_table_file(char *latex_fname0, int icol_x, int icol_y,
                          int icol_errorx, int icol_errory, double **xplot0,
                          double **yplot0, double **errorx0, double **errory0,
                          int *npts0, int *error_bars0);
int latex_read_ivalue(char *b_data, int *value, int icol);
int latex_read_fvalue(char *b_data, double *value, int icol, int verbose); 
int latex_read_svalue(char *b_data, char *value, int icol); 
int latex_write_fvalue(char *b_data, char *b_out, double value, int icol,
                       int nber_of_decimals);

#endif  /* EOF sentry */
