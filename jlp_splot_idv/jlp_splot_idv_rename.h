/********************************************************
* "jlp_splot_idv_rename.h"
* Interface C / Fortran
*           declared name in C ----> name when called from fortran
* IBM version      : JLP_EXAMPLE -----> jlp_example
* VAX/vms version  : JLP_EXAMPLE -----> JLP_EXAMPLE
* SUN version      : JLP_EXAMPLE -----> jlp_example_
* DEC/unix version : JLP_EXAMPLE -----> jlp_example_
* Linux version    : JLP_EXAMPLE -----> jlp_example__
* (GNU g77, Linux:
* With -funderscoring in effect, g77 appends two underscores to names with
* underscores and one underscore to external names with no underscores.)
*
Example in Cambridge:
printenv OSTYPE
solaris
linux
*
printenv uname
SunOS
* JLP
* Version 12-10-2008
 *********************************************************/
#ifndef jlp_splot_idv_rename_ /* Beginning of sentry */
#define jlp_splot_idv_rename_

/* Concatenates name and extension: */
#ifdef ibm
#define RENAME(name) name
#define RENAME_(name) name
#elif defined(sun) || defined(dec)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#elif defined(linux) && !defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
/* Cambridge gcc and f90 in Solaris: */
/* Debian gcc and f90 in Merate*/
#elif defined(solaris) || defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#elif defined(Windows)
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
#else   /* Just in case JLP_SYSTEM is not defined */
// #pragma message('jlp_splot_idv_rename/WARNING: unknown operating system (-D$(JLP_SYSTEM) as a CFLAG): will go anyway')
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
#endif

/* Debian10 */
#define RENAME(name) name ## _
#define RENAME_(name) name ## _

/* splot_for.c */
#define JLP_SPBOX              RENAME_(jlp_spbox)
#define JLP_SPDEVICE_CURVE     RENAME_(jlp_spdevice_curve)
#define JLP_SPDEVICE_IMAGE     RENAME_(jlp_spdevice_image)
#define JLP_SPLABEL            RENAME_(jlp_splabel)

/* jlp_display2.c */
#define SPLOT_IMAGE            RENAME_(splot_image)
#define SPLOT_IMAGE2           RENAME_(splot_image2)
#define SPLOT_DEVICE2          RENAME_(splot_device2)

/* jlp_splot.cpp */
#define JLP_DEVICE_CURVE       RENAME_(jlp_device_curve)
#define JLP_DEVICE_IMAGE       RENAME_(jlp_device_image)
#define JLP_DRAW               RENAME_(jlp_draw)
#define JLP_RELOC              RENAME_(jlp_reloc)
#define JLP_WHERE              RENAME_(jlp_where)
#define JLP_LINE1              RENAME_(jlp_line1)
#define JLP_LINE1_BACKUP       RENAME_(jlp_line1_backup)
#define JLP_CIRCLE1            RENAME_(jlp_circle1)
#define JLP_SYMBOL_ERRORX1     RENAME_(jlp_symbol_errorx1)
#define JLP_SYMBOL_ERRORY1     RENAME_(jlp_symbol_errory1)
#define JLP_SYMBOL             RENAME_(jlp_symbol)
#define JLP_SYMBOL1            RENAME_(jlp_symbol1)
#define JLP_SYMBOL2            RENAME_(jlp_symbol2)
#define JLP_CURVE              RENAME_(jlp_curve)
#define JLP_CURVE_LINE         RENAME_(jlp_curve_line)
#define JLP_CURVE_HISTO        RENAME_(jlp_curve_histo)
#define JLP_SETPCOLOR          RENAME_(jlp_setpcolor)
#define JLP_SETLINEPARAM       RENAME_(jlp_setlineparam)
#define JLP_SPCLOSE            RENAME_(jlp_spclose)
#define JLP_GFLUSH             RENAME_(jlp_gflush)
#define JLP_PLOT_IMAGE         RENAME_(jlp_plot_image)
#define JLP_SETCOLOR           RENAME_(jlp_setcolor)
#define JLP_GET_2CIRCLES       RENAME_(jlp_get_2circles)
#define JLP_SET_NEW_LIMITS     RENAME_(jlp_set_new_limits)
#define JLP_GET_DATA_PLOT_PARAM RENAME_(jlp_get_data_plot_param)
#define JLP_GET_PLOT_PARAM     RENAME_(jlp_get_plot_param)
#define JLP_SET_PLOT_PARAM     RENAME_(jlp_set_plot_param)
#define JLP_ERASE_STATUS_BAR   RENAME_(jlp_erase_status_bar)
#define JLP_DRAW_TO_STATUS_BAR RENAME_(jlp_draw_to_status_bar)
#define CONV_USER_TO_MGO       RENAME_(conv_user_to_mgo)
#define CONV_MGO_TO_USER       RENAME_(conv_mgo_to_user)

/* jlp_newplot.cpp */
#define NEWPLOT                RENAME(newplot)
#define NEWPLOT2               RENAME(newplot2)
#define NEWPLOT2_HARDCOPY      RENAME_(newplot2_hardcopy)
#define NEWPLOT21              RENAME(newplot21)

#endif /* EOF sentry */
