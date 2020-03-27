/********************************************************
* "jlp_plotlib_rename.h"
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
#ifndef jlp_fitslib_rename_included_ /* Beginning of sentry */
#define jlp_fitslib_rename_included_

/* Concatenates name and extension: */
/*********************************
#ifdef ibm
#define RENAME(name) name
#define RENAME_(name) name
#elif defined(sun) || defined(dec)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#elif defined(linux) && !defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
// Cambridge gcc and f90 in Solaris: 
// Debian gcc and f90 in Merate
#elif defined(solaris) || defined(gnu_f90)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#else   // Just in case JLP_SYSTEM is not defined
// #pragma message("WARNING: JLP_SYSTEM not defined: assuming linux !")
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
#endif
*****************************/

/* Debian10 */
#define RENAME(name) name ## _
#define RENAME_(name) name ## _

/* jlp0_rdfits.c and jlp0_wrfits.c */
#define jlp0_dble_rdfits       RENAME_(jlp0_dble_rdfits)
#define JLP_WRFITS             RENAME_(jlp_wrfits)
#define JLP_RDFITS             RENAME_(jlp_rdfits)
#define JLP_VM_RDFITS          RENAME_(jlp_vm_rdfits)
#define JLP_VM_RDFITS_3D       RENAME_(jlp_vm_rdfits_3d)

/* jlp_rdfitsio.cpp */
#define JLP_RDFITS_2D_dble           RENAME_(jlp_rdfits_2d_dble)
#define JLP_RDFITS_2D_flt            RENAME_(jlp_rdfits_2d_flt)
#define JLP_RDFITS_2D_dble           RENAME_(jlp_rdfits_2d_dble)
#define JLP_RDFITS_2D_dble_descr     RENAME_(jlp_rdfits_2d_dble_descr)
#define JLP_RDFITS_3D_dble           RENAME_(jlp_rdfits_3d_dble)

#endif /* EOF sentry */
