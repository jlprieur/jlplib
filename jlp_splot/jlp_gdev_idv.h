/**********************************************************************
* "jlp_gdev_idv.h" 
* Prototypes of functions included in jlp_gdev_idv.cpp
* and in jlp_gdev_access.cpp
* JLP
* Version 04/02/2017
**********************************************************************/
#include "jlp_gdev.h"         // JLP_GDev Graphic Device class

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

#ifdef __cplusplus
extern "C" {
#endif

// in jlp_gdev_idv.cpp)
int GDev_alloc_idv(JLP_GDev *Jgd0, int *idv, char *err_messg);
JLP_GDev *GDev_from_idv(int idv);
int GDev_free_idv(int idv);

// Accessors: (in jlp_gdev_access.cpp)
int Jgdev_ncolors(int idv);
int Jgdev_lut(int i, int idv);
int Jgdev_TeX(int idv);
int Jgdev_dev_type(int idv);
int Jgdev_dev_width(int idv);
int Jgdev_dev_height(int idv);
int Jgdev_yorigin_is_on_top(int idv);
int Jgdev_offx(int idv);
int Jgdev_offy(int idv);
int Jgdev_axlen(int idv);
int Jgdev_aylen(int idv);
int Jgdev_lwidth(int idv);
int Jgdev_ltype(int idv);
int Jgdev_box_plan(int idv);
double Jgdev_box_xmin(int idv);
double Jgdev_box_xmax(int idv);
double Jgdev_box_ymin(int idv);
double Jgdev_box_ymax(int idv);
double Jgdev_xmin_user(int idv);
double Jgdev_xmax_user(int idv);
double Jgdev_ymin_user(int idv);
double Jgdev_ymax_user(int idv);
double Jgdev_zmin_user(int idv);
double Jgdev_zmax_user(int idv);
double Jgdev_fsx(int idv);
double Jgdev_fsy(int idv);
double Jgdev_cheight(int idv);
double Jgdev_cwidth(int idv);
FILE *Jgdev_fp_backup(int idv);
int Jgdev_Set_ltype_and_lwidth(int ltype, int lwidth, int idv);
int Jgdev_Set_gamma_d(int gamma_d, int idv);

#ifdef __cplusplus
}
#endif

