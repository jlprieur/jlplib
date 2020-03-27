/******************************************************************
* "jlp_gdev_acces.cpp" 
*  Accessors to private variables of JLP_GDev class
*
* JLP
* Version 10/02/2015
*************************************************************/
#include "jlp_splot_idv.h"
#include "jlp_gdev_idv.h"

/*******************************************************************
*
* Accessors to JLP_GDevice internal values
*******************************************************************/
int Jgdev_ncolors(int idv) { 
int ncolors = 0;
  if(GDev_from_idv(idv) != NULL) ncolors = GDev_from_idv(idv)->Jgc0_ncolors(); 
  else {fprintf(stderr,"Fatal error in Jgdev_ncolors\n"); exit(-1);}
return ncolors;
} 
int Jgdev_lut(int i, int idv) { 
int ilut = 0;
  if(GDev_from_idv(idv) != NULL) ilut = GDev_from_idv(idv)->Jgc0_lut(i); 
  else {fprintf(stderr,"Fatal error in Jgdev_lut\n"); exit(-1);}
return ilut;
} 
int Jgdev_TeX_flag(int idv) {
int TeX_flag = 0;
  if(GDev_from_idv(idv) != NULL) TeX_flag = GDev_from_idv(idv)->Jgc0_TeX_flag();
  else {fprintf(stderr,"Fatal error in Jgdev_TeX_flag\n"); exit(-1);}
return TeX_flag;
}
int Jgdev_dev_type(int idv) {
int dev_type = 1;
  if(GDev_from_idv(idv) != NULL) dev_type = GDev_from_idv(idv)->Jgc0_dev_type();
  else {fprintf(stderr,"Fatal error in Jgdev_dev_type\n"); exit(-1);}
return dev_type;
}
int Jgdev_offx(int idv) { 
int offx = 0;
  if(GDev_from_idv(idv) != NULL) offx = GDev_from_idv(idv)->Jgc0_offx(); 
  else {fprintf(stderr,"Fatal error in Jgdev_offx\n"); exit(-1);}
return offx;
} 
int Jgdev_offy(int idv) { 
int offy = 0;
  if(GDev_from_idv(idv) != NULL) offy = GDev_from_idv(idv)->Jgc0_offy(); 
  else {fprintf(stderr,"Fatal error in Jgdev_offy\n"); exit(-1);}
return offy;
} 
int Jgdev_axlen(int idv) { 
int axlen = 0;
  if(GDev_from_idv(idv) != NULL) axlen = GDev_from_idv(idv)->Jgc0_axlen(); 
  else {fprintf(stderr,"Fatal error in Jgdev_axlen\n"); exit(-1);}
return axlen;
} 
int Jgdev_aylen(int idv) { 
int aylen = 0;
  if(GDev_from_idv(idv) != NULL) aylen = GDev_from_idv(idv)->Jgc0_aylen(); 
  else {fprintf(stderr,"Fatal error in Jgdev_aylen\n"); exit(-1);}
return aylen;
} 
int Jgdev_dev_width(int idv) { 
int dev_width = 0;
  if(GDev_from_idv(idv) != NULL) dev_width = GDev_from_idv(idv)->Jgc0_dev_width(); 
  else {fprintf(stderr,"Fatal error in Jgdev_dev_width\n"); exit(-1);}
return dev_width;
} 
int Jgdev_dev_height(int idv) { 
int dev_height = 0;
  if(GDev_from_idv(idv) != NULL) dev_height = GDev_from_idv(idv)->Jgc0_dev_height(); 
  else {fprintf(stderr,"Fatal error in Jgdev_dev_height\n"); exit(-1);}
return dev_height;
} 
int Jgdev_box_plan(int idv) { 
int box_plan = 0;
  if(GDev_from_idv(idv) != NULL) box_plan = GDev_from_idv(idv)->Jgc0_box_plan(); 
  else {fprintf(stderr,"Fatal error in Jgdev_box_plan\n"); exit(-1);}
return box_plan;
} 
double Jgdev_box_xmin(int idv) { 
double box_xmin = 0;
  if(GDev_from_idv(idv) != NULL) box_xmin = GDev_from_idv(idv)->Jgc0_box_xmin(); 
  else {fprintf(stderr,"Fatal error in Jgdev_box_xmin\n"); exit(-1);}
return box_xmin;
} 
double Jgdev_box_xmax(int idv) { 
double box_xmax = 0;
  if(GDev_from_idv(idv) != NULL) box_xmax = GDev_from_idv(idv)->Jgc0_box_xmax(); 
  else {fprintf(stderr,"Fatal error in Jgdev_box_xmaxn\n"); exit(-1);}
return box_xmax;
} 
double Jgdev_box_ymin(int idv) { 
double box_ymin = 0;
  if(GDev_from_idv(idv) != NULL) box_ymin = GDev_from_idv(idv)->Jgc0_box_ymin(); 
  else {fprintf(stderr,"Fatal error in Jgdev_box_ymin\n"); exit(-1);}
return box_ymin;
} 
double Jgdev_box_ymax(int idv) { 
double box_ymax = 0;
  if(GDev_from_idv(idv) != NULL) box_ymax = GDev_from_idv(idv)->Jgc0_box_ymax(); 
  else {fprintf(stderr,"Fatal error in Jgdev_box_ymaxn\n"); exit(-1);}
return box_ymax;
} 
double Jgdev_xmin_user(int idv) { 
double xmin_user = 0;
  if(GDev_from_idv(idv) != NULL) xmin_user = GDev_from_idv(idv)->Jgc0_xmin_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_xmin_user\n"); exit(-1);}
return xmin_user;
} 
double Jgdev_xmax_user(int idv) { 
double xmax_user = 0;
  if(GDev_from_idv(idv) != NULL) xmax_user = GDev_from_idv(idv)->Jgc0_xmax_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_xmax_user\n"); exit(-1);}
return xmax_user;
} 
double Jgdev_ymin_user(int idv) { 
double ymin_user = 0;
  if(GDev_from_idv(idv) != NULL) ymin_user = GDev_from_idv(idv)->Jgc0_ymin_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_ymin_user\n"); exit(-1);}
return ymin_user;
} 
double Jgdev_ymax_user(int idv) { 
double ymax_user = 0;
  if(GDev_from_idv(idv) != NULL) ymax_user = GDev_from_idv(idv)->Jgc0_ymax_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_ymax_user\n"); exit(-1);}
return ymax_user;
} 
double Jgdev_zmin_user(int idv) { 
double zmin_user = 0;
  if(GDev_from_idv(idv) != NULL) zmin_user = GDev_from_idv(idv)->Jgc0_zmin_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_zmin_user\n"); exit(-1);}
return zmin_user;
} 
double Jgdev_zmax_user(int idv) { 
double zmax_user = 0;
  if(GDev_from_idv(idv) != NULL) zmax_user = GDev_from_idv(idv)->Jgc0_zmax_user(); 
  else {fprintf(stderr,"Fatal error in Jgdev_zmax_user\n"); exit(-1);}
return zmax_user;
} 
double Jgdev_fsx(int idv) { 
double fsx = 0;
  if(GDev_from_idv(idv) != NULL) fsx = GDev_from_idv(idv)->Jgc0_fsx(); 
  else {fprintf(stderr,"Fatal error in Jgdev_fsx\n"); exit(-1);}
return fsx;
} 
double Jgdev_fsy(int idv) { 
double fsy = 0;
  if(GDev_from_idv(idv) != NULL) fsy = GDev_from_idv(idv)->Jgc0_fsy(); 
  else {fprintf(stderr,"Fatal error in Jgdev_fsy\n"); exit(-1);}
return fsy;
} 
double Jgdev_cheight(int idv) { 
double cheight = 0;
  if(GDev_from_idv(idv) != NULL) cheight = GDev_from_idv(idv)->Jgc0_cheight(); 
  else {fprintf(stderr,"Fatal error in Jgdev_cheight\n"); exit(-1);}
return cheight;
} 
double Jgdev_cwidth(int idv) { 
double cwidth = 0;
  if(GDev_from_idv(idv) != NULL) cwidth = GDev_from_idv(idv)->Jgc0_cwidth(); 
  else {fprintf(stderr,"Fatal error in Jgdev_cwidth\n"); exit(-1);}
return cwidth;
} 
FILE *Jgdev_fp_backup(int idv) { 
FILE* fpb = NULL;
  if(GDev_from_idv(idv) != NULL) fpb = GDev_from_idv(idv)->Jgc0_fp_backup(); 
  else {fprintf(stderr,"Fatal error in Jgdev_fp_backup\n"); exit(-1);}
return fpb;
} 
// Line type (0=solid, 1=dashed, 2=dash-dot, etc)
int Jgdev_ltype(int idv) {
int ltype = 0;
  if(GDev_from_idv(idv) != NULL) ltype = GDev_from_idv(idv)->Jgc0_ltype();
return ltype;
}
// Line weight (thickness of lines)
int Jgdev_lwidth(int idv) {
int lwidth = 0;
  if(GDev_from_idv(idv) != NULL) lwidth = GDev_from_idv(idv)->Jgc0_lwidth();
return lwidth;
}
/**********************************************************************
* Possibility of setting some parameters:
**********************************************************************/
// Line type (0=solid, 1=dashed, 2=dash-dot, etc)
// Line weight (thickness of lines)
int Jgdev_Set_ltype_and_lwidth(int ltype, int lwidth, int idv) {
int status = -1;
  if(GDev_from_idv(idv) != NULL) status = GDev_from_idv(idv)->Set_ltype_and_lwidth(ltype, lwidth);
return status;
}
