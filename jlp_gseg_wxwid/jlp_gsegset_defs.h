/****************************************************************************
* Name: jlp_gsegset_defs.h
* 
* JLP
* Version 12/06/2017
****************************************************************************/
#ifndef _jlp_gsegset_defs__ 
#define _jlp_gsegset_defs__ 

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#include <math.h>

#ifndef ABS
#define ABS(a) ((a) < 0.0  ? (-(a)) : (a))
#endif

#ifndef SQUARE
#define SQUARE(a) ((a) * (a))
#endif

#ifndef PI
#define PI 3.14159265
#endif

#ifndef MINI
#define MINI(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAXI
#define MAXI(a,b) ((a) < (b) ? (b) : (a))
#endif

// Is not a number (should be defined in math.h ?):
inline bool jlp_isnan(double x) {
    return x != x;
}

//***************************************************************************
// Maximum number of X points (for spectra) 
#define NWAVE_MAX 16384 

// Maximum number of selections 
#define NSELECT_MAX 32 

// Maximum number of curves 
#define NCUR_MAX 4 

//***************** GSEG_PARAM and GSEG structures **************************
#define NBLK_MAX 20 
#define NINT_MAX 20

typedef struct {
// Type: 0=int 1=float 2=bool 3=wxString
int type;
int ivalue;
float fvalue;
bool bvalue;
wxString svalue;
wxString keywd;
} GSEG_PARAM;

typedef struct {
double bestfit;
double sigma_gauss;
int n_pix;
bool dust_or_agn;
wxString dust_and_agn;
double start_dust;
double step_dust;
int n_dust;
double start_agn;
double step_agn;
int n_agn;
double norm_wavel;
int wavel_unit;
bool std_dev;
int alpha_inf;
int alpha_sup;
int red_ext1;
int red_ext2;
wxString redlaw_fname;
double start_disp;
double step_disp;
int n_disp;
bool const_sigma;
int n_weightblk;
int int1_blk[NBLK_MAX];
int int2_blk[NBLK_MAX];
double weight_blk[NBLK_MAX];
double start_z;
double step_z;
int endz;
int endz0;
bool all_flux;
int n_drop_int;
int int1_drop[NINT_MAX];
int int2_drop[NINT_MAX];
bool full_spect;
bool use_solution;
wxString solution_fname;
double SNR;
wxString database_fname;
wxString database_dir;
wxString listgal_fname;
wxString results_fname;
} GSEG_SETTINGS;

#endif
