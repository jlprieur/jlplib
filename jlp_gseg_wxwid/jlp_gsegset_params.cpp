/****************************************************************************
* Name: jlp_gsegset_params.cpp 
* 
* JLP
* Version 11/06/2017
****************************************************************************/
#include "jlp_gsegset_params.h"

/********************************************************************
* Init GSEG_PARAM array from GSEG_SETTINGS structure 
* type: (0=int, 1=float, 2=bool 3=wxString)
* 
********************************************************************/
void Init_GSEG_PARAM_from_SETTINGS(GSEG_PARAM *GsegParam0, 
                                   GSEG_SETTINGS GsegSet1,
                                   int *nparams)
{
int i, k, kk;

// GsegSet0.sigma_gauss = 0;
GsegParam0[0].type = 1;
GsegParam0[0].fvalue = GsegSet1.sigma_gauss;
GsegParam0[0].keywd = _T("CONVOLVE");

// GsegSet0.n_pix = 1;
GsegParam0[1].type = 0;
GsegParam0[1].ivalue = GsegSet1.n_pix; 
GsegParam0[1].keywd = _T("NBREPIX");

// GsegSet0.dust_or_agn = "n";
GsegParam0[2].type = 2;
GsegParam0[2].bvalue = GsegSet1.dust_or_agn;
GsegParam0[2].keywd = _T("ADDAGNDUST");

// GsegSet0.dust_and_agn = "B";
GsegParam0[3].type = 3;
GsegParam0[3].svalue = GsegSet1.dust_and_agn;
GsegParam0[3].keywd = _T("DUSTAGN");

// GsegSet0.start_dust = 0.0;
GsegParam0[4].type = 1;
GsegParam0[4].fvalue = GsegSet1.start_dust; 
GsegParam0[4].keywd = _T("STARTDUST");

// GsegSet0.step_dust = 0.0;
GsegParam0[5].type = 1;
GsegParam0[5].fvalue = GsegSet1.step_dust; 
GsegParam0[5].keywd = _T("STEPDUST");

// GsegSet0.n_dust = 0;
GsegParam0[6].type = 0;
GsegParam0[6].ivalue = GsegSet1.n_dust = 0; 
GsegParam0[6].keywd = _T("NBREDUST");

// GsegSet0.start_agn = 0.;
GsegParam0[7].type = 1;
GsegParam0[7].fvalue = GsegSet1.start_agn; 
GsegParam0[7].keywd = _T("STARTAGN");

// GsegSet0.step_agn = 0.;
GsegParam0[8].type = 1;
GsegParam0[8].fvalue = GsegSet1.step_agn;
GsegParam0[8].keywd = _T("STEPAGN");

// GsegSet0.n_agn = 0;
GsegParam0[9].type = 0;
GsegParam0[9].ivalue = GsegSet1.n_agn;
GsegParam0[9].keywd = _T("NBREAGN");

// GsegSet0.norm_wavel = 5089.;
GsegParam0[10].type = 1;
GsegParam0[10].fvalue = GsegSet1.norm_wavel;
GsegParam0[10].keywd = _T("NORMWAV");

// GsegSet0.wavel_unit = 0;
GsegParam0[11].type = 0;
GsegParam0[11].ivalue = GsegSet1.wavel_unit;
GsegParam0[11].keywd = _T("WAVUNIT");

// GsegSet0.std_dev = false;
GsegParam0[12].type = 2;
GsegParam0[12].bvalue = GsegSet1.std_dev;
GsegParam0[12].keywd = _T("STD");

// GsegSet0.alpha_inf = 0;
GsegParam0[13].type = 0;
GsegParam0[13].ivalue = GsegSet1.alpha_inf;
GsegParam0[13].keywd = _T("ALPHAINF");

// GsegSet0.alpha_sup = 0;
GsegParam0[14].type = 0;
GsegParam0[14].ivalue = GsegSet1.alpha_sup;
GsegParam0[14].keywd = _T("ALPHASUP");

// GsegSet0.red_ext1 = 0;
GsegParam0[15].type = 0;
GsegParam0[15].ivalue = GsegSet1.red_ext1;
GsegParam0[15].keywd = _T("RED1");

// GsegSet0.red_ext2 = 30;
GsegParam0[16].type = 0;
GsegParam0[16].ivalue = GsegSet1.red_ext2;
GsegParam0[16].keywd = _T("RED2");

// GsegSet0.redlaw_fname = wxT("LoiHowarthMILESf.txt");
GsegParam0[17].type = 3;
GsegParam0[17].svalue = GsegSet1.redlaw_fname;
GsegParam0[17].keywd = _T("REDLAW");

// GsegSet0.start_disp = 0.;
GsegParam0[18].type = 1;
GsegParam0[18].fvalue = GsegSet1.start_disp;
GsegParam0[18].keywd = _T("STARTDISP");

// GsegSet0.step_disp = 0.;
GsegParam0[19].type = 1;
GsegParam0[19].fvalue = GsegSet1.step_disp;
GsegParam0[19].keywd = _T("STEPDISP");

// GsegSet0.n_disp = 0;
GsegParam0[20].type = 0;
GsegParam0[20].ivalue = GsegSet1.n_disp;
GsegParam0[20].keywd = _T("NBRDISP");

// GsegSet0.const_sigma = false;
GsegParam0[21].type = 2;
GsegParam0[21].bvalue = GsegSet1.const_sigma;
GsegParam0[21].keywd = _T("CONSTSIG");

// GsegSet0.start_z = 0.;
GsegParam0[22].type = 1;
GsegParam0[22].fvalue = GsegSet1.start_z;
GsegParam0[22].keywd = _T("STARTZ");

// GsegSet0.step_z = 0.;
GsegParam0[23].type = 1;
GsegParam0[23].fvalue = GsegSet1.step_z;
GsegParam0[23].keywd = _T("STEPZ");

// GsegSet0.endz0 = 0;
GsegParam0[24].type = 0;
GsegParam0[24].ivalue = GsegSet1.endz0;
GsegParam0[24].keywd = _T("ENDZ0");

// GsegSet0.endz = 0;
GsegParam0[25].type = 0;
GsegParam0[25].ivalue = GsegSet1.endz;
GsegParam0[25].keywd = _T("ENDZ");

// GsegSet0.all_flux = false;
GsegParam0[26].type = 2;
GsegParam0[26].bvalue = GsegSet1.all_flux;
GsegParam0[26].keywd = _T("ALLFLUX");

// GsegSet0.full_spect = false;
GsegParam0[27].type = 2;
GsegParam0[27].bvalue = GsegSet1.full_spect;
GsegParam0[27].keywd = _T("FULLSPEC");

// GsegSet0.use_solution = false;
GsegParam0[28].type = 2;
GsegParam0[28].bvalue = GsegSet1.use_solution;
GsegParam0[28].keywd = _T("USESOL");

// GsegSet0.solution_fname = wxT("");
GsegParam0[29].type = 3;
GsegParam0[29].svalue = GsegSet1.solution_fname;
GsegParam0[29].keywd = _T("SOLUTION");

// GsegSet0.SNR = 0;
GsegParam0[30].type = 1;
GsegParam0[30].fvalue = GsegSet1.SNR;
GsegParam0[30].keywd = _T("SNR");

// GsegSet0.database_fname = wxT("MILES_libnorm.txt");
GsegParam0[31].type = 3;
GsegParam0[31].svalue = GsegSet1.database_fname;
GsegParam0[31].keywd = _T("DATABASE");

// GsegSet0.database_dir = wxT(".");
GsegParam0[32].type = 3;
GsegParam0[32].svalue = GsegSet1.database_dir;
GsegParam0[32].keywd = _T("DATABASE_DIR");

// GsegSet0.listgal_fname = wxT("test.dat");
GsegParam0[33].type = 3;
GsegParam0[33].svalue = GsegSet1.listgal_fname;
GsegParam0[33].keywd = _T("LISTGAL");

// GsegSet0.listgal_fname = wxT("results.txt");
GsegParam0[34].type = 3;
GsegParam0[34].svalue = GsegSet1.results_fname;
GsegParam0[34].keywd = _T("RESULTS");

// GsegSet0.bestfit = 0.01;
GsegParam0[35].type = 1;
GsegParam0[35].fvalue = GsegSet1.bestfit;
GsegParam0[35].keywd = _T("BESTFIT");

//**********************************************
// GsegSet0.n_weightblk = 1;
GsegParam0[36].type = 0;
GsegParam0[36].ivalue = GsegSet1.n_weightblk;
GsegParam0[36].keywd = _T("WEIGTHBLCK");

kk = 36;
for(k = 1; k <= NBLK_MAX; k++) {

// "INT1BLCK1", "INT1BLCK2", "INT1BLCK3"
// GsegSet0.int1_blk[0] = 1;
kk++;
GsegParam0[kk].type = 0;
GsegParam0[kk].ivalue = GsegSet1.int1_blk[k-1];
GsegParam0[kk].keywd.Printf(wxT("INT1BLCK%d"), k);

// "INT2BLCK1", "INT2BLCK2", "INT2BLCK3"
// GsegSet0.int2_blk[0] = 1;
kk++;
GsegParam0[kk].type = 0;
GsegParam0[kk].ivalue = GsegSet1.int2_blk[k-1];
GsegParam0[kk].keywd.Printf(wxT("INT2BLCK%d"), k);

// "WEIGHT1", "WEIGHT2", "WEIGHT3" 
// GsegSet0.weight_blk[0] = 1.;
kk++;
GsegParam0[kk].type = 1;
GsegParam0[kk].fvalue = GsegSet1.weight_blk[k-1];
GsegParam0[kk].keywd.Printf(wxT("WEIGHT%d"), k);

}

//**********************************************
// GsegSet0.n_drop_int = 0;
kk = 37 + 3 * NBLK_MAX;
GsegParam0[kk].type = 0;
GsegParam0[kk].ivalue = GsegSet1.n_drop_int;
GsegParam0[kk].keywd = _T("DROPINT");

for(k = 1; k <= NINT_MAX; k++) {

// "INT1_1", "INT2_1", "INT3_1"
// GsegSet0.int1_drop[0] = 1;
kk++;
GsegParam0[kk].type = 0;
GsegParam0[kk].ivalue = GsegSet1.int1_drop[k - 1];
GsegParam0[kk].keywd.Printf(wxT("INT%d_1"), k);

// "INT1_2", "INT2_2", "INT3_2"
// GsegSet0.int2_drop[0] = 1;
kk++;
GsegParam0[kk].type = 0;
GsegParam0[kk].ivalue = GsegSet1.int2_drop[k - 1];
GsegParam0[kk].keywd.Printf(wxT("INT%d_2"), k);

}

*nparams = kk + 1;

return;
}
/********************************************************************
* Copy GSEG_PARAM array to GSEG_SETTINGS structure 
* 
********************************************************************/
void Copy_GSEG_PARAM_to_SETTINGS(GSEG_PARAM *GsegParam0, GSEG_SETTINGS &GsegSet1)
{
int k, kk;

GsegSet1.sigma_gauss    = GsegParam0[0].fvalue;
GsegSet1.n_pix          = GsegParam0[1].ivalue; 
GsegSet1.dust_or_agn    = GsegParam0[2].bvalue;
GsegSet1.dust_and_agn   = GsegParam0[3].svalue;
GsegSet1.start_dust     = GsegParam0[4].fvalue; 
GsegSet1.step_dust      = GsegParam0[5].fvalue; 
GsegSet1.n_dust         = GsegParam0[6].ivalue; 
GsegSet1.start_agn      = GsegParam0[7].fvalue; 
GsegSet1.step_agn       = GsegParam0[8].fvalue;
GsegSet1.n_agn          = GsegParam0[9].ivalue;
GsegSet1.norm_wavel     = GsegParam0[10].fvalue;
GsegSet1.wavel_unit     = GsegParam0[11].ivalue;
GsegSet1.std_dev        = GsegParam0[12].bvalue;
GsegSet1.alpha_inf      = GsegParam0[13].ivalue;
GsegSet1.alpha_sup      = GsegParam0[14].ivalue;
GsegSet1.red_ext1       = GsegParam0[15].ivalue;
GsegSet1.red_ext2       = GsegParam0[16].ivalue;
GsegSet1.redlaw_fname   = GsegParam0[17].svalue;
GsegSet1.start_disp     = GsegParam0[18].fvalue;
GsegSet1.step_disp      = GsegParam0[19].fvalue;
GsegSet1.n_disp         = GsegParam0[20].ivalue;
GsegSet1.const_sigma    = GsegParam0[21].bvalue;
GsegSet1.start_z        = GsegParam0[22].fvalue;
GsegSet1.step_z         = GsegParam0[23].fvalue;
GsegSet1.endz0          = GsegParam0[24].ivalue;
GsegSet1.endz           = GsegParam0[25].ivalue;
GsegSet1.all_flux       = GsegParam0[26].bvalue;
GsegSet1.full_spect     = GsegParam0[27].bvalue;
GsegSet1.use_solution   = GsegParam0[28].bvalue;
GsegSet1.solution_fname = GsegParam0[29].svalue;
GsegSet1.SNR            = GsegParam0[30].fvalue;
GsegSet1.database_fname = GsegParam0[31].svalue;
GsegSet1.database_dir   = GsegParam0[32].svalue;
GsegSet1.listgal_fname  = GsegParam0[33].svalue;
GsegSet1.results_fname  = GsegParam0[34].svalue;
GsegSet1.bestfit        = GsegParam0[35].fvalue;

GsegSet1.n_weightblk    = GsegParam0[36].ivalue;
kk = 36;

for(k = 0; k < NBLK_MAX; k++) {
  kk++;
  GsegSet1.int1_blk[k] = GsegParam0[kk].ivalue;
  kk++;
  GsegSet1.int2_blk[k] = GsegParam0[kk].ivalue;
  kk++;
  GsegSet1.weight_blk[k] = GsegParam0[kk].fvalue;
  }

kk++;
GsegSet1.n_drop_int     = GsegParam0[kk].ivalue;

for(k = 0; k < NINT_MAX; k++) {
  kk++;
  GsegSet1.int1_drop[k]   = GsegParam0[kk].ivalue;
  GsegSet1.int2_drop[k]   = GsegParam0[kk].ivalue;
  }

return;
}
/********************************************************************
* Init GSEG_SETTINGS_ structure with defaults values 
********************************************************************/
void Init_GSEG_SETTINGS_with_default_values(GSEG_SETTINGS &GsegSet0)
{
int i;

GsegSet0.sigma_gauss = 0.;
GsegSet0.n_pix = 1;
GsegSet0.dust_or_agn = false;
GsegSet0.dust_and_agn = 'B';
GsegSet0.start_dust = 0.;
GsegSet0.step_dust = 0.;
GsegSet0.n_dust = 0;
GsegSet0.start_agn = 0.;
GsegSet0.step_agn = 0.;
GsegSet0.n_agn = 0;
GsegSet0.norm_wavel = 5089.;
GsegSet0.wavel_unit = 0;
GsegSet0.std_dev = false;
GsegSet0.alpha_inf = 0;
GsegSet0.alpha_sup = 0;
GsegSet0.red_ext1 = 0;
GsegSet0.red_ext2 = 30;
GsegSet0.redlaw_fname = wxT("LoiHowarthMILESf.txt");
GsegSet0.start_disp = 0.;
GsegSet0.step_disp = 0.;
GsegSet0.n_disp = 0;
GsegSet0.const_sigma = false;
GsegSet0.n_weightblk = 0;

for(i = 0; i < NBLK_MAX; i++) {
  GsegSet0.int1_blk[i] = 1;
  GsegSet0.int2_blk[i] = 1;
  GsegSet0.weight_blk[i] = 1.0;
  }

GsegSet0.start_z = 0.;
GsegSet0.step_z = 0.;
GsegSet0.endz0 = 0;
GsegSet0.endz = 0;
GsegSet0.all_flux = false;
GsegSet0.n_drop_int = 0;

for(i = 0; i < NINT_MAX; i++) {
  GsegSet0.int1_drop[i] = 1;
  GsegSet0.int2_drop[i] = 1;
  }

GsegSet0.full_spect = false;
GsegSet0.use_solution = false;
GsegSet0.solution_fname = wxT("");
GsegSet0.SNR = 0;
GsegSet0.database_fname = wxT("MILES_libnorm.txt");
GsegSet0.database_dir = wxT(".");
GsegSet0.listgal_fname = wxT("test.dat");
GsegSet0.results_fname = wxT("results.txt");
GsegSet0.bestfit = 0.01;

return;
}
/********************************************************************
* Copy GSEG_SETTINGS structure 
********************************************************************/
void Copy_GSEG_SETTINGS(GSEG_SETTINGS &GsegSet0, GSEG_SETTINGS GsegSet2)
{
int i;

GsegSet0.sigma_gauss = GsegSet2.sigma_gauss;
GsegSet0.n_pix = GsegSet2.n_pix;
GsegSet0.dust_or_agn = GsegSet2.dust_or_agn; 
GsegSet0.dust_and_agn = GsegSet2.dust_and_agn; 
GsegSet0.start_dust = GsegSet2.start_dust;
GsegSet0.step_dust = GsegSet2.step_dust;
GsegSet0.n_dust = GsegSet2.n_dust;
GsegSet0.start_agn = GsegSet2.start_agn;
GsegSet0.step_agn = GsegSet2.step_agn;
GsegSet0.n_agn = GsegSet2.n_agn;
GsegSet0.norm_wavel = GsegSet2.norm_wavel;
GsegSet0.wavel_unit = GsegSet2.wavel_unit;
GsegSet0.std_dev = GsegSet2.std_dev;
GsegSet0.alpha_inf = GsegSet2.alpha_inf;
GsegSet0.alpha_sup = GsegSet2.alpha_sup;
GsegSet0.red_ext1 = GsegSet2.red_ext1;
GsegSet0.red_ext2 = GsegSet2.red_ext2;
GsegSet0.redlaw_fname = GsegSet2.redlaw_fname;
GsegSet0.start_disp = GsegSet2.start_disp;
GsegSet0.step_disp = GsegSet2.step_disp;
GsegSet0.n_disp = GsegSet2.n_disp;
GsegSet0.const_sigma = GsegSet2.const_sigma;
GsegSet0.n_weightblk = GsegSet2.n_weightblk;

for(i = 0; i < NBLK_MAX; i++) {
  GsegSet0.int1_blk[i] = GsegSet2.int1_blk[i];
  GsegSet0.int2_blk[i] = GsegSet2.int2_blk[i];
  GsegSet0.weight_blk[i] = GsegSet2.weight_blk[i];
  }

GsegSet0.start_z = GsegSet2.start_z;
GsegSet0.step_z = GsegSet2.step_z;
GsegSet0.endz0 = GsegSet2.endz0;
GsegSet0.endz = GsegSet2.endz;
GsegSet0.all_flux = GsegSet2.all_flux;
GsegSet0.n_drop_int = GsegSet2.n_drop_int;

for(i = 0; i < NINT_MAX; i++) {
  GsegSet0.int1_drop[i] = GsegSet2.int1_drop[i];
  GsegSet0.int2_drop[i] = GsegSet2.int2_drop[i];
  }

GsegSet0.full_spect = GsegSet2.full_spect;
GsegSet0.use_solution = GsegSet2.use_solution;
GsegSet0.solution_fname = GsegSet2.solution_fname;
GsegSet0.SNR = GsegSet2.SNR;
GsegSet0.database_fname = GsegSet2.database_fname;
GsegSet0.database_dir = GsegSet2.database_dir;
GsegSet0.listgal_fname = GsegSet2.listgal_fname;
GsegSet0.results_fname = GsegSet2.results_fname;
GsegSet0.bestfit = GsegSet2.bestfit;

return;
}
