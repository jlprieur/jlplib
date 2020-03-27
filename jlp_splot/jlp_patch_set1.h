/***************************************************************************
* Set of the subroutines for "patching" images 
* contained in "jlp_patch_set1.cpp" 
*
* JLP
* Version 26/07/2013
***************************************************************************/
int POLY_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                    double xp, double yp, double diam, double diam_factor, 
                    double *noise_array, int noise_dim, double sigma_noise, 
                    int poly_order, double *sigma_sky, char *err_message);
int PROFILE_CIRC_PATCH(double *image1, int nx1, int ny1, int idim1,
                       double xp, double yp, double diam, double diam_factor, 
                       double *noise_array, int noise_dim, double sigma_noise, 
                       double *sigma_sky, char *err_message);
int CREATE_NOISE_ARRAY(double **noise_array, int noise_dim);
int DELETE_NOISE_ARRAY(double *noise_array);
