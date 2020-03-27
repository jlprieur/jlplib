/******************************************************************************
* Name:        JLP_wxImage1 class
* Purpose:     Structure used for displaying an image with wxImage
* Author:      JLP
* Version:     19/10/2016
******************************************************************************/
#ifndef jlp_wx_image1_h    // sentry
#define jlp_wx_image1_h

#include "wx/image.h"
#include "wx/file.h"
#include "wx/mstream.h"
#include "wx/wfstream.h"
#include "wx/quantize.h"

// For clipboard:
#if wxUSE_CLIPBOARD
    #include "wx/dataobj.h"
    #include "wx/clipbrd.h"
#endif // wxUSE_CLIPBOARD

#include "jlp_macros.h"       // To define MAXI, MINI, etc

// Class JLP_wxImage1:
class JLP_wxImage1
{
public:
// Constructor with no initial data (necessary for declaration with no parameters)
    JLP_wxImage1();

// Constructor from wxImage:
    JLP_wxImage1(wxImage image1);

// Constructor from double array:
    JLP_wxImage1(double *dble_image, int nx, int ny, wxSize window_size,
                 const int should_fit_inside, int max_lut_level);

// Destructor:
    ~JLP_wxImage1(){
       MyFreeMemory();
    };

// Defined in jlp_wx_image1.cpp:
    void Init_PrivateParameters(const int nx10, const int ny10,
                                const wxSize window_size0,
                                const int should_fit_inside0);
    int JLP_SetupFromVoid();
    int JLP_SetupFromDouble(double *dble_image, int nx, int ny,
                            const wxSize window_size,
                            const int should_fit_inside,
                            const int max_lut_level);
    void Update_with_LUT_and_ITT(int max_lut_level);
    void Update_with_ITT();
    void Compute_gamma_for_window(wxSize mini_bitmap_size);
    void ComputeBitmapSize(int zoom_fact);
    void UpdateBitmap();
    void GetInfo(wxString& InfoMessage);
    void RestoreOriginalImage() {
      register int i;
      for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = orig_dble_image1[i];
      }
    void SetZoom(int zoom_fact);
    void MyFreeMemory();

/*******************************************************************
* Copy private data to newly created array
*******************************************************************/
int GetDoubleArray(double **array0, int *nx0, int *ny0)
{
 int i, status = -1;
 *nx0 = 0;
 *ny0 = 0;
 *array0 = NULL;
 if(dble_image1 != NULL) {
    *array0 = new double[nx1 * ny1];
    *nx0 = nx1;
    *ny0 = ny1;
    for(i = 0; i < nx1 * ny1; i++) (*array0)[i] = dble_image1[i];
    status = 0;
    }
 return(status);
}
/*******************************************************************
* Copy data array to private array
*******************************************************************/
int PutDoubleArray(double *array0, int nx0, int ny0)
{
 int i;

 if((array0 == NULL) ||( nx0 != nx1) || (ny0 != ny1)) return(-1);

 for(i = 0; i < nx1 * ny1; i++) dble_image1[i] = array0[i];

 return(0);
}
//********************************************************************
// Accessors:
//********************************************************************
    wxBitmap* GetBitmap2(){return m_bitmap2;}

    int Get_nx1(){return nx1;}
    int Get_ny1(){return ny1;}
    int Get_nx2(){return nx2;}
    int Get_ny2(){return ny2;}
    int Get_gamma(int *gamma1_0, int *gamma_d_0)
      {*gamma1_0 = gamma1; *gamma_d_0 = gamma_d; return(0);}
    void Get_ITT_type(wxString &itt_type){itt_type = ITT_type; return;}
    int ITT_Is_Linear(){if(ITT_is_linear) return 1; else return 0;}
    void Get_LUT_type(char *lut_type){strcpy(lut_type, lut_type1); return;}
    void Get_LUT_param(int &lut_ioff, int &lut_islope, int &lut_reversed){
       lut_ioff = LUT_ioff;
       lut_islope = LUT_islope;
       lut_reversed = LUT_reversed;
       return;
       }
    int Get_LUT_reversed(){return LUT_reversed;}
    double GetValue(int ix, int iy){
      double value = 0.;
      if(ix >= 0 && ix < nx1 && iy >= 0 && iy < ny1)
            value = dble_image1[ix + iy*nx1];
      return value;
      }
/************************************************************************
* Change the ITT,  Intensity Transfer Table used for the display
*
* This table is filled with the indices k to be used for LUT conversion
* of a double precision image
************************************************************************/
  int GetITT_Thresh(wxString *itt_type0, double *low_thresh0, double *up_thresh0,
                      int *x1_box0, int *y1_box0, int *x2_box0, int *y2_box0)
  {
// Compute new thresholds if "FromBox" ITT type
      Update_with_ITT();
// Load update values:
      *itt_type0 = ITT_type;
      *low_thresh0 = Low_Threshold;
      *up_thresh0 = Up_Threshold;
      *x1_box0 = x1_box;
      *y1_box0 = y1_box;
      *x2_box0 = x2_box;
      *y2_box0 = y2_box;
      return(0);
  }
/************************************************************************
* Threshold determination
* ITT_type:
* "FromBox" "DirectInput" "MinMax" "HC" (high contrast)
* or "VHC" (very high contrast)
************************************************************************/
    int SetITT_thresh(const wxString itt_type00, const double low_thresh00,
                      const double up_thresh00, const double x1,
                      const double y1, const double x2, const double y2)
    {
    wxString itt_type0;
    double low_thresh0, up_thresh0, status = 0;

      itt_type0 = itt_type00;
      low_thresh0 = low_thresh00;
      up_thresh0 = up_thresh00;
      x1_box = x1;
      y1_box = y1;
      x2_box = x2;
      y2_box = y2;


// Handle error case:
      if(!itt_type0.Cmp(wxT("DirectInput"))) {
        if(low_thresh0 == up_thresh0) {
          itt_type0 = wxT("MinMax");
          status = -1;
        }
      }

      if(!itt_type0.Cmp(wxT("MinMax"))
         || !itt_type0.Cmp(wxT("HC"))
         || !itt_type0.Cmp(wxT("VHC"))) {
         Low_Threshold = 0.;
         Up_Threshold = 0.;
         x1_box = 0.;
         y1_box = 0.;
         x2_box = 0.;
         y2_box = 0.;
         ITT_type = itt_type0;
      } else {
         Low_Threshold = low_thresh0;
         Up_Threshold = up_thresh0;
         ITT_type = itt_type0;
      }

// Compute new thresholds if "FromBox" ITT type
      Update_with_ITT();

    return(status);
    }

// Set ITT type
    void SetITT_type(wxString itt_type0){
        ITT_type = itt_type0;
        Update_with_ITT();
        }

// Set linear mode: 1=Linear 0=Log
    void SetITT_linear(int itt_linear0){
        ITT_is_linear = itt_linear0;
        Update_with_ITT();
        }

/************************************************************************
* Change LUT Rainbow1
* lut_type: 'l'=log rainbow1 'r'=rainbow2 's'=saw 'g'=gray 'c'=curves
*           'p'=pisco
************************************************************************/
    void SetLUT(char *new_lut_type, int max_lut_level){
        strcpy(lut_type1, new_lut_type);
        Update_with_LUT_and_ITT(max_lut_level);
        }

    void ReverseLUT(int max_lut_level){
        if(LUT_reversed)
          LUT_reversed = 0;
        else
          LUT_reversed = 1;
        Update_with_LUT_and_ITT(max_lut_level);
        }

private:
// Index 1 is used for data in original size
// Index 2 is used for rescaled data (ready for display)
  wxImage *m_image1, *work_image;
  double *dble_image1, *orig_dble_image1;
  int nx1, ny1, nx2, ny2, gamma_d, gamma1;
  wxBitmap *m_bitmap2;
  wxSize window_size;
  BYTE *image1_rgb;
  int *LUT_r, *LUT_g, *LUT_b;
  char lut_type1[32];
  int LUT_ioff, LUT_islope, LUT_reversed;
  int ITT_is_linear, should_fit_inside1;
  double Low_Threshold, Up_Threshold;
  double x1_box, y1_box, x2_box, y2_box;
  wxString ITT_type;
// In wxWidgets, the dimension of the color palettes is always 256:
  static const int NColors = 256;
};

#endif               // EOF sentry
