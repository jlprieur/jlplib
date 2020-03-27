/******************************************************************************
* jlp_GsegData_Histogram.c
*
* Compute histogram of input data.
*
* JLP
* Version 15/04/2017
******************************************************************************/
#include <math.h>
#include "jlp_gseg_data.h"           // JLP_GsegData class
#include "jlp_gsegraf_defs.h"

/*******************************************************************
*
* INPUT:
*  iplot : plot index (from 1 to number of plots)
*
* OUTPUT:
*  yhist0[nbins] : histogram
*  nbins : number of bins
*  npts : total number of points 
*  binmin, binmax : smallest and largest bin value
*  binwidth : bin width 
*  histo_type0 : histogram type ("number", "fraction", or "percent")
**********************************************************************/
void JLP_GsegData::ComputeHistogram(int iplot, double **yhist0,
                                    int *nbins, int *npts, double *binmin,
                                    double *binmax, double *binwidth,
                                    char *histo_type0)
{
/* Declare variables */
int i, j, n1, n2;
double data_min, data_max, sum, sumsq, mean, std, q[5], bin1, bin2;

/* Calculate data mean and standard deviation */
 sum = 0.0;
 *npts = gseg_plotdata1[iplot].npts;
 for ( i = 1; i <= *npts; i++ )
    sum = sum + gseg_plotdata1[iplot].xdata[i-1];
 mean = sum / (*npts);

 sumsq = 0.0;
 for ( i = 1; i <= (*npts); i++ )
    sumsq = sumsq + (gseg_plotdata1[iplot].xdata[i-1] - mean) 
                    * (gseg_plotdata1[iplot].xdata[i-1] - mean);
 std = sqrt(sumsq/((*npts) - 1.0));


/* Calculate data quartile values */
 data_min = gseg_plotdata1[iplot].xdata[0];
 data_max = gseg_plotdata1[iplot].xdata[(*npts) - 1];
 q[0] = data_min;
 q[4] = data_max;
 for ( i = 1; i <= 3; i++ )
    {
    j = roundint(i * 0.25 * (*npts));
    q[i] = gseg_plotdata1[iplot].xdata[j-1];
    }


/* Estimate optimal histogram bin width */
 if ( gseg_plotdata1[iplot].bin_width <= 0.0 )
    {
/* Scott: 
    binwidth = 3.49*std/pow((double) (*npts), 1.0/3.0); 
*/   
/* Freedman, Diaconis: */
    *binwidth = 2.0*(q[3] - q[1])/pow((double) (*npts), 1.0/3.0);   
    gseg_plotdata1[iplot].bin_width = *binwidth;
    }
 else
    *binwidth = gseg_plotdata1[iplot].bin_width;


/* Calculate number of bins, binmin, and binmax */
 if ( strcmp(gseg_plotdata1[iplot].bin_ref, "mean") == 0 )
    {
    n1 = ceil(((mean - (*binwidth)/2.0) - data_min)/ (*binwidth));
    n2 = ceil((data_max - (mean + (*binwidth)/2.0))/ (*binwidth));
    *nbins = n1 + n2 + 1;
    *binmin = (mean - (*binwidth)/2.0) - n1 * (*binwidth);
    *binmax = (mean + (*binwidth)/2.0) + n2 * (*binwidth);
    }
 else if ( strcmp(gseg_plotdata1[iplot].bin_ref, "zero") == 0 )
    {
    if ( data_max > 0 )
       {
       *nbins = ceil(data_max/(*binwidth));
       *binmin = 0.0;
       *binmax = (*nbins) * (*binwidth);
       }
    else
       return;
    }
 else if ( strcmp(gseg_plotdata1[iplot].bin_ref, "integers") == 0 )
    {
    *binwidth = 1.0;
    *nbins = roundint(data_max) - roundint(data_min) + 1.0;
    *binmin = (double) roundint(data_min) - 0.5;
    *binmax = (double) roundint(data_max) + 0.5;
    }


/* Calculate number of samples in each bin */
*yhist0 = new double[(*nbins)];
for ( i = 1; i <= (*nbins); i++ )
   (*yhist0)[i-1] = 0.0;
j = 1;
for ( i = 1; i <= (*nbins); i++ )
   {
   bin1 = (*binmin) 
        + (i - 1.0) / ((*nbins) - 1.0) * ((*binmax) - (*binmin) - (*binwidth));
   bin2 = bin1 + (*binwidth);
   while ( (j <= (*npts)) && (bin1 <= gseg_plotdata1[iplot].xdata[j-1]) 
           && (gseg_plotdata1[iplot].xdata[j-1] < bin2) )
      {
      (*yhist0)[i-1] = (*yhist0)[i-1] + 1.0;
      j++;
      }
   }
if ( gseg_plotdata1[iplot].xdata[ (*npts) - 1] == bin2 )
   (*yhist0)[(*nbins) - 1] = (*yhist0)[(*nbins) - 1] + 1.0;


// Set value of output string:
// Histogram type ("number", "fraction", or "percent")
 
strcpy(histo_type0, gseg_plotdata1[iplot].bin_value);

return;
}
