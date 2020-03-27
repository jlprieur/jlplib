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
*  ihist : histogram index (from 1 to number of histograms)
*
* OUTPUT:
*  yhist0[nbins] : histogram
*  nbins : number of bins
*  npts : total number of points 
*  binmin, binmax : smallest and largest bin value
*  binwidth : bin width 
*  histo_type0 : histogram type ("number", "fraction", or "percent")
**********************************************************************/
void JLP_GsegData::ComputeHistogram(int iplot, int ihist, double **yhist0,
                                    int *nbins, int *npts, double *binmin,
                                    double *binmax, double *binwidth,
                                    char *histo_type0)
{
/* Declare variables */
int i, j, index, n1, n2, index_bin_values, index_bin_refs;
double data_min, data_max, sum, sumsq, mean, std, q[5], bin1, bin2;

/* Calculate data index */
 index = 0;
 if ( iplot > 1 )
    for ( i=1; i<iplot; i++ )
       index = index + ndata[i-1];

/* Calculate data mean and standard deviation */
 sum = 0.0;
 *npts = ndata[iplot-1];
 for ( i = 1; i <= *npts; i++ )
    sum = sum + xdata[index+i-1];
 mean = sum / (*npts);

 sumsq = 0.0;
 for ( i = 1; i <= (*npts); i++ )
    sumsq = sumsq + (xdata[index+i-1] - mean) * (xdata[index+i-1] - mean);
 std = sqrt(sumsq/((*npts) - 1.0));


/* Calculate data quartile values */
 data_min = xdata[index];
 data_max = xdata[index + (*npts) - 1];
 q[0] = data_min;
 q[4] = data_max;
 for ( i = 1; i <= 3; i++ )
    {
    j = roundint(i * 0.25 * (*npts));
    q[i] = xdata[index+j-1];
    }


/* Estimate optimal histogram bin width */
 if ( bin_widths[ihist-1] <= 0.0 )
    {
/* Scott: 
    binwidth = 3.49*std/pow((double) (*npts), 1.0/3.0); 
*/   
/* Freedman, Diaconis: */
    *binwidth = 2.0*(q[3] - q[1])/pow((double) (*npts), 1.0/3.0);   
    bin_widths[ihist-1] = *binwidth;
    }
 else
    *binwidth = bin_widths[ihist-1];


/* Calculate number of bins, binmin, and binmax */
 index_bin_refs = (ihist - 1)*9;
 if ( strcmp(&bin_refs[index_bin_refs], "mean") == 0 )
    {
    n1 = ceil(((mean - (*binwidth)/2.0) - data_min)/ (*binwidth));
    n2 = ceil((data_max - (mean + (*binwidth)/2.0))/ (*binwidth));
    *nbins = n1 + n2 + 1;
    *binmin = (mean - (*binwidth)/2.0) - n1 * (*binwidth);
    *binmax = (mean + (*binwidth)/2.0) + n2 * (*binwidth);
    }
 else if ( strcmp(&bin_refs[index_bin_refs], "zero") == 0 )
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
 else if ( strcmp(&bin_refs[index_bin_refs], "integers") == 0 )
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
   while ( (j <= (*npts)) && (bin1 <= xdata[index+j-1]) 
           && (xdata[index+j-1] < bin2) )
      {
      (*yhist0)[i-1] = (*yhist0)[i-1] + 1.0;
      j++;
      }
   }
if ( xdata[index + (*npts) - 1] == bin2 )
   (*yhist0)[(*nbins) - 1] = (*yhist0)[(*nbins) - 1] + 1.0;


// Set value of output string:
// Histogram type ("number", "fraction", or "percent")
 
index_bin_values = (ihist - 1)*9;
strcpy(histo_type0, &bin_values[index_bin_values]);

return;
}
