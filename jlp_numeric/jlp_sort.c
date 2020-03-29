/********************************************************************
* Set of routines to sort arrays in increasing order
*
* JLP
* Version 06/07/2010
********************************************************************/
#include "jlp_numeric.h" 

/* Contained in this file and defined in jlp_ftoc.h
* "jlp_ftoc_proto.h" and "
int JLP_QSORT_INDX_CHAR(char *array, int *index, int *nn);
int JLP_QSORT_INDX_DBLE(double *array, int *index, int *nn);
int JLP_QSORT_INDX(float *array, int *index, int *nn);
int JLP_QSORT_FLOAT(float *array, int *nn);
int JLP_QSORT_DBLE(double *array, int *nn);
int JLP_MEDIAN(float *data, int npts, float *value);
int JLP_MEDIAN_DBLE(double *data, int npts, double *value);
*/
static void qs(float *array, int left, int right);
static void qs_dble(double *array, int left, int right);
static void qs2(float *array, int *index, int left, int right);
static void qs2_dble(double *array, int *index, int left, int right);
static  int qs2_char(char *array, int length, int *index, int left, int right);
int JLP_strncmp(char *str1, char *str2, int len);

/* #define MAIN_PROGRAM */
#ifdef MAIN_PROGRAM 
#define NMAX 100 
int main(int argc, char *argv[])
{
float array[NMAX];
int nn, index[NMAX];
register int i, k;
  printf("JLP_SORT to test sorting routines\n");

 for (k = 0; k < 2; k++)
 {
   nn = 20;
   for(i = 0; i < nn; i++) array[i] = (nn - i);
   printf(" Input is: \n");
   for(i = 0; i < nn; i++) printf("%d ",(int)array[i]);
   printf("\n");

/* Calling sorting routine: */
   if(k == 0)
   {
   JLP_QSORT_FLOAT(array, &nn);
   printf("Output of JLP_QSORT_FLOAT is: \n");
   }
   else
   {
   JLP_QSORT_INDX(array, index, &nn);
   printf("Output of JLP_QSORT_INDX is: (index first) \n");
   for(i = 0; i < nn; i++) printf("%d ",(int)index[i]);
   printf("\n");
   }

   for(i = 0; i < nn; i++) printf("%d ",(int)array[i]);
   printf("\n");
 }
}
#endif
/****************************************************************************
* Quicksort (Cf. "C: The complete reference", Herbert Schildt)
*
* INPUT:
*  array[nn]: array to be sorted
*
* OUTPUT:
*  array[nn]: sorted array
****************************************************************************/
int JLP_QSORT_FLOAT(float *array, int *nn)
{
if(*nn < 2) return(0);
 qs(array, 0, (*nn)-1);
return(0);
}
/****************************************************************************
* Quicksort (Cf. "C: The complete reference", Herbert Schildt)
*
* INPUT:
*  array[nn]: array to be sorted
*
* OUTPUT:
*  array[nn]: sorted array
****************************************************************************/
int JLP_QSORT_DBLE(double *array, int *nn)
{
if(*nn < 2) return(0);
 qs_dble(array, 0, (*nn)-1);
return(0);
}
/**************************************************************************
* The Quicksort 
* "float" version
***************************************************************************/
static void qs(float *array, int left, int right)
{
register int i, j;
float x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */ 
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
    i++; j--;
  }
} while(i<=j);

if(left < j) qs(array, left, j);
if(i < right) qs(array, i, right);
}
/**************************************************************************
* The Quicksort
* "double precision" version
***************************************************************************/
static void qs_dble(double *array, int left, int right)
{
register int i, j;
double x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
    i++; j--;
  }
} while(i<=j);

/* Recursive calls: */
if(left < j) qs_dble(array, left, j);
if(i < right) qs_dble(array, i, right);
}
/****************************************************************************
* Quicksort (Cf. "C: The complete reference", Herbert Schildt)
*
* INPUT:
*  array[nn]: array to be sorted
*
* OUTPUT:
*  array[nn]: sorted array
*  index[nn]: array giving the index of the input array, 
*             to sort other arrays in the same way if necessary
*             (f.i. array2[i] := array2[index[i]])
****************************************************************************/
int JLP_QSORT_INDX(float *array, int *index, int *nn)
{
register int i;

/* Initialization of index array: */
for(i = 0; i < *nn; i++) index[i] = i;

if(*nn < 2) return(0);
 qs2(array, index, 0, (*nn)-1);

return(0);
}
/************** Double precision version: ********************/
int JLP_QSORT_INDX_DBLE(double *array, int *index, int *nn)
{
register int i;
for(i = 0; i < *nn; i++) index[i] = i;
if(*nn < 2) return(0);
 qs2_dble(array, index, 0, (*nn)-1);
return(0);
}
/*******************************************************************
* Character string version: 
* input character string with small pieces of "length" characters...
********************************************************************/
int JLP_QSORT_INDX_CHAR(char *array, int *length, int *index, int *nn)
{
register int i;
for(i = 0; i < *nn; i++) index[i] = i;
if(*nn < 2) return(0);
 qs2_char(array, *length, index, 0, (*nn)-1);
return(0);
}
/**************************************************************************
* The Quicksort, with index array 
***************************************************************************/
static void qs2(float *array, int *index, int left, int right)
{
register int i, j;
int iy;
float x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */ 
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
/* Exchange index[i] and index[j]: */
    iy = index[i];
    index[i] = index[j];
    index[j] = iy;
    i++; j--;
  }
} while(i<=j);

if(left < j) qs2(array, index, left, j);
if(i < right) qs2(array, index, i, right);

return;
}
/*******************************************************************
* The Quicksort, with index array 
*** Double precision version: 
*******************************************************************/
static void qs2_dble(double *array, int *index, int left, int right)
{
register int i, j;
int iy;
double x, y;

i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
x = array[(left+right)/2];

/* Put the elements < x to the left and those > x to the right: */ 
do {
  while(array[i] < x && i < right) i++;
  while(x < array[j] && j > left) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    y = array[i];
    array[i] = array[j];
    array[j] = y;
/* Exchange index[i] and index[j]: */
    iy = index[i];
    index[i] = index[j];
    index[j] = iy;
    i++; j--;
  }
} while(i<=j);

/* Recursive calls: */
if(left < j) qs2_dble(array, index, left, j);
if(i < right) qs2_dble(array, index, i, right);

return;
}
/*******************************************************************
* The Quicksort, with index array 
* Character string version: 
*******************************************************************/
static int qs2_char(char *array, int length, int *index, int left, int right)
{
register int i, j, jc;
int iy, nlen = length;
char x[80], y[80];

if(length > 80) {
  fprintf(stderr, "JLP_QSORT_INDX_CHAR/Fatal error: length=%d > 80!\n",
          length);
  exit(-1);
  }
i = left; j = right;
/* Take the element in the middle as reference to partition the array: */
jc = (left+right)/2;
strncpy(x, &array[length * jc], length);
x[length-1] = '\0';

/* Put the elements < x to the left and those > x to the right: */ 
/* nlen = 1: sort elements according to first letter only */
do {
  while((JLP_strncmp(&array[length * i], x, nlen) < 0) && (i < right)) i++;
  while((JLP_strncmp(x, &array[length * j], nlen) < 0)  && (j > left)) j--;
  if(i <= j) {
/* Exchange array[i] and array[j]: */
    strncpy(y, &array[length * i], length);
    y[length-1] = '\0';
    strncpy(&array[length * i], &array[length * j], length);
    strncpy(&array[length * j], y, length);
/* Exchange index[i] and index[j]: */
    iy = index[i];
    index[i] = index[j];
    index[j] = iy;
    i++; j--;
  }
} while(i <= j);

/* Recursive calls: */
if(left < j) qs2_char(array, length, index, left, j);
if(i < right) qs2_char(array, length, index, i, right);

return(0);
}
/***************************************************************
* To compare alphabetically two strings
* if str1 < str2 return -1
* if str1 == str2 return 0
* if str1 > str2 return 1
***************************************************************/
int JLP_strncmp(char *str1, char *str2, int len)
{
int i, ival = 0;

for(i = 0; i < len; i++) {
  if(str1[i] < str2[i]) {
    ival = -1;
    break;
  } else if (str1[i] > str2[i]) {
    ival = 1;
    break;
  } 
}

return(ival);
}
/**************************************************************************
* JLP_MEDIAN 
* to compute the median of the values contained in the input array
*
* INPUT:
* data: input (unsorted) array 
* npts: number of points of the array 
*
* OUTPUT:
* data: sorted array
* value: value corresponding to the median
**************************************************************************/
int JLP_MEDIAN(float *data, int npts, float *value)
{

/* Sort the array in increasing order: */
JLP_QSORT(data, &npts);

*value = data[npts/2];

return(0);
}
/**************************************************************************
* JLP_MEDIAN_DBLE
* to compute the median of the values contained in the input array
*
* INPUT:
* data: input (unsorted) array
* npts: number of points of the array
*
* OUTPUT:
* data: sorted array
* value: value corresponding to the median
**************************************************************************/
int JLP_MEDIAN_DBLE(double *data, int npts, double *value)
{

/* Sort the array in increasing order: */
JLP_QSORT_DBLE(data, &npts);

*value = data[npts/2];

return(0);
}

