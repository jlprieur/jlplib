/******************************************************************
*  Routines to obtain and free device numbers 
*
* int GDev_alloc_idv(JLP_GDev *Jgd0, int *idv, char *err_messg);
* JLP_GDev *GDev_from_idv(int idv);
* int GDev_free_idv(int idv);
*
* JLP
* Version 15/02/2017 
*************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "jlp_gdev_idv.h"

/*
#define DEBUG 1
*/

/* Maximum of DEV_MAX graphic devices opened simultaneously: */
#define DEV_MAX 64 

/* Declaring linkage specification to have "correct names"
* that can be linked with C programs */

/* Number of opened devices: */
static int n_open_dev = 0;
static int device_is_opened[DEV_MAX] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static JLP_GDev *Jgd1[DEV_MAX] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

/**************************************************************
*
* Routine to obtain a device identifier (idv1) 
*
* INPUT;
* pointer to a successfully opened graphic device
*
* RETURN:
* idv: device number (i.e. index in the array Jgd1[]
*             -1 if failure
***************************************************************/
int GDev_alloc_idv(JLP_GDev *Jgd0, int *idv, char *err_messg)
{
int i;

*idv = -1;

if(Jgd0 == NULL) {
  sprintf(err_messg,"gs_alloc_idv/Fatal error Jgd0 is NULL. \n"); 
  return(-2);
  }

if(n_open_dev < DEV_MAX - 1) {
  for(i = 0; i < DEV_MAX; i++)
     if(device_is_opened[i] == 0) {*idv = i; break;}
  }

// For debug:
sprintf(err_messg, "gs_alloc_idv/Device #%d opened (n_open_dev=%d/DEV_MAX=%d)\n",
         *idv, n_open_dev, DEV_MAX); 
#ifdef DEBUG
 printf("%s", err_messg);
#endif

if(*idv == -1) {
  sprintf(err_messg,"gs_alloc_idv/Fatal error: too many graphic devices \
have been opened (max=%d)\n", DEV_MAX);
  return(-1);
  }
else {
  device_is_opened[*idv] = 1;
  Jgd1[*idv] = Jgd0;
  n_open_dev++;
  }

return(0);
}
/**************************************************************
* Return pointer of graphic device of index idv if opened 
*        NULL otherwise
*
* INPUT:
*  idv: device number
* 
* RETURN:
* pointer to graphic device of index idv
* NULL if error
***************************************************************/
JLP_GDev *GDev_from_idv(int idv)
{
int opened = 0;
if(idv >= 0 && idv < n_open_dev) opened = device_is_opened[idv];
if(!opened ) {
  return(NULL); 
  } else {
  return(Jgd1[idv]);
  }
}
/**************************************************************
* Free idv index
***************************************************************/
int GDev_free_idv(int idv)
{
int status = -1;

if(idv >= 0 && idv < n_open_dev) {
  device_is_opened[idv] = 0;
  n_open_dev--;
  status = 0;
  }

return(status);
}
