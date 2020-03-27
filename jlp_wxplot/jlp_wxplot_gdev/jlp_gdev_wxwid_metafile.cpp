/*************************************************************************
* JLP_cGDev_wxWID Class (for curves)
* Handling MetaFile utilities 
*
* JLP
* Version 09/03/2012
**************************************************************************/
#if USE_METAFILE

#include <stdlib.h> // random generator
#include "jlp_cgdev_wxwid.h"
#include "jlp_string.h"  // jlp_compact_string 

/*
#define DEBUG
*/
/*******************************************************************
* Metafile with all drawing commands previously implemented in backup_pc
*
********************************************************************/
int JLP_cGDev_wxWID::NewNameForMetaFile()
{
int irand;

if(MetaFileIsOpened == true) CloseMetaFile(); 

// Random generator between 1 and RAND_MAX:
   irand = rand();
   sprintf(MetaFileName, "zGdisp1_%d.txt", rand());

return(0);
}
/*******************************************************************
* Metafile with all drawing commands previously implemented in backup_pc
*
********************************************************************/
int JLP_cGDev_wxWID::OpenMetaFile(const char* mode)
{
   if(MetaFileIsOpened == true) CloseMetaFile(); 

   if((fp_MetaFile = fopen(MetaFileName, mode)) == NULL) {
    fprintf(stderr, "OpenMetaFile/Error opening %s\n", MetaFileName); 
    return(-1);
    }

   MetaFileIsOpened = true;

return(0);
}
/**********************************************************************
*
**********************************************************************/
int JLP_cGDev_wxWID::CloseMetaFile()
{
if(MetaFileIsOpened != true) return(-1);

fclose(fp_MetaFile);
MetaFileIsOpened = false;

return(0);
}
/**********************************************************************
*
**********************************************************************/
int JLP_cGDev_wxWID::ReadAndExecuteMetaFile()
{
char metaline[80];
if(MetaFileIsOpened != true) return(-1);

CloseMetaFile();
OpenMetaFile("rw");

while(!feof(fp_MetaFile)) {
  fgets(metaline, 80, fp_MetaFile);
  ExecuteMetaFileLine(metaline);
  }

return(0);
}
/**********************************************************************
*
**********************************************************************/
int JLP_cGDev_wxWID::ExecuteMetaFileLine(char *metaline)
{
char command[80], arguments[60], buffer[40];
char *pc, label[40];
int nval, ix1, iy1, ix2, iy2, lwidth;
double angle, expand = 1.2;

if(MetaFileIsOpened != true) return(-1);

// Decode command:
strncpy(command, metaline, 80);
command[79] = '\0';
pc = command;
while(*pc && *pc != '(') pc++;
if(*pc == '(') strncpy(arguments, pc+1, 60); 
*pc = '\0';

/*
printf("DDEBUG/ExecuteMetaFileLine/ %s", metaline);
printf("-> Command = %s\n-> Arguments = %s\n", command, arguments);
*/

// JLP_cGDev_wxWID::line_device(int x1, int y1, int x2, int y2, int lwidth)
if(!strncmp(command, "line_device", 11)) {
  nval = sscanf(metaline, "line_device(%d,%d,%d,%d,%d)", 
                &ix1, &iy1, &ix2, &iy2, &lwidth);
  if(nval == 5) line_device(ix1, iy1, ix2, iy2, lwidth);
// JLP_cGDev_wxWID::label_device(const char *s, int xstart, int ystart,
//                            double angle, double expand, int drawit=1)
 } else if(!strncmp(command, "label_device", 12)) {
//
// Decode label:
   strncpy(label, arguments, 40);
   label[39] = '\0';
   pc = label; 
   while(*pc && *pc != ',') pc++;
   if(*pc == ',') strncpy(buffer, pc+1, 40); 
   *pc = '\0';
//
   nval = sscanf(buffer, "%d,%d,%f,%f", &ix1, &iy1, &angle, &expand);
   if(nval == 4) label_device(label,ix1, iy1, angle, expand, 1);
  }

return(0);
}
#endif // EOF USE_METAFILE
