/********************************************************************
* jlp_wxgseg_utils.cpp
*
* JLP
* Version 28/12/2016
********************************************************************/
#include "wx/wx.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

/***************************************************************************
*
***************************************************************************/
void JLP_ErrorDialog(const char *error_message)
{
wxString str1;

str1 = wxString(error_message);
fprintf(stderr, "JLP_ErrorDialog >%s<\n", error_message);

wxMessageBox(str1, _T("JLP_wxGSEG"), wxICON_ERROR);

return;
}
