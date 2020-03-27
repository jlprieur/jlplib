/******************************************************************************
* JLP_GsegText class
*
* JLP
* Version 01/04/2017
*******************************************************************************/
#ifndef _jlp_gseg_text_h_
#define _jlp_gseg_text_h_

#include "jlp_gsegraf.h"

class JLP_GsegText {
public:

// jlp_GsegText.cpp
 JLP_GsegText(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0, 
              JLP_GsegAxes *jlp_gseg_axes0);

// Desctructor:
 ~JLP_GsegText(){ };

private:
 JLP_Gseg *jlp_gseg1;
 JLP_Gsegraf *jlp_gsegraf1;
 JLP_GsegAxes *jlp_gseg_axes1;

};

#endif
