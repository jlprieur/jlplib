/*******************************************************************************
* jlp_gnome_defs.h
*
* Declares GNOME structures for gsegraf
*
* Modifieed version of the GSEGrafix package (gsegrafix-1.0.6, sept. 2011)
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* JLP
* Version 28/10/2016
*******************************************************************************/
#ifndef jlp_gnome_defs_h_
#define jlp_gnome_defs_h_

#include <gnome.h>
#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-job.h>
#include <libgnomeprintui/gnome-print-dialog.h>
#include <libgnomeprintui/gnome-print-job-preview.h>


/* Define structures */
typedef struct
   {
   GnomeApp *window;
   GtkWidget *canvas;
   int x, y, width, height;
   int flag;
   } window_data_type;

#endif
