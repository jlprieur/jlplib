/***************************************************************************
* JLP_GsegAxes class
*
* JLP
* Version 27/03/2017
***************************************************************************/
#include <time.h>
#include "jlp_gseg_axes.h"

/***************************************************************************
* Constructor
***************************************************************************/
JLP_GsegAxes::JLP_GsegAxes(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0)
{

// Save to private variables:
jlp_gseg1 = jlp_gseg0;
jlp_gsegraf1 = jlp_gsegraf0;

InitAxesParameters();

return;
}
/***************************************************************************
* Create pointers and structures for JLP_GsegAxes
*
***************************************************************************/
void JLP_GsegAxes::InitAxesParameters()
{
static plot_param_type plot_param;
static plot_param_3d_type plot_param_3d;
static plot_box_data_type plot_box_data;
static ticklabels_type ticklabels;

/* Create pointers to structures */
  p_plot_param = &plot_param;
  p_plot_param_3d = &plot_param_3d;
  p_plot_box_data = &plot_box_data;
  p_ticklabels = &ticklabels;

/* Initialize plot-parameter character arrays to zero bits */
  memset(p_plot_param->axis_type,        0, sizeof(p_plot_param->axis_type));
  memset(p_plot_param->axis_scale,       0, sizeof(p_plot_param->axis_scale));
  memset(p_plot_param->grid,             0, sizeof(p_plot_param->grid));
  memset(p_plot_param->date_time_anchor, 0, sizeof(p_plot_param->date_time_anchor));
  memset(p_plot_param->plot_box,         0, sizeof(p_plot_param->plot_box));
  memset(p_plot_param->x_tick_marks,     0, sizeof(p_plot_param->x_tick_marks));
  memset(p_plot_param->y_tick_marks,     0, sizeof(p_plot_param->y_tick_marks));
  memset(p_plot_param->z_tick_marks,     0, sizeof(p_plot_param->z_tick_marks));
  memset(p_plot_param->x_tick_labels,    0, sizeof(p_plot_param->x_tick_labels));
  memset(p_plot_param->y_tick_labels,    0, sizeof(p_plot_param->y_tick_labels));
  memset(p_plot_param->z_tick_labels,    0, sizeof(p_plot_param->z_tick_labels));

/* Set pointers to NULL */
  xlabel1               = NULL;
  ylabel1               = NULL;
  zlabel1               = NULL;
  title1                = NULL;
  background_image_file = NULL;
  pixbuf_xlabel.text    = NULL;
  pixbuf_ylabel.text    = NULL;
  pixbuf_zlabel.text    = NULL;
  pixbuf_title.text    = NULL;
  pixbuf_xlabel.font_name    = NULL;
  pixbuf_ylabel.font_name    = NULL;
  pixbuf_zlabel.font_name    = NULL;
  pixbuf_title.font_name    = NULL;

// string_get is used for reading lines (see gseg_get_string())
  string_get = NULL;

/* color-specification characters */
  strcpy(color_string, "kaswrylqbfmogtnpx");

/* Tabulate color array for colors specified by color characters */
  color_rgba[0]  = 0x000000FF;   /* k black   (black)        */
  color_rgba[1]  = 0x808080FF;   /* a gray    (gray50)       */
  color_rgba[2]  = 0xC0C0C0FF;   /* s silver  (gray75)       */
  color_rgba[3]  = 0xFFFFFFFF;   /* w white   (white)        */
  color_rgba[4]  = 0xFF0000FF;   /* r red     (red)          */
  color_rgba[5]  = 0xFFFF00FF;   /* y yellow  (yellow)       */
  color_rgba[6]  = 0x00FF00FF;   /* l lime    (green)        */
  color_rgba[7]  = 0x00FFFFFF;   /* q aqua    (cyan)         */
  color_rgba[8]  = 0x0000FFFF;   /* b blue    (blue)         */
  color_rgba[9]  = 0xFF00FFFF;   /* f fuchsia (magenta)      */
  color_rgba[10] = 0x800000FF;   /* m maroon  (dark red)     */
  color_rgba[11] = 0x808000FF;   /* o olive   (dark yellow)  */
  color_rgba[12] = 0x008000FF;   /* g green   (dark green)   */
  color_rgba[13] = 0x008080FF;   /* t teal    (dark cyan)    */
  color_rgba[14] = 0x000080FF;   /* n navy    (dark blue)    */
  color_rgba[15] = 0x800080FF;   /* p purple  (dark magenta) */
  color_rgba[16] = 0xFFFFFF00;   /* x transparent            */

return;
}
/***************************************************************************
* Initialize variables for JLP_GsegAxes
*
***************************************************************************/
void JLP_GsegAxes::InitializeAxesVariables()
{
int i;
time_t time_current;

/* Get date and time */
   memset(date_time1, 0, sizeof(date_time1));
   time_current = time(NULL);
   strftime(date_time1,
            sizeof(date_time1),
            "%d-%b-%Y %H:%M:%S",
            localtime(&time_current));

/* Specify default plot parameters */
   strcpy(p_plot_param->axis_scale,       "auto");
   strcpy(p_plot_param->grid,             "off");
   strcpy(p_plot_param->minor_ticks,      "off");
   strcpy(p_plot_param->date_time_anchor, "off");
   strcpy(p_plot_param->plot_box,         "on");
   strcpy(p_plot_param->x_tick_marks,     "on");
   strcpy(p_plot_param->y_tick_marks,     "on");
   strcpy(p_plot_param->z_tick_marks,     "on");
   strcpy(p_plot_param->x_tick_labels,    "on");
   strcpy(p_plot_param->y_tick_labels,    "on");
   strcpy(p_plot_param->z_tick_labels,    "on");

   gridchar1 = 'l';                /* solid line */
   gridchar2 = 's';                /* silver */
   gridcolor1 = 0xC0C0C0FF;        /* silver */
   background_image_style = 2;     /* fill */

   p_plot_param_3d->phi   = 30.0;
   p_plot_param_3d->theta = 30.0;

   minor_ticks_flag = 0;

// Axis limits to be set: initialize them to zero
   for ( i=1; i<=6; i++ )
      set_axis_limits1[i-1] = 0;


// Initialize font sizes
  font_size_title1 = 12.;
  font_size_axis_labels1 = 12.;
  font_size_tick_labels1 = 12.;

}
