
/* m_kemoviewer_menu.h */

#ifndef M_KEMOVIEWER_MENU_
#define M_KEMOVIEWER_MENU_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_color_table_c.h"
#include "m_colorbar_buffer.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"


#define VIEW_3D        0
#define VIEW_STEREO    1
#define VIEW_MAP       2
#define VIEW_XY        3
#define VIEW_XZ        4
#define VIEW_YZ        5
#define RESET         10

#define MESH_OFF          100
#define SURFNOD_TOGGLE    1
#define SURFSOLID_TOGGLE  2
#define SURFGRID_TOGGLE   3

#define DOMAIN_FLAG     0
#define NODE_GRP_FLAG   1
#define ELEM_GRP_FLAG   2
#define SURF_GRP_FLAG   3

#define ISET_FLINE_TYPE   11
#define ISET_FLINE_THICK  200
#define FLINE_OFF         50

#define SAVE_EPS      10
#define SAVE_PS       11
#define SAVE_PDF      20
#define SAVE_PNG       1
#define SAVE_BMP       2
#define SAVE_PPM_B     3
#define SAVE_PPM_A     4
#define SAVE_TIFF      5
#define SAVE_QT_MOVIE  999
#define NO_SAVE_FILE   0
#define SAVE_UNDEFINED  -1

#define IFLAG_MESH      99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_SURF_MESH   1
#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_VTD   20
#define IFLAG_SURF_VTK   21
#define IFLAG_SURF_SDT   30
#define IFLAG_PSF_BIN    31

#define IFLAG_SURF_MESH_GZ  101
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111
#define IFLAG_SURF_VTD_GZ   120
#define IFLAG_SURF_VTK_GZ   121
#define IFLAG_SURF_SDT_GZ   130
#define IFLAG_PSF_BIN_GZ    131

#define OFF 0
#define ON  1

/* Prototypes */

#endif
