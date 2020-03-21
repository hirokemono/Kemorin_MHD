
/*  kemoviewer_param_c.h */

#ifndef KEMOVIEWER_PARAM_C_
#define KEMOVIEWER_PARAM_C_

#include "calypso_param_c.h"
#include "kemoviewer.h"

/* color parameter */

#define RED_R   ONE
#define RED_G   ZERO
#define RED_B   ZERO
#define RED_A   ONE

#define WHITE_R   ONE
#define WHITE_G   ONE
#define WHITE_B   ONE
#define WHITE_A   ONE

#define BLACK_R   ZERO
#define BLACK_G   ZERO
#define BLACK_B   ZERO
#define BLACK_A   ONE

#define GRAY_R   SEVEN_DECI
#define GRAY_G   ONE
#define GRAY_B   SEVEN_DECI
#define GRAY_A   ONE

#define BLUE_R   ZERO
#define BLUE_G   ZERO
#define BLUE_B   ONE
#define BLUE_A   ONE

#define L_GREEN_R   THREE_DECI
#define L_GREEN_G   ONE
#define L_GREEN_B   THREE_DECI
#define L_GREEN_A   ONE

#define D_GREEN_R   ZERO
#define D_GREEN_G   SEVEN_DECI
#define D_GREEN_B   ZERO
#define D_GREEN_A   HALF


/* Init parameter */

#define  INIT_DRAW_SOLID   IONE
#define  INIT_DRAW_LINE    IZERO
#define  INIT_DRAW_NOD     IZERO

#define  INIT_COLOR_MODE       RAINBOW_COLOR
#define  INIT_NUM_COLOR_LOOP   ISIX

#define  INIT_POLYGON_MODE      NORMAL_POLYGON
#define  INIT_SHADING_MODE      FLAT_SHADE
#define  INIT_TANGENTIAL_VECT   FULL_COMPONENT
#define  INIT_VECTOR_WIDTH      0.05

#define  INIT_SURFACE_COLOR  DOMAIN_COLOR
#define  INIT_LINE_COLOR     BLACK_LINE
#define  INIT_NODE_COLOR     WHITE_SURFACE

#define  INIT_NODE_SIZE      CENT
#define  INIT_DISTANCE       ZERO
#define  INIT_OPACITY        ONE

#define  INIT_DRAW_PSF_SOLID   IONE
#define  INIT_DRAW_PSF_LINE    IZERO
#define  INIT_DRAW_PSF_ZERO    IZERO

#define  INIT_IF_DRAW_PSF    IZERO
#define  INIT_IC_DRAW_PSF    IZERO

#define  INIT_IF_DRAW_FLINE    IZERO
#define  INIT_IC_DRAW_FLINE    IZERO

#define  INIT_ISOLINE_COLOR  BLACK_LINE
#define  INIT_N_ISOLINE        20
#define  INIT_ISOLINE_WIDTH    0.01
#define  INIT_PSF_OPACITY     ONE

#define  INIT_FLDLINE_COLOR  BLACK_LINE
#define  INIT_FLDLINE_TYPE   IFLAG_LINE
#define  INIT_FLDLINE_THICK  0.01

#endif
