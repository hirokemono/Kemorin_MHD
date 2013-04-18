
/* draw_colorbar_gl.c */

#include "draw_colorbar_gl.h"
#include "gl2ps.h"

static int ibase_8x12;
static int ibase_12x16;
static int ibase_16x24;
static int ibase_20x32;

static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};


static void ysGlPlotBitmap2d_retina(int iflag_retina,
                                    GLfloat x_plot, GLfloat y_plot,
                                    const char *label){
    if(iflag_retina == IONE){
        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot+HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot+HALF), (GLubyte *)label);

        ysGlPlotBitmap2d(ibase_16x24, (x_plot-HALF), (y_plot    ), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot+HALF), (y_plot    ), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot-HALF), (GLubyte *)label);
        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot+HALF), (GLubyte *)label);

        ysGlPlotBitmap2d(ibase_16x24, (x_plot    ), (y_plot    ), (GLubyte *)label);
    } else {
        ysGlPlotBitmap2d(ibase_8x12, (x_plot    ), (y_plot    ), (GLubyte *)label);
    }
    
    gl2psText(label, "Times",12);
    return;
}

void init_colorbar_fonts(){
	
	/*printf("Base %d \n",ibase_8x12);*/
	ibase_8x12 = glGenLists(0x100);
	YsGlUseFontBitmap8x12(ibase_8x12);
	/*printf("Base %d \n",ibase_12x16);*/
	ibase_12x16 = glGenLists(0x100);
	YsGlUseFontBitmap12x16(ibase_12x16);
	
	/*printf("Base %d \n",ibase_16x24);*/
	ibase_16x24 = glGenLists(0x100);
	YsGlUseFontBitmap16x24(ibase_16x24);
	/*printf("Base %d \n",ibase_20x32);*/
	ibase_20x32 = glGenLists(0x100);
	YsGlUseFontBitmap20x32(ibase_20x32);
	return;
}

void draw_colorbar_gl(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], struct colormap_params *cmap_s){
	int i, inum;
    
	GLfloat xwin, ywin;
	GLfloat xbar_min, xbar_max;
	GLfloat ybar_min, ybar_max, ydelta, y1, yline_zero;
	GLfloat xy_buf[384][2];
	GLfloat rgba_buf[384][4];

	int iflag_zero, nd;
	double psf_min, psf_max;
	double psf_value, f_color[4], l_color[4];
	char minlabel[20], maxlabel[20], zerolabel[20];
	
    xwin = (GLfloat)nx_win;
	ywin = (GLfloat)ny_win;
	xbar_min = 0.85 *  xwin;
	xbar_max = 0.875 * xwin;
	ybar_min = 0.05 * ywin;
	ybar_max = 0.25 * ywin;
	ydelta =  (ybar_max - ybar_min) / ((GLfloat)64);
	
	iflag_zero = 0;
	
	psf_min = cmap_s->color_data[IZERO];
	psf_max = cmap_s->color_data[cmap_s->n_color_point-1];
	if( (psf_min*psf_max) < ZERO ) iflag_zero = 1;
	
	yline_zero = ybar_min + (ybar_max-ybar_min) * (-psf_min) / (psf_max-psf_min);
	
	
	sprintf(minlabel, "% 3.2E",psf_min);
	sprintf(maxlabel, "% 3.2E",psf_max);
	sprintf(zerolabel,"% 3.2E",ZERO);
	
	sprintf(colorbar_text, "% 3.2E% 3.2E% 3.2E",psf_min, psf_max, ZERO);
	
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	gluOrtho2D(0.0, xwin, 0.0, ywin);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(ITWO, GL_FLOAT, IZERO, xy_buf);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, rgba_buf);

	if(cmap_s->min_opacity < 1.0) {
		glEnable(GL_MULTISAMPLE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDepthMask(GL_FALSE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
	glColor4fv(black);
	glColorMaterial(GL_FRONT_AND_BACK,GL_EMISSION);

	set_rainbow_color_code(cmap_s, psf_min, l_color);

	for(i=0;i<64;i++){
		y1 = ybar_min + ydelta * (GLfloat)i;
		psf_value = psf_min + (psf_max-psf_min) * (double)(i+1) / (double)64;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
		
		xy_buf[6*i  ][0] = xbar_min;
		xy_buf[6*i+1][0] = xbar_max;
		xy_buf[6*i+2][0] = xbar_max;
		xy_buf[6*i  ][1] = y1;
		xy_buf[6*i+1][1] = y1;
		xy_buf[6*i+2][1] = y1 + ydelta;

		xy_buf[6*i+3][0] = xbar_max;
		xy_buf[6*i+4][0] = xbar_min;
		xy_buf[6*i+5][0] = xbar_min;
		xy_buf[6*i+3][1] = y1 + ydelta;
		xy_buf[6*i+4][1] = y1 + ydelta;
		xy_buf[6*i+5][1] = y1;
		for (nd=0; nd<4; nd++) {
			rgba_buf[6*i  ][nd] = l_color[nd];
			rgba_buf[6*i+1][nd] = l_color[nd];
			rgba_buf[6*i+2][nd] = f_color[nd];
			rgba_buf[6*i+3][nd] = f_color[nd];
			rgba_buf[6*i+4][nd] = f_color[nd];
			rgba_buf[6*i+5][nd] = l_color[nd];
		}
		for (nd=0; nd<4; nd++) {l_color[nd] = f_color[nd];};
	};
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*128));

	
	glColorMaterial(GL_FRONT_AND_BACK,GL_EMISSION);
	glColor4fv(black);
	
	if(cmap_s->min_opacity < 1.0) {
		glDisable(GL_BLEND);
		glDepthMask(GL_TRUE);
		glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glDisable(GL_MULTISAMPLE);
	}
	
	glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
	glColor4fv(text_color);

	xy_buf[0][0] = xbar_min;
	xy_buf[1][0] = xbar_min;
	xy_buf[0][1] = ybar_min;
	xy_buf[1][1] = ybar_max;

	xy_buf[2][0] = xbar_max;
	xy_buf[3][0] = xbar_max;
	xy_buf[2][1] = ybar_min;
	xy_buf[3][1] = ybar_max;
	
	xy_buf[4][0] = xbar_min;
	xy_buf[5][0] = (xbar_max+3.0);
	xy_buf[4][1] = ybar_min;
	xy_buf[5][1] = ybar_min;

	xy_buf[6][0] = xbar_min;
	xy_buf[7][0] = (xbar_max+3.0);
	xy_buf[6][1] = ybar_max;
	xy_buf[7][1] = ybar_max;

	if(iflag_zero == 1){
		xy_buf[6][0] = xbar_min;
		xy_buf[7][0] = (xbar_max+6.0);
		xy_buf[6][1] = yline_zero;
		xy_buf[7][1] = yline_zero;
	};

	inum = iflag_zero + IFOUR;
	for (i=0;i<ITWO*inum; i++) {
		for (nd=0; nd<4; nd++) {rgba_buf[i][nd] = text_color[nd];}
	}
	glDrawArrays(GL_LINES, IZERO, (ITWO*inum));
	
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);

    glDisable(GL_LIGHTING);
	glColor4fv(text_color);
	ysGlPlotBitmap2d_retina(iflag_retina,
                            (xbar_max+3.0), (ybar_min-6.0), minlabel);
	ysGlPlotBitmap2d_retina(iflag_retina,
                            (xbar_max+3.0), (ybar_max-6.0), maxlabel);
	
	
	if(iflag_zero == 1){
		ysGlPlotBitmap2d_retina(iflag_retina, (xbar_max+3.0),
                                (yline_zero-6.0), zerolabel);
	};
	glColor4fv(black);
    glEnable(GL_LIGHTING);

    
	glPopMatrix();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);

	return;
}