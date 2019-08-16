/*
// draw_colorbar_gl.c
*/

#include "draw_colorbar_gl.h"

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



void solid_colorbar_box_to_buf(struct colormap_params *cmap_s, 
			GLfloat xbar_min, GLfloat xbar_mid, GLfloat ybar_min, GLfloat ydelta, double psf_min, double psf_max, 
			GLfloat **xy_buf, GLfloat **rgba_buf){
	GLfloat y1;
	double psf_value;
	double f_color[4], l_color[4];
	int i, nd;
	
	int num_quad = 64;
	for(i=0;i<num_quad;i++){
		y1 = ybar_min + ydelta * (GLfloat) i;
		psf_value = psf_min + (psf_max-psf_min) * (double)(i+1) / (double)num_quad;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
	
		f_color[3] = ONE;
		
		xy_buf[6*i  ][0] = xbar_min;
		xy_buf[6*i+1][0] = xbar_mid;
		xy_buf[6*i+2][0] = xbar_mid;
		xy_buf[6*i  ][1] = y1;
		xy_buf[6*i+1][1] = y1;
		xy_buf[6*i+2][1] = y1 + ydelta;

		xy_buf[6*i+3][0] = xbar_mid;
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
	return;
};

void fade_colorbar_box_to_buf(struct colormap_params *cmap_s, GLfloat *bg_color, 
			GLfloat xbar_mid, GLfloat xbar_max, GLfloat ybar_min, GLfloat ydelta, double psf_min, double psf_max, 
			GLfloat **xy_buf, GLfloat **rgba_buf){
	GLfloat y1;
	double psf_value;
	double f_color[4], l_color[4];
	int i, nd;
	
	int num_quad = 64;
	for(i=0;i<num_quad;i++){
		y1 = ybar_min + ydelta * (GLfloat)i;
		psf_value = psf_min + (psf_max-psf_min) * (double)(i+1) / (double)num_quad;
		set_rainbow_color_code(cmap_s, psf_value, f_color);
	
		for (nd=0; nd<3; nd++) {
			f_color[nd] = f_color[nd] * f_color[3]
					+ bg_color[nd] * (ONE - f_color[3]);
        };
        f_color[3] = ONE;
		
		xy_buf[6*i  ][0] = xbar_mid;
		xy_buf[6*i+1][0] = xbar_max;
		xy_buf[6*i+2][0] = xbar_max;
		xy_buf[6*i  ][1] = y1;
		xy_buf[6*i+1][1] = y1;
		xy_buf[6*i+2][1] = y1 + ydelta;

		xy_buf[6*i+3][0] = xbar_max;
		xy_buf[6*i+4][0] = xbar_mid;
		xy_buf[6*i+5][0] = xbar_mid;
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
	return;
};


int colorbar_frame_to_buf(int iflag_zero, GLfloat *text_color, 
			GLfloat xbar_min, GLfloat xbar_max, GLfloat ybar_min, GLfloat ybar_max,
			GLfloat yline_zero,  GLfloat **xy_buf, GLfloat **rgba_buf){
	int i, nd, inum;
	
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
	
	return inum;
};

struct cbar_work{
	int iflag_zero;
	
	GLfloat xwin;
	GLfloat ywin;
	
	GLfloat xbar_min;
	GLfloat xbar_max;
	GLfloat xbar_mid;
	
	GLfloat ybar_min;
	GLfloat ybar_max;
	GLfloat ydelta;
	GLfloat yline_zero;
	
	double psf_min;
	double psf_max;
};

void set_colorbar_position(int iflag_retina, GLint nx_win, GLint ny_win,
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk){
    cbar_wk->xwin = (GLfloat)nx_win;
	cbar_wk->ywin = (GLfloat)ny_win;
    
    if( cbar_wk->xwin >= 640*(iflag_retina+1) ){
        cbar_wk->xbar_max = cbar_wk->xwin * 0.875;
    } else {
        cbar_wk->xbar_max = cbar_wk->xwin - (iflag_retina+1) * 80;
    }
	cbar_wk->xbar_min = cbar_wk->xbar_max - 0.025 * cbar_wk->xwin;
	cbar_wk->xbar_mid = (cbar_wk->xbar_min + cbar_wk->xbar_max) * 0.5;
	cbar_wk->ybar_min = 0.05 * cbar_wk->ywin;
	cbar_wk->ybar_max = 0.25 * cbar_wk->ywin;
	cbar_wk->ydelta =  (cbar_wk->ybar_max - cbar_wk->ybar_min)
			/ ((GLfloat)64);
	
	cbar_wk->iflag_zero = 0;
	
	cbar_wk->psf_min = cmap_s->color_data[IZERO];
	cbar_wk->psf_max = cmap_s->color_data[cmap_s->n_color_point-1];
	if( (cbar_wk->psf_min*cbar_wk->psf_max) < ZERO ) cbar_wk->iflag_zero = 1;
	
	cbar_wk->yline_zero = cbar_wk->ybar_min 
		+ (cbar_wk->ybar_max - cbar_wk->ybar_min) * (-cbar_wk->psf_min) 
		/ (cbar_wk->psf_max - cbar_wk->psf_min);
	
	return;
}

void draw_colorbar_gl(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], struct colormap_params *cmap_s){
	int inum, nd;
    
	GLfloat xy_buf[384][2];
	GLfloat rgba_buf[384][4];
	char minlabel[20], maxlabel[20], zerolabel[20];
	
	struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
	set_colorbar_position(iflag_retina, nx_win, ny_win, cmap_s, cbar_wk);
	
	sprintf(minlabel, "% 3.2E",cbar_wk->psf_min);
	sprintf(maxlabel, "% 3.2E",cbar_wk->psf_max);
	sprintf(zerolabel,"% 3.2E",ZERO);
	
	sprintf(colorbar_text, "% 3.2E% 3.2E% 3.2E", cbar_wk->psf_min, cbar_wk->psf_max, ZERO);
	
	orthogonalGL(0.0, cbar_wk->xwin, 0.0, cbar_wk->ywin,-1.0,1.0);
	set_view_by_identity();
	
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

	solid_colorbar_box_to_buf(cmap_s, 
				cbar_wk->xbar_min, cbar_wk->xbar_mid, cbar_wk->ybar_min, 
				cbar_wk->ydelta, cbar_wk->psf_min, cbar_wk->psf_max, 
				xy_buf, rgba_buf);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*128));

	fade_colorbar_box_to_buf(cmap_s, bg_color, 
				cbar_wk->xbar_mid, cbar_wk->xbar_max, cbar_wk->ybar_min, cbar_wk->ydelta,
				cbar_wk->psf_min, cbar_wk->psf_max, 
				xy_buf, rgba_buf);
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

	inum = colorbar_frame_to_buf(cbar_wk->iflag_zero, text_color,
				cbar_wk->xbar_min, cbar_wk->xbar_max, 
				cbar_wk->ybar_min, cbar_wk->ybar_max, cbar_wk->yline_zero,
				xy_buf, rgba_buf);
	glDrawArrays(GL_LINES, IZERO, (ITWO*inum));
	
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);

    glDisable(GL_LIGHTING);
	glColor4fv(text_color);
	ysGlPlotBitmap2d_retina(iflag_retina,
                            (cbar_wk->xbar_max+3.0), (cbar_wk->ybar_min-6.0), minlabel);
	ysGlPlotBitmap2d_retina(iflag_retina,
                            (cbar_wk->xbar_max+3.0), (cbar_wk->ybar_max-6.0), maxlabel);
	
	
	if(cbar_wk->iflag_zero == 1){
		ysGlPlotBitmap2d_retina(iflag_retina, (cbar_wk->xbar_max+3.0),
                                (cbar_wk->yline_zero-6.0), zerolabel);
	};
	
	free(cbar_wk);
	glColor4fv(black);
    glEnable(GL_LIGHTING);
	return;
}

