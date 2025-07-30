
/* view_modifier_gtk.c */


#include "view_modifier_gtk.h"

/* initial settings */

#define LEFT_BUTTON    1
#define MIDDLE_BUTTON  2
#define RIGHT_BUTTON   3

#define GTK_KEY_DOWN   10000
#define GTK_KEY_UP     10000
#define GTK_KEY_LEFT   10000
#define GTK_KEY_RIGHT  10000


struct kemoviewer_gl_type * kemoGL_GTK;


GtkWidget *gl_area;
int iflag_quickdraw = 0;

static int left_button_func =   ROTATE;
static int middle_button_func = PAN;
static int right_button_func =  ZOOM;
static int arrow_key_func =     PAN;

static int      moving_left, moving_middle, moving_right;
static double  begin_left[2], begin_middle[2], begin_right[2];

static double  begin[2];
static double  gTrackBallRotation[4];

gboolean mouseButtonCB(GtkWidget *widget, GdkEventButton *event, gpointer user_data) {
	double xpos = event->x;
	double ypos = event->y;
	printf("mouseButtonCB %d %d %lf, %lf\n", event->button, 
				event->type, xpos, ypos);

	if(event->button == LEFT_BUTTON && event->type == GDK_BUTTON_PRESS) {
		moving_left = 1;
		begin_left[0] = xpos;
		begin_left[1] = ypos;
	};
	if(event->button == LEFT_BUTTON && event->type == GDK_BUTTON_RELEASE) {
		moving_left = 0;
	};
	
	if(event->button == MIDDLE_BUTTON && event->type == GDK_BUTTON_PRESS) {
		moving_middle = 1;
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	};
	if(event->button == MIDDLE_BUTTON && event->type == GDK_BUTTON_RELEASE) {
		moving_middle = 0;
	};
	
	if(event->button == RIGHT_BUTTON && event->type == GDK_BUTTON_PRESS) {
		moving_right = 1;
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	if(event->button == RIGHT_BUTTON && event->type == GDK_BUTTON_RELEASE) {
		moving_right = 0;
	};
	
	if(event->type == GDK_BUTTON_RELEASE){
		draw_full(kemo_sgl);
	};
	return TRUE;
};

gboolean mousePosCB(GtkWidget *widget, GdkEventButton *event, gpointer user_data) {
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	/*! This gets called when the mouse moves */
	double xpos = event->x;
	double ypos = event->y;
	
	double factor;
	int button_function = left_button_func;
	
	/*printf("mousePosCB %.1lf %.1lf\n", xpos, ypos); */
	if (moving_left == 0 && moving_middle == 0 && moving_right == 0) return TRUE;
	
	/*! Determine and apply the button function */
	
	if (moving_left == 1) {
        button_function = left_button_func;
        begin[0] = begin_left[0];
        begin[1] = begin_left[1];
	}
	else if (moving_middle == 1) {
        button_function = middle_button_func;
        begin[0] = begin_middle[0];
        begin[1] = begin_middle[1];
	}
	else if (moving_right == 1) {
        button_function = right_button_func;
        begin[0] = begin_right[0];
        begin[1] = begin_right[1];
	};
	
	if (button_function == ZOOM){
		factor = -0.5*(ypos-begin[1]);
		kemoview_zooming(factor, kemo_sgl);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, xpos, ypos, kemo_sgl);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, xpos, ypos, kemo_sgl);
	}
	else if (button_function == ROTATE) {
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]), kemo_sgl);
		kemoview_rollToTrackball( xpos, (-ypos), kemo_sgl);
		kemoview_drugging_addToRotationTrackball(kemo_sgl);
	}
	else if (button_function == SCALE){
		double current_scale = kemoview_get_view_parameter(kemo_sgl,
                                                           ISET_SCALE, 0);
        
		if (ypos < begin[1]) {
			factor = ONE + TWO_MILI*(begin[1]-ypos);
		}
		else if (ypos > begin[1]) {
			factor = ONE/(ONE  + TWO_MILI*(ypos-begin[1]));
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemo_sgl);
	};
    /* ! update private variables and redisplay */
	
	if (moving_left) {
		begin_left[0] = xpos;
		begin_left[1] = ypos;
    }
	else if(moving_middle) {
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	}
	else if(moving_right) {
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	return TRUE;
}

void set_GTKindowSize(int width, int height,
                      struct kemoviewer_type *kemo_sgl){
	gtk_widget_set_size_request(gl_area, width, height);
	kemoview_update_projection_by_viewer_size(width, height,
                                              width, height, kemo_sgl);
	glViewport(IZERO, IZERO, (GLint) width, (GLint) height);
};

gboolean mouseScrollCB(GtkWidget *widget, GdkEventScroll *event, gpointer user_data) {
/*	printf("mouseScrollCB %.1lf %.1lf\n", x, y);*/
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
    double newScale = event->x + event->y;
	kemoview_zooming(newScale, kemo_sgl);
	return TRUE;
}

void charFunCB(GtkWidget *widget, unsigned int charInfo) {
	printf("charFunCB %d\n", charInfo);
}


/*! This routine handles the arrow key operations */
static void keyFuncCB(GtkWidget *widget, GdkEventKey *event, gpointer user_data) {
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double x_dbl, y_dbl;
	double factor;
	
/*	printf("keyFuncCB %d %d %d %d\n", key, scancode, action, mods);	*/
	
	x_dbl = ZERO;
	y_dbl = ZERO;
	if (arrow_key_func == ZOOM){
		if (event->keyval == GTK_KEY_DOWN && event->type == GDK_KEY_PRESS){
			factor = ONE;
		}
		else if (event->keyval == GTK_KEY_UP && event->type == GDK_KEY_PRESS){
			factor = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_zooming(factor, kemo_sgl);
	}
	
	else if (arrow_key_func == WALKTO){
		begin[0] = ZERO;
		begin[1] = ZERO;
		x_dbl = ZERO;
		y_dbl = ZERO;
		if (event->keyval == GTK_KEY_DOWN && event->type == GDK_KEY_PRESS){
			x_dbl = ONE;
		}
		else if (event->keyval == GTK_KEY_UP && event->type == GDK_KEY_PRESS){
			y_dbl = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_mousedolly(begin, x_dbl, y_dbl, kemo_sgl);
	}
	
	else if (arrow_key_func == PAN){
		begin[0] = ZERO;
		begin[1] = ZERO;
		if (event->keyval == GTK_KEY_LEFT && event->type == GDK_KEY_PRESS){
			x_dbl = -ONE;
			y_dbl = ZERO;
		}
		else if (event->keyval == GTK_KEY_RIGHT && event->type == GDK_KEY_PRESS){
			x_dbl = ONE;
			y_dbl = ZERO;
		}
		else if (event->keyval == GTK_KEY_DOWN && event->type == GDK_KEY_PRESS){
			x_dbl = ZERO;
			y_dbl = ONE;
		}
		else if (event->keyval == GTK_KEY_UP && event->type == GDK_KEY_PRESS){
			x_dbl = ZERO;
			y_dbl = -ONE;
		};
		kemoview_mousepan(begin, x_dbl, y_dbl, kemo_sgl);
	}
	
	else if (arrow_key_func == ROTATE){
		if (event->keyval == GTK_KEY_LEFT && event->type == GDK_KEY_PRESS){
			x_dbl = begin[0] - TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (event->keyval == GTK_KEY_RIGHT && event->type == GDK_KEY_PRESS){
			x_dbl = begin[0] + TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (event->keyval == GTK_KEY_DOWN && event->type == GDK_KEY_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] + TEN;
		}
		else if (event->keyval == GTK_KEY_UP && event->type == GDK_KEY_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] - TEN;
		};
		kemoview_startTrackball( begin[0], (-begin[1]), kemo_sgl);
		kemoview_rollToTrackball( x_dbl, (-y_dbl));
		kemoview_drugging_addToRotationTrackball(kemo_sgl);
	}
	
	else if (arrow_key_func == SCALE){
		double current_scale = kemoview_get_view_parameter(kemo_sgl,
                                                           ISET_SCALE, 0);
        
		if (event->keyval == GTK_KEY_DOWN && event->type == GDK_KEY_PRESS)
		factor = ONE/(ONE + TWO_CENT);
		else if (event->keyval == GTK_KEY_UP && event->type == GDK_KEY_PRESS){
			factor = ONE + TWO_CENT;
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemo_sgl);
 	};
	
/*	gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */
	return;
}

GtkWidget * open_kemoviwer_gl_panel(int npixel_x, int npixel_y){
	gl_area = gtk_gl_area_new();
	if (!gl_area){
        printf("Baka!!\n");
		exit(1);
	};
	return gl_area;
};


void gtk_callbacks_init(struct kemoviewer_type *kemo_sgl){
	/* set callback for mouse button */
	g_signal_connect(G_OBJECT(gl_area), "button_press_event", G_CALLBACK(mouseButtonCB), NULL);
	g_signal_connect(G_OBJECT(gl_area), "button_release_event", G_CALLBACK(mouseButtonCB), NULL);
	/* set callback for cursor position */
	g_signal_connect(G_OBJECT(gl_area), "motion_notify_event", G_CALLBACK(mousePosCB), kemo_sgl);
	/* set callback for cursor position */
	g_signal_connect(G_OBJECT(gl_area), "scroll_event", G_CALLBACK(mouseScrollCB), NULL);
	
	/* Set callback for keyboard input */
	g_signal_connect(G_OBJECT(gl_area), "key-press-event", G_CALLBACK(keyFuncCB), kemo_sgl);
	
	gtk_widget_set_events(GTK_WIDGET(gl_area),
				GDK_BUTTON_PRESS_MASK |
				GDK_BUTTON_RELEASE_MASK |
				GDK_POINTER_MOTION_MASK |
				GDK_ENTER_NOTIFY_MASK |
				GDK_LEAVE_NOTIFY_MASK | 
				GDK_SCROLL_MASK
				);
	
	return;
}


void draw_fast(struct kemoviewer_type *kemo_sgl){
//    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW, kemo_sgl);
//    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl)
//  kemoview_mono_viewmatrix(kemo_sgl);
//    glDrawBuffer(GL_BACK);
//	kemoview_modify_view(kemo_sgl, kemoGL_GTK);
/*	gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */
	return;
};

void draw_full(struct kemoviewer_type *kemo_sgl){
//    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    glDrawBuffer(GL_BACK);
	kemoview_modify_view(kemo_sgl, kemoGL_GTK);
/*	gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */
	return;
};

static void write_rotate_quilt_views(struct kemoviewer_gl_type *kemo_gl,
                                     int iflag_img, struct kv_string *image_prefix,
                                     int i_axis, int inc_deg) {
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    int nimg_column = kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_COLUMN);
	int nimg_raw = kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);

    int i, i_quilt, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;
	
	kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_gl->kemoview_data);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, int_degree, kemo_gl->kemoview_data);
		for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
			draw_quilt(i_quilt, kemo_gl->kemoview_data);
            
            struct gl_texure_image *image_t = alloc_kemoview_gl_texure();
			kemoview_add_quilt_img(i_quilt, kemo_gl->kemoview_data,
                                   kemo_gl->kemo_VAOs, kemo_gl->kemo_shaders,
                                   image_t, quilt_image);
            dealloc_kemoview_gl_texure(image_t);
		};
		kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
											 (nimg_column * npix_x),
											 (nimg_raw * npix_y), quilt_image);
    };;
    free(quilt_image);
    free(image);
	return;
}

static void write_rotate_views(struct kemoviewer_gl_type *kemo_gl,
                               int iflag_img, struct kv_string *image_prefix,
                               int i_axis, int inc_deg) {
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    int i, i_quilt, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;
	
	kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_gl->kemoview_data);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, int_degree, kemo_gl->kemoview_data);
        kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_gl->kemoview_data);
        kemoview_mono_viewmatrix(kemo_gl->kemoview_data);
        glDrawBuffer(GL_BACK);
        kemoview_modify_view(kemo_gl->kemoview_data, kemoGL_GTK);
		
        struct gl_texure_image *image_t = alloc_kemoview_gl_texure();
        kemoview_get_gl_buffer_to_bmp(kemo_gl->kemoview_data, kemo_gl->kemo_VAOs,
                                      kemo_gl->kemo_shaders, image_t);
		kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                             image_t->nipxel_xy[0], image_t->nipxel_xy[1],
                                             image_t->texure_rgba);
        dealloc_kemoview_gl_texure(image_t);
    };
    free(image);

    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_gl->kemoview_data);
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_gl->kemoview_data);
    kemoview_mono_viewmatrix(kemo_gl->kemoview_data);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view(kemo_gl->kemoview_data, kemoGL_GTK);
    return;
}


static void write_quilt_evolution_views(struct kemoviewer_gl_type *kemo_gl,
                                        int iflag_img, struct kv_string *image_prefix,
                                        int ist_udt, int ied_udt, int inc_udt){
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int nimg_column = kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_COLUMN);
	int nimg_raw = kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    int i, i_quilt;

/*	gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_gl->kemoview_data);
			
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
				draw_quilt(i_quilt, kemo_gl->kemoview_data);
                
                struct gl_texure_image *image_t = alloc_kemoview_gl_texure();
				kemoview_add_quilt_img(i_quilt, kemo_gl->kemoview_data,
                                       kemo_gl->kemo_VAOs, kemo_gl->kemo_shaders,
                                       image_t, quilt_image);
                dealloc_kemoview_gl_texure(image_t);
			};
			kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
												 (nimg_column * npix_x),
												 (nimg_raw * npix_y), quilt_image);
		}
	}
    free(quilt_image);
    free(image);
	return;
};

static void write_evolution_views(struct kemoviewer_gl_type *kemo_gl,
                                  int iflag_img, struct kv_string *image_prefix,
                                  int ist_udt, int ied_udt, int inc_udt){
    int i_current = kemo_gl->kemoview_data->kemo_mul_psf->psf_a->file_step_disp;
    
    int npix_x = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_gl->kemoview_data, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    int i;

/*	gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			kemoview_viewer_evolution(i, kemo_gl->kemoview_data);
			
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			draw_full();
/*		    gtk_gl_area_swap_buffers(GTK_GL_AREA(gl_area)); */

            struct gl_texure_image *image_t = alloc_kemoview_gl_texure();
            kemoview_get_gl_buffer_to_bmp(kemo_gl->kemoview_data, kemo_gl->kemo_VAOs,
                                          kemo_gl->kemo_shaders, image_t);
            kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                                 image_t->nipxel_xy[0], image_t->nipxel_xy[1],
                                                 image_t->texure_rgba);
            dealloc_kemoview_gl_texure(image_t);
		}
	}
    free(image);

    kemoview_viewer_evolution(i_current, kemo_gl->kemoview_data);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    draw_full();
	return;
};

void sel_write_rotate_views(struct kemoviewer_gl_type *kemo_gl,
                            int iflag_img, struct kv_string *image_prefix,
                            int i_axis, int inc_deg) {
	if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) != 0){
		write_rotate_quilt_views(kemo_gl->kemoview_data, iflag_img, image_prefix,
                                 i_axis, inc_deg);
	} else {
		write_rotate_views(kemo_gl->kemoview_data, iflag_img, image_prefix, i_axis, inc_deg);
	};
    draw_full();
	return;
}

void sel_write_evolution_views(struct kemoviewer_gl_type *kemo_gl,
                               int iflag_img, struct kv_string *image_prefix,
							   int ist_udt, int ied_udt, int inc_udt){
	if(kemoview_get_quilt_nums(kemo_gl->kemoview_data, ISET_QUILT_MODE) != 0){
		write_quilt_evolution_views(kemo_gl, iflag_img, image_prefix,
									ist_udt, ied_udt, inc_udt);
	} else {
		write_evolution_views(kemo_gl, iflag_img, image_prefix,
							  ist_udt, ied_udt, inc_udt);
	};
	draw_full();
    free(image);
};


void set_viewtype_mode(int selected){
	
	if(selected == RESET) selected = VIEW_3D;

	if(selected == VIEW_3D
				|| selected == VIEW_STEREO){
		left_button_func = ROTATE;
	}
	else if(selected == VIEW_MAP
				|| selected == VIEW_XY
				|| selected == VIEW_XZ
				|| selected == VIEW_YZ) {
		left_button_func = PAN;
	};
	
	kemoview_set_viewtype(selected, kemo_sgl);
	return;
}

