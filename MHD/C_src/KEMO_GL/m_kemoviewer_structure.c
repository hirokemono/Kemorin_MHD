
/* m_kemoviewer_structure.c*/

#include "m_kemoviewer_structure.h"

static void kemoview_allocate_pointers(struct kemoviewer_type *kemoviewer_data){
    kemoviewer_data->view_s = (struct view_element *) malloc(sizeof(struct view_element));
	
    kemoviewer_data->kemo_buffers = init_kemoview_buffers();

    kemoviewer_data->kemo_mesh =  init_kemoview_mesh();
    kemoviewer_data->kemo_mul_psf =   init_kemoview_mul_psf();
    kemoviewer_data->kemo_fline =     init_kemoview_fline();
    kemoviewer_data->kemo_tracer =    init_kemoview_tracer();
	
    kemoviewer_data->psf_ucd_tmp = (struct psf_data *) malloc(sizeof(struct psf_data));
    return;
}

struct kemoviewer_type * kemoview_allocate_single_viwewer_struct(void){
	/*! Initialize mesh data*/
	struct kemoviewer_type *kemoviewer_data 
		= (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
    kemoviewer_data->image_format_id = SAVE_PNG;
    
	kemoview_allocate_pointers(kemoviewer_data);
    
	init_kemoview_array(kemoviewer_data->kemo_mul_psf->psf_a);
        
	init_kemoviewer(IZERO, kemoviewer_data->kemo_mesh->mesh_d,
                    kemoviewer_data->kemo_mesh->mesh_m,
                    kemoviewer_data->view_s);
	init_fline_parameters(kemoviewer_data->kemo_fline->fline_m);
    init_fline_parameters(kemoviewer_data->kemo_tracer->tracer_m);

    return kemoviewer_data;
}

void kemoview_deallocate_pointers(struct kemoviewer_type *kemoviewer_data){
	free(kemoviewer_data->view_s);
	free(kemoviewer_data->psf_ucd_tmp);
	
	dealloc_kemoview_mesh(kemoviewer_data->kemo_mesh);
    dealloc_kemoview_tracer(kemoviewer_data->kemo_tracer);
	dealloc_kemoview_fline(kemoviewer_data->kemo_fline);
    dealloc_kemoview_mul_psf(kemoviewer_data->kemo_mul_psf);
	
    dealloc_kemoview_buffers(kemoviewer_data->kemo_buffers);
	return;
}


void kemoview_alloc_kvstringitem(unsigned long length, struct kv_string *kvstring){
	alloc_kvstringitem(length, kvstring);
	return;
};
struct kv_string* kemoview_alloc_kvstring(void){return alloc_kvstring();};
struct kv_string* kemoview_init_kvstring_by_string(const char *org_string){
	return init_kvstring_by_string(org_string);
};
void kemoview_free_kvstring(struct kv_string *kvstring){
	dealloc_kvstring(kvstring);
	return;
};
void kemoview_alloc_copy_string(const char *org_string, struct kv_string *kvstring){
	alloc_copy_string(org_string, kvstring);
	return;
};

/* Routines for Kemoviewer arrays */

void kemoview_set_current_viewer_id(int id_window, struct mul_kemoviewer_type *kemoview_array){
    if(id_window > kemoview_array->num_window){printf("Something wrong in window ID \n");};
	kemoview_array->id_current = id_window;
	return;
}

/*  Routines for threads setting */

void kemoview_set_number_of_threads(int input, struct kemoviewer_type *kemoviewer){
    set_number_of_threads(input, kemoviewer->kemo_buffers);
}
int kemoview_get_number_of_threads(struct kemoviewer_type *kemoviewer){
    return send_number_of_threads(kemoviewer->kemo_buffers);
}


/* Routines for draw by OpenGL */

void kemoview_init_background_color(struct kemoviewer_type *kemoviewer){
    init_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                           kemoviewer->kemo_mesh->text_color);
    set_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                          kemoviewer->kemo_mesh->text_color);
};
void kemoview_set_background_color(float color[4],
                                   struct kemoviewer_type *kemoviewer) {
    copy_rgba_color_c(color, kemoviewer->kemo_mesh->bg_color);
    set_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                          kemoviewer->kemo_mesh->text_color);
};
void kemoview_get_background_color(struct kemoviewer_type *kemoviewer,
                                   float color[4]){
    copy_rgba_color_c(kemoviewer->kemo_mesh->bg_color, color);
    return;
};



/* Routines for menu selection */
int kemoview_open_data(struct kv_string *filename,
                       struct kemoviewer_type *kemoviewer){
	int iflag_datatype = kemoviewer_open_data(filename,
                                              kemoviewer->kemo_mesh,
                                              kemoviewer->kemo_mul_psf,
                                              kemoviewer->kemo_fline,
                                              kemoviewer->kemo_tracer,
                                              kemoviewer->psf_ucd_tmp,
                                              kemoviewer->view_s);
	return iflag_datatype;
}

void kemoview_close_mesh_view(struct kemoviewer_type *kemoviewer){
	close_mesh_view(kemoviewer->kemo_mesh);
	return;
}

int kemoview_close_PSF_view(struct kemoviewer_type *kemoviewer){
	close_PSF_view(kemoviewer->kemo_mul_psf);
    return get_PSF_loaded_params(kemoviewer->kemo_mul_psf, NUM_LOADED);
}

void kemoview_close_fieldline_view(struct kemoviewer_type *kemoviewer){
	close_fieldline_view(kemoviewer->kemo_fline);
	return;
}
void kemoview_close_tracer_view(struct kemoviewer_type *kemoviewer){
    close_tracer_view(kemoviewer->kemo_tracer);
    return;
}

void kemoview_write_modelview_file(struct kv_string *filename,
                                   struct kemoviewer_type *kemoviewer){
	write_GL_modelview_file(filename, kemoviewer->view_s);
}
void kemoview_load_modelview_file(struct kv_string *filename,
                                  struct kemoviewer_type *kemoviewer){
	read_GL_modelview_file(filename, kemoviewer->view_s);
}



void kemoview_viewer_evolution(int istep, struct kemoviewer_type *kemoviewer){
	psf_viewer_evolution(istep, kemoviewer->kemo_mul_psf->psf_a);
	evolution_fline_viewer(kemoviewer->kemo_fline, kemoviewer->psf_ucd_tmp,
                           kemoviewer->kemo_mul_psf->psf_a->istep_sync);
    evolution_psf_viewer(kemoviewer->psf_ucd_tmp, kemoviewer->kemo_mul_psf);
	return;
}


void kemoview_set_viewtype(int sel, struct kemoviewer_type *kemoviewer){
    set_viewtype(kemoviewer->view_s, sel);
}

void kemoview_set_coastline_radius(double radius, struct kemoviewer_type *kemoviewer){
    kemoviewer->kemo_mesh->mesh_m->radius_coast = radius;
};
double kemoview_get_coastline_radius(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->radius_coast;
};

void kemoview_set_inner_core_radius(double r_ICB, struct kemoviewer_type *kemoviewer){
    kemoviewer->kemo_mesh->mesh_m->r_ICB = r_ICB;
};
double kemoview_get_inner_core_radius(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->r_ICB;
};


void kemoview_set_object_property_flags(int selected, int iflag,
                                        struct kemoviewer_type *kemoviewer){
	if (selected == AXIS_TOGGLE){
        set_axis_flag(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == AXIS_POSITION){
        set_axis_position(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == COASTLINE_SWITCH){
        set_coastline_flag(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == SPHEREGRID_SWITCH){
        set_sphere_grid_flag(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == TANGENT_CYLINDER_SWITCH){
        set_tangent_cylinder_flag(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == SHADING_SWITCH){
        kemoviewer->view_s->shading_mode = iflag;
    }else if(selected == POLYGON_SWITCH){
        set_polygon_mode(iflag, kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == TIME_LABEL_SWITCH){
        set_draw_time_flag(iflag, kemoviewer->kemo_mul_psf);
    }else if(selected == FILE_STEP_LABEL_SWITCH){
        set_draw_file_step_flag(iflag, kemoviewer->kemo_mul_psf);
    };
	return;
}

int kemoview_get_object_property_flags(struct kemoviewer_type *kemoviewer, int selected){
	if (selected == AXIS_TOGGLE){
        return kemoviewer->kemo_mesh->mesh_m->iflag_draw_axis;
    }else if(selected == AXIS_POSITION){
        return kemoviewer->kemo_mesh->mesh_m->iflag_axis_position;
    }else if(selected == COASTLINE_SWITCH){
        return kemoviewer->kemo_mesh->mesh_m->iflag_draw_coast;
    }else if(selected == SPHEREGRID_SWITCH){
        return kemoviewer->kemo_mesh->mesh_m->iflag_draw_sph_grid;
    }else if(selected == TANGENT_CYLINDER_SWITCH){
        return kemoviewer->kemo_mesh->mesh_m->iflag_draw_tangent_cyl;
    }else if(selected == SHADING_SWITCH){
        return kemoviewer->view_s->shading_mode;
    }else if(selected == POLYGON_SWITCH){
        return kemoviewer->kemo_mesh->mesh_m->polygon_mode;
    }else if(selected == TIME_LABEL_SWITCH){
        return get_draw_time_flag(kemoviewer->kemo_mul_psf);
    }else if(selected == FILE_STEP_LABEL_SWITCH){
        return get_draw_file_step_flag(kemoviewer->kemo_mul_psf);
    }else if(selected == TIME_LABEL_AVAIL){
        return get_avail_time_flag(kemoviewer->kemo_mul_psf);
    }else if(selected == FILE_STEP_LABEL_AVAIL){
        return get_avail_file_step_flag(kemoviewer->kemo_mul_psf);
    };
	return 0;
}

int kemoview_toggle_object_properties(int selected, struct kemoviewer_type *kemoviewer){
	if (selected == AXIS_TOGGLE){
        return toggle_draw_axis(kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == COASTLINE_SWITCH){
        return toggle_coastline_flag(kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == SPHEREGRID_SWITCH){
        return toggle_sphere_grid_flag(kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == SHADING_SWITCH){
        kemoviewer->view_s->shading_mode = toggle_value_c(kemoviewer->view_s->shading_mode);
		return kemoviewer->view_s->shading_mode;
	}else if(selected == POLYGON_SWITCH){
        return toggle_polygon_mode(kemoviewer->kemo_mesh->mesh_m);
    }else if(selected == TIME_LABEL_SWITCH){
        return toggle_draw_time_flag(kemoviewer->kemo_mul_psf);
    }else if(selected == FILE_STEP_LABEL_SWITCH){    
        return toggle_draw_file_step_flag(kemoviewer->kemo_mul_psf);
    };
	return 0;
}

/* Shader controls */

void kemoview_init_lighting(struct kemoviewer_type *kemoviewer){
    init_kemoview_perspective(kemoviewer->view_s);
    init_projection_struct(kemoviewer->view_s);
    return;
}

void kemoview_alloc_phong_light_list(int num, struct kemoviewer_type *kemoviewer){
	alloc_phong_light_list(kemoviewer->kemo_buffers->kemo_lights, num);
};

void kemoview_delete_phong_light_list(int i_delete, struct kemoviewer_type *kemoviewer){
	delete_phong_light_list(kemoviewer->kemo_buffers->kemo_lights, i_delete);
};
void kemoview_add_phong_light_list(int i_add, float r, float t, float p,
                                   struct kemoviewer_type *kemoviewer){
	add_phong_light_list(kemoviewer->view_s,
                         kemoviewer->kemo_buffers->kemo_lights,
                         i_add, r, t, p);
};

void kemoview_init_phong_light_list(struct kemoviewer_type *kemoviewer){
	init_phong_light_list(kemoviewer->view_s,
                          kemoviewer->kemo_buffers->kemo_lights);
};


void kemoview_set_each_light_position(int i_point, float r, float t, float p,
                                      struct kemoviewer_type *kemoviewer){
	set_each_light_position(kemoviewer->view_s,
                            kemoviewer->kemo_buffers->kemo_lights,
                            i_point, r, t, p);
};
int kemoview_get_num_light_position(struct kemoviewer_type *kemoviewer){
	return send_num_light_position(kemoviewer->kemo_buffers->kemo_lights);
};
void kemoview_get_each_light_rtp(struct kemoviewer_type *kemoviewer,
                                 int i_point, float *r, float *t, float *p){
	send_each_light_rtp(kemoviewer->kemo_buffers->kemo_lights, i_point, r, t, p);
};
void kemoview_get_each_light_xyz(struct kemoviewer_type *kemoviewer,
                                 int i_point, float *x, float *y, float *z){
    send_each_light_xyz(kemoviewer->kemo_buffers->kemo_lights, i_point, x, y, z);
};

void kemoview_set_material_parameter(int itype, float value,
                                     struct kemoviewer_type *kemoviewer){
	set_matrial_parameter(itype, value, kemoviewer->kemo_buffers->kemo_lights);
};
float kemoview_get_material_parameter(struct kemoviewer_type *kemoviewer, int itype){
	return get_matrial_parameter(itype, kemoviewer->kemo_buffers->kemo_lights);
};


int kemoview_get_view_type_flag(struct kemoviewer_type *kemoviewer){
    return kemoviewer->view_s->iflag_view_type;
};

void kemoview_add_bgra_to_quilt(struct kemoviewer_type *kemoviewer,
                                int istep_quilt, int npix_x, int npix_y,
                                unsigned char *glimage,
                                unsigned char *fliped_quilt){
    quilt_bitmap_by_bgra(kemoviewer->view_s->num_columns,
                         kemoviewer->view_s->num_raws,
                         istep_quilt, npix_x, npix_y, glimage,
                         fliped_quilt);
    return;
};


void kemoview_set_PSF_by_rgba_texture(int width, int height, 
                                      const unsigned char *bgra_in,
                                      struct kemoviewer_type *kemoviewer){
    set_texture_psf_from_bgra(kemoviewer->kemo_mul_psf->psf_a,
                              width, height, bgra_in);
};

void kemoview_const_buffers(struct kemoviewer_type *kemoviewer){
    set_kemoviewer_buffers(kemoviewer->kemo_mul_psf,
                           kemoviewer->kemo_fline,
                           kemoviewer->kemo_tracer,
                           kemoviewer->kemo_mesh,
                           kemoviewer->view_s,
                           kemoviewer->kemo_buffers);
    return;
};
void kemoview_transparent_buffers(struct kemoviewer_type *kemoviewer){
    set_transparent_buffers(kemoviewer->kemo_mul_psf, kemoviewer->kemo_mesh,
                            kemoviewer->view_s, kemoviewer->kemo_buffers);
    return;
};
void kemoview_fast_buffers(struct kemoviewer_type *kemoviewer){
    set_fast_buffers(kemoviewer->kemo_mul_psf, kemoviewer->kemo_fline,
                     kemoviewer->kemo_mesh, kemoviewer->view_s,
                     kemoviewer->kemo_buffers);
    return;
};


void kemoview_mono_viewmatrix(struct kemoviewer_type *kemoviewer){
    modify_mono_viewmat(kemoviewer->view_s);
};
void kemoview_step_viewmatrix(int istep, struct kemoviewer_type *kemoviewer){
    modify_step_viewmat(istep, kemoviewer->view_s);
};
void kemoview_left_viewmatrix(struct kemoviewer_type *kemoviewer){
    modify_left_viewmat(kemoviewer->view_s);
};
void kemoview_right_viewmatrix(struct kemoviewer_type *kemoviewer){
    modify_right_viewmat(kemoviewer->view_s);
};

void kemoviewer_reset_to_init_angle(struct kemoviewer_type *kemoviewer){
    reset_all_view_parameter(kemoviewer->view_s);
    init_rot_animation(kemoviewer->view_s);
};


void kemoview_set_retinamode(int i_retina, struct kemoviewer_type *kemoviewer){
    set_gl_retinamode(kemoviewer->view_s, i_retina);
}
int kemoview_get_retinamode(struct kemoviewer_type *kemoviewer){
    return send_gl_retinamode(kemoviewer->view_s);
}

void kemoview_set_windowsize(int npixel_x, int npixel_y,
                             int nwindow_x, int nwindow_y,
                             struct kemoviewer_type *kemoviewer){
    set_gl_windowsize(kemoviewer->view_s,
                      npixel_x, npixel_y,
                      nwindow_x, nwindow_y);
};
void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y,
                                               int nwindow_x, int nwindow_y,
                                               struct kemoviewer_type *kemoviewer){
	update_projection_by_windowsize(kemoviewer->view_s,
                                    npixel_x, npixel_y,
                                    nwindow_x, nwindow_y);
};
void kemoview_set_message_opacity(float opacity,
                                  struct kemoviewer_type *kemoviewer){
    kemoviewer->kemo_buffers->MESSAGE_bufs->message_buf->text_opacity = opacity;
    return;
}

void kemoview_set_view_integer(int selected, int ivalue,
                               struct kemoviewer_type *kemoviewer){
	if(selected == ISET_ROTATE_AXIS){
		set_gl_animation_rot_axis(kemoviewer->view_s, ivalue);
	}else if(selected == ISET_ROTATE_INCREMENT){
		set_gl_animation_rot_angle(kemoviewer->view_s, ivalue);
    }else if(selected == ISET_DRAW_MODE){
        set_gl_draw_mode(kemoviewer->view_s, ivalue);
    }else if(selected == LIGHTING_CHECK){
        set_lighting_check_flag(kemoviewer->view_s, ivalue);
    }else if(selected == COASTLINE_TUBE){
        set_coastline_tube_flag(kemoviewer->view_s, ivalue);
    }else if(selected == NUM_TUBE_CORNERS_FLAG){
        set_gl_tube_corners(kemoviewer->view_s, ivalue);
    }else if(selected == IMAGE_FORMAT_FLAG){
        set_default_image_format_id(kemoviewer, ivalue);
	}
	return;
};

int kemoview_get_view_integer(struct kemoviewer_type *kemoviewer,
                               int selected){
    int ivalue = 0;
    if(selected == ISET_PIXEL_X){
        ivalue = send_gl_windowsize_x(kemoviewer->view_s);
    }else if(selected == ISET_PIXEL_Y){
        ivalue = send_gl_windowsize_y(kemoviewer->view_s);
    }else if(selected == ISET_DRAW_MODE){
        ivalue = send_gl_draw_mode(kemoviewer->view_s);
    }else if(selected == LIGHTING_CHECK){
        ivalue = send_lighting_check_flag(kemoviewer->view_s);
    }else if(selected == COASTLINE_TUBE){
        ivalue = send_coastline_tube_flag(kemoviewer->view_s);
    }else if(selected == NUM_TUBE_CORNERS_FLAG){
        ivalue = send_gl_tube_corners(kemoviewer->view_s);
    }else if(selected == IMAGE_FORMAT_FLAG){
        ivalue = send_default_image_format_id(kemoviewer);
    }
    return ivalue;
};

void kemoview_set_view_parameter(int selected, int i, double value,
                                 struct kemoviewer_type *kemoviewer){
	if(selected == ISET_ROTATE){
		set_gl_rotation_parameter(kemoviewer->view_s, i, value);
	}else if(selected == ISET_SHIFT){
		set_gl_shift_vector(kemoviewer->view_s, i, value);
	}else if(selected == ISET_SCALE){
		set_gl_scalar_scale_factor(kemoviewer->view_s, value);
		
	}else if(selected == ISET_APERTURE){
		set_gl_projection_aperture(kemoviewer->view_s, value);
	};
	return;
};
void kemoview_set_stereo_parameter(int selected, double value,
                                   struct kemoviewer_type *kemoviewer){
    if(selected == ISET_FOCUS){
        set_gl_focal_length(kemoviewer->view_s, value);
    }else if(selected == ISET_EYESEP){
        set_gl_eye_separation_distance(kemoviewer->view_s, value);
    }else if(selected == ISET_EYEAGL){
        set_gl_eye_separation_angle(kemoviewer->view_s, value);
    };
    return;
};
void kemoview_set_quilt_nums(int selected, int ivalue,
                             struct kemoviewer_type *kemoviewer){
    if(selected == ISET_QUILT_MODE){
        set_quilt_mode_flag(kemoviewer->view_s, ivalue);
    }else if(selected == ISET_QUILT_RAW){
        set_quilt_image_num_raws(kemoviewer->view_s, ivalue);
    }else if(selected == ISET_QUILT_COLUMN){
        set_quilt_image_num_columns(kemoviewer->view_s, ivalue);
    }else if(selected == ISET_QUILT_NUM){
        set_quilt_image_num_views(kemoviewer->view_s, ivalue);
    };
    return;
};

double kemoview_get_view_parameter(struct kemoviewer_type *kemoviewer,
                                   int selected, int i){
	double value = 0.0;
	if(selected == ISET_ROTATE){
		value =  send_gl_rotation_parameter(kemoviewer->view_s, i);
	}else if(selected == ISET_SHIFT){
		value =  send_gl_shift_vector(kemoviewer->view_s, i);
	}else if(selected == ISET_VWPOINT){
		value =  send_gl_lookat_vector(kemoviewer->view_s, i);
	}else if(selected == ISET_SCALE){
		value =  send_scalar_scale_factor(kemoviewer->view_s);

	}else if(selected == ISET_APERTURE){
		value =  send_gl_projection_aperture(kemoviewer->view_s);
	}else if(selected == ISET_NEAR){
		value =  send_gl_projection_near(kemoviewer->view_s);
	}else if(selected == ISET_FAR){
		value =  send_gl_projection_far(kemoviewer->view_s);
	}else if(selected == ISET_ASPECT){
		value =  send_gl_projection_aspect(kemoviewer->view_s);
		
	}else if(selected == ISET_FOCUS){
		value =  send_gl_stereo_focus(kemoviewer->view_s);
	}else if(selected == ISET_EYESEP){
		value =  send_gl_stereo_eyeseparation(kemoviewer->view_s);
    }else if(selected == ISET_EYEAGL){
        value =  send_gl_stereo_eparation_angle(kemoviewer->view_s);
	};
	return value;
};
int kemoview_get_quilt_nums(struct kemoviewer_type *kemoviewer,
                            int selected){
    int num = 1;
    if(selected == ISET_QUILT_MODE){
        num =  send_quilt_mode_flag(kemoviewer->view_s);
    }else if(selected == ISET_QUILT_RAW){
        num =  send_quilt_image_num_raws(kemoviewer->view_s);
    }else if(selected == ISET_QUILT_COLUMN){
        num =  send_quilt_image_num_columns(kemoviewer->view_s);
    }else if(selected == ISET_QUILT_NUM){
        num =  send_quilt_image_num_views(kemoviewer->view_s);
    };
    return num;
}

void kemoview_mousedolly(double start[2], double x_dolly, double y_dolly,
                         struct kemoviewer_type *kemoviewer){
	gl_mousedolly_struct(kemoviewer->view_s, start, x_dolly, y_dolly);
}
void kemoview_mousepan(double start[2], double x_pan, double y_pan,
                       struct kemoviewer_type *kemoviewer){
	gl_mousepan_struct(kemoviewer->view_s, start, x_pan, y_pan);
}
void kemoview_zooming(double wheelDelta, struct kemoviewer_type *kemoviewer){
	gl_zooming_struct(kemoviewer->view_s, wheelDelta);
}

/* called with the start position and the window origin + size */
void kemoview_startTrackball(double x, double y,
                             struct kemoviewer_type *kemoviewer){
    gl_startTrackball(x, y, kemoviewer->view_s);
};
/* calculated rotation based on current mouse position */
void kemoview_rollToTrackball(double x, double y,
                              struct kemoviewer_type *kemoviewer){
    gl_rollToTrackball (x, y, kemoviewer->view_s);
};
/* add a GL rotation (dA) to an existing GL rotation (A) */
void kemoview_drugging_addToRotationTrackball(struct kemoviewer_type *kemoviewer){
    gl_drag_addToRotationTrackball(kemoviewer->view_s);
}

void kemoview_animation_add_rotation(double dt,
                                     struct kemoviewer_type *kemoviewer){
    add_animation_rotation(kemoviewer->view_s, dt);
};
void kemoview_reset_animation(struct kemoviewer_type *kemoviewer){
    reset_rot_animation(kemoviewer->view_s);
};


void kemoview_set_coastline_thickness_w_exp(double value, int i_digit,
                                            struct kemoviewer_type *kemoviewer){
    set_coastline_thickness_w_exp(value, i_digit, kemoviewer->view_s);
    return;
};
void kemoview_get_coastline_thickness_w_exp(struct kemoviewer_type *kemoviewer,
                                            double *value, int *i_digit){;
    get_coastline_thickness_w_exp(kemoviewer->view_s, value, i_digit);
    return;
};

void kemoview_set_axis_thickness_w_exp(double value, int i_digit,
                                       struct kemoviewer_type *kemoviewer){
    set_axis_thickness_w_exp(value, i_digit, kemoviewer->view_s);
    return;
};
void kemoview_get_axis_thickness_w_exp(struct kemoviewer_type *kemoviewer,
                                       double *value, int *i_digit){
    get_axis_thickness_w_exp(kemoviewer->view_s, value, i_digit);
    return;
};


/* Subroutines for surface rendering */
int kemoview_get_PSF_maximum_load(struct kemoviewer_type *kemoviewer){
    return get_PSF_maximum_load(kemoviewer->kemo_mul_psf->psf_a);
};

void kemoview_set_PSF_loaded_params(int selected, int input,
                                    struct kemoviewer_type *kemoviewer){
	set_PSF_loaded_params(selected, input, kemoviewer->kemo_mul_psf);
};

int kemoview_get_PSF_loaded_params(struct kemoviewer_type *kemoviewer, int selected){
	return get_PSF_loaded_params(kemoviewer->kemo_mul_psf, selected);
};
int kemoview_get_PSF_loaded_flag(struct kemoviewer_type *kemoviewer, int id_psf){
	return get_PSF_loaded_flag(id_psf, kemoviewer->kemo_mul_psf->psf_a);
};

static struct psf_menu_val * select_viz_menu_structure(int id_model, struct kemoviewer_type *kemoviewer){
    struct psf_menu_val * viz_menu;
    if(id_model == FIELDLINE_RENDERING){
        viz_menu = kemoviewer->kemo_fline->fline_m;
    }else if(id_model == TRACER_RENDERING){
        viz_menu = kemoviewer->kemo_tracer->tracer_m;
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        viz_menu = kemoviewer->kemo_mul_psf->psf_m[i_current];
    }
    return viz_menu;
}


void kemoview_get_full_path_file_name(struct kemoviewer_type *kemoviewer,
                                      int id_model, struct kv_string *ucd_m){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    alloc_set_ucd_file_name_by_psf(viz_menu, ucd_m);
}

int kemoview_get_full_path_file_prefix_step(struct kemoviewer_type *kemoviewer, int id_model,
                                            struct kv_string *psf_filehead, int *i_file_step){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    long i_format = send_VIZ_file_prefix_step_format(viz_menu, psf_filehead, i_file_step);
    return i_format;
}

void kemoview_set_VIZ_field_param(int input, int id_model, int selected,
                                  struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_each_PSF_field_param(selected, input,
                                 kemoviewer->kemo_fline->fline_d,
                                 kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_each_PSF_field_param(selected, input,
                                 kemoviewer->kemo_tracer->tracer_d,
                                 kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_each_PSF_field_param(selected, input,
                                 kemoviewer->kemo_mul_psf->psf_d[i_current],
                                 kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
};

int kemoview_get_VIZ_field_param(struct kemoviewer_type *kemoviewer,
                                 int id_model, int selected){
    long index = 0;
    if(id_model == FIELDLINE_RENDERING){
        index = get_VIZ_field_param(selected,
                                    kemoviewer->kemo_fline->fline_d,
                                    kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        index = get_VIZ_field_param(selected,
                                    kemoviewer->kemo_tracer->tracer_d,
                                    kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        index = get_VIZ_field_param(selected,
                                    kemoviewer->kemo_mul_psf->psf_d[i_current],
                                    kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
    return (int) index;
};

void kemoview_set_VIZ_draw_flag(int id_model, int iflag,
                                struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_draw_psf_solid(iflag, kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_draw_psf_solid(iflag, kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_draw_psf_solid(iflag, kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
}

int kemoview_check_all_VIZ_draw_flags(struct kemoviewer_type *kemoviewer){
    int i;
    int iflag = send_draw_psf_solid(kemoviewer->kemo_fline->fline_m)
            + send_draw_psf_solid(kemoviewer->kemo_tracer->tracer_m);
    for(i=0;i<kemoviewer->kemo_mul_psf->psf_a->num_loaded; i++){
        iflag = iflag + send_draw_psf_solid(kemoviewer->kemo_mul_psf->psf_m[i]);
    }
    if(iflag > 0) return 1;
    return 0;
}

int kemoview_get_VIZ_draw_flags(struct kemoviewer_type *kemoviewer,
                                int id_model){
    int iflag = 0;
    
    if(id_model == FIELDLINE_RENDERING){
        iflag =  send_draw_psf_solid(kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        iflag =  send_draw_psf_solid(kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        iflag =  send_draw_psf_solid(kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
    return iflag;
}


long kemoview_get_VIZ_num_component(struct kemoviewer_type *kemoviewer,
                                    int id_model, int i){
    long ncomp;
    if(id_model == FIELDLINE_RENDERING){
        ncomp =  send_VIZ_num_component(kemoviewer->kemo_fline->fline_d, i);
    }else if(id_model == TRACER_RENDERING){
        ncomp =  send_VIZ_num_component(kemoviewer->kemo_tracer->tracer_d, i);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        ncomp =  send_VIZ_num_component(kemoviewer->kemo_mul_psf->psf_d[i_current], i);
    }
    return ncomp;
};
void kemoview_get_VIZ_field_name(struct kemoviewer_type *kemoviewer, int id_model,
                                 struct kv_string *colorname, int i){
    if(id_model == FIELDLINE_RENDERING){
        send_VIZ_field_name(kemoviewer->kemo_fline->fline_d, colorname, i);
    }else if(id_model == TRACER_RENDERING){
        send_VIZ_field_name(kemoviewer->kemo_tracer->tracer_d, colorname, i);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        send_VIZ_field_name(kemoviewer->kemo_mul_psf->psf_d[i_current], colorname, i);
    }

    return;
};

void kemoview_set_PSF_polygon_mode(int iflag, struct kemoviewer_type *kemoviewer){
    set_PSF_polygon_mode(iflag, kemoviewer->kemo_mul_psf);
};
void kemoview_set_PSF_tangential_vec_mode(int iflag, struct kemoviewer_type *kemoviewer){
    set_PSF_tangential_vec_mode(iflag, kemoviewer->kemo_mul_psf);
};

int kemoview_get_PSF_draw_refv(struct kemoviewer_type *kemoviewer){
    return get_PSF_draw_refv(kemoviewer->kemo_mul_psf);
};

static void reset_colorbar_flag(struct kemoview_mul_psf *kemo_mul_psf,
                                struct kemoview_fline *kemo_fline,
                                struct kemoview_tracer *kemo_tracer){
    int i;
    kemo_fline->fline_m->iflag_draw_cbar =   0;
    kemo_tracer->tracer_m->iflag_draw_cbar = 0;
    for(i=0; i< kemo_mul_psf->psf_a->nmax_loaded; i++){
        kemo_mul_psf->psf_m[i]->iflag_draw_cbar = 0;
    }
    return;
}

void kemoview_set_colorbar_draw_flag(int iflag, int id_model, 
                                     struct kemoviewer_type *kemoviewer){
    reset_colorbar_flag(kemoviewer->kemo_mul_psf,
                        kemoviewer->kemo_fline,
                        kemoviewer->kemo_tracer);
    
    if(id_model == FIELDLINE_RENDERING){
        set_draw_VIZ_cbar(iflag, kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_draw_VIZ_cbar(iflag, kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_draw_VIZ_cbar(iflag, kemoviewer->kemo_mul_psf->psf_m[i_current]);
    };
}

int kemoview_get_colorbar_draw_flag(struct kemoviewer_type *kemoviewer,
                                    int id_model){
    int iflag;
    if(id_model == FIELDLINE_RENDERING){
        iflag = send_draw_VIZ_cbar(kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        iflag = send_draw_VIZ_cbar(kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        iflag = send_draw_VIZ_cbar(kemoviewer->kemo_mul_psf->psf_m[i_current]);
    };
    return iflag;
}


void kemoview_set_VIZ_vector_draw_flags(int iflag, int id_model,
                                        struct kemoviewer_type *kemoviewer){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    set_draw_VIZ_vector(iflag, viz_menu);
}
int kemoview_get_VIZ_vector_draw_flags(struct kemoviewer_type *kemoviewer,
                                       int id_model){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    return send_draw_VIZ_vector(viz_menu);
}

void kemoview_set_PSF_draw_flags(int iflag, int selected, 
                                 struct kemoviewer_type *kemoviewer){    
    set_each_PSF_draw_switch(selected, iflag, kemoviewer->kemo_mul_psf);
}
int kemoview_get_PSF_draw_flags(struct kemoviewer_type *kemoviewer,
                                int selected){
	return get_each_PSF_draw_switch(selected, kemoviewer->kemo_mul_psf);
}

void kemoview_update_PSF_textured_id(struct kemoviewer_type *kemoviewer){
    update_PSF_textured_id(kemoviewer->kemo_mul_psf);
    return;
};
void kemoview_set_PSF_color_param(int selected, int input,
                                  struct kemoviewer_type *kemoviewer){
	set_each_PSF_color_param(selected, input, kemoviewer->kemo_mul_psf);
	return;
};

int kemoview_get_VIZ_patch_color_mode(struct kemoviewer_type *kemoviewer,
                                      int id_model){
    int iflag = 0;
    if(id_model == FIELDLINE_RENDERING){
        iflag = send_VIZ_patch_color_mode(kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        iflag = send_VIZ_patch_color_mode(kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        iflag = send_VIZ_patch_color_mode(kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
    return iflag;
};

void kemoview_set_PSF_patch_color_mode(int input, struct kemoviewer_type *kemoviewer){
    int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
    set_VIZ_patch_color_mode(kemoviewer->kemo_mul_psf->psf_m[i_current], input);
    if(input != TEXTURED_SURFACE){kemoviewer->kemo_mul_psf->psf_a->ipsf_texured = -1;};
};

void kemoview_set_VIZ_patch_color_mode(int input, int id_model,
                                       struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_patch_color_mode(kemoviewer->kemo_fline->fline_m, input);
    }else{
        set_VIZ_patch_color_mode(kemoviewer->kemo_tracer->tracer_m, input);
    }
};


int kemoview_get_PSF_color_param(struct kemoviewer_type *kemoviewer,
                                 int selected){
    return get_each_PSF_color_param(selected, kemoviewer->kemo_mul_psf);
};

void kemoview_set_colormap_param(int id_model, int selected, int input,
                                 struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_viz_colormap_param(selected, input,
                               kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_viz_colormap_param(selected, input,
                               kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_viz_colormap_param(selected, input,
                               kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
    return;
};

int kemoview_get_viz_colormap_param(struct kemoviewer_type *kemoviewer,
                                    int id_model, int selected){
    int iflag = 0;
    if(id_model == FIELDLINE_RENDERING){
        iflag = get_viz_colormap_param(selected,
                                       kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        iflag = get_viz_colormap_param(selected,
                                       kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        iflag = get_viz_colormap_param(selected,
                                       kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
    return iflag;
};

void kemoview_get_VIZ_color_RGB_value(struct kemoviewer_type *kemoviewer, int id_model,
                                      int i_point, double *value, double *color){
    if(id_model == FIELDLINE_RENDERING){
        get_VIZ_color_RGB_value(kemoviewer->kemo_fline->fline_m,
                                i_point, value, color);
    }else if(id_model == TRACER_RENDERING){
        get_VIZ_color_RGB_value(kemoviewer->kemo_tracer->tracer_m,
                                i_point, value, color);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        get_VIZ_color_RGB_value(kemoviewer->kemo_mul_psf->psf_m[i_current],
                                i_point, value, color);
    }
}

void kemoview_set_VIZ_color_point(int i_point, double value, double color,
                                  int id_model, struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_color_point(kemoviewer->kemo_fline->fline_m,
                            i_point, value, color);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_color_point(kemoviewer->kemo_tracer->tracer_m,
                            i_point, value, color);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_color_point(kemoviewer->kemo_mul_psf->psf_m[i_current],
                            i_point, value, color);
    }
}

void kemoview_delete_VIZ_color_list(int i_delete, int id_model,
                                    struct kemoviewer_type *kemoviewer){

    if(id_model == FIELDLINE_RENDERING){
        delete_VIZ_color_index_list(kemoviewer->kemo_fline->fline_m, i_delete);
    }else if(id_model == TRACER_RENDERING){
        delete_VIZ_color_index_list(kemoviewer->kemo_tracer->tracer_m, i_delete);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        delete_VIZ_color_index_list(kemoviewer->kemo_mul_psf->psf_m[i_current], i_delete);
    }
}

void kemoview_add_VIZ_color_list(double add_value, double add_color, int id_model,
                                 struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        add_VIZ_color_index_list(kemoviewer->kemo_fline->fline_m,
                                 add_value, add_color);
    }else if(id_model == TRACER_RENDERING){
        add_VIZ_color_index_list(kemoviewer->kemo_tracer->tracer_m,
                                 add_value, add_color);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        add_VIZ_color_index_list(kemoviewer->kemo_mul_psf->psf_m[i_current],
                                 add_value, add_color);
    }
}
void kemoview_add_VIZ_opacity_list(double add_value, double add_opacity, int id_model,
                                   struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        add_VIZ_opacity_index_list(kemoviewer->kemo_fline->fline_m,
                                   add_value, add_opacity);
    }else if(id_model == TRACER_RENDERING){
        add_VIZ_opacity_index_list(kemoviewer->kemo_tracer->tracer_m,
                                   add_value, add_opacity);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        add_VIZ_opacity_index_list(kemoviewer->kemo_mul_psf->psf_m[i_current],
                                   add_value, add_opacity);
    }
}

void kemoview_delete_VIZ_opacity_list(int i_delete, int id_model,
                                      struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        delete_VIZ_opacity_index_list(kemoviewer->kemo_fline->fline_m,
                                      i_delete);
    }else if(id_model == TRACER_RENDERING){
        delete_VIZ_opacity_index_list(kemoviewer->kemo_tracer->tracer_m,
                                      i_delete);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        delete_VIZ_opacity_index_list(kemoviewer->kemo_mul_psf->psf_m[i_current],
                                      i_delete);
    }

}

void kemoview_set_VIZ_opacity_data(int i_point, double value, double opacity,
                                   int id_model, struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_opacity_point(kemoviewer->kemo_fline->fline_m,
                              i_point, value, opacity);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_opacity_point(kemoviewer->kemo_tracer->tracer_m,
                              i_point, value, opacity);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_opacity_point(kemoviewer->kemo_mul_psf->psf_m[i_current],
                              i_point, value, opacity);
    }
}

void kemoview_get_PSF_opacity_items(struct kemoviewer_type *kemoviewer, int id_model,
                                    int i_point, double *value, double *opacity){
    if(id_model == FIELDLINE_RENDERING){
        get_VIZ_opacity_table_items(kemoviewer->kemo_fline->fline_m,
                                    i_point, value, opacity);
    }else if(id_model == TRACER_RENDERING){
        get_VIZ_opacity_table_items(kemoviewer->kemo_tracer->tracer_m,
                                    i_point, value, opacity);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        get_VIZ_opacity_table_items(kemoviewer->kemo_mul_psf->psf_m[i_current],
                                    i_point, value, opacity);
    }
}

double kemoview_get_VIZ_opacity_range(struct kemoviewer_type *kemoviewer,
                                      int id_model, int selected){
    double value = 0.0;
    if(id_model == FIELDLINE_RENDERING){
        value = get_VIZ_opacity_range(selected, kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        value = get_VIZ_opacity_range(selected, kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        value = get_VIZ_opacity_range(selected, kemoviewer->kemo_mul_psf->psf_m[i_current]);
    };
    return value;
};

void kemoview_set_VIZ_color_value_w_exp(int id_model, int selected,
                                        double value, int i_digit,
                                        struct kemoviewer_type *kemoviewer){
    double data = const_from_digit_order(value, i_digit);
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_line_width(data, kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_line_width(data, kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_line_width(data, kemoviewer->kemo_mul_psf->psf_m[i_current]);
    };
};


void kemoview_set_each_VIZ_vector_w_exp(int selected, double value, int i_digit,
                                        int id_model, struct kemoviewer_type *kemoviewer){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    set_VIZ_vector_w_exp(selected, value, i_digit, viz_menu);
};

void kemoview_get_VIZ_vector_w_exp(struct kemoviewer_type *kemoviewer, int id_model,
                                   int selected, double *value, int *i_digit){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    get_VIZ_vector_w_exp(selected, viz_menu, value, i_digit);
    return;
};

void kemoview_get_VIZ_color_w_exp(struct kemoviewer_type *kemoviewer, int id_model,
                                  int selected, double *value, int *i_digit){
    struct psf_menu_val * viz_menu = select_viz_menu_structure(id_model, kemoviewer);
    get_VIZ_color_w_exp(selected, viz_menu, value, i_digit);
	return;
};

void kemoview_set_VIZ_single_color(double *rgba, int id_model,
                                   struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_fixed_color(kemoviewer->kemo_fline->fline_d,
                            kemoviewer->kemo_fline->fline_m,
                            rgba);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_fixed_color(kemoviewer->kemo_tracer->tracer_d,
                            kemoviewer->kemo_tracer->tracer_m,
                            rgba);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_fixed_color(kemoviewer->kemo_mul_psf->psf_d[i_current],
                            kemoviewer->kemo_mul_psf->psf_m[i_current],
                            rgba);
    }
}

void kemoview_set_constant_opacity(double opacity, int id_model,
                                   struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_constant_opacity(kemoviewer->kemo_fline->fline_d,
                                 kemoviewer->kemo_fline->fline_m,
                                 opacity);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_constant_opacity(kemoviewer->kemo_tracer->tracer_d,
                                 kemoviewer->kemo_tracer->tracer_m,
                                 opacity);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_constant_opacity(kemoviewer->kemo_mul_psf->psf_d[i_current],
                                 kemoviewer->kemo_mul_psf->psf_m[i_current],
                                 opacity);
    }
}

void kemoview_set_linear_colormap(double minvalue, int i_min_digit,
                                  double maxvalue, int i_max_digit,
                                  int id_model,
                                  struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        set_VIZ_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit,
                                kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        set_VIZ_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit,
                                kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        set_VIZ_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit,
                                kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
}

void kemoview_read_colormap_file(struct kv_string *filename, int id_model,
                                 struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        read_VIZ_colormap_control_file(filename, kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        read_VIZ_colormap_control_file(filename, kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        read_VIZ_colormap_control_file(filename, kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
}

void kemoview_write_colormap_file(struct kv_string *filename, int id_model,
                                  struct kemoviewer_type *kemoviewer){
    if(id_model == FIELDLINE_RENDERING){
        write_VIZ_colormap_control_file(filename,
                                        kemoviewer->kemo_mesh->mesh_m->iflag_draw_axis,
                                        kemoviewer->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        write_VIZ_colormap_control_file(filename,
                                        kemoviewer->kemo_mesh->mesh_m->iflag_draw_axis,
                                        kemoviewer->kemo_tracer->tracer_m);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        write_VIZ_colormap_control_file(filename,
                                        kemoviewer->kemo_mesh->mesh_m->iflag_draw_axis,
                                        kemoviewer->kemo_mul_psf->psf_m[i_current]);
    }
}

double kemoview_get_VIZ_data_range(struct kemoviewer_type *kemoviewer,
                                   int id_model, int selected, int icomp){
    if(id_model == FIELDLINE_RENDERING){
        return get_VIZ_data_range(selected, icomp,
                                  kemoviewer->kemo_fline->fline_d);
    }else if(id_model == TRACER_RENDERING){
        return get_VIZ_data_range(selected, icomp,
                                  kemoviewer->kemo_tracer->tracer_d);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        return get_VIZ_data_range(selected, icomp,
                                  kemoviewer->kemo_mul_psf->psf_d[i_current]);
    }
};


void kemoview_get_PSF_rgb_at_value(struct kemoviewer_type *kemoviewer,
                                   int id_model, double value,
                                   double *red, double *green, double *blue){
    if(id_model == FIELDLINE_RENDERING){
        get_VIZ_rgb_from_value(kemoviewer->kemo_fline->fline_m,
                               value, red, green, blue);
    }else if(id_model == TRACER_RENDERING){
        get_VIZ_rgb_from_value(kemoviewer->kemo_tracer->tracer_m,
                               value, red, green, blue);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        get_VIZ_rgb_from_value(kemoviewer->kemo_mul_psf->psf_m[i_current],
                               value, red, green, blue);
    }

}
double kemoview_get_PSF_opacity_at_value(struct kemoviewer_type *kemoviewer, 
                                         int id_model, double value){
    double opacity;
    if(id_model == FIELDLINE_RENDERING){
        opacity = get_VIZ_opacity_at_value(kemoviewer->kemo_fline->fline_m, value);
    }else if(id_model == TRACER_RENDERING){
        opacity = get_VIZ_opacity_at_value(kemoviewer->kemo_tracer->tracer_m, value);
    }else{
        int i_current = kemoviewer->kemo_mul_psf->psf_a->id_current;
        opacity = get_VIZ_opacity_at_value(kemoviewer->kemo_mul_psf->psf_m[i_current], value);
    };
    return opacity;
}



/* Subroutines for field lines */

void kemoview_set_fline_file_step(int istep, struct kemoviewer_type *kemoviewer){
	set_fline_file_step(kemoviewer->kemo_fline->fline_m, istep);
};

void kemoview_set_line_type_flag(int input, struct kemoviewer_type *kemoviewer){
    return set_fline_type(kemoviewer->kemo_fline->fline_m, input);
};
int kemoview_get_line_type_flag(struct kemoviewer_type *kemoviewer){
    return (int) get_fline_type(kemoviewer->kemo_fline->fline_m);
};


/* mesh controls  */
void kemoview_draw_with_modified_domain_distance(struct kemoviewer_type *kemoviewer){
    cal_range_4_mesh_c(kemoviewer->kemo_mesh->mesh_d, kemoviewer->view_s);
    modify_object_multi_viewer_c(kemoviewer->kemo_mesh->mesh_m->dist_domains,
                                 kemoviewer->kemo_mesh->mesh_d);
    return;
}

void kemoview_set_mesh_color_mode(int icolor, struct kemoviewer_type *kemoviewer){
    set_mesh_color_mode(icolor, kemoviewer->kemo_mesh->mesh_m);
};
void kemoview_set_num_of_color_loop(int icolor, struct kemoviewer_type *kemoviewer){
    set_num_of_color_loop(icolor, kemoviewer->kemo_mesh->mesh_m);
};

void kemoview_set_node_diamater(double factor, int i_digit,
                                struct kemoviewer_type *kemoviewer){
    set_node_diamater(factor, i_digit, kemoviewer->kemo_mesh->mesh_m);
};
int kemoview_get_mesh_color_mode(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->mesh_color_mode;
};
void kemoview_get_node_diamater(struct kemoviewer_type *kemoviewer,
                                double *factor, int *i_digit){
    get_node_diamater(kemoviewer->kemo_mesh->mesh_m, factor, i_digit);    
};
int kemoview_get_num_of_color_loop(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->num_of_color_loop;
};

void kemoview_set_domain_distance(double dist, struct kemoviewer_type *kemoviewer){
    set_domain_distance(dist, kemoviewer->kemo_mesh->mesh_m);
};
double kemoview_get_domain_distance(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->dist_domains;
    
};


void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor,
                                  struct kemoviewer_type *kemoviewer){
    set_mesh_color_flag(iflag_group, selected, icolor, kemoviewer->kemo_mesh);
    return;
}
int kemoview_get_mesh_color_flag(struct kemoviewer_type *kemoviewer,
                                 int iflag_group, int selected){
    return get_mesh_color_flag(iflag_group, selected, kemoviewer->kemo_mesh);
}

void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4],
                                  struct kemoviewer_type *kemoviewer){
    set_mesh_color_code(iflag_group, selected, color_code4, kemoviewer->kemo_mesh);
};
void kemoview_get_mesh_color_code(struct kemoviewer_type *kemoviewer,
                                  int iflag_group, int selected, float color_code4[4]){
    get_mesh_color_code(kemoviewer->kemo_mesh, iflag_group, selected, color_code4);
};

void kemoview_set_mesh_opacity(int iflag_group, double opacity_in,
                               struct kemoviewer_type *kemoviewer){
    set_mesh_opacity(iflag_group, opacity_in, kemoviewer->kemo_mesh);
    return;
};

double kemoview_get_mesh_opacity(struct kemoviewer_type *kemoviewer,
                                 int iflag_group){
    return get_mesh_opacity(kemoviewer->kemo_mesh, iflag_group);
};

void kemoview_set_mesh_draw_flag(int selected, int iflag,
                                 struct kemoviewer_type *kemoviewer){
    int num_pe = get_num_of_mesh_group(DOMAIN_FLAG, kemoviewer->kemo_mesh);
    set_mesh_draw_flag(num_pe, selected, iflag, kemoviewer->kemo_mesh->mesh_m);
    return;
};

void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag,
                                 struct kemoviewer_type *kemoviewer){
    set_draw_mesh_flag(iflag_group, selected, igrp, iflag, kemoviewer->kemo_mesh);
    return;
};

int kemoview_get_draw_mesh_flag(struct kemoviewer_type *kemoviewer){
    return kemoviewer->kemo_mesh->mesh_m->iflag_draw_mesh;
};

int kemoview_get_num_of_mesh_group(struct kemoviewer_type *kemoviewer,
                                   int iflag_group){
    return get_num_of_mesh_group(iflag_group, kemoviewer->kemo_mesh);
};

int kemoview_get_draw_mesh_item(struct kemoviewer_type *kemoviewer,
                                int iflag_group, int selected, int igrp){
    return get_draw_mesh_flag(kemoviewer->kemo_mesh, iflag_group, selected, igrp);
};

struct kv_string * kemoview_get_group_name(struct kemoviewer_type *kemoviewer,
                                           int selected, int i){
    struct kv_string* groupname = kemoview_alloc_kvstring();
    if(selected == NODE_GRP_FLAG){
        alloc_copy_string(kemoviewer->kemo_mesh->mesh_d->nod_gp_name_sf[i], groupname);
    }else if(selected == ELEM_GRP_FLAG){
        alloc_copy_string(kemoviewer->kemo_mesh->mesh_d->ele_gp_name_sf[i], groupname);
    }else if(selected == SURF_GRP_FLAG){
        alloc_copy_string(kemoviewer->kemo_mesh->mesh_d->surf_gp_name_sf[i], groupname); 
    } 
    return groupname;
};
