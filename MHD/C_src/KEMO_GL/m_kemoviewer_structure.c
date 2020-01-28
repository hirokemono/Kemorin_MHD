
/* m_kemoviewer_structure.c*/

#include "m_kemoviewer_structure.h"

struct kemoviewer_type *kemo_sgl;

void kemoview_allocate_pointers(){
	kemo_sgl->view_s = (struct view_element *)      malloc(sizeof(struct view_element));
	
	kemo_sgl->kemo_shaders = init_kemoview_shaders();
	kemo_sgl->kemo_VAOs = init_kemoview_VAOs();
	kemo_sgl->menu_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));

	kemo_sgl->kemo_mesh =  init_kemoview_mesh();
	kemo_sgl->kemo_fline = init_kemoview_fline();
	kemo_sgl->kemo_psf =   init_kemoview_psf();
	
	kemo_sgl->psf_ucd_tmp = (struct psf_data *) malloc(sizeof(struct psf_data));
	return;
}

void kemoview_allocate_viwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh){
	/*! Initialize mesh data*/
	kemoviewer_data = (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
    
    kemo_sgl = kemoviewer_data;
	kemoview_allocate_pointers();
	
	init_kemoview_array(kemo_sgl->kemo_psf->psf_a);
    
	init_kemoviewer(iflag_dmesh, kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->kemo_fline->fline_m);
	
	return;
}

struct kemoviewer_type * kemoview_allocate_single_viwewer_struct(){
	/*! Initialize mesh data*/
	struct kemoviewer_type *kemoviewer_data 
		= (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
	kemoviewer_data->window_ID = 0;
    
    kemo_sgl = kemoviewer_data;
	kemoview_allocate_pointers();
    
	init_kemoview_array(kemo_sgl->kemo_psf->psf_a);
    
	init_kemoviewer(IZERO, kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->kemo_fline->fline_m);
	
	return kemoviewer_data;
}

void kemoview_deallocate_pointers(struct kemoviewer_type *kemoviewer_data){
	free(kemoviewer_data->view_s);
	free(kemoviewer_data->psf_ucd_tmp);
	
	dealloc_kemoview_mesh(kemoviewer_data->kemo_mesh);
	dealloc_kemoview_fline(kemoviewer_data->kemo_fline);
	dealloc_kemoview_psf(kemoviewer_data->kemo_psf);
	
	clear_kemoview_VAOs(kemoviewer_data->kemo_VAOs);
	dealloc_kemoview_VAOs(kemoviewer_data->kemo_VAOs);
	return;
}

int kemoview_get_PSF_maximum_load(void){
	return get_PSF_maximum_load(kemo_sgl->kemo_psf->psf_a);
};

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


void kemoview_set_single_viewer_id(int id_window){
    if(id_window != kemo_sgl->window_ID){printf("Something wrong in window ID \n");};
    return;
}

void kemoview_set_current_viewer_id(int id_window, struct mul_kemoviewer_type *kemoview_array){
    if(id_window > kemoview_array->num_window){printf("Something wrong in window ID \n");};
	kemoview_array->id_current = id_window;
	return;
}
int kemoview_get_current_viewer_id(void){return kemo_sgl->window_ID;};

/* Routines for draw by OpenGL */

void kemoview_draw_fast_gl3(void){
	quick_mono_kemoview(kemo_sgl);
	return;
};

void kemoview_orthogonalGL(double left, double right, double bottom, double top,
						   double near, double far){
	orthogonalGL(left, right, bottom, top, near, far);
	return;
};
void kemoview_indentity_projectionmatrix(void){set_projection_by_identity();};
void kemoview_indentity_viewmatrix(void){set_view_by_identity();};
void kemoview_message_viewmatrix(void){set_view_for_message(kemo_sgl->view_s);};

void kemoview_init_lighting(){
	kemo_gl_initial_lighting_c(kemo_sgl->view_s, kemo_sgl->kemo_shaders);
	assign_kemoview_VAOs(kemo_sgl->kemo_VAOs);
}

void kemoview_init_background_color(void){init_bg_color_kemoview(kemo_sgl->kemo_mesh->mesh_m);}
void kemoview_set_background_color(float color[4]) {
    copy_rgba_color_c(color, kemo_sgl->kemo_mesh->mesh_m->bg_color);
    set_bg_color_kemoview(kemo_sgl->kemo_mesh->mesh_m);
};
void kemoview_get_background_color(float color[4]){copy_rgba_color_c(kemo_sgl->kemo_mesh->mesh_m->bg_color, color);};


/* Routines for menu selection */
int kemoview_set_data_format_flag(struct kv_string *filename, 
                                  struct kv_string *stripped_prefix, struct kv_string *stripped_ext){
    alloc_kvstringitem(strlen(filename->string), stripped_prefix);
    alloc_kvstringitem(strlen(filename->string), stripped_ext);
    return set_data_format_flag(filename->string, stripped_prefix->string, stripped_ext->string);
}

int kemoview_open_data(struct kv_string *filename){
	int iflag_datatype;
	iflag_datatype = kemoviewer_open_data(filename, 
				kemo_sgl->kemo_mesh, kemo_sgl->kemo_psf, kemo_sgl->kemo_fline,
				kemo_sgl->psf_ucd_tmp,kemo_sgl->view_s);
	return iflag_datatype;
}

void kemoview_close_mesh_view(void){
	close_mesh_view(kemo_sgl->kemo_mesh);
	return;
}

int kemoview_close_PSF_view(void){
	close_PSF_view(kemo_sgl->kemo_psf);
	return kemoview_get_PSF_loaded_params(NUM_LOADED);
}

void kemoview_close_fieldline_view(void){
	close_fieldline_view(kemo_sgl->kemo_fline);
	return;
}

void kemoview_write_modelview_file(struct kv_string *filename){
	write_GL_modelview_file(filename, kemo_sgl->view_s);
}
void kemoview_load_modelview_file(struct kv_string *filename){
	read_GL_modelview_file(filename, kemo_sgl->view_s);
}



void kemoview_viewer_evolution(int istep){
	int ierr = 0;
	psf_viewer_evolution(istep, kemo_sgl->kemo_psf->psf_a);
	ierr = evolution_fline_viewer(kemo_sgl->kemo_fline, kemo_sgl->psf_ucd_tmp,
				kemo_sgl->kemo_psf->psf_a->istep_sync);
    evolution_psf_viewer(kemo_sgl->psf_ucd_tmp, kemo_sgl->kemo_psf);
	return;
}


void kemoview_draw_with_modified_domain_distance(void){
	cal_range_4_mesh_c(kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->view_s);
	modify_object_multi_viewer_c(kemo_sgl->kemo_mesh->mesh_m->dist_domains, kemo_sgl->kemo_mesh->mesh_d);
	return;
}

void kemoview_set_viewtype(int sel){
    set_viewtype(kemo_sgl->view_s, sel);
}

void kemoview_set_coastline_radius(double radius){kemo_sgl->kemo_mesh->mesh_m->radius_coast = radius;};
double kemoview_get_coastline_radius(void){return kemo_sgl->kemo_mesh->mesh_m->radius_coast;};

void kemoview_set_object_property_flags(int selected, int iflag){
	if (selected == AXIS_TOGGLE) {set_axis_flag(iflag, kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {set_coastline_flag(iflag, kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {set_sphere_grid_flag(iflag, kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == SHADING_SWITCH) {kemo_sgl->view_s->shading_mode = iflag;}
    else if(selected == POLYGON_SWITCH) {set_polygon_mode(iflag, kemo_sgl->kemo_mesh->mesh_m);};
	return;
}

int kemoview_get_object_property_flags(int selected){
	if (selected == AXIS_TOGGLE) {return kemo_sgl->kemo_mesh->mesh_m->iflag_draw_axis;}
    else if(selected == COASTLINE_SWITCH) {return kemo_sgl->kemo_mesh->mesh_m->iflag_draw_coast;}
    else if(selected == SPHEREGRID_SWITCH) {return kemo_sgl->kemo_mesh->mesh_m->iflag_draw_sph_grid;}
    else if(selected == SHADING_SWITCH) {return kemo_sgl->view_s->shading_mode;}
    else if(selected == POLYGON_SWITCH) {return kemo_sgl->kemo_mesh->mesh_m->polygon_mode;};
    
	return 0;
}

int kemoview_toggle_object_properties(int selected){
	if (selected == AXIS_TOGGLE) {return toggle_draw_axis(kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {return toggle_coastline_flag(kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {return toggle_sphere_grid_flag(kemo_sgl->kemo_mesh->mesh_m);}
    else if(selected == SHADING_SWITCH){
		kemo_sgl->view_s->shading_mode = toggle_value_c(kemo_sgl->view_s->shading_mode);
		return kemo_sgl->view_s->shading_mode;
	}
    else if(selected == POLYGON_SWITCH) {return toggle_polygon_mode(kemo_sgl->kemo_mesh->mesh_m);};
    
	return 0;
}

/* Shader controls */



void kemoview_alloc_phong_light_list(int num){
	alloc_phong_light_list(kemo_sgl->kemo_shaders->lights, num);
};
void kemoview_dealloc_phong_light_list(void){
	dealloc_phong_light_list(kemo_sgl->kemo_shaders->lights);
};
void kemoview_realloc_phong_light_list(int num){
	realloc_phong_light_list(kemo_sgl->kemo_shaders->lights, num);
};

void kemoview_delete_phong_light_list(int i_delete){
	delete_phong_light_list(kemo_sgl->kemo_shaders->lights, i_delete);
};
void kemoview_add_phong_light_list(int i_add, float r, float t, float p){
	add_phong_light_list(kemo_sgl->view_s, kemo_sgl->kemo_shaders->lights, i_add, r, t, p);
};

void kemoview_init_phong_light_list(void){
	init_phong_light_list(kemo_sgl->view_s, kemo_sgl->kemo_shaders->lights);
};


void kemoview_set_each_light_position(int i_point, float r, float t, float p){
	set_each_light_position(kemo_sgl->view_s, kemo_sgl->kemo_shaders->lights, i_point, r, t, p);
};
int kemoview_get_num_light_position(void){
	return send_num_light_position(kemo_sgl->kemo_shaders->lights);
};
void kemoview_get_each_light_rtp(int i_point, float *r, float *t, float *p){
	send_each_light_rtp(kemo_sgl->kemo_shaders->lights, i_point, r, t, p);
};

void kemoview_set_material_parameter(int itype, float value){
	set_matrial_parameter(itype, value, kemo_sgl->kemo_shaders->lights);
};
float kemoview_get_material_parameter(int itype){
	return get_matrial_parameter(itype, kemo_sgl->kemo_shaders->lights);
};


/* mesh controls  */
void kemoview_set_mesh_color_mode(int icolor)  {
	set_mesh_color_mode(icolor, kemo_sgl->kemo_mesh->mesh_m);
};
void kemoview_set_num_of_color_loop(int icolor){
	set_num_of_color_loop(icolor, kemo_sgl->kemo_mesh->mesh_m);
};

void kemoview_set_node_diamater(double factor, int i_digit){
	set_node_diamater(factor, i_digit, kemo_sgl->kemo_mesh->mesh_m);
};
void kemoview_get_node_diamater(double *factor, int *i_digit){
	get_node_diamater(kemo_sgl->kemo_mesh->mesh_m, factor, i_digit);	
};

void kemoview_set_domain_distance(double dist){
	set_domain_distance(dist, kemo_sgl->kemo_mesh->mesh_m);
};


void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor){
	set_mesh_color_flag(iflag_group, selected, icolor, kemo_sgl->kemo_mesh);
    return;
}
int kemoview_get_mesh_color_flag(int iflag_group, int selected){
	return get_mesh_color_flag(iflag_group, selected, kemo_sgl->kemo_mesh);
}

void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4]){
	set_mesh_color_code(iflag_group, selected, color_code4, kemo_sgl->kemo_mesh);
};
void kemoview_get_mesh_color_code(int iflag_group, int selected, float color_code4[4]){
	get_mesh_color_code(kemo_sgl->kemo_mesh, iflag_group, selected, color_code4);
};

void kemoview_set_mesh_opacity(int iflag_group, double opacity_in){
	set_mesh_opacity(iflag_group, opacity_in, kemo_sgl->kemo_mesh);
	return;
};

double kemoview_get_mesh_opacity(int iflag_group){
	return get_mesh_opacity(kemo_sgl->kemo_mesh, iflag_group);
};

int kemoview_get_draw_mesh_flag(void){return kemo_sgl->kemo_mesh->mesh_m->iflag_draw_mesh;};
int kemoview_get_num_of_mesh_group(int iflag_group){
	return get_num_of_mesh_group(iflag_group, kemo_sgl->kemo_mesh);
};


void kemoview_set_mesh_draw_flag(int selected, int iflag){
	int num_pe = get_num_of_mesh_group(DOMAIN_FLAG, kemo_sgl->kemo_mesh);
	set_mesh_draw_flag(num_pe, selected, iflag, kemo_sgl->kemo_mesh->mesh_m);
	return;
};

void kemoview_mesh_draw_toggle(int selected){
	int num_pe = get_num_of_mesh_group(DOMAIN_FLAG, kemo_sgl->kemo_mesh);
	mesh_draw_toggle(num_pe, selected, kemo_sgl->kemo_mesh->mesh_m);
	return;
};


void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag){
	set_draw_mesh_flag(iflag_group, selected, igrp, iflag, kemo_sgl->kemo_mesh);
	return;
};
void kemoview_toggle_draw_mesh_item(int iflag_group, int selected, int igrp){
	toggle_draw_mesh_flag(iflag_group, selected, igrp, kemo_sgl->kemo_mesh);
	return;
};
int kemoview_get_draw_mesh_item(int iflag_group, int selected, int igrp){
	return get_draw_mesh_flag(kemo_sgl->kemo_mesh, iflag_group, selected, igrp);
};

void kemoview_get_node_grp_name(struct kv_string *groupname, int i){
    alloc_copy_string(kemo_sgl->kemo_mesh->mesh_d->nod_gp_name_sf[i], groupname);
};
void kemoview_get_ele_grp_name(struct kv_string *groupname, int i){ 
    alloc_copy_string(kemo_sgl->kemo_mesh->mesh_d->ele_gp_name_sf[i], groupname);
};
void kemoview_get_surf_grp_name(struct kv_string *groupname, int i){
    alloc_copy_string(kemo_sgl->kemo_mesh->mesh_d->surf_gp_name_sf[i], groupname); 
};

int kemoview_get_view_type_flag(void){return kemo_sgl->view_s->iflag_view_type;};

int kemoview_get_mesh_color_mode(void){return kemo_sgl->kemo_mesh->mesh_m->mesh_color_mode;};
int kemoview_get_num_of_color_loop(void){return kemo_sgl->kemo_mesh->mesh_m->num_of_color_loop;};

double kemoview_get_domain_distance(void){return kemo_sgl->kemo_mesh->mesh_m->dist_domains;};


void kemoview_get_ext_from_file_name(struct kv_string *filename,
                                     struct kv_string *stripped_prefix, struct kv_string *stripped_ext){
    alloc_kvstringitem((int) strlen(filename->string), stripped_prefix);
    alloc_kvstringitem((int) strlen(filename->string), stripped_ext);
	get_ext_from_file_name_c(filename->string, stripped_prefix->string, stripped_ext->string);
}
void kemoview_add_ext_to_file_name(struct kv_string *file_prefix, struct kv_string *added_ext,
                                   struct kv_string *file_name){
    int lentgh = (int) strlen(file_prefix->string) + (int) strlen(added_ext->string);
    alloc_kvstringitem(lentgh+2, file_name);
    add_ext_to_file_name_c(file_prefix->string, added_ext->string, file_name->string);
}

void kemoview_set_text_color_code(float c_code[4]){copy_rgba_color_c(c_code, kemo_sgl->kemo_mesh->mesh_m->text_color);};
void kemoview_get_text_color_code(float c_code[4]){copy_rgba_color_c(kemo_sgl->kemo_mesh->mesh_m->text_color, c_code);};




void kemoview_get_fliped_img(int npixel_x, int npixel_y,
                               unsigned char *glimage, unsigned char *fliped_img){
    get_gl_buffer_to_bmp(npixel_x, npixel_y, glimage);
    flip_gl_bitmap(npixel_x, npixel_y, glimage, fliped_img);
    return;
}

void kemoview_set_PSF_by_rgba_texture(int width, int height, const unsigned char *bgra_in){
    set_texture_psf_from_bgra(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
							  width, height, bgra_in);
};

void kemoview_quick_view(void){
	quick_mono_kemoview(kemo_sgl);
};
void kemoview_modify_view(void){
	modify_stereo_kemoview(kemo_sgl);
};
void kemoview_rotate(void){rotate_stereo_kemoview(kemo_sgl);};

void kemoviewer_reset_to_init_angle(void){
    reset_all_view_parameter(kemo_sgl->view_s);
    init_rot_animation(kemo_sgl->view_s);
};


void kemoview_set_retinamode(int i_retina){
    set_gl_retinamode(kemo_sgl->view_s, i_retina);
}
int kemoview_get_retinamode(void){
    return send_gl_retinamode(kemo_sgl->view_s);
}

void kemoview_set_windowsize(int npixel_x, int npixel_y, int nwindow_x, int nwindow_y){
    set_gl_windowsize(kemo_sgl->view_s, npixel_x, npixel_y, nwindow_x, nwindow_y);
};
void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y, int nwindow_x, int nwindow_y){
	update_projection_by_windowsize(kemo_sgl->view_s, npixel_x, npixel_y, nwindow_x, nwindow_y);
};
void kemoview_set_windowsize_message(int iflag){
    set_message_switch(iflag, kemo_sgl->kemo_mesh->msg_wk);
    return;
}


void kemoview_update_distance(void){
	update_projection_struct(kemo_sgl->view_s);
};

void kemoview_set_view_integer(int selected, int ivalue){
	if(selected == ISET_ROTATE_AXIS){
		set_gl_animation_rot_axis(kemo_sgl->view_s, ivalue);
	}else if(selected == ISET_ROTATE_INCREMENT){
		set_gl_animation_rot_angle(kemo_sgl->view_s, ivalue);
	}else if(selected == ISET_SHUTTER){
		kemo_sgl->view_s->iflag_streo_stutter = ivalue;
	}else if(selected == ISET_ANAGYLYPH){
		kemo_sgl->view_s->iflag_streo_anaglyph = ivalue;
	}
	return;
};
void kemoview_set_view_parameter(int selected, int i, double value){
	if(selected == ISET_ROTATE){
		set_gl_rotation_parameter(kemo_sgl->view_s, i, value);
	}else if(selected == ISET_SHIFT){
		set_gl_shift_vector(kemo_sgl->view_s, i, value);
	}else if(selected == ISET_SCALE){
		set_gl_scalar_scale_factor(kemo_sgl->view_s, value);
		
	}else if(selected == ISET_APERTURE){
		set_gl_projection_aperture(kemo_sgl->view_s, value);
	};
	return;
};
void kemoview_set_stereo_parameter(double focus, double eye_sep){
	set_gl_stereo_parameter(kemo_sgl->view_s, focus, eye_sep);
};


int kemoview_get_view_integer(int selected){
	int ivalue = 0;
	if(selected == ISET_PIXEL_X){
		ivalue = send_gl_windowsize_x(kemo_sgl->view_s);
	}else if(selected == ISET_PIXEL_Y){
		ivalue = send_gl_windowsize_y(kemo_sgl->view_s);

	}else if(selected == ISET_SHUTTER){
		ivalue = kemo_sgl->view_s->iflag_streo_stutter;
	}else if(selected == ISET_ANAGYLYPH){
		ivalue = kemo_sgl->view_s->iflag_streo_anaglyph;
	};
	return ivalue;
};
double kemoview_get_view_parameter(int selected, int i){
	double value = 0.0;
	if(selected == ISET_ROTATE){
		value =  send_gl_rotation_parameter(kemo_sgl->view_s, i);
	}else if(selected == ISET_SHIFT){
		value =  send_gl_shift_vector(kemo_sgl->view_s, i);
	}else if(selected == ISET_VWPOINT){
		value =  send_gl_lookat_vector(kemo_sgl->view_s, i);
	}else if(selected == ISET_SCALE){
		value =  send_scalar_scale_factor(kemo_sgl->view_s);

	}else if(selected == ISET_APERTURE){
		value =  send_gl_projection_aperture(kemo_sgl->view_s);
	}else if(selected == ISET_NEAR){
		value =  send_gl_projection_near(kemo_sgl->view_s);
	}else if(selected == ISET_FAR){
		value =  send_gl_projection_far(kemo_sgl->view_s);
	}else if(selected == ISET_ASPECT){
		value =  send_gl_projection_aspect(kemo_sgl->view_s);
		
	}else if(selected == ISET_FOCUS){
		value =  send_gl_stereo_focus(kemo_sgl->view_s);
	}else if(selected == ISET_EYESEP){
		value =  send_gl_stereo_eyeseparation(kemo_sgl->view_s);
	};
	return value;
};

void kemoview_mousedolly(double start[2], double x_dolly, double y_dolly){
	gl_mousedolly_struct(kemo_sgl->view_s, start, x_dolly, y_dolly);
}
void kemoview_mousepan(double start[2], double x_pan, double y_pan){
	gl_mousepan_struct(kemo_sgl->view_s, start, x_pan, y_pan);
}
void kemoview_zooming(double wheelDelta){
	gl_zooming_struct(kemo_sgl->view_s, wheelDelta);
}

/* called with the start position and the window origin + size */
void kemoview_startTrackball(double x, double y){gl_startTrackball(x, y, kemo_sgl->view_s);};
/* calculated rotation based on current mouse position */
void kemoview_rollToTrackball(double x, double y){ gl_rollToTrackball (x, y, kemo_sgl->view_s);};
/* add a GL rotation (dA) to an existing GL rotation (A) */
void kemoview_drugging_addToRotationTrackball(void){
    gl_drag_addToRotationTrackball(kemo_sgl->view_s);
}

void kemoview_animation_add_rotation(double dt){add_animation_rotation(kemo_sgl->view_s, dt);}
void kemoview_reset_animation(void){reset_rot_animation(kemo_sgl->view_s);};


/* Subroutines for surface rendering */
void kemoview_set_PSF_loaded_params(int selected, int input){
	set_PSF_loaded_params(selected, input, kemo_sgl->kemo_psf);
};

int kemoview_get_PSF_loaded_params(int selected){
	return get_PSF_loaded_params(kemo_sgl->kemo_psf, selected);
};
int kemoview_get_PSF_loaded_flag(int id_psf){
	return get_PSF_loaded_flag(id_psf, kemo_sgl->kemo_psf->psf_a);
};


void kemoview_get_PSF_full_path_file_name(struct kv_string *ucd_m){
	alloc_set_ucd_file_name_by_psf(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], ucd_m);
	return;
}
int kemoview_get_PSF_full_path_file_prefix(struct kv_string *psf_filehead, int *iflag){
    return send_each_psf_file_header_full(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
										  psf_filehead, iflag);
}

int kemoview_get_PSF_file_prefix(struct kv_string *stripped_filehead){
	struct kv_string* stripped_dir = alloc_kvstring();
	int istep = send_each_psf_file_dir_head(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
											stripped_dir, stripped_filehead);
	dealloc_kvstring(stripped_dir);
	return istep;
}

void kemoview_set_each_PSF_field_param(int selected, int input){
	return set_each_PSF_field_param(selected, input, kemo_sgl->kemo_psf);
};
int kemoview_get_each_PSF_field_param(int selected){
	return get_each_PSF_field_param(selected, kemo_sgl->kemo_psf);
};

int kemoview_get_PSF_num_component(int i){
	return send_ncomp_each_psf(kemo_sgl->kemo_psf->psf_d[kemo_sgl->kemo_psf->psf_a->id_current], i);
};
void kemoview_get_PSF_field_name(struct kv_string *colorname, int i){
    send_each_psf_data_name(kemo_sgl->kemo_psf->psf_d[kemo_sgl->kemo_psf->psf_a->id_current], colorname, i);
};

void kemoview_set_PSF_polygon_mode(int iflag){
	set_psf_polygon_mode(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], iflag);
};
void kemoview_set_PSF_tangential_vec_mode(int iflag){
	set_psf_vector_mode(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], iflag);
};

int kemoview_get_PSF_draw_refv(void){
	return send_draw_psf_refv(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
};
int kemoview_toggle_PSF_draw_refv(void){
	return toggle_draw_psf_refv(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
};

void * kemoview_link_active_colormap_param(void){
	int i_current = kemoview_get_PSF_loaded_params(SET_CURRENT);
	int icomp = kemoview_get_each_PSF_field_param(DRAW_ADDRESS_FLAG);
	void *current_cmap = kemo_sgl->kemo_psf->psf_m[i_current]->cmap_psf_comp[icomp];
	return current_cmap;
}

int kemoview_select_PSF_draw_switch(int selected){
	return toggle_each_PSF_draw_switch(selected, kemo_sgl->kemo_psf);
}
int kemoview_get_PSF_draw_flags(int selected){
	return get_each_PSF_draw_switch(selected, kemo_sgl->kemo_psf);
}

void kemoview_set_PSF_color_param(int selected, int input){
	set_each_PSF_color_param(selected, input, kemo_sgl->kemo_psf);
	return;
};
int kemoview_get_PSF_color_param(int selected){
	return get_each_PSF_color_param(selected, kemo_sgl->kemo_psf);
};

void kemoview_delete_PSF_color_list(int i_delete){
    delete_PSF_color_index_list(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], i_delete);
}
void kemoview_delete_PSF_opacity_list(int i_delete){
    delete_PSF_opacity_index_list(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], i_delete);
}

void kemoview_add_PSF_color_list(double add_value, double add_color){
    add_PSF_color_index_list(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
							 add_value, add_color);
}
void kemoview_add_PSF_opacity_list(double add_value, double add_opacity){
    add_PSF_opacity_index_list(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
							   add_value, add_opacity);
}

void kemoview_set_PSF_linear_colormap(double minvalue, int i_min_digit,
									  double maxvalue, int i_max_digit){
	set_PSF_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit, 
							kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
}
void kemoview_set_each_PSF_color_w_exp(int selected, double value, int i_digit){
	set_each_PSF_color_w_exp(selected, value, i_digit, kemo_sgl->kemo_psf);
};
void kemoview_get_each_PSF_color_w_exp(int selected, double *value, int *i_digit){;
	get_each_PSF_color_w_exp(selected, kemo_sgl->kemo_psf, value, i_digit);
	return;
};

void kemoview_set_PSF_single_color(double *rgba){
    set_PSF_fixed_color(kemo_sgl->kemo_psf->psf_d[kemo_sgl->kemo_psf->psf_a->id_current],
						kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], rgba);
}

void kemoview_set_PSF_constant_opacity(double opacity){
    set_PSF_constant_opacity(kemo_sgl->kemo_psf->psf_d[kemo_sgl->kemo_psf->psf_a->id_current],
							 kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], opacity);
}

void kemoview_get_PSF_rgb_at_value(double value, double *red, double *green, double *blue){
    set_PSF_rgb_from_value(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
						   value, red, green, blue);
}
double kemoview_get_PSF_opacity_at_value(double value){
    return get_PSF_opacity_at_value(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], value);
}
void kemoview_set_PSF_color_data(int i_point, double value, double color){
    set_each_PSF_color_point(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
							 i_point, value, color);
}
void kemoview_set_PSF_opacity_data(int i_point, double value, double opacity){
    set_each_PSF_opacity_point(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
							   i_point, value, opacity);
}

double kemoview_get_each_PSF_data_range(int selected, int icomp){
	return get_each_PSF_data_range(selected, icomp, kemo_sgl->kemo_psf);
};
double kemoview_get_each_PSF_colormap_range(int selected){
	return get_each_PSF_colormap_range(selected, kemo_sgl->kemo_psf);
};

void kemoview_get_PSF_color_items(int i_point, double *value, double *color){
    send_each_PSF_color_table_items(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current], 
									i_point, value, color);
}
void kemoview_get_PSF_opacity_items(int i_point, double *value, double *opacity){
    send_each_PSF_opacity_table_items(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
									  i_point, value, opacity);
}

void kemoview_write_PSF_colormap_file(struct kv_string *filename){
    write_each_PSF_colormap_control_file(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
										 filename->string);
}
void kemoview_read_PSF_colormap_file(struct kv_string *filename){
    read_each_PSF_colormap_control_file(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current],
										filename->string);
}
void kemoview_check_PSF_colormap_control(void){
    check_each_PSF_colormap_control(kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
}


/* Subroutines for field lines */

void kemoview_get_fline_full_path_file_name(struct kv_string *ucd_m){
	get_fline_full_path_file_name(kemo_sgl->kemo_fline->fline_m, ucd_m);
	return;
}
int kemoview_get_fline_file_step_prefix(struct kv_string *fline_filehead){
	return get_fline_file_step_prefix(kemo_sgl->kemo_fline->fline_m, fline_filehead);
};
void kemoview_set_fline_file_step(int istep){
	set_fline_file_step(kemo_sgl->kemo_fline->fline_m, istep);
};

void kemoview_set_fline_parameters(int selected, int iflag){
	set_fline_parameters(selected, iflag, kemo_sgl->kemo_fline);
};
int kemoview_get_fline_parameters(int selected){
	return get_fline_parameters(kemo_sgl->kemo_fline, selected);
};

void kemoview_set_fline_color_param(int selected, int input) {
	set_fline_color_param(selected, input, kemo_sgl->kemo_fline);
};
int kemoview_get_fline_color_param(int selected){
	return get_fline_color_param(selected, kemo_sgl->kemo_fline);
};


int kemoview_get_fline_color_num_comps(int i){
	return fline_color_num_comps(kemo_sgl->kemo_fline->fline_d, i);
};
void kemoview_get_fline_color_data_name(struct kv_string *colorname, int i){
	get_fline_color_data_name(kemo_sgl->kemo_fline->fline_d, colorname, i);
};

int kemoview_toggle_fline_type(void){return toggle_fline_type(kemo_sgl->kemo_fline->fline_m);};

void kemoview_set_fline_field_param(int selected, int input){
	return set_fline_field_param(selected, input, kemo_sgl->kemo_fline);
};
int kemoview_get_fline_field_param(int selected){
	return get_fline_field_param(selected, kemo_sgl->kemo_fline);
};

void kemoview_set_fline_linear_colormap(double minvalue, int i_min_digit,
										double maxvalue, int i_max_digit){
	set_fline_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit, 
							  kemo_sgl->kemo_fline->fline_m);
}
void kemoview_set_fline_color_w_exp(int selected, double value, int i_digit){
	set_fline_color_w_exp(selected, value, i_digit, kemo_sgl->kemo_fline);
};
void kemoview_get_fline_color_w_exp(int selected, double *value, int *i_digit){
	get_fline_color_w_exp(selected, kemo_sgl->kemo_fline, value, i_digit);
	return;
};

void kemoview_set_fline_constant_opacity(double opacity){
	set_fline_constant_opacity(kemo_sgl->kemo_fline->fline_d, kemo_sgl->kemo_fline->fline_m, opacity);
}

double kemoview_get_fline_opacity_at_value(double value){
	return get_fline_opacity_at_value(kemo_sgl->kemo_fline->fline_m, value);
}
void kemoview_set_fline_color_data(int i_point, double value, double color){
	set_fline_color_data(kemo_sgl->kemo_fline->fline_m, i_point, value, color);
}
void kemoview_set_fline_opacity_data(int i_point, double value, double opacity){
	set_fline_opacity_data(kemo_sgl->kemo_fline->fline_m, i_point, value, opacity);
}

double kemoview_get_fline_data_range(int selected, int icomp){
	return get_fline_data_range(selected, icomp, kemo_sgl->kemo_fline);
};
double kemoview_get_fline_colormap_range(int selected){
	return get_fline_colormap_range(selected, kemo_sgl->kemo_fline);
};

void kemoview_get_fline_color_item(int i_point, double *value, double *color){
	get_fline_color_item(kemo_sgl->kemo_fline->fline_m, i_point, value, color);
}
void kemoview_get_fline_opacity_item(int i_point, double *value, double *opacity){
	get_fline_opacity_item(kemo_sgl->kemo_fline->fline_m, i_point, value, opacity);
}

void kemoview_write_fline_colormap_file(struct kv_string *filename){
	write_fline_colormap_file(kemo_sgl->kemo_fline->fline_m, filename);
}
void kemoview_read_fline_colormap_file(struct kv_string *filename){
	read_fline_colormap_file(kemo_sgl->kemo_fline->fline_m, filename);
}

/*  Temporal routines */

struct shader_ids sampleShader;

void kemoview_draw_menu_setup(void){
	LoadShaderFromStrings(kemo_sgl->kemo_shaders->menu, load_menu_vert(), load_menu_frag());
}

void kemo_Cleanup(void)
{
  destory_shaders(kemo_sgl->kemo_shaders->simple);
  destory_shaders(kemo_sgl->kemo_shaders->simple);
}

/*  Routines using libpng */
#ifdef PNG_OUTPUT
int kemoview_set_image_file_format_id(struct kv_string *image_ext){
    return set_image_format_id_by_ext(image_ext->string);
}

void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix){
    write_gl_window_to_file(iflag_img, image_prefix->string, 
                            kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame);
}
void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix){
    write_gl_window_step_file(iflag_img, istep, image_prefix->string, 
                              kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame);
}

void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix){
    set_texture_to_psf(img_fmt, image_prefix->string, 
					   kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
};
#endif

