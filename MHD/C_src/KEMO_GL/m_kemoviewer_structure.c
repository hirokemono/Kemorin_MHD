
/* m_kemoviewer_structure.c*/

#include "m_kemoviewer_structure.h"

struct kemoviewer_type{
	int window_ID;
	
    struct viewer_mesh        *mesh_d;
    struct mesh_menu_val      *mesh_m;
    
    struct kemo_array_control   *psf_a;
    struct psf_data            **psf_d;
    struct psf_menu_val        **psf_m;
    
    struct psf_data           *fline_d;
    struct fline_menu_val     *fline_m;
    
    struct view_element       *view_s;
	struct buffer_for_gl      *gl_buf;
	struct gl_strided_buffer  *strided_buf;
	struct VAO_ids *cube_VAO;
	
	struct psf_menu_val       *psf_current_menu;
	struct psf_data           *psf_current_data;
	struct psf_data           *psf_ucd_tmp;
	
	struct kemoviewer_type    *next;
};

struct mul_kemoviewer_type{
	int num_window;
	int id_current;
	struct kemoviewer_type   **kemo_mul;
};

struct kemoviewer_type *kemo_sgl;

void kemoview_allocate_pointers(){
	int i;
	
	kemo_sgl->view_s = (struct view_element *)      malloc(sizeof(struct view_element));
	kemo_sgl->gl_buf = (struct buffer_for_gl *)     malloc(sizeof(struct buffer_for_gl));
	kemo_sgl->strided_buf = init_strided_buffer();
	kemo_sgl->cube_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
	
	kemo_sgl->mesh_d =  (struct viewer_mesh *)       malloc(sizeof(struct viewer_mesh));
	kemo_sgl->fline_d = (struct psf_data *)          malloc(sizeof(struct psf_data));
	kemo_sgl->mesh_m =  (struct mesh_menu_val *)     malloc(sizeof(struct mesh_menu_val));
	kemo_sgl->fline_m = (struct fline_menu_val *)    malloc(sizeof(struct fline_menu_val));
	
	kemo_sgl->psf_a =  (struct kemo_array_control *) malloc(sizeof(struct kemo_array_control));
	kemo_sgl->psf_a->nlimit_loaded = NMAX_PSF;
	
	kemo_sgl->psf_d =  (struct psf_data **)     malloc(NMAX_PSF*sizeof(struct psf_data *));
	kemo_sgl->psf_m =  (struct psf_menu_val **) malloc(NMAX_PSF*sizeof(struct psf_menu_val *));
	for(i=0;i<kemo_sgl->psf_a->nlimit_loaded;i++){
		kemo_sgl->psf_d[i] =   (struct psf_data *) malloc(sizeof(struct psf_data));
		kemo_sgl->psf_m[i] =   (struct psf_menu_val *) malloc(sizeof(struct psf_menu_val));
		init_psf_parameters(kemo_sgl->psf_m[i]);
	}
	
	kemo_sgl->psf_ucd_tmp =    (struct psf_data *)          malloc(sizeof(struct psf_data));
	return;
}

void kemoview_allocate_viwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh){
	/*! Initialize mesh data*/
	kemoviewer_data = (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
    
    kemo_sgl = kemoviewer_data;
	kemoview_allocate_pointers();
    
	init_kemoview_array(kemo_sgl->psf_a->nlimit_loaded, kemo_sgl->psf_a);
    
	init_kemoviewer(iflag_dmesh, kemo_sgl->mesh_d, kemo_sgl->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->fline_m);
	
	return;
}

void kemoview_allocate_single_viwewer_struct(struct kemoviewer_type *kemoviewer_data){
	/*! Initialize mesh data*/
	kemoviewer_data = (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
	kemoviewer_data->window_ID = 0;
    
    kemo_sgl = kemoviewer_data;
	kemoview_allocate_pointers();
    
	init_kemoview_array(kemo_sgl->psf_a->nlimit_loaded, kemo_sgl->psf_a);
    
	init_kemoviewer(IZERO, kemo_sgl->mesh_d, kemo_sgl->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->fline_m);
	
	return;
}

void kemoview_deallocate_pointers(struct kemoviewer_type *kemoviewer_data){
	int i;
	for(i=0;i<kemoviewer_data->psf_a->nlimit_loaded;i++){
		free(kemoviewer_data->psf_d[i]);
		free(kemoviewer_data->psf_m[i]);
	}
	
	free(kemoviewer_data->mesh_d);
	free(kemoviewer_data->psf_a);
	free(kemoviewer_data->fline_d);
	free(kemoviewer_data->mesh_m);
	free(kemoviewer_data->fline_m);
	free(kemoviewer_data->psf_d);
	free(kemoviewer_data->psf_m);
    
	free(kemoviewer_data->gl_buf);
	free(kemoviewer_data->view_s);
	
	free(kemoviewer_data->psf_ucd_tmp);
	
	return;
}

int kemoview_get_PSF_maximum_load(){return kemo_sgl->psf_a->nlimit_loaded;};

void kemoview_alloc_kvstringitem(int length, struct kv_string *kvstring){
	alloc_kvstringitem(length, kvstring);
	return;
};
struct kv_string* kemoview_alloc_kvstring(){return alloc_kvstring();};
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
int kemoview_get_current_viewer_id(){return kemo_sgl->window_ID;};

/* Routines for draw by OpenGL */

void kemoview_draw_objects_c(){
    /*    printf("Draw objects to ID: %d\n", kemo_sgl->view_s->gl_drawID);*/
	draw_objects(kemo_sgl->mesh_d, kemo_sgl->psf_d, kemo_sgl->fline_d, kemo_sgl->mesh_m, 
                 kemo_sgl->psf_m, kemo_sgl->psf_a, kemo_sgl->fline_m, kemo_sgl->view_s, 
				 kemo_sgl->gl_buf, kemo_sgl->strided_buf, kemo_sgl->cube_VAO);
	return;
};

void kemoview_draw_viewer_to_ps(){
    /*    printf("Draw objects to ID: %d\n", kemo_sgl->view_s->gl_drawID);*/
    kemo_sgl->view_s->iflag_write_ps = ON;
	draw_objects(kemo_sgl->mesh_d, kemo_sgl->psf_d, kemo_sgl->fline_d, kemo_sgl->mesh_m,
                 kemo_sgl->psf_m, kemo_sgl->psf_a, kemo_sgl->fline_m, kemo_sgl->view_s,
				 kemo_sgl->gl_buf, kemo_sgl->strided_buf, kemo_sgl->cube_VAO);
    kemo_sgl->view_s->iflag_write_ps = OFF;
	return;
};

void kemoview_orthogonalGL(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top,
						   GLdouble near, GLdouble far){
	orthogonalGL(left, right, bottom, top, near, far);
	return;
};
void kemoview_indentity_projectionmatrix(){set_projection_by_identity();};
void kemoview_indentity_viewmatrix(){set_view_by_identity();};
void kemoview_message_viewmatrix(){set_view_for_message(kemo_sgl->view_s);};

void kemoview_init_lighting(int iflag_core_profile){
	kemo_sgl->view_s->iflag_core_profile = iflag_core_profile;
	kemo_sgl->view_s->gl_drawID = glGenLists(IONE);
	
	kemo_gl_initial_lighting_c(kemo_sgl->view_s);
	/* ! set bitmap font list (8x12) */
	init_colorbar_fonts();
}

void kemoview_init_background_color(){init_bg_color_kemoview(kemo_sgl->mesh_m);}
void kemoview_set_background_color(GLfloat color[4]) {
    copy_rgba_color_c(color, kemo_sgl->mesh_m->bg_color);
    set_bg_color_kemoview(kemo_sgl->mesh_m);
};
void kemoview_get_background_color(GLfloat color[4]){copy_rgba_color_c(kemo_sgl->mesh_m->bg_color, color);};


/* Routines for menu selection */
int kemoview_set_data_format_flag(struct kv_string *filename, 
                                  struct kv_string *stripped_prefix, struct kv_string *stripped_ext){
    alloc_kvstringitem(strlen(filename->string), stripped_prefix);
    alloc_kvstringitem(strlen(filename->string), stripped_ext);
    return set_data_format_flag(filename->string, stripped_prefix->string, stripped_ext->string);
}

int kemoview_open_data(struct kv_string *filename){
	int iflag_datatype;
	iflag_datatype = kemoviewer_open_data(filename, kemo_sgl->mesh_d, kemo_sgl->mesh_m,
				kemo_sgl->psf_a, kemo_sgl->psf_d, kemo_sgl->psf_m,
				kemo_sgl->fline_d, kemo_sgl->fline_m,
				kemo_sgl->psf_ucd_tmp,kemo_sgl->view_s);
    
	if (kemo_sgl->psf_a->id_current >= IZERO) {
		kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
		kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
	};
    
	return iflag_datatype;
}

void kemoview_close_mesh_view(){
	kemo_sgl->mesh_m->iflag_draw_mesh = 0;
	dealloc_all_mesh_4_viewer_s(kemo_sgl->mesh_d);
	dealloc_draw_mesh_flags(kemo_sgl->mesh_m);
	return;
}

int kemoview_close_PSF_view(){
	dealloc_draw_psf_flags(kemo_sgl->psf_d[kemo_sgl->psf_a->id_current],
                           kemo_sgl->psf_m[kemo_sgl->psf_a->id_current]);
	deallc_all_psf_data(kemo_sgl->psf_d[kemo_sgl->psf_a->id_current]);
	
	set_close_current_kemoview_array(kemo_sgl->psf_a);
    
	kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
	kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
    
	return kemoview_get_PSF_num_loaded();
}

void kemoview_close_fieldline_view(){
	kemo_sgl->fline_m->iflag_draw_fline = IZERO;
	dealloc_draw_fline_flags(kemo_sgl->fline_d, kemo_sgl->fline_m);
	deallc_all_fline_data(kemo_sgl->fline_d);
	return;
}

void kemoview_set_pick_surface_command(struct kv_string *command){
    dealloc_kvstring(kemo_sgl->mesh_m->pick_surface_command);
    kemo_sgl->mesh_m->pick_surface_command = alloc_kvstring();
	alloc_kvstringitem(command->string, kemo_sgl->mesh_m->pick_surface_command);
};
void kemoview_get_pick_surface_command(struct kv_string *command){
	alloc_kvstringitem(kemo_sgl->mesh_m->pick_surface_command->string, command);
};

void kemoview_write_modelview_file(struct kv_string *filename){
	write_GL_modelview_file(filename, kemo_sgl->mesh_m->iflag_view_type, kemo_sgl->view_s);
}
void kemoview_load_modelview_file(struct kv_string *filename){
	read_GL_modelview_file(filename, kemo_sgl->mesh_m->iflag_view_type, kemo_sgl->view_s);
}



static void evolution_psf_viewer(){
	int id_load;
	printf("Loading PSF %d \n",kemo_sgl->psf_a->nmax_loaded);
	for(id_load=0; id_load<kemo_sgl->psf_a->nmax_loaded; id_load++){
		if(kemo_sgl->psf_a->iflag_loaded[id_load] > 0){
			printf("Loaded PSF file %d %d %s\n",id_load, kemo_sgl->psf_m[id_load]->iflag_psf_file,
                   kemo_sgl->psf_m[id_load]->psf_header->string);
			kemo_sgl->psf_m[id_load]->psf_step = kemo_sgl->psf_a->istep_sync;
			evolution_PSF_data(kemo_sgl->psf_d[id_load], kemo_sgl->psf_ucd_tmp, kemo_sgl->psf_m[id_load]);
		};
	}
	
	return;
}

int evolution_fline_viewer(){
	int ierr;
	if (kemo_sgl->fline_m->iflag_draw_fline > 0) {
		kemo_sgl->fline_m->fline_step = kemo_sgl->psf_a->istep_sync;
		ierr = refresh_FLINE_data(kemo_sgl->fline_d, kemo_sgl->psf_ucd_tmp, kemo_sgl->fline_m);
	}
	return ierr;
}

void kemoview_viewer_evolution(int istep){
    kemo_sgl->psf_a->istep_sync = istep;
    evolution_fline_viewer();
    evolution_psf_viewer();
	return;
}


void kemoview_draw_with_modified_domain_distance(){
	cal_range_4_mesh_c(kemo_sgl->mesh_d, kemo_sgl->view_s);
	modify_object_multi_viewer_c(kemo_sgl->mesh_m->dist_domains, kemo_sgl->mesh_d);
	return;
}

void kemoview_set_viewtype(int sel){
    kemo_sgl->mesh_m->iflag_view_type = set_viewtype(kemo_sgl->view_s, sel, kemo_sgl->mesh_m->iflag_view_type);
}

void kemoview_set_coastline_radius(double radius){kemo_sgl->mesh_m->radius_coast = radius;};
double kemoview_get_coastline_radius(){return kemo_sgl->mesh_m->radius_coast;};

void kemoview_set_object_property_flags(int selected, int iflag){
	if (selected == AXIS_TOGGLE) {set_axis_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {set_coastline_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {set_sphere_grid_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == SHADING_SWITCH) {set_shading_mode(iflag, kemo_sgl->mesh_m);}
    else if(selected == POLYGON_SWITCH) {set_polygon_mode(iflag, kemo_sgl->mesh_m);};
	return;
}

int kemoview_get_object_property_flags(int selected){
	if (selected == AXIS_TOGGLE) {return kemo_sgl->mesh_m->iflag_draw_axis;}
    else if(selected == COASTLINE_SWITCH) {return kemo_sgl->mesh_m->iflag_draw_coast;}
    else if(selected == SPHEREGRID_SWITCH) {return kemo_sgl->mesh_m->iflag_draw_sph_grid;}
    else if(selected == SHADING_SWITCH) {return kemo_sgl->mesh_m->shading_mode;}
    else if(selected == POLYGON_SWITCH) {return kemo_sgl->mesh_m->polygon_mode;};
    
	return 0;
}

int kemoview_toggle_object_properties(int selected){
	if (selected == AXIS_TOGGLE) {return toggle_draw_axis(kemo_sgl->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {return toggle_coastline_flag(kemo_sgl->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {return toggle_sphere_grid_flag(kemo_sgl->mesh_m);}
    else if(selected == SHADING_SWITCH) {return toggle_shading_mode(kemo_sgl->mesh_m);}
    else if(selected == POLYGON_SWITCH) {return toggle_polygon_mode(kemo_sgl->mesh_m);};
    
	return 0;
}

void kemoview_set_mesh_color_mode(int icolor)  {kemo_sgl->mesh_m->mesh_color_mode = icolor;};
void kemoview_set_num_of_color_loop(int icolor){kemo_sgl->mesh_m->num_of_color_loop = icolor;};

void kemoview_set_node_diamater(double diam)   {kemo_sgl->mesh_m->node_diam = diam;};
void kemoview_set_domain_distance(double dist){kemo_sgl->mesh_m->dist_domains = dist;};


void kemoview_set_domain_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->domain_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_domain_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_domain_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void kemoview_set_node_grp_color_flag(int icolor)  {select_node_grp_node_color(icolor, kemo_sgl->mesh_m);};

void kemoview_set_ele_grp_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->ele_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_ele_grp_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_ele_grp_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void kemoview_set_surf_grp_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->surf_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_surf_grp_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_surf_grp_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void kemoview_set_domain_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_surface_color_code);
        kemoview_set_domain_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_node_color_code);
    };
};

void kemoview_set_node_grp_color_code(GLfloat color_code4[4]){
    copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->node_node_color_code);
    return;
}

void kemoview_set_ele_grp_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_surface_color_code);
        kemoview_set_ele_grp_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_node_color_code);
    };
    return;
}

void kemoview_set_surf_grp_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_surface_color_code);
        kemoview_set_surf_grp_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_node_color_code);
    };
    return;
}


void kemoview_set_domain_opacity(double opacity_in){kemo_sgl->mesh_m->domain_opacity = opacity_in;};
void kemoview_set_ele_grp_opacity(double opacity_in)   {kemo_sgl->mesh_m->ele_grp_opacity = opacity_in;};
void kemoview_set_surf_grp_opacity(double opacity_in)  {kemo_sgl->mesh_m->surf_grp_opacity = opacity_in;};

double kemoview_get_domain_opacity(){return kemo_sgl->mesh_m->domain_opacity;};
double kemoview_get_ele_grp_opacity(){return kemo_sgl->mesh_m->ele_grp_opacity;};
double kemoview_get_surf_grp_opacity(){return kemo_sgl->mesh_m->surf_grp_opacity;};


int kemoview_get_draw_mesh_node() {return kemo_sgl->mesh_m->draw_surface_nod;};
int kemoview_get_draw_mesh_grid() {return kemo_sgl->mesh_m->draw_surface_grid;};
int kemoview_get_draw_mesh_patch(){return kemo_sgl->mesh_m->draw_surface_solid;};

void kemoview_set_mesh_draw_flag(int selected, int iflag){
	int num_pe = kemoview_get_num_subdomain();
	
	if     (selected == SURFSOLID_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_solid = iflag;
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_solid, num_pe, kemo_sgl->mesh_m->draw_domains_solid);
	}else if(selected == SURFGRID_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_grid = iflag;
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_grid, num_pe, kemo_sgl->mesh_m->draw_domains_grid);
	}else if(selected == SURFNOD_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_nod = iflag;
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_nod, num_pe, kemo_sgl->mesh_m->draw_domains_nod);
	};
	return;
};

void kemoview_mesh_draw_toggle(int selected){
	int num_pe = kemoview_get_num_subdomain();
	
	if     (selected == SURFSOLID_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_solid = toggle_value_c(kemo_sgl->mesh_m->draw_surface_solid);
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_solid, num_pe, kemo_sgl->mesh_m->draw_domains_solid);
	}else if(selected == SURFGRID_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_grid = toggle_value_c(kemo_sgl->mesh_m->draw_surface_grid);
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_grid, num_pe, kemo_sgl->mesh_m->draw_domains_grid);
	}else if(selected == SURFNOD_TOGGLE){
		kemo_sgl->mesh_m->draw_surface_nod = toggle_value_c(kemo_sgl->mesh_m->draw_surface_nod);
		set_draw_flag_for_all(kemo_sgl->mesh_m->draw_surface_nod, num_pe, kemo_sgl->mesh_m->draw_domains_nod);
	};
	return;
};


void kemoview_set_draw_domain_patch(int iflag, int i){
    kemo_sgl->mesh_m->draw_surface_solid = IONE;
    kemo_sgl->mesh_m->draw_domains_solid[i] = iflag;
}
void kemoview_set_draw_domain_grid(int iflag, int i) {
    kemo_sgl->mesh_m->draw_surface_grid = IONE;
    kemo_sgl->mesh_m->draw_domains_grid[i] = iflag;
}
void kemoview_set_draw_domain_nod(int iflag, int i)  {
    kemo_sgl->mesh_m->draw_surface_nod = IONE;
    kemo_sgl->mesh_m->draw_domains_nod[i] = iflag;
}


void kemoview_set_draw_nodgrp_node(int iflag, int i){kemo_sgl->mesh_m->draw_nodgrp_nod[i] = iflag;};

void kemoview_set_draw_elegrp_patch(int iflag, int i){kemo_sgl->mesh_m->draw_elegrp_solid[i] = iflag;};
void kemoview_set_draw_elegrp_grid(int iflag, int i) {kemo_sgl->mesh_m->draw_elegrp_grid[i] = iflag;};
void kemoview_set_draw_elegrp_node(int iflag, int i)  {kemo_sgl->mesh_m->draw_elegrp_nod[i] = iflag;};

void kemoview_set_draw_surfgrp_patch(int iflag, int i){kemo_sgl->mesh_m->draw_surfgrp_solid[i] = iflag;};
void kemoview_set_draw_surfgrp_grid(int iflag, int i) {kemo_sgl->mesh_m->draw_surfgrp_grid[i] = iflag;};
void kemoview_set_draw_surfgrp_node(int iflag, int i)  {kemo_sgl->mesh_m->draw_surfgrp_nod[i] = iflag;};


int kemoview_get_draw_nodgrp_node(int i){return kemo_sgl->mesh_m->draw_nodgrp_nod[i];};

int kemoview_get_draw_elegrp_patch(int i){return kemo_sgl->mesh_m->draw_elegrp_solid[i];};
int kemoview_get_draw_elegrp_grid(int i) {return kemo_sgl->mesh_m->draw_elegrp_grid[i];};
int kemoview_get_draw_elegrp_node(int i)  {return kemo_sgl->mesh_m->draw_elegrp_nod[i];};

int kemoview_get_draw_surfgrp_patch(int i){return kemo_sgl->mesh_m->draw_surfgrp_solid[i];};
int kemoview_get_draw_surfgrp_grid(int i) {return kemo_sgl->mesh_m->draw_surfgrp_grid[i];};
int kemoview_get_draw_surfgrp_node(int i)  {return kemo_sgl->mesh_m->draw_surfgrp_nod[i];};

void kemoview_nod_grp_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_nod_sf, kemo_sgl->mesh_m->draw_nodgrp_nod);
	return;
}


void kemoview_ele_grp_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_ele_sf, kemo_sgl->mesh_m->draw_elegrp_solid);
	return;
}

void kemoview_ele_grp_nod_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_ele_sf, kemo_sgl->mesh_m->draw_elegrp_nod);
	return;
}

void kemoview_ele_grp_grid_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_ele_sf, kemo_sgl->mesh_m->draw_elegrp_grid);
	return;
}


void kemoview_surf_grp_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_surf_sf, kemo_sgl->mesh_m->draw_surfgrp_solid);
	return;
}

void kemoview_surf_grp_nod_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_surf_sf, kemo_sgl->mesh_m->draw_surfgrp_nod);
	return;
}

void kemoview_surf_grp_grid_toggle(int selected){
	select_draw_flag_toggle(selected, kemo_sgl->mesh_d->ngrp_surf_sf, kemo_sgl->mesh_m->draw_surfgrp_grid);
	return;
}

int kemoview_get_draw_mesh_flag(){return kemo_sgl->mesh_m->iflag_draw_mesh;};

int kemoview_get_num_subdomain()    {return kemo_sgl->mesh_d->num_pe_sf;};
int kemoview_get_num_node_grp() {return kemo_sgl->mesh_d->ngrp_nod_sf;};
int kemoview_get_num_ele_grp() {return kemo_sgl->mesh_d->ngrp_ele_sf;};
int kemoview_get_num_surf_grp(){return kemo_sgl->mesh_d->ngrp_surf_sf;};


void kemoview_get_node_grp_name(struct kv_string *groupname, int i){
    alloc_copy_string(kemo_sgl->mesh_d->nod_gp_name_sf[i], groupname);
};
void kemoview_get_ele_grp_name(struct kv_string *groupname, int i){ 
    alloc_copy_string(kemo_sgl->mesh_d->ele_gp_name_sf[i], groupname);
};
void kemoview_get_surf_grp_name(struct kv_string *groupname, int i){
    alloc_copy_string(kemo_sgl->mesh_d->surf_gp_name_sf[i], groupname); 
};

int kemoview_get_draw_type_flag(){return kemo_sgl->mesh_m->iflag_draw_type;};
int kemoview_get_view_type_flag(){return kemo_sgl->mesh_m->iflag_view_type;};

int kemoview_get_num_of_color_loop(){return kemo_sgl->mesh_m->num_of_color_loop;};

double kemoview_get_node_diamater()   {return kemo_sgl->mesh_m->node_diam;};
double kemoview_get_domain_distance(){return kemo_sgl->mesh_m->dist_domains;};


void kemoview_get_ext_from_file_name(struct kv_string *filename,
                                     struct kv_string *stripped_prefix, struct kv_string *stripped_ext){
    alloc_kvstringitem(strlen(filename->string), stripped_prefix);
    alloc_kvstringitem(strlen(filename->string), stripped_ext);
	get_ext_from_file_name_c(filename->string, stripped_prefix->string, stripped_ext->string);
}
void kemoview_add_ext_to_file_name(struct kv_string *file_prefix, struct kv_string *added_ext,
                                   struct kv_string *file_name){
    int lentgh = strlen(file_prefix->string) + strlen(added_ext->string);
    alloc_kvstringitem(lentgh+2, file_name);
    add_ext_to_file_name_c(file_prefix->string, added_ext->string, file_name->string);
}

void kemoview_set_text_color_code(float c_code[4]){copy_rgba_color_c(c_code, kemo_sgl->mesh_m->text_color);};
void kemoview_get_text_color_code(float c_code[4]){copy_rgba_color_c(kemo_sgl->mesh_m->text_color, c_code);};




void kemoview_get_fliped_img(int npixel_x, int npixel_y,
                               unsigned char *glimage, unsigned char *fliped_img){
    get_gl_buffer_to_bmp(npixel_x, npixel_y, glimage);
    flip_gl_bitmap(npixel_x, npixel_y, glimage, fliped_img);
    return;
}

void kemoview_write_window_to_vector_file(int iflag_img, struct kv_string *file_prefix){
	sel_gl_buffer_2_vector_img(iflag_img, file_prefix->string);
}

void kemoview_set_PSF_by_rgba_texture(int width, int height, const unsigned char *bgra_in){
    set_texture_psf_from_bgra(kemo_sgl->psf_current_menu, width, height, bgra_in);
};

void kemoview_modify_view(){modify_stereo_kemoview(kemo_sgl->mesh_m, kemo_sgl->view_s);};
void kemoview_rotate(){rotate_stereo_kemoview(kemo_sgl->mesh_m, kemo_sgl->view_s);};

void kemoviewer_reset_to_init_angle(){
    reset_all_view_parameter(kemo_sgl->view_s);
    init_rot_animation(kemo_sgl->view_s);
};


void kemoview_set_retinamode(int i_retina){
    set_gl_retinamode(kemo_sgl->view_s, i_retina);
}

void kemoview_set_windowsize(GLint npixel_x, GLint npixel_y){
	set_gl_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
};

void kemoview_update_projection_by_viewer_size(GLint npixel_x, GLint npixel_y){
	update_projection_by_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
};


void kemoview_update_distance(){
	update_projection_struct(kemo_sgl->view_s);
};

void kemoview_set_rotation_parameter(GLdouble rot_vect[4]){
	set_gl_rotation_parameter(kemo_sgl->view_s, rot_vect);
};
void kemoview_set_dragging_rotation(GLdouble rot_vect[4]){
	set_gl_dragging_rotation(kemo_sgl->view_s, rot_vect);
};
void kemoview_set_animation_rot_axis(int iaxis){
	set_gl_animation_rot_axis(kemo_sgl->view_s, iaxis);
}
void kemoview_set_animation_rot_angle(int int_degree){
	set_gl_animation_rot_angle(kemo_sgl->view_s, int_degree);
}
void kemoview_set_shift_vector(GLdouble position[3]){
	set_gl_shift_vector(kemo_sgl->view_s, position);
};
void kemoview_set_scale_factor(GLdouble scale_s){
	set_gl_scalar_scale_factor(kemo_sgl->view_s, scale_s);
};
void kemoview_set_projection_aperture(GLdouble aperture_s){
	set_gl_projection_aperture(kemo_sgl->view_s, aperture_s);
};
void kemoview_set_stereo_parameter(GLdouble focus, GLdouble eye_sep){
	set_gl_stereo_parameter(kemo_sgl->view_s, focus, eye_sep);
};


void kemoview_get_windowsize(GLint *npixel_x, GLint *npixel_y){
	send_gl_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
}
void kemoview_get_rotation_parameter(GLdouble rot_vect[4]){
	send_gl_rotation_parameter(kemo_sgl->view_s, rot_vect);
}

void kemoview_get_shift_vector(GLdouble position[3]){
	send_gl_shift_vector(kemo_sgl->view_s, position);
}

void kemoview_get_lookat_vector(GLdouble position[3]){
	send_gl_lookat_vector(kemo_sgl->view_s, position);
}

GLdouble kemoview_get_scale_factor(){
    return send_scalar_scale_factor(kemo_sgl->view_s);
}

GLdouble kemoview_get_projection_aperture(){
	return send_gl_projection_aperture(kemo_sgl->view_s);
}
void kemoview_get_projection_parameters(GLdouble *aperture_s, GLdouble *near_s,
                                        GLdouble *far_s, GLdouble *aspect_s){
	send_gl_projection_parameters(kemo_sgl->view_s, aperture_s, near_s, far_s, aspect_s);
}

GLdouble kemoview_get_stereo_parameters(){return send_gl_stereo_parameters(kemo_sgl->view_s);};
GLdouble kemoview_get_stereo_eyeseparation(){return send_gl_stereo_eyeseparation(kemo_sgl->view_s);};

void kemoview_mousedolly(GLdouble start[2], GLdouble x_dolly, GLdouble y_dolly){
	gl_mousedolly_struct(kemo_sgl->view_s, start, x_dolly, y_dolly);
}
void kemoview_mousepan(GLdouble start[2], GLdouble x_pan, GLdouble y_pan){
	gl_mousepan_struct(kemo_sgl->view_s, start, x_pan, y_pan);
}
void kemoview_zooming(GLdouble wheelDelta){
	gl_zooming_struct(kemo_sgl->view_s, wheelDelta);
}

/* called with the start position and the window origin + size */
void kemoview_startTrackball(GLdouble x, GLdouble y){gl_startTrackball(x, y, kemo_sgl->view_s);};
/* calculated rotation based on current mouse position */
void kemoview_rollToTrackball(GLdouble x, GLdouble y){ gl_rollToTrackball (x, y, kemo_sgl->view_s);};
/* add a GL rotation (dA) to an existing GL rotation (A) */
void kemoview_drugging_addToRotationTrackball(){
    gl_drag_addToRotationTrackball(kemo_sgl->view_s);
}

void kemoview_animation_add_rotation(GLdouble dt){add_animation_rotation(kemo_sgl->view_s, dt);}
void kemoview_reset_animation(){reset_rot_animation(kemo_sgl->view_s);};


void kemoview_set_stereo_shutter(int iflag){kemo_sgl->mesh_m->iflag_streo_stutter = iflag;}
void kemoview_set_anaglyph_flag(int iflag){kemo_sgl->mesh_m->iflag_streo_anaglyph = iflag;}
int kemoview_get_stereo_shutter(){return kemo_sgl->mesh_m->iflag_streo_stutter;}
int kemoview_get_anaglyph_flag(){return kemo_sgl->mesh_m->iflag_streo_anaglyph;}

void kemoview_draw_glut_menubottun(){draw_menubottun_gl();}
void kemoview_draw_glut_menubottun3(){draw_menubottun_gl3();}

/* Subroutines for surface rendering */
void kemoview_set_PSF_num_loaded(int num){kemo_sgl->psf_a->num_loaded = num;};
void kemoview_set_PSF_max_loaded(int num){kemo_sgl->psf_a->nmax_loaded = num;};
void kemoview_set_loaded_PSF_flag(int id_psf, int iflag){kemo_sgl->psf_a->iflag_loaded[id_psf] = iflag;};
void kemoview_set_current_PSF(int id_psf){
    kemo_sgl->psf_a->id_current = id_psf;
    kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
    kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
}

int kemoview_get_PSF_num_loaded(){return kemo_sgl->psf_a->num_loaded;};
int kemoview_get_PSF_max_loaded(){return kemo_sgl->psf_a->nmax_loaded;};
int kemoview_get_PSF_loaded_flag(int id_psf){return kemo_sgl->psf_a->iflag_loaded[id_psf];};
int kemoview_get_curent_PSF_ID(){return kemo_sgl->psf_a->id_current;};
int kemoview_get_curent_PSF_filename(){return kemo_sgl->psf_a->id_current;};


void kemoview_get_PSF_full_path_file_name(struct kv_string *ucd_m){
	alloc_set_ucd_file_name_by_psf(kemo_sgl->psf_current_menu, ucd_m);
	return;
}
int kemoview_get_PSF_full_path_file_prefix(struct kv_string *psf_filehead, int *iflag){
    return send_each_psf_file_header_full(kemo_sgl->psf_current_menu, psf_filehead, iflag);
}

int kemoview_get_PSF_file_prefix(struct kv_string *stripped_filehead){
	struct kv_string* stripped_dir = alloc_kvstring();
	int istep = send_each_psf_file_dir_head(kemo_sgl->psf_current_menu,
				stripped_dir, stripped_filehead);
	dealloc_kvstring(stripped_dir);
	return istep;
}

void kemoview_set_PSF_field(int sel){
	set_PSF_field(sel, kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};
void kemoview_set_PSF_component(int sel){
	set_PSF_component(sel, kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};

int kemoview_get_PSF_num_field()         {return send_nfield_each_psf(kemo_sgl->psf_current_data);};
int kemoview_get_PSF_ncomptot()          {return send_ncomptot_each_psf(kemo_sgl->psf_current_data);};
int kemoview_get_PSF_num_component(int i){return send_ncomp_each_psf(kemo_sgl->psf_current_data, i);};
void kemoview_get_PSF_field_name(struct kv_string *colorname, int i){
    send_each_psf_data_name(kemo_sgl->psf_current_data, colorname, i);
};

int kemoview_get_PSF_draw_switch(){return kemo_sgl->psf_a->iflag_loaded[kemo_sgl->psf_a->id_current];};

int kemoview_get_PSF_field_id()    {return send_field_draw_each_psf(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_component_id(){return send_draw_comp_id_psf(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_draw_data_address(){return send_draw_component_psf(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_coordinate_id(){
    return send_coordinate_id_psf(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};



void kemoview_set_PSF_polygon_mode(int iflag){set_psf_polygon_mode(kemo_sgl->psf_current_menu, iflag);};
void kemoview_set_PSF_tangential_vec_mode(int iflag){set_psf_vector_mode(kemo_sgl->psf_current_menu, iflag);};

int kemoview_get_PSF_draw_refv()  {return send_draw_psf_refv(kemo_sgl->psf_current_menu);};
int kemoview_toggle_PSF_draw_refv(){return toggle_draw_psf_refv(kemo_sgl->psf_current_menu);};

void kemoview_set_PSF_patch_color_mode(int iflag){set_psf_patch_color_mode(kemo_sgl->psf_current_menu, iflag);};

void kemoview_set_PSF_isoline_color_mode(int iflag)  {set_each_isoline_color(kemo_sgl->psf_current_menu, iflag);};
void kemoview_set_PSF_num_isoline(int nlline)        {set_each_n_isoline(kemo_sgl->psf_current_menu, nlline);};
void kemoview_set_PSF_vector_increment(int increment){set_each_increment_vect(kemo_sgl->psf_current_menu, increment);};
void kemoview_set_PSF_vector_scale(double scale)     {set_each_scale_vect(kemo_sgl->psf_current_menu, scale);};
void kemoview_set_PSF_vector_thickness(double size)  {set_each_vector_thick(kemo_sgl->psf_current_menu, size);};

int kemoview_get_PSF_patch_color_mode()    {return send_each_psf_patch_color(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_isoline_color_mode()  {return send_each_isoline_color(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_num_isoline()         {return send_num_isoline(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_vector_color_mode()   {return send_each_vector_patch_color(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_vector_increment()    {return send_increment_vector(kemo_sgl->psf_current_menu);};
double kemoview_get_PSF_vector_scale()     {return send_scale_vector(kemo_sgl->psf_current_menu);};
double kemoview_get_PSF_vector_thickness() {return send_vector_thick(kemo_sgl->psf_current_menu);};


int kemoview_get_PSF_draw_flags(int selected){
	if      (selected == PSFSOLID_TOGGLE)    {return send_draw_psf_solid(kemo_sgl->psf_current_menu);}
	else if (selected == PSFGRID_TOGGLE)     {return send_draw_psf_grid(kemo_sgl->psf_current_menu);}
	else if (selected == ZEROGRID_TOGGLE)    {return send_draw_psf_zero(kemo_sgl->psf_current_menu);}
	else if (selected == COLORBAR_TOGGLE)    {return send_draw_psf_cbar(kemo_sgl->psf_current_menu);}
	else if (selected == PSF_POLYGON_SWITCH) {return send_each_psf_polygon_mode(kemo_sgl->psf_current_menu);}
	else if (selected == PSFVECT_TOGGLE)     {return send_draw_psf_vect(kemo_sgl->psf_current_menu);}
    else if (selected == PSFTANVEC_TOGGLE)   {return send_each_psf_vector_mode(kemo_sgl->psf_current_menu);};
	return 0;
}

int kemoview_select_PSF_draw_switch(int selected){
	int toggle = 0;
	
	if      (selected == PSFSOLID_TOGGLE)    {return toggle_draw_psf_solid(kemo_sgl->psf_current_menu);}
	else if (selected == PSFGRID_TOGGLE)     {return toggle_draw_psf_grid(kemo_sgl->psf_current_menu);}
	else if (selected == ZEROGRID_TOGGLE)    {return toggle_draw_psf_zero(kemo_sgl->psf_current_menu);}
	else if (selected == COLORBAR_TOGGLE)    {return toggle_draw_psf_cbar(kemo_sgl->psf_current_menu);}
	else if (selected == PSF_POLYGON_SWITCH) {return toggle_each_psf_polygon_mode(kemo_sgl->psf_current_menu);}
	else if (selected == PSFVECT_TOGGLE)     {return toggle_draw_psf_vect(kemo_sgl->psf_current_menu);}
	else if (selected == PSFREFV_TOGGLE)     {return toggle_draw_psf_refv(kemo_sgl->psf_current_menu);}
    else if (selected == PSFTANVEC_TOGGLE)   {return toggle_each_psf_vector_mode(kemo_sgl->psf_current_menu);}
	else if (selected == WHITE_PSF_VECT)     {set_each_vector_patch_color(kemo_sgl->psf_current_menu, WHITE_SURFACE);}
	else if (selected == RAINBOW_PSF_VECT)   {set_each_vector_patch_color(kemo_sgl->psf_current_menu, RAINBOW_SURFACE);};
	return toggle;
}

void kemoview_set_PSF_color_mode(int isel){set_PSF_color_mode_id(kemo_sgl->psf_current_menu, isel);}
int kemoview_get_PSF_color_mode(){return send_PSF_color_mode_id(kemo_sgl->psf_current_menu);}

double kemoview_get_PSF_min_data(int i){return send_psf_data_min(kemo_sgl->psf_current_data, i);};
double kemoview_get_PSF_max_data(int i){return send_psf_data_max(kemo_sgl->psf_current_data, i);};



void kemoview_delete_PSF_color_list(int i_delete){
    delete_PSF_color_index_list(kemo_sgl->psf_current_menu, i_delete);
}
void kemoview_delete_PSF_opacity_list(int i_delete){
    delete_PSF_opacity_index_list(kemo_sgl->psf_current_menu, i_delete);
}

void kemoview_add_PSF_color_list(double add_value, double add_color){
    add_PSF_color_index_list(kemo_sgl->psf_current_menu, add_value, add_color);
}
void kemoview_add_PSF_opacity_list(double add_value, double add_opacity){
    add_PSF_opacity_index_list(kemo_sgl->psf_current_menu, add_value, add_opacity);
}

void kemoview_set_PSF_linear_colormap(double minvalue, double maxvalue){
    set_PSF_linear_colormap(kemo_sgl->psf_current_menu, minvalue, maxvalue);
}

void kemoview_set_PSF_single_color(double *rgba){
    set_PSF_fixed_color(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu, rgba);
}

void kemoview_set_PSF_constant_opacity(double opacity){
    set_PSF_constant_opacity(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu, opacity);
}

struct colormap_params * kemoview_get_psf_colormap_params(){
    return kemo_sgl->psf_current_menu->cmap_psf;
};
void kemoview_get_PSF_rgb_at_value(double value, double *red, double *green, double *blue){
    set_PSF_rgb_from_value(kemo_sgl->psf_current_menu, value, red, green, blue);
}
double kemoview_get_PSF_opacity_at_value(double value){
    return get_PSF_opacity_at_value(kemo_sgl->psf_current_menu, value);
}
void kemoview_set_PSF_color_data(int i_point, double value, double color){
    set_each_PSF_color_point(kemo_sgl->psf_current_menu, i_point, value, color);
}
void kemoview_set_PSF_opacity_data(int i_point, double value, double opacity){
    set_each_PSF_opacity_point(kemo_sgl->psf_current_menu, i_point, value, opacity);
}

double kemoview_get_PSF_color_table_min(){return send_each_PSF_color_table_min(kemo_sgl->psf_current_menu);};
double kemoview_get_PSF_color_table_max(){return send_each_PSF_color_table_max(kemo_sgl->psf_current_menu);};
double kemoview_get_PSF_min_opacity(){return send_each_PSF_minimum_opacity(kemo_sgl->psf_current_menu);};
double kemoview_get_PSF_max_opacity(){return send_each_PSF_maximum_opacity(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_color_table_num(){return send_each_PSF_color_table_num(kemo_sgl->psf_current_menu);};
int kemoview_get_PSF_opacity_table_num(){return send_each_PSF_opacity_table_num(kemo_sgl->psf_current_menu);};

void kemoview_get_PSF_color_items(int i_point, double *value, double *color){
    send_each_PSF_color_table_items(kemo_sgl->psf_current_menu, i_point, value, color);
}
void kemoview_get_PSF_opacity_items(int i_point, double *value, double *opacity){
    send_each_PSF_opacity_table_items(kemo_sgl->psf_current_menu, i_point, value, opacity);
}

void kemoview_write_PSF_colormap_file(struct kv_string *filename){
    write_each_PSF_colormap_control_file(kemo_sgl->psf_current_menu, filename->string);
}
void kemoview_read_PSF_colormap_file(struct kv_string *filename){
    read_each_PSF_colormap_control_file(kemo_sgl->psf_current_menu, filename->string);
}
void kemoview_check_PSF_colormap_control(){
    check_each_PSF_colormap_control(kemo_sgl->psf_current_menu);
}


/* Subroutines for field lines */

void kemoview_get_fline_full_path_file_name(struct kv_string *ucd_m){
	alloc_set_ucd_file_name_by_fline(kemo_sgl->fline_m, ucd_m);
	return;
}
int kemoview_get_fline_file_step_prefix(struct kv_string *fline_filehead){
	alloc_copy_string(kemo_sgl->fline_m->fline_header->string, fline_filehead);
	return kemo_sgl->fline_m->fline_step;
};
void kemoview_set_fline_file_step(int istep){kemo_sgl->fline_m->fline_step = istep;};

void kemoview_set_fline_switch(int iflag) {kemo_sgl->fline_m->iflag_draw_fline = iflag;};
void kemoview_set_fline_color_type(int iflag) {kemo_sgl->fline_m->fieldline_color = iflag;};
void kemoview_set_fline_color_field(int sel){
    set_fline_color_field(sel, kemo_sgl->fline_d, kemo_sgl->fline_m);
};
void kemoview_set_fline_color_component(int sel){
    set_fline_color_component(sel, kemo_sgl->fline_d, kemo_sgl->fline_m);
};


int kemoview_get_fline_switch(){return kemo_sgl->fline_m->iflag_draw_fline;};
int kemoview_get_fline_color_num_field(){return kemo_sgl->fline_d->nfield;};
int kemoview_get_fline_color_ncomptot(){return kemo_sgl->fline_d->ncomptot;};
int kemoview_get_fline_color_num_comps(int i){return kemo_sgl->fline_d->ncomp[i];};
int kemoview_get_fline_color_istack(int i){return kemo_sgl->fline_d->istack_comp[i];};
void kemoview_get_fline_color_data_name(struct kv_string *colorname, int i){
    alloc_copy_string(kemo_sgl->fline_d->data_name[i], colorname);
};
int kemoview_get_fline_color_field(){return kemo_sgl->fline_m->if_draw_fline;};
int kemoview_get_fline_color_component(){return kemo_sgl->fline_m->ic_draw_fline;};
int kemoview_get_fline_color_data_adress(){return kemo_sgl->fline_m->icomp_draw_fline;};
int kemoview_get_fline_colormode() {return kemo_sgl->fline_m->fieldline_color;};


void kemoview_set_fline_type(int iflag) {kemo_sgl->fline_m->fieldline_type = iflag;};
int kemoview_get_fline_type() {return kemo_sgl->fline_m->fieldline_type;};
int kemoview_toggle_fline_type(){
	return kemo_sgl->fline_m->fieldline_type = toggle_value_c(kemo_sgl->fline_m->fieldline_type);
};


void kemoview_set_fline_thickness(double thick) {kemo_sgl->fline_m->fieldline_thick = thick;};
double kemoview_get_fline_thickness() {return kemo_sgl->fline_m->fieldline_thick;};

double kemoview_get_fline_data_min(int i){return kemo_sgl->fline_d->d_min[i];};
double kemoview_get_fline_data_max(int i){return kemo_sgl->fline_d->d_max[i];};


void kemoview_set_fline_linear_colormap(double minvalue, double maxvalue){
	set_linear_colormap(kemo_sgl->fline_m->cmap_fline, minvalue, maxvalue);
}
void kemoview_set_fline_constant_opacity(double opacity){
	set_constant_opacitymap(kemo_sgl->fline_m->cmap_fline,
                            kemo_sgl->fline_d->d_min[kemo_sgl->fline_m->icomp_draw_fline],
                            kemo_sgl->fline_d->d_max[kemo_sgl->fline_m->icomp_draw_fline], opacity);
}

void kemoview_get_fline_rgb_at_value(double value, double *red, double *green, double *blue){
	set_rgb_from_value_s(kemo_sgl->fline_m->cmap_fline, value, red, green, blue);
}

double kemoview_get_fline_opacity_at_value(double value){
	return set_opacity_from_value_s(kemo_sgl->fline_m->cmap_fline, value);
}
void kemoview_set_fline_color_data(int i_point, double value, double color){
	set_each_color_point_s(kemo_sgl->fline_m->cmap_fline, i_point, value, color);
}
void kemoview_set_fline_opacity_data(int i_point, double value, double opacity){
	set_each_opacity_point_s(kemo_sgl->fline_m->cmap_fline, i_point, value, opacity);
}

void kemoview_set_fline_color_mode_id(int isel){
	set_color_mode_by_id(kemo_sgl->fline_m->cmap_fline, isel);
}

double kemoview_get_fline_min_color(){
	double d, c;
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, 0, &d, &c);
	return d;
}
double kemoview_get_fline_max_color(){
	double d, c;
	int n = send_color_table_num_s(kemo_sgl->fline_m->cmap_fline);
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, n-1, &d, &c);
	return d;
}
double kemoview_get_fline_min_opacity(){return send_minimum_opacity_s(kemo_sgl->fline_m->cmap_fline);};
double kemoview_get_fline_max_opacity(){return send_maximum_opacity_s(kemo_sgl->fline_m->cmap_fline);};

int kemoview_get_fline_color_num()  {return send_color_table_num_s(kemo_sgl->fline_m->cmap_fline);};
int kemoview_get_fline_opacity_num(){return send_opacity_table_num_s(kemo_sgl->fline_m->cmap_fline);};


void kemoview_get_fline_color_item(int i_point, double *value, double *color){
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, i_point, value, color);
}
void kemoview_get_fline_opacity_item(int i_point, double *value, double *opacity){
	send_opacity_table_items_s(kemo_sgl->fline_m->cmap_fline, i_point, value, opacity);
}

void kemoview_write_fline_colormap_file(struct kv_string *filename){
	write_colormap_control_file_s(filename->string, kemo_sgl->fline_m->cmap_fline);
}
void kemoview_read_fline_colormap_file(struct kv_string *filename){
	read_colormap_control_file_s(filename->string, kemo_sgl->fline_m->cmap_fline);
}

/*  Routines using libpng */
#ifdef PNG_OUTPUT
int kemoview_set_image_file_format_id(struct kv_string *image_ext){
    return set_image_format_id_by_ext(image_ext->string);
}

void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix){
    write_gl_window_to_file(iflag_img, image_prefix->string, 
                            kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}
void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix){
    write_gl_window_step_file(iflag_img, istep, image_prefix->string, 
                              kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}

void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix){
    set_texture_to_psf(img_fmt, image_prefix->string, kemo_sgl->psf_current_menu);
};
#endif

