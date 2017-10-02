
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
	
	struct psf_menu_val       *psf_current_menu;
	struct psf_data           *psf_current_data;
	
	struct ucd_file_menu_val  *ucd_menu;
	struct psf_data           *psf_ucd_tmp;
	
	struct kemoviewer_type    *next;
};

struct mul_kemoviewer_type{
	int num_window;
	int id_current;
	struct kemoviewer_type   **kemo_mul;
};

struct kemoviewer_type *kemo_sgl;

void allocate_kemoviwewer_pointers(){
	int i;
	
	kemo_sgl->view_s = (struct view_element *)      malloc(sizeof(struct view_element));
	kemo_sgl->gl_buf = (struct buffer_for_gl *)     malloc(sizeof(struct buffer_for_gl));
	
	
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
	kemo_sgl->ucd_menu =   (struct ucd_file_menu_val *) malloc(sizeof(struct ucd_file_menu_val));
	return;
}

void allocate_kemoviwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh){
	/*! Initialize mesh data*/
	kemoviewer_data = (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
    
    kemo_sgl = kemoviewer_data;
	allocate_kemoviwewer_pointers();
    
	init_kemoview_array(kemo_sgl->psf_a->nlimit_loaded, kemo_sgl->psf_a);
    
	init_kemoviewer(iflag_dmesh, kemo_sgl->mesh_d, kemo_sgl->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->fline_m);
	
	return;
}

void allocate_single_kemoviwewer_struct(struct kemoviewer_type *kemoviewer_data){
	/*! Initialize mesh data*/
	kemoviewer_data = (struct kemoviewer_type *)malloc(sizeof(struct kemoviewer_type));
	kemoviewer_data->window_ID = 0;
    
    kemo_sgl = kemoviewer_data;
	allocate_kemoviwewer_pointers();
    
	init_kemoview_array(kemo_sgl->psf_a->nlimit_loaded, kemo_sgl->psf_a);
    
	init_kemoviewer(IZERO, kemo_sgl->mesh_d, kemo_sgl->mesh_m, kemo_sgl->view_s);
	init_fline_parameters(kemo_sgl->fline_m);
	
	return;
}

void deallocate_kemoviwewer_pointers(struct kemoviewer_type *kemoviewer_data){
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
	free(kemoviewer_data->ucd_menu);
	
	return;
}

int send_nlimit_load_psf(){return kemo_sgl->psf_a->nlimit_loaded;};



/* Routines for Kemoviewer arrays */


void set_single_kemoview_ID(int id_window){
    if(id_window != kemo_sgl->window_ID){printf("Something wrong in window ID \n");};
    return;
}

void set_current_kemoview(int id_window, struct mul_kemoviewer_type *kemoview_array){
    if(id_window > kemoview_array->num_window){printf("Something wrong in window ID \n");};
	kemoview_array->id_current = id_window;
	return;
}
int send_current_kemoview(){return kemo_sgl->window_ID;};

/* Routines for draw by OpenGL */

void draw_kemoviewer_c(){
    /*    printf("Draw objects to ID: %d\n", kemo_sgl->view_s->gl_drawID);*/
	draw_objects(kemo_sgl->mesh_d, kemo_sgl->psf_d, kemo_sgl->fline_d, kemo_sgl->mesh_m, 
                 kemo_sgl->psf_m, kemo_sgl->psf_a, kemo_sgl->fline_m, kemo_sgl->view_s, kemo_sgl->gl_buf);
	return;
};

void draw_kemoviewer_to_ps(){
    /*    printf("Draw objects to ID: %d\n", kemo_sgl->view_s->gl_drawID);*/
    kemo_sgl->view_s->iflag_write_ps = ON;
	draw_objects(kemo_sgl->mesh_d, kemo_sgl->psf_d, kemo_sgl->fline_d, kemo_sgl->mesh_m,
                 kemo_sgl->psf_m, kemo_sgl->psf_a, kemo_sgl->fline_m, kemo_sgl->view_s, kemo_sgl->gl_buf);
    kemo_sgl->view_s->iflag_write_ps = OFF;
	return;
};

void kemoviewer_initial_lighting(){
	kemo_sgl->view_s->gl_drawID = glGenLists(IONE);
	
	kemo_gl_initial_lighting_c(kemo_sgl->view_s);
	/* ! set bitmap font list (8x12) */
	init_colorbar_fonts();
}

void init_kemoview_background_color(){init_bg_color_kemoview(kemo_sgl->mesh_m);}
void set_kemoview_background_color(GLfloat color[4]) {
    copy_rgba_color_c(color, kemo_sgl->mesh_m->bg_color);
    set_bg_color_kemoview(kemo_sgl->mesh_m);
};
void send_background_color(GLfloat color[4]){copy_rgba_color_c(kemo_sgl->mesh_m->bg_color, color);};


/* Routines for menu selection */
int kemoview_open_data_glut(const char *file_name){
	int iflag_datatype;
	iflag_datatype = kemoview_open_data(file_name, kemo_sgl->mesh_d, kemo_sgl->mesh_m,
				kemo_sgl->psf_a, kemo_sgl->psf_d, kemo_sgl->psf_m,
				kemo_sgl->fline_d, kemo_sgl->fline_m,
				kemo_sgl->psf_ucd_tmp, kemo_sgl->ucd_menu,kemo_sgl->view_s);
    
	if (kemo_sgl->psf_a->id_current >= IZERO) {
		kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
		kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
	};
    
	return iflag_datatype;
}

void close_mesh_view(){
	kemo_sgl->mesh_m->iflag_draw_mesh = 0;
	dealloc_all_mesh_4_viewer_s(kemo_sgl->mesh_d);
	dealloc_draw_mesh_flags(kemo_sgl->mesh_m);
	return;
}

int close_psf_view(){
	dealloc_draw_psf_flags(kemo_sgl->psf_d[kemo_sgl->psf_a->id_current],
                           kemo_sgl->psf_m[kemo_sgl->psf_a->id_current]);
	deallc_all_psf_data(kemo_sgl->psf_d[kemo_sgl->psf_a->id_current]);
	
	set_close_current_kemoview_array(kemo_sgl->psf_a);
    
	kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
	kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
    
	return send_num_loaded_PSF();
}

void close_fline_view(){
	kemo_sgl->fline_m->iflag_draw_fline = IZERO;
	dealloc_draw_fline_flags(kemo_sgl->fline_d, kemo_sgl->fline_m);
	deallc_all_fline_data(kemo_sgl->fline_d);
	return;
}

void set_to_pick_surface_command(const char *command){
	strngcopy(kemo_sgl->mesh_m->pick_surface_command, command);
};
void send_pick_surface_command(char *command){
	strngcopy(command, kemo_sgl->mesh_m->pick_surface_command);
};

void write_modelview_file_glut(const char *file_name){
	write_GL_modelview_file(file_name, kemo_sgl->mesh_m->iflag_view_type, kemo_sgl->view_s);
}
void load_modelview_file_glut(const char *file_name){
	read_GL_modelview_file(file_name, kemo_sgl->mesh_m->iflag_view_type, kemo_sgl->view_s);
}



static void evolution_psf_viewer(){
	int id_load;
	printf("Loading PSF %d \n",kemo_sgl->psf_a->nmax_loaded);
	for(id_load=0; id_load<kemo_sgl->psf_a->nmax_loaded; id_load++){
		if(kemo_sgl->psf_a->iflag_loaded[id_load] > 0){
			printf("Loaded PSF file %d %d %s\n",id_load, kemo_sgl->psf_m[id_load]->iflag_psf_file,
                   kemo_sgl->psf_m[id_load]->psf_header);
			kemo_sgl->psf_m[id_load]->psf_step = kemo_sgl->psf_a->istep_sync;
			evolution_PSF_data(kemo_sgl->psf_d[id_load], kemo_sgl->psf_ucd_tmp,
                               kemo_sgl->psf_m[id_load], kemo_sgl->ucd_menu);
		};
	}
	
	return;
}

void evolution_fline_viewer(){
	int ierr;
	if (kemo_sgl->fline_m->iflag_draw_fline > 0) {
		kemo_sgl->fline_m->fline_step = kemo_sgl->psf_a->istep_sync;
		ierr = refresh_FLINE_data(kemo_sgl->fline_d, kemo_sgl->psf_ucd_tmp,
                                  kemo_sgl->fline_m, kemo_sgl->ucd_menu);
	}
	return;
}

void evolution_viewer(int istep){
    kemo_sgl->psf_a->istep_sync = istep;
    evolution_fline_viewer();
    evolution_psf_viewer();
	return;
}


void draw_modified_object_distance(){
	cal_range_4_mesh_c(kemo_sgl->mesh_d, kemo_sgl->view_s);
	modify_object_multi_viewer_c(kemo_sgl->mesh_m->dist_domains, kemo_sgl->mesh_d);
	return;
}

void set_viewtype_glut(int sel){
    kemo_sgl->mesh_m->iflag_view_type = set_viewtype(kemo_sgl->view_s, sel, kemo_sgl->mesh_m->iflag_view_type);
}

void set_to_coastline_radius(double radius){kemo_sgl->mesh_m->radius_coast = radius;};
double send_coastline_radius(){return kemo_sgl->mesh_m->radius_coast;};

void set_object_property_flags(int selected, int iflag){
	if (selected == AXIS_TOGGLE) {set_axis_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {set_coastline_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {set_sphere_grid_flag(iflag, kemo_sgl->mesh_m);}
    else if(selected == SHADING_SWITCH) {set_shading_mode(iflag, kemo_sgl->mesh_m);}
    else if(selected == POLYGON_SWITCH) {set_polygon_mode(iflag, kemo_sgl->mesh_m);};
	return;
}

int send_object_property_flags(int selected){
	if (selected == AXIS_TOGGLE) {return kemo_sgl->mesh_m->iflag_draw_axis;}
    else if(selected == COASTLINE_SWITCH) {return kemo_sgl->mesh_m->iflag_draw_coast;}
    else if(selected == SPHEREGRID_SWITCH) {return kemo_sgl->mesh_m->iflag_draw_sph_grid;}
    else if(selected == SHADING_SWITCH) {return kemo_sgl->mesh_m->shading_mode;}
    else if(selected == POLYGON_SWITCH) {return kemo_sgl->mesh_m->polygon_mode;};
    
	return 0;
}

int object_properties_toggle(int selected){
	if (selected == AXIS_TOGGLE) {return toggle_draw_axis(kemo_sgl->mesh_m);}
    else if(selected == COASTLINE_SWITCH) {return toggle_coastline_flag(kemo_sgl->mesh_m);}
    else if(selected == SPHEREGRID_SWITCH) {return toggle_sphere_grid_flag(kemo_sgl->mesh_m);}
    else if(selected == SHADING_SWITCH) {return toggle_shading_mode(kemo_sgl->mesh_m);}
    else if(selected == POLYGON_SWITCH) {return toggle_polygon_mode(kemo_sgl->mesh_m);};
    
	return 0;
}

void set_to_mesh_color_mode(int icolor)  {kemo_sgl->mesh_m->mesh_color_mode = icolor;};
void set_to_num_of_color_loop(int icolor){kemo_sgl->mesh_m->num_of_color_loop = icolor;};

void set_to_node_diam(double diam)   {kemo_sgl->mesh_m->node_diam = diam;};
void set_to_dist_domains(double dist){kemo_sgl->mesh_m->dist_domains = dist;};


void set_domain_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->domain_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_domain_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_domain_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void set_node_grp_color_flag(int icolor)  {select_node_grp_node_color(icolor, kemo_sgl->mesh_m);};

void set_ele_grp_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->ele_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_ele_grp_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_ele_grp_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void set_surf_grp_color_flag(int selected, int icolor){
    if(selected == SURFSOLID_TOGGLE){kemo_sgl->mesh_m->surf_surface_color = icolor;}
    else if(selected == SURFGRID_TOGGLE){select_surf_grp_grid_color(icolor, kemo_sgl->mesh_m);}
    else if(selected == SURFNOD_TOGGLE){select_surf_grp_node_color(icolor, kemo_sgl->mesh_m);};
    return;
}

void set_domain_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_surface_color_code);
        set_to_domain_surface_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->domain_node_color_code);
    };
};

void set_to_node_grp_color_code(GLfloat color_code4[4]){
    copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->node_node_color_code);
    return;
}

void set_ele_grp_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_surface_color_code);
        set_to_ele_surface_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->ele_node_color_code);
    };
    return;
}

void set_surf_grp_color_code(int selected, GLfloat color_code4[4]){
    if(selected == SURFSOLID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_surface_color_code);
        set_to_surf_surface_opacity((double) color_code4[3]);
    } else if(selected == SURFGRID_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_grid_color_code);
    } else if(selected == SURFNOD_TOGGLE){
        copy_rgba_color_c(color_code4, kemo_sgl->mesh_m->surf_node_color_code);
    };
    return;
}


void set_to_domain_surface_opacity(double opacity_in){kemo_sgl->mesh_m->domain_opacity = opacity_in;};
void set_to_ele_surface_opacity(double opacity_in)   {kemo_sgl->mesh_m->ele_grp_opacity = opacity_in;};
void set_to_surf_surface_opacity(double opacity_in)  {kemo_sgl->mesh_m->surf_grp_opacity = opacity_in;};

double send_domain_surface_opacity(){return kemo_sgl->mesh_m->domain_opacity;};
double send_ele_surface_opacity(){return kemo_sgl->mesh_m->ele_grp_opacity;};
double send_surf_surface_opacity(){return kemo_sgl->mesh_m->surf_grp_opacity;};


int send_draw_surface_nod(){return kemo_sgl->mesh_m->draw_surface_nod;};
int send_draw_surface_grid() {return kemo_sgl->mesh_m->draw_surface_grid;};
int send_draw_surface_solid(){return kemo_sgl->mesh_m->draw_surface_solid;};

void set_kemoview_mesh_draw(int selected, int iflag){
	int num_pe = send_num_pe_sf();
	
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
	int num_pe = send_num_pe_sf();
	
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


void set_to_draw_domain_nod(int iflag, int i)  {
    kemo_sgl->mesh_m->draw_surface_nod = IONE;
    kemo_sgl->mesh_m->draw_domains_nod[i] = iflag;
}
void set_to_draw_domain_grid(int iflag, int i) {
    kemo_sgl->mesh_m->draw_surface_grid = IONE;
    kemo_sgl->mesh_m->draw_domains_grid[i] = iflag;
}
void set_to_draw_domain_solid(int iflag, int i){
    kemo_sgl->mesh_m->draw_surface_solid = IONE;
    kemo_sgl->mesh_m->draw_domains_solid[i] = iflag;
}


void set_to_draw_nodgrp_nod(int iflag, int i){kemo_sgl->mesh_m->draw_nodgrp_nod[i] = iflag;};
int send_draw_nodgrp_nod(int i){return kemo_sgl->mesh_m->draw_nodgrp_nod[i];};


void set_to_draw_elegrp_nod(int iflag, int i)  {kemo_sgl->mesh_m->draw_elegrp_nod[i] = iflag;};
int send_draw_elegrp_nod(int i)  {return kemo_sgl->mesh_m->draw_elegrp_nod[i];};

void set_to_draw_elegrp_grid(int iflag, int i) {kemo_sgl->mesh_m->draw_elegrp_grid[i] = iflag;};
int send_draw_elegrp_grid(int i) {return kemo_sgl->mesh_m->draw_elegrp_grid[i];};

void set_to_draw_elegrp_solid(int iflag, int i){kemo_sgl->mesh_m->draw_elegrp_solid[i] = iflag;};
int send_draw_elegrp_solid(int i){return kemo_sgl->mesh_m->draw_elegrp_solid[i];};


void set_to_draw_surfgrp_nod(int iflag, int i)  {kemo_sgl->mesh_m->draw_surfgrp_nod[i] = iflag;};
int send_draw_surfgrp_nod(int i)  {return kemo_sgl->mesh_m->draw_surfgrp_nod[i];};

void set_to_draw_surfgrp_grid(int iflag, int i) {kemo_sgl->mesh_m->draw_surfgrp_grid[i] = iflag;};
int send_draw_surfgrp_grid(int i) {return kemo_sgl->mesh_m->draw_surfgrp_grid[i];};

void set_to_draw_surfgrp_solid(int iflag, int i){kemo_sgl->mesh_m->draw_surfgrp_solid[i] = iflag;};
int send_draw_surfgrp_solid(int i){return kemo_sgl->mesh_m->draw_surfgrp_solid[i];};

void kemoview_nod_grp_toggle(int selected){
	int ngrp = send_ngrp_nod_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_nodgrp_nod);
	return;
}


void kemoview_ele_grp_toggle(int selected){
	int ngrp = send_ngrp_ele_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_elegrp_solid);
	return;
}

void kemoview_ele_grp_nod_toggle(int selected){
	int ngrp = send_ngrp_ele_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_elegrp_nod);
	return;
}

void kemoview_ele_grp_grid_toggle(int selected){
	int ngrp = send_ngrp_ele_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_elegrp_grid);
	return;
}


void kemoview_surf_grp_toggle(int selected){
	int ngrp = send_ngrp_surf_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_surfgrp_solid);
	return;
}

void kemoview_surf_grp_nod_toggle(int selected){
	int ngrp = send_ngrp_surf_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_surfgrp_nod);
	return;
}

void kemoview_surf_grp_grid_toggle(int selected){
	int ngrp = send_ngrp_surf_sf();
	select_draw_flag_toggle(selected, ngrp, kemo_sgl->mesh_m->draw_surfgrp_grid);
	return;
}

int send_iflag_draw_mesh(){return kemo_sgl->mesh_m->iflag_draw_mesh;};

int send_num_pe_sf()    {return kemo_sgl->mesh_d->num_pe_sf;};
int send_ngrp_nod_sf() {return kemo_sgl->mesh_d->ngrp_nod_sf;};
int send_ngrp_ele_sf() {return kemo_sgl->mesh_d->ngrp_ele_sf;};
int send_ngrp_surf_sf(){return kemo_sgl->mesh_d->ngrp_surf_sf;};


void send_nod_gp_name_sf(char *name, int i){ strngcopy(name, kemo_sgl->mesh_d->nod_gp_name_sf[i]); };
void send_ele_gp_name_sf(char *name, int i){ strngcopy(name, kemo_sgl->mesh_d->ele_gp_name_sf[i]); };
void send_surf_gp_name_sf(char *name, int i){ strngcopy(name, kemo_sgl->mesh_d->surf_gp_name_sf[i]); };

int send_iflag_draw_type(){return kemo_sgl->mesh_m->iflag_draw_type;};
int send_iflag_view_type(){return kemo_sgl->mesh_m->iflag_view_type;};

int send_num_of_color_loop(){return kemo_sgl->mesh_m->num_of_color_loop;};

double send_node_diam()   {return kemo_sgl->mesh_m->node_diam;};
double send_dist_domains(){return kemo_sgl->mesh_m->dist_domains;};


void get_ext_from_file_name(const char *file_head, char *stripped_fhead, char *stripped_ext){
	get_ext_from_file_name_c(file_head, stripped_fhead, stripped_ext);
}
void add_ext_to_file_name(const char *file_head, const char *added_ext, char *file_name){
	add_ext_to_file_name_c(file_head, added_ext, file_name);
}

void set_to_text_color_code(GLfloat c_code[4]){copy_rgba_color_c(c_code, kemo_sgl->mesh_m->text_color);};
void send_text_color_code(float c_code[4])    {copy_rgba_color_c(kemo_sgl->mesh_m->text_color, c_code);};




void get_kemoviewer_fliped_img(int npixel_x, int npixel_y,
                               unsigned char *glimage, unsigned char *fliped_img){
    get_gl_buffer_to_bmp(npixel_x, npixel_y, glimage);
    flip_gl_bitmap(npixel_x, npixel_y, glimage, fliped_img);
    return;
}

void write_kemoviewer_window_to_vector_file(int iflag_img, const char *fhead){
	sel_gl_buffer_2_vector_img(iflag_img, fhead);
}

void set_texture_rgba_to_current_psf(int width, int height, const unsigned char *bgra_in){
    set_texture_psf_from_bgra(kemo_sgl->psf_current_menu, width, height, bgra_in);
};

void modify_view_kemoview(){modify_stereo_kemoview(kemo_sgl->mesh_m, kemo_sgl->view_s);};
void rotate_kemoview(){rotate_stereo_kemoview(kemo_sgl->mesh_m, kemo_sgl->view_s);};

void reset_kemoviewer_to_init_angle(){
    reset_all_view_parameter(kemo_sgl->view_s);
    init_rot_animation(kemo_sgl->view_s);
};


void set_kemoview_retinamode(int i_retina){
    set_gl_retinamode(kemo_sgl->view_s, i_retina);
}

void set_kemoview_windowsize(GLint npixel_x, GLint npixel_y){
	set_gl_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
};

void update_projection_by_kemoviewer_size(GLint npixel_x, GLint npixel_y){
	update_projection_by_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
};


void update_kemoviewer_distance(){
	update_projection_struct(kemo_sgl->view_s);
};

void set_kemoview_rotation_parameter(GLdouble rot_vect[4]){
	set_gl_rotation_parameter(kemo_sgl->view_s, rot_vect);
};
void set_kemoview_dragging_rotation(GLdouble rot_vect[4]){
	set_gl_dragging_rotation(kemo_sgl->view_s, rot_vect);
};
void set_kemoview_animation_rot_axis(int iaxis){
	set_gl_animation_rot_axis(kemo_sgl->view_s, iaxis);
}
void set_kemoview_animation_rot_angle(int int_degree){
	set_gl_animation_rot_angle(kemo_sgl->view_s, int_degree);
}
void set_kemoview_shift_vector(GLdouble position[3]){
	set_gl_shift_vector(kemo_sgl->view_s, position);
};
void set_kemoview_scale_factor(GLdouble scale_s){
	set_gl_scalar_scale_factor(kemo_sgl->view_s, scale_s);
};
void set_kemoview_projection_aperture(GLdouble aperture_s){
	set_gl_projection_aperture(kemo_sgl->view_s, aperture_s);
};
void set_kemoview_stereo_parameter(GLdouble focus, GLdouble eye_sep){
	set_gl_stereo_parameter(kemo_sgl->view_s, focus, eye_sep);
};


void send_kemoview_windowsize(GLint *npixel_x, GLint *npixel_y){
	send_gl_windowsize(kemo_sgl->view_s, npixel_x, npixel_y);
}
void send_kemoview_rotation_parameter(GLdouble rot_vect[4]){
	send_gl_rotation_parameter(kemo_sgl->view_s, rot_vect);
}

void send_kemoview_shift_vector(GLdouble position[3]){
	send_gl_shift_vector(kemo_sgl->view_s, position);
}

void send_kemoview_lookat_vector(GLdouble position[3]){
	send_gl_lookat_vector(kemo_sgl->view_s, position);
}

GLdouble send_kemoview_scale_factor(){
    return send_scalar_scale_factor(kemo_sgl->view_s);
}

GLdouble send_kemoview_projection_aperture(){
	return send_gl_projection_aperture(kemo_sgl->view_s);
}
void send_kemoview_projection_parameters(GLdouble *aperture_s, GLdouble *near_s,
										 GLdouble *far_s, GLdouble *aspect_s){
	send_gl_projection_parameters(kemo_sgl->view_s, aperture_s, near_s, far_s, aspect_s);
}

GLdouble send_kemoview_stereo_parameters(){return send_gl_stereo_parameters(kemo_sgl->view_s);};
GLdouble send_kemoview_stereo_eyeseparation(){return send_gl_stereo_eyeseparation(kemo_sgl->view_s);};

void kemoviewer_mousedolly(GLdouble start[2], GLdouble x_dolly, GLdouble y_dolly){
	gl_mousedolly_struct(kemo_sgl->view_s, start, x_dolly, y_dolly);
}
void kemoviewer_mousepan(GLdouble start[2], GLdouble x_pan, GLdouble y_pan){
	gl_mousepan_struct(kemo_sgl->view_s, start, x_pan, y_pan);
}
void kemoviewer_zooming(GLdouble wheelDelta){
	gl_zooming_struct(kemo_sgl->view_s, wheelDelta);
}

/* called with the start position and the window origin + size */
void kemoview_startTrackball(GLdouble x, GLdouble y){gl_startTrackball(x, y, kemo_sgl->view_s);};
/* calculated rotation based on current mouse position */
void kemoview_rollToTrackball(GLdouble x, GLdouble y){ gl_rollToTrackball (x, y, kemo_sgl->view_s);};
/* add a GL rotation (dA) to an existing GL rotation (A) */
void drugging_addToRotationTrackball(){ gl_drag_addToRotationTrackball(kemo_sgl->view_s);}

void add_kemoview_animation_rot(GLdouble dt){add_animation_rotation(kemo_sgl->view_s, dt);}
void reset_kemoviewer_animation(){reset_rot_animation(kemo_sgl->view_s);};


void set_to_stereo_shutter(int iflag){kemo_sgl->mesh_m->iflag_streo_stutter = iflag;}
void set_to_iflag_anaglyph(int iflag){kemo_sgl->mesh_m->iflag_streo_anaglyph = iflag;}
int send_stereo_shutter(){return kemo_sgl->mesh_m->iflag_streo_stutter;}
int send_iflag_anaglyph(){return kemo_sgl->mesh_m->iflag_streo_anaglyph;}

void draw_menubottun_glut(){draw_menubottun_gl();}

/* Subroutines for surface rendering */
void set_num_loaded_PSF(int num){kemo_sgl->psf_a->num_loaded = num;};
void set_max_loaded_PSF(int num){kemo_sgl->psf_a->nmax_loaded = num;};
void set_to_loaded_PSF_flag(int id_psf, int iflag){kemo_sgl->psf_a->iflag_loaded[id_psf] = iflag;};
void set_to_current_PSF(int id_psf){
    kemo_sgl->psf_a->id_current = id_psf;
    kemo_sgl->psf_current_data = kemo_sgl->psf_d[kemo_sgl->psf_a->id_current];
    kemo_sgl->psf_current_menu = kemo_sgl->psf_m[kemo_sgl->psf_a->id_current];
}

int send_num_loaded_PSF(){return kemo_sgl->psf_a->num_loaded;};
int send_max_loaded_PSF(){return kemo_sgl->psf_a->nmax_loaded;};
int send_loaded_PSF_flag(int id_psf){return kemo_sgl->psf_a->iflag_loaded[id_psf];};
int send_current_PSF(){return kemo_sgl->psf_a->id_current;};


int send_current_psf_full_path_header(char *file_head, int *iflag){
    return send_each_psf_file_header_full(kemo_sgl->psf_current_menu, file_head, iflag);
}

void send_current_psf_file_header(char *file_head){
	int istep;
	istep = send_each_psf_file_header(kemo_sgl->psf_current_menu, file_head);
	return;
}

void set_current_psf_field_flag(int sel){
	set_PSF_field(sel, kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};
void set_current_psf_component_flag(int sel){
	set_PSF_component(sel, kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};

int send_nfield_current_psf()          {return send_nfield_each_psf(kemo_sgl->psf_current_data);};
int send_ncomptot_current_psf()        {return send_ncomptot_each_psf(kemo_sgl->psf_current_data);};
int send_ncomp_current_psf(int i)      {return send_ncomp_each_psf(kemo_sgl->psf_current_data, i);};
int send_istack_comp_current_psf(int i){return send_istack_each_comp_psf(kemo_sgl->psf_current_data, i);};
void send_current_psf_data_name(char *name, int i){
    send_each_psf_data_name(kemo_sgl->psf_current_data, name, i);
};

int send_iflag_draw_current_psf(){return kemo_sgl->psf_a->iflag_loaded[kemo_sgl->psf_a->id_current];};

int send_draw_field_current_psf()    {return send_field_draw_each_psf(kemo_sgl->psf_current_menu);};
int send_draw_comp_id_current_psf()  {return send_draw_comp_id_psf(kemo_sgl->psf_current_menu);};
int send_draw_component_current_psf(){return send_draw_component_psf(kemo_sgl->psf_current_menu);};
int send_coordinate_id_current_psf(){
    return send_coordinate_id_psf(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu);
};



void set_current_psf_polygon_mode(int iflag){set_psf_polygon_mode(kemo_sgl->psf_current_menu, iflag);};
void set_current_psf_tanvec_mode(int iflag){set_psf_vector_mode(kemo_sgl->psf_current_menu, iflag);};

int send_draw_current_psf_refv()  {return send_draw_psf_refv(kemo_sgl->psf_current_menu);};
int toggle_draw_current_psf_refv(){return toggle_draw_psf_refv(kemo_sgl->psf_current_menu);};

void set_current_psf_patch_color_mode(int iflag){set_psf_patch_color_mode(kemo_sgl->psf_current_menu, iflag);};

void set_current_isoline_color(int iflag)     {set_each_isoline_color(kemo_sgl->psf_current_menu, iflag);};
void set_current_n_isoline(int nlline)        {set_each_n_isoline(kemo_sgl->psf_current_menu, nlline);};
void set_current_increment_vect(int increment){set_each_increment_vect(kemo_sgl->psf_current_menu, increment);};
void set_current_scale_vect(double scale)     {set_each_scale_vect(kemo_sgl->psf_current_menu, scale);};
void set_current_vector_thick(double size)    {set_each_vector_thick(kemo_sgl->psf_current_menu, size);};

int send_current_psf_patch_color()   {return send_each_psf_patch_color(kemo_sgl->psf_current_menu);};
int send_current_isoline_color()     {return send_each_isoline_color(kemo_sgl->psf_current_menu);};
int send_current_num_isoline()       {return send_num_isoline(kemo_sgl->psf_current_menu);};
int send_current_vector_patch_color(){return send_each_vector_patch_color(kemo_sgl->psf_current_menu);};
int send_current_increment_vect()    {return send_increment_vector(kemo_sgl->psf_current_menu);};
double send_current_scale_vect()     {return send_scale_vector(kemo_sgl->psf_current_menu);};
double send_current_vector_thick()   {return send_vector_thick(kemo_sgl->psf_current_menu);};


int send_kemoview_psf_draw_flags(int selected){
	if      (selected == PSFSOLID_TOGGLE)    {return send_draw_psf_solid(kemo_sgl->psf_current_menu);}
	else if (selected == PSFGRID_TOGGLE)     {return send_draw_psf_grid(kemo_sgl->psf_current_menu);}
	else if (selected == ZEROGRID_TOGGLE)    {return send_draw_psf_zero(kemo_sgl->psf_current_menu);}
	else if (selected == COLORBAR_TOGGLE)    {return send_draw_psf_cbar(kemo_sgl->psf_current_menu);}
	else if (selected == PSF_POLYGON_SWITCH) {return send_each_psf_polygon_mode(kemo_sgl->psf_current_menu);}
	else if (selected == PSFVECT_TOGGLE)     {return send_draw_psf_vect(kemo_sgl->psf_current_menu);}
    else if (selected == PSFTANVEC_TOGGLE)   {return send_each_psf_vector_mode(kemo_sgl->psf_current_menu);};
	return 0;
}

int kemoview_psf_draw_switch_select(int selected){
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

void set_current_PSF_color_mode_id(int isel){set_PSF_color_mode_id(kemo_sgl->psf_current_menu, isel);}

double send_current_psf_data_min(int i){return send_psf_data_min(kemo_sgl->psf_current_data, i);};
double send_current_psf_data_max(int i){return send_psf_data_max(kemo_sgl->psf_current_data, i);};



void delete_current_PSF_color_idx_list(int i_delete){
    delete_PSF_color_index_list(kemo_sgl->psf_current_menu, i_delete);
}
void delete_current_PSF_opacity_idx_list(int i_delete){
    delete_PSF_opacity_index_list(kemo_sgl->psf_current_menu, i_delete);
}

void add_current_PSF_color_idx_list(double add_value, double add_color){
    add_PSF_color_index_list(kemo_sgl->psf_current_menu, add_value, add_color);
}
void add_current_PSF_opacity_idx_list(double add_value, double add_opacity){
    add_PSF_opacity_index_list(kemo_sgl->psf_current_menu, add_value, add_opacity);
}

void set_current_PSF_linear_colormap(double minvalue, double maxvalue){
    set_PSF_linear_colormap(kemo_sgl->psf_current_menu, minvalue, maxvalue);
}

void set_current_PSF_fixed_color(double *rgba){
    set_PSF_fixed_color(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu, rgba);
}

void set_current_PSF_constant_opacity(double opacity){
    set_PSF_constant_opacity(kemo_sgl->psf_current_data, kemo_sgl->psf_current_menu, opacity);
}

void set_current_PSF_rgb_from_value(double value, double *red, double *green, double *blue){
    set_PSF_rgb_from_value(kemo_sgl->psf_current_menu, value, red, green, blue);
}
void set_current_PSF_opacity_from_value(double value, double *opacity){
    set_PSF_opacity_from_value(kemo_sgl->psf_current_menu, value, opacity);
}
void set_current_PSF_color_point(int i_point, double value, double color){
    set_each_PSF_color_point(kemo_sgl->psf_current_menu, i_point, value, color);
}
void set_current_PSF_opacity_point(int i_point, double value, double opacity){
    set_each_PSF_opacity_point(kemo_sgl->psf_current_menu, i_point, value, opacity);
}

double send_current_PSF_color_table_min(){return send_each_PSF_color_table_min(kemo_sgl->psf_current_menu);};
double send_current_PSF_color_table_max(){return send_each_PSF_color_table_max(kemo_sgl->psf_current_menu);};
double send_current_PSF_minimum_opacity(){return send_each_PSF_minimum_opacity(kemo_sgl->psf_current_menu);};
double send_current_PSF_maximum_opacity(){return send_each_PSF_maximum_opacity(kemo_sgl->psf_current_menu);};
int send_current_PSF_color_table_num(){return send_each_PSF_color_table_num(kemo_sgl->psf_current_menu);};
int send_current_PSF_opacity_table_num(){return send_each_PSF_opacity_table_num(kemo_sgl->psf_current_menu);};

void send_current_PSF_color_table_items(int i_point, double *value, double *color){
    send_each_PSF_color_table_items(kemo_sgl->psf_current_menu, i_point, value, color);
}
void send_current_PSF_opacity_table_items(int i_point, double *value, double *opacity){
    send_each_PSF_opacity_table_items(kemo_sgl->psf_current_menu, i_point, value, opacity);
}

void write_current_PSF_colormap_control_file(const char *file_name){
    write_each_PSF_colormap_control_file(kemo_sgl->psf_current_menu, file_name);
}
void check_current_PSF_colormap_control(){
    check_each_PSF_colormap_control(kemo_sgl->psf_current_menu);
}


/* Subroutines for field lines */

int send_fline_file_header(char *file_head){
	strngcopy(file_head, kemo_sgl->fline_m->fline_header);
	return kemo_sgl->fline_m->fline_step;
};
void set_to_fline_file_step(int istep){kemo_sgl->fline_m->fline_step = istep;};

void set_fline_color_field_flag(int sel){set_fline_color_field(sel, kemo_sgl->fline_d, kemo_sgl->fline_m);};
void set_fline_color_comp_flag(int sel){set_fline_color_component(sel, kemo_sgl->fline_d, kemo_sgl->fline_m);};

int send_nfield_fline(){return kemo_sgl->fline_d->nfield;};
int send_ncomptot_fline(){return kemo_sgl->fline_d->ncomptot;};
int send_ncomp_fline(int i){return kemo_sgl->fline_d->ncomp[i];};
int send_istack_comp_fline(int i){return kemo_sgl->fline_d->istack_comp[i];};
void send_fline_data_name(char *name, int i){ strngcopy(name, kemo_sgl->fline_d->data_name[i]); };


void set_to_draw_fline(int iflag) {kemo_sgl->fline_m->iflag_draw_fline = iflag;};
void set_to_if_draw_fline(int ifield){kemo_sgl->fline_m->if_draw_fline = ifield;};
void set_to_ic_draw_fline(int icomp) {kemo_sgl->fline_m->ic_draw_fline = icomp;};
void set_to_fieldline_color(int iflag) {kemo_sgl->fline_m->fieldline_color = iflag;};

int send_iflag_draw_fline(){return kemo_sgl->fline_m->iflag_draw_fline;};
int send_if_draw_fline(){return kemo_sgl->fline_m->if_draw_fline;};
int send_ic_draw_fline(){return kemo_sgl->fline_m->ic_draw_fline;};
int send_icomp_draw_fline(){return kemo_sgl->fline_m->icomp_draw_fline;};
int send_fieldline_color() {return kemo_sgl->fline_m->fieldline_color;};


void set_to_fline_type_flag(int iflag) {kemo_sgl->fline_m->fieldline_type = iflag;};
int send_fline_type_flag() {return kemo_sgl->fline_m->fieldline_type;};
int toggle_fline_type_flag(){
	return kemo_sgl->fline_m->fieldline_type = toggle_value_c(kemo_sgl->fline_m->fieldline_type);
};


void set_to_fline_thickness(double thick) {kemo_sgl->fline_m->fieldline_thick = thick;};
double send_fline_thickness() {return kemo_sgl->fline_m->fieldline_thick;};

double send_fline_data_min(int i){return kemo_sgl->fline_d->d_min[i];};
double send_fline_data_max(int i){return kemo_sgl->fline_d->d_max[i];};


void input_fline_linear_colormap(double minvalue, double maxvalue){
	set_linear_colormap(kemo_sgl->fline_m->cmap_fline, minvalue, maxvalue);
}
void set_fline_constant_opacitymap(double opacity){
	set_constant_opacitymap(kemo_sgl->fline_m->cmap_fline,
                            kemo_sgl->fline_d->d_min[kemo_sgl->fline_m->icomp_draw_fline],
                            kemo_sgl->fline_d->d_max[kemo_sgl->fline_m->icomp_draw_fline], opacity);
}

void realloc_fline_color_index_list(int id_cmode, int num){
	realloc_color_index_list_s(kemo_sgl->fline_m->cmap_fline, id_cmode, num);
}
void realloc_flie_opacity_index_list(int num){
	realloc_opacity_index_list_s(kemo_sgl->fline_m->cmap_fline, num);
}

void set_fline_rgb_from_value(double value, double *red, double *green, double *blue){
	set_rgb_from_value_s(kemo_sgl->fline_m->cmap_fline, value, red, green, blue);
}

void set_fline_opacity_from_value(double value, double *opacity){
	set_opacity_from_value_s(kemo_sgl->fline_m->cmap_fline, value, opacity);
}
void set_each_fline_color_point(int i_point, double value, double color){
	set_each_color_point_s(kemo_sgl->fline_m->cmap_fline, i_point, value, color);
}
void set_each_fline_opacity_point(int i_point, double value, double opacity){
	set_each_opacity_point_s(kemo_sgl->fline_m->cmap_fline, i_point, value, opacity);
}

void set_fline_color_mode_id(int isel){
	set_color_mode_id_s(kemo_sgl->fline_m->cmap_fline, isel);
}

double send_fline_color_table_min(){
	double d, c;
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, 0, &d, &c);
	return d;
}
double send_fline_color_table_max(){
	double d, c;
	int n = send_color_table_num_s(kemo_sgl->fline_m->cmap_fline);
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, n-1, &d, &c);
	return d;
}
double send_fline_minimum_opacity(){return send_minimum_opacity_s(kemo_sgl->fline_m->cmap_fline);};
double send_fline_maximum_opacity(){return send_maximum_opacity_s(kemo_sgl->fline_m->cmap_fline);};

int send_fline_color_table_num()  {return send_color_table_num_s(kemo_sgl->fline_m->cmap_fline);;}
int send_fline_opacity_table_num(){return send_opacity_table_num_s(kemo_sgl->fline_m->cmap_fline);;}

void send_fline_color_table_items(int i_point, double *value, double *color){
	send_color_table_items_s(kemo_sgl->fline_m->cmap_fline, i_point, value, color);
}

void send_fline_opacity_table_items(int i_point, double *value, double *opacity){
	send_opacity_table_items_s(kemo_sgl->fline_m->cmap_fline, i_point, value, opacity);
}

void write_fline_colormap_control_file(const char *file_name){
	write_colormap_control_file_s(file_name, kemo_sgl->fline_m->cmap_fline);
}

double round_to_3digit(double value){return round_2_3digit(value);};

/*  Routines using libpng */
#ifdef PNG_OUTPUT
int set_image_file_format_id(char *image_fmt){return set_image_format_id_by_ext(image_fmt);}
void write_kemoviewer_window_to_file(int iflag_img, const char *fhead){
    write_gl_window_to_file(iflag_img, fhead, kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}
void write_kemoviewer_window_step_file(int iflag_img, int istep, const char *fhead){
    write_gl_window_step_file(iflag_img, istep, fhead, kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}

void write_kemoviewer_window_to_png(const char *fhead){
    write_gl_window_to_png(fhead, kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}
void write_kemoviewer_window_step_png(int istep, const char *fhead){
    write_gl_window_step_png(istep, fhead, kemo_sgl->view_s->nx_window, kemo_sgl->view_s->ny_window);
}

void set_texture_file_to_current_psf(int img_fmt, const char *img_head){
    set_texture_from_file(img_fmt, img_head, kemo_sgl->psf_current_menu);
};
#endif

