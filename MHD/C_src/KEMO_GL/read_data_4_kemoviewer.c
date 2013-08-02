
/* read_data_4_kemoviewer.c */

#include "read_data_4_kemoviewer.h"

/* subroutine for reading PSF data by UCD */


static void set_kemoviewer_mesh(const char *file_head, struct viewer_mesh *mesh_s, 
								struct mesh_menu_val *mesh_m, struct view_element *view){
	
	check_gzip_viewer_mesh_first(file_head, mesh_s);
	
	set_viewer_mesh(mesh_s);
	
	alloc_draw_mesh_flags(mesh_s, mesh_m);
	modify_object_for_mesh(mesh_m->dist_domains, mesh_s, view);
	return;
}

void init_kemoviewer(int iflag_dmesh, struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m, struct view_element *view){
	const char *mesh_file_head = "in_surface";
	
    view->iflag_retina = IONE;
    view->iflag_write_ps = OFF;
    
	mesh_m->iflag_draw_mesh = iflag_dmesh;
	mesh_m->iflag_view_type = VIEW_3D;
	
	/* !  set icosahearon information */
	init_mapgrid_position();
	init_icosahedron_c();
	init_viewer_parameters(mesh_m);
	
	if (mesh_m->iflag_draw_mesh > 0) set_kemoviewer_mesh(mesh_file_head, mesh_s, mesh_m, view);
	return;
}

static void set_psf_data_by_UCD(struct psf_data *psf_s, struct psf_data *ucd_tmp) {
/*	set_viewer_ucd_data(psf_s, ucd_tmp);*/
	set_ucd_with_mapping(psf_s, ucd_tmp);

	take_normal_psf(psf_s);
	take_minmax_psf(psf_s);
	
	check_psf_ave_rms_c(psf_s);
	check_psf_min_max_c(psf_s);
	return;
}

static void set_fline_data_by_UCD(struct psf_data *fline_s, struct psf_data *ucd_tmp) {
	set_viewer_ucd_data(fline_s, ucd_tmp);

	take_length_fline(fline_s);
	take_minmax_fline(fline_s);
	
	check_psf_ave_rms_c(fline_s);
	check_psf_min_max_c(fline_s);
	return;
}

int evolution_PSF_data(struct psf_data *psf_s, struct psf_data *ucd_tmp, struct psf_menu_val *psf_m){
	int ierr;
	
	if(psf_m->iflag_psf_file == IFLAG_UDT){
		ierr = check_gzip_psf_grd_first(psf_m->psf_header, ucd_tmp);
		check_gzip_psf_udt_first(psf_m->psf_header, psf_m->psf_step, ucd_tmp);
	} else if(psf_m->iflag_psf_file == IFLAG_UCD){
		ierr = check_gzip_kemoview_ucd_first(psf_m->psf_header, psf_m->psf_step, ucd_tmp);
	}
    deallc_all_psf_data(psf_s);
    set_psf_data_by_UCD(psf_s, ucd_tmp);
	return ierr;
}

int refresh_FLINE_data(const char *file_head, int istep, 
					   struct psf_data *fline_s, struct psf_data *ucd_tmp) {
	int ierr;
	
	ierr = check_gzip_kemoview_ucd_first(file_head, istep, ucd_tmp);
	if (ierr!= IFLAG_LINE_UCD && ierr != IFLAG_LINE_UCD_GZ){
		dealloc_psf_data_s(ucd_tmp);
		dealloc_psf_grid_s(ucd_tmp);
		return ierr;
	}
		
	deallc_all_fline_data(fline_s);
	set_fline_data_by_UCD(fline_s, ucd_tmp);
	return 0;
}


void set_kemoview_mesh_data(struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m, struct view_element *view){
	mesh_m->iflag_draw_mesh = IONE;
	
	set_kemoviewer_mesh(mesh_m->mesh_header, mesh_s, mesh_m, view);

	reset_light_by_size_of_domain(view->scale_factor[0]);
	reset_to_init_angle(view);
	return;
}

void set_kemoview_psf_data(struct psf_data *psf_s,struct psf_data *ucd_tmp,
			struct mesh_menu_val *mesh_m, struct psf_menu_val *psf_m,
			struct view_element *view){
	int i;
	
	set_psf_data_by_UCD(psf_s, ucd_tmp);
	
	alloc_draw_psf_flags(psf_s, psf_m);
	
	if ( mesh_m->iflag_draw_mesh == IZERO ) {
		cal_range_4_psf_grid_c(psf_s, view);
		reset_light_by_size_of_domain(view->scale_factor[0]);
		reset_to_init_angle(view);
	};
	
	mesh_m->iflag_view_type = VIEW_3D;
	psf_m->draw_psf_solid =   IONE;
	psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
	psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;
	
	for (i=0;i<psf_s->nfield;i++){
		set_linear_colormap(psf_m->cmap_psf_fld[i], psf_s->amp_min[i], psf_s->amp_max[i]);
		set_full_opacitymap(psf_m->cmap_psf_fld[i], psf_s->amp_min[i], psf_s->amp_max[i]);
	};
	
	for (i=0;i<psf_s->ncomptot;i++){
		set_linear_colormap(psf_m->cmap_psf_comp[i], psf_s->d_min[i], psf_s->d_max[i]);
		set_full_opacitymap(psf_m->cmap_psf_comp[i], psf_s->d_min[i], psf_s->d_max[i]);
	};
	
	return;
}

void set_kemoview_fline_data(struct psf_data *fline_s, struct psf_data *ucd_tmp, 
							struct mesh_menu_val *mesh_m, struct fline_menu_val *fline_m,
							struct view_element *view, int num_loaded){
	int i;
	
	if (fline_m->iflag_draw_fline > 0){
		dealloc_draw_fline_flags(fline_s, fline_m);
		deallc_all_fline_data(fline_s);
	}

	set_fline_data_by_UCD(fline_s, ucd_tmp);
	alloc_draw_fline_flags(fline_s, fline_m);
	
	if ( (mesh_m->iflag_draw_mesh + num_loaded) == IZERO ) {
		cal_range_4_psf_grid_c(fline_s, view);
		reset_light_by_size_of_domain(view->scale_factor[0]);
		reset_to_init_angle(view);
	};
	
	mesh_m->iflag_view_type =   VIEW_3D;
	fline_m->iflag_draw_fline = IONE;
	
	for (i=0;i<fline_s->nfield;i++){
		set_linear_colormap(fline_m->cmap_fline_fld[i], fline_s->amp_min[i], fline_s->amp_max[i]);
		set_full_opacitymap(fline_m->cmap_fline_fld[i], fline_s->amp_min[i], fline_s->amp_max[i]);
	};
	
	for (i=0;i<fline_s->ncomptot;i++){
		set_linear_colormap(fline_m->cmap_fline_comp[i], fline_s->d_min[i], fline_s->d_max[i]);
		set_full_opacitymap(fline_m->cmap_fline_comp[i], fline_s->d_min[i], fline_s->d_max[i]);
	};
	
	return;
}
