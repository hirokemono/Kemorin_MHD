
/* read_data_4_kemoviewer.c */

#include "read_data_4_kemoviewer.h"

/* subroutine for reading PSF data by UCD */


static void set_viewer_mesh(struct viewer_mesh *mesh_s){
	
	alloc_normal_surf_viewer_s(mesh_s);
	alloc_domain_center_s(mesh_s);
	alloc_mesh_draw_s(mesh_s);
	
	set_surface_mesh_size(mesh_s);
	take_normal_surf_mesh_c(mesh_s);
	set_surface_normal_4_each_node(mesh_s);
	set_normal_on_node_4_mesh(mesh_s);
	return;
}

static void set_kemoviewer_mesh(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m, 
								struct view_element *view){
	int ierr;
    
    if (mesh_m->iformat_surface_mesh == IFLAG_SURF_MESH_GZ) {
        ierr = read_viewer_mesh_gz_c(mesh_m->mesh_file_name->string, mesh_s);
    } else {
        ierr = read_viewer_mesh(mesh_m->mesh_file_name->string, mesh_s);
    };
	
    if (ierr != 0) {
        printf("File %s is not found.\n", mesh_m->mesh_file_name->string);
        exit(1);
    }
	set_viewer_mesh(mesh_s);
	
	alloc_draw_mesh_flags(mesh_s, mesh_m);
	cal_range_4_mesh_c(mesh_s, view);
	modify_object_multi_viewer_c(mesh_m->dist_domains, mesh_s);
	return;
}

void init_kemoviewer(int iflag_dmesh, struct viewer_mesh *mesh_s, 
                     struct mesh_menu_val *mesh_m, struct view_element *view){
	
    view->iflag_retina = IONE;
	view->iflag_view_type = VIEW_3D;
	view->iflag_streo_stutter =  SHUTTER_OFF;
	view->iflag_streo_anaglyph = ANAGLYPH_ON;
	view->shading_mode =         INIT_SHADING_MODE;
    
    mesh_m->mesh_file_name = init_kvstring_by_string("in.ksm");
	
	mesh_m->iformat_surface_mesh = IFLAG_SURF_MESH;
	mesh_m->iflag_draw_mesh = iflag_dmesh;
	
	/* !  set icosahearon information */
	init_mapgrid_position();
	init_icosahedron_c();
	init_viewer_parameters(mesh_m);
	
	if (mesh_m->iflag_draw_mesh > 0) {set_kemoviewer_mesh(mesh_s, mesh_m, view);};
	return;
}

static void set_psf_data_by_UCD(struct psf_data *psf_s, struct psf_data *ucd_tmp) {
    /*	set_viewer_ucd_data(psf_s, ucd_tmp);*/
	set_ucd_with_mapping(psf_s, ucd_tmp);
    
	take_normal_psf(psf_s);
	take_minmax_psf(psf_s);
    /*
    psf_s->psf_edge = init_all_edge_4_psf(psf_s->nnod_viz, psf_s->nele_viz, 
                                          psf_s->nnod_4_ele_viz, psf_s->ie_viz,
                                          psf_s->xx_viz, psf_s->norm_nod);

     check_psf_ave_rms_c(psf_s);
     check_psf_min_max_c(psf_s);
     */
	return;
}

static void set_fline_data_by_UCD(struct psf_data *fline_s, struct psf_data *ucd_tmp) {
	set_viewer_ucd_data(fline_s, ucd_tmp);
    
	take_length_fline(fline_s);
	take_minmax_fline(fline_s);
	/*
     check_psf_ave_rms_c(fline_s);
     check_psf_min_max_c(fline_s);
     */
	return;
};

void evolution_PSF_data(struct psf_data *psf_s, struct psf_data *ucd_tmp, struct psf_menu_val *psf_m){
	int iflag_datatype;
    double time;
	
	if(psf_m->iflag_psf_file == IFLAG_SURF_UDT
	   || psf_m->iflag_psf_file == IFLAG_SURF_UDT_GZ
	   || psf_m->iflag_psf_file == IFLAG_SURF_VTD
	   || psf_m->iflag_psf_file == IFLAG_SURF_VTD_GZ
	   || psf_m->iflag_psf_file == IFLAG_SURF_SDT
	   || psf_m->iflag_psf_file == IFLAG_SURF_SDT_GZ){
		iflag_datatype = check_gzip_psf_grd_first(psf_m->iflag_psf_file, 
                                                  psf_m->psf_header->string, ucd_tmp);
		check_gzip_psf_udt_first(psf_m->iflag_psf_file, psf_m->psf_step, &time, 
                                 psf_m->psf_header->string, ucd_tmp);
	} else if(psf_m->iflag_psf_file == IFLAG_SURF_UCD
			  || psf_m->iflag_psf_file == IFLAG_SURF_UCD_GZ
			  || psf_m->iflag_psf_file == IFLAG_SURF_VTK
			  || psf_m->iflag_psf_file == IFLAG_SURF_VTK_GZ
			  || psf_m->iflag_psf_file == IFLAG_PSF_BIN
			  || psf_m->iflag_psf_file == IFLAG_PSF_BIN_GZ){
		iflag_datatype = check_gzip_kemoview_ucd_first(psf_m->iflag_psf_file, psf_m->psf_step, &time, 
                                                       psf_m->psf_header->string, ucd_tmp);
	}
    
    set_iflag_draw_time(time, psf_m);
    deallc_all_psf_data(psf_s);
    set_psf_data_by_UCD(psf_s, ucd_tmp);
    return;
}

int refresh_FLINE_data(struct psf_data *fline_s, struct psf_data *ucd_tmp, struct fline_menu_val *fline_m){
	int iflag_datatype;
    double time;
	
	iflag_datatype = check_gzip_kemoview_ucd_first(fline_m->iformat_fline_file, fline_m->fline_step, &time, 
                                                   fline_m->fline_header->string, ucd_tmp);
	if (iflag_datatype == IFLAG_SURFACES){
		dealloc_psf_data_s(ucd_tmp);
		dealloc_psf_mesh_c(ucd_tmp);
		return iflag_datatype;
	}
    
	deallc_all_fline_data(fline_s);
	set_fline_data_by_UCD(fline_s, ucd_tmp);
	return 0;
}


void set_kemoview_mesh_data(struct viewer_mesh *mesh_s,
                            struct mesh_menu_val *mesh_m, struct view_element *view){
	mesh_m->iflag_draw_mesh = IONE;
	
	set_kemoviewer_mesh(mesh_s, mesh_m, view);
    
	reset_to_init_angle(view);
	return;
}

void set_kemoview_psf_data(struct psf_data *psf_s,struct psf_data *ucd_tmp,
                           struct psf_menu_val *psf_m){
	int i;
	
	set_psf_data_by_UCD(psf_s, ucd_tmp);
	
	alloc_draw_psf_flags(psf_s, psf_m);
	
	psf_m->draw_psf_solid =   IONE;
	psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
	psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;
    psf_m->vector_thick = INIT_VECTOR_WIDTH;
	
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
                             struct fline_menu_val *fline_m){
	int i;
	
	set_fline_data_by_UCD(fline_s, ucd_tmp);
	alloc_draw_fline_flags(fline_s, fline_m);
	
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

void alloc_set_ucd_file_name_by_psf(struct psf_menu_val *psf_m, struct kv_string *ucd_m){
	alloc_set_ucd_field_file_name(psf_m->iflag_psf_file, psf_m->psf_step, psf_m->psf_header->string, ucd_m);
	return;
}
void alloc_set_ucd_file_name_by_fline(struct fline_menu_val *fline_m, struct kv_string *ucd_m){
	alloc_set_ucd_field_file_name(fline_m->iformat_fline_file, fline_m->fline_step, fline_m->fline_header->string, ucd_m);
	return;
}

