
/* read_data_4_kemoviewer.c */

#include "read_data_4_kemoviewer.h"

/* subroutine for reading PSF data by UCD */

static void set_viewer_mesh(struct viewer_mesh *mesh_s){
	
	alloc_normal_surf_viewer_s(mesh_s);
	alloc_domain_center_s(mesh_s);
	alloc_mesh_draw_s(mesh_s);
	
	set_surface_mesh_size(mesh_s);
    center_of_mesh_triangles(mesh_s, mesh_s->surf_center_view);
	take_normal_surf_mesh_c(mesh_s);
    
    alloc_mesh_normals_s(mesh_s);
	set_normal_on_node_4_mesh(mesh_s);
    set_mesh_patch_group_id(mesh_s);
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
	
    view->iflag_draw_mode = FULL_DRAW;
    view->iflag_retina =    ON;
	view->iflag_view_type = VIEW_3D;
	view->shading_mode =    INIT_SHADING_MODE;
    
    view->iflag_light_check =    OFF;
    view->iflag_coastline_tube = ON;
    view->ncorner_tube = 6;
    view->width_tube =   0.003;
    
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

static long set_psf_data_by_UCD(struct map_interpolate *map_itp,
                                struct psf_data *psf_s, struct psf_normals *psf_n,
                                struct psf_data *ucd_tmp) {
    alloc_psf_norm_s(ucd_tmp, psf_n);
    cal_colat_and_longitude(0, ucd_tmp->nnod_viz, ucd_tmp->xyzw_viz, 
                            psf_n->rt_viz);
    dealloc_psf_norm_s(psf_n);

    long nadded_for_phi0 = set_viewer_mesh_with_mapping(map_itp, psf_s, ucd_tmp);
    set_viewer_data_with_mapping(map_itp, ucd_tmp, psf_s);

    alloc_psf_norm_s(psf_s, psf_n);
	take_normal_psf(nadded_for_phi0, psf_s, psf_n);
    take_minmax_psf(psf_s, psf_n);
    psf_n->psf_edge = init_all_edge_4_psf(psf_s->nnod_viz, psf_s->nele_viz,
                                          psf_s->nnod_4_ele_viz, psf_s->ie_viz,
                                          psf_s->xyzw_viz, psf_n->norm_nod);

/*
     check_psf_ave_rms_c(psf_s,psf_n);
     check_psf_min_max_c(psf_s);
*/
	return nadded_for_phi0;
}

static void set_fline_data_by_UCD(struct fline_data *fline_d,
                                  struct fline_directions *fline_dir,
                                  struct psf_data *ucd_tmp){
    set_viewer_fieldline_data(fline_d, ucd_tmp);
    
    alloc_fline_work_data(fline_d, fline_dir);
    take_length_fline(fline_d, fline_dir);
    
    alloc_fline_ave_data(fline_d);
	take_minmax_fline(fline_dir, fline_d);
    dealloc_fline_work_data(fline_dir);
	return;
};

void evolution_PSF_data(struct psf_data *psf_s,
                        struct psf_normals *psf_n,
                        struct psf_data *ucd_tmp,
                        struct psf_menu_val *psf_m){
	int iflag_datatype;
    double time = 0.0;
	if(psf_m->iformat_viz_file == IFLAG_SURF_UDT
	   || psf_m->iformat_viz_file == IFLAG_SURF_UDT_GZ
	   || psf_m->iformat_viz_file == IFLAG_SURF_VTD
	   || psf_m->iformat_viz_file == IFLAG_SURF_VTD_GZ
	   || psf_m->iformat_viz_file == IFLAG_SURF_SDT
	   || psf_m->iformat_viz_file == IFLAG_SURF_SDT_GZ){
		check_gzip_psf_num_nod_first(psf_m->iformat_viz_file,
                                     psf_m->viz_prefix_c->string, ucd_tmp);
		check_gzip_psf_udt_first(psf_m->iformat_viz_file, psf_m->viz_step_c, &time, 
                                 psf_m->viz_prefix_c->string, ucd_tmp);
        set_iflag_draw_time(time, psf_m);
        set_viewer_data_with_mapping(psf_m->map_itp, ucd_tmp, psf_s);
        take_minmax_psf(psf_s, psf_n);
	} else if(psf_m->iformat_viz_file == IFLAG_SURF_UCD
			  || psf_m->iformat_viz_file == IFLAG_SURF_UCD_GZ
			  || psf_m->iformat_viz_file == IFLAG_SURF_VTK
			  || psf_m->iformat_viz_file == IFLAG_SURF_VTK_GZ
			  || psf_m->iformat_viz_file == IFLAG_PSF_BIN
			  || psf_m->iformat_viz_file == IFLAG_PSF_BIN_GZ){
		iflag_datatype = check_gzip_kemoview_ucd_first(psf_m->iformat_viz_file, psf_m->viz_step_c, &time, 
                                                       psf_m->viz_prefix_c->string, ucd_tmp);
        set_iflag_draw_time(time, psf_m);
        
        dealloc_edge_data_4_psf(psf_s->nele_viz, psf_n->psf_edge);
        dealloc_psf_norm_s(psf_n);
        deallc_all_psf_data(psf_s);
        psf_m->map_itp = alloc_psf_cutting_4_map();
        psf_m->nadded_for_phi0 = set_psf_data_by_UCD(psf_m->map_itp,
                                                     psf_s, psf_n, ucd_tmp);
	}
    
    return;
}

int refresh_FLINE_data(struct psf_data *ucd_tmp,
                       struct fline_data *fline_d,
                       struct fline_directions *fline_dir,
                       struct psf_menu_val *fline_m){
	int iflag_datatype;
    double time;
	
	iflag_datatype = check_gzip_kemoview_ucd_first(fline_m->iformat_viz_file, 
                                                   fline_m->viz_step_c, &time, 
                                                   fline_m->viz_prefix_c->string,
                                                   ucd_tmp);
	if (iflag_datatype == IFLAG_SURFACES){
        dealloc_psf_color_data_c(ucd_tmp);
		dealloc_psf_data_s(ucd_tmp);
		dealloc_psf_mesh_c(ucd_tmp);
		return iflag_datatype;
	}
    
    dealloc_fline_direction_data(fline_dir);
	deallc_all_fline_data(fline_d);
	set_fline_data_by_UCD(fline_d, fline_dir, ucd_tmp);
	return 0;
}


void set_kemoview_mesh_data(struct viewer_mesh *mesh_s,
                            struct mesh_menu_val *mesh_m, struct view_element *view){
	mesh_m->iflag_draw_mesh = IONE;
	
	set_kemoviewer_mesh(mesh_s, mesh_m, view);
    
	reset_to_init_angle(view);
	return;
}

void set_kemoview_psf_data(struct psf_data *psf_s,
                           struct psf_normals *psf_n,
                           struct psf_data *ucd_tmp,
                           struct psf_menu_val *psf_m){
	int i;
    psf_m->map_itp = alloc_psf_cutting_4_map();
    psf_m->nadded_for_phi0 = set_psf_data_by_UCD(psf_m->map_itp,
                                                 psf_s, psf_n, ucd_tmp);
	
	alloc_draw_psf_flags(psf_s->nfield, psf_s->ncomptot, psf_m);
	
	psf_m->iflag_draw_viz =   IONE;
	psf_m->polygon_mode_psf = INIT_POLYGON_MODE;
	psf_m->ivect_tangential = INIT_TANGENTIAL_VECT;
    psf_m->vector_thick = INIT_VECTOR_WIDTH;
	
	for (i=0;i<psf_s->nfield;i++){
		set_linear_colormap(psf_m->cmap_viz_fld[i], psf_s->amp_min[i], psf_s->amp_max[i]);
		set_full_opacitymap(psf_m->cmap_viz_fld[i], psf_s->amp_min[i], psf_s->amp_max[i]);
	};
	
	for (i=0;i<psf_s->ncomptot;i++){
		set_linear_colormap(psf_m->cmap_viz_comp[i], psf_s->d_min[i], psf_s->d_max[i]);
		set_full_opacitymap(psf_m->cmap_viz_comp[i], psf_s->d_min[i], psf_s->d_max[i]);
	};
	
	return;
}

void set_kemoview_fline_data(struct psf_data *ucd_tmp,
                             struct fline_data *fline_d,
                             struct fline_directions *fline_dir,
                             struct psf_menu_val *fline_m){
	int i;
	
	set_fline_data_by_UCD(fline_d, fline_dir, ucd_tmp);
    alloc_draw_psf_flags(fline_d->nfield, fline_d->ncomptot,
                         fline_m);
	
	fline_m->iflag_draw_viz = IONE;
	
	for (i=0;i<fline_d->nfield;i++){
		set_linear_colormap(fline_m->cmap_viz_fld[i], fline_d->amp_min[i], fline_d->amp_max[i]);
		set_full_opacitymap(fline_m->cmap_viz_fld[i], fline_d->amp_min[i], fline_d->amp_max[i]);
	};
	
	for (i=0;i<fline_d->ncomptot;i++){
		set_linear_colormap(fline_m->cmap_viz_comp[i], fline_d->d_min[i], fline_d->d_max[i]);
		set_full_opacitymap(fline_m->cmap_viz_comp[i], fline_d->d_min[i], fline_d->d_max[i]);
	};
	
	return;
}

void alloc_set_ucd_file_name_by_psf(struct psf_menu_val *psf_m, struct kv_string *ucd_m){
	alloc_set_ucd_field_file_name(psf_m->iformat_viz_file, psf_m->viz_step_c, psf_m->viz_prefix_c->string, ucd_m);
	return;
}
void alloc_set_ucd_file_name_by_fline(struct psf_menu_val *fline_m, struct kv_string *ucd_m){
	alloc_set_ucd_field_file_name(fline_m->iformat_viz_file, fline_m->viz_step_c, 
                                  fline_m->viz_prefix_c->string, ucd_m);
	return;
}

