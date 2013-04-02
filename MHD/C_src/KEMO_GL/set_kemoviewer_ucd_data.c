/*
 *  set_kemoviewer_ucd_data.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/02.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_kemoviewer_ucd_data.h"

void run_pick_surface_c(char *file_head, struct mesh_menu_val *mesh_m)
{
	char command[LENGTHBUF];
	strcpy(command,mesh_m->pick_surface_command);
	strcat(command, "    ");
	strcat(command, file_head);
	printf("pick surface command line: %s\n", command);
	system(command);
	return;
}

int set_kemoview_data_fmt_flag(const char *file_name, char *file_head){
	int ifile_type;
	char file_head2[LENGTHBUF];
	char file_ext[LENGTHBUF];
	
	get_ext_from_file_name(file_name, file_head, file_ext);
	
	if (		  (file_ext[0] == 'g'
				&& file_ext[1] == 'z')
			||	  (file_ext[0] == 'G'
				&& file_ext[1] == 'Z') ){
		get_ext_from_file_name(file_head, file_head2, file_ext);
		if(		  (file_ext[0] == 'k' 
				&& file_ext[1] == 's' 
				&& file_ext[2] == 'm')
			||	  (file_ext[0] == 'K'
				&& file_ext[1] == 'S'
				&& file_ext[2] == 'M') ){
			ifile_type = IFLAG_SURF_MESH_GZ;
		} else if( file_ext[0] == '0' && file_ext[1] == '\0'){
			ifile_type = IFLAG_FULL_MESH_GZ;
		} else if((file_ext[0] == 'u' 
				&& file_ext[1] == 'd' 
				&& file_ext[2] == 't')
			||	  (file_ext[0] == 'U'
				&& file_ext[1] == 'D'
				&& file_ext[2] == 'T') ){
			ifile_type = IFLAG_SURF_UDT_GZ;
		} else if((file_ext[0] == 'i' 
				&& file_ext[1] == 'n' 
				&& file_ext[2] == 'p')
			||	  (file_ext[0] == 'I'
				&& file_ext[1] == 'N'
				&& file_ext[2] == 'P') ){
			ifile_type = IFLAG_SURF_UCD_GZ;
		} else {
			ifile_type = 99;
		};
		strngcopy(file_head, file_head2);
		
	} else if(	  (file_ext[0] == 'k' 
				&& file_ext[1] == 's' 
				&& file_ext[2] == 'm')
			||	  (file_ext[0] == 'K'
				&& file_ext[1] == 'S'
				&& file_ext[2] == 'M') ){
			ifile_type = IFLAG_SURF_MESH;
	} else if(	   file_ext[0] == '0' && file_ext[1] == '\0'){
			ifile_type = IFLAG_FULL_MESH;
	} else if(	  (file_ext[0] == 'u' 
				&& file_ext[1] == 'd' 
				&& file_ext[2] == 't')
			||	  (file_ext[0] == 'U'
				&& file_ext[1] == 'D'
				&& file_ext[2] == 'T') ){
			ifile_type = IFLAG_SURF_UDT;
	} else if(	  (file_ext[0] == 'i' 
				&& file_ext[1] == 'n' 
				&& file_ext[2] == 'p')
			||	  (file_ext[0] == 'I'
				&& file_ext[1] == 'N'
				&& file_ext[2] == 'P') ){
			ifile_type = IFLAG_SURF_UCD;
	} else {
		ifile_type = 99;
	};
	return ifile_type;
}

void init_draw_mesh(struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m,
					struct view_element *view){
	if (mesh_m->iflag_draw_mesh > 0) {
		dealloc_all_mesh_4_viewer_s(mesh_d);
		dealloc_draw_mesh_flags(mesh_m);
	};
	set_kemoview_mesh_data(mesh_d, mesh_m, view);
	return;
}

static void init_draw_psf(struct mesh_menu_val *mesh_m, struct kemo_array_control *psf_a,
					   struct psf_data **psf_d, struct psf_menu_val **psf_m, 
					   struct psf_data *ucd_tmp, struct ucd_file_menu_val *ucd_m,
					   struct view_element *view){
	int id_load = add_new_kemoview_array(psf_a);
	
	strngcopy(psf_m[id_load]->psf_header, ucd_m->ucd_header);
	psf_m[id_load]->psf_step = ucd_m->ucd_step;
	
	if(psf_a->num_loaded == psf_a->nlimit_loaded){
		dealloc_draw_psf_flags(psf_d[id_load], psf_m[id_load]);
		deallc_all_psf_data(psf_d[id_load]);
	};
	
	set_kemoview_psf_data(psf_d[id_load], ucd_tmp, mesh_m, psf_m[id_load], view);
	return;
};

static void init_draw_fline(struct mesh_menu_val *mesh_m, struct kemo_array_control *psf_a,
					  struct psf_data *fline_d, struct fline_menu_val *fline_m, 
					  struct psf_data *ucd_tmp, struct ucd_file_menu_val *ucd_m,
					  struct view_element *view){

	strngcopy(fline_m->fline_header, ucd_m->ucd_header);
	fline_m->fline_step = ucd_m->ucd_step;
	
	set_kemoview_fline_data(fline_d, ucd_tmp, mesh_m, fline_m, view, psf_a->num_loaded);
	return;
};

int kemoview_open_data(const char *file_name, struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m, 
					  struct kemo_array_control *psf_a, struct psf_data **psf_d, struct psf_menu_val **psf_m,
					  struct psf_data *fline_d, struct fline_menu_val *fline_m, 
					  struct psf_data *ucd_tmp, struct ucd_file_menu_val *ucd_m,
					  struct view_element *view){
	int ierr;
	char file_head[LENGTHBUF];
	
	int ifile_type = set_kemoview_data_fmt_flag(file_name, file_head);
	printf("ifile_type %d\n", ifile_type);
	printf("file_name %s\n", file_name);
		
	if(ifile_type == IFLAG_SURF_MESH || ifile_type == IFLAG_SURF_MESH_GZ){
		strngcopy(mesh_m->mesh_header, file_head);
		init_draw_mesh(mesh_d, mesh_m, view);
		ierr = ifile_type;
	} else if(ifile_type == IFLAG_FULL_MESH_GZ){
		strngcopy(mesh_m->mesh_header, file_head);
		return ifile_type;
	} else if(ifile_type == IFLAG_FULL_MESH){
		strngcopy(mesh_m->mesh_header, file_head);
		run_pick_surface_c(file_head, mesh_m);
		init_draw_mesh(mesh_d, mesh_m, view);
		ierr = IFLAG_SURF_MESH;
	} else if(ifile_type == IFLAG_SURF_UDT || ifile_type == IFLAG_SURF_UDT_GZ){
		ucd_m->ucd_step = get_index_from_file_head(file_head, ucd_m->ucd_header);
		ierr = check_gzip_psf_grd_first(ucd_m->ucd_header, ucd_tmp);
		if(ierr == IFLAG_SURF_UDT || ierr == IFLAG_SURF_UDT_GZ){
			check_gzip_psf_udt_first(ucd_m->ucd_header, ucd_m->ucd_step, ucd_tmp);
			init_draw_psf(mesh_m, psf_a, psf_d, psf_m, ucd_tmp, ucd_m, view);
			psf_m[psf_a->id_current]->iflag_psf_file = IFLAG_UDT;
		} else{
			dealloc_psf_grid_s(ucd_tmp);
		};

	} else if(ifile_type == IFLAG_SURF_UCD || ifile_type == IFLAG_SURF_UCD_GZ
			  || ifile_type == IFLAG_LINE_UCD || ifile_type == IFLAG_LINE_UCD_GZ){
		ucd_m->ucd_step = get_index_from_file_head(file_head, ucd_m->ucd_header);
		ierr = check_gzip_kemoview_ucd_first(ucd_m->ucd_header, ucd_m->ucd_step, ucd_tmp);

		if(ierr == IFLAG_SURF_UCD || ierr == IFLAG_SURF_UCD_GZ){
			init_draw_psf(mesh_m, psf_a, psf_d, psf_m, ucd_tmp, ucd_m, view);
			psf_m[psf_a->id_current]->iflag_psf_file = IFLAG_UCD;
		} else if(ierr == IFLAG_LINE_UCD || ierr == IFLAG_LINE_UCD_GZ){
			init_draw_fline(mesh_m, psf_a, fline_d, fline_m, ucd_tmp, ucd_m, view);
		} else {
			dealloc_psf_data_s(ucd_tmp);
			dealloc_psf_grid_s(ucd_tmp);
		}
	} else {
		ierr = 0;
	};
	
	return ierr;
}

