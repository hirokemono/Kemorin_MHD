/*
 *  set_kemoviewer_ucd_data.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/02.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_kemoviewer_ucd_data.h"

static void run_pick_surface_c(struct mesh_menu_val *mesh_m){
	int length = strlen(mesh_m->pick_surface_command) 
                + strlen(mesh_m->mesh_file_name) + 10;
	char *file_head = alloc_string(strlen(mesh_m->mesh_file_name));
	char *file_tmp = alloc_string(strlen(mesh_m->mesh_file_name));
	char *file_ext = alloc_string(strlen(mesh_m->mesh_file_name));
	char *command = alloc_string(length);
	
	if (mesh_m->iformat_surface_mesh == IFLAG_FULL_MESH_GZ) {
        get_ext_from_file_name_c(mesh_m->mesh_file_name, file_head, file_ext);
    } else {
        strngcopy(file_head, mesh_m->mesh_file_name);
    };
    get_ext_from_file_name_c(file_head, file_tmp, file_ext);
    get_ext_from_file_name_c(file_tmp, file_head, file_ext);
    
	strcpy(command,mesh_m->pick_surface_command);
	strcat(command, "    ");
	strcat(command, file_head);
	printf("pick surface command line: %s\n", command);
	system(command);
	
    if (mesh_m->iformat_surface_mesh == IFLAG_FULL_MESH_GZ) {
        sprintf(mesh_m->mesh_file_name, "%s.ksm.gz",file_head);
        mesh_m->iformat_surface_mesh = IFLAG_SURF_MESH_GZ;
    } else {
        sprintf(mesh_m->mesh_file_name, "%s.ksm",file_head);
        mesh_m->iformat_surface_mesh = IFLAG_SURF_MESH;
    };
	
	free(command);
	free(file_ext);
	free(file_tmp);
	free(file_head);
	return;
}

int set_data_format_flag(const char *file_name, char *file_head, char *file_ext){
	int ifile_type;
	
	char *file_head2 = alloc_string(strlen(file_name));
	
	get_ext_from_file_name_c(file_name, file_head, file_ext);
	
	if((file_ext[0] == 'g' && file_ext[1] == 'z') 
       || (file_ext[0] == 'G' && file_ext[1] == 'Z') ){
        get_ext_from_file_name_c(file_head, file_head2, file_ext);

        if((file_ext[0] == 'k' && file_ext[1] == 's' && file_ext[2] == 'm')
           || (file_ext[0] == 'K' && file_ext[1] == 'S' && file_ext[2] == 'M') ){
            ifile_type = IFLAG_SURF_MESH_GZ;
        } else if((file_ext[0] == 'g' && file_ext[1] == 'f' && file_ext[2] == 'm')
                  || (file_ext[0] == 'G' && file_ext[1] == 'F' && file_ext[2] == 'M') ){
            ifile_type = IFLAG_FULL_MESH_GZ;
            get_ext_from_file_name_c(file_head2, file_head, file_ext);
            strngcopy(file_head2, file_head);
        } else if((file_ext[0] == 'u' && file_ext[1] == 'd' && file_ext[2] == 't')
                  || (file_ext[0] == 'U' && file_ext[1] == 'D' && file_ext[2] == 'T') ){
            ifile_type = IFLAG_SURF_UDT_GZ;
        } else if((file_ext[0] == 'i' && file_ext[1] == 'n' && file_ext[2] == 'p')
                  ||	  (file_ext[0] == 'I' && file_ext[1] == 'N' && file_ext[2] == 'P') ){
            ifile_type = IFLAG_SURF_UCD_GZ;
        } else if((file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'd')
                  ||	  (file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'D') ){
            ifile_type = IFLAG_SURF_VTD_GZ;
        } else if(	  (file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'k')
                  ||	  (file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'K') ){
            ifile_type = IFLAG_SURF_VTK_GZ;
        } else {
            ifile_type = 99;
        };

        strngcopy(file_head, file_head2);
            
        } else if(	  (file_ext[0] == 'k' && file_ext[1] == 's' && file_ext[2] == 'm')
                  ||	  (file_ext[0] == 'K' && file_ext[1] == 'S' && file_ext[2] == 'M') ){
            ifile_type = IFLAG_SURF_MESH;
        } else if(    (file_ext[0] == 'g' && file_ext[1] == 'f' && file_ext[2] == 'm')
                  ||	  (file_ext[0] == 'G' && file_ext[1] == 'F' && file_ext[2] == 'M') ){
            ifile_type = IFLAG_FULL_MESH;

            get_ext_from_file_name_c(file_head, file_head2, file_ext);
            strngcopy(file_head, file_head2);
        } else if(	  (file_ext[0] == 'u' && file_ext[1] == 'd' && file_ext[2] == 't')
                  ||	  (file_ext[0] == 'U' && file_ext[1] == 'D' && file_ext[2] == 'T') ){
            ifile_type = IFLAG_SURF_UDT;
        } else if(	  (file_ext[0] == 'i' && file_ext[1] == 'n'  && file_ext[2] == 'p')
                  ||	  (file_ext[0] == 'I' && file_ext[1] == 'N' && file_ext[2] == 'P') ){
            ifile_type = IFLAG_SURF_UCD;
        } else if(	  (file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'd')
                  ||	  (file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'D') ){
            ifile_type = IFLAG_SURF_VTD;
        } else if(	  (file_ext[0] == 'v' && file_ext[1] == 't' && file_ext[2] == 'k')
                  ||	  (file_ext[0] == 'V' && file_ext[1] == 'T' && file_ext[2] == 'K') ){
            ifile_type = IFLAG_SURF_VTK;
        } else {
            ifile_type = 99;
	};
	free(file_head2);
	return ifile_type;
}

static void init_draw_mesh(struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m,
                           struct view_element *view){
	if (mesh_m->iflag_draw_mesh > 0) {
		dealloc_all_mesh_4_viewer_s(mesh_d);
		dealloc_draw_mesh_flags(mesh_m);
	};
	set_kemoview_mesh_data(mesh_d, mesh_m, view);
	return;
}

static void init_draw_psf(struct mesh_menu_val *mesh_m, struct kemo_array_control *psf_a,
			struct psf_data **psf_d, struct psf_menu_val **psf_m, struct psf_data *ucd_tmp,
			int iflag_fileformat, int istep, const char *ucd_header){
    int id_load;
    id_load = add_new_kemoview_array(psf_a);
	strngcopy(psf_m[id_load]->psf_header, ucd_header);
	psf_m[id_load]->psf_step = istep;
	psf_m[id_load]->iflag_psf_file = iflag_fileformat;
	
	if(psf_a->num_loaded == psf_a->nlimit_loaded){
		dealloc_draw_psf_flags(psf_d[id_load], psf_m[id_load]);
		deallc_all_psf_data(psf_d[id_load]);
	};
	
	set_kemoview_psf_data(psf_d[id_load], ucd_tmp, mesh_m, psf_m[id_load]);
    return;
};

static void init_draw_fline(struct mesh_menu_val *mesh_m,
			struct psf_data *fline_d, struct fline_menu_val *fline_m, struct psf_data *ucd_tmp,
			int iformat_ucd_file, int istep, const char *ucd_header){
    alloc_fline_file_param(fline_m);
    alloc_copy_string(ucd_header, fline_m->fline_header);
	fline_m->fline_step = istep;
	fline_m->iformat_fline_file = iformat_ucd_file;
    
	set_kemoview_fline_data(fline_d, ucd_tmp, mesh_m, fline_m);
    return;
};

int kemoviewer_open_data(const char *file_name, struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m, 
                       struct kemo_array_control *psf_a, struct psf_data **psf_d, struct psf_menu_val **psf_m,
                       struct psf_data *fline_d, struct fline_menu_val *fline_m, 
                       struct psf_data *ucd_tmp, struct view_element *view){
	int iflag_datatype;
	int iflag_fileformat;
	int istep;
	
	int length;
	char *file_head_w_step;
	char *ucd_header;
    char *file_ext = alloc_string(strlen(file_name));
	
	length = strlen(file_name);
	file_head_w_step = (char *)calloc(length+1, sizeof(char));
	length = strlen(file_name);
	ucd_header = (char *)calloc(length+1, sizeof(char));
	
	iflag_fileformat = set_data_format_flag(file_name, file_head_w_step, file_ext);
	printf("iflag_fileformat %d\n", iflag_fileformat);
	printf("file_name %s\n", file_name);
    free(file_ext);
	if(   iflag_fileformat == IFLAG_SURF_MESH || iflag_fileformat == IFLAG_SURF_MESH_GZ
       || iflag_fileformat == IFLAG_FULL_MESH || iflag_fileformat == IFLAG_FULL_MESH_GZ){
        mesh_m->iformat_surface_mesh = iflag_fileformat;
		strngcopy(mesh_m->mesh_file_name, file_name);
        
        if(iflag_fileformat == IFLAG_FULL_MESH || iflag_fileformat == IFLAG_FULL_MESH_GZ){
            run_pick_surface_c(mesh_m);
        };
        
		init_draw_mesh(mesh_d, mesh_m, view);
		iflag_datatype = IFLAG_MESH;
        
    } else if(   iflag_fileformat == IFLAG_SURF_UDT || iflag_fileformat == IFLAG_SURF_UDT_GZ
              || iflag_fileformat == IFLAG_SURF_VTD || iflag_fileformat == IFLAG_SURF_VTD_GZ){
		istep = get_index_from_file_head(file_head_w_step, ucd_header);
		iflag_datatype = check_gzip_psf_grd_first(iflag_fileformat, ucd_header, ucd_tmp);
		if(iflag_datatype != 0){
			check_gzip_psf_udt_first(iflag_fileformat, istep, ucd_header, ucd_tmp);
			init_draw_psf(mesh_m, psf_a, psf_d, psf_m, ucd_tmp, 
						iflag_fileformat, istep, ucd_header);
		} else{
			dealloc_psf_mesh_c(ucd_tmp);
		};
        
	} else if( iflag_fileformat == IFLAG_SURF_UCD || iflag_fileformat == IFLAG_SURF_UCD_GZ
              || iflag_fileformat == IFLAG_SURF_VTK || iflag_fileformat == IFLAG_SURF_VTK_GZ){
		istep = get_index_from_file_head(file_head_w_step, ucd_header);
		
		iflag_datatype = check_gzip_kemoview_ucd_first(iflag_fileformat, istep, ucd_header, ucd_tmp);

        if(iflag_datatype == IFLAG_SURFACES){
			init_draw_psf(mesh_m, psf_a, psf_d, psf_m, ucd_tmp, 
						iflag_fileformat, istep, ucd_header);
		} else if(iflag_datatype == IFLAG_LINES){
			init_draw_fline(mesh_m, fline_d, fline_m, ucd_tmp, 
						iflag_fileformat, istep, ucd_header);
		} else {
			dealloc_psf_data_s(ucd_tmp);
			dealloc_psf_mesh_c(ucd_tmp);
		}
	} else {
		iflag_datatype = 0;
	};
    
    if ( mesh_m->iflag_draw_mesh == IZERO ) {
        cal_psf_viewer_range(psf_d, psf_a, fline_d, fline_m, view);
        reset_light_by_size_of_domain(view->r_max);
        reset_to_init_angle(view);
    };
	
	free(ucd_header);
	free(file_head_w_step);
	return iflag_datatype;
}
