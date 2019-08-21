/*
//  m_kemoview_mesh.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#include "m_kemoview_mesh.h"

struct kemoview_mesh * init_kemoview_mesh(){
	struct kemoview_mesh *kemo_mesh
			= (struct kemoview_mesh *) malloc(sizeof(struct kemoview_mesh));
	if(kemo_mesh == NULL){
		printf("malloc error for kemoview_mesh\n");
		exit(0);
	}
	
	kemo_mesh->mesh_d =  (struct viewer_mesh *)     malloc(NMAX_PSF*sizeof(struct viewer_mesh));
	kemo_mesh->mesh_m =  (struct mesh_menu_val *) malloc(NMAX_PSF*sizeof(struct mesh_menu_val));
	return kemo_mesh;
};

void dealloc_kemoview_mesh(struct kemoview_mesh *kemo_mesh){
	free(kemo_mesh->mesh_d);
	free(kemo_mesh->mesh_m);
	free(kemo_mesh);
	return;
};

void reset_draw_mesh(struct kemoview_mesh *kemo_mesh){
	if (kemo_mesh->mesh_m->iflag_draw_mesh > 0) {
		dealloc_all_mesh_4_viewer_s(kemo_mesh->mesh_d);
		dealloc_draw_mesh_flags(kemo_mesh->mesh_m);
	};
    return;
};

void close_mesh_view(struct kemoview_mesh *kemo_mesh){
	kemo_mesh->mesh_m->iflag_draw_mesh = 0;
	dealloc_all_mesh_4_viewer_s(kemo_mesh->mesh_d);
	dealloc_draw_mesh_flags(kemo_mesh->mesh_m);
	return;
}

