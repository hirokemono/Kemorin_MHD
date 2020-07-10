/*
//  m_kemoview_mesh.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#include "m_kemoview_mesh.h"

struct kemoview_mesh * init_kemoview_mesh(void){
	struct kemoview_mesh *kemo_mesh
			= (struct kemoview_mesh *) malloc(sizeof(struct kemoview_mesh));
	if(kemo_mesh == NULL){
		printf("malloc error for kemoview_mesh\n");
		exit(0);
	}
	
    kemo_mesh->mesh_d =  alloc_viewer_mesh();
	kemo_mesh->mesh_m =  alloc_mesh_menu_val();
    kemo_mesh->msg_wk =  alloc_message_work();
	return kemo_mesh;
};

void dealloc_kemoview_mesh(struct kemoview_mesh *kemo_mesh){
	free(kemo_mesh->mesh_d);
	free(kemo_mesh->mesh_m);
    free(kemo_mesh->msg_wk);
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

int get_num_of_mesh_group(int iflag_group, struct kemoview_mesh *kemo_mesh){
	int np = 0;
	if(iflag_group == DOMAIN_FLAG){
		np = kemo_mesh->mesh_d->num_pe_sf;
	}else if(iflag_group == NODE_GRP_FLAG){
		np = kemo_mesh->mesh_d->ngrp_nod_sf;
	}else if(iflag_group == ELEM_GRP_FLAG){
		np = kemo_mesh->mesh_d->ngrp_ele_sf;
	}else if(iflag_group == SURF_GRP_FLAG){
		np = kemo_mesh->mesh_d->ngrp_surf_sf;
	};
	return np;
};

void set_draw_mesh_flag(int iflag_group, int selected, int igrp, int iflag, 
					   struct kemoview_mesh *kemo_mesh){
	if(iflag_group == DOMAIN_FLAG){
		set_draw_domain_flag(selected, igrp, iflag, kemo_mesh->mesh_m);
	}else if(iflag_group == NODE_GRP_FLAG){
		set_draw_nodgrp_flag(igrp, iflag, kemo_mesh->mesh_m);
	}else if(iflag_group == ELEM_GRP_FLAG){
		set_draw_elegrp_flag(selected, igrp, iflag, kemo_mesh->mesh_m);
	}else if(iflag_group == SURF_GRP_FLAG){
		set_draw_surfgrp_flag(selected, igrp, iflag, kemo_mesh->mesh_m);
	};
	return;
};

void toggle_draw_mesh_flag(int iflag_group, int selected, int igrp, 
						struct kemoview_mesh *kemo_mesh){
	if(iflag_group == DOMAIN_FLAG){
		toggle_draw_domain_flag(selected, igrp, kemo_mesh->mesh_d->num_pe_sf, kemo_mesh->mesh_m);
	}else if(iflag_group == NODE_GRP_FLAG){
		toggle_draw_nodgrp_flag(igrp, kemo_mesh->mesh_d->ngrp_nod_sf, kemo_mesh->mesh_m);
	}else if(iflag_group == ELEM_GRP_FLAG){
		toggle_draw_elegrp_flag(selected, igrp, kemo_mesh->mesh_d->ngrp_ele_sf, kemo_mesh->mesh_m);
	}else if(iflag_group == SURF_GRP_FLAG){
		toggle_draw_surfgrp_flag(selected, igrp, kemo_mesh->mesh_d->ngrp_surf_sf, kemo_mesh->mesh_m);
	};
	return;
};

int get_draw_mesh_flag(struct kemoview_mesh *kemo_mesh, 
						 int iflag_group, int selected, int igrp){
	int iflag = 0;
	if(iflag_group == DOMAIN_FLAG){
		iflag = get_draw_domain_flag(kemo_mesh->mesh_m, selected, igrp);
	}else if(iflag_group == NODE_GRP_FLAG){
		iflag = get_draw_nodgrp_flag(kemo_mesh->mesh_m, igrp);
	}else if(iflag_group == ELEM_GRP_FLAG){
		iflag = get_draw_elegrp_flag(kemo_mesh->mesh_m, selected, igrp);
	}else if(iflag_group == SURF_GRP_FLAG){
		iflag = get_draw_surfgrp_flag(kemo_mesh->mesh_m, selected, igrp);
	};
	return iflag;
};



void set_mesh_color_flag(int iflag_group, int selected, int icolor,
						 struct kemoview_mesh *kemo_mesh){
	if(iflag_group == DOMAIN_FLAG){
		set_domain_color_flag(selected, icolor, kemo_mesh->mesh_m);
	}else if(iflag_group == NODE_GRP_FLAG){
		set_node_grp_color_flag(icolor, kemo_mesh->mesh_m);
	}else if(iflag_group == ELEM_GRP_FLAG){
		set_ele_grp_color_flag(selected, icolor, kemo_mesh->mesh_m);
	}else if(iflag_group == SURF_GRP_FLAG){
		set_surf_grp_color_flag(selected, icolor, kemo_mesh->mesh_m);
	};
	return;
};

int get_mesh_color_flag(int iflag_group, int selected, 
						struct kemoview_mesh *kemo_mesh){
	int iflag = 0;
	if(iflag_group == DOMAIN_FLAG){
		iflag = get_domain_color_flag(selected, kemo_mesh->mesh_m);
	}else if(iflag_group == NODE_GRP_FLAG){
		iflag = get_node_grp_color_flag(kemo_mesh->mesh_m);
	}else if(iflag_group == ELEM_GRP_FLAG){
		iflag = get_ele_grp_color_flag(selected, kemo_mesh->mesh_m);
	}else if(iflag_group == SURF_GRP_FLAG){
		iflag = get_surf_grp_color_flag(selected, kemo_mesh->mesh_m);
	};
	return iflag;
};

void set_mesh_color_code(int iflag_group, int selected, float color_code4[4],
						 struct kemoview_mesh *kemo_mesh){
	if(iflag_group == DOMAIN_FLAG){
		set_domain_color_code(selected, color_code4, kemo_mesh->mesh_m);
	}else if(iflag_group == NODE_GRP_FLAG){
		set_node_grp_color_code(color_code4, kemo_mesh->mesh_m);
	}else if(iflag_group == ELEM_GRP_FLAG){
		set_ele_grp_color_code(selected, color_code4, kemo_mesh->mesh_m);
	}else if(iflag_group == SURF_GRP_FLAG){
		set_surf_grp_color_code(selected, color_code4, kemo_mesh->mesh_m);
	};
	return;
};

void get_mesh_color_code(struct kemoview_mesh *kemo_mesh, 
						 int iflag_group, int selected, float color_code4[4]){
	if(iflag_group == DOMAIN_FLAG){
		send_domain_color_code(kemo_mesh->mesh_m, selected, color_code4);
	}else if(iflag_group == NODE_GRP_FLAG){
		send_node_grp_color_code(kemo_mesh->mesh_m, color_code4);
	}else if(iflag_group == ELEM_GRP_FLAG){
		send_ele_grp_color_code(kemo_mesh->mesh_m, selected, color_code4);
	}else if(iflag_group == SURF_GRP_FLAG){
		send_surf_grp_color_code(kemo_mesh->mesh_m, selected, color_code4);
	};
	return;
};

void set_mesh_opacity(int iflag_group, double opacity_in, struct kemoview_mesh *kemo_mesh){
	if(iflag_group == DOMAIN_FLAG){
		kemo_mesh->mesh_m->domain_opacity = opacity_in;
	}else if(iflag_group == ELEM_GRP_FLAG){
		kemo_mesh->mesh_m->ele_grp_opacity = opacity_in;
	}else if(iflag_group == SURF_GRP_FLAG){
		kemo_mesh->mesh_m->surf_grp_opacity = opacity_in;
	};
	return;
};

double get_mesh_opacity(struct kemoview_mesh *kemo_mesh, int iflag_group){
	double opacity_out = 1.0;
	if(iflag_group == DOMAIN_FLAG){
		opacity_out = kemo_mesh->mesh_m->domain_opacity;
	}else if(iflag_group == ELEM_GRP_FLAG){
		opacity_out = kemo_mesh->mesh_m->ele_grp_opacity;
	}else if(iflag_group == SURF_GRP_FLAG){
		opacity_out = kemo_mesh->mesh_m->surf_grp_opacity;
	};
	return opacity_out;
};

