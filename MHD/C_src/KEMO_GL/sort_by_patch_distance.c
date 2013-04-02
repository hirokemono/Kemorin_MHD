
/* sort_by_patch_distance.c */

#include "sort_by_patch_distance.h"

void copy_patch_distance_mesh(struct viewer_mesh *mesh_s){
	int i;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	for(i=0; i < mesh_s->nsurf_domain_sf;i++) mesh_s->iele_domain_far[i] = i;
	for(i=0; i < mesh_s->nele_ele_sf;i++) mesh_s->iele_grp_far[i] = i;
	for(i=0; i < mesh_s->nsurf_surf_sf;i++) mesh_s->isurf_grp_far[i] = i;
	
	return;
}

void sort_by_patch_distance_mesh(struct viewer_mesh *mesh_s, struct view_element *view_s){
	int ip, i, j, ist, ied, num;
	
	num = mesh_s->nsurf_each_sf * mesh_s->surfpetot_viewer;
	set_distance_in_model(view_s, num, mesh_s->surf_center_view,  mesh_s->z_ele_view);
	set_distance_in_model(view_s, mesh_s->num_pe_sf, mesh_s->domain_center,  mesh_s->z_center_view);
	
	for(i=0; i < mesh_s->num_pe_sf;i++){
		mesh_s->ip_domain_far[i] = i+1;
	};	
	for(i=0; i < mesh_s->nsurf_domain_sf;i++){
		j = mesh_s->isurf_domain_sf[i] * mesh_s->nsurf_each_sf;
		mesh_s->iele_domain_far[i] = i;
		mesh_s->z_domain_view[i] = mesh_s->z_ele_view[j];
	};
	for(i=0; i < mesh_s->nele_ele_sf;i++) {
		j = mesh_s->ele_item_sf[i] * mesh_s->nsurf_each_sf;
		mesh_s->iele_grp_far[i] = i;
		mesh_s->z_ele_grp_view[i] = mesh_s->z_ele_view[j];
	};
	for(i=0; i < mesh_s->nsurf_surf_sf;i++) {
		j = mesh_s->surf_item_sf[i] * mesh_s->nsurf_each_sf;
		mesh_s->isurf_grp_far[i] = i;
		mesh_s->z_surf_grp_view[i] = mesh_s->z_ele_view[j];
	};
	
	ied = mesh_s->num_pe_sf - 1;
	quicksort_double_c(mesh_s->z_center_view, mesh_s->ip_domain_far, IZERO, ied);
	
	for(ip=0; ip<mesh_s->num_pe_sf;ip++){
		ist = mesh_s->isurf_stack_domain_sf[ip];
		ied = mesh_s->isurf_stack_domain_sf[ip+1]-1;
		quicksort_double_c(mesh_s->z_domain_view, mesh_s->iele_domain_far, ist, ied);
	}
	
	for(j=0; j < mesh_s->ngrp_ele_sf;j++) {
		for(ip=0; ip < mesh_s->num_pe_sf;ip++){
			i = ip + j*mesh_s->num_pe_sf;
			ist = mesh_s->ele_stack_sf[i];
			ied = mesh_s->ele_stack_sf[i+1]-1;
			quicksort_double_c(mesh_s->z_ele_grp_view, mesh_s->iele_grp_far, ist, ied);
		}
	}
	
	for(j=0; j < mesh_s->ngrp_surf_sf;j++) {
		for(ip=0; ip < mesh_s->num_pe_sf;ip++){
			i = ip + j*mesh_s->num_pe_sf;
			ist = mesh_s->surf_stack_sf[i];
			ied = mesh_s->surf_stack_sf[i+1]-1;
			quicksort_double_c(mesh_s->z_surf_grp_view, mesh_s->isurf_grp_far, ist, ied);
		}
	}
	/*
	for(ip=0; ip<mesh_s->num_pe_sf;ip++){
		ist = mesh_s->isurf_stack_domain_sf[ip];
		ied = mesh_s->isurf_stack_domain_sf[ip+1]-1;
		for(i=ist;i<ied+1;i++){
			j = mesh_s->iele_domain_far[i];
			printf("mesh_s->iele_domain_far %d %d\n",i,mesh_s->isurf_domain_sf[j]);
		}
	}
	*/
	return;
}


void copy_patch_distance_psf(struct psf_data *viz_s){
	int i;
	for(i=0; i < viz_s->nele_viz;i++) {viz_s->iele_viz_far[i] = i+1;};
	return;
}

void sort_by_patch_distance_psf(struct psf_data *viz_s, struct view_element *view_s){
	int i;
	
	
	set_distance_in_model(view_s, viz_s->nele_viz, viz_s->x_ele_viz, viz_s->z_ele_viz);
	
	for(i=0; i < viz_s->nele_viz;i++) {
		viz_s->iele_viz_far[i] =   i+1;
		viz_s->z_ele_viz[i] = viz_s->z_ele_viz[i];
	};
	quicksort_double_c(viz_s->z_ele_viz, viz_s->iele_viz_far, IZERO, (viz_s->nele_viz-1));
	
	return;
}
