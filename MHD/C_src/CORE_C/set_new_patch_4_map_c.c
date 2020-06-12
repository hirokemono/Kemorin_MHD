/*  set_new_patch_4_map_c.c */

#include "set_new_patch_4_map_c.h"

static const int ie_map_12_23[9] = {1, 4, 5,  1, 5, 3,  4, 2, 5};
static const int ie_map_12_31[9] = {5, 2, 3,  5, 3, 4,  1, 5, 4};
static const int ie_map_23_31[9] = {1, 2, 4,  1, 4, 5,  5, 4, 3};

static const int ie_map_12_3[6] = {1, 4, 3,  4, 2, 3};
static const int ie_map_23_1[6] = {4, 1, 2,  1, 4, 3};
static const int ie_map_31_2[6] = {1, 2, 4,  4, 2, 3};


void projection_patch_4_map(double *xyz_patch, double *xyz_map){
	int i;
	double xy_map[6];
	
	projection_patch_to_map(&xyz_patch[0], &xy_map[0]);
	
	for (i = 0; i <3; i++){
		xyz_map[3*i  ] = xy_map[2*i  ];
		xyz_map[3*i+1] = xy_map[2*i+1];
		xyz_map[3*i+2] = ZERO;
	};
	return;
}


int count_new_patch_at_phi180(double *xyz_tri){
	int num_map_patch;
	int i;
	double y[3];
	
	for (i = 0; i <3; i++){y[i] = xyz_tri[3*i+1];};
	/*
	for (i=0; i<3; i++) if(fabs(y[i]) < EPSILON) y[i] = ZERO;
	*/
	if(xyz_tri[0] >= 0. && xyz_tri[3] >= 0. && xyz_tri[6] >= 0.){num_map_patch = 1;}
	else if( ((y[0]*y[1]) < ZERO) && ((y[1]*y[2]) < ZERO) ){num_map_patch = 3;}
	else if( ((y[0]*y[1]) < ZERO) && ( y[2] == ZERO) )     {num_map_patch = 2;}
	
	else if( ((y[0]*y[1]) < ZERO) && ((y[2]*y[0]) < ZERO) ){num_map_patch = 3;}
	else if( ((y[1]*y[2]) < ZERO) && ( y[0] == ZERO) )     {num_map_patch = 2;}
	
	else if( ((y[1]*y[2]) < ZERO) && ((y[2]*y[0]) < ZERO) ){num_map_patch = 3;}
	else if( ((y[2]*y[0]) < ZERO) && ( y[1] == ZERO) )     {num_map_patch = 2;}
	else{num_map_patch = 1;};
	
	return num_map_patch;
}

int cut_new_patch_at_phi180(double *xyz_org, long ie_cut[9],
							long inod_src[4], double coef_cut[4]){
	int num_map_patch;
	long i;
	double y[3];
	
	for (i=0; i<3; i++){y[i] = xyz_org[3*i+1];};
	
	for (i=0; i<4; i++){inod_src[i] = 0;};
	for (i=0; i<4; i++){coef_cut[i] = 0.0;};
	
	if(xyz_org[0] >= 0. && xyz_org[3] >= 0. && xyz_org[6] >= 0.){
		num_map_patch = 1;
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = i+1;};
	}
	else if ( ((y[0]*y[1]) < ZERO) && ((y[1]*y[2]) < ZERO) ){
		num_map_patch = 3;
		
		inod_src[0] = 1;
		inod_src[1] = 2;
		coef_cut[0] =  y[1] / (y[1]-y[0]);
		coef_cut[1] = -y[0] / (y[1]-y[0]);
		
		inod_src[2] = 2;
		inod_src[3] = 3;
		coef_cut[2] =  y[2] / (y[2]-y[1]);
		coef_cut[3] = -y[1] / (y[2]-y[1]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_12_23[i];};
	}
	else if( ((y[0]*y[1]) < ZERO) && (y[2] == ZERO) ){
		num_map_patch = 2;
		
		inod_src[0] = 1;
		inod_src[1] = 2;
		coef_cut[0] =  y[1] / (y[1]-y[0]);
		coef_cut[1] = -y[0] / (y[1]-y[0]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_12_3[i];};
	}
	else if( ((y[0]*y[1]) < ZERO) && ((y[2]*y[0]) < ZERO) ){
		num_map_patch = 3;
		
		inod_src[0] = 3;
		inod_src[1] = 1;
		coef_cut[0] =  y[0] / (y[0]-y[2]);
		coef_cut[1] = -y[2] / (y[0]-y[2]);
		
		inod_src[2] = 1;
		inod_src[3] = 2;
		coef_cut[2] =  y[1] / (y[1]-y[0]);
		coef_cut[3] = -y[0] / (y[1]-y[0]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_12_31[i];};
	}
	else if( ((y[1]*y[2]) < ZERO) && (y[0] == ZERO) ){
		num_map_patch = 2;
		
		inod_src[0] = 2;
		inod_src[1] = 3;
		coef_cut[0] =  y[2] / (y[2]-y[1]);
		coef_cut[1] = -y[1] / (y[2]-y[1]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_23_1[i];};
	}
	else if( ((y[1]*y[2]) < ZERO) && ((y[2]*y[0]) < ZERO) ){
		num_map_patch = 3;
		
		inod_src[0] = 2;
		inod_src[1] = 3;
		coef_cut[0] =  y[2] / (y[2]-y[1]);
		coef_cut[1] = -y[1] / (y[2]-y[1]);
		
		inod_src[2] = 3;
		inod_src[3] = 1;
		coef_cut[2] =  y[0] / (y[0]-y[2]);
		coef_cut[3] = -y[2] / (y[0]-y[2]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_23_31[i];};
	}
	
	else if( ((y[2]*y[0]) < ZERO) && (y[1] == ZERO) ){
		num_map_patch = 2;
		
		inod_src[0] = 3;
		inod_src[1] = 1;
		coef_cut[0] =  y[0] / (y[0]-y[2]);
		coef_cut[1] = -y[2] / (y[0]-y[2]);
		
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = ie_map_31_2[i];};
	}
	else{
		num_map_patch = 1;
		for (i=0; i<(3*num_map_patch); i++){ie_cut[i] = i+1;};
	};
	
	return num_map_patch;
}
