

/*  projection_patch_to_map.c */

#include "projection_patch_to_map.h"


int latitude_longitude_on_map(double *xyz_patch, double *rtp_patch){
	int iflag = 0;
	double xyz_center[3];
	double pi;
	int i;
	

	xyz_center[0] = (xyz_patch[0] + xyz_patch[3] + xyz_patch[6]) / THREE;
	xyz_center[1] = (xyz_patch[1] + xyz_patch[4] + xyz_patch[7]) / THREE;
	/* xyz_center[2] = (xyz_patch[2] + xyz_patch[5] + xyz_patch[8]) / THREE; */
	
	pi = FOUR*atan(ONE);
	position_2_sph_c(ITHREE, xyz_patch, rtp_patch);
	for (i=0;i<3;i++){rtp_patch[3*i+2] = fmod(rtp_patch[3*i+2]+pi,(TWO*pi));};

	for (i=0; i<3; i++) if(fabs(xyz_patch[3*i+1]) < EPSILON) xyz_patch[3*i+1] = ZERO;
	/*
	if(   fabs(rtp_patch[8]-rtp_patch[2]) >1.0
	   || fabs(rtp_patch[8]-rtp_patch[5]) >1.0
	   || fabs(rtp_patch[5]-rtp_patch[2]) >1.0){
		printf("Error x: %e %e %e \n",  xyz_patch[0], xyz_patch[3], xyz_patch[6]);
		printf("Error y: %e %e %e \n",  xyz_patch[1], xyz_patch[4], xyz_patch[7]);
		printf("Error phi: %e %e %e \n",  rtp_patch[2], rtp_patch[5], rtp_patch[8]);
	};
*/ 
	if( (xyz_patch[1]*xyz_patch[4]*xyz_patch[7]) == ZERO && xyz_center[0] <= ZERO){
		iflag = 1;

/*
		if(   fabs(rtp_patch[8]-rtp_patch[2]) >1.0
		   || fabs(rtp_patch[8]-rtp_patch[5]) >1.0
		   || fabs(rtp_patch[5]-rtp_patch[2]) >1.0){
			printf("Error x: %e %e %e \n",  xyz_patch[0], xyz_patch[3], xyz_patch[6]);
			printf("Error y: %e %e %e \n",  xyz_patch[1], xyz_patch[4], xyz_patch[7]);
			printf("Error phi: %e %e %e \n",  rtp_patch[2], rtp_patch[5], rtp_patch[8]);
		};
*/
		if(xyz_center[1] <= ZERO){
/*
			printf("Before A %e %e %e %e %e %e\n", 
				   rtp_patch[1], rtp_patch[4], rtp_patch[7],
				   rtp_patch[2], rtp_patch[5], rtp_patch[8]);
*/
			if(fabs(xyz_patch[1]) < EPSILON && xyz_patch[0] < ZERO) rtp_patch[2] = ZERO;
			if(fabs(xyz_patch[4]) < EPSILON && xyz_patch[3] < ZERO) rtp_patch[5] = ZERO;
			if(fabs(xyz_patch[7]) < EPSILON && xyz_patch[6] < ZERO) rtp_patch[8] = ZERO;
/*
			printf("After  A %e %e %e %e %e %e\n", 
				   rtp_patch[1], rtp_patch[4], rtp_patch[7],
				   rtp_patch[2], rtp_patch[5], rtp_patch[8]);
			printf("\n");
*/
		}
		else{
/*
			printf("Before B %e %e %e %e %e %e\n", 
				   rtp_patch[1], rtp_patch[4], rtp_patch[7],
				   rtp_patch[2], rtp_patch[5], rtp_patch[8]);
*/
			if(fabs(xyz_patch[1]) < EPSILON && xyz_patch[0] < ZERO) rtp_patch[2] = TWO * pi;
			if(fabs(xyz_patch[4]) < EPSILON && xyz_patch[3] < ZERO) rtp_patch[5] = TWO * pi;
			if(fabs(xyz_patch[7]) < EPSILON && xyz_patch[6] < ZERO) rtp_patch[8] = TWO * pi;
/*
			printf("After  B %e %e %e %e %e %e\n", 
				   rtp_patch[1], rtp_patch[4], rtp_patch[7],
				   rtp_patch[2], rtp_patch[5], rtp_patch[8]);
			printf("\n");
*/
		};
 
	};
	
	/*
	if( (xyz_patch[1]*xyz_patch[4]*xyz_patch[7]) == ZERO && xyz_center[0] >= ZERO){
			 printf("xyz_center a  %e %e %e \n", xyz_center[0], xyz_center[1], xyz_center[2]);
			 printf("xyz_patch a  %e %e %e \n", xyz_patch[0], xyz_patch[1], xyz_patch[2]);
			 printf("xyz_patch b  %e %e %e \n", xyz_patch[3], xyz_patch[4], xyz_patch[5]);
			 printf("xyz_patch c  %e %e %e \n", xyz_patch[6], xyz_patch[7], xyz_patch[8]);
			 printf("rtp_patch a  %e %e %e \n", rtp_patch[0], rtp_patch[1], rtp_patch[2]);
			 printf("rtp_patch b  %e %e %e \n", rtp_patch[3], rtp_patch[4], rtp_patch[5]);
			 printf("rtp_patch c  %e %e %e \n", rtp_patch[6], rtp_patch[7], rtp_patch[8]);
	 }
	*/
	return iflag;
}

void projection_patch_to_map(double *xyz_patch, double *xy_map_patch){
	double rtp_patch[9];
	
	latitude_longitude_on_map(xyz_patch, rtp_patch);
	aitoff_c(ITHREE, rtp_patch, xy_map_patch);
	/*
	if(iflag >= 0){
			 printf("xy_map_patch a  %e %e \n", xy_map_patch[0], xy_map_patch[1]);
			 printf("xy_map_patch b  %e %e \n", xy_map_patch[2], xy_map_patch[3]);
			 printf("xy_map_patch c  %e %e \n", xy_map_patch[4], xy_map_patch[5]);
	 }
	*/
	return;
}
