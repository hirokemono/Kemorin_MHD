/*  set_new_patch_4_map_c.c */

#include "set_new_patch_4_map_c.h"

static const int ie_map_12_23[9] = {3, 1, 5,  2, 5, 4,   1, 4, 5};
static const int ie_map_12_31[9] = {2, 3, 5,  3, 4, 5,   1, 5, 4};
static const int ie_map_23_31[9] = {1, 2, 4,  1, 4, 5,   3, 5, 4};

static const int ie_map_12_3[6] = {3, 1, 4,   2, 3, 4};
static const int ie_map_23_1[6] = {1, 2, 4,   3, 1, 4};
static const int ie_map_31_2[6] = {2, 3, 4,   1, 2, 4};


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
	
	for(i = 0; i <3; i++){y[i] = xyz_tri[3*i+1];};
    if(xyz_tri[0] >= 0. && xyz_tri[3] >= 0. && xyz_tri[6] >= 0.){return 0;};

//	for (i=0; i<3; i++) if(fabs(y[i]) < 1.e-16) y[i] = ZERO;
    
    if((y[0] == ZERO) && (y[1] == ZERO) && (y[2] == ZERO)){return 0;}
/*
    else if((y[0] > ZERO) && (y[1] == ZERO) && (y[2] == ZERO)){return 21;}
    else if((y[1] > ZERO) && (y[2] == ZERO) && (y[0] == ZERO)){return 22;}
    else if((y[2] > ZERO) && (y[0] == ZERO) && (y[1] == ZERO)){return 23;}

    else if((y[0] > ZERO) && (y[1] > ZERO) && (y[2] == ZERO)){return 11;}
    else if((y[1] > ZERO) && (y[2] > ZERO) && (y[0] == ZERO)){return 12;}
    else if((y[2] > ZERO) && (y[0] > ZERO) && (y[1] == ZERO)){return 13;}

 */
    else if( ((y[0]*y[1]) < ZERO) && ((y[1]*y[2]) < ZERO) ){return 221;}
    else if( ((y[0]*y[1]) < ZERO) && ( y[2] == ZERO) )     {return 121;}
	
    else if( ((y[0]*y[1]) < ZERO) && ((y[2]*y[0]) < ZERO) ){return 222;}
    else if( ((y[1]*y[2]) < ZERO) && ( y[0] == ZERO) )     {return 122;}
	
    else if( ((y[1]*y[2]) < ZERO) && ((y[2]*y[0]) < ZERO) ){return 223;}
    else if( ((y[2]*y[0]) < ZERO) && ( y[1] == ZERO) )     {return 123;}
	
	return 0;
}

int cut_new_patch_at_phi180(double *xyz_org, long ie_cut[9], double y_cut[3],
							long inod_src[4], double coef_cut[4]){
	int num_map_patch;
	long i, k1;
	double y[3];
	
	for (i=0; i<3; i++){y[i] = xyz_org[3*i+1];};
//    for (i=0; i<3; i++) if(fabs(y[i]) < 1.e-16) y[i] = ZERO;

	for (i=0; i<4; i++){inod_src[i] = 0;};
	for (i=0; i<4; i++){coef_cut[i] = 0.0;};
	
	if(xyz_org[0] >= 0. && xyz_org[3] >= 0. && xyz_org[6] >= 0.){
		num_map_patch = 0;
		for (i=0; i<3; i++){ie_cut[i] = i+1;};
	}
    else if((y[0] == ZERO) && (y[1] == ZERO) && (y[2] == ZERO)){
        num_map_patch = 0;
        for (i=0; i<3; i++){ie_cut[i] = i+1;};
    }
/*
    else if((y[0] > ZERO) && (y[1] == ZERO) && (y[2] == ZERO)){
        num_map_patch = 21;
        ie_cut[0] = 1;
        ie_cut[1] = 4;
        ie_cut[2] = 5;
        y_cut[0] = y[0];

        inod_src[0] = 2;
        inod_src[1] = 2;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
        
        inod_src[2] = 3;
        inod_src[3] = 3;
        coef_cut[2] = 1.0;
        coef_cut[3] = 0.0;
    }
    else if((y[1] > ZERO) && (y[2] == ZERO) && (y[0] == ZERO)){
        num_map_patch = 22;
        ie_cut[0] = 5;
        ie_cut[1] = 1;
        ie_cut[2] = 4;
        y_cut[0] = y[1];

        inod_src[0] = 3;
        inod_src[1] = 3;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
        
        inod_src[2] = 1;
        inod_src[3] = 1;
        coef_cut[2] = 1.0;
        coef_cut[3] = 0.0;
    }
    else if((y[2] > ZERO) && (y[0] == ZERO) && (y[1] == ZERO)){
        num_map_patch = 23;
        ie_cut[0] = 4;
        ie_cut[1] = 5;
        ie_cut[2] = 1;
        y_cut[0] = y[2];
        
        inod_src[0] = 1;
        inod_src[1] = 1;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
        
        inod_src[2] = 2;
        inod_src[3] = 2;
        coef_cut[2] = 1.0;
        coef_cut[3] = 0.0;
    }

    else if((y[0] > ZERO) && (y[1] > ZERO) && (y[2] == ZERO)){
        num_map_patch = 11;
        ie_cut[0] = 1;
        ie_cut[1] = 2;
        ie_cut[2] = 4;
        y_cut[0] = y[0];
        
        inod_src[0] = 3;
        inod_src[1] = 3;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
    }
    else if((y[1] > ZERO) && (y[2] > ZERO) && (y[0] == ZERO)){
        num_map_patch = 12;
        ie_cut[0] = 4;
        ie_cut[1] = 2;
        ie_cut[2] = 3;
        y_cut[0] = y[1];
        
        inod_src[0] = 1;
        inod_src[1] = 1;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
    }
    else if((y[2] > ZERO) && (y[0] > ZERO) && (y[1] == ZERO)){
        num_map_patch = 13;
        ie_cut[0] = 1;
        ie_cut[1] = 4;
        ie_cut[2] = 3;
        y_cut[0] = y[2];
        
        inod_src[0] = 2;
        inod_src[1] = 2;
        coef_cut[0] = 1.0;
        coef_cut[1] = 0.0;
   }
*/
    
    
	else if ( ((y[0]*y[1]) < ZERO) && ((y[1]*y[2]) < ZERO) ){
		num_map_patch = 221;
		
		inod_src[0] = 1;
		inod_src[1] = 2;
		coef_cut[0] =  y[1] / (y[1]-y[0]);
		coef_cut[1] = -y[0] / (y[1]-y[0]);
		
		inod_src[2] = 2;
		inod_src[3] = 3;
		coef_cut[2] =  y[2] / (y[2]-y[1]);
		coef_cut[3] = -y[1] / (y[2]-y[1]);
		
        for (i=0; i<3; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_12_23[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1];
        y_cut[2] = y[ie_cut[6]-1];
	}
	else if( ((y[0]*y[1]) < ZERO) && (y[2] == ZERO) ){
        num_map_patch = 121;

		inod_src[0] = 1;
		inod_src[1] = 2;
		coef_cut[0] =  y[1] / (y[1]-y[0]);
		coef_cut[1] = -y[0] / (y[1]-y[0]);
        inod_src[2] = 1;
        inod_src[3] = 2;
        coef_cut[2] =  y[1] / (y[1]-y[0]);
        coef_cut[3] = -y[0] / (y[1]-y[0]);

        for(i=0; i<2; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_12_3[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1] + y[ie_cut[4]-1];
	}
	else if( ((y[0]*y[1]) < ZERO) && ((y[2]*y[0]) < ZERO) ){
        num_map_patch = 222;

		inod_src[0] = 3;
		inod_src[1] = 1;
		coef_cut[0] =  y[0] / (y[0]-y[2]);
		coef_cut[1] = -y[2] / (y[0]-y[2]);
		
		inod_src[2] = 1;
		inod_src[3] = 2;
		coef_cut[2] =  y[1] / (y[1]-y[0]);
		coef_cut[3] = -y[0] / (y[1]-y[0]);
		
        for (i=0; i<3; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_12_31[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1];
        y_cut[2] = y[ie_cut[6]-1];
	}
	else if( ((y[1]*y[2]) < ZERO) && (y[0] == ZERO) ){
        num_map_patch = 122;

        inod_src[0] = 2;
        inod_src[1] = 3;
        coef_cut[0] =  y[2] / (y[2]-y[1]);
        coef_cut[1] = -y[1] / (y[2]-y[1]);
        inod_src[2] = 2;
        inod_src[3] = 3;
        coef_cut[2] =  y[2] / (y[2]-y[1]);
        coef_cut[3] = -y[1] / (y[2]-y[1]);

        for(i=0; i<2; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_23_1[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1] + y[ie_cut[4]-1];
	}
	else if( ((y[1]*y[2]) < ZERO) && ((y[2]*y[0]) < ZERO) ){
        num_map_patch = 223;

		inod_src[0] = 2;
		inod_src[1] = 3;
		coef_cut[0] =  y[2] / (y[2]-y[1]);
		coef_cut[1] = -y[1] / (y[2]-y[1]);
		
		inod_src[2] = 3;
		inod_src[3] = 1;
		coef_cut[2] =  y[0] / (y[0]-y[2]);
		coef_cut[3] = -y[2] / (y[0]-y[2]);
		
        for (i=0; i<3; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_23_31[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1];
        y_cut[2] = y[ie_cut[6]-1];
	}
	
	else if( ((y[2]*y[0]) < ZERO) && (y[1] == ZERO) ){
        num_map_patch = 123;

		inod_src[0] = 3;
		inod_src[1] = 1;
		coef_cut[0] =  y[0] / (y[0]-y[2]);
		coef_cut[1] = -y[2] / (y[0]-y[2]);
        inod_src[2] = 3;
        inod_src[3] = 1;
        coef_cut[2] =  y[0] / (y[0]-y[2]);
        coef_cut[3] = -y[2] / (y[0]-y[2]);

        for(i=0; i<2; i++){
            for(k1=0;k1<3;k1++){ie_cut[3*i+k1] = ie_map_31_2[3*i+k1];};
        };
        y_cut[0] = y[ie_cut[0]-1] + y[ie_cut[1]-1];
        y_cut[1] = y[ie_cut[3]-1] + y[ie_cut[4]-1];
	}
	else{
		num_map_patch = 0;
		for (i=0; i<3; i++){ie_cut[i] = i+1;};
	};
	
	return num_map_patch;
}
