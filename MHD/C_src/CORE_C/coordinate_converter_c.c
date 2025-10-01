
/*  coordinate_converter_c.c */

#include "coordinate_converter_c.h"

void aitoff_c(int nnod, double *rtp, double *xy_map){
	
	int i;
	double sin_t, cos_t, phi;
	double xl2, den;
	
	for (i = 0; i < nnod; i++){
		cos_t = cos( rtp[3*i+1] );
		sin_t = sin( rtp[3*i+1] );
		phi =   rtp[3*i+2];
		
		xl2 = phi / TWO;
		den = sqrt( ONE + sin_t*sin(xl2) );
		
		xy_map[2*i  ] = -TWO * sin_t * cos(xl2) / den;
		xy_map[2*i+1] = cos_t / den;
	};
	return;
}


void position_2_sph_c(int nnod, double *xyz, double *rtp){
	
	int i;
	double rs, ar, as, ratio_x, ratio_z, pi;
	
	pi = FOUR*atan(ONE);
	
	for (i = 0; i < nnod; i++){
		rtp[3*i  ]
		=   sqrt( xyz[3*i  ] * xyz[3*i  ]
				+ xyz[3*i+1] * xyz[3*i+1]
				+ xyz[3*i+2] * xyz[3*i+2]);
		rs = sqrt(xyz[3*i  ] * xyz[3*i  ]
				+ xyz[3*i+1] * xyz[3*i+1]);
		/*
		printf("rtp, rs %e %e \n", rtp[3*i  ], rs);
		*/
		if( rtp[3*i  ] == ZERO){
			rtp[3*i+1] = ZERO;
			rtp[3*i+2] = ZERO;
		}
		else if(rs == ZERO){
			if( xyz[3*i+2] >= ZERO) {
				rtp[3*i+1] = ZERO;
			} else {
				rtp[3*i+1] = pi;
			};
		}
		else {
			ar = ONE / rtp[3*i  ];
			as = ONE / rs;
			ratio_x = xyz[3*i  ] * as;
			ratio_z = xyz[3*i+2] * ar;
			
			if( ratio_z > ONE){
				rtp[3*i+1] = ZERO;
			} else if ( ratio_z < -ONE){
				rtp[3*i+1] = pi;
			} else {
				rtp[3*i+1] = acos(ratio_z);
			}
			
			if( xyz[3*i+1] == ZERO){
				if( xyz[3*i  ] > ZERO){
					rtp[3*i+2] = ZERO;
				} else {
					rtp[3*i+2] = pi;
				}
			} else if(ratio_x >=  ONE) {
				rtp[3*i+2] = ZERO;
			} else if(ratio_x <= -ONE) {
				rtp[3*i+2] = pi;
			} else if(xyz[3*i+1] >= ZERO) {
				rtp[3*i+2] = acos(ratio_x);
			} else if(xyz[3*i+1] <  ZERO) {
				rtp[3*i+2] = TWO*pi - acos(ratio_x);
			};
		};
		
	};
	
	return;
};

void xyzw_to_rtpw_c(long nnod, double *xyzw, double *rtpw){
    int i;
    double rs, ar, as, ratio_x, ratio_z, pi;
    
    pi = FOUR*atan(ONE);
    
    for (i = 0; i < nnod; i++){
        rtpw[4*i  ]
        =   sqrt( xyzw[4*i  ] * xyzw[4*i  ]
                + xyzw[4*i+1] * xyzw[4*i+1]
                + xyzw[4*i+2] * xyzw[4*i+2]);
        rs = sqrt(xyzw[4*i  ] * xyzw[4*i  ]
                + xyzw[4*i+1] * xyzw[4*i+1]);
        /*
        printf("rtpw, rs %e %e \n", rtpw[4*i  ], rs);
        */
        if( rtpw[4*i  ] == ZERO){
            rtpw[4*i+1] = ZERO;
            rtpw[4*i+2] = ZERO;
        }
        else if(rs == ZERO){
            if( xyzw[4*i+2] >= ZERO) {
                rtpw[4*i+1] = ZERO;
            } else {
                rtpw[4*i+1] = pi;
            };
        }
        else {
            ar = ONE / rtpw[4*i  ];
            as = ONE / rs;
            ratio_x = xyzw[4*i  ] * as;
            ratio_z = xyzw[4*i+2] * ar;
            
            if( ratio_z > ONE){
                rtpw[4*i+1] = ZERO;
            } else if ( ratio_z < -ONE){
                rtpw[4*i+1] = pi;
            } else {
                rtpw[4*i+1] = acos(ratio_z);
            }
            
            if( xyzw[4*i+1] == ZERO){
                if( xyzw[4*i  ] > ZERO){
                    rtpw[4*i+2] = ZERO;
                } else {
                    rtpw[4*i+2] = pi;
                }
            } else if(ratio_x >=  ONE) {
                rtpw[4*i+2] = ZERO;
            } else if(ratio_x <= -ONE) {
                rtpw[4*i+2] = pi;
            } else if(xyzw[4*i+1] >= ZERO) {
                rtpw[4*i+2] = acos(ratio_x);
            } else if(xyzw[4*i+1] <  ZERO) {
                rtpw[4*i+2] = TWO*pi - acos(ratio_x);
            };
        };
        rtpw[4*i+3] = 1.0;
    };
    return;
};


void const_map_data_from_xyz(int nnod, double *xyz, double *rtp, double *xy_map){
	int i;
	double pi = TWO * acos(ZERO);
	
	position_2_sph_c(nnod, xyz, rtp);
	
	for (i=0;i<nnod;i++){rtp[3*i+2] = fmod(rtp[3*i+2]+pi,(TWO*pi));};
	
	aitoff_c(nnod, rtp, xy_map);
	return;
}

void sph_vector_to_xyz_vect(double theta, double phi, double v_sph[3], double v_xyz[3]){
	v_xyz[0] = v_sph[0]*sin(theta)*cos(phi) + v_sph[1]*cos(theta)*cos(phi) - v_sph[2]*sin(phi);
	v_xyz[1] = v_sph[0]*sin(theta)*sin(phi) + v_sph[1]*cos(theta)*sin(phi) + v_sph[2]*cos(phi);
	v_xyz[2] = v_sph[0]*cos(theta) - v_sph[1]*sin(theta);
	return;
};
void cyl_vector_to_xyz_vect(double phi, double v_cyl[3], double v_xyz[3]){
	v_xyz[0] = v_cyl[0]*cos(phi) - v_cyl[1]*sin(phi);
	v_xyz[1] = v_cyl[0]*sin(phi) + v_cyl[1]*cos(phi);
	v_xyz[2] = v_cyl[2];
	return;
};

