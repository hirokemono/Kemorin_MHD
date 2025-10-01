/*
 *  set_hex_tube_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_hex_tube_to_buf.h"

void interpolate_on_edge(double xyzw_mid[4], const double xyzw1[4], const double xyzw2[4], 
						 const double dat1, const double dat2, const double v_line){
	int nd;
	double coef = (dat2 - v_line) / (dat2 - dat1);
	for(nd=0; nd<4; nd++){
        xyzw_mid[nd] = coef * xyzw1[nd] + (1.0 - coef) * xyzw2[nd];
	};
    xyzw_mid[3] = 1.0;
	return;
};

int find_isoline_on_triangle(const double d_tri[3], const double v_line){
	double sig[3];
	int k1, i1, i2, i3;
	
	int idraw = 0;
	for (k1 = 0; k1 < 3; k1++) {
		i1 = (k1  )%3;
		i2 = (k1+1)%3;
		i3 = (k1+2)%3;
		
		sig[0] = (d_tri[i2] - v_line) * (d_tri[i3] - v_line);
		sig[1] = (d_tri[i3] - v_line) * (d_tri[i1] - v_line);
		sig[2] = (d_tri[i1] - v_line) * (d_tri[i2] - v_line);
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
	}
	return idraw;
};

int set_isoline_on_triangle(long iedge_itp[2], double xyzw_line[8],
                            long iele, const double xyzw_tri[12], 
                            const double d_tri[3], const double v_line,
                            struct psf_edge_data_c *psf_edge){
	double sig[3];
	int k1, nd, i1, i2, i3;
	
	int idraw = 0;
	for (k1 = 0; k1 < 3; k1++) {
		i1 = (k1  )%3;
		i2 = (k1+1)%3;
		i3 = (k1+2)%3;
		
		sig[0] = (d_tri[i2] - v_line) * (d_tri[i3] - v_line);
		sig[1] = (d_tri[i3] - v_line) * (d_tri[i1] - v_line);
		sig[2] = (d_tri[i1] - v_line) * (d_tri[i2] - v_line);
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			interpolate_on_edge(&xyzw_line[0],
								&xyzw_tri[4*i1], &xyzw_tri[4*i2], 
                                d_tri[i1], d_tri[i2], v_line);
			for(nd=0; nd<3; nd++){
                xyzw_line[4+nd] = xyzw_tri[4*i3+nd];
			};
            iedge_itp[0] = psf_edge->iedge_4_sf[iele][i1];
            iedge_itp[1] = psf_edge->iedge_4_sf[iele][i3];
            idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_on_edge(&xyzw_line[0],
								&xyzw_tri[4*i1], &xyzw_tri[4*i2], 
								d_tri[i1], d_tri[i2], v_line);
			interpolate_on_edge(&xyzw_line[4],
								&xyzw_tri[4*i3], &xyzw_tri[4*i2], 
								d_tri[i3], d_tri[i2], v_line);
            iedge_itp[0] = psf_edge->iedge_4_sf[iele][i1];
            iedge_itp[1] = psf_edge->iedge_4_sf[iele][i2];
            idraw = 1;
			break;
		}
	}
    xyzw_line[ 3] = 1.0;
    xyzw_line[ 7] = 1.0;
	if(idraw == 0) return idraw;
    
	if(v_line < 0.0){
		for(nd=0; nd<4; nd++){
            xyzw_line[ 4+nd] =   0.5*(xyzw_line[  nd] + xyzw_line[4+nd]);
		};
	};
	return idraw;
};
