
/* set_coastline_to_buf.c */

#include  "set_coastline_to_buf.h"

#define NUM_T   6
#define NUM_P   6
#define N_CURVE 180

double theta_p_grid[N_CURVE+1];
double phi_p_grid[NUM_P+1];
double theta_t_grid[NUM_T+1];
double phi_t_grid[N_CURVE+1];


void init_mapgrid_position(void){
	int i, j;
	double pi;
	
	pi = TWO * acos(ZERO);
	
	for(i=0; i<N_CURVE+1; i++){theta_p_grid[i] = (double)i * pi / (double)N_CURVE;}
	for(j=0; j<NUM_P+1; j++) {phi_p_grid[j] = (double)j * TWO * pi / (double)NUM_P;}
	
	for(j=0; j<NUM_T-1; j++){theta_t_grid[j] = (double)(j+1) * pi / (double)NUM_T;}
	for(i=0; i<N_CURVE+1; i++){phi_t_grid[i] = (double)i * TWO * pi / (double)N_CURVE;}
	return;
}

long count_sph_flame(void){
	long num_edge = (NUM_P+1) * N_CURVE + (NUM_T-1) * N_CURVE;
	return num_edge;
}

static void set_meridional_frame(int i_theta, double radius,
                                 double *theta_grid, double phi_grid, 
                                 double xyzw_line[8], double dir_line[8],
                                 double norm_line[8]){
    int nd;
    double t_min = theta_grid[i_theta];
    double t_mid = 0.5 * (theta_grid[i_theta] + theta_grid[i_theta+1]);
    norm_line[0] = sin(t_min) * cos(phi_grid);
    norm_line[1] = sin(t_min) * sin(phi_grid);
    norm_line[2] = cos(t_min);
    norm_line[3] = 1.0;
    norm_line[4] = sin(t_mid) * cos(phi_grid);
    norm_line[5] = sin(t_mid) * sin(phi_grid);
    norm_line[6] = cos(t_mid);
    norm_line[7] = 1.0;
    
    dir_line[0] =  cos(t_min) * cos(phi_grid);
    dir_line[1] =  cos(t_min) * sin(phi_grid);
    dir_line[2] = -sin(t_min);
    dir_line[3] = 1.0;
    dir_line[4] =  cos(t_mid) * cos(phi_grid);
    dir_line[5] =  cos(t_mid) * sin(phi_grid);
    dir_line[6] = -sin(t_mid);
    dir_line[7] = 1.0;
    
    for(nd = 0;nd < 8; nd++){xyzw_line[nd] = radius * norm_line[nd];};
    xyzw_line[3] = 1.0;
    xyzw_line[7] = 1.0;
    return;
}

static void set_latitude_frame(int i_phi, double radius,
                               double theta_grid, double *phi_grid, 
                               double xyzw_line[8], double dir_line[8],
                               double norm_line[8]){
    int nd;
    double p_min = phi_grid[i_phi];
	double p_mid = 0.5 * (phi_grid[i_phi] + phi_grid[i_phi+1]);
    norm_line[0] = sin(theta_grid) * cos(p_min);
    norm_line[1] = sin(theta_grid) * sin(p_min);
    norm_line[2] = cos(theta_grid);
    norm_line[3] = 1.0;
    norm_line[4] = sin(theta_grid) * cos(p_mid);
    norm_line[5] = sin(theta_grid) * sin(p_mid);
    norm_line[6] = cos(theta_grid);
    norm_line[7] = 1.0;
    
    dir_line[0] = -sin(p_min);
    dir_line[1] =  cos(p_min);
    dir_line[2] = 0.0;
    dir_line[3] = 1.0;
    dir_line[4] = -sin(p_mid);
    dir_line[5] =  cos(p_mid);
    dir_line[6] = 0.0;
    dir_line[7] = 1.0;
    
    for(nd = 0;nd < 8; nd++){xyzw_line[nd] = radius * norm_line[nd];};
    xyzw_line[3] = 1.0;
    xyzw_line[7] = 1.0;
	return;
}

long set_sph_flame_line_to_buf(double radius, struct gl_strided_buffer *strided_buf){
    long inum;
	int i, j, nd;
	
    double xyzw_line[8];
    double dir_line[8], norm_line[8];
    double color_line[8];
    
	set_black_color_c(&color_line[0]);
    for(nd=0;nd<4;nd++){color_line[nd+4] = color_line[nd];}

	inum = 0;
	for(j=0; j<NUM_P+1; j++){
		for(i=0; i<N_CURVE; i++){
            set_meridional_frame(i, radius, theta_p_grid, phi_p_grid[j],
                                 xyzw_line, dir_line, norm_line);
            inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		for(i=0; i<N_CURVE; i++){
            set_latitude_frame(i, radius, theta_t_grid[j], phi_t_grid,
                               xyzw_line, dir_line, norm_line);
            inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
		}
	}
	return inum;
}

long set_sph_flame_tube_to_buf(int ncorner, double tube_radius, 
                               double radius, struct gl_strided_buffer *strided_buf){
    long inum;
    int i, j, nd;
    
    double xyzw_line[8];
    double dir_line[8], norm_line[8];
    double color_line[8];
    
    set_black_color_c(&color_line[0]);
    for(nd=0;nd<4;nd++){color_line[nd+4] = color_line[nd];}

    inum = 0;
    for(j=0; j<NUM_P+1; j++){
        for(i=0; i<N_CURVE; i++){
            set_meridional_frame(i, radius, theta_p_grid, phi_p_grid[j], 
                                 xyzw_line, dir_line, norm_line);
            inum = set_tube_strided_buffer(inum, ncorner, tube_radius, 
                                           xyzw_line, dir_line, color_line,
                                           strided_buf);
        }
    }
    
    for(j=0; j<NUM_T-1; j++){
        for(i=0; i<N_CURVE; i++){
            set_latitude_frame(i, radius, theta_t_grid[j], phi_t_grid, 
                                 xyzw_line, dir_line, norm_line);
            inum = set_tube_strided_buffer(inum, ncorner, tube_radius, 
                                           xyzw_line, dir_line, color_line,
                                           strided_buf);
        }
    }
    return inum;
}


long set_map_flame_line_to_buf(struct gl_strided_buffer *strided_buf){
    long inum;
	int i, j, nd;
	double rtp_flame[6], d_map_flame[4];

    double xyzw_line[8];
    double norm_line[8];
    double color_line[8];
    
    xyzw_line[2] = 0.002;
    xyzw_line[3] = 1.0;

    norm_line[0] = 0.0;
    norm_line[1] = 0.0;
    norm_line[2] = 0.0;
    norm_line[3] = 1.0;

    set_black_color_c(&color_line[0]);
    
    for(nd=0;nd<4;nd++){
        xyzw_line[nd+4] =  xyzw_line[nd];
        norm_line[nd+4] =  norm_line[nd];
        color_line[nd+4] = color_line[nd];
    }

	inum = 0;
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	for(j=0; j<NUM_P+1; j++){
		rtp_flame[2] = phi_p_grid[j];
		rtp_flame[5] = phi_p_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[1] = theta_p_grid[i];
			rtp_flame[4] = theta_p_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
            xyzw_line[0] = d_map_flame[0];
            xyzw_line[1] = d_map_flame[1];
			
            xyzw_line[4] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
            xyzw_line[5] = 0.5 * (d_map_flame[1] + d_map_flame[3]);

            inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		rtp_flame[1] = theta_t_grid[j];
		rtp_flame[4] = theta_t_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[2] = phi_t_grid[i];
			rtp_flame[5] = phi_t_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
            xyzw_line[0] = d_map_flame[0];
            xyzw_line[1] = d_map_flame[1];
            
            xyzw_line[4] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
            xyzw_line[5] = 0.5 * (d_map_flame[1] + d_map_flame[3]);

            inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
		}
	}
	return inum;
}



long count_coastline_buf(void){
	return (long) get_nedge_coastline();
};

long set_coastline_line_buf(double radius, struct gl_strided_buffer *strided_buf){
	int iedge, k, nd;
	long nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	
    double xyzw_line[8];
    double norm_line[8];
    double color_line[8];
    
    set_black_color_c(&color_line[0]);
    for(nd=0;nd<4;nd++){color_line[nd+4] = color_line[nd];}


	nedge_coast = get_nedge_coastline();

    long inum = 0;
	for (iedge=0; iedge<nedge_coast;iedge++) {
		get_coastline(iedge, tp_coast, lake);
		
		for (k = 0; k < 2; k++) {
            norm_line[4*k  ] = cos(tp_coast[2*k]) * cos(tp_coast[2*k+1]);
            norm_line[4*k+1] = cos(tp_coast[2*k]) * sin(tp_coast[2*k+1]);
            norm_line[4*k+2] = sin(tp_coast[2*k]);
            norm_line[4*k+3]  = 1.0;
		};
        for(nd = 0;nd < 8; nd++){xyzw_line[nd] = radius * norm_line[nd];};
        xyzw_line[3] = 1.0;
        xyzw_line[7] = 1.0;
        inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
	};
	return nedge_coast;
}

long set_coastline_tube_buf(double radius, struct gl_strided_buffer *strided_buf){
	int iedge, k, nd;
	long nedge_coast;
	double tp_coast[4], lake[2];
	
    double xyzw_line[8];
    double norm_line[8], dir_line[8];
    double color_line[8];
    
    set_black_color_c(&color_line[0]);
    for(nd=0;nd<4;nd++){color_line[nd+4] = color_line[nd];}


	nedge_coast = get_nedge_coastline();

    long inum = 0;
	for (iedge=0; iedge<nedge_coast;iedge++) {
		get_coastline(iedge, tp_coast, lake);
		
		for (k = 0; k < 2; k++) {
            norm_line[4*k  ] = cos(tp_coast[2*k]) * cos(tp_coast[2*k+1]);
            norm_line[4*k+1] = cos(tp_coast[2*k]) * sin(tp_coast[2*k+1]);
            norm_line[4*k+2] = sin(tp_coast[2*k]);
            norm_line[4*k+3] = 1.0;
		};
        for(nd = 0;nd < 8; nd++){xyzw_line[nd] = radius * norm_line[nd];};
        xyzw_line[3] = 1.0;
        xyzw_line[7] = 1.0;
        inum = set_line_strided_buffer(inum, xyzw_line, dir_line, color_line, strided_buf);
	};
	return nedge_coast;
}

long set_map_coastline_line_buf(struct gl_strided_buffer *strided_buf){
	int iedge, k, nd;
	long nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	double rtp_flame[6], xy_coast[4];
	double pi = TWO * acos(ZERO);
	
    double xyzw_line[8];
    double norm_line[8];
    double color_line[8];
    
    xyzw_line[2] = 0.002;
    xyzw_line[3] = 1.0;

    norm_line[0] = 0.0;
    norm_line[1] = 0.0;
    norm_line[2] = 0.0;
    norm_line[3] = 1.0;

    set_black_color_c(&color_line[0]);
    
    for(nd=0;nd<4;nd++){
        xyzw_line[nd+4] =  xyzw_line[nd];
        norm_line[nd+4] =  norm_line[nd];
        color_line[nd+4] = color_line[nd];
    }

	set_black_color_c(f_color);
	
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;

    nedge_coast = get_nedge_coastline();
    long inum = 0;
	for (iedge = 0; iedge < nedge_coast; iedge++) {
		get_coastline(iedge, tp_coast, lake);
		for (k = 0; k < 2; k++) {
			rtp_flame[3*k+1] = -tp_coast[2*k  ]+HALF*pi;
			rtp_flame[3*k+2] =  tp_coast[2*k+1]+pi;
		};
		aitoff_c(ITWO, rtp_flame, xy_coast);
		for (k = 0; k < 2; k++) {
            xyzw_line[4*k  ] = xy_coast[2*k  ];
            xyzw_line[4*k+1] = xy_coast[2*k+1];
		};
        inum = set_line_strided_buffer(inum, xyzw_line, norm_line, color_line, strided_buf);
	};
	return nedge_coast;
}

