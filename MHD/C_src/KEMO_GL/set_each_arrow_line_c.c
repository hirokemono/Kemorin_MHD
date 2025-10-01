/*
//  set_each_arrow_line_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "set_each_arrow_line_c.h"


static void set_certecian_viz_vector(int icomp, long inod, 
                                     int if_draw_viz,
                                     struct psf_data *psf_s,
                                     double v_xyz[3]){
    int nd;
    double v_tmp[3], x_rtp[3];
    
    for(nd=0; nd<3; nd++) v_tmp[nd] = psf_s->d_nod[inod*psf_s->ncomptot + icomp+nd];
    if(psf_s->id_coord[if_draw_viz]==1){
        position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
        sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
    } else if(psf_s->id_coord[if_draw_viz]==2){
        position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
        cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
    } else {
        for (nd=0; nd<3; nd++) v_xyz[nd] = v_tmp[nd];
    };
    return;
}

static void cal_tangencial_viz_vector(int ivect_tangential, 
                                      double norm_nod[4],
                                      double v_xyz[3]){
    int nd;
    if(ivect_tangential==TANGENTIAL_COMPONENT){
        for(nd=0; nd<3; nd++) {
            v_xyz[nd] = v_xyz[nd] - norm_nod[nd]
                    * (  v_xyz[0]*norm_nod[0]
                       + v_xyz[1]*norm_nod[1]
                       + v_xyz[2]*norm_nod[2]);
        };
    };
    return;
}

static void set_each_arrov_line(double scale_vect,
                                double v_xyz[3], double xyzw_viz[4],
                                double xyzw_line[8], double dir_line[8]){
	double ascale = ONE / scale_vect;
    for (int nd=0; nd<3; nd++){
        xyzw_line[nd  ] = xyzw_viz[nd];
        xyzw_line[nd+4] = xyzw_viz[nd] + v_xyz[nd]*ascale;
        dir_line[nd  ] =  v_xyz[nd];
        dir_line[nd+4] =  v_xyz[nd];
    };
    xyzw_line[3] = 1.0;
    xyzw_line[7] = 1.0;
    dir_line[3] = 1.0;
    dir_line[7] = 1.0;
    return;
}

void set_line_for_tracer_arrow(int icomp, long inod, 
                               struct psf_data *psf_s,
                               struct psf_menu_val *psf_m,
                               double xyzw_line[8], 
                               double dir_line[8]){
    double v_xyz[3];
    set_certecian_viz_vector(icomp, inod,
                             psf_m->if_draw_viz,
                             psf_s, v_xyz);
    set_each_arrov_line(psf_m->scale_vect, v_xyz, 
                        &psf_s->xyzw_viz[4*inod],
                        xyzw_line, dir_line);
    return;
}

void set_line_for_psf_arrow(int icomp, long inod, 
                            struct psf_data *psf_s,
                            struct psf_normals *psf_n,
                            struct psf_menu_val *psf_m,
                            double xyzw_line[8], 
                            double dir_line[8]){
    double v_xyz[3];
    set_certecian_viz_vector(icomp, inod,
                             psf_m->if_draw_viz,
                             psf_s, v_xyz);
    cal_tangencial_viz_vector(psf_m->ivect_tangential,
                              &psf_n->norm_nod[4*inod], v_xyz);
    set_each_arrov_line(psf_m->scale_vect, v_xyz, 
                        &psf_s->xyzw_viz[4*inod],
                        xyzw_line, dir_line);
    return;
}

