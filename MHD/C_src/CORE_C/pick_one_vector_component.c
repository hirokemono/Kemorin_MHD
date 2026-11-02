/*
//  pick_one_vector_component.c
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include "pick_one_vector_component.h"

/* vector to cartecian coordinate */

void one_xyz_vector_from_sph(float theta, float phi, 
                             float *v_sph, float *v_xyz){
    v_xyz[0] = x_vector_from_sph(theta, phi, v_sph[0], v_sph[1], v_sph[2]);
    v_xyz[1] = y_vector_from_sph(theta, phi, v_sph[0], v_sph[1], v_sph[2]);
    v_xyz[2] = z_vector_from_sph(theta, v_sph[0], v_sph[1]);
    return;
}

void one_xyz_vector_from_cyl(float phi, float *v_cyl, float *v_xyz){
    v_xyz[0] = x_vector_from_cyl(phi, v_cyl[0], v_cyl[1]);
    v_xyz[1] = y_vector_from_cyl(phi, v_cyl[0], v_cyl[1]);
    v_xyz[2] = v_cyl[2];
    return;
}



/* select vector component from spherical coordinate */

float pick_from_one_sph_vector(const int id_pick,
                               float theta, float phi,
                               float v_r, float v_t, float v_p){
    float data = 0.0;
    if      (id_pick == SPH_to_R){
        data = v_r;
    }else if(id_pick == SPH_to_T){
        data = v_t;
    }else if(id_pick == SPH_to_P){
        data = v_p;

    }else if(id_pick == SPH_to_S){
        data = s_vector_from_sph(theta, v_r, v_t);
    }else if(id_pick == SPH_to_Z){
        data = z_vector_from_sph(theta, v_r, v_t);

    }else if(id_pick == SPH_to_X){
        data = x_vector_from_sph(theta, phi, v_r, v_t, v_p);
    }else if(id_pick == SPH_to_Y){
        data = y_vector_from_sph(theta, phi, v_r, v_t, v_p);
        
    }else if(id_pick == SPH_to_MAG){
        data = sqrt(v_r*v_r + v_t*v_t + v_p*v_p);
    }else{
        data = 0.0;
    }
    return data;
}


float pick_from_one_cyl_vector(const int id_pick,
                               float theta, float phi,
                               float v_s, float v_p, float v_z){
    float data = 0.0;
    if      (id_pick == CYL_to_S){
        data = v_s;
    }else if(id_pick == CYL_to_P){
        data = v_p;
    }else if(id_pick == CYL_to_Z){
        data = v_z;

    }else if(id_pick == CYL_to_R){
        data = r_vector_from_cyl(theta, v_s, v_z);
    }else if(id_pick == CYL_to_T){
        data = theta_vector_from_cyl(theta, v_s, v_z);

    }else if(id_pick == CYL_to_X){
        data = x_vector_from_cyl(phi, v_s, v_p);
    }else if(id_pick == CYL_to_Y){
        data = y_vector_from_cyl(phi, v_s, v_p);
        
    }else if(id_pick == CYL_to_MAG){
        data = sqrt(v_s*v_s + v_z*v_z + v_p*v_p);
    }else{
        data = 0.0;
    }
    return data;
}


float pick_from_one_xyz_vector(const int id_pick,
                               float theta, float phi,
                               float v_x, float v_y, float v_z){
    float data = 0.0;
    if      (id_pick == XYZ_to_X){
        data = v_x;
    }else if(id_pick == XYZ_to_Y){
        data = v_y;
    }else if(id_pick == XYZ_to_Z){
        data = v_z;

    }else if(id_pick == XYZ_to_R){
        data = r_vector_from_xyz(theta, phi,
                                 v_x, v_y, v_z);
    }else if(id_pick == XYZ_to_T){
        data = theta_vector_from_xyz(theta, phi,
                                     v_x, v_y, v_z);
   }else if(id_pick == XYZ_to_P){
        data = phi_vector_from_xyz(phi, v_x, v_y);

    }else if(id_pick == XYZ_to_S){
        data = s_vector_from_xyz(phi, v_x, v_y);
        
    }else if(id_pick == XYZ_to_MAG){
        data = sqrt(v_x*v_x + v_z*v_z + v_y*v_y);
    }else{
        data = 0.0;
    }
    return data;
}
