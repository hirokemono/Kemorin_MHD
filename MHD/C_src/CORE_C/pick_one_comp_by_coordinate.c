/*
//  pick_one_comp_by_coordinate.c
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include "pick_one_comp_by_coordinate.h"



void vector_in_xyz(const int id_coordinate,
                     float theta, float phi,
                     float *vector, float *v_xyz){
    if      (id_coordinate == SPH_COORDINATE){
        one_xyz_vector_from_sph(theta, phi, vector, v_xyz);
    }else if(id_coordinate == CYL_COORDINATE){
        one_xyz_vector_from_cyl(phi, vector, v_xyz);
    }else{
        v_xyz[0] = vector[0];
        v_xyz[1] = vector[1];
        v_xyz[2] = vector[2];
    };
    return;
}

float pick_from_vector(const int id_coordinate,
                       const int id_pick,
                       float theta, float phi,
                       float *vector){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_one_sph_vector(id_pick, theta, phi, 
                                        vector[0], vector[1], vector[2]);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_one_cyl_vector(id_pick, theta, phi,
                                        vector[0], vector[1], vector[2]);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_one_xyz_vector(id_pick, theta, phi,
                                        vector[0], vector[1], vector[2]);
    };
    return data;
}

float pick_from_sym_tensor(const int id_coordinate,
                           const int id_pick,
                           float theta, float phi,
                           float *sym_t){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_one_sph_sym_tensor(id_pick, theta, phi, 
                                            sym_t[0], sym_t[1], sym_t[2],
                                            sym_t[3], sym_t[4], sym_t[5]);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_one_cyl_sym_tensor(id_pick, theta, phi,
                                            sym_t[0], sym_t[1], sym_t[2],
                                            sym_t[3], sym_t[4], sym_t[5]);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_one_cyl_sym_tensor(id_pick, theta, phi,
                                            sym_t[0], sym_t[1], sym_t[2],
                                            sym_t[3], sym_t[4], sym_t[5]);
    };
    return data;
}

float pick_from_asym_tensor(const int id_coordinate,
                            const int id_pick,
                            float theta, float phi,
                            float *asym_t){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_one_sph_asym_tensor(id_pick, theta, phi, 
                                             asym_t[0], asym_t[1], asym_t[2]);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_one_cyl_asym_tensor(id_pick, theta, phi, 
                                             asym_t[0], asym_t[1], asym_t[2]);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_one_xyz_asym_tensor(id_pick, theta, phi, 
                                             asym_t[0], asym_t[1], asym_t[2]);
    };
    return data;
}
