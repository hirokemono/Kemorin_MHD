/*
//  vector_tensor_select.frag
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#define XYZ_COORDINATE    0
#define SPH_COORDINATE    1
#define CYL_COORDINATE    2

/* select conversion by coordinate */

vec3 vector_in_xyz(uniform int id_coordinate,
                     uniform int id_pick,
                     float theta, float phi, 
                     vec3 vector){
    vec3 v_xyz = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        v_xyz = xyz_vector_from_sph(theta, phi, vector);
    }else if(id_coordinate == CYL_COORDINATE){
        v_xyz = xyz_vector_from_cyl(phi, vector);
    }else if(id_coordinate == XYZ_COORDINATE){
        v_xyz = vector;
    };
    return v_xyz;
}


float pick_from_vector(uniform int id_coordinate,
                       uniform int id_pick,
                       float theta, float phi, 
                       vec3 vector){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_sph_vector(id_pick, theta, phi, vector);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_cyl_vector(id_pick, theta, phi, vector);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_xyz_vector(id_pick, theta, phi, vector);
    };
    return data;
}

float pick_from_sym_tensor(uniform int id_coordinate,
                           uniform int id_pick,
                           float theta, float phi,
                           vec3 sym_t1, vec3 sym_t2){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_sph_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_cyl_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_xyz_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    };
    return data;
}

float pick_from_asym_tensor(uniform int id_coordinate,
                            uniform int id_pick,
                            float theta, float phi,
                            vec3 asym_t1){
    float data = 0.0;
    if      (id_coordinate == SPH_COORDINATE){
        data = pick_from_sph_asym_tensor(id_pick, theta, phi, asym_t1);
    }else if(id_coordinate == CYL_COORDINATE){
        data = pick_from_cyl_asym_tensor(id_pick, theta, phi, asym_t1);
    }else if(id_coordinate == XYZ_COORDINATE){
        data = pick_from_xyz_asym_tensor(id_pick, theta, phi, asym_t1);
    };
    return data;
}
