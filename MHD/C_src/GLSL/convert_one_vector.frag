/***********************************************************************
!!  convert_one_vector.frag
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
!!*
!!*      vr =  vx*sin(th)*cos(phi) + vy*sin(th)*sin(sin) + vz*cos(th)
!!*      vt =  vx*cos(th)*cos(phi) + vy*cos(th)*sin(phi) - vz*sin(th)
!!*      vp = -vx*sin(phi) +         vy*cos(phi)
!!*
!!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!!*      vz =  vr*cos(th) -          vt*sin(th)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vs =  vx*cos(phi) + vy*sin(phi)
!!*      vp = -vx*sin(phi) + vy*cos(phi)
!!*      vz =             vz
!!
!!*      vx =  vs*cos(phi) - vp*sin(phi)
!!*      vy =  vs*sin(phi) + vp*cos(phi)
!!*      vz =             vz
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vr =  vs*sin(th) + vz*cos(th)
!!*      vt =  vs*cos(th) - vz*sin(th)
!!*      vp =             vp
!!*
!!*      vs =  vr*sin(th) + vt*cos(th)
!!*      vp =                          vp
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
 ***********************************************************************/

/* vector components from cartecian coordinate to spherical coordinate */

float r_vector_from_xyz(float theta, float phi,
                        float v_x, float v_y, float v_z){
    float v_r =  v_x * sin(theta)*cos(phi)
               + v_y * sin(theta)*sin(phi)
               + v_z * cos(theta);
    return v_r;
};

float theta_vector_from_xyz(float theta, float phi,
                            float v_x, float v_y, float v_z){
    float v_t =  v_x * cos(theta) * cos(phi)
               + v_y * cos(theta) * sin(phi)
               - v_z * sin(phi);
    return v_t;
};

float phi_vector_from_xyz(float phi,
                          float v_x, float v_y){
    float v_p = -v_x * sin(phi)
               + v_y * cos(phi);
    return v_p;
};

/*  vector components from cartecian coordinate to cylindrical coordinate */

float s_vector_from_xyz(float phi, 
                        float v_x, float v_y){
    float v_s =  v_x * cos(phi)
               + v_y * sin(phi);
    return v_s;
};


/* vector components from spherical coordinate to cartecian coordinate */

float x_vector_from_sph(float theta, float phi, 
                        float v_r, float v_t, float v_p){
    float v_x =  v_r * sin(theta) * cos(phi)
               + v_t * cos(theta) * cos(phi)
               - v_p * sin(phi);
    return v_x;
};

float y_vector_from_sph(float theta, float phi, 
                        float v_r, float v_t, float v_p){
    float v_y =  v_r * sin(theta) * sin(phi)
               + v_t * cos(theta) * sin(phi)
               + v_p * cos(phi);
    return v_y;
};

float z_vector_from_sph(float theta, 
                        float v_r, float v_t){
    float v_z =  v_r * cos(theta)
               - v_t * sin(theta);
    return v_z;
};

/* vector components from cylindrical coordinate to cartecian coordinate */

float x_vector_from_cyl(float phi, 
                        float v_s, float v_p){
    float v_x =  v_s * cos(phi)
               - v_p * sin(phi);
    return v_x;
};

float y_vector_from_cyl(float phi, 
                        float v_s, float v_p){
    float v_y =  v_s * sin(phi)
               + v_p * cos(phi);
    return v_y;
};


/* vector components from spherical coordinate to cylindrical coordinate */

float r_vector_from_cyl(float theta, float v_s, float v_z){
    float v_r =  v_s * sin(theta)
               + v_z * cos(theta);
    return v_r;
};

float theta_vector_from_cyl(float theta, float v_s, float v_z){
    float v_t =  v_s * cos(theta)
               - v_z * sin(theta);
    return v_t;
};

/* vector components from cylindrical coordinate to spherical coordinate */

float s_vector_from_sph(float theta, float v_r, float v_t){
    float v_s =  v_r * sin(theta)
               + v_t * cos(theta);
    return v_s;
};

