/***********************************************************************
!!  convert_one_vector_component.h
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
#ifndef CONVERT_ONE_VECTOR_COMPONENT_
#define CONVERT_ONE_VECTOR_COMPONENT_

#include <math.h>

/* prototypes */

/* vector components from cartecian coordinate to spherical coordinate */
float r_vector_from_xyz(float theta, float phi,
                        float v_x, float v_y, float v_z);
float theta_vector_from_xyz(float theta, float phi,
                            float v_x, float v_y, float v_z);
float phi_vector_from_xyz(float phi,
                          float v_x, float v_y);

/*  vector components from cartecian coordinate to cylindrical coordinate */
float s_vector_from_xyz(float phi,
                        float v_x, float v_y);


/* vector components from spherical coordinate to cartecian coordinate */
float x_vector_from_sph(float theta, float phi, 
                        float v_r, float v_t, float v_p);
float y_vector_from_sph(float theta, float phi, 
                        float v_r, float v_t, float v_p);
float z_vector_from_sph(float theta, 
                        float v_r, float v_t);

/* vector components from cylindrical coordinate to cartecian coordinate */
float x_vector_from_cyl(float phi, 
                        float v_s, float v_p);
float y_vector_from_cyl(float phi, 
                        float v_s, float v_p);

/* vector components from spherical coordinate to cylindrical coordinate */

float r_vector_from_cyl(float theta, float v_s, float v_z);
float theta_vector_from_cyl(float theta, float v_s, float v_z);

/* vector components from cylindrical coordinate to spherical coordinate */
float s_vector_from_sph(float theta, float v_r, float v_t);


#endif /* CONVERT_ONE_VECTOR_COMPONENT_ */
