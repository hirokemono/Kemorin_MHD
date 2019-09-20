
/*  rotate_animation.c */

#include "rotate_animation.h"

void init_rot_animation(struct view_element *view_s){
	view_s->rVel_animate[0] = 0.3;
	view_s->rVel_animate[1] = 0.1;
	view_s->rVel_animate[2] = 0.2;
	
	view_s->rAccel_animate[0] = -0.003;
	view_s->rAccel_animate[1] =  0.005;
	view_s->rAccel_animate[2] = -0.004;
	return;
}

void reset_rot_animation(struct view_element *view_s){
	view_s->rRot_animate[0] = ZERO;
	view_s->rRot_animate[1] = ZERO;
	view_s->rRot_animate[2] = ZERO;
	return;
}


void add_animation_rotation(struct view_element *view_s, double dt){
    // update rotation based on vel and accel
	double AnimateRotation[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	double fVMax = ONE;
	short i;
    
	// do velocities
	for (i = 0; i < 3; i++) {
		view_s->rVel_animate[i] = view_s->rVel_animate[i]
				 + view_s->rAccel_animate[i] * 30.0*dt;
		
		if (view_s->rVel_animate[i] > fVMax) {
			view_s->rAccel_animate[i] = -view_s->rAccel_animate[i];
			view_s->rVel_animate[i] = fVMax;
		} else if (view_s->rVel_animate[i] < -fVMax) {
			view_s->rAccel_animate[i] = -view_s->rAccel_animate[i];
			view_s->rVel_animate[i] = -fVMax;
		}
		
		view_s->rRot_animate[i] = view_s->rRot_animate[i]
				 + view_s->rVel_animate[i] * 30.0*dt;
		
		while (view_s->rRot_animate[i] > 360.0)
			view_s->rRot_animate[i] = view_s->rRot_animate[i] - 360.0;
		while (view_s->rRot_animate[i] < -360.0)
			view_s->rRot_animate[i] = view_s->rRot_animate[i] + 360.0;
	}
	AnimateRotation[0] = view_s->rRot_animate[0];
	AnimateRotation[1] = ONE;
	AnimateRotation[2] = ZERO;
	AnimateRotation[3] = ZERO;
	addToRotationTrackball_c(AnimateRotation, view_s->rotation);
	AnimateRotation[0] = view_s->rRot_animate[1];
	AnimateRotation[1] = ZERO;
	AnimateRotation[2] = ONE;
	addToRotationTrackball_c(AnimateRotation, view_s->rotation);
	AnimateRotation[0] = view_s->rRot_animate[2];
	AnimateRotation[2] = ZERO;
	AnimateRotation[3] = ONE;
	addToRotationTrackball_c(AnimateRotation, view_s->rotation);
	return;
}

