//
//  ResetViewControll.m
//  Cocoa OpenGL
//
//  Created by Hiroaki Matsui on 10/10/06.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "ResetViewControll.h"


@implementation ResetViewControll

@synthesize ViewPointX;
@synthesize ViewPointY;
@synthesize ViewPointZ;

@synthesize LookPointX;
@synthesize LookPointY;
@synthesize LookPointZ;

@synthesize ScaleFactor;

@synthesize RotationAxisX;
@synthesize RotationAxisY;
@synthesize RotationAxisZ;
@synthesize RotationAngle;

@synthesize ProjentionAperture;
@synthesize ProjentionAspect;
@synthesize ProjentionNear;
@synthesize ProjentionFar;

@synthesize FocusPoint;
@synthesize eyeRatio;
- (void) SetViewByInpit;
{
	GLdouble tmpShift[3], tmpRotation[4];
	
	tmpShift[0] = -(GLdouble) self.ViewPointX;
	tmpShift[1] = -(GLdouble) self.ViewPointY;
	tmpShift[2] = -(GLdouble) self.ViewPointZ;
	
	tmpRotation[1] = (GLdouble) self.RotationAxisX;
	tmpRotation[2] = (GLdouble) self.RotationAxisY;
	tmpRotation[3] = (GLdouble) self.RotationAxisZ;
	tmpRotation[0] = (GLdouble) self.RotationAngle;	
    
	set_kemoview_rotation_parameter(tmpRotation);
	set_kemoview_shift_vector(tmpShift);
	set_kemoview_scale_factor((GLdouble) self.ScaleFactor);
	set_kemoview_projection_aperture((GLdouble) self.ProjentionAperture);
    
	set_kemoview_stereo_parameter((GLdouble) self.FocusPoint, (GLdouble) self.eyeRatio);
}

- (void) UpdateParameters
{
	GLdouble tmpShift[3], tmpLookPoint[3];
	GLdouble tmpScale, tmpRotation[4];
	GLdouble tmpAperture, tmpNear;
	GLdouble tmpFar, tmpAspect;
	GLdouble tmpFocus, tmpEyeRatio;
	
	send_kemoview_rotation_parameter(tmpRotation);
	send_kemoview_shift_vector(tmpShift);
	send_kemoview_lookat_vector(tmpLookPoint);
	tmpScale = send_kemoview_scale_factor();
	send_kemoview_projection_parameters(&tmpAperture, &tmpNear,
										&tmpFar, &tmpAspect);
	tmpFocus = send_kemoview_stereo_parameters();
	tmpEyeRatio = send_kemoview_stereo_eyeseparation();
	
	self.ViewPointX = -tmpShift[0];
	self.ViewPointY = -tmpShift[1];
	self.ViewPointZ = -tmpShift[2];
	
	self.LookPointX = tmpLookPoint[0];
	self.LookPointY = tmpLookPoint[1];
	self.LookPointZ = tmpLookPoint[2];
	
	self.ScaleFactor = tmpScale;
	
	self.RotationAxisX = tmpRotation[1];
	self.RotationAxisY = tmpRotation[2];
	self.RotationAxisZ = tmpRotation[3];
	self.RotationAngle = tmpRotation[0];
	
	self.ProjentionAperture = tmpAperture;
	self.ProjentionAspect =   tmpAspect;
	self.ProjentionNear =     tmpNear;
	self.ProjentionFar =      tmpFar;
	
	self.FocusPoint =     tmpFocus;
	self.eyeRatio=        tmpEyeRatio;
}

// given a delta time in seconds and current rotation accel, velocity and position, update overall object rotation
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime
{
    GLdouble dt = deltaTime;
	add_kemoview_animation_rot(dt);
	[self UpdateParameters];
}

@end
