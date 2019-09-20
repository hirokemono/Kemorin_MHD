//
//  ResetViewControll.m
//  Cocoa OpenGL
//
//  Created by Hiroaki Matsui on 10/10/06.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "ResetViewControll.h"


@implementation ResetViewControll

@synthesize xPixel;
@synthesize yPixel;

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
	double tmpShift[3], tmpRotation[4];
	
	tmpShift[0] = -(double) self.ViewPointX;
	tmpShift[1] = -(double) self.ViewPointY;
	tmpShift[2] = -(double) self.ViewPointZ;
	
	tmpRotation[1] = (double) self.RotationAxisX;
	tmpRotation[2] = (double) self.RotationAxisY;
	tmpRotation[3] = (double) self.RotationAxisZ;
	tmpRotation[0] = (double) self.RotationAngle;	
    
	kemoview_set_windowsize((GLint) self.xPixel, (GLint) self.yPixel);
	kemoview_set_rotation_parameter(tmpRotation);
	kemoview_set_shift_vector(tmpShift);
	kemoview_set_scale_factor((double) self.ScaleFactor);
	kemoview_set_projection_aperture((double) self.ProjentionAperture);
    
	kemoview_set_stereo_parameter((double) self.FocusPoint, (double) self.eyeRatio);
	
}

- (void) UpdateParameters
{
	int tmpWidth, tmpheigh;
	double tmpLookPoint[3];
	double tmpShift[3];
	double tmpRotation[4];
	double tmpScale;
	double tmpAperture, tmpNear;
	double tmpFar, tmpAspect;
	double tmpFocus, tmpEyeRatio;
	
	kemoview_get_windowsize(&tmpWidth, &tmpheigh);
	kemoview_get_rotation_parameter(tmpRotation);
	kemoview_get_shift_vector(tmpShift);
	kemoview_get_lookat_vector(tmpLookPoint);
	tmpScale = kemoview_get_scale_factor();
	kemoview_get_projection_parameters(&tmpAperture, &tmpNear,
                                       &tmpFar, &tmpAspect);
	tmpFocus = kemoview_get_stereo_focus();
	tmpEyeRatio = kemoview_get_stereo_eyeseparation();

	self.xPixel = tmpWidth;
	self.yPixel = tmpheigh;
	
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
    double dt = deltaTime;
	kemoview_animation_add_rotation(dt);
	[self UpdateParameters];
}
@end
