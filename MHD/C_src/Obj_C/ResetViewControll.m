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
	kemoview_set_windowsize((int) self.xPixel, (int) self.yPixel);

	kemoview_set_rotation_parameter(1, (double) self.RotationAxisX);
	kemoview_set_rotation_parameter(2, (double) self.RotationAxisY);
	kemoview_set_rotation_parameter(3, (double) self.RotationAxisZ);
	kemoview_set_rotation_parameter(0, (double) self.RotationAngle);

	kemoview_set_shift_vector(0, (double) (-self.ViewPointX));
	kemoview_set_shift_vector(1, (double) (-self.ViewPointY));
	kemoview_set_shift_vector(2, (double) (-self.ViewPointZ));
	kemoview_set_scale_factor((double) self.ScaleFactor);
	kemoview_set_projection_aperture((double) self.ProjentionAperture);
    
	kemoview_set_stereo_parameter((double) self.FocusPoint, (double) self.eyeRatio);	
}

- (void) UpdateParameters
{
	self.xPixel = kemoview_get_windowsize_x();
	self.yPixel = kemoview_get_windowsize_y();
	
	self.ViewPointX = (CGFloat) -kemoview_get_shift_vector(0);
	self.ViewPointY = (CGFloat) -kemoview_get_shift_vector(1);
	self.ViewPointZ = (CGFloat) -kemoview_get_shift_vector(2);
	
	self.LookPointX = (CGFloat) kemoview_get_lookat_vector(0);
	self.LookPointY = (CGFloat) kemoview_get_lookat_vector(1);
	self.LookPointZ = (CGFloat) kemoview_get_lookat_vector(2);
	
	self.ScaleFactor = (CGFloat) kemoview_get_scale_factor();
	
	self.RotationAxisX = (CGFloat) kemoview_get_rotation_parameter(1);
	self.RotationAxisY = (CGFloat) kemoview_get_rotation_parameter(2);
	self.RotationAxisZ = (CGFloat) kemoview_get_rotation_parameter(3);
	self.RotationAngle = (CGFloat) kemoview_get_rotation_parameter(0);
	
	self.ProjentionAperture = (CGFloat) kemoview_get_projection_aperture();
	self.ProjentionAspect =   (CGFloat) kemoview_get_projection_aspect();
	self.ProjentionNear =     (CGFloat) kemoview_get_projection_near();
	self.ProjentionFar =      (CGFloat) kemoview_get_projection_far();
	
	self.FocusPoint =     (CGFloat) kemoview_get_stereo_focus();
	self.eyeRatio=        (CGFloat) kemoview_get_stereo_eyeseparation();
	
}

// given a delta time in seconds and current rotation accel, velocity and position, update overall object rotation
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime
{
    double dt = deltaTime;
	kemoview_animation_add_rotation(dt);
	[self UpdateParameters];
}
@end
