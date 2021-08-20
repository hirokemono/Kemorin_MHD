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
@synthesize eyeAngle;

@synthesize Quilt_flag;
@synthesize NumberOfRows;
@synthesize NumberOfColumns;
@synthesize NumberOfQuilts;

- (int) ToggleQuiltMode
{
    kemoview_set_quilt_nums(ISET_QUILT_MODE, (int) self.Quilt_flag);
    return self.Quilt_flag;
}
- (IBAction) SetQuiltRawByInput:(id)sender
{
    kemoview_set_quilt_nums(ISET_QUILT_RAW, (int) self.NumberOfRows);
}
- (IBAction) SetQuiltColumnByInput:(id)sender
{
    kemoview_set_quilt_nums(ISET_QUILT_COLUMN, (int) self.NumberOfColumns);
}
- (IBAction) SetEyeSeparationAngleByInput:(id)sender
{
    kemoview_set_stereo_parameter(ISET_EYEAGL, (double) self.eyeAngle);
    self.eyeRatio= (CGFloat) kemoview_get_view_parameter(ISET_EYESEP, 0);
}
- (IBAction) SetEyeSeparationDistanceByInput:(id)sender
{
    kemoview_set_stereo_parameter(ISET_EYESEP, (double) self.eyeRatio);
    self.eyeAngle= (CGFloat) kemoview_get_view_parameter(ISET_EYEAGL, 0);
}
- (IBAction) SetFoculPointDistanceByInput:(id)sender
{
    kemoview_set_stereo_parameter(ISET_FOCUS, (double) self.FocusPoint);
}

- (IBAction) SetProjectionAperture:(id)sender
{
    kemoview_set_view_parameter(ISET_APERTURE, 0, (double) self.ProjentionAperture);
}

- (IBAction) SetViewScaleFactor:(id)sender
{
    kemoview_set_view_parameter(ISET_SCALE, 0, (double) self.ScaleFactor);
}
- (IBAction) SetViewPointX:(id)sender
{
    kemoview_set_view_parameter(ISET_SHIFT, 0, (double) (-self.ViewPointX));
}
- (IBAction) SetViewPointY:(id)sender
{
    kemoview_set_view_parameter(ISET_SHIFT, 1, (double) (-self.ViewPointY));
}
- (IBAction) SetViewPointZ:(id)sender
{
    kemoview_set_view_parameter(ISET_SHIFT, 2, (double) (-self.ViewPointZ));
}
- (IBAction) SetViewRotationAxisX:(id)sender
{
    kemoview_set_view_parameter(ISET_ROTATE, 1, (double) self.RotationAxisX);
}
- (IBAction) SetViewRotationAxisY:(id)sender
{
    kemoview_set_view_parameter(ISET_ROTATE, 2, (double) self.RotationAxisY);
}
- (IBAction) SetViewRotationAxisZ:(id)sender
{
    kemoview_set_view_parameter(ISET_ROTATE, 3, (double) self.RotationAxisZ);
}
- (IBAction) SetViewRotationAngle:(id)sender
{
    kemoview_set_view_parameter(ISET_ROTATE, 0, (double) self.RotationAngle);
}


- (void) SetViewByInpit
{

}

- (void) UpdateParameters
{
	self.xPixel = kemoview_get_view_integer(ISET_PIXEL_X);
	self.yPixel = kemoview_get_view_integer(ISET_PIXEL_Y);
	
	self.ViewPointX = (CGFloat) -kemoview_get_view_parameter(ISET_SHIFT, 0);
	self.ViewPointY = (CGFloat) -kemoview_get_view_parameter(ISET_SHIFT, 1);
	self.ViewPointZ = (CGFloat) -kemoview_get_view_parameter(ISET_SHIFT, 2);
	
	self.LookPointX = (CGFloat)  kemoview_get_view_parameter(ISET_VWPOINT, 0);
	self.LookPointY = (CGFloat)  kemoview_get_view_parameter(ISET_VWPOINT, 1);
	self.LookPointZ = (CGFloat)  kemoview_get_view_parameter(ISET_VWPOINT, 2);
	
	self.ScaleFactor = (CGFloat) kemoview_get_view_parameter(ISET_SCALE, 0);
	
	self.RotationAxisX = (CGFloat) kemoview_get_view_parameter(ISET_ROTATE, 1);
	self.RotationAxisY = (CGFloat) kemoview_get_view_parameter(ISET_ROTATE, 2);
	self.RotationAxisZ = (CGFloat) kemoview_get_view_parameter(ISET_ROTATE, 3);
	self.RotationAngle = (CGFloat) kemoview_get_view_parameter(ISET_ROTATE, 0);
	
	self.ProjentionAperture = (CGFloat) kemoview_get_view_parameter(ISET_APERTURE, 0);
	self.ProjentionAspect =   (CGFloat) kemoview_get_view_parameter(ISET_ASPECT, 0);
	self.ProjentionNear =     (CGFloat) kemoview_get_view_parameter(ISET_NEAR, 0);
	self.ProjentionFar =      (CGFloat) kemoview_get_view_parameter(ISET_FAR, 0);
	
	self.FocusPoint =     (CGFloat) kemoview_get_view_parameter(ISET_FOCUS, 0);
	self.eyeRatio=        (CGFloat) kemoview_get_view_parameter(ISET_EYESEP, 0);
    self.eyeAngle=        (CGFloat) kemoview_get_view_parameter(ISET_EYEAGL, 0);

    self.Quilt_flag=      (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_MODE);
    self.NumberOfRows=    (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_RAW);
    self.NumberOfColumns= (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_COLUMN);
    self.NumberOfQuilts=  (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_NUM);
}

// given a delta time in seconds and current rotation accel, velocity and position, update overall object rotation
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime
{
    double dt = deltaTime;
	kemoview_animation_add_rotation(dt);
	[self UpdateParameters];
}
@end
