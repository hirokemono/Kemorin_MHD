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

- (void) initQuiltMode
{
    self.Quilt_flag = 0;
}
- (int) ToggleQuiltMode
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_quilt_nums(ISET_QUILT_MODE, (int) self.Quilt_flag, kemo_sgl);
    if(self.Quilt_flag > 0){
        self.NumberOfColumns = 5;
        self.NumberOfRows = 9;
        self.FocusPoint = 9.5;
        self.eyeAngle =   35.0;
        kemoview_set_quilt_nums(ISET_QUILT_RAW, (int) self.NumberOfRows, kemo_sgl);
        kemoview_set_quilt_nums(ISET_QUILT_COLUMN, (int) self.NumberOfColumns,
                                kemo_sgl);
        kemoview_set_stereo_parameter(ISET_FOCUS, (double) self.FocusPoint,
                                      kemo_sgl);
        kemoview_set_stereo_parameter(ISET_EYEAGL, (double) self.eyeAngle,
                                      kemo_sgl);
        self.eyeRatio= (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_EYESEP, 0);
    }
    return (int) self.Quilt_flag;
}
- (IBAction) SetQuiltRawByInput:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_quilt_nums(ISET_QUILT_RAW, (int) self.NumberOfRows, kemo_sgl);
}
- (IBAction) SetQuiltColumnByInput:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_quilt_nums(ISET_QUILT_COLUMN, (int) self.NumberOfColumns,
                            kemo_sgl);
}
- (IBAction) SetEyeSeparationAngleByInput:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_stereo_parameter(ISET_EYEAGL, (double) self.eyeAngle,
                                  kemo_sgl);
    self.eyeRatio= (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_EYESEP, 0);
}
- (IBAction) SetEyeSeparationDistanceByInput:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_stereo_parameter(ISET_EYESEP, (double) self.eyeRatio,
                                  kemo_sgl);
    self.eyeAngle= (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_EYEAGL, 0);
}
- (IBAction) SetFoculPointDistanceByInput:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_stereo_parameter(ISET_FOCUS, (double) self.FocusPoint,
                                  kemo_sgl);
}

- (IBAction) SetProjectionAperture:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_APERTURE, 0,
                                (double) self.ProjentionAperture, kemo_sgl);
}

- (IBAction) SetViewScaleFactor:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_SCALE, 0,
                                (double) self.ScaleFactor, kemo_sgl);
}
- (IBAction) SetViewPointX:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_SHIFT, 0,
                                (double) (-self.ViewPointX), kemo_sgl);
}
- (IBAction) SetViewPointY:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_SHIFT, 1,
                                (double) (-self.ViewPointY), kemo_sgl);
}
- (IBAction) SetViewPointZ:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_SHIFT, 2,
                                (double) (-self.ViewPointZ), kemo_sgl);
}
- (IBAction) SetViewRotationAxisX:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_ROTATE, 1,
                                (double) self.RotationAxisX, kemo_sgl);
}
- (IBAction) SetViewRotationAxisY:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_ROTATE, 2,
                                (double) self.RotationAxisY, kemo_sgl);
}
- (IBAction) SetViewRotationAxisZ:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_ROTATE, 3,
                                (double) self.RotationAxisZ, kemo_sgl);
}
- (IBAction) SetViewRotationAngle:(id)sender
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_parameter(ISET_ROTATE, 0,
                                (double) self.RotationAngle, kemo_sgl);
}


- (void) SetViewByInpit
{

}

- (void) UpdateParameters
{
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
	self.xPixel = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
	self.yPixel = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
	
	self.ViewPointX
        = (CGFloat) (-1.0 * kemoview_get_view_parameter(kemo_sgl, ISET_SHIFT, 0));
	self.ViewPointY
        = (CGFloat) (-1.0 * kemoview_get_view_parameter(kemo_sgl, ISET_SHIFT, 1));
	self.ViewPointZ
        = (CGFloat) (-1.0 * kemoview_get_view_parameter(kemo_sgl, ISET_SHIFT, 2));
	
	self.LookPointX
        = (CGFloat)  kemoview_get_view_parameter(kemo_sgl, ISET_VWPOINT, 0);
	self.LookPointY
        = (CGFloat)  kemoview_get_view_parameter(kemo_sgl, ISET_VWPOINT, 1);
	self.LookPointZ
        = (CGFloat)  kemoview_get_view_parameter(kemo_sgl, ISET_VWPOINT, 2);
	
	self.ScaleFactor
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_SCALE, 0);
	
	self.RotationAxisX
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_ROTATE, 1);
	self.RotationAxisY
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_ROTATE, 2);
	self.RotationAxisZ
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_ROTATE, 3);
	self.RotationAngle
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_ROTATE, 0);
	
	self.ProjentionAperture
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_APERTURE, 0);
	self.ProjentionAspect
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_ASPECT, 0);
	self.ProjentionNear
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_NEAR, 0);
	self.ProjentionFar
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_FAR, 0);
	
	self.FocusPoint
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_FOCUS, 0);
	self.eyeRatio
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_EYESEP, 0);
    self.eyeAngle
        = (CGFloat) kemoview_get_view_parameter(kemo_sgl, ISET_EYEAGL, 0);

    self.Quilt_flag
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE);
    self.NumberOfRows
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
    self.NumberOfColumns
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
    self.NumberOfQuilts
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_NUM);
}

// given a delta time in seconds and current rotation accel, velocity and position, update overall object rotation
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime
{
    double dt = deltaTime;
    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
	kemoview_animation_add_rotation(dt, kemo_sgl);
	[self UpdateParameters];
}
@end
