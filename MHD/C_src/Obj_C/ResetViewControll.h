//
//  ResetViewControll.h
//  Cocoa OpenGL
//
//  Created by Hiroaki Matsui on 10/10/06.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#import "KemoViewerObject.h"

#include "kemoviewer.h"


@interface ResetViewControll : NSObject {
    IBOutlet KemoViewerObject *_kmv;
    
    NSInteger xPixel;
	NSInteger yPixel;
	
	CGFloat ViewPointX;
	CGFloat ViewPointY;
	CGFloat ViewPointZ;
	
	CGFloat LookPointX;
	CGFloat LookPointY;
	CGFloat LookPointZ;
	
	CGFloat ScaleFactor;
	
	CGFloat RotationAxisX;
	CGFloat RotationAxisY;
	CGFloat RotationAxisZ;
	CGFloat RotationAngle;
	
	CGFloat ProjentionAperture;
	CGFloat ProjentionAspect;
	CGFloat ProjentionNear;
	CGFloat ProjentionFar;
	
	CGFloat FocusPoint;
	CGFloat eyeRatio;
    CGFloat eyeAngle;
    
    NSInteger Quilt_flag;
    NSInteger NumberOfRows;
    NSInteger NumberOfColumns;
    NSInteger NumberOfQuilts;
}
@property NSInteger xPixel;
@property NSInteger yPixel;

@property CGFloat ViewPointX;
@property CGFloat ViewPointY;
@property CGFloat ViewPointZ;

@property CGFloat LookPointX;
@property CGFloat LookPointY;
@property CGFloat LookPointZ;

@property CGFloat ScaleFactor;

@property CGFloat RotationAxisX;
@property CGFloat RotationAxisY;
@property CGFloat RotationAxisZ;
@property CGFloat RotationAngle;

@property CGFloat ProjentionAperture;
@property CGFloat ProjentionAspect;
@property CGFloat ProjentionNear;
@property CGFloat ProjentionFar;

@property CGFloat FocusPoint;
@property CGFloat eyeRatio;
@property CGFloat eyeAngle;

@property NSInteger Quilt_flag;
@property NSInteger NumberOfRows;
@property NSInteger NumberOfColumns;
@property NSInteger NumberOfQuilts;

- (void) initQuiltMode;
- (int) ToggleQuiltMode;

- (IBAction) SetQuiltRawByInput:(id)sender;
- (IBAction) SetQuiltColumnByInput:(id)sender;
- (IBAction) SetEyeSeparationAngleByInput:(id)sender;
- (IBAction) SetEyeSeparationDistanceByInput:(id)sender;
- (IBAction) SetFoculPointDistanceByInput:(id)sender;

- (IBAction) SetProjectionAperture:(id)sender;

- (IBAction) SetViewScaleFactor:(id)sender;
- (IBAction) SetViewPointX:(id)sender;
- (IBAction) SetViewPointY:(id)sender;
- (IBAction) SetViewPointZ:(id)sender;
- (IBAction) SetViewRotationAxisX:(id)sender;
- (IBAction) SetViewRotationAxisY:(id)sender;
- (IBAction) SetViewRotationAxisZ:(id)sender;
- (IBAction) SetViewRotationAngle:(id)sender;


- (void) SetViewByInpit;
- (void) UpdateParameters;
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime;

@end
