//
//  ResetViewControll.h
//  Cocoa OpenGL
//
//  Created by Hiroaki Matsui on 10/10/06.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include "kemoviewer.h"


@interface ResetViewControll : NSObject {
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

- (void) SetViewByInpit;
- (void) UpdateParameters;
- (void) updateObjectRotationForTimeDelta:(CFAbsoluteTime)deltaTime;

@end
