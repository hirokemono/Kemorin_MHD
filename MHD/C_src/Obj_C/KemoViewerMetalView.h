/*
//KemoViewerMetalView.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

@import Cocoa;
@import MetalKit;

#import "AAPLRenderer.h"

#include "kemoviewer.h"

@interface KemoViewerMetalView : MTKView{
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
}

-(void) init;

-(void)InitBackGroundColor;
-(void) updateBackground;
@end

