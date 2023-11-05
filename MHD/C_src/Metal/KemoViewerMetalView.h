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
#import "KemoViewerObject.h"

@interface KemoViewerMetalView : MTKView{
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerObject * _singleKemoView;
}

-(void)InitBackGroundColor;
-(void) updateBackground;
@end

