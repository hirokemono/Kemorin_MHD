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
#import "KemoViewerOpenGLView.h"

#define PAN     2
#define ROTATE  3

@interface KemoViewerMetalView : MTKView{
    IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerObject * _singleKemoView;
    IBOutlet KemoViewerOpenGLView*  _kemoviewer;

    GLboolean _gDolly;
    GLboolean _gPan;
    GLboolean _gTrackball;
    double _gDollyPanStartPoint[2];
    NSInteger _leftBottunFlag;
}

-(void)InitBackGroundColor;
-(void) updateBackground;

-(void) UpdateImage;
-(void) QuickUpdateImage;

- (void) setRetinaMode;

- (void)mouseDown:(NSEvent *)theEvent;


@end

