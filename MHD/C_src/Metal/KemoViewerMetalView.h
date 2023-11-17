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
#import "ResetViewControll.h"

#define PAN     2
#define ROTATE  3

@interface KemoViewerMetalView : MTKView{
    IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerObject     * _singleKemoView;
    IBOutlet KemoViewerOpenGLView * _kemoviewer;
    IBOutlet ResetViewControll    * _resetview;

    int id_window;
    bool fDrawCaps;
    GLboolean gDolly;
    GLboolean gPan;
    GLboolean gTrackball;
    double gDollyPanStartPoint[2];
    NSInteger leftBottunFlag;
}
@property int id_window;

-(void)InitBackGroundColor;
-(void) updateBackground;

-(void) UpdateImage;
-(void) QuickUpdateImage;

- (void) setRetinaMode;
- (void) setViewerSize;

- (void) keyDown:(NSEvent *)theEvent;
- (void) magnifyWithEvent:(NSEvent *)theEvent;
- (void) scrollWheel:(NSEvent *)theEvent;
- (void) mouseDown:(NSEvent *)theEvent;
- (void) rightMouseDown:(NSEvent *)theEvent;
- (void) otherMouseDown:(NSEvent *)theEvent;
- (void) mouseUp:(NSEvent *)theEvent;
- (void) rightMouseUp:(NSEvent *)theEvent;
- (void) otherMouseUp:(NSEvent *)theEvent;
- (void) mouseDragged:(NSEvent *)theEvent;
- (void) rightMouseDragged:(NSEvent *)theEvent;
- (void) otherMouseDragged:(NSEvent *)theEvent;

- (BOOL) acceptsFirstResponder;
- (BOOL) becomeFirstResponder;
- (BOOL) resignFirstResponder;


@end

