/*
//KemoViewerMetalView.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

@import Cocoa;
@import MetalKit;

#import "KemoViewerRenderer.h"

#import "KemoViewerObject.h"
#import "ResetViewControll.h"
#import "KemoViewerObject.h"

#include "Kemoviewer.h"

#define PAN     2
#define ROTATE  3

@interface KemoViewerMetalView : MTKView{
    IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerObject     * _kmv;
    IBOutlet ResetViewControll    * _resetview;

    bool fDrawCaps;
    GLboolean gDolly;
    GLboolean gPan;
    GLboolean gTrackball;
    double gDollyPanStartPoint[2];
    NSInteger leftBottunFlag;
    
    int XpixelGLWindow;
    int YpixelGLWindow;
    int XpixelRectView;
    int YpixelRectView;
    
    int iflag_resize;
    NSTimer* timer_msg;

    CFTimeInterval reftime_msg;

//    int id_window;
}
// @property int id_window;

-(void) updateBackground:(struct kemoviewer_type *) kemo_sgl;

- (void) setViewerSize;

- (NSUInteger) getHorizontalViewSize;
- (NSUInteger) getVerticalViewSize;

- (void)initMessageTimer;
- (void)messageTimer:(NSTimer *)timer;

-(void) UpdateImage:(struct kemoviewer_type *) kemo_sgl;

-(id) DrawQuilt:(NSInteger) istep_quilt
         degree:(NSInteger) int_degree
           axis:(NSInteger) rotationaxis
       kemoview:(struct kemoviewer_type *) kemo_sgl;

-(void) Resetview;

-(void) setInfo:(NSInteger)flag;
-(void) setQuickHelp:(NSInteger)flag;

- (void) keyDown:(NSEvent *)theEvent;
- (void) magnifyWithEvent:(NSEvent *)theEvent;
- (void) scrollWheel:(NSEvent *)theEvent;
- (void) mouseDown:(NSEvent *)theEvent;
- (void) rightMouseDown:(NSEvent *)theEvent;
- (void) otherMouseDown:(NSEvent *)theEvent;
- (void) mouseUp:(NSEvent *)theEvent;
- (void) mouseDragged:(NSEvent *)theEvent;
- (void) rightMouseDragged:(NSEvent *)theEvent;
- (void) otherMouseDragged:(NSEvent *)theEvent;

- (void) setViewerType:(NSInteger) selected;


- (BOOL) acceptsFirstResponder;
- (BOOL) becomeFirstResponder;
- (BOOL) resignFirstResponder;


@end

