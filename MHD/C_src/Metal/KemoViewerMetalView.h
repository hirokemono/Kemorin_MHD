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
#import "ResetViewControll.h"

#define PAN     2
#define ROTATE  3

@interface KemoViewerMetalView : MTKView{
    IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerObject     * _singleKemoView;
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

    int id_window;
}
@property int id_window;

-(void)InitBackGroundColor;
-(void) updateBackground;

- (void) setRetinaMode;
- (void) setViewerSize;

- (NSUInteger) getHorizontalViewSize;
- (NSUInteger) getVerticalViewSize;

- (void)initMessageTimer;
- (void)messageTimer:(NSTimer *)timer;

-(void) UpdateImage;
-(void) FastUpdateImage;
-(void) QuickUpdateImage;

-(id) DrawRotation: (NSInteger) int_degree : (NSInteger) rotationaxis;
-(id) DrawQuilt: (NSInteger) int_degree : (NSInteger) rotationaxis;
-(id) DrawEvolution:(NSInteger)timeStep;

- (void) updateProjection;
- (void) Resetview;

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

