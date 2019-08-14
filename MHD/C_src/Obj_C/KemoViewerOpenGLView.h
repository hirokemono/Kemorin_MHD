//
//  KemoViewerOpenGLView.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <OpenGL/gl.h>
#import <OpenGL/glext.h>
#import <OpenGL/glu.h>

#import "ResetViewControll.h"
#import "SetCocoaGLMessases.h"

#include "kemoviewer.h"

#define PAN     2
#define ROTATE  3

typedef struct {
	GLdouble x,y,z;
} recVec;

typedef struct {
	recVec viewPos; // View position
	recVec viewDir; // View direction vector
	GLdouble aperture; // pContextInfo->camera aperture
} recCamera;



@interface KemoViewerOpenGLView : NSOpenGLView {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet ResetViewControll*  _resetview;
	IBOutlet SetCocoaGLMessases* _cocoaGLMessages;
	
	NSOpenGLContext * _context;
	
	// image buffer
	NSBitmapImageRep *bmpRep;
	// string textures
	
	NSTimer* timer;
	
    bool fAnimate;
    bool fResizing;
	bool fDrawCaps;
    int id_window;
	
	CFAbsoluteTime time;

	// left (second) bottun handling
	NSInteger leftBottunFlag;
	
	IBOutlet NSProgressIndicator *evolutionProgreessBar;
	IBOutlet NSProgressIndicator *FlineEvolutionProgreessBar;
	float	_evolutionProgressValue;
}

- (void) setRetinaMode;
- (int) getViewSize;
- (GLint) KemoviewHorizontalViewSize;
- (GLint) KemoviewVerticalViewSize;

- (void) updateProjection;
- (void) updateModelView;
- (void) resizeGL;
- (void) modify_view_Cocoa;
- (void) UpdateImage;
- (void) swapbuffer_cocoa;

- (void) animationTimer:(NSTimer *)timer;

- (void) setViewerType:(NSInteger) selected;
- (void) Resetview;

-(void) setAnimate:(NSInteger)flag;
-(void) setInfo:(NSInteger)flag;
-(void) setQuickHelp:(NSInteger)flag;

-(id) DrawEvolution:(NSInteger)timeStep;
-(id) DrawRotation: (NSInteger) int_degree : (NSInteger) rotationaxis;

- (void)keyDown:(NSEvent *)theEvent;

- (void) mouseDown:(NSEvent *)theEvent;
- (void) rightMouseDown:(NSEvent *)theEvent;
- (void) otherMouseDown:(NSEvent *)theEvent;
- (void) mouseUp:(NSEvent *)theEvent;
- (void) rightMouseUp:(NSEvent *)theEvent;
- (void) otherMouseUp:(NSEvent *)theEvent;
- (void) mouseDragged:(NSEvent *)theEvent;
- (void) scrollWheel:(NSEvent *)theEvent;
- (void) rightMouseDragged:(NSEvent *)theEvent;
- (void) otherMouseDragged:(NSEvent *)theEvent;
- (void) magnifyWithEvent:(NSEvent *)theEvent;

- (BOOL) acceptsFirstResponder;
- (BOOL) becomeFirstResponder;
- (BOOL) resignFirstResponder;

- (void) prepareOpenGL:(int) iflag_core_profile;
- (void) awakeFromNib;

@end
