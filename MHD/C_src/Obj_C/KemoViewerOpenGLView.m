//
//  KemoViewerOpenGLView.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoViewerOpenGLView.h"

// ==================================

int XpixelGLWindow, YpixelGLWindow;
int XpixelRectView, YpixelRectView;

// single set of interaction flags and states
double gDollyPanStartPoint[2] = {0.0, 0.0};
GLboolean gDolly = GL_FALSE;
GLboolean gPan = GL_FALSE;
GLboolean gTrackball = GL_FALSE;
KemoViewerOpenGLView * gTrackingViewInfo = NULL;

@implementation KemoViewerOpenGLView
// ---------------------------------

// get number of pixels for OpenGL Window
- (void) setRetinaMode
{
    NSRect rectView_DISP = [self bounds];
	if ((XpixelRectView != rectView_DISP.size.height) ||
	    (YpixelRectView != rectView_DISP.size.width)) {
        YpixelRectView = rectView_DISP.size.height;
        XpixelRectView = rectView_DISP.size.width;
    }
    
    if(XpixelGLWindow > XpixelRectView){
        kemoview_set_retinamode(IONE);
    } else {
        kemoview_set_retinamode(IZERO);
    };
    return;
}

- (int) getViewSize
{
    int iflag_updated = 0;
    
//	NSRect rectView = [self bounds];
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    
	// ensure camera knows size changed
	if ((YpixelGLWindow != rectView.size.height) ||
	    (XpixelGLWindow != rectView.size.width)) {
        YpixelGLWindow = rectView.size.height;
        XpixelGLWindow = rectView.size.width;
        
        [self setRetinaMode];
//        printf("Pixel size %d, %d\n",XpixelGLWindow, YpixelGLWindow);
        
        iflag_updated = 1;
    }
    return iflag_updated;
}

- (int) KemoviewHorizontalViewSize
{
    return XpixelGLWindow;
}

- (int) KemoviewVerticalViewSize
{
    return YpixelGLWindow;
}

// update the projection matrix based on camera and view info
- (void) updateProjection
{
    [_context makeCurrentContext];
	// set projection
	kemoview_update_distance();
}

// ---------------------------------

// updates the contexts model view matrix for object and camera moves
- (void) updateModelView
{
    [_context makeCurrentContext];
	
	// move view
	kemoview_rotate();
	kemoview_reset_animation();
	[_resetview UpdateParameters];
}

// ---------------------------------

// handles resizing of GL need context update and if the window dimensions change, a
// a window dimension update, reseting of viewport and an update of the projection matrix
- (void) resizeGL
{
	// ensure camera knows size changed
    int iflag_updated = [self getViewSize];
	if (iflag_updated != 0) {
		
		kemoview_update_projection_by_viewer_size(XpixelGLWindow, YpixelGLWindow,
                                                  XpixelRectView, YpixelRectView);
        kemoview_set_windowsize_message(1);
        [_context makeCurrentContext];
		
		[self swapbuffer_cocoa];
    } else{
	    kemoview_set_windowsize_message(0);
    }
}

- (void) swapbuffer_cocoa{
	[_context makeCurrentContext];
	[_context flushBuffer];
	[self setNeedsDisplay:YES];
}

// ---------------------------------

// move camera in z axis
-(void)mouseDolly: (NSPoint) location
{
	kemoview_mousedolly(gDollyPanStartPoint, (double) location.x, (double) location.y);
}
	
// ---------------------------------
	
// move camera in x/y plane
- (void)mousePan: (NSPoint) location
{
	kemoview_mousepan(gDollyPanStartPoint, (double) location.x, (double) location.y);
}

// ---------------------------------

-(id) DrawRotation: (NSInteger) int_degree : (NSInteger)rotationaxis
{
	kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis);
	kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (int) int_degree);
    kemoview_set_single_viewer_id(id_window);
	kemoview_rotate();
	
	[self swapbuffer_cocoa];
	return self;
}

-(id) DrawEvolution:(NSInteger)timeStep
{	
    kemoview_set_single_viewer_id(id_window);
	kemoview_viewer_evolution((int) timeStep);
	kemoview_modify_view();
	
	[self swapbuffer_cocoa];
	return self;
}

-(void) UpdateImage
{
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
	kemoview_modify_view();
	[_resetview UpdateParameters];

	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
	return;
}

-(void) QuickUpdateImage
{
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
	kemoview_quick_view();
	[_resetview UpdateParameters];
	
	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
	return;
}

- (void) drawRectforError:(NSRect)rect
{		
	// setup viewport and prespective
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[self updateModelView];  // update model view matrix for object
	[self swapbuffer_cocoa];
}

// ---------------------------------

// per-window timer function, basic time based animation preformed here
- (void)animationTimer:(NSTimer *)timer
{
	BOOL shouldDraw = NO;
	if (fAnimate) {
		CFTimeInterval deltaTime = CFAbsoluteTimeGetCurrent () - time;
		if (deltaTime > 10.0) // skip pauses
			return;
		else {
			// if we are not rotating with trackball in this window
			if (!gTrackball || (gTrackingViewInfo != self)) {
				[_resetview updateObjectRotationForTimeDelta: deltaTime]; // update object rotation
			}
			shouldDraw = YES; // force redraw
		}
	}
	time = CFAbsoluteTimeGetCurrent (); //reset time in all cases
	// if we have current messages
	[self drawRectforError:[self bounds]]; // redraw now instead dirty to enable updates during live resize
}

// ---------------------------------
#pragma mark ---- IB Actions ----

- (void) setViewerType:(NSInteger) selected;
{
	if(selected == VIEW_3D
	   || selected == VIEW_STEREO){
		leftBottunFlag = ROTATE;
	}
	else{
		leftBottunFlag = PAN;
		fAnimate = 0;
	};
	kemoview_set_viewtype(selected);
}

-(void) setAnimate:(NSInteger)flag
{
	fAnimate = flag;
}

-(void) setInfo:(NSInteger)flag
{
//    [_cocoaGLMessages setDrawInfoFlag:flag];
	[self setNeedsDisplay: YES];
}

-(void) setQuickHelp:(NSInteger)flag
{
//	[_cocoaGLMessages  setQuickHelpFlag:flag];
	[self setNeedsDisplay: YES];
}

// ---------------------------------

- (void) Resetview
{
	kemoviewer_reset_to_init_angle();

	[self updateProjection];
	[self swapbuffer_cocoa];
}

// ---------------------------------

#pragma mark ---- Method Overrides ----

-(void)keyDown:(NSEvent *)theEvent
{
    NSString *characters = [theEvent characters];
    if ([characters length]) {
        unichar character = [characters characterAtIndex:0];
		switch (character) {
			case 'c':
				// toggle caps
				fDrawCaps = 1 - fDrawCaps;
				[self setNeedsDisplay: YES];
				break;
		}
	}
}

// ---------------------------------

- (void)mouseDown:(NSEvent *)theEvent // trackball
{
    if ([theEvent modifierFlags] & NSControlKeyMask) // send to pan
		[self rightMouseDown:theEvent];
	else if ([theEvent modifierFlags] & NSAlternateKeyMask) // send to dolly
		[self otherMouseDown:theEvent];
	else if(leftBottunFlag == PAN)
		[self rightMouseDown:theEvent];
	else {
		NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
		location.y = YpixelGLWindow - location.y;
		gDolly = GL_FALSE; // no dolly
		gPan = GL_FALSE; // no pan
		gTrackball = GL_TRUE;
		kemoview_startTrackball(location.x, -location.y);
		gTrackingViewInfo = self;
	}
	[self QuickUpdateImage];
}

// ---------------------------------

- (void)rightMouseDown:(NSEvent *)theEvent // pan
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
/*	
	if (gTrackball) { // if we are currently tracking, end trackball
		kemoview_drugging_addToRotationTrackball();
	}
 */
	gDolly = GL_FALSE; // no dolly
	gPan = GL_TRUE; 
	gTrackball = GL_FALSE; // no trackball
	gDollyPanStartPoint[0] = (double) location.x;
	gDollyPanStartPoint[1] = (double) location.y;
	gTrackingViewInfo = self;
}

// ---------------------------------

- (void)otherMouseDown:(NSEvent *)theEvent //dolly
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
/*	
	if (gTrackball) { // if we are currently tracking, end trackball
		kemoview_drugging_addToRotationTrackball();
	}
 */
	gDolly = GL_TRUE;
	gPan = GL_FALSE; // no pan
	gTrackball = GL_FALSE; // no trackball
	gDollyPanStartPoint[0] = (double) location.x;
	gDollyPanStartPoint[1] = (double) location.y;
	gTrackingViewInfo = self;
}

// ---------------------------------

- (void)mouseUp:(NSEvent *)theEvent
{
    kemoview_set_single_viewer_id(id_window);

	if (gDolly) { // end dolly
		gDolly = GL_FALSE;
	} else if (gPan) { // end pan
		gPan = GL_FALSE;
	} else if (gTrackball) { // end trackball
		gTrackball = GL_FALSE;
/*		kemoview_drugging_addToRotationTrackball();*/
		[_resetview UpdateParameters];
	} 
	gTrackingViewInfo = NULL;

	[self UpdateImage];
}

// ---------------------------------

- (void)rightMouseUp:(NSEvent *)theEvent
{
	[self mouseUp:theEvent];
}

// ---------------------------------

- (void)otherMouseUp:(NSEvent *)theEvent
{
	[self mouseUp:theEvent];
}

// ---------------------------------

- (void)mouseDragged:(NSEvent *)theEvent
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
	if (gTrackball) {
		kemoview_rollToTrackball (location.x, -location.y);
		kemoview_drugging_addToRotationTrackball();
		kemoview_startTrackball(location.x, -location.y);
	} else if (gDolly) {
		[self mouseDolly: location];
	} else if (gPan) {
		[self mousePan: location];
	}
	
	[self QuickUpdateImage];
}

// ---------------------------------

- (void)scrollWheel:(NSEvent *)theEvent
{
	float wheelDelta = [theEvent deltaX] + [theEvent deltaY] + [theEvent deltaZ];
	if (wheelDelta)
	{
		kemoview_zooming((double) wheelDelta);
		[self QuickUpdateImage];
	}
}

// ---------------------------------

- (void)rightMouseDragged:(NSEvent *)theEvent
{
	[self mouseDragged: theEvent];
}

// ---------------------------------

- (void)otherMouseDragged:(NSEvent *)theEvent
{
	[self mouseDragged: theEvent];
}

// ---------------------------------

- (void)magnifyWithEvent:(NSEvent *)theEvent
{
    double newScale = 200.0*[theEvent magnification];
    kemoview_zooming(newScale);
	[self QuickUpdateImage];
}

// ---------------------------------

- (BOOL)acceptsFirstResponder{return YES;}
- (BOOL)becomeFirstResponder{return YES;}
- (BOOL)resignFirstResponder{return YES;}

// ---------------------------------

// set initial OpenGL state (current context is set)
// called after context is created
- (void) prepareKemoOpenGL
{
	float BgColor4f[4];

	static const NSOpenGLPixelFormatAttribute attr[]= {
			NSOpenGLPFAOpenGLProfile, NSOpenGLProfileVersion3_2Core,
			NSOpenGLPFANoRecovery,
			NSOpenGLPFADoubleBuffer,
			//NSOpenGLPFAWindow,
			NSOpenGLPFAAccelerated,
			NSOpenGLPFAColorSize,    24,
			NSOpenGLPFAAlphaSize,     8,
			NSOpenGLPFADepthSize,    24,
			NSOpenGLPFAStencilSize,   8,
			NSOpenGLPFAMultisample,
//			NSOpenGLPFASampleBuffers, 1,
//			NSOpenGLPFASamples, 4, 
			0
	};
	NSOpenGLPixelFormat *pixelFormat = [[[NSOpenGLPixelFormat alloc] initWithAttributes:attr] autorelease];

	_context = [ self openGLContext ];
	[_context initWithFormat: pixelFormat shareContext: nil];
	[_context makeCurrentContext];

	fprintf(
			stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	// init GL stuff here
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
	
	glEnable(GL_CULL_FACE);
	glFrontFace(GL_CCW);
	glPolygonOffset (1.0f, 1.0f);
	
	int iflag_updates = [self getViewSize];
    [self setRetinaMode];
    
    if(iflag_updates != 0){
        kemoview_set_windowsize(XpixelGLWindow, YpixelGLWindow, 
                                XpixelRectView, YpixelRectView);
    };
		
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	kemoview_set_background_color(BgColor4f);

}

// ---------------------------------

- (void) awakeFromNib
{
    id_window = kemoview_get_current_viewer_id();
    kemoview_set_single_viewer_id(id_window);

	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	int anaglyphFlag = [[defaults stringForKey:@"AnaglyphFlag"] intValue];
	
	kemoview_set_view_integer(ISET_SHUTTER, SHUTTER_OFF);
	kemoview_set_view_integer(ISET_ANAGYLYPH, anaglyphFlag);
	
	[self prepareKemoOpenGL];
	
	kemoviewer_reset_to_init_angle();
	kemoview_init_lighting();

	// set start values...
	fAnimate =  0;
	
    [NSColor setIgnoresAlpha:NO];
	
	// start animation timer

	time = CFAbsoluteTimeGetCurrent ();  // set animation time start time
	timer = [NSTimer timerWithTimeInterval:(1.0f/60.0f) target:self selector:@selector(animationTimer:) userInfo:nil repeats:NO];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSEventTrackingRunLoopMode]; // ensure timer fires during resize

 }

@end
