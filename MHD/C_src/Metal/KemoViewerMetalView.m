/*
//  KemoViewerMetalView.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

#import "KemoViewerMetalView.h"

@implementation KemoViewerMetalView

-(void)InitBackGroundColor
{
    float BgColor4f[4];
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	kemoview_set_background_color(BgColor4f);
    return;
};

-(void) updateBackground
{
    float bgcolor[4];
    [self InitBackGroundColor];
	kemoview_get_background_color(bgcolor);
    self.clearColor = MTLClearColorMake(bgcolor[0], bgcolor[1], bgcolor[2], bgcolor[3]);
    return;
}

- (void) setRetinaMode
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    XpixelGLWindow = rectView.size.width;
    
    NSRect rectView_DISP = [self bounds];
    XpixelRectView = rectView_DISP.size.width;
    
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
    
//    NSRect rectView = [self bounds];
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    
    // ensure camera knows size changed
    if ((YpixelGLWindow != rectView.size.height) ||
        (XpixelGLWindow != rectView.size.width)) {
        YpixelGLWindow = rectView.size.height;
        XpixelGLWindow = rectView.size.width;
        
//        printf("Pixel size %d, %d\n",XpixelGLWindow, YpixelGLWindow);
        
        iflag_updated = 1;
    }
    return iflag_updated;
}

/* handles resizing of GL need context update and if the window dimensions change */
- (void) resizeMetal
{
    // ensure camera knows size changed
    int iflag_updated = [self getViewSize];
    if (iflag_updated != 0) {
        iflag_resize = 1;
        reftime_msg = CFAbsoluteTimeGetCurrent(); //reset time in all cases
        kemoview_update_projection_by_viewer_size(XpixelGLWindow, YpixelGLWindow,
                                                  XpixelRectView, YpixelRectView);
        kemoview_set_message_opacity(1.0);
    } else{
    }
}

- (void) setViewerSize
{
    [self setRetinaMode];
    [self resizeMetal];
}

- (NSUInteger) getHorizontalViewSize
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    return rectView.size.width;
}

- (NSUInteger) getVerticalViewSize
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    return rectView.size.height;
}

// ---------------------------------
- (void)messageTimer:(NSTimer *)timer
{
    float message_opacity;
    if(iflag_resize == 0) return;

    CFTimeInterval deltaTime = CFAbsoluteTimeGetCurrent() - reftime_msg;
    if(deltaTime < 4.5){ // skip pauses
        message_opacity = log10f(10.0 - 2.0*deltaTime);
    } else {
        iflag_resize = 0;
        message_opacity = 0.0;
    }
    kemoview_set_message_opacity(message_opacity);
    [self UpdateImage];
    return;
}

- (void)initMessageTimer
{
    iflag_resize = 0;
    reftime_msg = CFAbsoluteTimeGetCurrent ();  // set animation time start time
    timer_msg = [NSTimer timerWithTimeInterval:(1.0f/2.0f)
                                        target:self
                                      selector:@selector(messageTimer:)
                                      userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:timer_msg
                                 forMode:NSDefaultRunLoopMode];
    [[NSRunLoop currentRunLoop] addTimer:timer_msg
                                 forMode:NSEventTrackingRunLoopMode]; // ensure timer fires during resize
}
// ---------------------------------

-(void) UpdateImage
{
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
    kemoview_mono_viewmatrix();
    [_resetview UpdateParameters];
    
    [self setNeedsDisplay: YES];
    return;
}
-(void) FastUpdateImage
{
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    [_resetview UpdateParameters];

    [self setNeedsDisplay: YES];
    return;
}
-(void) QuickUpdateImage
{
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
    kemoview_mono_viewmatrix();
    [_resetview UpdateParameters];

    [self setNeedsDisplay: YES];
    return;
}

-(id) DrawQuilt: (NSInteger) int_degree : (NSInteger)rotationaxis
{
    kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (int) int_degree);
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    kemoview_step_viewmatrix();

    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    [self setNeedsDisplay: YES];
    return self;
}

-(id) DrawEvolution:(NSInteger)timeStep
{
    kemoview_viewer_evolution((int) timeStep);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    kemoview_step_viewmatrix();
    [self setNeedsDisplay: YES];
    return self;
}

// ---------------------------------
// updates the contexts model view matrix for object and camera moves
- (void) Resetview
{
    kemoviewer_reset_to_init_angle();
    kemoview_mono_viewmatrix();
    [self setNeedsDisplay: YES];
}

// ---------------------------------
#pragma mark ---- IB Actions ----

-(void) setInfo:(NSInteger)flag
{
/*    [_cocoaGLMessages setDrawInfoFlag:flag]; */
    [self setNeedsDisplay: YES];
}

-(void) setQuickHelp:(NSInteger)flag
{
/*    [_cocoaGLMessages  setQuickHelpFlag:flag]; */
    [self setNeedsDisplay: YES];
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
                kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
                [self setNeedsDisplay: YES];
                break;
        }
    }
}

// ---------------------------------
- (void)mouseDown:(NSEvent *)theEvent // trackball
{
    gDolly =     FALSE;
    gPan =       FALSE;
    gTrackball = FALSE;

    if ([theEvent modifierFlags] & NSEventModifierFlagControl) // send to pan
        [self rightMouseDown:theEvent];
    else if ([theEvent modifierFlags] & NSEventModifierFlagShift) // send to dolly
        [self otherMouseDown:theEvent];
    else if(leftBottunFlag == PAN)
        [self rightMouseDown:theEvent];
    else {
        NSRect rectView = [self convertRectToBacking:[self bounds]];
        CGFloat YpixelGLWindow = rectView.size.height;
        NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
        double xmove = (double) location.x;
        double ymove = (double) (location.y - YpixelGLWindow);
        gDolly =     FALSE; // no dolly
        gPan =       FALSE; // no pan
        gTrackball = TRUE;
        kemoview_startTrackball(xmove, ymove);
//        gTrackingViewInfo = self;
    }
    [self QuickUpdateImage];
    return;
}
// ---------------------------------
- (void)rightMouseDown:(NSEvent *)theEvent // pan
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (location.y - YpixelGLWindow);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball();
    }
 */
    gDolly =     FALSE; // no dolly
    gPan =       TRUE;
    gTrackball = FALSE; // no trackball
    gDollyPanStartPoint[0] =  xmove;
    gDollyPanStartPoint[1] = -ymove;
//    gTrackingViewInfo = self;
}

// ---------------------------------
- (void)otherMouseDown:(NSEvent *)theEvent //dolly
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (YpixelGLWindow - location.y);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball();
    }
 */
    gDolly =     TRUE;
    gPan =       FALSE; // no pan
    gTrackball = FALSE; // no trackball
    gDollyPanStartPoint[0] =  xmove;
    gDollyPanStartPoint[1] =  ymove;
//    gTrackingViewInfo = self;
}

// ---------------------------------
- (void)mouseUp:(NSEvent *)theEvent
{
    if (gDolly) { // end dolly
        gDolly =     FALSE;
    } else if (gPan) { // end pan
        gPan =       FALSE;
    } else if (gTrackball) { // end trackball
        gTrackball = FALSE;
/*        kemoview_drugging_addToRotationTrackball();*/
        [_resetview UpdateParameters];
    }
//    gTrackingViewInfo = NULL;
    [self FastUpdateImage];
}

// ---------------------------------
- (void)mouseDragged:(NSEvent *)theEvent
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (YpixelGLWindow - location.y);
    if(gTrackball) {
        kemoview_rollToTrackball (xmove, -ymove);
        kemoview_drugging_addToRotationTrackball();
        kemoview_startTrackball(xmove, -ymove);
    } else if (gDolly) {
        kemoview_mousedolly(gDollyPanStartPoint, xmove, ymove);
    } else if (gPan) {
        kemoview_mousepan(gDollyPanStartPoint, xmove, ymove);
    }
    [self QuickUpdateImage];
    return;
}

// ---------------------------------
- (void)scrollWheel:(NSEvent *)theEvent
{
    float wheelDelta = [theEvent deltaX] + [theEvent deltaY] + [theEvent deltaZ];
    if (wheelDelta)
    {
        kemoview_zooming((double) wheelDelta);
    }
    [self QuickUpdateImage];
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
#pragma mark ---- IB Actions ----

- (void) setViewerType:(NSInteger) selected
{
    if(selected == VIEW_3D
       || selected == VIEW_STEREO){
        leftBottunFlag = ROTATE;
    }
    else{
        leftBottunFlag = PAN;
    };
}

// ---------------------------------

- (BOOL)acceptsFirstResponder{return YES;}
- (BOOL)becomeFirstResponder{return YES;}
- (BOOL)resignFirstResponder{return YES;}

@end
