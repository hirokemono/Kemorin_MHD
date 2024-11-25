/*
//  KemoViewerMetalView.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

#import "KemoViewerMetalView.h"

@implementation KemoViewerMetalView

-(void)InitBackGroundColor:(struct kemoviewer_type *) kemo_sgl
{
    float BgColor4f[4];
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
    
	kemoview_set_background_color(BgColor4f, kemo_sgl);
    return;
};

-(void) updateBackground:(struct kemoviewer_type *) kemo_sgl
{
    float bgcolor[4];
    [self InitBackGroundColor:kemo_sgl];
    kemoview_get_background_color(kemo_sgl, bgcolor);
    self.clearColor = MTLClearColorMake(bgcolor[0], bgcolor[1], bgcolor[2], bgcolor[3]);
    return;
}

- (void) setRetinaMode:(struct kemoviewer_type *) kemo_sgl
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    XpixelGLWindow = rectView.size.width;
    
    NSRect rectView_DISP = [self bounds];
    XpixelRectView = rectView_DISP.size.width;
    
    if(XpixelGLWindow > XpixelRectView){
        kemoview_set_retinamode(IONE, kemo_sgl);
    } else {
        kemoview_set_retinamode(IZERO, kemo_sgl);
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
- (void) resizeMetal:(struct kemoviewer_type *) kemo_sgl
{
    // ensure camera knows size changed
    int iflag_updated = [self getViewSize];
    if (iflag_updated != 0) {
        iflag_resize = 1;
        reftime_msg = CFAbsoluteTimeGetCurrent(); //reset time in all cases
        kemoview_update_projection_by_viewer_size(XpixelGLWindow, YpixelGLWindow,
                                                  XpixelRectView, YpixelRectView,
                                                  kemo_sgl);
        kemoview_set_message_opacity(1.0, kemo_sgl);
    } else{
    }
}

- (void) setViewerSize
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self setRetinaMode:kemo_sgl];
    [self resizeMetal:kemo_sgl];
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
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_message_opacity(message_opacity, kemo_sgl);
    [self UpdateImage:kemo_sgl];
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

-(void) UpdateImage:(struct kemoviewer_type *) kemo_sgl
{
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    [_resetview UpdateParameters];
    
    [self setNeedsDisplay: YES];
    return;
}

-(void) QuickUpdateImage
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    [_resetview UpdateParameters];

    [self setNeedsDisplay: YES];
    return;
}

-(void) TripleUpdateImage:(struct kemoviewer_type *) kemo_sgl
{
    kemoview_set_view_integer(ISET_DRAW_MODE, TRIPLE_UPDATE, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO,  kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    [_resetview UpdateParameters];
    
    [self setNeedsDisplay: YES];
    return;
}

-(id) DrawQuilt:(NSInteger) istep_quilt
         degree:(NSInteger) int_degree
           axis:(NSInteger) rotationaxis
       kemoview:(struct kemoviewer_type *) kemo_sgl
{
    kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (int) int_degree, kemo_sgl);
    kemoview_step_viewmatrix((int) istep_quilt, kemo_sgl);

    [self draw];
    return self;
}

// ---------------------------------
// updates the contexts model view matrix for object and camera moves
- (void) Resetview
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoviewer_reset_to_init_angle(kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
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
                kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW,
                                          [_kmv KemoViewPointer]);
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

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    if ([theEvent modifierFlags] & NSEventModifierFlagControl) // send to pan
        [self rightMouseDown:theEvent];
    else if ([theEvent modifierFlags] & NSEventModifierFlagShift) // send to dolly
        [self otherMouseDown:theEvent];
    else if(leftBottunFlag == PAN)
        [self rightMouseDown:theEvent];
    else {
//        NSRect rectView = [self convertRectToBacking:[self bounds]];
        NSRect rectView = [self bounds];
        NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
        double xmove = (double) location.x;
        double ymove = (double) (location.y - rectView.size.height);
        gDolly =     FALSE; // no dolly
        gPan =       FALSE; // no pan
        gTrackball = TRUE;
        
        kemoview_startTrackball(xmove, ymove, kemo_sgl);
//        gTrackingViewInfo = self;
    }
    [self TripleUpdateImage:kemo_sgl];
    return;
}
// ---------------------------------
- (void)rightMouseDown:(NSEvent *)theEvent // pan
{
    NSRect rectView = [self bounds];
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (location.y - rectView.size.height);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball(kemo_sgl);
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
    NSRect rectView = [self bounds];
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (rectView.size.height - location.y);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball(kemo_sgl);
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
/*        kemoview_drugging_addToRotationTrackball(kemo_sgl);*/
        [_resetview UpdateParameters];
    }
//    gTrackingViewInfo = NULL;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self UpdateImage:kemo_sgl];
}

// ---------------------------------
- (void)mouseDragged:(NSEvent *)theEvent
{
    NSRect rectView = [self bounds];
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double)  location.x;
    double ymove = (double) (rectView.size.height - location.y);

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    if(gTrackball) {
        kemoview_rollToTrackball (xmove, -ymove, kemo_sgl);
        kemoview_drugging_addToRotationTrackball(kemo_sgl);
        kemoview_startTrackball(xmove, -ymove, kemo_sgl);
    } else if (gDolly) {
        kemoview_mousedolly(gDollyPanStartPoint, xmove, ymove, kemo_sgl);
    } else if (gPan) {
        kemoview_mousepan(gDollyPanStartPoint, xmove, ymove, kemo_sgl);
    }
    [self QuickUpdateImage];
    return;
}

// ---------------------------------
- (void)scrollWheel:(NSEvent *)theEvent
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    float wheelDelta = [theEvent deltaX] + [theEvent deltaY] + [theEvent deltaZ];
    if (wheelDelta)
    {
        kemoview_zooming((double) wheelDelta, kemo_sgl);
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    double newScale = 200.0*[theEvent magnification];
    kemoview_zooming(newScale, kemo_sgl);
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
