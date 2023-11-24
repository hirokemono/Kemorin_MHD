//
//  KemoViewerOpenGLView.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;
#import <OpenGL/gl3.h>
#import <OpenGL/glext.h>

#import "ResetViewControll.h"

#include <math.h>
#include "kemoviewer.h"

#define PAN     2
#define ROTATE  3

typedef struct {
	double x,y,z;
} recVec;

typedef struct {
	recVec viewPos; // View position
	recVec viewDir; // View direction vector
	double aperture; // pContextInfo->camera aperture
} recCamera;



@interface KemoViewerOpenGLView : NSOpenGLView {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet ResetViewControll *  _resetview;

	IBOutlet NSWindow*  mainWindow;
	
	NSOpenGLContext * _context;
	
	// image buffer
	NSBitmapImageRep *bmpRep;
	// string textures
	
	NSTimer* timer_anime;
    NSTimer* timer_quick;
    NSTimer* timer_msg;

	bool fDrawCaps;
    int iflag_resize;
    int iflag_fast;
    int id_window;
    float message_opacity;
	
    CFTimeInterval reftime_anime;
    CFTimeInterval reftime_quick;
    CFTimeInterval reftime_msg;

    CFTimeInterval time_anime;
    CFTimeInterval time_quick;
    CFTimeInterval time_msg;

	// left (second) bottun handling
	NSInteger leftBottunFlag;
	
	IBOutlet NSProgressIndicator *evolutionProgreessBar;
	IBOutlet NSProgressIndicator *FlineEvolutionProgreessBar;
	float	_evolutionProgressValue;
}

- (int) getViewSize;

- (void) resizeGL;
- (void) UpdateImage;
- (void) QuickUpdateImage;
- (void) swapbuffer_cocoa;

- (void) fullDrawTimer:(NSTimer *)timer;
- (void) messageTimer:(NSTimer *)timer;

- (void) prepareKemoOpenGL;
- (void) awakeFromNib;
@end
