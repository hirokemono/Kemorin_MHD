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


@interface KemoViewerOpenGLView : NSOpenGLView {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet ResetViewControll *  _resetview;

	IBOutlet NSWindow*  mainWindow;
	
	NSOpenGLContext * _context;
    int id_window;
}

- (int) getViewSize;

- (void) UpdateImage;
- (void) QuickUpdateImage;
- (void) swapbuffer_cocoa;

- (void) messageTimer:(NSTimer *)timer;

- (void) prepareKemoOpenGL;
- (void) awakeFromNib;
@end
