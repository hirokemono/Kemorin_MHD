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
	NSOpenGLContext * _context;
    int id_window;
}

- (void) swapbuffer_cocoa;
- (void) awakeFromNib;
@end
