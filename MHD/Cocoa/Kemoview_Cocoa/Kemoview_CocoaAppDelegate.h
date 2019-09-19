//
//  Kemoview_CocoaAppDelegate.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoviewDragDropWindow.h"
#include "kemoviewer.h"

@interface Kemoview_CocoaAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *_window;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
}

@property (assign) IBOutlet NSWindow *window;

- (id) init;
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication;
@end
