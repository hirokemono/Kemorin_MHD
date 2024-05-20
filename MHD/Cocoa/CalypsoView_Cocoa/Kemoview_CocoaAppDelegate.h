//
//  Kemoview_CocoaAppDelegate.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#import "KemoViewerObject.h"
#import "KemoviewDragDropWindow.h"
#import "KemoViewerMetalView.h"
#import "ElasticMenuWindow.h"

#include "kemoviewer.h"

@interface Kemoview_CocoaAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *_window;
    IBOutlet KemoViewerObject * _singleKemoView;
    IBOutlet KemoViewerMetalView * _metalView;
    
    IBOutlet ElasticMenuWindowController * _ElasticControl;
}

@property (assign) IBOutlet NSWindow *window;

- (id) init;
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication;

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;

@end
