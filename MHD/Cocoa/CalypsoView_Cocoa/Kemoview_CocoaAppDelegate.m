//
//  Kemoview_CocoaAppDelegate.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "Kemoview_CocoaAppDelegate.h"


@implementation Kemoview_CocoaAppDelegate

- (id) init{
    [_singleKemoView init];
    return self;
}
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application 

	// Set windowDidMove as a responder for the NSWindowDidMoveNotification
	[[NSNotificationCenter defaultCenter]
	 addObserver:self
	 selector:@selector(Tako:) name:NSWindowDidResizeNotification
	 object:nil];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication{
	return YES;
}

- (void)Tako:(NSNotification *)notification {
/*
    NSInteger tag = [[_SurfGrpNodeColorItem selectedCell] tag];
	[_metalView UpdateImage:kemo_sgl];

 // show window position in TextField
	NSString *st = [NSString stringWithFormat:@"(x, y)=(%.1f, %.1f), (h ,w)=(%.1f, %.1f)",
					self.window.frame.origin.x,      self.window.frame.origin.y,                    self.window.frame.size.width,                  self.window.frame.size.height];
//	[self.windowPosition　setStringValue: st];
	// show window position (in Log)
	NSLog (@"window position: %.1f, %.1f", self.window.frame.origin.x, self.window.frame.origin.y);
	NSLog (@"window size    : %.1f, %.1f", self.window.frame.size.width, self.window.frame.size.height);    
*/
}

- (IBAction)newDocument:(id)sender
{
//	NSLog(@"newDocument in Kemoview_CocoaAppDelegate:");
	
	// "WinController"インスタンスを生成する。
	[[KemoviewDragDropWindow alloc] init];
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
    [_ElasticControl UpdateWindow:0];
    return NSTerminateNow;
}

@end
