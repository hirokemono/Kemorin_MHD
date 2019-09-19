//
//  Kemoview_CocoaAppDelegate.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "Kemoview_CocoaAppDelegate.h"

struct kemoviewer_type *single_kemoview;

@implementation Kemoview_CocoaAppDelegate

- (id) init{
    single_kemoview = kemoview_allocate_single_viwewer_struct();
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
	[_kemoviewer UpdateImage];

	/*	
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
@end
