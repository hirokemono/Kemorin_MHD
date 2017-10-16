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
    kemoview_allocate_single_viwewer_struct(single_kemoview);
    return self;
}
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application 
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication{
	return YES;
}

- (IBAction)newDocument:(id)sender
{
	NSLog(@"newDocument in Kemoview_CocoaAppDelegate:");
	
	// "WinController"インスタンスを生成する。
	[[KemoviewDragDropWindow alloc] init];
}
@end
