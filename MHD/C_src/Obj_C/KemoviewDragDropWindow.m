//
//  KemoviewDragDropWindow.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/03/03.
//  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewDragDropWindow.h"


@implementation KemoviewDragDropWindow

- (id)init
{
	self = [super init];
	return(self);
}

- (void) awakeFromNib
{
    [self registerForDraggedTypes:[NSArray arrayWithObjects:
                                   NSPasteboardTypeFileURL, nil]];
};

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender
{
	NSPasteboard *pbd = [sender draggingPasteboard];
    NSArray<Class> *classes = @[[NSURL class]];
    NSDictionary *options = @{};
    NSArray<NSURL*> *files = [pbd readObjectsForClasses:classes options:options];
    for (NSURL *url in files)
    {
       NSString *filename = [url path];

       // TODO: do something with str.
        [self setAlphaValue:1.0];
        
        struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
        [_kemoviewIOControl OpenKemoviewerFile:filename
                                      kemoview:kemo_sgl];
    }
    return YES;
}

- (unsigned int)draggingEntered:(id <NSDraggingInfo>)sender
{
	[self makeKeyAndOrderFront:nil];
	[self setAlphaValue:0.7];
    [self displayIfNeeded];
    
    return NSDragOperationMove;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
	[self setAlphaValue:1.0];
    [self displayIfNeeded];
}

- (void)windowWillClose:(NSNotification *)aNotofication;
{
//	[self release];
}

- (void)windowDidBecomeKey:(NSNotification *)aNotofication;
{
//	NSLog(@"Window %d is front", id_window);
}
@end
