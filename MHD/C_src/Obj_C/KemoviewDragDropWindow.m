//
//  KemoviewDragDropWindow.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/03/03.
//  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewDragDropWindow.h"


@implementation KemoviewDragDropWindow

@synthesize kemoview_s;

- (id)init
{
//	self = [super init];
	
//	if (self) {
//        kemoview_allocate_viwewer_struct(kemoview_s, IZERO);
        id_window = kemoview_get_current_viewer_id();
        
//        NSLog(@"init KemoviewDragDropWindow %d", id_window);
//		[NSBundle loadNibNamed:@"Kemoviewer" owner:self];
//    }
	
	return(self);
}

- (void) awakeFromNib
{
    [self registerForDraggedTypes:[NSArray arrayWithObjects:
                                   NSFilenamesPboardType, nil]];
};

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender
{
	NSPasteboard *pbd = [sender draggingPasteboard];
    
    if ( [[pbd types] containsObject:NSFilenamesPboardType] ) {
        NSArray *filearray = [pbd propertyListForType:NSFilenamesPboardType];
 //       int numberOfFiles = [filearray count];
        NSString *filename = [filearray objectAtIndex:0];

        [self setAlphaValue:1.0];
        [_kemoviewIOControl OpenKemoviewerFile:filename];
   };
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
//	NSLog(@"Window %d is closing", id_window);
//    kemoview_set_single_viewer_id(id_window);
//	[self release];
}

- (void)windowDidBecomeKey:(NSNotification *)aNotofication;
{
//	NSLog(@"Window %d is front", id_window);
//    kemoview_set_single_viewer_id(id_window);
}
@end
