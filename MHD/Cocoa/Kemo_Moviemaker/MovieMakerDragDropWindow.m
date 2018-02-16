//
//  MovieMakerDragDropWindow.m
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 2018/02/16.
//
//

#import <Foundation/Foundation.h>
#import "MovieMakerDragDropWindow.h"


@implementation KemoMovieDragDropWindow

- (id)init
{
    return(self);
}

- (void) awakeFromNib
{
    [self registerForDraggedTypes:[NSArray arrayWithObjects:
                                   NSFilenamesPboardType, nil]];
}


- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender
{
    NSPasteboard *pbd = [sender draggingPasteboard];
    
    if ( [[pbd types] containsObject:NSFilenamesPboardType] ) {
        NSArray *filearray = [pbd propertyListForType:NSFilenamesPboardType];
        //       int numberOfFiles = [filearray count];
        NSString *filename = [filearray objectAtIndex:0];
        
        [self setAlphaValue:1.0];
        [_kemoMovieIOControl OpenReferenceImageFile:filename];
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
