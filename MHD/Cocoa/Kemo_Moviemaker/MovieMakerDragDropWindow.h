//
//  MovieMakerDragDropWindow.h
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 2018/02/16.
//
//

#ifndef MovieMakerDragDropWindow_h
#define MovieMakerDragDropWindow_h

#import <Cocoa/Cocoa.h>
#import "KemoMovieControll.h"

@interface KemoMovieDragDropWindow : NSWindow {
    IBOutlet KemoMovieControll* _kemoMovieIOControl;
    int id_window;
}


- (id)init;
- (void) awakeFromNib;
- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender;

- (unsigned int)draggingEntered:(id <NSDraggingInfo>)sender;
- (void)draggingExited:(id <NSDraggingInfo>)sender;

- (void)windowWillClose:(NSNotification *)aNotofication;
- (void)windowDidBecomeKey:(NSNotification *)aNotofication;
@end


#endif /* MovieMakerDragDropWindow_h */
