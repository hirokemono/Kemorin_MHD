/*
//  ElasticMenuWindow.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 2020/07/10.
*/

#ifndef ElasticMenuWindow_h
#define ElasticMenuWindow_h

#import "KemoViewerObject.h"

@interface ElasticMenuWindowController : NSObject{
    IBOutlet KemoViewerObject *_kmv;
    
    IBOutlet NSView *_main_menu_view;
    IBOutlet NSView *_PSF_menu_view;
    IBOutlet NSWindow *_menu_window;
}

- (void) UpdateWindow:(NSInteger)DrawPsfFlag;

@end

#endif /* ElasticMenuWindow_h */
