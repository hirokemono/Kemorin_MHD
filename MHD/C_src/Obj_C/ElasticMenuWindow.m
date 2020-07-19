//
//  ElasticMenuWindow.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 2020/07/10.
//

#import "ElasticMenuWindow.h"

@implementation ElasticMenuWindowController
    
#define MARGIN_RIGHT 25

- (void) UpdateWindow:(NSInteger)DrawPsfFlag
{
    NSRect window_frame = [_menu_window frame];
    CGFloat mainMenuWidth = _main_menu_view.frame.size.width;
    CGFloat PSFMenuWidth =  _PSF_menu_view.frame.size.width;

    window_frame.size.width = mainMenuWidth + MARGIN_RIGHT
                            + (PSFMenuWidth + MARGIN_RIGHT) * DrawPsfFlag;
    [_menu_window setFrame:window_frame display:YES animate:YES];
    printf("Size again: %d %d %d \n", (int) DrawPsfFlag, 
           (int) window_frame.size.width, (int) window_frame.size.height);
}

- (void)awakeFromNib
{
    CGFloat mainMenuWidth = _main_menu_view.frame.size.width;
    NSRect window_frame = [_menu_window frame];
    window_frame.size.width = mainMenuWidth + MARGIN_RIGHT;
    [_menu_window setFrame:window_frame display:YES animate:YES];
    printf("Size: %e %e \n", window_frame.size.width, window_frame.size.height);
//    [self update];
}
    
    
- (void)tabView:(NSTabView *)tabView didSelectTabViewItem:(NSView *)tabViewItem
{
//    [self update];    
}

@end
