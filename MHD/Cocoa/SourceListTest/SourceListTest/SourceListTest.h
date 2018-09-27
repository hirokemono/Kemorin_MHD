//
//  SourceListTest.h
//  SourceListTest
//
//  Created by Hiroaki Matsui on 2018/09/06.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#include "SourcelistItem.h"

@interface SourceListTest : NSObject
{
    IBOutlet NSView *sourcePlaceHolderView;
    IBOutlet NSView *containerView;
    IBOutlet NSOutlineView *sourceListOutlineView;
}
@property (assign) IBOutlet NSWindow *window;
@property (atomic, retain) NSMutableArray *sourceListItems;

@end
