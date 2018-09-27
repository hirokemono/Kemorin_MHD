//
//  SourcelistItem.h
//  SourceListTest
//
//  Created by Hiroaki Matsui on 2018/09/06.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SourcelistItem : NSObject
{
    NSString *title;
    NSString *identifier; //This is required to differentiate if a Item is header/Group By object or a data item
    NSImage  *icon;       //This is required as an image placeholder for image representation for each item
    NSArray  *children; 
}

@property (nonatomic, retain)       NSString *title;
@property (nonatomic, retain)       NSString *identifier;
@property (nonatomic, retain)       NSImage  *icon;
@property (nonatomic, retain)       NSArray  *children;

//Convenience methods
+ (id)itemWithTitle:(NSString*)aTitle identifier:(NSString*)anIdentifier;
+ (id)itemWithTitle:(NSString*)aTitle identifier:(NSString*)anIdentifier icon:(NSImage*)anIcon;

- (BOOL)hasChildren;
- (BOOL)hasIcon;
@end

