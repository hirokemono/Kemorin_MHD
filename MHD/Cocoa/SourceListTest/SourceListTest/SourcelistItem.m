//
//  SourcelistItem.m
//  SourceListTest
//
//  Created by Hiroaki Matsui on 2018/09/06.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import "SourcelistItem.h"

@implementation SourcelistItem
@synthesize title;
@synthesize identifier;
@synthesize icon;
@synthesize children;

+ (id)itemWithTitle:(NSString*)aTitle identifier:(NSString*)anIdentifier{
    //convenience method for creating a Source List Item without an icon. Ideal for Group headers
    SourcelistItem *item = [SourcelistItem itemWithTitle:aTitle identifier:anIdentifier icon:nil];
    return item;
}

+ (id)itemWithTitle:(NSString*)aTitle identifier:(NSString*)anIdentifier icon:(NSImage*)anIcon
{
    SourcelistItem *item = [[SourcelistItem alloc] init];
    [item setTitle:aTitle];
    [item setIdentifier:anIdentifier];
    [item setIcon:anIcon];
    return item;
}

- (BOOL)hasChildren{
    return [children count]>0;
}

- (BOOL)hasIcon{
    return icon!=nil;
}

@end
