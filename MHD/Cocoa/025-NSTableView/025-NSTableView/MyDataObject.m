//
//  MyDataObject.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 2018/09/05.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import "MyDataObject.h"

@implementation MyDataObject
@synthesize nsStrName1;
@synthesize nsStrName2;
@synthesize nsStrName3;

- (id)initWithString1:(NSString *)pStr1 andString2:(NSString *)pStr2 
           andString3:(NSString *)pStr3 {
    if (! (self = [super init])) {
        NSLog(@"MyDataObject **** ERROR : [super init] failed ***");
        return self;
    } // end if
    
    self.nsStrName1 = pStr1;
    self.nsStrName2 = pStr2;
    self.nsStrName3 = pStr3;
    
    return self;
    
} // end initWithString1:andString2:andString3:
@end

