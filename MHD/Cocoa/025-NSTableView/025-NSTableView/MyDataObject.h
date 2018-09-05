//
//  MyDataObject.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 2018/09/05.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>

@interface MyDataObject : NSObject {
    NSString *nsStrName1;
    NSString *nsStrName2;
    NSString *nsStrName3;
}
@property (copy) NSString *nsStrName1;
@property (copy) NSString *nsStrName2;
@property (copy) NSString *nsStrName3;

- (id)initWithString1:(NSString *)pStr1 andString2:(NSString *)pStr2
           andString3:(NSString *)pStr3;

@end

