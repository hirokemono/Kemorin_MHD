//
//  MyTableController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 2018/09/05.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import "MyDataObject.h"


@interface MyTableController : NSObject {
    NSMutableDictionary * mutableDictionary;
    NSMutableArray * mutableDataArray;
    NSTableView * myTableView ;
    IBOutlet NSView *tableViewPlaceholderView;
}
@property(strong) NSMutableDictionary * mutableDictionary;
@property(strong) NSMutableArray * mutableDataArray;
@property(strong) NSTableView * myTableView;

- (void)awakeFromNib;
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(NSInteger)rowIndex;
- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender;
- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex;
- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

@end
