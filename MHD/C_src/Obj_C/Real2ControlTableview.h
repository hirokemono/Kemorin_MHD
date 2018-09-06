/*
//  Real2ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "kemosrc_param_c.h"
#include "t_control_real2_IO.h"
#include "t_SGS_MHD_control_c.h"

@interface Real2ControlTableview : NSObject {
    NSMutableDictionary * real2ControlDictionary;
    NSMutableArray * real2ControlArray;
    NSTableView * real2TableView;
    NSNumberFormatter * numberFormatter;
    IBOutlet NSView *real2TableViewOutlet;
}
@property(strong) NSMutableDictionary * real2ControlDictionary;
@property(strong) NSMutableArray * real2ControlArray;
@property(strong) NSTableView * real2TableView;

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
