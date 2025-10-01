/*
//  Int2ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_int2_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface Int2ControlTableview : NSObject {
    NSMutableDictionary * int2ControlDictionary;
    NSMutableArray * int2ControlArray;
    NSTableView * int2TableView;
    NSNumberFormatter * integerFormatter;
    IBOutlet NSView *int2TableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    
    struct int2_clist * int2CtlList;
}
@property(strong) NSMutableDictionary * int2ControlDictionary;
@property(strong) NSMutableArray * int2ControlArray;
@property(strong) NSTableView * int2TableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToIntRealclist;

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
