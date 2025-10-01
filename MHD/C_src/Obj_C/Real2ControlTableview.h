/*
//  Real2ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_real2_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface Real2ControlTableview : NSObject {
    NSMutableDictionary * real2ControlDictionary;
    NSMutableArray * real2ControlArray;
    NSTableView * real2TableView;
    NSNumberFormatter * numberFormatter;
    IBOutlet NSView *real2TableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    
    struct real2_clist * Real2CtlList;
}
@property(strong) NSMutableDictionary * real2ControlDictionary;
@property(strong) NSMutableArray * real2ControlArray;
@property(strong) NSTableView * real2TableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToReal2clist;

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
