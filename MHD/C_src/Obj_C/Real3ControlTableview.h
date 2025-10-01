/*
//  Real3ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_real3_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface Real3ControlTableview : NSObject {
    NSMutableDictionary * real3ControlDictionary;
    NSMutableArray * real3ControlArray;
    NSTableView * real3TableView;
    NSNumberFormatter * numberFormatter;
    IBOutlet NSView *real3TableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    NSString *key3;

    struct real3_clist * Real3CtlList;
}
@property(strong) NSMutableDictionary * real3ControlDictionary;
@property(strong) NSMutableArray * real3ControlArray;
@property(strong) NSTableView * real3TableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;
@property(strong) NSString * key3;

-(void)linkToReal3clist;

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
