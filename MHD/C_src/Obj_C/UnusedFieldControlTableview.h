/*
//  UnusedFieldControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "kemosrc_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "load_MHD_control_c.h"

@interface UnusedFieldControlTableview : NSObject {
    NSTableView * FieldTableView;
    NSNumberFormatter * numberFormatter;

    NSMutableDictionary * unusedFieldControlDictionary;
    NSMutableArray * unusedFieldControlArray;
    NSTableView * unusedFieldTableView;
    IBOutlet NSView *unusedFieldTableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    NSString *key3;
    NSString *key4;
    NSString *key0;

    struct SGS_MHD_control_c * mhd_ctl_m;
    struct chara2_real_clist * Chara2RealCtlList;
    struct all_field_ctl_c **all_fld_tbl;
}
@property(strong) NSTableView * FieldTableView;

@property(strong) NSMutableDictionary * unusedFieldControlDictionary;
@property(strong) NSMutableArray * unusedFieldControlArray;
@property(strong) NSTableView * unusedFieldTableView;

@property(strong) NSString * key1;
@property(strong) NSString * key2;
@property(strong) NSString * key3;
@property(strong) NSString * key4;
@property(strong) NSString * key0;

-(void)linkToFieldclist;

- (void)awakeFromNib;
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(NSInteger)rowIndex;
- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender;
- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex;

@end
