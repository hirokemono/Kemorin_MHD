/*
//  FieldControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "kemosrc_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "load_MHD_control_c.h"
#include "UsedFieldControlTableview.h"
#include "UnusedFieldControlTableview.h"

@interface FieldControlTableview : NSObject {
    
    UsedFieldControlTableview * usedFieldCtlTableview;
    UnusedFieldControlTableview * unusedFieldCtlTableview;
    
    NSMutableDictionary * FieldControlDictionary;
    NSMutableArray * FieldControlArray;
    NSTableView * FieldTableView;
    NSNumberFormatter * integerFormatter;
    
    IBOutlet NSView *FieldTableViewOutlet;
    IBOutlet NSView *usedFieldTableViewOutlet;
    IBOutlet NSView *unUsedFieldTableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    NSString *key3;
    NSString *key4;
    NSString *key0;

    struct SGS_MHD_control_c * mhd_ctl_m;
    struct all_field_ctl_c **all_fld_tbl;
}
@property(strong) NSMutableDictionary * FieldControlDictionary;
@property(strong) NSMutableArray * FieldControlArray;
@property(strong) NSTableView * FieldTableView;

@property(strong) NSString * key1;
@property(strong) NSString * key2;
@property(strong) NSString * key3;
@property(strong) NSString * key4;
@property(strong) NSString * key0;

-(void)linkToFieldclist;

- (void)awakeFromNib;
- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

@end
