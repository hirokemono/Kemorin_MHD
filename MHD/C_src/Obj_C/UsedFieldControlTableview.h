/*
//  UsedFieldControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "load_MHD_control_c.h"

@interface UsedFieldControlTableview : NSObject {
    NSMutableDictionary * FieldControlDictionary;
    NSMutableArray * FieldControlArray;
    NSTableView * FieldTableView;
    NSNumberFormatter * integerFormatter;
    
    NSString *key1;
    NSString *key2;
    NSString *key3;
    NSString *key4;
    NSString *key0;
    
    struct SGS_MHD_control_c * mhd_ctl_m;
    struct all_field_ctl_c *all_fld_list;
}
@property(strong) NSMutableDictionary * FieldControlDictionary;
@property(strong) NSMutableArray * FieldControlArray;
@property(strong) NSTableView * FieldTableView;

@property(strong) NSString * key1;
@property(strong) NSString * key2;
@property(strong) NSString * key3;
@property(strong) NSString * key4;
@property(strong) NSString * key0;

-(void)linkToFieldclist:(struct all_field_ctl_c *) ref_all_fld_list;
-(void)initMutablearray;
-(void)createMutablearray;
-(void)createFieldView:(NSView *)usedFieldTableViewOutlet;
-(void)updateUsedFieldTable;
- (void) deleteUsedField;

@end

