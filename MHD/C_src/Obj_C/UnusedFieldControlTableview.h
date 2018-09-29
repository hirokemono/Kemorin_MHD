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
    NSMutableDictionary * baseFieldDictionary;
    NSMutableDictionary * forceFieldDictionary;
    NSMutableDictionary * energyFieldDictionary;
    NSMutableDictionary * sgsFieldDictionary;

    NSMutableArray * FieldControlArray;
    NSTableView * FieldTableView;
    NSNumberFormatter * integerFormatter;
    
    NSString *key1;
    NSString *key0;
    
    struct SGS_MHD_control_c * mhd_ctl_m;
    struct all_field_ctl_c **all_fld_tbl;
}
@property(strong) NSMutableDictionary * baseFieldDictionary;
@property(strong) NSMutableDictionary * forceFieldDictionary;
@property(strong) NSMutableDictionary * energyFieldDictionary;
@property(strong) NSMutableDictionary * sgsFieldDictionary;

@property(strong) NSMutableArray * FieldControlArray;
@property(strong) NSTableView * FieldTableView;

@property(strong) NSString * key1;
@property(strong) NSString * key0;

-(void)linkToFieldclist:(struct all_field_ctl_c **) ref_all_fld_table;
-(void)initMutablearray;
-(void)createMutablearray;
-(void)createFieldView:(NSView *) unUsedFieldTableViewOutlet;
-(void)updateUnusedFieldTable;

- (void)addUsedField;

@end

