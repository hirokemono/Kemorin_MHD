/*
//  UnusedFieldControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "load_MHD_control_c.h"

@interface UnusedFieldControlTableview : NSObject {
    NSMutableDictionary * baseFieldDictionary;
    NSMutableDictionary * forceFieldDictionary;
    NSMutableDictionary * energyFieldDictionary;
    NSMutableDictionary * sgsFieldDictionary;

    NSMutableArray * baseFieldArray;
    NSMutableArray * forceFieldArray;
    NSMutableArray * energyFieldArray;
    NSMutableArray * sgsFieldArray;
    
    NSMutableDictionary * baseParentsDictionary;
    NSMutableDictionary * forceParentsDictionary;
    NSMutableDictionary * energyParentsDictionary;
    NSMutableDictionary * sgsParentsDictionary;
    
    NSMutableDictionary * FieldControlDictionary;
    NSMutableArray * FieldControlArray;
    NSOutlineView * unusedFieldTableView;
    NSNumberFormatter * integerFormatter;
    
    NSString *key2;
    NSString *key1;
    NSString *key0;
    
    NSMutableDictionary *firstParent;
    NSMutableDictionary *secondParent;
    NSMutableArray *list;

    struct SGS_MHD_control_c * mhd_ctl_m;
    struct all_field_ctl_c *all_fld_list;
}
@property(strong) NSMutableDictionary * firstParent;
@property(strong) NSMutableDictionary * secondParent;
@property(strong) NSMutableArray * list;

@property(strong) NSMutableDictionary * baseFieldDictionary;
@property(strong) NSMutableDictionary * forceFieldDictionary;
@property(strong) NSMutableDictionary * energyFieldDictionary;
@property(strong) NSMutableDictionary * sgsFieldDictionary;

@property(strong) NSMutableArray * baseFieldArray;
@property(strong) NSMutableArray * forceFieldArray;
@property(strong) NSMutableArray * energyFieldArray;
@property(strong) NSMutableArray * sgsFieldArray;

@property(strong) NSMutableDictionary * baseParentsDictionary;
@property(strong) NSMutableDictionary * forceParentsDictionary;
@property(strong) NSMutableDictionary * energyParentsDictionary;
@property(strong) NSMutableDictionary * sgsParentsDictionary;

@property(strong) NSMutableDictionary * FieldControlDictionary;
@property(strong) NSMutableArray * FieldControlArray;
@property(strong) NSOutlineView * unusedFieldTableView;

@property(strong) NSString * key2;
@property(strong) NSString * key1;
@property(strong) NSString * key0;

-(void)linkToFieldclist:(struct all_field_ctl_c *) ref_all_fld_list;
-(void)initMutablearray;
-(void)createMutablearray;
-(void)createFieldView:(NSView *) unUsedFieldTableViewOutlet;
-(void)updateUnusedFieldTable;

- (void)addUsedField;

@end

