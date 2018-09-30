/*
//  FieldControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "FieldControlTableview.h"

@implementation FieldControlTableview

-(void)linkToFieldclist
{
    load_MHD_control_c();
    mhd_ctl_m = link_to_mhd_ctl();
    
    all_fld_tbl = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(all_fld_tbl);

    load_field_w_qflag_from_ctl(mhd_ctl_m->model_ctl->fld_ctl, all_fld_tbl);
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToFieldclist];
    
    usedFieldCtlTableview = [[UsedFieldControlTableview alloc] init];
    [usedFieldCtlTableview linkToFieldclist:all_fld_tbl];
    [usedFieldCtlTableview initMutablearray];    
    [usedFieldCtlTableview createMutablearray];
    [usedFieldCtlTableview createFieldView:usedFieldTableViewOutlet];
    
    unusedFieldCtlTableview = [[UnusedFieldControlTableview alloc] init];
    [unusedFieldCtlTableview linkToFieldclist:all_fld_tbl];
    [unusedFieldCtlTableview initMutablearray];    
    [unusedFieldCtlTableview createMutablearray];
    [unusedFieldCtlTableview createFieldView:unUsedFieldTableViewOutlet];
} // end awakeFromNib


- (IBAction)addAtSelectedRow:(id)pId
{
    [unusedFieldCtlTableview addUsedField];
    [usedFieldCtlTableview createMutablearray];
    [usedFieldCtlTableview updateUsedFieldTable];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    [usedFieldCtlTableview deleteUsedField];
    [unusedFieldCtlTableview createMutablearray];
    [unusedFieldCtlTableview updateUnusedFieldTable];
}

@end

