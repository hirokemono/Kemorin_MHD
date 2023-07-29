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
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
    if(f_MHD_ctl == NULL){
        printf("malloc error for f_MHD_ctl\n");
        exit(0);
    };
    f_MHD_ctl->f_self = c_read_control_sph_SGS_MHD("control_MHD");

    all_fld_list = init_all_field_ctl_c();

    load_field_w_qflag_from_ctl(mhd_ctl_m->model_ctl->fld_ctl, all_fld_list);
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToFieldclist];
    
    usedFieldCtlTableview = [[UsedFieldControlTableview alloc] init];
    [usedFieldCtlTableview linkToFieldclist:all_fld_list];
    [usedFieldCtlTableview initMutablearray];    
    [usedFieldCtlTableview createMutablearray];
    [usedFieldCtlTableview createFieldView:usedFieldTableViewOutlet];
    
    unusedFieldCtlTableview = [[UnusedFieldControlTableview alloc] init];
    [unusedFieldCtlTableview linkToFieldclist:all_fld_list];
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

