/*
//  FieldControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_4_fields_c.h"
#include "load_MHD_control_c.h"
#include "UsedFieldControlTableview.h"
#include "UnusedFieldControlTableview.h"

@interface FieldControlTableview : NSObject {
    
    UsedFieldControlTableview * usedFieldCtlTableview;
    UnusedFieldControlTableview * unusedFieldCtlTableview;
    
    IBOutlet NSView *usedFieldTableViewOutlet;
    IBOutlet NSView *unUsedFieldTableViewOutlet;
    
    struct SGS_MHD_control_c * mhd_ctl_m;
    struct all_field_ctl_z **all_fld_tbl;
}

-(void)linkToFieldclist;

- (void)awakeFromNib;
- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

@end
