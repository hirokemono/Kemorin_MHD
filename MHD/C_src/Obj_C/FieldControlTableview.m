/*
//  FieldControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "FieldControlTableview.h"

@implementation FieldControlTableview

@synthesize FieldControlDictionary;
@synthesize FieldControlArray;
@synthesize FieldTableView;

@synthesize key1;
@synthesize key2;
@synthesize key3;
@synthesize key4;
@synthesize key0;

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

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.FieldControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


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

