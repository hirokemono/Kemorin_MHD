/*
//  UsedFieldControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "UsedFieldControlTableview.h"

@implementation UsedFieldControlTableview

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

-(void)createMutablearray
{
    int i;
    char *c1_out;
    
    /*
     for(i=0;i<NUM_FIELD;i++){
     printf("%d %s %d %d %d %d \n", i, all_fld_tbl[i]->field_name, 
     all_fld_tbl[i]->iflag_use, all_fld_tbl[i]->iflag_viz,
     all_fld_tbl[i]->iflag_monitor, all_fld_tbl[i]->iflag_quad);
     }
     */
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    self.FieldControlArray = [[NSMutableArray alloc]init];    
    
    self.key1 = [NSString stringWithCString:"Field_name" encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:"Viz_flag" encoding:NSUTF8StringEncoding];    
    self.key3 = [NSString stringWithCString:"Monitor" encoding:NSUTF8StringEncoding];    
    self.key4 = [NSString stringWithCString:"Quadrature_field" encoding:NSUTF8StringEncoding];    
    self.key0 = [NSString stringWithCString:"ID" encoding:NSUTF8StringEncoding];    
    
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<NUM_FIELD;i++){
        if(all_fld_tbl[i]->iflag_use > 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num2 = [[NSNumber alloc] initWithDouble:all_fld_tbl[i]->iflag_viz];
            NSNumber *num3 = [[NSNumber alloc] initWithDouble:all_fld_tbl[i]->iflag_monitor];
            NSNumber *num4 = [[NSNumber alloc] initWithDouble:all_fld_tbl[i]->iflag_quad];
            NSNumber *num0 = [[NSNumber alloc] initWithDouble:i];
            self.FieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, num2,self.key2, num3,self.key3, num4,self.key4, nil];
            [self.FieldControlArray addObject:self.FieldControlDictionary];
        };
    };
    free(c1_out);
}


-(void)createFieldView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:FieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.FieldTableView = [[NSTableView alloc] initWithFrame:FieldTableViewOutlet.bounds];
    NSTableColumn *tCol;
    NSButtonCell *cell;
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key0];
    [tCol setWidth:40.0];
    [[tCol headerCell] setStringValue:self.key0];
    [self.FieldTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:80.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.FieldTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:50.0];
    [[tCol headerCell] setStringValue:self.key2];
    cell = [[NSButtonCell alloc ] initTextCell: @"" ];
    [cell setEditable: NO];
    [cell setButtonType: NSSwitchButton ];
    [tCol setDataCell: cell ];
    [self.FieldTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key3];
    [tCol setWidth:50.0];
    [[tCol headerCell] setStringValue:self.key3];
    cell = [[NSButtonCell alloc ] initTextCell: @"" ];
    [cell setEditable: NO];
    [cell setButtonType: NSSwitchButton ];
    [tCol setDataCell: cell ];
    [self.FieldTableView addTableColumn:tCol];
    
    [self.FieldTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.FieldTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.FieldTableView setGridColor:[NSColor grayColor]];
    [self.FieldTableView setRowHeight:23.0];
    [self.FieldTableView setDelegate:self];
    [self.FieldTableView setDataSource:self];
    [self.FieldTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.FieldTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.FieldTableView];
    [FieldTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToFieldclist];
    [self createMutablearray];
    [self createFieldView];
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


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    int index;
    NSString *selectedKey = [pTableColumn identifier];
    NSString *selectedID = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:self.key0];
    index =  [selectedID intValue];
    
    /*
     NSString *editedtext = [self.FieldControlArray objectAtIndex:pRowIndex];
     NSString *selectedItem = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
     
     NSLog(@"Mutablearray   %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
     NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
     
     NSLog(@"Mutablearray Selected  %@",selectedItem);
     
     NSLog(@"%d %d %@¥n", pRowIndex, index, editedtext);
     NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
     */
    
    [[self.FieldControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
    if([selectedKey isEqualToString:self.key2]){
        all_fld_tbl[index]->iflag_viz = [pObject intValue];
    }
    if([selectedKey isEqualToString:self.key3]){
        all_fld_tbl[index]->iflag_monitor = [pObject intValue];
    }
    update_field_flag_wqflag_in_ctl(all_fld_tbl[index], mhd_ctl_m->model_ctl->fld_ctl);
    
    //    NSLog(@"Mutablearray again  %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out, *c2_out;
    double r1_out;
    NSInteger isel = [self.FieldTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_chara2_real_clist(Chara2RealCtlList)) return;
    
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara2_real_clist_at_index((int) isel, Chara2RealCtlList, c1_out, c2_out, &r1_out);
    add_chara2_real_clist_before_c_tbl(c1_out, c2_out, c1_out, c2_out, r1_out, Chara2RealCtlList);
    
    set_from_chara2_real_clist_at_index((int) isel, Chara2RealCtlList, c1_out, c2_out, &r1_out);
    NSNumber *num3 = [[NSNumber alloc] initWithDouble:r1_out];
    NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
    NSString *data2 = [NSString stringWithCString:c2_out encoding:NSUTF8StringEncoding];
    NSString *data3 = [integerFormatter stringFromNumber:num3];
    free(c1_out);
    free(c2_out);
    
    self.FieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2,data3,self.key3, nil];
    [self.FieldControlArray insertObject:self.FieldControlDictionary atIndex:isel];
    
    [self.FieldTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.FieldTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara2_real_clist(Chara2RealCtlList)) return;
    
    del_chara2_real_clist_by_index((int) isel, Chara2RealCtlList);
    [self.FieldControlArray removeObjectAtIndex:isel];
    
    [self.FieldTableView reloadData];
}

@end

