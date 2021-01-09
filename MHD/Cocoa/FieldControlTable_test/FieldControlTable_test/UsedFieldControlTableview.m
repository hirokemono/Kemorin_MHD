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

-(void)linkToFieldclist:(struct all_field_ctl_c *) ref_all_fld_list
{ 
    mhd_ctl_m = link_to_mhd_ctl();
    all_fld_list = ref_all_fld_list;
}

-(void)initMutablearray
{
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    self.FieldControlArray = [[NSMutableArray alloc]init];    
    self.key1 = [NSString stringWithCString:"Field_name" encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:"Viz_flag" encoding:NSUTF8StringEncoding];    
    self.key3 = [NSString stringWithCString:"Monitor" encoding:NSUTF8StringEncoding];    
    self.key4 = [NSString stringWithCString:"Quadrature_field" encoding:NSUTF8StringEncoding];    
    self.key0 = [NSString stringWithCString:"ID" encoding:NSUTF8StringEncoding];    
}

-(void)createMutablearray
{
    int i;
    char *c1_out;
    
    /*
     for(i=0;i<all_fld_list->fld_list->ntot_fields;i++){
     printf("%d %s %d %d %d %d \n", i, all_fld_list->fld_list->field_name[i], 
     all_fld_list->iflag_use[i], all_fld_list->iflag_viz[i],
     all_fld_list->iflag_monitor[i], all_fld_list->iflag_quad[i]);
     }
     */
    
    [self.FieldControlArray removeAllObjects];
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<all_fld_list->fld_list->ntot_fields;i++){
        if(all_fld_list->iflag_use[i] > 0){
            NSString *data1 = [NSString stringWithCString:all_fld_list->fld_list->field_name[i] encoding:NSUTF8StringEncoding];
            NSNumber *num2 = [[NSNumber alloc] initWithInt:all_fld_list->iflag_viz[i]];
            NSNumber *num3 = [[NSNumber alloc] initWithInt:all_fld_list->iflag_monitor[i]];
            NSNumber *num4 = [[NSNumber alloc] initWithInt:all_fld_list->iflag_quad[i]];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.FieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, num2,self.key2, num3,self.key3, num4,self.key4, nil];
            [self.FieldControlArray addObject:self.FieldControlDictionary];
        };
    };
    free(c1_out);
}


-(void)createFieldView:(NSView *)usedFieldTableViewOutlet
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:usedFieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.FieldTableView = [[NSTableView alloc] initWithFrame:usedFieldTableViewOutlet.bounds];
    NSTableColumn *tCol;
    NSButtonCell *cell;
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key0];
    [tCol setWidth:40.0];
    [[tCol headerCell] setStringValue:self.key0];
    [self.FieldTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:180.0];
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
    [self.FieldTableView setAllowsMultipleSelection:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.FieldTableView];
    [usedFieldTableViewOutlet addSubview:scrollView];
}

-(void) updateUsedFieldTable
{
    [self.FieldTableView reloadData];
}

- (void) deleteUsedField
{
    NSIndexSet *selectedRows = [self.FieldTableView selectedRowIndexes];
    //    NSUInteger numberOfSelectedRows = [selectedRows count];
    NSUInteger isel;
    int index;
    
    isel = [selectedRows lastIndex];
    NSMutableIndexSet *field_Indices = [NSMutableIndexSet indexSet];
    while(isel != NSNotFound) {
        NSLog(@"index = %d", (int) isel);
        NSString *selectedID = [[self.FieldControlArray objectAtIndex:isel] objectForKey:self.key0];
        index =  [selectedID intValue];
        [field_Indices addIndex:index];
        all_fld_list->iflag_use[i] =     0;
        all_fld_list->iflag_viz[i] =     0;
        all_fld_list->iflag_monitor[i] = 0;
        all_fld_list->iflag_quad[i] =    0;
        delete_field_wqflag_in_ctl(index, all_fld_list, mhd_ctl_m->model_ctl->fld_ctl);
        
        [self.FieldControlArray removeObjectAtIndex:isel];
        isel = [selectedRows indexLessThanIndex:isel];
    }
    [self.FieldTableView reloadData];
    //    NSLog(@"field_Indices   %@",field_Indices);
}

// TableView Datasource method implementation
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

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
        all_fld_list->iflag_viz[index] = [pObject intValue];
    }
    if([selectedKey isEqualToString:self.key3]){
        all_fld_list->iflag_monitor[index] = [pObject intValue];
    }
    update_field_flag_wqflag_in_ctl(index, all_fld_list, mhd_ctl_m->model_ctl->fld_ctl);
    
    //    NSLog(@"Mutablearray again  %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
};
@end

