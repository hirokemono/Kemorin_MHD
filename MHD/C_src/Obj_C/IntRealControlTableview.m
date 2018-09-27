/*
//  IntRealControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "IntRealControlTableview.h"

@implementation IntRealControlTableview
@synthesize intRealControlDictionary;
@synthesize intRealControlArray;
@synthesize intRealTableView;
@synthesize key1;
@synthesize key2;

-(void)linkToIntRealclist
{
    load_MHD_control_c();
    struct PVR_ctl_list *pvr1 = link_to_pvr_ctl_list();
    intRealCtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->colortbl_list;
}
-(void)createMutablearray
{
    int i;
    int i_out;
    double r_out;
    
    numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.minimumSignificantDigits = 2;
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    
    self.intRealControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:intRealCtlList->i1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:intRealCtlList->r1_name encoding:NSUTF8StringEncoding];    
    for(i=0;i<count_int_real_clist(intRealCtlList);i++){
        set_from_int_real_clist_at_index(i, intRealCtlList, &i_out, &r_out);
        NSNumber *num1 = [[NSNumber alloc] initWithInt:i_out];    
        NSNumber *num2 = [[NSNumber alloc] initWithDouble:r_out];
        NSString *data1 = [integerFormatter stringFromNumber:num1];
        NSString *data2 = [numberFormatter stringFromNumber:num2];
        self.intRealControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
        [self.intRealControlArray addObject:self.intRealControlDictionary];
    };
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:intRealTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.intRealTableView = [[NSTableView alloc] initWithFrame:intRealTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.intRealTableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.intRealTableView addTableColumn:tCol];
    
    [self.intRealTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.intRealTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.intRealTableView setGridColor:[NSColor grayColor]];
    [self.intRealTableView setRowHeight:23.0];
    [self.intRealTableView setDelegate:self];
    [self.intRealTableView setDataSource:self];
    [self.intRealTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.intRealTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.intRealTableView];
    [intRealTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToIntRealclist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.intRealControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.intRealControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    int i_out;
    double r_out;
    NSString *selectedKey = [pTableColumn identifier];
    /*
    NSString *editedtext = [self.intRealControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.intRealControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.intRealControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    set_from_int_real_clist_at_index((int) pRowIndex, intRealCtlList, &i_out, &r_out);

    if([selectedKey isEqualToString:self.key1]){
        NSNumber *new_value = [integerFormatter numberFromString:pObject];
        if(new_value == nil) return;
        i_out = [new_value intValue];
    }
    if([selectedKey isEqualToString:self.key2]){
        NSNumber *new_value = [numberFormatter numberFromString:pObject];
        if(new_value == nil) return;
        r_out = [new_value doubleValue];
    }
    update_int_real_clist_by_index((int) pRowIndex, i_out, r_out, intRealCtlList);
    [[self.intRealControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.intRealControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    int i_out;
    double r_out;
    NSInteger isel = [self.intRealTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_int_real_clist(intRealCtlList)) return;

    set_from_int_real_clist_at_index((int) isel, intRealCtlList, &i_out, &r_out);
    add_int_real_clist_before_c_tbl(i_out, r_out, i_out, r_out, intRealCtlList);
    
    set_from_int_real_clist_at_index((int) isel, intRealCtlList, &i_out, &r_out);
    NSNumber *num1 = [[NSNumber alloc] initWithInt:i_out];    
    NSNumber *num2 = [[NSNumber alloc] initWithDouble:r_out];
    NSString *data1 = [integerFormatter stringFromNumber:num1];
    NSString *data2 = [numberFormatter stringFromNumber:num2];
    
    self.intRealControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
    [self.intRealControlArray insertObject:self.intRealControlDictionary atIndex:isel];

    [self.intRealTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.intRealTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_int_real_clist(intRealCtlList)) return;

    del_int_real_clist_by_index((int) isel, intRealCtlList);
    [self.intRealControlArray removeObjectAtIndex:isel];

    [self.intRealTableView reloadData];
}

@end

