/*
//  Int2ControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "Int2ControlTableview.h"

@implementation Int2ControlTableview
@synthesize int2ControlDictionary;
@synthesize int2ControlArray;
@synthesize int2TableView;
@synthesize key1;
@synthesize key2;

-(void)linkToIntRealclist
{
    load_MHD_control_c();
    struct PVR_ctl_list *pvr1 = link_to_pvr_ctl_list();
    int2CtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->colortbl_list;
}
-(void)createMutablearray
{
    int i;
    int i1_out, i2_out;
    
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    
    self.int2ControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:int2CtlList->i1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:int2CtlList->i2_name encoding:NSUTF8StringEncoding];    
    for(i=0;i<count_int2_clist(int2CtlList);i++){
        set_from_int2_clist_at_index(i, int2CtlList, &i1_out, &i2_out);
        NSNumber *num1 = [[NSNumber alloc] initWithInt:i1_out];    
        NSNumber *num2 = [[NSNumber alloc] initWithInt:i2_out];
        NSString *data1 = [integerFormatter stringFromNumber:num1];
        NSString *data2 = [integerFormatter stringFromNumber:num2];
        self.int2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
        [self.int2ControlArray addObject:self.int2ControlDictionary];
    };
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:int2TableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.int2TableView = [[NSTableView alloc] initWithFrame:int2TableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.int2TableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.int2TableView addTableColumn:tCol];
    
    [self.int2TableView setUsesAlternatingRowBackgroundColors:YES];
    [self.int2TableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.int2TableView setGridColor:[NSColor grayColor]];
    [self.int2TableView setRowHeight:23.0];
    [self.int2TableView setDelegate:self];
    [self.int2TableView setDataSource:self];
    [self.int2TableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.int2TableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.int2TableView];
    [int2TableViewOutlet addSubview:scrollView];
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
    aString = [[self.int2ControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.int2ControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    int i1_out, i2_out;
    NSString *selectedKey = [pTableColumn identifier];
    /*
    NSString *editedtext = [self.int2ControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.int2ControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.int2ControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    set_from_int2_clist_at_index((int) pRowIndex, int2CtlList, &i1_out, &i2_out);

    if([selectedKey isEqualToString:self.key1]){
        NSNumber *new_value = [integerFormatter numberFromString:pObject];
        if(new_value == nil) return;
        i1_out = [new_value intValue];
    }
    if([selectedKey isEqualToString:self.key2]){
        NSNumber *new_value = [integerFormatter numberFromString:pObject];
        if(new_value == nil) return;
        i2_out = [new_value intValue];
    }
    update_int2_clist_by_index((int) pRowIndex, i1_out, i2_out, int2CtlList);
    [[self.int2ControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.int2ControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    int i1_out, i2_out;
    NSInteger isel = [self.int2TableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_int2_clist(int2CtlList)) return;

    set_from_int2_clist_at_index((int) isel, int2CtlList, &i1_out, &i2_out);
    add_int2_clist_before_c_tbl(i1_out, i2_out, i1_out, i2_out, int2CtlList);
    
    set_from_int2_clist_at_index((int) isel, int2CtlList, &i1_out, &i2_out);
    NSNumber *num1 = [[NSNumber alloc] initWithInt:i1_out];    
    NSNumber *num2 = [[NSNumber alloc] initWithInt:i2_out];
    NSString *data1 = [integerFormatter stringFromNumber:num1];
    NSString *data2 = [integerFormatter stringFromNumber:num2];
    
    self.int2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
    [self.int2ControlArray insertObject:self.int2ControlDictionary atIndex:isel];

    [self.int2TableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.int2TableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_int2_clist(int2CtlList)) return;

    del_int2_clist_by_index((int) isel, int2CtlList);
    [self.int2ControlArray removeObjectAtIndex:isel];

    [self.int2TableView reloadData];
}

@end

