//
//  Real2ControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
//

#import <Foundation/Foundation.h>
#include "Real2ControlTableview.h"

struct real2_clist * Real2CtlList;

struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */


@implementation Real2ControlTableview
@synthesize real2ControlArray;
@synthesize real2ControlDictionary;
@synthesize real2TableView;

NSString *_key1 = @"key1";
NSString *_key2 = @"key2";
NSString *_key3 = @"key3";

-(void)createMutablearray
{
    int i;
    double r1_out, r2_out;
    
    self.real2ControlArray = [[NSMutableArray alloc]init];    

    NSString *key1 = [NSString stringWithCString:Real2CtlList->r1_name encoding:NSUTF8StringEncoding];    
    NSString *key2 = [NSString stringWithCString:Real2CtlList->r2_name encoding:NSUTF8StringEncoding];    
    for(i=0;i<count_real2_clist(Real2CtlList);i++){
        set_from_real2_clist_at_index(i, Real2CtlList, &r1_out, &r2_out);
        NSNumber *num1 = [[NSNumber alloc] initWithDouble:r1_out];    
        NSNumber *num2 = [[NSNumber alloc] initWithDouble:r2_out];
        NSString *data1 = [numberFormatter stringFromNumber:num1];
        NSString *data2 = [numberFormatter stringFromNumber:num2];
        self.real2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,key1,data2,key2, nil];
        [self.real2ControlArray addObject:self.real2ControlDictionary];
    };
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:real2TableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.real2TableView = [[NSTableView alloc] initWithFrame:real2TableViewOutlet.bounds];
    NSTableColumn *tCol;

    NSString *key1 = [NSString stringWithCString:Real2CtlList->r1_name encoding:NSUTF8StringEncoding];    
    tCol = [[NSTableColumn alloc] initWithIdentifier:key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:key1];
    [self.real2TableView addTableColumn:tCol];

    NSString *key2 = [NSString stringWithCString:Real2CtlList->r2_name encoding:NSUTF8StringEncoding];    
    tCol = [[NSTableColumn alloc] initWithIdentifier:key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:key2];
    [self.real2TableView addTableColumn:tCol];
    
    [self.real2TableView setUsesAlternatingRowBackgroundColors:YES];
    [self.real2TableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.real2TableView setGridColor:[NSColor redColor]];
    [self.real2TableView setRowHeight:23.0];
    [self.real2TableView setDelegate:self];
    [self.real2TableView setDataSource:self];
    [self.real2TableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.real2TableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.real2TableView];
    [real2TableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
    alloc_SGS_MHD_control_c(mhd_ctl);
    read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    struct PVR_ctl_list *pvr1 = &mhd_ctl->viz_c->pvr_ctl_list;
    Real2CtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->colortbl_list;
    
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.real2ControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.real2ControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    double r1_out, r2_out;
    NSString *editedtext = [self.real2ControlArray objectAtIndex:pRowIndex];
    NSString *selectedKey = [pTableColumn identifier];
    NSString *selectedItem = [[self.real2ControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    
    NSLog(@"Mutablearray   %@",[self.real2ControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
    
    [[self.real2ControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
    set_from_real2_clist_at_index((int) pRowIndex, Real2CtlList, &r1_out, &r2_out);
    update_real2_clist_by_index((int) pRowIndex, double r1_in, double r2_in, Real2CtlList);
    NSLog(@"Mutablearray again  %@",[self.real2ControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    double r1_out, r2_out;
    NSInteger isel = [self.real2TableView selectedRow];
    NSString *key1 = [NSString stringWithCString:Real2CtlList->r1_name encoding:NSUTF8StringEncoding];    
    NSString *key2 = [NSString stringWithCString:Real2CtlList->r2_name encoding:NSUTF8StringEncoding];    
    
    if(isel < 1) return;
    if(isel >= count_real2_clist(Real2CtlList)) return;

    set_from_real2_clist_at_index((int) isel, Real2CtlList, &r1_out, &r2_out);
    add_real2_clist_before_c_tbl(r1_out, r2_out, Real2CtlList);
    
    set_from_real2_clist_at_index((int) isel, Real2CtlList, &r1_out, &r2_out);
    NSNumber *num1 = [[NSNumber alloc] initWithDouble:r1_out];    
    NSNumber *num2 = [[NSNumber alloc] initWithDouble:r2_out];
    NSString *data1 = [numberFormatter stringFromNumber:num1];
    NSString *data2 = [numberFormatter stringFromNumber:num2];
    
    self.real2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,key1,data2,key2, nil];
    [self.real2ControlArray insertObject:self.real2ControlDictionary atIndex:isel];

    [self.real2TableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.real2TableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_real2_clist(Real2CtlList)) return;

    del_real2_clist_by_index((int) isel, Real2CtlList);
    [self.real2ControlArray removeObjectAtIndex:isel];

    [self.real2TableView reloadData];
}

@end

