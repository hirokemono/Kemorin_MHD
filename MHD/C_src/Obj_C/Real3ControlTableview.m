/*
//  Real3ControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "Real3ControlTableview.h"

@implementation Real3ControlTableview
@synthesize real3ControlDictionary;
@synthesize real3ControlArray;
@synthesize real3TableView;
@synthesize key1;
@synthesize key2;
@synthesize key3;

-(void)linkToReal3clist
{
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
    if(f_MHD_ctl == NULL){
        printf("malloc error for f_MHD_ctl\n");
        exit(0);
    };
    f_MHD_ctl->f_self = c_read_control_sph_SGS_MHD("control_MHD");
    struct f_VIZ_PVR_ctl *f_ctl_tmp = void_clist_at_index(0,f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
    Real3CtlList = f_pvr_ctl->f_light->f_light_position_ctl;
}
-(void)createMutablearray
{
    int i;
    double r1_out, r2_out, r3_out;
    
    numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.minimumSignificantDigits = 2;
    
    
    self.real3ControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:Real3CtlList->r1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:Real3CtlList->r2_name encoding:NSUTF8StringEncoding];    
    self.key3 = [NSString stringWithCString:Real3CtlList->r2_name encoding:NSUTF8StringEncoding];    
    for(i=0;i<count_real3_clist(Real3CtlList);i++){
        set_from_real3_clist_at_index(i, Real3CtlList, &r1_out, &r2_out, &r3_out);
        NSNumber *num1 = [[NSNumber alloc] initWithDouble:r1_out];    
        NSNumber *num2 = [[NSNumber alloc] initWithDouble:r2_out];
        NSNumber *num3 = [[NSNumber alloc] initWithDouble:r3_out];
        NSString *data1 = [numberFormatter stringFromNumber:num1];
        NSString *data2 = [numberFormatter stringFromNumber:num2];
        NSString *data3 = [numberFormatter stringFromNumber:num3];
        self.real3ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2,data3,self.key3, nil];
        [self.real3ControlArray addObject:self.real3ControlDictionary];
    };
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:real3TableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.real3TableView = [[NSTableView alloc] initWithFrame:real3TableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.real3TableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.real3TableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key3];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key3];
    [self.real3TableView addTableColumn:tCol];
    
    [self.real3TableView setUsesAlternatingRowBackgroundColors:YES];
    [self.real3TableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.real3TableView setGridColor:[NSColor grayColor]];
    [self.real3TableView setRowHeight:23.0];
    [self.real3TableView setDelegate:self];
    [self.real3TableView setDataSource:self];
    [self.real3TableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.real3TableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.real3TableView];
    [real3TableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToReal3clist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.real3ControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.real3ControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    double r1_out, r2_out, r3_out;
    NSString *selectedKey = [pTableColumn identifier];
    
    /*
    NSString *editedtext = [self.real3ControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.real3ControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.real3ControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    set_from_real3_clist_at_index((int) pRowIndex, Real3CtlList, &r1_out, &r2_out, &r3_out);

    NSNumber *new_value = [numberFormatter numberFromString:pObject];
    if(new_value == nil) return;
    if([selectedKey isEqualToString:self.key1]){
        r1_out = [new_value doubleValue];
    }
    if([selectedKey isEqualToString:self.key2]){
        r2_out = [new_value doubleValue];
    }
    if([selectedKey isEqualToString:self.key3]){
        r3_out = [new_value doubleValue];
    }
    update_real3_clist_by_index((int) pRowIndex, r1_out, r2_out, r3_out, Real3CtlList);
    [[self.real3ControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.real3ControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    double r1_out, r2_out, r3_out;
    NSInteger isel = [self.real3TableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_real3_clist(Real3CtlList)) return;

    set_from_real3_clist_at_index((int) isel, Real3CtlList, &r1_out, &r2_out, &r3_out);
    add_real3_clist_before_c_tbl(r1_out, r2_out,  r3_out, Real3CtlList);
    
    set_from_real3_clist_at_index((int) isel, Real3CtlList, &r1_out, &r2_out, &r3_out);
    NSNumber *num1 = [[NSNumber alloc] initWithDouble:r1_out];    
    NSNumber *num2 = [[NSNumber alloc] initWithDouble:r2_out];
    NSNumber *num3 = [[NSNumber alloc] initWithDouble:r3_out];
    NSString *data1 = [numberFormatter stringFromNumber:num1];
    NSString *data2 = [numberFormatter stringFromNumber:num2];
    NSString *data3 = [numberFormatter stringFromNumber:num3];
    
    self.real3ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2,data3,self.key3, nil];
    [self.real3ControlArray insertObject:self.real3ControlDictionary atIndex:isel];

    [self.real3TableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.real3TableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_real3_clist(Real3CtlList)) return;

    del_real3_clist_by_index((int) isel, Real3CtlList);
    [self.real3ControlArray removeObjectAtIndex:isel];

    [self.real3TableView reloadData];
}

@end

