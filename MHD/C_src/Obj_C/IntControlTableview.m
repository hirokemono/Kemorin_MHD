/*
//  IntControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "IntControlTableview.h"

@implementation IntControlTableview
@synthesize intControlDictionary;
@synthesize intControlArray;
@synthesize intTableView;
@synthesize key1;

-(void)linkToIntRealclist
{
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
    if(f_MHD_ctl == NULL){
        printf("malloc error for f_MHD_ctl\n");
        exit(0);
    };
    f_MHD_ctl->f_self = c_read_control_sph_SGS_MHD("control_MHD");
    intCtlList = f_MHD_ctl->f_smonitor_ctl->f_g_pwr->f_idx_gauss_l_ctl;
}
-(void)createMutablearray
{
    int i;
    int i1_out;
    
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    
    self.intControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:intCtlList->i1_name encoding:NSUTF8StringEncoding];    
    for(i=0;i<count_int_clist(intCtlList);i++){
        set_from_int_clist_at_index(i, intCtlList, &i1_out);
        NSNumber *num1 = [[NSNumber alloc] initWithInt:i1_out];    
        NSString *data1 = [integerFormatter stringFromNumber:num1];
        self.intControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1, nil];
        [self.intControlArray addObject:self.intControlDictionary];
    };
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:intTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.intTableView = [[NSTableView alloc] initWithFrame:intTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.intTableView addTableColumn:tCol];

    [self.intTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.intTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.intTableView setGridColor:[NSColor grayColor]];
    [self.intTableView setRowHeight:23.0];
    [self.intTableView setDelegate:self];
    [self.intTableView setDataSource:self];
    [self.intTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.intTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.intTableView];
    [intTableViewOutlet addSubview:scrollView];
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
    aString = [[self.intControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.intControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    int i1_out;
    NSString *selectedKey = [pTableColumn identifier];
    /*
    NSString *editedtext = [self.intControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.intControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.intControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    set_from_int_clist_at_index((int) pRowIndex, intCtlList, &i1_out);

    if([selectedKey isEqualToString:self.key1]){
        NSNumber *new_value = [integerFormatter numberFromString:pObject];
        if(new_value == nil) return;
        i1_out = [new_value intValue];
    }
    update_int_clist_by_index((int) pRowIndex, i1_out, intCtlList);
    [[self.intControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.intControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    int i1_out;
    NSInteger isel = [self.intTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_int_clist(intCtlList)) return;

    set_from_int_clist_at_index((int) isel, intCtlList, &i1_out);
    add_int_clist_before_c_tbl(i1_out, i1_out, intCtlList);
    
    set_from_int_clist_at_index((int) isel, intCtlList, &i1_out);
    NSNumber *num1 = [[NSNumber alloc] initWithInt:i1_out];    
    NSString *data1 = [integerFormatter stringFromNumber:num1];
    
    self.intControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1, nil];
    [self.intControlArray insertObject:self.intControlDictionary atIndex:isel];

    [self.intTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.intTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_int_clist(intCtlList)) return;

    del_int_clist_by_index((int) isel, intCtlList);
    [self.intControlArray removeObjectAtIndex:isel];

    [self.intTableView reloadData];
}

@end

