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
@synthesize chara2RealTableView;
@synthesize key1;
@synthesize key2;
@synthesize key3;

-(void)linkToFieldclist
{
    load_MHD_control_c();
    struct SGS_MHD_control_c * mhd_ctl_m = link_to_mhd_ctl();
    
    all_fld_tbl = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(all_fld_tbl);

    load_field_w_qflag_from_ctl(mhd_ctl_m->model_ctl->fld_ctl, all_fld_tbl);
}

-(void)createMutablearray
{
    int i;
    char *c1_out, *c2_out;
    double r1_out;
    
    numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.minimumSignificantDigits = 2;
    
    
    self.FieldControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:Chara2RealCtlList->c1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:Chara2RealCtlList->c2_name encoding:NSUTF8StringEncoding];    
    self.key3 = [NSString stringWithCString:Chara2RealCtlList->r1_name encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<count_chara2_real_clist(Chara2RealCtlList);i++){
        set_from_chara2_real_clist_at_index(i, Chara2RealCtlList, c1_out, c2_out, &r1_out);
        NSNumber *num3 = [[NSNumber alloc] initWithDouble:r1_out];
        NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
        NSString *data2 = [NSString stringWithCString:c2_out encoding:NSUTF8StringEncoding];
        NSString *data3 = [numberFormatter stringFromNumber:num3];
        self.FieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2,data3,self.key3, nil];
        [self.FieldControlArray addObject:self.FieldControlDictionary];
    };
    free(c1_out);
    free(c2_out);
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:FieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.chara2RealTableView = [[NSTableView alloc] initWithFrame:FieldTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.chara2RealTableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.chara2RealTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key3];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key3];
    [self.chara2RealTableView addTableColumn:tCol];
    
    [self.chara2RealTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.chara2RealTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.chara2RealTableView setGridColor:[NSColor grayColor]];
    [self.chara2RealTableView setRowHeight:23.0];
    [self.chara2RealTableView setDelegate:self];
    [self.chara2RealTableView setDataSource:self];
    [self.chara2RealTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.chara2RealTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.chara2RealTableView];
    [FieldTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToFieldclist];
    [self createMutablearray];
    [self createTableView];
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
    char *c1_out, *c2_out;
    double r1_out;
    NSString *selectedKey = [pTableColumn identifier];
    
    
    /*
    NSString *editedtext = [self.FieldControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara2_real_clist_at_index((int) pRowIndex, Chara2RealCtlList, c1_out, c2_out, &r1_out);

    if([selectedKey isEqualToString:self.key1]){
        sprintf(c1_out, "%s", [pObject UTF8String]);
    }
    if([selectedKey isEqualToString:self.key2]){
        sprintf(c2_out, "%s", [pObject UTF8String]);
    }
    if([selectedKey isEqualToString:self.key3]){
        NSNumber *new_value = [numberFormatter numberFromString:pObject];
        if(new_value == nil){
            free(c1_out);
            free(c2_out);
            return;
        };
        r1_out = [new_value doubleValue];
    }
    update_chara2_real_clist_by_index((int) pRowIndex, c1_out, c2_out, r1_out, Chara2RealCtlList);
    free(c1_out);
    free(c2_out);

    [[self.FieldControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out, *c2_out;
    double r1_out;
    NSInteger isel = [self.chara2RealTableView selectedRow];
    
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
    NSString *data3 = [numberFormatter stringFromNumber:num3];
    free(c1_out);
    free(c2_out);
    
    self.FieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2,data3,self.key3, nil];
    [self.FieldControlArray insertObject:self.FieldControlDictionary atIndex:isel];

    [self.chara2RealTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.chara2RealTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara2_real_clist(Chara2RealCtlList)) return;

    del_chara2_real_clist_by_index((int) isel, Chara2RealCtlList);
    [self.FieldControlArray removeObjectAtIndex:isel];

    [self.chara2RealTableView reloadData];
}

@end

