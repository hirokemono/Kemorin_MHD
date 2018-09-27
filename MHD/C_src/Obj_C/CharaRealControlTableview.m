/*
//  CharaRealControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "CharaRealControlTableview.h"

@implementation CharaRealControlTableview
@synthesize charaRealControlDictionary;
@synthesize charaRealControlArray;
@synthesize charaRealTableView;
@synthesize key1;
@synthesize key2;

-(void)linkToCharaRealclist
{
    load_MHD_control_c();
    struct PVR_ctl_list *pvr1 = link_to_pvr_ctl_list();
    CharaRealCtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->step_opacity_list;
}
-(void)createMutablearray
{
    int i;
    char *c1_out;
    double r1_out;
    
    numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.minimumSignificantDigits = 2;
    
    
    self.charaRealControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:CharaRealCtlList->c1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:CharaRealCtlList->r1_name encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<count_chara_real_clist(CharaRealCtlList);i++){
        set_from_chara_real_clist_at_index(i, CharaRealCtlList, c1_out, &r1_out);
        NSNumber *num2 = [[NSNumber alloc] initWithDouble:r1_out];
        NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
        NSString *data2 = [numberFormatter stringFromNumber:num2];
        self.charaRealControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
        [self.charaRealControlArray addObject:self.charaRealControlDictionary];
    };
    free(c1_out);
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:charaRealTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.charaRealTableView = [[NSTableView alloc] initWithFrame:charaRealTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.charaRealTableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.charaRealTableView addTableColumn:tCol];
    
    [self.charaRealTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.charaRealTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.charaRealTableView setGridColor:[NSColor grayColor]];
    [self.charaRealTableView setRowHeight:23.0];
    [self.charaRealTableView setDelegate:self];
    [self.charaRealTableView setDataSource:self];
    [self.charaRealTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.charaRealTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.charaRealTableView];
    [charaRealTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToCharaRealclist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.charaRealControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.charaRealControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    char *c1_out;
    double r1_out;
    NSString *selectedKey = [pTableColumn identifier];
    
    
    /*
    NSString *editedtext = [self.charaRealControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.charaRealControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.charaRealControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_real_clist_at_index((int) pRowIndex, CharaRealCtlList, c1_out, &r1_out);

    if([selectedKey isEqualToString:self.key1]){
        sprintf(c1_out, "%s", [pObject UTF8String]);
    }
    if([selectedKey isEqualToString:self.key2]){
        NSNumber *new_value = [numberFormatter numberFromString:pObject];
        if(new_value == nil){
            free(c1_out);
            return;
        };
        r1_out = [new_value doubleValue];
    }
    update_chara_real_clist_by_index((int) pRowIndex, c1_out, r1_out, CharaRealCtlList);
    free(c1_out);

    [[self.charaRealControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.charaRealControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out;
    double r1_out;
    NSInteger isel = [self.charaRealTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_chara_real_clist(CharaRealCtlList)) return;

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_real_clist_at_index((int) isel, CharaRealCtlList, c1_out, &r1_out);
    add_chara_real_clist_before_c_tbl(c1_out, c1_out, r1_out, CharaRealCtlList);
    
    set_from_chara_real_clist_at_index((int) isel, CharaRealCtlList, c1_out, &r1_out);
    NSNumber *num2 = [[NSNumber alloc] initWithDouble:r1_out];
    NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
    NSString *data2 = [numberFormatter stringFromNumber:num2];
    free(c1_out);
    
    self.charaRealControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
    [self.charaRealControlArray insertObject:self.charaRealControlDictionary atIndex:isel];

    [self.charaRealTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.charaRealTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara_real_clist(CharaRealCtlList)) return;

    del_chara_real_clist_by_index((int) isel, CharaRealCtlList);
    [self.charaRealControlArray removeObjectAtIndex:isel];

    [self.charaRealTableView reloadData];
}

@end

