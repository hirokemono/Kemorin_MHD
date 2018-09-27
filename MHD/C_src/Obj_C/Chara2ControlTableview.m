/*
//  Chara2ControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "Chara2ControlTableview.h"

@implementation Chara2ControlTableview
@synthesize chara2ControlDictionary;
@synthesize chara2ControlArray;
@synthesize chara2TableView;
@synthesize key1;
@synthesize key2;

-(void)linkToChara2clist
{
    load_MHD_control_c();
    struct PVR_ctl_list *pvr1 = link_to_pvr_ctl_list();
    Chara2CtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->step_opacity_list;
}
-(void)createMutablearray
{
    int i;
    char *c1_out, *c2_out;
        
    self.chara2ControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:Chara2CtlList->c1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:Chara2CtlList->c2_name encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<count_chara2_clist(Chara2CtlList);i++){
        set_from_chara2_clist_at_index(i, Chara2CtlList, c1_out, c2_out);
        NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
        NSString *data2 = [NSString stringWithCString:c2_out encoding:NSUTF8StringEncoding];
        self.chara2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
        [self.chara2ControlArray addObject:self.chara2ControlDictionary];
    };
    free(c1_out);
    free(c2_out);
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:chara2TableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.chara2TableView = [[NSTableView alloc] initWithFrame:chara2TableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.chara2TableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.chara2TableView addTableColumn:tCol];
    
    [self.chara2TableView setUsesAlternatingRowBackgroundColors:YES];
    [self.chara2TableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.chara2TableView setGridColor:[NSColor grayColor]];
    [self.chara2TableView setRowHeight:23.0];
    [self.chara2TableView setDelegate:self];
    [self.chara2TableView setDataSource:self];
    [self.chara2TableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.chara2TableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.chara2TableView];
    [chara2TableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToChara2clist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.chara2ControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.chara2ControlArray count];
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
    NSString *selectedKey = [pTableColumn identifier];
    
    
    /*
    NSString *editedtext = [self.chara2ControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.chara2ControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.chara2ControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara2_clist_at_index((int) pRowIndex, Chara2CtlList, c1_out, c2_out);

    if([selectedKey isEqualToString:self.key1]){
        sprintf(c1_out, "%s", [pObject UTF8String]);
    }
    if([selectedKey isEqualToString:self.key2]){
        sprintf(c2_out, "%s", [pObject UTF8String]);
    }
    update_chara2_clist_by_index((int) pRowIndex, c1_out, c2_out, Chara2CtlList);
    free(c1_out);
    free(c2_out);

    [[self.chara2ControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.chara2ControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out, *c2_out;
    NSInteger isel = [self.chara2TableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_chara2_clist(Chara2CtlList)) return;

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    c2_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara2_clist_at_index((int) isel, Chara2CtlList, c1_out, c2_out);
    add_chara2_clist_before_c_tbl(c1_out, c2_out, c1_out, c2_out, Chara2CtlList);
    
    set_from_chara2_clist_at_index((int) isel, Chara2CtlList, c1_out, c2_out);
    NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
    NSString *data2 = [NSString stringWithCString:c2_out encoding:NSUTF8StringEncoding];
    free(c1_out);
    free(c2_out);
    
    self.chara2ControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
    [self.chara2ControlArray insertObject:self.chara2ControlDictionary atIndex:isel];

    [self.chara2TableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.chara2TableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara2_clist(Chara2CtlList)) return;

    del_chara2_clist_by_index((int) isel, Chara2CtlList);
    [self.chara2ControlArray removeObjectAtIndex:isel];

    [self.chara2TableView reloadData];
}

@end

