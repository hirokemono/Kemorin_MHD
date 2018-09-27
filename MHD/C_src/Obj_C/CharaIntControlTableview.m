/*
//  CharaIntControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "CharaIntControlTableview.h"

@implementation CharaIntControlTableview
@synthesize charaIntControlDictionary;
@synthesize charaIntControlArray;
@synthesize charaIntTableView;
@synthesize key1;
@synthesize key2;

-(void)linkToCharaIntclist
{
    load_MHD_control_c();
    struct PVR_ctl_list *pvr1 = link_to_pvr_ctl_list();
    charaIntCtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->step_opacity_list;
}
-(void)createMutablearray
{
    int i;
    char *c1_out;
    int i1_out;
    
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    
    self.charaIntControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:charaIntCtlList->c1_name encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:charaIntCtlList->i1_name encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<count_chara_int_clist(charaIntCtlList);i++){
        set_from_chara_int_clist_at_index(i, charaIntCtlList, c1_out, &i1_out);
        NSNumber *num2 = [[NSNumber alloc] initWithInt:i1_out];
        NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
        NSString *data2 = [integerFormatter stringFromNumber:num2];
        self.charaIntControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
        [self.charaIntControlArray addObject:self.charaIntControlDictionary];
    };
    free(c1_out);
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:charaIntTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.charaIntTableView = [[NSTableView alloc] initWithFrame:charaIntTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.charaIntTableView addTableColumn:tCol];

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.charaIntTableView addTableColumn:tCol];
    
    [self.charaIntTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.charaIntTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.charaIntTableView setGridColor:[NSColor grayColor]];
    [self.charaIntTableView setRowHeight:23.0];
    [self.charaIntTableView setDelegate:self];
    [self.charaIntTableView setDataSource:self];
    [self.charaIntTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.charaIntTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.charaIntTableView];
    [charaIntTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToCharaIntclist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.charaIntControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.charaIntControlArray count];
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
    int i1_out;
    NSString *selectedKey = [pTableColumn identifier];
    
    
    /*
    NSString *editedtext = [self.charaIntControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.charaIntControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.charaIntControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_int_clist_at_index((int) pRowIndex, charaIntCtlList, c1_out, &i1_out);

    if([selectedKey isEqualToString:self.key1]){
        sprintf(c1_out, "%s", [pObject UTF8String]);
    }
    if([selectedKey isEqualToString:self.key2]){
        NSNumber *new_value = [integerFormatter numberFromString:pObject];
        if(new_value == nil){
            free(c1_out);
            return;
        };
        i1_out = [new_value intValue];
    }
    update_chara_int_clist_by_index((int) pRowIndex, c1_out, i1_out, charaIntCtlList);
    free(c1_out);

    [[self.charaIntControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.charaIntControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out;
    int i1_out;
    NSInteger isel = [self.charaIntTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_chara_int_clist(charaIntCtlList)) return;

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_int_clist_at_index((int) isel, charaIntCtlList, c1_out, &i1_out);
    add_chara_int_clist_before_c_tbl(c1_out, c1_out, i1_out, charaIntCtlList);
    
    set_from_chara_int_clist_at_index((int) isel, charaIntCtlList, c1_out, &i1_out);
    NSNumber *num2 = [[NSNumber alloc] initWithDouble:i1_out];
    NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
    NSString *data2 = [integerFormatter stringFromNumber:num2];
    free(c1_out);
    
    self.charaIntControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1,data2,self.key2, nil];
    [self.charaIntControlArray insertObject:self.charaIntControlDictionary atIndex:isel];

    [self.charaIntTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.charaIntTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara_int_clist(charaIntCtlList)) return;

    del_chara_int_clist_by_index((int) isel, charaIntCtlList);
    [self.charaIntControlArray removeObjectAtIndex:isel];

    [self.charaIntTableView reloadData];
}

@end

