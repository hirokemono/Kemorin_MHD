/*
//  CharaControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "CharaControlTableview.h"

@implementation CharaControlTableview
@synthesize charaControlDictionary;
@synthesize charaControlArray;
@synthesize charaTableView;
@synthesize key1;

-(void)linkToCharaclist
{
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
    if(f_MHD_ctl == NULL){
        printf("malloc error for f_MHD_ctl\n");
        exit(0);
    };
    f_MHD_ctl->f_self = c_read_control_sph_SGS_MHD("control_MHD");
    CharaCtlList = f_MHD_ctl->f_model_ctl->f_frc_ctl->f_force_names;
}
-(void)createMutablearray
{
    int i;
    char *c1_out;
    
    self.charaControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:CharaCtlList->c1_name encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<count_chara_clist(CharaCtlList);i++){
        set_from_chara_clist_at_index(i, CharaCtlList, c1_out);
        NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
        self.charaControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1, nil];
        [self.charaControlArray addObject:self.charaControlDictionary];
    };
    free(c1_out);
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:charaTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.charaTableView = [[NSTableView alloc] initWithFrame:charaTableViewOutlet.bounds];
    NSTableColumn *tCol;

    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:60.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.charaTableView addTableColumn:tCol];

    [self.charaTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.charaTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.charaTableView setGridColor:[NSColor grayColor]];
    [self.charaTableView setRowHeight:23.0];
    [self.charaTableView setDelegate:self];
    [self.charaTableView setDataSource:self];
    [self.charaTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.charaTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.charaTableView];
    [charaTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self linkToCharaclist];
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.charaControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.charaControlArray count];
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
    NSString *selectedKey = [pTableColumn identifier];
    
    
    /*
    NSString *editedtext = [self.charaControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.charaControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.charaControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_clist_at_index((int) pRowIndex, CharaCtlList, c1_out);

    if([selectedKey isEqualToString:self.key1]){
        sprintf(c1_out, "%s", [pObject UTF8String]);
    }
    update_chara_clist_by_index((int) pRowIndex, c1_out, CharaCtlList);
    free(c1_out);

    [[self.charaControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    
//    NSLog(@"Mutablearray again  %@",[self.charaControlArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    char *c1_out;
    NSInteger isel = [self.charaTableView selectedRow];
    
    if(isel < 1) return;
    if(isel >= count_chara_clist(CharaCtlList)) return;

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    set_from_chara_clist_at_index((int) isel, CharaCtlList, c1_out);
    add_chara_clist_before_c_tbl(c1_out, c1_out, CharaCtlList);
    
    set_from_chara_clist_at_index((int) isel, CharaCtlList, c1_out);
    NSString *data1 = [NSString stringWithCString:c1_out encoding:NSUTF8StringEncoding];
    free(c1_out);
    
    self.charaControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:data1,self.key1, nil];
    [self.charaControlArray insertObject:self.charaControlDictionary atIndex:isel];

    [self.charaTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.charaTableView selectedRow];
    
    if(isel < 0) return;
    if(isel >= count_chara_clist(CharaCtlList)) return;

    del_chara_clist_by_index((int) isel, CharaCtlList);
    [self.charaControlArray removeObjectAtIndex:isel];

    [self.charaTableView reloadData];
}

@end

