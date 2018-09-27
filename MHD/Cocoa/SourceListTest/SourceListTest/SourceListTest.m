//
//  SourceListTest.m
//  SourceListTest
//
//  Created by Hiroaki Matsui on 2018/09/06.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import "SourceListTest.h"

@implementation SourceListTest
@synthesize sourceListItems;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    [self performSelector:@selector(expandSourceList) withObject:nil afterDelay:0.0];
}

- (IBAction)expandSourceList
{
    //If Expand Children is set to NO, All the Groups will be displayed as a collapsed view
    [sourceListOutlineView expandItem:nil expandChildren:YES];
}

-(void)awakeFromNib
{
    //In this method we have created 3 groups and have added Items to each group.
    //If the List of Items is static it can also be read from a Plist File. 
    self.sourceListItems = [[NSMutableArray alloc] init];
    
    SourcelistItem *groupOne = [SourcelistItem itemWithTitle:@"GROUP 1" identifier:@"headerOne"];
    SourcelistItem *groupTwo = [SourcelistItem itemWithTitle:@"GROUP 2" identifier:@"headerOne"];
    SourcelistItem *groupThree = [SourcelistItem itemWithTitle:@"GROUP 3" identifier:@"headerOne"];
    
    SourcelistItem *kItemOne      = [SourcelistItem itemWithTitle:@"Item One" identifier:@"itemOne"];                   [kItemOne setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemTwo      = [SourcelistItem itemWithTitle:@"Item Two" identifier:@"item"];                      [kItemTwo setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemThree    = [SourcelistItem itemWithTitle:@"Item Three" identifier:@"item"];                    [kItemThree setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemFour     = [SourcelistItem itemWithTitle:@"Item Four" identifier:@"item"];                     [kItemFour setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemFive     = [SourcelistItem itemWithTitle:@"Item Five" identifier:@"item"];                     [kItemFive setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemSix      = [SourcelistItem itemWithTitle:@"Item Six" identifier:@"item"];                      [kItemSix setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemSeven    = [SourcelistItem itemWithTitle:@"Item Seven" identifier:@"item"];                    [kItemSeven setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemEight    = [SourcelistItem itemWithTitle:@"Item Eight" identifier:@"item"];                    [kItemEight setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemNine     = [SourcelistItem itemWithTitle:@"Item Nine" identifier:@"item"];                     [kItemNine setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *kItemTen      = [SourcelistItem itemWithTitle:@"Item Ten" identifier:@"item"];                      [kItemTen setIcon:[NSImage imageNamed:@"time.png"]];
    
    [groupOne setChildren:[NSArray arrayWithObjects:
                           kItemOne,
                           kItemTwo,
                           kItemThree,
                           kItemFour,
                           kItemFive,
                           kItemSix,
                           kItemSeven,
                           kItemEight,
                           kItemNine,
                           kItemTen,
                           nil]];
    
    SourcelistItem *pAreaOne      = [SourcelistItem itemWithTitle:@"G2 Item One" identifier:@"item"];           [pAreaOne setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *pAreaTwo      = [SourcelistItem itemWithTitle:@"G2 Item Two" identifier:@"item"];           [pAreaTwo setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *pAreaThree    = [SourcelistItem itemWithTitle:@"G2 Item Three" identifier:@"item"];         [pAreaThree setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *pAreaFour     = [SourcelistItem itemWithTitle:@"G2 Item Four" identifier:@"item"];          [pAreaFour setIcon:[NSImage imageNamed:@"time.png"]];
    SourcelistItem *pAreaFive     = [SourcelistItem itemWithTitle:@"G2 Item Five" identifier:@"item"];          [pAreaFive setIcon:[NSImage imageNamed:@"time.png"]];
    
    [groupTwo setChildren:[NSArray arrayWithObjects:
                           pAreaOne,
                           pAreaTwo,
                           pAreaThree,
                           pAreaFour,
                           pAreaFive,
                           nil]];
    
    [self.sourceListItems addObject:groupOne];
    [self.sourceListItems addObject:groupTwo];
    [self.sourceListItems addObject:groupThree];
}

#pragma mark OUTLINE VIEW DELEGATE & DATASOURCE
- (NSInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item 
{
    if(item==nil) 
    {
        return [self.sourceListItems count];
    }
    else {
        return [[item children] count];
    }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
    return [item hasChildren];
}

- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(id)item
{
    if(item==nil) 
    {
        return [self.sourceListItems objectAtIndex:index];
    }
    else 
    {
        return [[item children] objectAtIndex:index];
    }
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
    return [item title];
}

- (void)outlineView:(NSOutlineView *)outlineView setObjectValue:(id)object forTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
    // This method needs to be implemented if the SourceList is editable. e.g Changing the name of a Playlist in iTunes
    //[item setTitle:object];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView shouldEditTableColumn:(NSTableColumn *)tableColumn item:(id)item 
{
    //Making the Source List Items Non Editable
    return NO;
}

- (NSCell *)outlineView:(NSOutlineView *)outlineView dataCellForTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
    NSInteger row = [outlineView rowForItem:item];
    return [tableColumn dataCellForRow:row];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isGroupItem:(id)item
{
    if ([[item identifier] isEqualToString:@"headerOne"])
    {
        return YES;
    }
    else 
    {
        return NO;
    }
    return NO;
    
}

- (NSView *)outlineView:(NSOutlineView *)outlineView viewForTableColumn:(NSTableColumn *)tableColumn item:(id)item {
    // Different Source List Items can have completely different UI based on the Item Type. In this sample we have only two types of views (Header and Data Cell). One can have multiple types of data cells.
    // If there is a need to have more than one type of Data Cells. It can be done in this method
    NSTableCellView *view = nil;
    if ([[item identifier] isEqualToString:@"headerOne"] || [[item identifier] isEqualToString:@"headerTwo"])
    {
        view = [outlineView makeViewWithIdentifier:@"HeaderCell" owner:self];
    }
    else {
        view = [outlineView makeViewWithIdentifier:@"DataCell" owner:self];
        [[view imageView] setImage:[item icon]];
    }
    [[view textField] setStringValue:[item title]];
    return view;
}

- (BOOL)outlineView:(NSOutlineView *)outlineView shouldSelectItem:(id)item
{
    //Here we are restricting users for selecting the Header/ Groups. Only the Data Cell Items can be selected. The group headers can only be shown or hidden. 
    if ([outlineView parentForItem:item])
    {
        return YES;
    }
    return NO;
}

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
    NSIndexSet *selectedIndexes = [sourceListOutlineView selectedRowIndexes];
    if([selectedIndexes count]>1)
    {
        //This is required only when multi-select is enabled in the SourceList/ Outline View and we are allowing users to do an action on multiple items
    }
    else {
        //Add code here for triggering an action on change of SourceList selection.
        //e.g: Loading the list of songs on changing the playlist selection
    }
}

@end

