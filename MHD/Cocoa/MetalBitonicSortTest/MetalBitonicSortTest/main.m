/*
//  main.m
//  MetalBitonicSortTest
//
//  Created by Hiroaki Matsui on 5/5/24.
*/

#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import "MetalBitonicSort.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();

        // Create the custom object used to encapsulate the Metal code.
        // Initializes objects to communicate with the GPU.
        MetalBitonicSort* _bitonic = [[MetalBitonicSort alloc] initWithDevice:device];

        struct sortdata *_d;
        if((_d = (struct sortdata *) malloc(sizeof(struct sortdata))) == NULL) {
            printf("malloc error for sortdata\n");
            exit(0);
        }
        [_bitonic prepareSort:_d];
        [_bitonic sendSortCommand:_d];
        NSLog(@"Sort finished");
 
    }
    return 0;
}
