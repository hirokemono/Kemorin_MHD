//
//  main.m
//  SortComparison
//
//  Created by Hiroaki Matsui on 5/6/24.
//

#include <stdlib.h>

#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import <Accelerate/Accelerate.h>

#include "quicksort_c.h"
#include "bitonic_sort_c.h"
#include "bitonic_sort_float_pthread.h"
#include "float_sorting_tests.h"

#import "MetalSortComparison.h"


int main(int argc, const char * argv[]) {
    @autoreleasepool {
        int nthreads = 32;
        long nArray = 1 << 21;
        
        struct sort_float_array *rSort = init_sort_float_array(nthreads, nArray);
        printf("nthreads %d \n", nthreads);
        printf("rSort->narrayP2 %ld \n", rSort->narrayP2);


        id<MTLDevice> device = MTLCreateSystemDefaultDevice();

        // Create the custom object used to encapsulate the Metal code.
        // Initializes objects to communicate with the GPU.
        MetalSortComparison* _bitonic = [[MetalSortComparison alloc] initWithDevice:device];

        double seq_time7 = [_bitonic sendSortCommand:rSort];
        NSLog(@"Metal Sort finished");
 

        double seq_time1 = quicksort_float_test(rSort);
        double seq_time2 = bitonicsort_rec_float_test(rSort);
        double seq_time4 = bitonicsort_imp_float_test(rSort);
        double seq_time3 = bitonicsort_pthread_float_test(rSort);
        double seq_time6 = vDSP_vsorti_test(rSort);

        dealloc_sort_float_array(rSort);

        printf("-------------------------------------\n\n");
        printf("--- Single precision sorting wall clock times ---\n");
        printf("                 Quicksort wall clock time = %f\n", seq_time1);
        printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
        printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
        printf("Bitonic by Metal shader    wall clock time = %f\n", seq_time7);
        printf("Bitonic parallel recursive with %i threads\n", nthreads);
        printf("             and quicksort wall clock time = %f\n", seq_time3);
        printf("               vDSP_vsorti wall clock time = %f\n", seq_time6);
    }
    return 0;
}
