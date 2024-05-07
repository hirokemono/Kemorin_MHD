/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A class to manage all of the Metal objects this app creates.
*/

@import Metal;
@import MetalKit;
#import "MetalSortComparison.h"

@implementation MetalSortComparison
- (instancetype) initWithDevice: (id<MTLDevice>) device
{
    _sort = [[MetalBitonicSort alloc] initWithDevice:device];
    return self;
}


long prod_padding_4096chars(long num_bytes){
    long nsize = 1 + num_bytes / 4096;
    return (4096 * nsize);
};
static long prod_padding_1024floats(int ncomp_buf){
    long nsize = 4 * ncomp_buf;
    return prod_padding_4096chars(nsize);
};

void verifySort(struct sort_float_array *rSort)
{
    long nout = rSort->narrayP2;
    if(rSort->narrayP2 > 256) nout = 128;
    for(long i=0;i<nout;i++){
        printf("%ld %f: %ld %f: \n", i, rSort->org[i],
               rSort->idx[i], rSort->ra[i]);
    }
}


- (double) sendSortCommand:(struct sort_float_array *) rSort;
{
    struct timeval startwtime;
    struct timeval endwtime;
    long i;
    
    int *intinput;
    posix_memalign((void**)&intinput, 4096, rSort->narrayP2*sizeof(int));
    if(intinput == NULL){
        printf("malloc error for intinput\n");
        exit(0);
    };
    for (i=0;i<rSort->Narray;i++) {intinput[i] = (int) i;}
    for (i=rSort->Narray;i<rSort->narrayP2;i++) {intinput[i] = -1;}
    alloc_sort_float_works(rSort);

    gettimeofday( &startwtime, NULL );
    [_sort bitonicSortCommand:rSort->nextP2
                   sortArray:rSort->ra
                  indexArray:intinput];
    for(i=0;i<rSort->narrayP2;i++) {rSort->idx[i] = intinput[i];}
    free(intinput);
    gettimeofday( &endwtime, NULL );
    verifySort(rSort);
    check_sorted_Float(rSort->narrayP2, rSort->ra);
    dealloc_sort_float_works(rSort);

    double seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                               + endwtime.tv_sec - startwtime.tv_sec );
    return seq_time;
}


@end
