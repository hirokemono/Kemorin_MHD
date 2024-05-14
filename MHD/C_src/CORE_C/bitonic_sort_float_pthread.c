/*
 bitonic_sort_float_pthread.c

 This file contains the bitonic sort using pthread
         bitonicsort_Float_Pthread(int nthreads, long num, float *res, long *idx);
 

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#include <pthread.h>

#include "bitonic_sort_float_pthread.h"

typedef struct{
    int id;
    int nthreads;
    
    long lo;
    long hi;
    int iflag_ascend;
    int maxlayers;
    int layer;
    
    float  *res;
    float  *rmax;
    
    long *idx;
} args_pthread_float_sort;


/** Procedure bitonic_Int_Merge
 *  Same as serial, but uses pthreads.
 **/
static void * max_Float_Array_each_thread(void *args)
{
    int id =  ((args_pthread_float_sort *) args)->id;
    long lo = ((args_pthread_float_sort *) args)->lo;
    long hi = ((args_pthread_float_sort *) args)->hi;
    float *res =   ((args_pthread_float_sort *) args)->res;
    float *rmax =  ((args_pthread_float_sort *) args)->rmax;
    
    rmax[id] = res[lo];
    for(long i=lo+1;i<hi;i++){
        if(res[i] > rmax[id]){rmax[id] = res[i];};
    };
    return 0;
}


float max_Float_Array_pthreads(int nthreads, long num, float *res){
    int i;
    /* Allocate thread arguments. */
    args_pthread_float_sort *args 
            = (args_pthread_float_sort *) malloc (nthreads * sizeof(args_pthread_float_sort));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_float_sort.\n"); exit(1);}
    
    float *rmax = (float *) malloc (nthreads * sizeof(float));
    if (!rmax) {fprintf (stderr, "Malloc failed for rmax.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = num * i /     nthreads;
        args[i].hi = num * (i+1) / nthreads;
        args[i].res = res;
        args[i].rmax = rmax;
        pthread_create(&thread_handles[i], NULL, max_Float_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(thread_handles);
    free(args);
    
    return max_float_array((long) nthreads, rmax);
}


static void * flip_sign_Float_each_thread(void *args)
{
    long lo =      ((args_pthread_float_sort *) args)->lo;
    long hi =      ((args_pthread_float_sort *) args)->hi;
    float *res =   ((args_pthread_float_sort *) args)->res;
    
    for(long i=lo;i<hi;i++){res[i] = -res[i];};
    return 0;
}

void flip_sign_Float_pthreads(int nthreads, long num, float *res){
    int i;
    /* Allocate thread arguments. */
    args_pthread_float_sort *args
            = (args_pthread_float_sort *) malloc (nthreads * sizeof(args_pthread_float_sort));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_float_sort.\n"); exit(1);}
    /* Thread-related variables. */
    pthread_t *thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].hi = num;
        args[i].res = res;
/* Create a thread. */
        pthread_create(&thread_handles[i], NULL, flip_sign_Float_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(thread_handles);
    free(args);
    return;
}


static void * bitonicMerge_Float_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread_float_sort *) arg)->lo;
    long hi = ((args_pthread_float_sort *) arg)->hi;
    int iflag_ascend = ((args_pthread_float_sort *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread_float_sort *) arg)->maxlayers;
    int layer =  ((args_pthread_float_sort *) arg)->layer;
    float *res = ((args_pthread_float_sort *) arg)->res;
    long *idx =  ((args_pthread_float_sort *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (res[i]>res[i+k])){
                exchange_float(&res[i], &res[i+k]);
                exchange_long(&idx[i],   &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Float_Merge(lo,     k, iflag_ascend, res, idx);
            bitonic_Float_Merge((lo+k), k, iflag_ascend, res, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread_float_sort arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.res = res;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread_float_sort arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.res = res;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Float_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Float_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}


/** function rec_Float_BitonicSort_pthread() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonic_Int_Merge to make them in the same order 

    Uses pthreads
 **/

static void * rec_Float_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread_float_sort *) arg)->lo;
    long hi = ((args_pthread_float_sort *) arg)->hi;
    int iflag_ascend = ((args_pthread_float_sort *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread_float_sort *) arg)->maxlayers;
    int layer = ((args_pthread_float_sort *) arg)->layer;
    float *res = ((args_pthread_float_sort *) arg)->res;
    long *idx =  ((args_pthread_float_sort *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_real_c(&res[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_float_sign(k, &res[lo+k]);
            quicksort_real_c(&res[lo+k], &idx[lo+k], 0, (k-1));
            flip_float_sign(k, &res[lo+k]);
        }
        else{
            args_pthread_float_sort arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.res = res;
            arg1.idx =  idx;
            
            args_pthread_float_sort arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.res = res;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Float_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Float_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread_float_sort arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.res = res;
        arg3.idx = idx;
        bitonicMerge_Float_pthread(&arg3);
    }
    return 0;
}



/** bitonicsort_Float_Pthread sort() 
   Caller of rec_Int_BitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/

void bitonicsort_Float_Pthread(int nthreads, long num, float *res, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread_float_sort arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.res = res;
    arg.idx = idx;
    
    rec_Float_BitonicSort_pthread(&arg);
}

