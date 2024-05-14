/*
 bitonic_sort_int_pthread.c

 This file contains the bitonic sort using pthread
         bitonicsort_Int_Pthread(int nthreads, long num, int *ires, long *idx);

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#include <pthread.h>

#include "bitonic_sort_int_pthread.h"

typedef struct{
    int id;
    int nthreads;
    
    long lo;
    long hi;
    int iflag_ascend;
    int maxlayers;
    int layer;
    
    int  *ires;
    int  *imax;
    
    long *idx;
} args_pthread_int_sort;


static void * max_Int_Array_each_thread(void *args)
{
    int id =  ((args_pthread_int_sort *) args)->id;
    long lo = ((args_pthread_int_sort *) args)->lo;
    long hi = ((args_pthread_int_sort *) args)->hi;
    int *ires = ((args_pthread_int_sort *) args)->ires;
    int *imax = ((args_pthread_int_sort *) args)->imax;
    
    imax[id] = ires[lo];
    for(long i=lo+1;i<hi;i++){
        if(ires[i] > imax[id]){imax[id] = ires[i];};
    };
    return 0;
}

int max_Int_Array_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    args_pthread_int_sort *args
            = (args_pthread_int_sort *) malloc (nthreads * sizeof(args_pthread_int_sort));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_int_sort.\n"); exit(1);}
    int *imax = (int *) malloc (nthreads * sizeof(int));
    if (!imax) {fprintf (stderr, "Malloc failed for imax.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].lo = num * i /     nthreads;
        args[i].hi = num * (i+1) / nthreads;
        args[i].ires = ires;
        args[i].imax = imax;
        pthread_create(&thread_handles[i], NULL, max_Int_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(thread_handles);
    free(args);
    return max_int_array((long) nthreads, imax);
}

static void * flip_sign_Int_each_thread(void *args)
{
    long lo =      ((args_pthread_int_sort *) args)->lo;
    long hi =      ((args_pthread_int_sort *) args)->hi;
    int *ires =    ((args_pthread_int_sort *) args)->ires;
    
    for(int i=lo;i<hi;i++){ires[i] = -ires[i];};
    return 0;
}


void flip_sign_Int_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    args_pthread_int_sort *args
            = (args_pthread_int_sort *) malloc (nthreads * sizeof(args_pthread_int_sort));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_int_sort.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].lo = num * i /     nthreads;
        args[i].hi = num * (i+1) / nthreads;
        args[i].ires = ires;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_Int_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(thread_handles);
    free(args);
    return;
}


static void * bitonicMerge_Int_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread_int_sort *) arg)->lo;
    long hi = ((args_pthread_int_sort *) arg)->hi;
    int iflag_ascend = ((args_pthread_int_sort *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread_int_sort *) arg)->maxlayers;
    int layer = ((args_pthread_int_sort *) arg)->layer;
    int *ires = ((args_pthread_int_sort *) arg)->ires;
    long *idx = ((args_pthread_int_sort *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Int_Merge(lo,     k, iflag_ascend, ires, idx);
            bitonic_Int_Merge((lo+k), k, iflag_ascend, ires, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread_int_sort arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.ires = ires;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread_int_sort arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.ires = ires;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Int_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Int_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}

/** function rec_Int_BitonicSort_pthread() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonic_Int_Merge to make them in the same order 

    Uses pthreads
 **/

static void * rec_Int_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread_int_sort *) arg)->lo;
    long hi = ((args_pthread_int_sort *) arg)->hi;
    int iflag_ascend = ((args_pthread_int_sort *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread_int_sort *) arg)->maxlayers;
    int layer = ((args_pthread_int_sort *) arg)->layer;
    int *ires = ((args_pthread_int_sort *) arg)->ires;
    long *idx = ((args_pthread_int_sort *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_int_c(&ires[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_int_sign(k, &ires[lo+k]);
            quicksort_int_c(&ires[lo+k], &idx[lo+k], 0, (k-1));
            flip_int_sign(k, &ires[lo+k]);
        }
        else{
            args_pthread_int_sort arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.ires = ires;
            arg1.idx =  idx;
            
            args_pthread_int_sort arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.ires = ires;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Int_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Int_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread_int_sort arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.ires = ires;
        arg3.idx =  idx;
        bitonicMerge_Int_pthread(&arg3);
    }
    return 0;
}

/** function sort() 
   Caller of rec_Int_BitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void bitonicsort_Int_Pthread(int nthreads, long num, int *ires, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread_int_sort arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.ires = ires;
    arg.idx = idx;
    
    rec_Int_BitonicSort_pthread(&arg);
}

