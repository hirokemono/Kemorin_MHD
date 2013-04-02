/*
 * ISPACK FORTRAN SUBROUTINE LIBRARY FOR SCIENTIFIC COMPUTING
 * Copyright (C) 1998--2011 Keiichi Ishioka <ishioka@gfd-dennou.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
#include<cuda.h>
#include<stdio.h>
#define REDUCE(n) if(j<64){sdata[j]+=sdata[j+64];}\
	__syncthreads(); \
	if(j<32){\
	     sdata[j]+=sdata[j+32];\
	     sdata[j]+=sdata[j+16];\
	     sdata[j]+=sdata[j+8];\
	     sdata[j]+=sdata[j+4];\
	     sdata[j]+=sdata[j+2];\
	     sdata[j]+=sdata[j+1];\
	     ws[n]=sdata[0];\
	  }
/*----------------------------------------------------------------*/	  

__global__ void sjwg2s_kernel512(double *p, double *r, double *ws, double *g,
		       int im, int jh, int mm, int nm, int nn, int ipow)
{
   volatile __shared__ double sdata[128];

   int m=blockIdx.x;
   int j=threadIdx.x;
   int n;
   int ns;
   int nsr;   
   int nst;
   int nsrt;   
   double qs10=p[j];
   double qs11=p[128+j];      
   double qs12=p[256+j];                  
   double qs13=p[384+j];                              

   double qs20,qs21,qs22,qs23;
   double qs30,qs31,qs32,qs33;
   double qs40,qs41,qs42,qs43;
   double qs50,qs51,qs52,qs53;
   double qs60,qs61,qs62,qs63;
   double qs70,qs71,qs72,qs73;

   if(m == 0)
     {
	n=m;
	ns=nm;
	qs20=1;
	qs21=1;
	qs22=1;
	qs23=1;
	qs30=qs10;
	qs31=qs11;	
	qs32=qs12;
	qs33=qs13;
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh];
	     qs42=(g[im*(jh+j+256)]+g[im*(jh-(j+256)-1)])*p[j+256+jh];
	     qs43=(g[im*(jh+j+384)]+g[im*(jh-(j+384)-1)])*p[j+384+jh];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh];
	     qs52=(g[im*(jh+j+256)]-g[im*(jh-(j+256)-1)])*p[j+256+jh];
	     qs53=(g[im*(jh+j+384)]-g[im*(jh-(j+384)-1)])*p[j+384+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3];
	     qs42=(g[im*(jh+j+256)]+g[im*(jh-(j+256)-1)])*p[j+256+jh]*p[j+256+jh*3];
	     qs43=(g[im*(jh+j+384)]+g[im*(jh-(j+384)-1)])*p[j+384+jh]*p[j+384+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3];
	     qs52=(g[im*(jh+j+256)]-g[im*(jh-(j+256)-1)])*p[j+256+jh]*p[j+256+jh*3];
	     qs53=(g[im*(jh+j+384)]-g[im*(jh-(j+384)-1)])*p[j+384+jh]*p[j+384+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs42=(g[im*(jh+j+256)]+g[im*(jh-(j+256)-1)])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs43=(g[im*(jh+j+384)]+g[im*(jh-(j+384)-1)])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs52=(g[im*(jh+j+256)]-g[im*(jh-(j+256)-1)])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs53=(g[im*(jh+j+384)]-g[im*(jh-(j+384)-1)])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	  }
	__syncthreads();
	sdata[j]=qs40+qs41+qs42+qs43;
	__syncthreads();
	REDUCE(n);
   
	for(n=m+1;n<nn;n=n+2)
	  {
	     __syncthreads();		     
	     sdata[j]=qs50*qs30+qs51*qs31+qs52*qs32+qs53*qs33;
	     __syncthreads();
	     REDUCE(n);
	     qs20=qs20+r[ns+n-1]*qs10*qs30;
	     qs21=qs21+r[ns+n-1]*qs11*qs31;	     
	     qs22=qs22+r[ns+n-1]*qs12*qs32;
	     qs23=qs23+r[ns+n-1]*qs13*qs33;	     
	     
	     __syncthreads();		     	     
	     sdata[j]=qs40*qs20+qs41*qs21+qs42*qs22+qs43*qs23;
	     __syncthreads();
	     REDUCE(n+1);	     
	     
	     qs30=qs30+r[ns+n]*qs10*qs20;
	     qs31=qs31+r[ns+n]*qs11*qs21;	     
	     qs32=qs32+r[ns+n]*qs12*qs22;
	     qs33=qs33+r[ns+n]*qs13*qs23;	     
	     
	     
	  }
	if(((nn-m) % 2) == 1)
	  {
	     n=nn;
	     __syncthreads();		     	     	     
	     sdata[j]=qs50*qs30+qs51*qs31+qs52*qs32+qs53*qs33;
	     __syncthreads();
	     REDUCE(n);
	  }
     }
   else
     {
	ns=nn+2+(m-1)*(2*nn+2-m);
        nsr=(2*nm-m)*m+nm-m;
	qs20=p[j+(4+m-1)*jh];
	qs21=p[128+j+(4+m-1)*jh];      
	qs22=p[256+j+(4+m-1)*jh];                  
	qs23=p[384+j+(4+m-1)*jh];                              
	qs30=qs10*qs20;
	qs31=qs11*qs21;	
	qs32=qs12*qs22;
	qs33=qs13*qs23;
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh];
	     qs42=(g[im*(jh+j+256)+2*m]+g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh];
	     qs43=(g[im*(jh+j+384)+2*m]+g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh];
	     qs62=(g[im*(jh+j+256)+2*m]-g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh];
	     qs63=(g[im*(jh+j+384)+2*m]-g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh];
	     qs52=(g[im*(jh+j+256)+2*m+1]+g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh];
	     qs53=(g[im*(jh+j+384)+2*m+1]+g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh];
	     qs72=(g[im*(jh+j+256)+2*m+1]-g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh];
	     qs73=(g[im*(jh+j+384)+2*m+1]-g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3];
	     qs42=(g[im*(jh+j+256)+2*m]+g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh]*p[j+256+jh*3];
	     qs43=(g[im*(jh+j+384)+2*m]+g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh]*p[j+384+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3];
	     qs62=(g[im*(jh+j+256)+2*m]-g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh]*p[j+256+jh*3];
	     qs63=(g[im*(jh+j+384)+2*m]-g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh]*p[j+384+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3];
	     qs52=(g[im*(jh+j+256)+2*m+1]+g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh]*p[j+256+jh*3];
	     qs53=(g[im*(jh+j+384)+2*m+1]+g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh]*p[j+384+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3];
	     qs72=(g[im*(jh+j+256)+2*m+1]-g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh]*p[j+256+jh*3];
	     qs73=(g[im*(jh+j+384)+2*m+1]-g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh]*p[j+384+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs42=(g[im*(jh+j+256)+2*m]+g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs43=(g[im*(jh+j+384)+2*m]+g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs62=(g[im*(jh+j+256)+2*m]-g[im*(jh-(j+256)-1)+2*m])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs63=(g[im*(jh+j+384)+2*m]-g[im*(jh-(j+384)-1)+2*m])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs52=(g[im*(jh+j+256)+2*m+1]+g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs53=(g[im*(jh+j+384)+2*m+1]+g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs72=(g[im*(jh+j+256)+2*m+1]-g[im*(jh-(j+256)-1)+2*m+1])*p[j+256+jh]*p[j+256+jh*3]*p[j+256+jh*3];
	     qs73=(g[im*(jh+j+384)+2*m+1]-g[im*(jh-(j+384)-1)+2*m+1])*p[j+384+jh]*p[j+384+jh*3]*p[j+384+jh*3];
	  }
	__syncthreads();		     	     	     	
	sdata[j]=qs40*qs20+qs41*qs21+qs42*qs22+qs43*qs23;
	__syncthreads();
	REDUCE(ns-1);
	__syncthreads();		     	     	     	
	sdata[j]=qs50*qs20+qs51*qs21+qs52*qs22+qs53*qs23;
	__syncthreads();
	REDUCE(ns);
	nsrt=nsr;
	nst=ns+1;

	for(n=m+1;n<nn;n=n+2)
	  {
	     qs20=qs20+r[nsrt]*qs10*qs30;
	     qs21=qs21+r[nsrt]*qs11*qs31;	     
	     qs22=qs22+r[nsrt]*qs12*qs32;
	     qs23=qs23+r[nsrt]*qs13*qs33;	     
	     __syncthreads();		     	     	     		     
	     sdata[j]=qs60*qs30+qs61*qs31+qs62*qs32+qs63*qs33;
	     __syncthreads();
	     REDUCE(nst);
	     __syncthreads();
	     sdata[j]=qs70*qs30+qs71*qs31+qs72*qs32+qs73*qs33;
	     __syncthreads();
	     REDUCE(nst+1);
	     __syncthreads();
	     sdata[j]=qs40*qs20+qs41*qs21+qs42*qs22+qs43*qs23;
	     __syncthreads();
	     REDUCE(nst+2);
	     __syncthreads();
	     sdata[j]=qs50*qs20+qs51*qs21+qs52*qs22+qs53*qs23;
	     __syncthreads();
	     REDUCE(nst+3);
	     qs30=qs30+r[nsrt+1]*qs10*qs20;
	     qs31=qs31+r[nsrt+1]*qs11*qs21;	     
	     qs32=qs32+r[nsrt+1]*qs12*qs22;
	     qs33=qs33+r[nsrt+1]*qs13*qs23;	     
	     nsrt=nsrt+2;
	     nst=nst+4; 
	  }
	if(((nn-m) % 2) == 1){
	   n=nn;
	   __syncthreads();	     	   
	   sdata[j]=qs60*qs30+qs61*qs31+qs62*qs32+qs63*qs33;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+1);
	   __syncthreads();	     	   	   
	   sdata[j]=qs70*qs30+qs71*qs31+qs72*qs32+qs73*qs33;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+2);
	}
     }
}
   
/*----------------------------------------------------------------*/

__global__ void sjwg2s_kernel256(double *p, double *r, double *ws, double *g,
		       int im, int jh, int mm, int nm, int nn, int ipow)
{
   volatile __shared__ double sdata[128];

   int m=blockIdx.x;
   int j=threadIdx.x;
   int n;
   int ns;
   int nsr;   
   int nst;
   int nsrt;   
   double qs10=p[j];
   double qs11=p[128+j];                  

   double qs20,qs21;
   double qs30,qs31;
   double qs40,qs41;
   double qs50,qs51;
   double qs60,qs61;
   double qs70,qs71;

   if(m == 0)
     {
	n=m;
	ns=nm;
	qs20=1;
	qs21=1;
	qs30=qs10;
	qs31=qs11;	
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)]+g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)]-g[im*(jh-(j+128)-1)])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	  }
	__syncthreads();
	sdata[j]=qs40+qs41;
	__syncthreads();
	REDUCE(n);
   
	for(n=m+1;n<nn;n=n+2)
	  {
	     __syncthreads();		     
	     sdata[j]=qs50*qs30+qs51*qs31;
	     __syncthreads();
	     REDUCE(n);
	     qs20=qs20+r[ns+n-1]*qs10*qs30;
	     qs21=qs21+r[ns+n-1]*qs11*qs31;	     
	     
	     __syncthreads();		     	     
	     sdata[j]=qs40*qs20+qs41*qs21;
	     __syncthreads();
	     REDUCE(n+1);	     
	     
	     qs30=qs30+r[ns+n]*qs10*qs20;
	     qs31=qs31+r[ns+n]*qs11*qs21;	     
	     
	  }
	if(((nn-m) % 2) == 1)
	  {
	     n=nn;
	     __syncthreads();		     	     	     
	     sdata[j]=qs50*qs30+qs51*qs31;
	     __syncthreads();
	     REDUCE(n);
	  }
     }
   else
     {
	ns=nn+2+(m-1)*(2*nn+2-m);
        nsr=(2*nm-m)*m+nm-m;
	qs20=p[j+(4+m-1)*jh];
	qs21=p[128+j+(4+m-1)*jh];                  
	qs30=qs10*qs20;
	qs31=qs11*qs21;	
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs41=(g[im*(jh+j+128)+2*m]+g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs61=(g[im*(jh+j+128)+2*m]-g[im*(jh-(j+128)-1)+2*m])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs51=(g[im*(jh+j+128)+2*m+1]+g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs71=(g[im*(jh+j+128)+2*m+1]-g[im*(jh-(j+128)-1)+2*m+1])*p[j+128+jh]*p[j+128+jh*3]*p[j+128+jh*3];
	  }
	__syncthreads();
	sdata[j]=qs40*qs20+qs41*qs21;
	__syncthreads();
	REDUCE(ns-1);
	__syncthreads();		     	     	     	
	sdata[j]=qs50*qs20+qs51*qs21;
	__syncthreads();
	REDUCE(ns);
	nsrt=nsr;
	nst=ns+1;

	for(n=m+1;n<nn;n=n+2)
	  {
	     qs20=qs20+r[nsrt]*qs10*qs30;
	     qs21=qs21+r[nsrt]*qs11*qs31;	     
	     __syncthreads();		     	     	     		     
	     sdata[j]=qs60*qs30+qs61*qs31;
	     __syncthreads();
	     REDUCE(nst);
	     __syncthreads();
	     sdata[j]=qs70*qs30+qs71*qs31;
	     __syncthreads();
	     REDUCE(nst+1);
	     __syncthreads();
	     sdata[j]=qs40*qs20+qs41*qs21;
	     __syncthreads();
	     REDUCE(nst+2);
	     __syncthreads();
	     sdata[j]=qs50*qs20+qs51*qs21;
	     __syncthreads();
	     REDUCE(nst+3);
	     qs30=qs30+r[nsrt+1]*qs10*qs20;
	     qs31=qs31+r[nsrt+1]*qs11*qs21;	     
	     nsrt=nsrt+2;
	     nst=nst+4; 
	  }
	if(((nn-m) % 2) == 1){
	   n=nn;
	   __syncthreads();	     	   
	   sdata[j]=qs60*qs30+qs61*qs31;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+1);
	   __syncthreads();	     	   	   
	   sdata[j]=qs70*qs30+qs71*qs31;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+2);
	}
     }
}
   
/*----------------------------------------------------------------*/

__global__ void sjwg2s_kernel128(double *p, double *r, double *ws, double *g,
		       int im, int jh, int mm, int nm, int nn, int ipow)
{
   volatile __shared__ double sdata[128];

   int m=blockIdx.x;
   int j=threadIdx.x;
   int n;
   int ns;
   int nsr;   
   int nst;
   int nsrt;   
   double qs10=p[j];

   double qs20;
   double qs30;
   double qs40;
   double qs50;
   double qs60;
   double qs70;

   if(m == 0)
     {
	n=m;
	ns=nm;
	qs20=1;
	qs30=qs10;
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)]+g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs50=(g[im*(jh+j)]-g[im*(jh-j-1)])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	  }
	__syncthreads();
	sdata[j]=qs40;
	__syncthreads();
	REDUCE(n);
   
	for(n=m+1;n<nn;n=n+2)
	  {
	     __syncthreads();		     
	     sdata[j]=qs50*qs30;
	     __syncthreads();
	     REDUCE(n);
	     qs20=qs20+r[ns+n-1]*qs10*qs30;
	     
	     __syncthreads();		     	     
	     sdata[j]=qs40*qs20;
	     __syncthreads();
	     REDUCE(n+1);	     
	     
	     qs30=qs30+r[ns+n]*qs10*qs20;
	     
	  }
	if(((nn-m) % 2) == 1)
	  {
	     n=nn;
	     __syncthreads();		     	     	     
	     sdata[j]=qs50*qs30;
	     __syncthreads();
	     REDUCE(n);
	  }
     }
   else
     {
	ns=nn+2+(m-1)*(2*nn+2-m);
        nsr=(2*nm-m)*m+nm-m;
	qs20=p[j+(4+m-1)*jh];
	qs30=qs10*qs20;
	if(ipow == 0)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh];
	  }
	else if(ipow == 1)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3];
	  }
	else if(ipow == 2)
	  {
	     qs40=(g[im*(jh+j)+2*m]+g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs60=(g[im*(jh+j)+2*m]-g[im*(jh-j-1)+2*m])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs50=(g[im*(jh+j)+2*m+1]+g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	     qs70=(g[im*(jh+j)+2*m+1]-g[im*(jh-j-1)+2*m+1])*p[j+jh]*p[j+jh*3]*p[j+jh*3];
	  }
	__syncthreads();
	sdata[j]=qs40*qs20;
	__syncthreads();
	REDUCE(ns-1);
	__syncthreads();		     	     	     	
	sdata[j]=qs50*qs20;
	__syncthreads();
	REDUCE(ns);
	nsrt=nsr;
	nst=ns+1;

	for(n=m+1;n<nn;n=n+2)
	  {
	     qs20=qs20+r[nsrt]*qs10*qs30;
	     __syncthreads();		     	     	     		     
	     sdata[j]=qs60*qs30;
	     __syncthreads();
	     REDUCE(nst);
	     __syncthreads();
	     sdata[j]=qs70*qs30;
	     __syncthreads();
	     REDUCE(nst+1);
	     __syncthreads();
	     sdata[j]=qs40*qs20;
	     __syncthreads();
	     REDUCE(nst+2);
	     __syncthreads();
	     sdata[j]=qs50*qs20;
	     __syncthreads();
	     REDUCE(nst+3);
	     qs30=qs30+r[nsrt+1]*qs10*qs20;
	     nsrt=nsrt+2;
	     nst=nst+4; 
	  }
	if(((nn-m) % 2) == 1){
	   n=nn;
	   __syncthreads();	     	   
	   sdata[j]=qs60*qs30;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+1);
	   __syncthreads();	     	   	   
	   sdata[j]=qs70*qs30;
	   __syncthreads();
	   REDUCE(ns+(n-m-1)*2+2);
	}
     }
}
   
/*----------------------------------------------------------------*/

extern "C" void sjwg2s_(int *mm, int *nm, int *nn, int *im, int *jm, 
			double *p, double *r, double *ws, double *g,
			int *ipow, long *ip)
{
   int jh = *jm/2;
   size_t sizews,sizeg;
   double *pd;
   double *rd;
   double *wsd;         
   double *gd;

   pd=(double *) ip[0];
   rd=(double *) ip[1];
   wsd=(double *) ip[2];
   gd=(double *) ip[3];

   sizeg=sizeof(double)*(*jm)*(*im);
   sizews=sizeof(double)*((2*(*nn)+1-*mm)*(*mm)+(*nn)+1);

   cudaMemcpy(gd,g,sizeg,cudaMemcpyHostToDevice);
   
   if(jh == 512)
     {
	sjwg2s_kernel512<<<(*mm+1),128>>>(pd,rd,wsd,gd,*im,jh,*mm,*nm,*nn,*ipow);
     }
   else if(jh == 256)
     {
	sjwg2s_kernel256<<<(*mm+1),128>>>(pd,rd,wsd,gd,*im,jh,*mm,*nm,*nn,*ipow);
     }
   else if(jh == 128)
     {
	sjwg2s_kernel128<<<(*mm+1),128>>>(pd,rd,wsd,gd,*im,jh,*mm,*nm,*nn,*ipow);
     }
   else
     {
        cudaFree(pd);
        cudaFree(rd);   
	cudaFree(wsd);
	cudaFree(gd);
	puts("***** ERROR (SJWG2S) ***  This value of JM is not supported.");	
        exit(1);
     }
   
   cudaMemcpy(ws,wsd,sizews,cudaMemcpyDeviceToHost);
   
   cudaThreadSynchronize(); 
}

/*----------------------------------------------------------------*/

__global__ void sjws2g_kernel(double *p, double *r, double *ws, double *g,
		       int im, int jh, int mm, int nm, int nn, int ipow)
{
   int m=blockIdx.x;
   int j=threadIdx.x;
   int i;
   int n;
   int ns;
   int nsr;   
   int nst;
   int nsrt;   
   double qs1=p[j];
   double qs2;   
   double qs3;      
   double qs4;         
   double qs5;            
   double qs6=0;   
   double qs7=0;

   if(m == 0)
     {
	n=m;
	ns=nm;
	qs2=1;
	qs3=qs1;
	qs4=ws[n];
	qs5=0;

	for(n=m+1;n<nn;n=n+2)
	  {
	     qs5=qs5+ws[n]*qs3;
	     qs2=qs2+r[ns+n-1]*qs1*qs3;
	     qs4=qs4+ws[n+1]*qs2;
	     qs3=qs3+r[ns+n]*qs1*qs2;
	  }
	if(((nn-m) % 2) == 1){
	   n=nn;
	   qs5=qs5+ws[n]*qs3;
	}
	if(ipow == 0)
	  {
	     g[im*(jh+j)]  =(qs4+qs5);
	     g[im*(jh-j-1)]=(qs4-qs5);
	  }
	else if(ipow == 1)
	  {
	     g[im*(jh+j)]  =(qs4+qs5)*p[jh*3+j];
	     g[im*(jh-j-1)]=(qs4-qs5)*p[jh*3+j];
	  }
	else if(ipow == 2)
	  {
	     g[im*(jh+j)]  =(qs4+qs5)*p[jh*3+j]*p[jh*3+j];
	     g[im*(jh-j-1)]=(qs4-qs5)*p[jh*3+j]*p[jh*3+j];
	  }
	
	g[im*j+1]     =0;     
	g[im*(jh+j)+1]=0;	
	for(i=2*(mm+1);i<im;i++)
	  {
	     g[im*j+i]     =0;
	     g[im*(jh+j)+i]=0;
	  }
     }
   else
     {
	ns=nn+2+(m-1)*(2*nn+2-m);
        nsr=(2*nm-m)*m+nm-m;
	qs2=p[j+(4+m-1)*jh];
	qs3=qs1*qs2;
	qs4=ws[ns-1]*qs2;
	qs5=ws[ns  ]*qs2;	
	qs6=0;
	qs7=0;
	
	nsrt=nsr;
	nst=ns+1;

	for(n=m+1;n<nn;n=n+2)
	  {
	     qs2=qs2+r[nsrt]*qs1*qs3;	     
	     qs6=qs6+ws[nst]*qs3;
	     qs7=qs7+ws[nst+1]*qs3;
	     qs4=qs4+ws[nst+2]*qs2;
	     qs5=qs5+ws[nst+3]*qs2;
	     qs3=qs3+r[nsrt+1]*qs1*qs2;	     	     
	     nsrt=nsrt+2;
	     nst=nst+4; 
	  }
	if(((nn-m) % 2) == 1){
	   n=nn;
	   qs6=qs6+ws[ns+(n-m-1)*2+1]*qs3;
	   qs7=qs7+ws[ns+(n-m-1)*2+2]*qs3; 
	}
	if(ipow == 0)
	  {
	     g[im*(jh+j)+2*m  ]  =(qs4+qs6);
	     g[im*(jh+j)+2*m+1]  =(qs5+qs7);
	     g[im*(jh-j-1)+2*m]  =(qs4-qs6);
	     g[im*(jh-j-1)+2*m+1]=(qs5-qs7);
	  }
	else if(ipow == 1)
	  {
	     g[im*(jh+j)+2*m  ]  =(qs4+qs6)*p[jh*3+j];
	     g[im*(jh+j)+2*m+1]  =(qs5+qs7)*p[jh*3+j];
	     g[im*(jh-j-1)+2*m]  =(qs4-qs6)*p[jh*3+j];
	     g[im*(jh-j-1)+2*m+1]=(qs5-qs7)*p[jh*3+j];
	  }
	else if(ipow == 2)
	  {
	     g[im*(jh+j)+2*m  ]  =(qs4+qs6)*p[jh*3+j]*p[jh*3+j];
	     g[im*(jh+j)+2*m+1]  =(qs5+qs7)*p[jh*3+j]*p[jh*3+j];
	     g[im*(jh-j-1)+2*m]  =(qs4-qs6)*p[jh*3+j]*p[jh*3+j];
	     g[im*(jh-j-1)+2*m+1]=(qs5-qs7)*p[jh*3+j]*p[jh*3+j];
	  }
     }
}
   
/*----------------------------------------------------------------*/

extern "C" void sjws2g_(int *mm, int *nm, int *nn, int *im, int *jm, 
			double *p, double *r, double *ws, double *g,
			int *ipow, long *ip)
{
   int jh = *jm/2;
   size_t sizews,sizeg;
   double *pd;
   double *rd;
   double *wsd;         
   double *gd;

   pd=(double *) ip[0];
   rd=(double *) ip[1];
   wsd=(double *) ip[2];
   gd=(double *) ip[3];

   sizeg=sizeof(double)*(*jm)*(*im);
   sizews=sizeof(double)*((2*(*nn)+1-*mm)*(*mm)+(*nn)+1);

   cudaMemcpy(wsd,ws,sizews,cudaMemcpyHostToDevice); 
   
   sjws2g_kernel<<<(*mm+1),jh>>>(pd,rd,wsd,gd,*im,jh,*mm,*nm,*nn,*ipow);

   cudaMemcpy(g,gd,sizeg,cudaMemcpyDeviceToHost);
   
   cudaThreadSynchronize(); 
}

/*----------------------------------------------------------------*/

extern "C" void sjvopn_(int *mm, int *nm, int *jm, int *im, 
			double *p, double *r, long *ip)
{
   int jh = *jm/2;
   size_t sizep,sizer,sizews,sizeg;
   double *pd;
   double *rd;
   double *wsd;
   double *gd;   

   sizep=sizeof(double)*jh*(*mm+4);
   sizer=sizeof(double)*((*mm+1)*(2*(*nm)-(*mm)-1)+1);
   sizews=sizeof(double)*((2*(*nm)+1-*mm)*(*mm)+(*nm)+1);
   sizeg=sizeof(double)*(*jm)*(*im);

   cudaMalloc((void **)&pd,sizep);
   cudaMalloc((void **)&rd,sizer);
   cudaMalloc((void **)&wsd,sizews);
   cudaMalloc((void **)&gd,sizeg);
   
   cudaMemcpy(pd,p,sizep,cudaMemcpyHostToDevice);
   cudaMemcpy(rd,r,sizer,cudaMemcpyHostToDevice);
   
   ip[0]=(long)pd;
   ip[1]=(long)rd;   
   ip[2]=(long)wsd;
   ip[3]=(long)gd;
}

/*----------------------------------------------------------------*/
   
extern "C" void sjvcls_(long *ip)
{
   double *pd;
   double *rd;
   double *wsd;         
   double *gd;   
   
   pd=(double *) ip[0];
   rd=(double *) ip[1];
   wsd=(double *) ip[2];
   gd=(double *) ip[3];

   cudaFree(pd);
   cudaFree(rd);   
   cudaFree(wsd);
   cudaFree(gd);
}
   
