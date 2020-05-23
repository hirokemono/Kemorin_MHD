//
// Compute jump ahead coefficients for Mersenne Twister RNG.
// Ken-Ichi Ishikawa [ishikawa[at]theo.phys.sci.hiroshima-u.ac.jp]
//
// See also: 
//  H. Haramoto, M. Matsumoto, T. Nishimura, F. Panneton, and P. L'Ecuyer, 
//   ``Efficient Jump Ahead for F_2-Linear Random Number Generators'', 
//  GERAD Report G-2006-62. INFORMS Journal on Computing, 20, 3 (2008), 385-390. 
//
// This routine uses;
//  Fast arithmetic in GF(2)[x], [http://wwwmaths.anu.edu.au/~brent/software.html]
//  NTL : A Library for doing Number Theory, [http://www.shoup.net/ntl/index.html]
//
//
// Copyright (c) 2010, Ken-Ichi Ishikawa [ishikawa[at]theo.phys.sci.hiroshima-u.ac.jp]
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// 
// * Redistributions of source code must retain the above copyright
//   notice, this list of conditions and the following disclaimer. 
//   
// * Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer listed
//   in this license in the documentation and/or other materials
//   provided with the distribution.
//   
// * Neither the name of the copyright holders nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
//   
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT  
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
// 

#include <NTL/GF2X.h>
NTL_CLIENT

//#define _DEBUG_
#ifdef _DEBUG_
#define DEBUG_PRINTF(format, args...)  fprintf(stderr, format, ## args)
#else
#define DEBUG_PRINTF(format, args...)  ;
#endif

void print_hex(GF2X &f)
{
    int ww = 32;
    int nb = NumBits(f);
    int nn = (int)ceil((double)nb/(double)ww);
    unsigned int pp[nn];
    for(int i = 0; i < nn ; i++) pp[i] = 0;
    for(int i = 0; i < nb ; i++) {
      int iw = i / ww;
      int ib = i % ww;
      if (1 == coeff(f,i)) pp[iw] += (1 << ib);
    }
    if (0 != pp[nn-1]) printf("%X",pp[nn-1]);
    for(int i = nn-2; i > -1 ; i--) {
      printf("%8.8X",pp[i]);
    }
    printf("\n");
}

extern "C"
void get_coeff (const int& nn,     // MT param n
                const int& mm,     //          m
                const int& rr,     //          r
                const int& ww,     //          w => MT period_exp = n*w - r
                const int& avec,   //        aaa => companion matirx component vector a
                const int& nj,     // jump ahead step exponent   : id*(2^nj)
                const int& id,     // jump ahead step id=[0,...] : id*(2^nj)
                unsigned int *pp,  // jump ahead polynomial coefficients, pp[nn]
                int& np)           // jump ahead polynomial order(bit size)
{
  if (id <= 0) return;
  int period_exp = nn*ww-rr;
  int ns = (int)ceil(log((double)period_exp)/log(2.0));

  //
  // Compute MT characteristic polynomial f(x)
  // [see,
  //  M. Matsumoto and T. Nishimura, 
  //  "Mersenne Twister: A 623-dimensionally equidistributed uniform
  //                                   pseudorandom number generator", 
  //  ACM Trans. on Modeling and Computer Simulation Vol. 8, No. 1, 
  //  January pp.3-30 (1998) DOI:10.1145/272991.272995 for explicit form]
  //
  // f(x) =         af^(w-r) * bf^r
  //      + a(0)  * af^(w-r) * bf^(r-1)
  //      + a(1)  * af^(w-r) * bf^(r-2)
  //      + ....
  //      + a(r-2)* af^(w-r) * bf^(1)
  //      + a(r-1)* af^(w-r)
  //      + a(r)  * af^(w-r-1)
  //      + a(r+1)* af^(w-r-2)
  //      + ...
  //      + a(w-2)* af^(1)
  //      + a(w-1)
  // where
  //   af = x^nn     + x^mm;
  //   bf = x^(nn-1) + x^(mm-1);
  //
  GF2X *af, *bf;
  af = new GF2X;
  bf = new GF2X;
  SetCoeff(*af,nn);
  SetCoeff(*af,mm);   // af = x^nn + x^mm;
  SetCoeff(*bf,nn-1);
  SetCoeff(*bf,mm-1); // bf = x^(nn-1) + x^(mm-1)
  GF2X *f;
  f = new GF2X;
  *f = power(*af,ww-rr) * power(*bf,rr);
  for (int i=0;i<rr;i++) {
    int ib = i % ww;
    int a = (avec >> ib) & 0x1;
    if (1 == a) *f += power(*af,ww-rr) * power(*bf,rr-1-i);
  }
  for (int i=rr;i<ww;i++) {
    int ib = i % ww;
    int a = (avec >> ib) & 0x1;
    if (1 == a) *f += power(*af,ww-1-i);
  }
  delete af;
  delete bf;

  //
  // compute r(x) = x^((2^nj)*id) mod f(x)
  //

  //
  // g(x) = x^(2^nj) mod f(x) 
  //
  GF2X *g;
  g = new GF2X;
  if ( ns < nj ) {
    SetCoeff(*g,(1 << ns)); // g(x) = x^(2^ns)
    *g = *g % (*f);
    for (int i=ns;i < nj;i++) *g = power(*g,2) % (*f);

  } else {

    SetCoeff(*g,(1 << nj)); // g(x) = x^(2^nj)

  }


#ifdef _DEBUG_
  printf("%8d\n",ns);
  printf("%8d\n",nj);
  printf("@");
  print_hex(*f);
  printf("@");
  print_hex(*g);
#endif


  //
  // r(x) = g(x)^id mod f(x)
  //

  int id_bits = (int)floor(log(double(id))/log(2.0)) + 1; // bit size of id
  DEBUG_PRINTF("id=%d id_bits=%d\n",id,id_bits);
  GF2X *r;
  r = new GF2X;
  SetCoeff(*r,0);  // r(x) = 1
  for (int i=id_bits;i >=0; --i) {

    *r = power(*r,2) % (*f);

    if (1 == ((id >> i)& 1)) *r = ((*r) * (*g)) % (*f);

  }
  delete g;
  delete f;

#ifdef _DEBUG_
  printf("@");
  print_hex(*r);
#endif

  //
  // extract bit sequence (=pp[]) from r(x)
  //
  {
    int nb = NumBits(*r);
    for(int i = 0; i < nn ; i++) pp[i] = 0;
    for(int i = 0; i < nb ; i++) {
      int iw = i / ww;
      int ib = i % ww;
      if (1 == coeff(*r,i)) pp[iw] += (1 << ib);
    }
    np = nb;
  }
  delete r;

}
