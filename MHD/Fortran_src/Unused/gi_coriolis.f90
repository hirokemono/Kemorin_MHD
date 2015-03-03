!
!*****************************************
!*                                       *
!*   Program of Gaunt integral           *
!*              updated on '95, 9,Aug    *
!*                                       *
!*****************************************
!*
!*****************************************************
!*  ki,gi :gaunt integral (m3,l3,m2,l2,m1,l1)        *
!*  li,ei :elsasser integral (m3,l3,m2,l2,m1,l1)     *
!*            Sin : negative m   Cos : positive m    *
!*****************************************************
!*
!*****    selection rule    ***********************************
!*                                                            *
!*   For gaunt integral                                       *
!*    1) l1+l2+l3 is even                                     *
!*    2) l1,l2,l3 form the side of a triangle                 *
!*          l1 + l2 >= l3                                     *
!*          l2 + l3 >= l1                                     *
!*          l3 + l1 >= l2                                     *
!*    3) m1 +- m2 +- m3 = 0                                   *
!*    4) three of the harmonics of fai have COS or one has    *
!*                                                            *
!*   For Elsasser integral                                    *
!*    1) l1+l2+l3 is odd                                      *
!*    2) l1,l2,l3 form the side of a triangle                 *
!*    3) m1 +- m2 +- m3 = 0                                   *
!*    4) two of the harmonics of fai have COS or none has     *
!*                                                            *
!**************************************************************
!*
!*******************************************************************
!*                                                                 *
!*  Adams - Gaunt integrals                                        *
!*                                                                 *
!*  (m1,m2,m3)  /  (m1)   (m2)   (m3)                              *
!* Gi         = | P    * P    * P    *sin(theta) d(theta)          *
!*  (l1,l2,l3)  /  (l1)   (l2)   (l3)                              *
!*                                                                 *
!*                                        (m3)                     *
!*  (m1,m2,m3)  /  (m1)         (m2)    dy(l3)                     *
!* Ei         = | P    *[ m2 * P     * --------                    *
!*  (l1,l2,l3)  /  (l1)         (l2)   d(theta)                    *
!*                                                                 *
!*                        (m2)                                     *
!*                      dy(l2)     (m3)                            *
!*              - m3 * -------- * P     ] d(theta)                 *
!*                     d(theta)    (l3)                            *
!*                                                                 *
!*  (m1,m2,m3)  //  (m1)   (m2)   (m3)                             *
!* Ki         = || Y    * Y    * Y    *sin(theta) d(theta)d(phi)   *
!*  (l1,l2,l3)  //  (l1)   (l2)   (l3)                             *
!*                                                                 *
!*                            (m2)        (m3)                     *
!*  (m1,m2,m3)  //  (m1)    dy(l2)      dy(l3)                     *
!* Li         = || Y    *[ -------- * ----------                   *
!*  (l1,l2,l3)  //  (l1)   d(theta)    d(phi)                      *
!*                                                                 *
!*                    (m2)        (m3)                             *
!*                  dy(l2)      dy(l3)                             *
!*              - ---------- * -------- ] d(theta)d(phi)           *
!*                  d(phi)     d(theta)                            *
!*                                                                 *
!*  where                                                          *
!*                   (m)   (m)  | sin(m*phi) |                     *
!*                  Y   = P   * |   1        |                     *
!*                   (l)   (l)  | cos(m*phi) |                     *
!*                                                                 *
!*                         (m)    2*(l-m)!     1                   *
!*                        P   = [----------]**--- * P(l,m)         *
!*                         (l)      (l+m)!     2                   *
!*                         (0)                                     *
!*                        P   =           P(l,0)                   *
!*                         (l)                                     *
!*                                                                 *
!*******************************************************************
!*
      program gi_coriolis
!
      use m_precision
!
      use tri_sph_for_coriolis
      use m_int_4_sph_coriolis_IO
!
      implicit none
!
      integer(kind = kint) :: ltr
!
!
      write(6,'(a)') ' input truncation number'
      read(5,*) ltr
      write(6,'(a)') ' input data file name'
      read(5,*) sph_cor_file_name
!
      call gaunt_coriolis(ltr)
!
      end program gi_coriolis

