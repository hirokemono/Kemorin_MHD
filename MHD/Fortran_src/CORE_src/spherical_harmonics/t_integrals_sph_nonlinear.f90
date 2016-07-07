!>@file   t_integrals_sph_nonlinear.f90
!!@brief  module t_integrals_sph_nonlinear
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in March, 2010
!!@n    Modified in Jubne, 2016
!
!> @brief Module of Adams-Gaunt integram
!!
!!@verbatim
!!*****************************************
!!*                                       *
!!*   Program of Gaunt integral           *
!!*              updated on '95, 9,Aug    *
!!*                                       *
!!*****************************************
!!*
!!*****************************************************
!!*  ki,gi :gaunt integral (m3,l3,m2,l2,m1,l1)        *
!!*  li,ei :elsasser integral (m3,l3,m2,l2,m1,l1)     *
!!*            Sin : negative m   Cos : positive m    *
!!*****************************************************
!!*
!!*****    selection rule    ***********************************
!!*                                                            *
!!*   For gaunt integral                                       *
!!*    1) l1+l2+l3 is even                                     *
!!*    2) l1,l2,l3 form the side of a triangle                 *
!!*          l1 + l2 >= l3                                     *
!!*          l2 + l3 >= l1                                     *
!!*          l3 + l1 >= l2                                     *
!!*    3) m1 +- m2 +- m3 = 0                                   *
!!*    4) three of the harmonics of fai have COS or one has    *
!!*                                                            *
!!*   For Elsasser integral                                    *
!!*    1) l1+l2+l3 is odd                                      *
!!*    2) l1,l2,l3 form the side of a triangle                 *
!!*    3) m1 +- m2 +- m3 = 0                                   *
!!*    4) two of the harmonics of fai have COS or none has     *
!!*                                                            *
!!**************************************************************
!!*
!!*******************************************************************
!!*                                                                 *
!!*  Adams - Gaunt integrals                                        *
!!*                                                                 *
!!*  (m1,m2,m3)  /  (m1)   (m2)   (m3)                              *
!!* Gi         = | P    * P    * P    *sin(theta) d(theta)          *
!!*  (l1,l2,l3)  /  (l1)   (l2)   (l3)                              *
!!*                                                                 *
!!*                                        (m3)                     *
!!*  (m1,m2,m3)  /  (m1)         (m2)    dy(l3)                     *
!!* Ei         = | P    *[ m2 * P     * --------                    *
!!*  (l1,l2,l3)  /  (l1)         (l2)   d(theta)                    *
!!*                                                                 *
!!*                        (m2)                                     *
!!*                      dy(l2)     (m3)                            *
!!*              - m3 * -------- * P     ] d(theta)                 *
!!*                     d(theta)    (l3)                            *
!!*                                                                 *
!!*  (m1,m2,m3)  //  (m1)   (m2)   (m3)                             *
!!* Ki         = || Y    * Y    * Y    *sin(theta) d(theta)d(phi)   *
!!*  (l1,l2,l3)  //  (l1)   (l2)   (l3)                             *
!!*                                                                 *
!!*                            (m2)        (m3)                     *
!!*  (m1,m2,m3)  //  (m1)    dy(l2)      dy(l3)                     *
!!* Li         = || Y    *[ -------- * ----------                   *
!!*  (l1,l2,l3)  //  (l1)   d(theta)    d(phi)                      *
!!*                                                                 *
!!*                    (m2)        (m3)                             *
!!*                  dy(l2)      dy(l3)                             *
!!*              - ---------- * -------- ] d(theta)d(phi)           *
!!*                  d(phi)     d(theta)                            *
!!*                                                                 *
!!*  where                                                          *
!!*                   (m)   (m)  | sin(m*phi) |                     *
!!*                  Y   = P   * |   1        |                     *
!!*                   (l)   (l)  | cos(m*phi) |                     *
!!*                                                                 *
!!*                         (m)    2*(l-m)!     1                   *
!!*                        P   = [----------]**--- * P(l,m)         *
!!*                         (l)      (l+m)!     2                   *
!!*                         (0)                                     *
!!*                        P   =           P(l,0)                   *
!!*                         (l)                                     *
!!*                                                                 *
!!*******************************************************************
!!*
!!      subroutine alloc_gi_stack_nl(ltr, jmax, gaunt)
!!      subroutine alloc_gi_for_nl(gaunt)
!!      subroutine alloc_ei_for_nl(gaunt)
!!
!!      subroutine dealloc_gi_stack_nl(gaunt)
!!      subroutine dealloc_gi_for_nl(gaunt)
!!      subroutine dealloc_ei_for_nl(gaunt)
!!
!!      subroutine check_gaunt_nl                                       &
!!     &         (file_name, id_file, jmax, idx_lc, gaunt)
!!      subroutine read_gaunt_nl                                        &
!!     &         (file_name, id_file, jmax, idx_lc, gaunt, ierr)
!!@endverbatim
!
      module t_integrals_sph_nonlinear
!
      use m_precision
!
      implicit none
!
!
!>      Structure of Adams-Gaunt integrals
      type adams_gaunt_integrals
!>        Truncation degree
        integer(kind = kint) :: ltr
!>        Number of modes excluding l=m=0
        integer(kind = kint) :: jmax
!
!>        Number of Adams-Gaunt integrals
        integer(kind = kint) :: ntot_larger_gei_nl_lm3
!>        Number of Adams-Gaunt integrals
        integer(kind = kint) :: ntot_gi_nl_lm3
!>        Number of Elsasser integrals
        integer(kind = kint) :: ntot_ei_nl_lm3
!>        Number of Adams-Gaunt integrals for each mode
        integer(kind = kint), pointer :: num_gi_nl_lm3(:,:)
!>        Stack of Adams-Gaunt integrals for each mode
        integer(kind = kint), pointer :: istack_gi_nl_lm3(:,:)
!>        Maximum number of Adams-Gaunt integrals
        integer(kind = kint) :: max_j12_gi
!>        Maximum number of Elsasser integrals
        integer(kind = kint) :: max_j12_ei
!
!>        Mode IDs of Adams-Gaunt integrals
        integer(kind = kint), pointer :: lm_gi_nl2(:,:,:)
!>        Mode IDs of Elsasser integrals
        integer(kind = kint), pointer :: lm_ei_nl2(:,:,:)
!>        Adams-Gaunt integrals
        real(kind = kreal), pointer ::  gi_nl2(:,:)
!>        Elsasser integrals
        real(kind = kreal), pointer ::  ei_nl2(:,:)
      end type adams_gaunt_integrals
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_gi_stack_nl(ltr, jmax, gaunt)
!
      integer(kind = kint), intent(in) :: ltr, jmax
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
!
      gaunt%ltr =   ltr
      gaunt%jmax = jmax
      allocate(gaunt%num_gi_nl_lm3(0:gaunt%jmax,2))
      allocate(gaunt%istack_gi_nl_lm3(-1:gaunt%jmax,2))
!
      gaunt%ntot_gi_nl_lm3 = 0
      gaunt%ntot_ei_nl_lm3 = 0
      gaunt%ntot_larger_gei_nl_lm3 = 0
      gaunt%max_j12_gi = 1
      gaunt%max_j12_ei = 1
!
      gaunt%num_gi_nl_lm3 =    0
      gaunt%istack_gi_nl_lm3 = 0
!
      end subroutine alloc_gi_stack_nl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_gi_for_nl(gaunt)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
!
      allocate(gaunt%lm_gi_nl2(gaunt%max_j12_gi,2,gaunt%jmax))
      allocate(gaunt%gi_nl2(gaunt%max_j12_gi,gaunt%jmax))
!
      gaunt%lm_gi_nl2 = 0
      gaunt%gi_nl2 =    0.0d0
!
      end subroutine alloc_gi_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ei_for_nl(gaunt)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
!
      allocate(gaunt%lm_ei_nl2(gaunt%max_j12_ei,2,gaunt%jmax))
      allocate(gaunt%ei_nl2(gaunt%max_j12_ei,gaunt%jmax))
!
      gaunt%lm_ei_nl2 = 0
      gaunt%ei_nl2 =    0.0d0
!
      end subroutine alloc_ei_for_nl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_gi_stack_nl(gaunt)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      deallocate(gaunt%num_gi_nl_lm3, gaunt%istack_gi_nl_lm3)
!
      end subroutine dealloc_gi_stack_nl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_gi_for_nl(gaunt)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      deallocate(gaunt%lm_gi_nl2, gaunt%gi_nl2)
!
      end subroutine dealloc_gi_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ei_for_nl(gaunt)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      deallocate(gaunt%lm_ei_nl2, gaunt%ei_nl2)
!
      end subroutine dealloc_ei_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine check_gaunt_nl                                         &
     &         (file_name, id_file, jmax, idx_lc, gaunt)
!*
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_lc(jmax,3)
!
      type(adams_gaunt_integrals), intent(in) :: gaunt
!
!
      integer(kind = kint) :: j3, jz
!
!
      open (id_file, FILE = file_name)
!*
      write (id_file,'(a)') ' #--- hermony.dat ---'
      write (id_file,'(a)')   'gaunt%ltr, gaunt%jmax'
      write (id_file,'(2i6)')  gaunt%ltr, gaunt%jmax
      write (id_file,'(a)')                                             &
     &      'ntot_larger_gei_nl_lm3, ntot_gi_nl_lm3, ntot_ei_nl_lm3'
      write (id_file,'(3i16)') gaunt%ntot_larger_gei_nl_lm3,            &
     &                   gaunt%ntot_gi_nl_lm3, gaunt%ntot_ei_nl_lm3
!
      write (id_file,'(a)')                                             &
     &      'j3, lm3_gl, istack_gi, istack_ei, num_gi, num_ei'
      do j3 = 1 ,jmax
        write (id_file,'(6i16)') j3, idx_lc(j3,1),                      &
     &                           gaunt%istack_gi_nl_lm3(j3,1:2),        &
     &                           gaunt%num_gi_nl_lm3(j3,1:2)
      end do
!
      write (id_file,*) ' j3 j1 j2 Ki/pi '
!*
      do j3 = 1 ,jmax
        do jz = 1, gaunt%num_gi_nl_lm3(j3,1)
          write (id_file,'(3i6,1pE25.15e3)') idx_lc(j3,1),              &
     &          gaunt%lm_gi_nl2(jz,1:2,j3), gaunt%gi_nl2(jz,j3)
        end do
      end do
      write (id_file,*) ' j3 j1 j2 Li/pi '
!*
      do j3 = 1 ,jmax
        do jz = 1, gaunt%num_gi_nl_lm3(j3,2)
          write (id_file,'(3i6,1pE25.15e3)') idx_lc(j3,1),              &
     &           gaunt%lm_ei_nl2(jz,1:2,j3), gaunt%ei_nl2(jz,j3)
        end do
      end do
!
      close(id_file)
!
      end subroutine check_gaunt_nl
!
!  ---------------------------------------------------------------------
!
      subroutine read_gaunt_nl                                          &
     &         (file_name, id_file, jmax, idx_lc, gaunt, ierr)
!*
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_lc(jmax,3)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: j3, jz, itmp, it0, ltr_IO, jmax_IO
      character(len=kchara) :: tmpchara
!
!
      ierr = 0
      write(*,*) 'read gaunt integrals: ', trim(file_name)
      open (id_file, FILE = file_name)
!*
      read (id_file,*) tmpchara
      read (id_file,*) tmpchara
      read (id_file,*)  ltr_IO, jmax_IO
!
      call alloc_gi_stack_nl(ltr_IO, jmax_IO, gaunt)
!
      read (id_file,*) tmpchara
      read (id_file,*) gaunt%ntot_larger_gei_nl_lm3,                    &
     &                 gaunt%ntot_gi_nl_lm3, gaunt%ntot_ei_nl_lm3
!
      if(gaunt%jmax .ne. jmax) then
        ierr = 1
        write(*,*) 'error in truncation'
        return
      end if
!
      read (id_file,*) tmpchara
      do j3 = 1 ,gaunt%jmax
        read (id_file,*) it0, itmp, gaunt%istack_gi_nl_lm3(j3,1:2),     &
     &                   gaunt%num_gi_nl_lm3(j3,1:2)
        if(itmp .ne. idx_lc(j3,1)) then
          ierr = 1
          write(*,*) 'error in stack for gaunt integral'
          return
        end if
      end do
!
      gaunt%max_j12_gi = gaunt%num_gi_nl_lm3(1,1)
      gaunt%max_j12_ei = gaunt%num_gi_nl_lm3(1,2)
      do j3 = 2, gaunt%jmax
        gaunt%max_j12_gi                                                &
     &       = max(gaunt%max_j12_gi,gaunt%num_gi_nl_lm3(j3,1))
        gaunt%max_j12_ei                                                &
     &       = max(gaunt%max_j12_ei,gaunt%num_gi_nl_lm3(j3,2))
      end do
      call alloc_gi_for_nl(gaunt)
      call alloc_ei_for_nl(gaunt)
!
      read (id_file,*) tmpchara
      do j3 = 1 ,gaunt%jmax
        do jz = 1, gaunt%num_gi_nl_lm3(j3,1)
          read (id_file,*) itmp,                                       &
     &          gaunt%lm_gi_nl2(jz,1:2,j3), gaunt%gi_nl2(jz,j3)
          if(itmp .ne. idx_lc(j3,1)) then
            ierr = 1
            write(*,*) 'error in gaunt integral'
            return
          end if
        end do
      end do
!*
      read (id_file,*) tmpchara
      do j3 = 1 ,gaunt%jmax
        do jz = 1, gaunt%num_gi_nl_lm3(j3,2)
          read (id_file,'(3i6,1pE25.15e3)') itmp,                      &
     &           gaunt%lm_ei_nl2(jz,1:2,j3), gaunt%ei_nl2(jz,j3)
          if(itmp .ne. idx_lc(j3,1)) then
            ierr = 1
            write(*,*) 'error in elsasser integral'
            return
          end if
        end do
      end do
!
      close(id_file)
!
      end subroutine read_gaunt_nl
!
!  ---------------------------------------------------------------------
!
      end module t_integrals_sph_nonlinear
