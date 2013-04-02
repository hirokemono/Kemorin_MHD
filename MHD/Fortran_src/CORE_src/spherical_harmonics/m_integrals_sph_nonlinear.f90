!m_integrals_sph_nonlinear.f90
!      module m_integrals_sph_nonlinear
!
!     Written by H. Matsui on March, 2010
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
!      subroutine allocate_gi_stack_nl
!      subroutine allocate_gi_for_nl
!      subroutine allocate_ei_for_nl
!
!      subroutine deallocate_gi_stack_nl
!      subroutine deallocate_gi_for_nl
!      subroutine deallocate_ei_for_nl
!
!      subroutine check_gaunt_nl(file_name, id_file, jmax, idx_lc)
!      subroutine read_gaunt_nl(file_name, id_file, jmax, idx_lc, ierr)
!
      module m_integrals_sph_nonlinear
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ltr_gaunt, jmax_gaunt
!
      integer(kind = kint) :: ntot_larger_gei_nl_lm3
      integer(kind = kint) :: ntot_gi_nl_lm3, ntot_ei_nl_lm3
      integer(kind = kint), allocatable :: num_gi_nl_lm3(:,:)
      integer(kind = kint), allocatable :: istack_gi_nl_lm3(:,:)
!
      integer(kind = kint) :: max_j12_gi, max_j12_ei
      integer(kind = kint), allocatable :: lm_gi_nl2(:,:,:)
      integer(kind = kint), allocatable :: lm_ei_nl2(:,:,:)
      real(kind = kreal), allocatable ::  gi_nl2(:,:) ,ei_nl2(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_gi_stack_nl
!
!
      allocate(num_gi_nl_lm3(0:jmax_gaunt,2))
      allocate(istack_gi_nl_lm3(-1:jmax_gaunt,2))
!
      num_gi_nl_lm3 =    0
      istack_gi_nl_lm3 = 0
!
      end subroutine allocate_gi_stack_nl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_gi_for_nl
!
!
      allocate(lm_gi_nl2(max_j12_gi,2,jmax_gaunt))
      allocate(gi_nl2(max_j12_gi,jmax_gaunt))
!
      lm_gi_nl2 = 0
      gi_nl2 =    0.0d0
!
      end subroutine allocate_gi_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ei_for_nl
!
!
      allocate(lm_ei_nl2(max_j12_ei,2,jmax_gaunt))
      allocate(ei_nl2(max_j12_ei,jmax_gaunt))
!
      lm_ei_nl2 = 0
      ei_nl2 =    0.0d0
!
      end subroutine allocate_ei_for_nl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_gi_stack_nl
!
      deallocate(num_gi_nl_lm3, istack_gi_nl_lm3)
!
      end subroutine deallocate_gi_stack_nl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_gi_for_nl
!
      deallocate(lm_gi_nl2, gi_nl2)
!
      end subroutine deallocate_gi_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ei_for_nl
!
      deallocate(lm_ei_nl2, ei_nl2)
!
      end subroutine deallocate_ei_for_nl
!
!  ---------------------------------------------------------------------
!
      subroutine check_gaunt_nl(file_name, id_file, jmax, idx_lc)
!*
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_lc(jmax,3)
!
      integer(kind = kint) :: j3, jz
!
!
      open (id_file, FILE = file_name)
!*
      write (id_file,'(a)') ' #--- hermony.dat ---'
      write (id_file,'(a)')   'ltr_gaunt, jmax_gaunt'
      write (id_file,'(2i6)')  ltr_gaunt, jmax_gaunt
      write (id_file,'(a)')                                             &
     &      'ntot_larger_gei_nl_lm3, ntot_gi_nl_lm3, ntot_ei_nl_lm3'
      write (id_file,'(3i10)') ntot_larger_gei_nl_lm3,                  &
     &                         ntot_gi_nl_lm3, ntot_ei_nl_lm3
!
      write (id_file,'(a)')                                             &
     &      'j3, lm3_gl, istack_gi, istack_ei, num_gi, num_ei'
      do j3 = 1 ,jmax
        write (id_file,'(6i10)') j3, idx_lc(j3,1),                      &
     &          istack_gi_nl_lm3(j3,1:2), num_gi_nl_lm3(j3,1:2)
      end do
!
      write (id_file,*) ' j3 j1 j2 Ki/pi '
!*
      do j3 = 1 ,jmax
        do jz = 1, num_gi_nl_lm3(j3,1)
          write (id_file,'(3i6,1pE25.15e3)') idx_lc(j3,1),              &
     &          lm_gi_nl2(jz,1:2,j3), gi_nl2(jz,j3)
        end do
      end do
      write (id_file,*) ' j3 j1 j2 Li/pi '
!*
      do j3 = 1 ,jmax
        do jz = 1, num_gi_nl_lm3(j3,2)
          write (id_file,'(3i6,1pE25.15e3)') idx_lc(j3,1),              &
     &           lm_ei_nl2(jz,1:2,j3), ei_nl2(jz,j3)
        end do
      end do
!
      close(id_file)
!
      end subroutine check_gaunt_nl
!
!  ---------------------------------------------------------------------
!
      subroutine read_gaunt_nl(file_name, id_file, jmax, idx_lc, ierr)
!*
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_lc(jmax,3)
!
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: j3, jz, itmp, it0
      character(len=kchara) :: tmpchara
!
!
      ierr = 0
      write(*,*) 'read gaunt integrals: ', trim(file_name)
      open (id_file, FILE = file_name)
!*
      read (id_file,*) tmpchara
      read (id_file,*) tmpchara
      read (id_file,*)  ltr_gaunt, jmax_gaunt
      read (id_file,*) tmpchara
      read (id_file,*) ntot_larger_gei_nl_lm3,                         &
     &                 ntot_gi_nl_lm3, ntot_ei_nl_lm3
!
      if(jmax_gaunt .ne. jmax) then
        ierr = 1
        write(*,*) 'error in truncation'
        return
      end if
!
      call allocate_gi_stack_nl
!
      read (id_file,*) tmpchara
      do j3 = 1 ,jmax_gaunt
        read (id_file,*) it0, itmp,                                     &
     &          istack_gi_nl_lm3(j3,1:2), num_gi_nl_lm3(j3,1:2)
        if(itmp .ne. idx_lc(j3,1)) then
          ierr = 1
          write(*,*) 'error in stack for gaunt integral'
          return
        end if
      end do
!
      max_j12_gi = num_gi_nl_lm3(1,1)
      max_j12_ei = num_gi_nl_lm3(1,2)
      do j3 = 2, jmax_gaunt
        max_j12_gi = max(max_j12_gi,num_gi_nl_lm3(j3,1))
        max_j12_ei = max(max_j12_ei,num_gi_nl_lm3(j3,2))
      end do
      call allocate_gi_for_nl
      call allocate_ei_for_nl
!
      read (id_file,*) tmpchara
      do j3 = 1 ,jmax_gaunt
        do jz = 1, num_gi_nl_lm3(j3,1)
          read (id_file,*) itmp,                                       &
     &          lm_gi_nl2(jz,1:2,j3), gi_nl2(jz,j3)
          if(itmp .ne. idx_lc(j3,1)) then
            ierr = 1
            write(*,*) 'error in gaunt integral'
            return
          end if
        end do
      end do
!*
      read (id_file,*) tmpchara
      do j3 = 1 ,jmax_gaunt
        do jz = 1, num_gi_nl_lm3(j3,2)
          read (id_file,'(3i6,1pE25.15e3)') itmp,                      &
     &           lm_ei_nl2(jz,1:2,j3), ei_nl2(jz,j3)
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
      end module m_integrals_sph_nonlinear
