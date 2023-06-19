!>@file   filter_moments_IO.f90
!!@brief  module filter_moments_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!!@n     modified by H. Matsui in Nov., 2006
!!@n     modified by H. Matsui in Mar., 2008
!
!> @brief Data IO routines for filtering data
!!
!!@verbatim
!!      subroutine read_filter_elen_head                                &
!!     &         (id_file, nnod, nele, nf_type, iend)
!!      subroutine write_filter_elen_head(id_file, nnod, nele, nf_type)
!!
!!      subroutine read_filter_moms_head(id_file, nnod, nele,           &
!!     &          n_filter, nf_type, iend)
!!      subroutine write_filter_moms_head(id_file, nnod, nele,          &
!!     &          n_filter, nf_type)
!!
!!      subroutine read_elength(id_file, num, el1, el2, el3, iend)
!!      subroutine read_mom_coefs_dx(id_file, num, el1, el2, el3, iend)
!!
!!      subroutine write_elength(id_file, num, el1, el2, el3)
!!      subroutine write_mom_coefs_dx(id_file, num, el1, el2, el3)
!!@endverbatim
!
      module filter_moments_IO
!
      use m_precision
!
      implicit none
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_elen_head                                  &
     &         (id_file, nnod, nele, nf_type, iend)
!
      use skip_comment_f
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(inout) :: nnod, nele, nf_type
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) nnod, nele
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) nf_type
!
      end subroutine read_filter_elen_head
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_head(id_file, nnod, nele, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: nnod, nele, nf_type
!
!
      write(id_file,'(a)') '! number of node for filtering: '
      write(id_file,'(2i16)') nnod, nele
!
      write(id_file,'(a)')  '! number of filter function '
      write(id_file,'(i16)') nf_type
!
!
      end subroutine write_filter_elen_head
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_head(id_file, nnod, nele,             &
     &          n_filter, nf_type, iend)
!
      use skip_comment_f
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(inout) :: nnod, nele
      integer (kind=kint), intent(inout) :: n_filter, nf_type
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) nnod, nele, n_filter
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) nf_type
!
      end subroutine read_filter_moms_head
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moms_head(id_file, nnod, nele,            &
     &          n_filter, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: nnod, nele
      integer (kind=kint), intent(in) :: n_filter, nf_type
!
!
      write(id_file,'(a)') '! number of node for filtering: '
      write(id_file,'(3i16)')  nnod, nele, n_filter
!
      write(id_file,'(a)')  '! number of filter function '
      write(id_file,'(i16)') nf_type
!
      end subroutine write_filter_moms_head
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_elength(id_file, num, el1, el2, el3, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i, itmp
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) itmp, el1(1), el2(1), el3(1)
      do i = 2, num
        read(id_file,*) itmp, el1(i), el2(i), el3(i)
      end do
!
      end subroutine read_elength
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx(id_file, num, el1, el2, el3, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: nd, i, itmp
!
      do nd = 1, 3
        call skip_comment(id_file, character_4_read, iend)
        if(iend .gt. 0) return
        read(character_4_read,*) itmp, itmp,                            &
     &                        el1(1,nd), el2(1,nd), el3(1,nd)
        do i = 2, num
          read(id_file,*) itmp, itmp, el1(i,nd), el2(i,nd), el3(i,nd)
        end do
      end do
!
      end subroutine read_mom_coefs_dx
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
!
      integer(kind = kint) :: i
!
      do i = 1, num
        write(id_file,'(i16,1p3E25.15e3)') i, el1(i), el2(i), el3(i)
      end do
!
      end subroutine write_elength
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
!
      integer(kind = kint) :: nd, i
!
        do nd = 1, 3
          do i = 1, num
            write(id_file,'(2i16,1p3E25.15e3)') nd, i, el1(i,nd),       &
     &         el2(i,nd), el3(i,nd)
          end do
        end do
!
      end subroutine write_mom_coefs_dx
!
! ----------------------------------------------------------------------
!
      end module filter_moments_IO
