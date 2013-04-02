!filter_moments_IO_b.f90
!     module filter_moments_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_filter_elen_head_b(id_file, nnod, nele, nf_type)
!      subroutine write_filter_elen_head_b(id_file, nnod, nele, nf_type)
!
!      subroutine read_filter_moms_head_b(id_file, nnod, nele,          &
!     &          n_filter, nf_type)
!      subroutine write_filter_moms_head_b(id_file, nnod, nele,         &
!     &          n_filter, nf_type)
!
!      subroutine read_elength_b(id_file, num, el1, el2, el3)
!      subroutine read_mom_coefs_dx_b(id_file, num, el1, el2, el3)
!
!      subroutine write_elength_b(id_file, num, el1, el2, el3)
!      subroutine write_mom_coefs_dx_b(id_file, num, el1, el2, el3)
!
      module filter_moments_IO_b
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
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_head_b(id_file, nnod, nele, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(inout) :: nnod, nele, nf_type
!
!
      read(id_file) nnod, nele
      read(id_file) nf_type
!
      end subroutine read_filter_elen_head_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_head_b(id_file, nnod, nele, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: nnod, nele, nf_type
!
!
      write(id_file) nnod, nele
      write(id_file) nf_type
!
      end subroutine write_filter_elen_head_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_head_b(id_file, nnod, nele,           &
     &          n_filter, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(inout) :: nnod, nele
      integer (kind=kint), intent(inout) ::  n_filter, nf_type
!
!
      read(id_file) nnod, nele, n_filter
      read(id_file) nf_type
!
      end subroutine read_filter_moms_head_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moms_head_b(id_file, nnod, nele,          &
     &          n_filter, nf_type)
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: nnod, nele, n_filter, nf_type
!
!
      write(id_file) nnod, nele, n_filter
      write(id_file) nf_type
!
      end subroutine write_filter_moms_head_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_elength_b(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
!
!
      read(id_file) el1(1:num)
      read(id_file) el2(1:num)
      read(id_file) el3(1:num)
!
      end subroutine read_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx_b(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
!
      integer(kind = kint) :: nd, i
!
      read(id_file) ((el1(i,nd), i=1,num),nd=1,3)
      read(id_file) ((el2(i,nd), i=1,num),nd=1,3)
      read(id_file) ((el3(i,nd), i=1,num),nd=1,3)
!
      end subroutine read_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength_b(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
!
!
      write(id_file) el1(1:num)
      write(id_file) el2(1:num)
      write(id_file) el3(1:num)
!
      end subroutine write_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx_b(id_file, num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: id_file, num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
!
      integer(kind = kint) :: nd, i
!
      write(id_file) ((el1(i,nd), i=1,num),nd=1,3)
      write(id_file) ((el2(i,nd), i=1,num),nd=1,3)
      write(id_file) ((el3(i,nd), i=1,num),nd=1,3)
!
      end subroutine write_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!
      end module filter_moments_IO_b
