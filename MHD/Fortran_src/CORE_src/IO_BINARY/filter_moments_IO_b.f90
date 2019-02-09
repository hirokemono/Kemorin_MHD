!filter_moments_IO_b.f90
!     module filter_moments_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine read_ref_filter_param_b(iflag_swap,                  &
!!     &          nf_type, filter_type, f_width, xmom_1d_org, ierr)
!!      subroutine write_ref_filter_param_b                             &
!!     &         (nf_type, filter_type, f_width, xmom_1d_org)
!!
!!      subroutine read_filter_elen_head_b                              &
!!     &         (iflag_swap, nnod, nele, nf_type, ierr)
!!      subroutine write_filter_elen_head_b(nnod, nele, nf_type)
!!
!!      subroutine read_filter_moms_head_b                              &
!!      &         (iflag_swap, nnod, nele, n_filter, nf_type, ierr)
!!      subroutine write_filter_moms_head_b                             &
!!     &         (nnod, nele, n_filter, nf_type)
!!
!!      subroutine read_elength_b(iflag_swap, num, el1, el2, el3, ierr)
!!      subroutine read_mom_coefs_dx_b                                  &
!!     &         (iflag_swap, num, el1, el2, el3, ierr)
!!
!!      subroutine write_elength_b(num, el1, el2, el3)
!!      subroutine write_mom_coefs_dx_b(num, el1, el2, el3)
!
      module filter_moments_IO_b
!
      use m_precision
      use m_constants
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_ref_filter_param_b(iflag_swap,                    &
     &          nf_type, filter_type, f_width, xmom_1d_org, ierr)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: nf_type
!
      character(len=kchara), intent(inout) :: filter_type(nf_type)
      real(kind=kreal), intent(inout) :: f_width(nf_type)
      real(kind=kreal), intent(inout) :: xmom_1d_org(nf_type,0:2)
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_mul_character_b(nf_type, filter_type, ierr)
      if(ierr .gt. 0) return
      call read_1d_vector_b(iflag_swap, nf_type, f_width, ierr)
      if(ierr .gt. 0) return
!
      call read_2d_vector_b                                             &
     &   (iflag_swap, nf_type, ithree, xmom_1d_org(1,0), ierr)
!
      end subroutine read_ref_filter_param_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_ref_filter_param_b                               &
     &         (nf_type, filter_type, f_width, xmom_1d_org)
!
      integer(kind = kint), intent(in) :: nf_type
      character(len=kchara), intent(in) :: filter_type(nf_type)
      real(kind=kreal), intent(in) :: f_width(nf_type)
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
!
      call write_mul_character_b(nf_type, filter_type)
      call write_1d_vector_b(nf_type, f_width)
      call write_2d_vector_b(nf_type, ithree, xmom_1d_org(1,0))
!
      end subroutine write_ref_filter_param_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_elen_head_b                                &
     &         (iflag_swap, nnod, nele, nf_type, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer (kind=kint), intent(inout) :: nnod, nele, nf_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_one_integer_b(iflag_swap, nnod, ierr)
      call read_one_integer_b(iflag_swap, nele, ierr)
      call read_one_integer_b(iflag_swap, nf_type, ierr)
!
      end subroutine read_filter_elen_head_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_head_b(nnod, nele, nf_type)
!
      integer (kind=kint), intent(in) :: nnod, nele, nf_type
!
!
      call write_one_integer_b(nnod)
      call write_one_integer_b(nele)
      call write_one_integer_b(nf_type)
!
      end subroutine write_filter_elen_head_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_head_b                                &
     &         (iflag_swap, nnod, nele, n_filter, nf_type, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer (kind=kint), intent(inout) :: nnod, nele
      integer (kind=kint), intent(inout) ::  n_filter, nf_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_one_integer_b(iflag_swap, nnod, ierr)
      call read_one_integer_b(iflag_swap, nele, ierr)
      call read_one_integer_b(iflag_swap, n_filter, ierr)
      call read_one_integer_b(iflag_swap, nf_type, ierr)
!
      end subroutine read_filter_moms_head_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moms_head_b                               &
     &         (nnod, nele, n_filter, nf_type)
!
      integer (kind=kint), intent(in) :: nnod, nele, n_filter, nf_type
!
!
      call write_one_integer_b(nnod)
      call write_one_integer_b(nele)
      call write_one_integer_b(n_filter)
      call write_one_integer_b(nf_type)
!
      end subroutine write_filter_moms_head_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_elength_b(iflag_swap, num, el1, el2, el3, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_1d_vector_b(iflag_swap, num, el1, ierr)
      if(ierr .gt. 0) return
      call read_1d_vector_b(iflag_swap, num, el2, ierr)
      if(ierr .gt. 0) return
      call read_1d_vector_b(iflag_swap, num, el3, ierr)
!
      end subroutine read_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx_b                                    &
     &         (iflag_swap, num, el1, el2, el3, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_2d_vector_b(iflag_swap, num, ithree, el1, ierr)
      if(ierr .gt. 0) return
      call read_2d_vector_b(iflag_swap, num, ithree, el2, ierr)
      if(ierr .gt. 0) return
      call read_2d_vector_b(iflag_swap, num, ithree, el3, ierr)
!
      end subroutine read_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength_b(num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
!
!
      call write_1d_vector_b(num, el1)
      call write_1d_vector_b(num, el2)
      call write_1d_vector_b(num, el3)
!
      end subroutine write_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx_b(num, el1, el2, el3)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
!
!
      call write_2d_vector_b(num, ithree, el1)
      call write_2d_vector_b(num, ithree, el2)
      call write_2d_vector_b(num, ithree, el3)
!
      end subroutine write_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!
      end module filter_moments_IO_b
