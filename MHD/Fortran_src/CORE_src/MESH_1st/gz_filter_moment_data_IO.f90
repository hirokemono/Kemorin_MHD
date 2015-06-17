!gz_filter_moment_data_IO.f90
!     module gz_filter_moment_data_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine write_filter_elen_data_gz
!      subroutine write_filter_moments_data_gz
!
!      subroutine read_filter_moment_num_gz
!      subroutine read_filter_elen_data_gz(numnod, numele, ierr)
!      subroutine read_filter_moments_data_gz(numnod, numele, ierr)
!
      module gz_filter_moment_data_IO
!
      use m_precision
!
      use m_constants
      use m_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_data_gz
!
      use gz_filter_moments_IO
      use gz_filter_moments_ele_IO
!
      call write_filter_elen_head_gz(FEM1_elen%nnod_filter_mom,         &
    &     FEM1_elen%nele_filter_mom, FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_gz
        call write_elength_ele_gz
      end if
!
      end subroutine write_filter_elen_data_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moments_data_gz
!
      use m_filter_moments
      use gz_filter_moments_IO
      use gz_filter_moments_ele_IO
!
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head_gz(nnod_fmom, nele_fmom,              &
     &    num_filter_moms, FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_gz
        do ifil = 1, num_filter_moms
          call write_filter_moments_ele_gz(ifil)
        end do
      end if
!
      end subroutine write_filter_moments_data_gz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moment_num_gz
!
      use m_filter_moments
      use gz_filter_moments_IO
!
!
      call read_filter_moms_head_gz                                     &
     &   (FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    num_filter_moms, FEM1_elen%filter_conf%nf_type)
!
      end subroutine read_filter_moment_num_gz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_data_gz(numnod, numele, ierr)
!
      use gz_filter_moments_IO
      use gz_filter_moments_ele_IO
!
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
!
!
      call read_filter_elen_head_gz                                     &
     &   (FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%nnod_filter_mom .ne. numnod) then
        ierr = 500
      else if (FEM1_elen%nele_filter_mom .ne. numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      call allocate_ref_1d_moment
      call allocate_ele_length
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call read_base_filter_info_gz
        call read_elength_ele_gz
      end if
!
      end subroutine read_filter_elen_data_gz
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moments_data_gz(numnod, numele, ierr)
!
      use m_filter_moments
      use gz_filter_moments_IO
      use gz_filter_moments_ele_IO
!
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
!
      integer (kind=kint) :: ifil
!
!
      call read_filter_moment_num_gz
!
      if (FEM1_elen%nnod_filter_mom .ne. numnod) then
        ierr = 500
      else if (FEM1_elen%nele_filter_mom .ne. numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      nnod_fmom = FEM1_elen%nnod_filter_mom
      call allocate_ref_1d_moment
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call read_base_filter_info_gz
        do ifil = 1, num_filter_moms
          call read_filter_moments_ele_gz(ifil)
        end do
      end if
!
      end subroutine read_filter_moments_data_gz
!
! ----------------------------------------------------------------------
!
      end module gz_filter_moment_data_IO
