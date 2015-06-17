!
!     module filter_moment_data_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine write_filter_elen_data(id_file)
!      subroutine write_filter_moments_data(id_file)
!      subroutine read_filter_moment_num(id_file)
!      subroutine read_filter_elen_data(id_file, numnod, numele, ierr)
!      subroutine read_filter_moments_data(id_file, numnod, numele, ierr)
!
      module filter_moment_data_IO
!
      use m_precision
!
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
      subroutine write_filter_elen_data(id_file)
!
      use filter_moments_IO
      use filter_moments_on_ele_IO
!
      integer (kind = kint), intent(in) :: id_file
!
!
      call write_filter_elen_head(id_file,                              &
     &    nnod_filter_mom, FEM1_elen%nele_filter_mom,      &
     &    filter_conf1%nf_type)
!
      if (filter_conf1%nf_type .gt. 0) then
        call write_base_filter_info(id_file)
        call write_elength_ele(id_file)
      end if
!
      end subroutine write_filter_elen_data
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moments_data(id_file)
!
      use m_filter_moments
      use filter_moments_IO
      use filter_moments_on_ele_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head(id_file,                              &
     &    nnod_fmom, nele_fmom, num_filter_moms, filter_conf1%nf_type)
!
      if (filter_conf1%nf_type .gt. 0) then
        call write_base_filter_info(id_file)
        do ifil = 1, num_filter_moms
          call write_filter_moments_ele(id_file, ifil)
        end do
      end if
!
      end subroutine write_filter_moments_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moment_num(id_file)
!
      use m_filter_moments
      use filter_moments_IO
!
      integer (kind=kint), intent(in) :: id_file
!
!
      call read_filter_moms_head(id_file,                               &
     &    nnod_filter_mom, FEM1_elen%nele_filter_mom,   &
     &    num_filter_moms, filter_conf1%nf_type)
!
      end subroutine read_filter_moment_num
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_data(id_file, numnod, numele, ierr)
!
      use filter_moments_IO
      use filter_moments_on_ele_IO
!
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
!
!
      call read_filter_elen_head(id_file,                               &
     &    nnod_filter_mom, FEM1_elen%nele_filter_mom, &
     &    filter_conf1%nf_type)
!
      if (nnod_filter_mom.ne.numnod) then
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
      if (filter_conf1%nf_type .gt. 0) then
        call read_base_filter_info(id_file)
        call read_elength_ele(id_file)
      end if
!
      end subroutine read_filter_elen_data
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moments_data(id_file, numnod, numele, ierr)
!
      use m_filter_moments
      use filter_moments_on_ele_IO
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
!
      integer (kind=kint) :: ifil
!
!
      call read_filter_moment_num(id_file)
!
      if (nnod_filter_mom.ne.numnod) then
        ierr = 500
      else if (FEM1_elen%nele_filter_mom .ne. numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      nnod_fmom = nnod_filter_mom
      call allocate_ref_1d_moment
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
!
      if (filter_conf1%nf_type .gt. 0) then
!
        call read_base_filter_info(id_file)
        do ifil = 1, num_filter_moms
          call read_filter_moments_ele(id_file, ifil)
        end do
!
      end if
!
      end subroutine read_filter_moments_data
!
! ----------------------------------------------------------------------
!
      end module filter_moment_data_IO
