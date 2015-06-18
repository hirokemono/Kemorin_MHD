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
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
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
      call write_filter_moms_head                                       &
     &   (id_file, mom1%nnod_fmom, mom1%nele_fmom,                      &
     &    num_filter_moms, FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
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
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    num_filter_moms, FEM1_elen%filter_conf%nf_type)
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
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom, &
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
      call alloc_ref_1d_mom_type(FEM1_elen%filter_conf)
      call alloc_elen_ele_type                                          &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
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
      if (FEM1_elen%nnod_filter_mom .ne. numnod) then
        ierr = 500
      else if (FEM1_elen%nele_filter_mom .ne. numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      mom1%nnod_fmom = FEM1_elen%nnod_filter_mom
      call alloc_ref_1d_mom_type(FEM1_elen%filter_conf)
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
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
