!
!     module filter_moment_data_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine write_filter_elen_data_b(id_file)
!      subroutine write_filter_moments_data_b(id_file)
!      subroutine read_filter_moment_num_b(id_file)
!      subroutine read_filter_elen_data_b(id_file, numnod, numele, ierr)
!      subroutine read_filter_moments_data_b(id_file, numnod,           &
!     &          numele, ierr)
!
      module filter_moment_data_IO_b
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
      subroutine write_filter_elen_data_b(id_file)
!
      use m_filter_elength
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      integer (kind = kint), intent(in) :: id_file
!
!
      call write_filter_elen_head_b(id_file,                            &
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b                              &
     &     (id_file, FEM1_elen%filter_conf)
        call write_elen_ele_type_b                                      &
     &     (id_file, FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
      end if
!
      end subroutine write_filter_elen_data_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moments_data_b(id_file)
!
      use m_filter_elength
      use m_filter_moments
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head_b                                     &
     &   (id_file, mom1%nnod_fmom, mom1%nele_fmom,                      &
     &    mom1%num_filter_moms, FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b                              &
     &     (id_file, FEM1_elen%filter_conf)
!
        do ifil = 1, mom1%num_filter_moms
          call write_filter_moms_ele_type_b                             &
     &       (id_file, mom1%nele_fmom, mom1%mom_ele(ifil))
        end do
      end if
!
      end subroutine write_filter_moments_data_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moment_num_b(id_file)
!
      use m_filter_moments
      use filter_moments_IO_b
!
      integer (kind=kint), intent(in) :: id_file
!
!
      call read_filter_moms_head_b(id_file,                             &
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    mom1%num_filter_moms, FEM1_elen%filter_conf%nf_type)
!
      end subroutine read_filter_moment_num_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_data_b(id_file, numnod, numele, ierr)
!
      use m_filter_elength
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
!
!
      call read_filter_elen_head_b(id_file,                             &
     &    FEM1_elen%nnod_filter_mom, FEM1_elen%nele_filter_mom,         &
     &    FEM1_elen%filter_conf%nf_type)
!
      if (FEM1_elen%nnod_filter_mom.ne.numnod) then
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
        call read_base_filter_info_type_b                               &
     &     (id_file, FEM1_elen%filter_conf)
        call read_elen_ele_type_b                                       &
     &     (id_file, FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
      end if
!
      end subroutine read_filter_elen_data_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moments_data_b(id_file,                    &
     &          numnod, numele, ierr)
!
      use m_filter_moments
      use m_filter_elength
      use filter_mom_type_on_ele_IO_b
!
      integer (kind=kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(inout) :: ierr
      integer (kind=kint) :: ifil
!
!
      call read_filter_moment_num_b(id_file)
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
      call alloc_filter_moms_ele_type(FEM1_elen%nele_filter_mom,mom1)
!
      if (FEM1_elen%filter_conf%nf_type .gt. 0) then
!
        call read_base_filter_info_type_b                               &
     &     (id_file, FEM1_elen%filter_conf)
        do ifil = 1, mom1%num_filter_moms
          call read_filter_moms_ele_type_b                              &
     &       (id_file, mom1%nele_fmom, mom1%mom_ele(ifil))
        end do
!
      end if
!
      end subroutine read_filter_moments_data_b
!
! ----------------------------------------------------------------------
!
      end module filter_moment_data_IO_b
