!gz_filter_mom_type_data_IO.f90
!     module gz_filter_mom_type_data_IO
!
!     Written by H. Matsui on Feb., 2012
!
!      subroutine gz_write_filter_elen_data_type(FEM_elens)
!      subroutine gz_write_filter_moms_data_type(FEM_elens, FEM_moms)
!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!      subroutine gz_read_filter_moment_num_type(FEM_elens, FEM_moms)
!      subroutine gz_read_filter_elen_data_type(numnod, numele,         &
!     &          FEM_elens, ierr)
!      subroutine gz_read_filter_moms_data_type(numnod, numele,         &
!     &          FEM_elens, FEM_moms, ierr)
!        integer (kind=kint), intent(in) :: numnod, numele
!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      module gz_filter_mom_type_data_IO
!
      use m_precision
!
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_filter_elen_data_type(FEM_elens)
!
      use gz_filter_moments_IO
      use gz_filter_mom_type_ele_IO
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call write_filter_elen_head_gz(FEM_elens%nnod_filter_mom,         &
     &    FEM_elens%nele_filter_mom, FEM_elens%filter_conf%nf_type)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_gz(FEM_elens%filter_conf)
        call write_elen_ele_type_gz(FEM_elens%nele_filter_mom,          &
     &      FEM_elens%elen_ele)
      end if
!
      call dealloc_filter_mom_type(FEM_elens)
!
      end subroutine gz_write_filter_elen_data_type
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_filter_moms_data_type(FEM_elens, FEM_moms)
!
      use t_filter_moments
      use gz_filter_moments_IO
      use gz_filter_mom_type_ele_IO
!
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head_gz(FEM_moms%nnod_fmom,                &
     &    FEM_moms%nele_fmom, FEM_moms%num_filter_moms,                 &
     &    FEM_elens%filter_conf%nf_type)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_gz(FEM_elens%filter_conf)
        do ifil = 1, FEM_moms%num_filter_moms
          call write_filter_moms_ele_type_gz(FEM_moms%nele_fmom,        &
     &        FEM_moms%mom_ele(ifil))
        end do
      end if
!
      call dealloc_filter_moms_ele_type(FEM_moms)
!
      end subroutine gz_write_filter_moms_data_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_moment_num_type(FEM_elens, FEM_moms)
!
      use t_filter_moments
      use gz_filter_moments_IO
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      call read_filter_moms_head_gz(FEM_elens%nnod_filter_mom,          &
     &    FEM_elens%nele_filter_mom, FEM_moms%num_filter_moms,          &
     &    FEM_elens%filter_conf%nf_type)
!
      end subroutine gz_read_filter_moment_num_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_elen_data_type(numnod, numele,          &
     &          FEM_elens, ierr)
!
      use gz_filter_moments_IO
      use gz_filter_mom_type_ele_IO
!
!
      integer (kind=kint), intent(in) :: numnod, numele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer (kind=kint), intent(inout) :: ierr
!
!
      call read_filter_elen_head_gz(FEM_elens%nnod_filter_mom,          &
     &    FEM_elens%nele_filter_mom, FEM_elens%filter_conf%nf_type)
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        ierr = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_elen_ele_type(FEM_elens%nele_filter_mom,               &
     &    FEM_elens%elen_ele)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call read_base_filter_info_type_gz(FEM_elens%filter_conf)
        call read_elen_ele_type_gz(FEM_elens%nele_filter_mom,           &
     &      FEM_elens%elen_ele)
      end if
!
      end subroutine gz_read_filter_elen_data_type
!
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_moms_data_type(numnod, numele,          &
     &          FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
      use gz_filter_mom_type_ele_IO
!
      integer (kind=kint), intent(in) :: numnod, numele
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer (kind=kint), intent(inout) :: ierr
!
      integer (kind=kint) :: ifil
!
!
      call gz_read_filter_moment_num_type(FEM_elens, FEM_moms)
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        ierr = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      FEM_moms%nnod_fmom = FEM_elens%nnod_filter_mom
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_filter_moms_ele_type(FEM_elens%nele_filter_mom,        &
     &    FEM_moms)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
!
        call read_base_filter_info_type_gz(FEM_elens%filter_conf)
        do ifil = 1, FEM_moms%num_filter_moms
          call read_filter_moms_ele_type_gz(FEM_moms%nele_fmom,         &
     &        FEM_moms%mom_ele(ifil))
        end do
!
      end if
!
      end subroutine gz_read_filter_moms_data_type
!
! ----------------------------------------------------------------------
!
      end module gz_filter_mom_type_data_IO
