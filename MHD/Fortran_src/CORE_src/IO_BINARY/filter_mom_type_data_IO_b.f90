!filter_mom_type_data_IO_b.f90
!     module filter_mom_type_data_IO_b
!
!     Written by H. Matsui on Feb., 2012
!
!!      subroutine write_filter_elen_data_type_b(FEM_elens)
!!      subroutine write_filter_moms_data_type_b(FEM_elens, FEM_moms)
!!      subroutine read_filter_moment_num_type_b                        &
!!     &         (bin_flags, FEM_elens, FEM_moms)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!
!!      subroutine read_filter_elen_data_type_b(numnod, numele,         &
!!     &          bin_flags, FEM_elens)
!!      subroutine read_filter_moms_data_type_b                         &
!!     &         (numnod, numele, bin_flags, FEM_elens, FEM_moms)
!!        integer (kind=kint), intent(in) :: numnod, numele
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      module filter_mom_type_data_IO_b
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
      subroutine write_filter_elen_data_type_b(FEM_elens)
!
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call write_filter_elen_head_b                                     &
     &   (FEM_elens%nnod_filter_mom, FEM_elens%nele_filter_mom,         &
     &    FEM_elens%filter_conf%nf_type)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b(FEM_elens%filter_conf)
        call write_elen_ele_type_b(FEM_elens%nele_filter_mom,           &
     &      FEM_elens%elen_ele)
      end if
!
      call dealloc_filter_mom_type(FEM_elens)
!
      end subroutine write_filter_elen_data_type_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moms_data_type_b(FEM_elens, FEM_moms)
!
      use t_filter_moments
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head_b                                     &
     &   (FEM_moms%nnod_fmom, FEM_moms%nele_fmom,                       &
     &    FEM_moms%num_filter_moms, FEM_elens%filter_conf%nf_type)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b(FEM_elens%filter_conf)
        do ifil = 1, FEM_moms%num_filter_moms
          call write_filter_moms_ele_type_b                             &
     &       (FEM_moms%nele_fmom, FEM_moms%mom_ele(ifil))
        end do
      end if
!
      call dealloc_filter_moms_ele_type(FEM_moms)
!
      end subroutine write_filter_moms_data_type_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moment_num_type_b                          &
     &         (bin_flags, FEM_elens, FEM_moms)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      call read_filter_moms_head_b(bin_flags%iflag_bin_swap,            &
     &    FEM_elens%nnod_filter_mom, FEM_elens%nele_filter_mom,         &
     &    FEM_moms%num_filter_moms, FEM_elens%filter_conf%nf_type,      &
     &    bin_flags%ierr_IO)
!
      end subroutine read_filter_moment_num_type_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_data_type_b(numnod, numele,           &
     &          bin_flags, FEM_elens)
!
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
!
      integer (kind=kint), intent(in) :: numnod, numele
      type(file_IO_flags), intent(inout) :: bin_flags
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call read_filter_elen_head_b(bin_flags%iflag_bin_swap,            &
     &    FEM_elens%nnod_filter_mom, FEM_elens%nele_filter_mom,         &
     &    FEM_elens%filter_conf%nf_type, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        bin_flags%ierr_IO = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        bin_flags%ierr_IO = 501
      else
        bin_flags%ierr_IO = 0
      end if
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_elen_ele_type(FEM_elens%nele_filter_mom,               &
     &    FEM_elens%elen_ele)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call read_base_filter_info_type_b                               &
     &     (FEM_elens%filter_conf, bin_flags)
        if(bin_flags%ierr_IO .gt. 0) return
        call read_elen_ele_type_b(FEM_elens%nele_filter_mom,            &
     &      FEM_elens%elen_ele, bin_flags)
        if(bin_flags%ierr_IO .gt. 0) return
      end if
!
      end subroutine read_filter_elen_data_type_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_data_type_b                           &
     &         (numnod, numele, bin_flags, FEM_elens, FEM_moms)
!
      use t_filter_moments
      use filter_mom_type_on_ele_IO_b
!
      integer (kind=kint), intent(in) :: numnod, numele
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      integer (kind=kint) :: ifil
!
!
      call read_filter_moment_num_type_b                                &
     &   (bin_flags, FEM_elens, FEM_moms)
      if(bin_flags%ierr_IO .gt. 0) return
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        bin_flags%ierr_IO = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        bin_flags%ierr_IO = 501
      else
        bin_flags%ierr_IO = 0
      end if
      if(bin_flags%ierr_IO .gt. 0) return
!
      FEM_moms%nnod_fmom = FEM_elens%nnod_filter_mom
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_filter_moms_ele_type(FEM_elens%nele_filter_mom,        &
     &    FEM_moms)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
!
        call read_base_filter_info_type_b                               &
     &     (FEM_elens%filter_conf, bin_flags)
        if(bin_flags%ierr_IO .gt. 0) return
!
        do ifil = 1, FEM_moms%num_filter_moms
          call read_filter_moms_ele_type_b                              &
     &       (FEM_moms%nele_fmom, FEM_moms%mom_ele(ifil), bin_flags)
          if(bin_flags%ierr_IO .gt. 0) return
        end do
!
      end if
!
      end subroutine read_filter_moms_data_type_b
!
! ----------------------------------------------------------------------
!
      end module filter_mom_type_data_IO_b
