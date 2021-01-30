!SPH_analyzer_back_trans_old.f90
!     module SPH_analyzer_back_trans_old
!
!      Written by H. Matsui
!
!!      subroutine SPH_initialize_back_trans(SPH_MHD, SPH_STR, t_IO)
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(time_data), intent(inout) :: t_IO
!
      module SPH_analyzer_back_trans_old
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_SPH_data_4_SPH_trans
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_SPH_SGS_structure
      use t_phys_name_4_sph_trans
      use t_work_4_sph_trans
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_back_trans                              &
     &         (i_step, SPH_MHD, SPH_STR, t_IO)
!
      use m_legendre_transform_list
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use set_field_data_w_SGS
      use init_sph_trans
      use sph_transfer_all_field
!
      integer(kind = kint), intent(in) :: i_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(time_data), intent(inout) :: t_IO
!
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    i_step, SPH_STR%org_rst_file_IO, t_IO, SPH_STR%fld_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(SPH_MHD%fld, SPH_STR%fld_rtp)
!
!  ------    set original spectr modes
!
      if(SPH_STR%org_rj_file_IO%iflag_IO .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans(SPH_STR%org_rj_file_IO,             &
     &      SPH_MHD%sph%sph_params%l_truncation, SPH_MHD%sph%sph_rj)
        call set_sph_magne_address(SPH_MHD%fld, SPH_MHD%ipol)
      end if
!
      call set_cmb_icb_radial_point                                     &
     &   (SPH_STR%cmb_radial_grp, SPH_STR%icb_radial_grp,               &
     &    SPH_MHD%groups%radial_rj_grp)
!
!  ---- allocate spectr data
!
      call init_field_data_w_SGS(SPH_MHD%sph%sph_rj%nnod_rj,            &
     &    SPH_MHD%fld, SPH_MHD%ipol, SPH_STR%ipol_LES)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp(SPH_STR%fld_rtp)
      call initialize_sph_trans(SPH_STR%fld_rtp%ncomp_trans,            &
     &    SPH_STR%fld_rtp%num_vector, SPH_STR%fld_rtp%nscalar_trans,    &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_STR%trans_p,                  &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs)
!
      call allocate_d_pole_4_all_trans                                  &
     &   (SPH_STR%fld_rtp, SPH_MHD%sph%sph_rtp)
!
!      call calypso_MPI_barrier
!      call check_schmidt_poly_rtm(my_rank+40, SPH_MHD%sph%sph_rtm,     &
!     &    SPH_MHD%sph%sph_rlm, SPH_STR%trans_p%leg)
!
      end subroutine SPH_initialize_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans_old
