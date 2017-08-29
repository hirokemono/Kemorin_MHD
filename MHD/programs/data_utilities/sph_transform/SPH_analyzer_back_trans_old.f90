!SPH_analyzer_back_trans_old.f90
!     module SPH_analyzer_back_trans_old
!
!      Written by H. Matsui
!
!!      subroutine SPH_initialize_back_trans(files_param, sph_mesh,     &
!!     &          ipol, idpdr, itor, rj_fld, t_IO, fld_IO)
!!        type(SPH_TRNS_file_IO_params), intent(in) :: files_param
!!        type(sph_mesh_data), intent(inout) :: sph_mesh
!!        type(phys_address), intent(inout) :: ipol, idpdr, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO), intent(inout) :: fld_IO
!!        type(time_data), intent(inout) :: t_IO
!
      module SPH_analyzer_back_trans_old
!
      use m_precision
      use m_machine_parameter
      use m_SPH_transforms
      use calypso_mpi
!
      use t_ctl_params_sph_trans
      use t_spheric_mesh
      use t_phys_address
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_phys_name_4_sph_trans
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_back_trans(files_param, sph_mesh,       &
     &          ipol, idpdr, itor, rj_fld, t_IO, fld_IO)
!
      use m_legendre_transform_list
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use set_sph_phys_address
      use init_sph_trans
      use pole_sph_transform
      use sph_transfer_all_field
!
      type(SPH_TRNS_file_IO_params), intent(in) :: files_param
      type(sph_mesh_data), intent(inout) :: sph_mesh
      type(phys_address), intent(inout) :: ipol, idpdr, itor
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
      type(time_data), intent(inout) :: t_IO
!
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &   t_STR%init_d%i_time_step, files_param%org_rst_file_IO,         &
     &   t_IO, fld_IO)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(rj_fld, fld_rtp_TRNS)
!
!  ------    set original spectr modes
!
      if(files_param%org_rj_file_IO%iflag_IO .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans(files_param%org_rj_file_IO,         &
     &      sph_mesh%sph%sph_params%l_truncation, sph_mesh%sph%sph_rj)
        call set_sph_magne_address(rj_fld, ipol)
      end if
!
      call set_cmb_icb_radial_point                                     &
     &   (files_param%cmb_radial_grp, files_param%icb_radial_grp,       &
     &    sph_mesh%sph_grps%radial_rj_grp)
!
!  ---- allocate spectr data
!
      call set_sph_sprctr_data_address                                  &
     &   (sph_mesh%sph%sph_rj, ipol, idpdr, itor, rj_fld)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp(fld_rtp_TRNS)
      call initialize_sph_trans(fld_rtp_TRNS%ncomp_trans,               &
     &    fld_rtp_TRNS%num_vector, fld_rtp_TRNS%nscalar_trans,          &
     &    sph_mesh%sph, sph_mesh%sph_comms, trns_param, WK_sph_TRNS)
!
      call init_pole_transform(sph_mesh%sph%sph_rtp)
      call allocate_d_pole_4_all_trans                                  &
     &   (fld_rtp_TRNS, sph_mesh%sph%sph_rtp)
!
!      call calypso_MPI_barrier
!      call check_schmidt_poly_rtm(my_rank+40, sph_mesh%sph%sph_rtm,    &
!     &    sph_mesh%sph%sph_rlm, trns_param%leg)
!
      end subroutine SPH_initialize_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans_old
