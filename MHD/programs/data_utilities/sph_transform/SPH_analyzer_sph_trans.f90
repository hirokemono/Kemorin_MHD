!SPH_analyzer_sph_trans.f90
!     module SPH_analyzer_sph_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_initialize_sph_trans
!
!!      subroutine SPH_initialize_sph_trans(SPH_MHD)
!!      subroutine SPH_analyze_sph_trans                                &
!!     &         (i_step, sph_file_IO, SPH_MHD, fld_IO)
!!      subroutine SPH_analyze_sph_zm_trans                             &
!!     &         (i_step, sph_file_IO, SPH_MHD, fld_IO)
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(field_IO), intent(inout) :: fld_IO
!
      module SPH_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_SPH_transforms
      use calypso_mpi
!
      use t_time_data
!
      implicit none
!
      type(time_data), save, private :: time_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_sph_trans(SPH_MHD)
!
      use m_legendre_transform_list
!
      use t_ctl_params_sph_trans
      use t_SPH_mesh_field_data
      use t_phys_name_4_sph_trans
!
      use count_num_sph_smp
      use init_sph_trans
      use sph_transfer_all_field
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!  ---- allocate spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(SPH_MHD%fld, fld_rtp_TRNS)
      call calypso_mpi_barrier
!
      call alloc_phys_data_type                                         &
     &   (SPH_MHD%sph%sph_rj%nnod_rj, SPH_MHD%fld)
      call calypso_mpi_barrier
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp(fld_rtp_TRNS)
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans(fld_rtp_TRNS%ncomp_trans,               &
     &    fld_rtp_TRNS%num_vector, fld_rtp_TRNS%nscalar_trans,          &
     &    SPH_MHD%sph, SPH_MHD%comms, trns_param, WK_sph_TRNS)
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'allocate_d_rtp_4_all_trans'
      call allocate_d_rtp_4_all_trans                                   &
     &   (fld_rtp_TRNS, SPH_MHD%sph%sph_rtp)
!
      end subroutine SPH_initialize_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_trans                                  &
     &         (i_step, sph_file_IO, SPH_MHD, fld_IO)
!
      use t_file_IO_parameter
      use t_SPH_mesh_field_data
      use t_field_data_IO
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
      use sph_transfer_all_field
      use const_global_element_ids
!
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: sph_file_IO
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(field_IO), intent(inout) :: fld_IO
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, femmesh_STR%mesh, trns_param,     &
     &    fld_rtp_TRNS, field_STR, SPH_MHD%fld, WK_sph_TRNS)
!
!      call check_all_field_data(my_rank, SPH_MHD%fld)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_phys_data_to_IO'
      call copy_rj_phys_data_to_IO                                      &
     &   (SPH_MHD%fld%num_phys, SPH_MHD%fld, fld_IO)
!
      call reset_time_data(time_IO)
      call sel_write_step_SPH_field_file(nprocs, my_rank, i_step,       &
     &    sph_file_IO, time_IO, fld_IO)
!
      end subroutine SPH_analyze_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_zm_trans                               &
     &         (i_step, sph_file_IO, SPH_MHD, fld_IO)
!
      use t_file_IO_parameter
      use t_SPH_mesh_field_data
      use t_field_data_IO
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
      use const_global_element_ids
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: sph_file_IO
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(field_IO), intent(inout) :: fld_IO
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, femmesh_STR%mesh,                 &
     &    trns_param, fld_rtp_TRNS, field_STR, SPH_MHD%fld,             &
     &    WK_sph_TRNS)
!
!      call check_all_field_data(my_rank, SPH_MHD%fld)
!
!  pick zonal mean
!
      if (iflag_debug.gt.0)  write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr                                    &
     &   (SPH_MHD%sph%sph_rj, SPH_MHD%fld)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_phys_data_to_IO'
      call copy_rj_phys_data_to_IO                                      &
     &   (SPH_MHD%fld%num_phys, SPH_MHD%fld, fld_IO)
      call count_number_of_node_stack                                   &
     &   (fld_IO%nnod_IO, fld_IO%istack_numnod_IO)
!
      call reset_time_data(time_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, i_step, sph_file_IO, time_IO, fld_IO)
!
      end subroutine SPH_analyze_sph_zm_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_sph_trans
