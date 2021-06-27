!SPH_analyzer_sph_trans.f90
!     module SPH_analyzer_sph_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_initialize_sph_trans
!
!!      subroutine SPH_initialize_sph_trans(SPH_MHD, SPH_STR,           &
!!     &                                    SR_sig, SR_r)
!!      subroutine SPH_analyze_sph_trans(i_step, geofem, nod_fld,       &
!!     &          SPH_MHD, SPH_STR, SR_sig, SR_r)
!!      subroutine SPH_analyze_sph_zm_trans(i_step, geofem, nod_fld,    &
!!     &          SPH_MHD, SPH_STR, SR_sig, SR_r)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module SPH_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_SPH_data_4_SPH_trans
      use t_time_data
      use t_solver_SR
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
      subroutine SPH_initialize_sph_trans(SPH_MHD, SPH_STR,             &
     &                                    SR_sig, SR_r)
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
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  ---- allocate spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(SPH_MHD%fld, SPH_STR%fld_rtp)
      call calypso_mpi_barrier
!
      call alloc_phys_data(SPH_MHD%sph%sph_rj%nnod_rj, SPH_MHD%fld)
      call calypso_mpi_barrier
!
!  ---- initialize spherical harmonics transform
!
      call copy_sph_trans_nums_from_rtp(SPH_STR%fld_rtp)
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans(SPH_STR%fld_rtp%ncomp_trans,            &
     &    SPH_STR%fld_rtp%num_vector, SPH_STR%fld_rtp%nscalar_trans,    &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_STR%trans_p,                  &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig, SR_r)
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'allocate_d_rtp_4_all_trans'
      call allocate_d_rtp_4_all_trans                                   &
     &   (SPH_STR%fld_rtp, SPH_MHD%sph%sph_rtp)
!
      end subroutine SPH_initialize_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_trans(i_step, geofem, nod_fld,         &
     &          SPH_MHD, SPH_STR, SR_sig, SR_r)
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
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, geofem%mesh, SPH_STR%trans_p,     &
     &    SPH_STR%fld_rtp, nod_fld, SPH_MHD%fld,                        &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig, SR_r)
!
!      call check_all_field_data(my_rank, SPH_MHD%fld)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_phys_data_to_IO'
      call copy_rj_phys_data_to_IO                                      &
     &   (SPH_MHD%fld%num_phys, SPH_MHD%fld, SPH_STR%fld_IO)
!
      call reset_time_data(time_IO)
      call sel_write_step_SPH_field_file                                &
     &   (i_step, SPH_STR%sph_file_IO, time_IO, SPH_STR%fld_IO)
!
      end subroutine SPH_analyze_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_zm_trans(i_step, geofem, nod_fld,      &
     &          SPH_MHD, SPH_STR, SR_sig, SR_r)
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
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, geofem%mesh, SPH_STR%trans_p,     &
     &    SPH_STR%fld_rtp, nod_fld, SPH_MHD%fld,                        &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig, SR_r)
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
     &   (SPH_MHD%fld%num_phys, SPH_MHD%fld, SPH_STR%fld_IO)
      call count_number_of_node_stack                                   &
     &   (SPH_STR%fld_IO%nnod_IO, SPH_STR%fld_IO%istack_numnod_IO)
!
      call reset_time_data(time_IO)
      call sel_write_step_SPH_field_file                                &
     &   (i_step, SPH_STR%sph_file_IO, time_IO, SPH_STR%fld_IO)
!
      end subroutine SPH_analyze_sph_zm_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_sph_trans
