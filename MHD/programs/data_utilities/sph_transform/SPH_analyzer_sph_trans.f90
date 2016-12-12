!SPH_analyzer_sph_trans.f90
!     module SPH_analyzer_sph_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_initialize_sph_trans
!
!!      subroutine SPH_initialize_sph_trans(sph_mesh, rj_fld)
!!      subroutine SPH_analyze_sph_trans                                &
!!     &         (i_step, sph_mesh, rj_fld, fld_IO)
!!      subroutine SPH_analyze_sph_zm_trans                             &
!!     &         (i_step, sph_mesh, rj_fld, fld_IO)
!!        type(sph_mesh_data), intent(in) :: sph_mesh
!!        type(phys_data), intent(inout) :: rj_fld
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
      use t_time_data_IO
!
      implicit none
!
      type(time_params_IO), save, private :: time_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_sph_trans(sph_mesh, rj_fld)
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
!
      use t_spheric_mesh
      use t_phys_data
!
      use count_num_sph_smp
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use legendre_transform_select
      use sph_transfer_all_field
!
      type(sph_mesh_data), intent(inout) :: sph_mesh
      type(phys_data), intent(inout) :: rj_fld
!
!  ---- allocate spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(rj_fld)
      call calypso_mpi_barrier
!
      call alloc_phys_data_type(sph_mesh%sph%sph_rj%nnod_rj, rj_fld)
      call calypso_mpi_barrier
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp(ncomp_sph_trans)
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans(ncomp_sph_trans,                        &
     &    sph_mesh%sph, sph_mesh%sph_comms, trns_param)
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'allocate_d_rtp_4_all_trans'
      call allocate_d_rtp_4_all_trans                                   &
     &   (ncomp_sph_trans, sph_mesh%sph%sph_rtp)
!
      end subroutine SPH_initialize_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_trans                                  &
     &         (i_step, sph_mesh, rj_fld, fld_IO)
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use t_spheric_mesh
      use t_phys_data
      use t_field_data_IO
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
      use sph_transfer_all_field
      use const_global_element_ids
!
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_mesh_data), intent(in) :: sph_mesh
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (ncomp_sph_trans, sph_mesh%sph, sph_mesh%sph_comms,            &
     &    femmesh_STR%mesh, trns_param, field_STR, rj_fld)
!
!      call check_all_field_data(my_rank, rj_fld)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_phys_data_to_IO'
      call copy_rj_phys_data_to_IO(rj_fld%num_phys, rj_fld, fld_IO)
!
     call reset_time_data_IO(time_IO)
     call set_field_file_fmt_prefix                                     &
     &   (sph_file_trns_p%iflag_format, sph_file_trns_p%file_prefix,    &
     &    fld_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, i_step, time_IO, fld_IO)
!
      end subroutine SPH_analyze_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_zm_trans                               &
     &         (i_step, sph_mesh, rj_fld, fld_IO)
!
      use m_t_step_parameter
      use t_spheric_mesh
      use t_phys_data
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
      type(sph_mesh_data), intent(in) :: sph_mesh
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
!  spherical transform for vector
      call sph_f_trans_all_field                                        &
     &   (ncomp_sph_trans, sph_mesh%sph, sph_mesh%sph_comms,            &
     &    femmesh_STR%mesh, trns_param, field_STR, rj_fld)
!
!      call check_all_field_data(my_rank, rj_fld)
!
!  pick zonal mean
!
      if (iflag_debug.gt.0)  write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr(sph_mesh%sph%sph_rj, rj_fld)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_phys_data_to_IO'
      call copy_rj_phys_data_to_IO(rj_fld%num_phys, rj_fld, fld_IO)
      call count_number_of_node_stack                                   &
     &   (fld_IO%nnod_IO, fld_IO%istack_numnod_IO)
!
      call reset_time_data_IO(time_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, i_step, time_IO, fld_IO)
!
      end subroutine SPH_analyze_sph_zm_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_sph_trans
