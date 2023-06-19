!SPH_analyzer_gauss_b_trans.f90
!     module SPH_analyzer_gauss_b_trans
!
!      Written by H. Matsui
!
!!      subroutine SPH_init_gauss_back_trans                            &
!!     &         (SPH_MHD, SPH_STR, SR_sig, SR_r)
!!      subroutine SPH_analyze_gauss_back_trans(i_step, geofem, SPH_MHD,&
!!     &          SPH_STR, nod_fld, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: i_step
!!        type(mesh_data), intent(in) :: geofem
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module SPH_analyzer_gauss_b_trans
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_SPH_data_4_SPH_trans
      use t_work_4_sph_trans
      use t_SPH_mesh_field_data
      use t_phys_name_4_sph_trans
      use t_solver_SR
!
      implicit none
!
      type(parameters_4_sph_trans), save :: trns_gauss
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_gauss_back_trans                              &
     &         (SPH_MHD, SPH_STR, SR_sig, SR_r)
!
      use m_legendre_transform_list
!
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use init_sph_trans
      use pole_sph_transform
      use sph_transfer_all_field
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(SPH_MHD%fld, SPH_STR%fld_rtp)
!
!  ------    set original spectr modes
!
      call set_sph_magne_address(SPH_MHD%fld, SPH_MHD%ipol)
      call set_cmb_icb_radial_point                                     &
     &   (SPH_STR%cmb_radial_grp, SPH_STR%icb_radial_grp,               &
     &    SPH_MHD%groups%radial_rj_grp, SPH_STR%rj_itp)
!
!  ---- allocate spectr data
!
      call alloc_phys_data(SPH_MHD%sph%sph_rj%nnod_rj, SPH_MHD%fld)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp(SPH_STR%fld_rtp)
      call initialize_sph_trans(SPH_STR%fld_rtp%ncomp_trans,            &
     &    SPH_STR%fld_rtp%num_vector, SPH_STR%fld_rtp%nscalar_trans,    &
     &    SPH_MHD%sph, SPH_MHD%comms, trns_gauss,                       &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig, SR_r)
      call allocate_d_pole_4_all_trans                                  &
     &   (SPH_STR%fld_rtp, SPH_MHD%sph%sph_rtp)
!
      end subroutine SPH_init_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_gauss_back_trans(i_step, geofem, SPH_MHD,  &
     &          SPH_STR, nod_fld, SR_sig, SR_r)
!
      use t_ctl_params_sph_trans
      use t_VIZ_step_parameter
!
      use t_global_gauss_coefs
!
      use sph_transfer_all_field
      use set_parallel_file_name
!
!
      integer(kind = kint), intent(in) :: i_step
      type(mesh_data), intent(in) :: geofem
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(phys_data), intent(inout) :: nod_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: ierr = 0
!
!   Input spectr data
      if (iflag_debug.gt.0) write(*,*) 'read_gauss_global_coefs'
      call read_gauss_global_coefs(i_step, SPH_STR%d_gauss, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, 'File read Error')
!
!    copy and extend magnetic field to outside
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                        'set_poloidal_b_by_gauss_coefs'
      call set_poloidal_b_by_gauss_coefs(SPH_MHD%sph%sph_params,        &
     &   SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_STR%d_gauss,             &
     &    SPH_MHD%fld)
      call dealloc_gauss_global_coefs(SPH_STR%d_gauss)
!
!        call check_all_field_data(my_rank, SPH_MHD%fld)
!  spherical transform for vector
      call sph_b_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, geofem%mesh,                      &
     &    trns_gauss, SPH_STR%fld_rtp, SPH_MHD%fld, nod_fld,            &
     &    SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig, SR_r)
!
      end subroutine SPH_analyze_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_gauss_b_trans
