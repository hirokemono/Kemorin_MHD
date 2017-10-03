!SPH_analyzer_gauss_b_trans.f90
!     module SPH_analyzer_gauss_b_trans
!
!      Written by H. Matsui
!
!!      subroutine SPH_init_gauss_back_trans(files_param, SPH_MHD)
!!      subroutine SPH_analyze_gauss_back_trans                         &
!!     &         (i_step, viz_step, SPH_MHD, visval)
!
      module SPH_analyzer_gauss_b_trans
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use m_SPH_transforms
      use t_work_4_sph_trans
      use t_SPH_mesh_field_data
      use t_phys_name_4_sph_trans
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
      subroutine SPH_init_gauss_back_trans(files_param, SPH_MHD)
!
      use m_legendre_transform_list
      use t_ctl_params_sph_trans
!
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use init_sph_trans
      use pole_sph_transform
      use sph_transfer_all_field
!
      type(SPH_TRNS_file_IO_params), intent(in) :: files_param
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(SPH_MHD%fld, fld_rtp_TRNS)
!
!  ------    set original spectr modes
!
      call set_sph_magne_address(SPH_MHD%fld, SPH_MHD%ipol)
      call set_cmb_icb_radial_point                                     &
     &   (files_param%cmb_radial_grp, files_param%icb_radial_grp,       &
     &    SPH_MHD%groups%radial_rj_grp)
!
!  ---- allocate spectr data
!
      call alloc_phys_data_type                                         &
     &   (SPH_MHD%sph%sph_rj%nnod_rj, SPH_MHD%fld)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp(fld_rtp_TRNS)
      call initialize_sph_trans(fld_rtp_TRNS%ncomp_trans,               &
     &    fld_rtp_TRNS%num_vector, fld_rtp_TRNS%nscalar_trans,          &
     &    SPH_MHD%sph, SPH_MHD%comms, trns_gauss, WK_sph_TRNS)
      call init_pole_transform(SPH_MHD%sph%sph_rtp)
      call allocate_d_pole_4_all_trans                                  &
     &   (fld_rtp_TRNS, SPH_MHD%sph%sph_rtp)
!
      end subroutine SPH_init_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_gauss_back_trans                           &
     &         (i_step, viz_step, SPH_MHD, visval)
!
      use t_ctl_params_sph_trans
      use t_VIZ_step_parameter
!
      use r_interpolate_sph_data
!
      use sph_transfer_all_field
      use set_parallel_file_name
!
!
      integer(kind = kint), intent(in) :: i_step
      type(VIZ_step_params), intent(in) :: viz_step
!
      integer(kind = kint), intent(inout) :: visval
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      character(len=kchara) :: fname_tmp
!
!
      call accum_output_flag_4_viz(i_step, viz_step, visval)
      visval = visval * output_IO_flag(i_step, t_STR%ucd_step)
!
      if(visval .eq. 0) then
!
!
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'read_gauss_global_coefs'
        call add_int_suffix                                             &
     &     (i_step, d_gauss_trans%fhead_gauss, fname_tmp)
        call add_dat_extension(fname_tmp, d_gauss_trans%fname_gauss)
        call read_gauss_global_coefs(d_gauss_trans)
!
!    copy and extend magnetic field to outside
!
        if (iflag_debug.gt.0) write(*,*)                                &
     &                        'set_poloidal_b_by_gauss_coefs'
        call set_poloidal_b_by_gauss_coefs                              &
     &     (SPH_MHD%sph%sph_rj, SPH_MHD%ipol, d_gauss_trans,            &
     &      SPH_MHD%fld)
        call dealloc_gauss_global_coefs(d_gauss_trans)
!
!        call check_all_field_data(my_rank, SPH_MHD%fld)
!  spherical transform for vector
        call sph_b_trans_all_field                                      &
     &     (SPH_MHD%sph, SPH_MHD%comms, femmesh_STR%mesh,               &
     &      trns_gauss, fld_rtp_TRNS, SPH_MHD%fld,                      &
     &      field_STR, WK_sph_TRNS)
      end if
!
      end subroutine SPH_analyze_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_gauss_b_trans
