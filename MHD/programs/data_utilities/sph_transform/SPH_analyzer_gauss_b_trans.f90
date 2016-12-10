!SPH_analyzer_gauss_b_trans.f90
!     module SPH_analyzer_gauss_b_trans
!
!      Written by H. Matsui
!
!!      subroutine SPH_init_gauss_back_trans(sph_mesh, ipol, rj_fld)
!!      subroutine SPH_analyze_gauss_back_trans                         &
!!     &         (i_step, sph_mesh, ipol, rj_fld, visval)
!
      module SPH_analyzer_gauss_b_trans
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_work_4_sph_trans
      use t_spheric_mesh
      use t_phys_address
      use t_phys_data
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
      subroutine SPH_init_gauss_back_trans(sph_mesh, ipol, rj_fld)
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_time_data_IO
!
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use pole_sph_transform
      use legendre_transform_select
      use sph_transfer_all_field
!
      type(sph_mesh_data), intent(inout) :: sph_mesh
      type(phys_address), intent(inout) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp(rj_fld)
!
!  ------    set original spectr modes
!
      call set_sph_magne_address(rj_fld, ipol)
      call set_cmb_icb_radial_point(cmb_radial_grp, icb_radial_grp,     &
     &    sph_mesh%sph_grps%radial_rj_grp)
!
!  ---- allocate spectr data
!
      call alloc_phys_data_type(sph_mesh%sph%sph_rj%nnod_rj, rj_fld)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp(ncomp_sph_trans)
      call initialize_sph_trans(ncomp_sph_trans,                        &
     &    sph_mesh%sph, sph_mesh%sph_comms, trns_gauss)
      call init_pole_transform(sph_mesh%sph%sph_rtp)
      call allocate_d_pole_4_all_trans                                  &
     &   (ncomp_sph_trans, sph_mesh%sph%sph_rtp)
!
      end subroutine SPH_init_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_gauss_back_trans                           &
     &         (i_step, sph_mesh, ipol, rj_fld, visval)
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
!
      use r_interpolate_sph_data
!
      use sph_transfer_all_field
      use set_exit_flag_4_visualizer
      use set_parallel_file_name
!
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_mesh_data), intent(in) :: sph_mesh
      type(phys_address), intent(in) :: ipol
!
      integer(kind = kint), intent(inout) :: visval
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara) :: fname_tmp
!
      integer(kind = kint) :: i_udt
!
!
      call set_output_flag(i_udt, i_step, i_step_output_ucd)
      call set_output_flag_4_viz(i_step, visval)
      visval = visval * i_udt
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
     &     (sph_mesh%sph%sph_rj, ipol, d_gauss_trans, rj_fld)
        call dealloc_gauss_global_coefs(d_gauss_trans)
!
!        call check_all_field_data(my_rank, rj_fld)
!  spherical transform for vector
        call sph_b_trans_all_field                                      &
     &     (ncomp_sph_trans, sph_mesh%sph, sph_mesh%sph_comms,          &
     &      femmesh_STR%mesh, trns_gauss, rj_fld, field_STR)
      end if
!
      end subroutine SPH_analyze_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_gauss_b_trans
