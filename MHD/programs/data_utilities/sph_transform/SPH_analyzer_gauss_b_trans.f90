!SPH_analyzer_gauss_b_trans.f90
!     module SPH_analyzer_gauss_b_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_init_gauss_back_trans
!      subroutine SPH_analyze_gauss_back_trans(i_step, visval)
!
      module SPH_analyzer_gauss_b_trans
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_gauss_back_trans
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_node_id_spherical_IO
      use m_sph_spectr_data
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
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp
!
!  ------    set original spectr modes
!
      call set_sph_magne_address(rj_fld1)
      call set_cmb_icb_radial_point(cmb_radial_grp, icb_radial_grp)
!
!  ---- allocate spectr data
!
      call alloc_phys_data_type(nnod_rj, rj_fld1)
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp
      call initialize_sph_trans
      call init_pole_transform
      call allocate_d_pole_4_all_trans
!
      end subroutine SPH_init_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_gauss_back_trans(i_step, visval)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_global_gauss_coefs
!
      use r_interpolate_sph_data
!
      use sph_transfer_all_field
      use set_exit_flag_4_visualizer
      use set_parallel_file_name
!
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: visval
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
        call add_int_suffix(i_step, fhead_gauss, fname_tmp)
        call add_dat_extension(fname_tmp, fname_gauss)
        call read_gauss_global_coefs
!
!    copy and extend magnetic field to outside
!
        if (iflag_debug.gt.0) write(*,*)                                &
     &                        'set_poloidal_b_by_gauss_coefs'
        call set_poloidal_b_by_gauss_coefs(rj_fld1)
        call deallocate_gauss_global_coefs
!
!        call check_all_field_data(my_rank, rj_fld1)
!  spherical transform for vector
        call sph_b_trans_all_field(femmesh_STR%mesh, field_STR)
      end if
!
      end subroutine SPH_analyze_gauss_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_gauss_b_trans
