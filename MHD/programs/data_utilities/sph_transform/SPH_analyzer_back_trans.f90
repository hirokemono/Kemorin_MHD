!SPH_analyzer_back_trans.f90
!     module SPH_analyzer_back_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_initialize_back_trans
!      subroutine SPH_analyze_back_trans(i_step, visval)
!
      module SPH_analyzer_back_trans
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
      subroutine SPH_initialize_back_trans
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_sph_spectr_data
      use m_sph_phys_address
!
      use load_data_for_sph_IO
      use r_interpolate_sph_data
      use count_num_sph_smp
      use field_IO_select
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use pole_sph_transform
!
!  ------    set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_fld_file'
      call sel_read_alloc_step_SPH_file(my_rank, i_step_init)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp
!
!  ------    set original spectr modes
!
      if(iflag_org_sph_rj_head .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans(my_rank)
        call set_sph_magne_address
      end if
!
      call set_cmb_icb_radial_point(cmb_radial_grp, icb_radial_grp)
!
!  ---- allocate spectr data
!
      call allocate_phys_rj_data
      call allocate_phys_rtp_data
!
      call set_sph_sprctr_data_address
      call set_sph_nod_data_address
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call copy_sph_trans_nums_from_rtp
      call initialize_sph_trans
      call init_pole_transform
!
!      call calypso_MPI_barrier
!      call check_schmidt_poly_rtm(my_rank+40)
!
      end subroutine SPH_initialize_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_back_trans(i_step, visval)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_field_data_IO
      use m_node_id_spherical_IO
!      use m_schmidt_poly_on_rtm
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use set_exit_flag_4_visualizer
!
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: visval
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
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
!    copy and extend magnetic field to outside
!
        if(iflag_org_sph_rj_head .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO
        end if
!
!          call check_rj_spectr_data(my_rank)
!  spherical transform for vector
        call sph_b_trans_all_field
      end if
!
      end subroutine SPH_analyze_back_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans
