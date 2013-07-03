!SPH_analyzer_zm_streamfunc.f90
!     module SPH_analyzer_zm_streamfunc
!
!      Written by H. Matsui
!
!      subroutine SPH_analyze_zm_streamfunc(i_step, visval)
!      subroutine set_ctl_data_4_zm_streamline
!
      module SPH_analyzer_zm_streamfunc
!
      use m_precision
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
      private :: set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_streamfunc(i_step, visval)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_field_data_IO
      use m_node_id_spherical_IO
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
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
        call set_rj_phys_for_zm_streamfunc
        call zonal_mean_all_sph_spectr
!
!  spherical transform for vector
        call sph_b_trans_all_vector
!
      end if
!
      end subroutine SPH_analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_streamline
!
      use m_ctl_data_4_fields
      use m_phys_labels
!
      integer(kind = kint) :: ifld, iflag_velo, iflag_magne
!
      iflag_velo =  0
      iflag_magne = 0
      do ifld = 1, num_nod_phys_ctl
        if(phys_nod_name_ctl(ifld) .eq. fhd_velo)  iflag_velo =  1
        if(phys_nod_name_ctl(ifld) .eq. fhd_magne) iflag_magne = 1
      end do
!
      call deallocate_phys_control
!
      num_nod_phys_ctl = iflag_velo + iflag_magne
      call allocate_phys_control
!
      ifld = 0
      if(iflag_velo .eq. 1) then
        ifld = ifld+1
        phys_nod_name_ctl(ifld) = fhd_velo
        visualize_ctl(ifld) =  'Viz_On'
        monitor_ctl(ifld) =    'Monitor_Off'
      end if
      if(iflag_magne .eq. 1) then
        ifld = ifld+1
        phys_nod_name_ctl(ifld) = fhd_magne
        visualize_ctl(ifld) =     'Viz_On'
        monitor_ctl(ifld) =       'Monitor_Off'
      end if
!
      end subroutine set_ctl_data_4_zm_streamline
!
! ----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_zm_streamfunc
!
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
!
      integer(kind = kint) :: inod, ist, i
!
!
      do i = 1, num_phys_rj
        if     (phys_name_rj(i) .eq. fhd_velo                           &
     &     .or. phys_name_rj(i) .eq. fhd_magne) then
          ist = istack_phys_comp_rj(i-1)
!$omp parallel do
          do inod = 1, nnod_rj
            d_rj(inod,ist+3) =  d_rj(inod,ist+1)
            d_rj(inod,ist+1) =  zero
            d_rj(inod,ist+2) =  zero
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
     end module SPH_analyzer_zm_streamfunc
