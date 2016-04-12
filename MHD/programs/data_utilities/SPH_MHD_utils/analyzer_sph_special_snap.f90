!>@file   analyzer_sph_special_snap.f90
!!@brief  module analyzer_sph_special_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        including some special treatment
!!
!!@verbatim
!!      subroutine evolution_sph_special_snap
!!@endverbatim
!
      module analyzer_sph_special_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
!
      implicit none
!
      private :: SPH_analyze_special_snap
      private :: lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_special_snap
!
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_mesh_data
      use m_node_phys_data
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use sections_for_1st
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
        if( mod(i_step_MHD,i_step_output_rst) .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_special_snap'
        call SPH_analyze_special_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'SPH_to_FEM_bridge_special_snap'
        call SPH_to_FEM_bridge_special_snap
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(i_step_MHD, istep_psf, istep_iso,      &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_eleps_time(12)
          call visualize_surface(istep_psf, istep_iso,                  &
     &        mesh1, ele_mesh1, nod_fld1)
          call end_eleps_time(12)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_special_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(idx_rj_degree_zero,                &
     &    nnod_rj, nidx_rj, radius_1d_rj_r, reftemp_rj,                 &
     &    ntot_phys_rj, rj_fld1%d_fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(idx_rj_degree_zero,               &
     &    nnod_rj, nidx_rj, radius_1d_rj_r, reftemp_rj,                 &
     &    ntot_phys_rj, rj_fld1%d_fld)
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd
      call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(11)
      call end_eleps_time(4)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_special_rj_fields
!
      use m_sph_phys_address
      use m_spheric_parameter
!
      use cal_zonal_mean_sph_spectr
!
      integer (kind =kint), allocatable :: ipick_degree(:)
      integer(kind = kint) :: ltr_half
      integer(kind = kint) :: l
!
!
      ltr_half = 1*(l_truncation+1) / 2
      allocate(ipick_degree(ltr_half))
      do l = 1, ltr_half
        ipick_degree(l) = l-1
      end do
!
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,               &
!     &    ithree, ipol%i_velo)
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,               &
!     &    ithree, ipol%i_magne)
!      deallocate(ipick_degree)

      if (my_rank.eq.0) write(*,*) 'delete zonam mean velocity'
      call take_zonal_mean_rj_field(ithree, ipol%i_velo)
      call take_zonal_mean_rj_field(ithree, ipol%i_vort)
      if (my_rank.eq.0) write(*,*) 'delete zonam mean toroidal'
      call delete_zonal_mean_rj_field(ione, ipol%i_velo)
      call delete_zonal_mean_rj_field(ione, idpdr%i_velo)
      call delete_zonal_mean_rj_field(ione, itor%i_vort)
!
      end subroutine set_special_rj_fields
!
! ----------------------------------------------------------------------
!
      subroutine lead_special_fields_4_sph_mhd
!
      use t_phys_address
      use m_sph_phys_address
      use output_viz_file_control
      use lead_fields_4_sph_mhd
!
      use cal_zonal_mean_sph_spectr
      use sph_transforms_4_MHD
!
!
      call s_lead_fields_4_sph_mhd
!
      call sph_back_trans_4_MHD
!
      call sph_forward_trans_snapshot_MHD
!
! ----  Take zonal mean
!
      if (my_rank.eq.0) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr
!
      end subroutine lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_special_snap
!
      use m_mesh_data
      use m_node_phys_data
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
      use m_sph_spectr_data
!
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp(mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_from_trans(mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_to_trans(mesh1%node, iphys, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(mesh1%node, nod_fld1)
!
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd(mesh1%node, iphys, nod_fld1)
!
      call nod_fields_send_recv(mesh1%node, mesh1%nod_comm, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_special_snap
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_special_snap
