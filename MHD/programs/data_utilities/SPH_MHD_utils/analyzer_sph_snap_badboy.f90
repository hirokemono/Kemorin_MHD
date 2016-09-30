!>@file   analyzer_sph_snap_badboy.f90
!!@brief  module analyzer_sph_snap_badboy
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine evolution_sph_snap_badboy
!!@endverbatim
!
      module analyzer_sph_snap_badboy
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_element_id_4_node
      use m_jacobians
!
      use SPH_analyzer_snap
      use visualizer_all
      use volume_rendering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap_badboy
!
      use m_spheric_parameter
      use m_node_phys_data
      use m_sph_trans_arrays_MHD
!
      use FEM_analyzer_sph_MHD
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
!
      real(kind = kreal) :: total_max, total_prev
!
!     ---------------------
!
      i_step_check = 0
      i_step_output_ucd = 0
      if(elapsed_time .gt. 1800.0) then
        if (my_rank.eq.0) write(*,*) 'This code can use up to 30 min.'
        elapsed_time = 1800.0
      end if
      if(elapsed_time .lt. 1800.0) elapsed_time = 1800.0
      call start_eleps_time(3)
!
!*  ----------- REad spectr data and get field data --------------
!*
      i_step_MHD = i_step_init
      if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
      call SPH_analyze_snap(i_step_MHD)
!*
      if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
      call SPH_to_FEM_bridge_MHD                                        &
     &   (sph1%sph_params, sph1%sph_rtp, trns_WK1,                      &
     &    mesh1, iphys, nod_fld1)
      if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
      call FEM_analyze_sph_MHD(i_step_MHD, istep_psf, istep_iso,        &
     &    istep_pvr, istep_fline, visval)
      call end_eleps_time(4)
!
      if(visval .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'visualize_all'
        call start_eleps_time(12)
        call visualize_all                                              &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        mesh1, group1, ele_mesh1, nod_fld1,                       &
     &        next_tbl1%neib_ele, jac1_3d_q)
        call deallocate_pvr_data
        call end_eleps_time(12)
      end if
!
!*  ----------- Visualization --------------
!*
      do
        visval = check_PVR_update()
        call calypso_mpi_barrier
!
        if(visval .eq. IFLAG_TERMINATE) then
          if (my_rank.eq.0) write(*,*) 'end flag is recieved'
          exit
!
        else if(visval .eq. IFLAG_UPDATE) then
          if (my_rank.eq.0) then
            write(*,*) 'visualization start!'
            write(*,*) 'Current elapsed time: ', total_time
          end if
!
          call start_eleps_time(12)
          call PVR_initialize                                           &
     &       (mesh1%node, mesh1%ele, ele_mesh1%surf, group1, nod_fld1)
          call PVR_visualize                                            &
     &       (istep_pvr, mesh1%node, mesh1%ele,                         &
     &        ele_mesh1%surf, group1, jac1_3d_q, nod_fld1)
          call deallocate_pvr_data
          call end_eleps_time(12)
        end if
        call end_eleps_time(1)
!
        total_prev = total_time
        total_time = MPI_WTIME() - total_start
!
        if(my_rank .eq. 0) then
          if(int(total_time/60.0) .ne. int(total_prev/60.0)) then
            write(*,*) int(total_time/60), 'minuts passed'
          end if
        end if
!
        if(total_time .gt. elapsed_time) then
          call calypso_mpi_barrier
          call MPI_allREDUCE (total_time, total_max, ione,              &
     &       CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
          exit
        end if
      end do
      call end_eleps_time(3)
!
  10   continue
!    Loop end
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
      end subroutine evolution_sph_snap_badboy
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap_badboy
