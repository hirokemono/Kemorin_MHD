!analyzer_sph_back_trans.f90
!      module analyzer_sph_back_trans
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize_sph_back_trans
!      subroutine analyze_sph_back_trans
!
      module analyzer_sph_back_trans
!
      use m_precision
      use calypso_mpi
      use m_work_time
      use m_SPH_transforms
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use t_field_data_IO
!
      use SPH_analyzer_back_trans
      use FEM_analyzer_back_trans
      use visualizer_all
!
      implicit none
!
      type(field_IO), save, private :: sph_trns_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_back_trans
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use parallel_load_data_4_sph
      use load_mesh_data
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_4_sph_back_trans'
      call set_control_4_sph_back_trans(ucd_SPH_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(nod_comm, node1, ele1,            &
     &    surf1, edge1, nod_grp1, ele_grp1, sf_grp1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(ele_4_nod_SPH_TRANS,               &
     &    jac_STR_l, jac_STR_q, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans(node1%numnod, sph_trns_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (node1, ele1, surf1, edge1, nod_comm, edge_comm,               &
     &    ele_grp1, sf_grp1, sf_grp_nod1, nod_fld1)
!
      end subroutine initialize_sph_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sph_back_trans
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_back_trans(i_step, visval, sph_trns_IO)
!
        call FEM_analyze_back_trans(ucd_SPH_TRNS, i_step,               &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
        if(visval .eq. 0) then
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        node1, ele1, surf1, edge1, nod_comm, edge_comm,           &
     &        ele_grp1, nod_fld1, ele_4_nod_SPH_TRANS, jac_STR_q)
        end if
      end do
!
      end subroutine analyze_sph_back_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_back_trans
