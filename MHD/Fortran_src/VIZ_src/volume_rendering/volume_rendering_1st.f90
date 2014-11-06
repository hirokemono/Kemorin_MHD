!volume_rendering_1st.f90
!      module volume_rendering_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_pvr
!      subroutine visualize_pvr(istep_psf)
!
!      subroutine pvr_init_1st
!      subroutine pvr_main_1st(istep_pvr)
!
      module volume_rendering_1st
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_pvr
!
      use m_control_data_pvrs
      use m_control_params_4_pvr
!
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) call pvr_init_1st
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr(istep_pvr)
!
      use m_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: istep_pvr
!
!
      if (num_pvr.gt.0 .and. istep_pvr.gt.0) then
        call pvr_main_1st(istep_pvr)
      end if
!
      end subroutine visualize_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine pvr_init_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_node_phys_data
!
      use volume_rendering
!
!
      call pvr_init(numnod, numele, numsurf,                            &
     &          nnod_4_surf, inod_smp_stack, xx,                        &
     &          e_multi, ie_surf, isf_4_ele, iele_4_surf,               &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name)
!
      end subroutine pvr_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main_1st(istep_pvr)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
!
      call pvr_main(istep_pvr, numnod, numele, numsurf,                 &
     &         nnod_4_ele, nnod_4_surf, inod_smp_stack, iele_smp_stack, &
     &         xx, radius, a_radius, s_cylinder, a_s_cylinder, ie,      &
     &         a_vol_ele, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &         ntot_int_3d, dwx, xjac, num_nod_phys, num_tot_nod_phys,  &
     &         istack_nod_component, d_nod)
!
!
      end subroutine pvr_main_1st
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_1st
