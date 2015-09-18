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
      use volume_rendering
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
      use volume_rendering
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
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
!
      use volume_rendering
!
!
      call pvr_init(node1%numnod, ele1%numele, surf1%numsurf,           &
     &    surf1%nnod_4_surf, node1%xx, ele1%interior_ele,               &
     &    surf1%ie_surf, surf1%isf_4_ele, surf1%iele_4_surf,            &
     &    ele_grp1, nod_fld1%num_phys, phys_nod_name)
!
      end subroutine pvr_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main_1st(istep_pvr)
!
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
!
      call pvr_main                                                     &
     &   (istep_pvr, node1%numnod, ele1%numele, surf1%numsurf,          &
     &    ele1%nnod_4_ele, surf1%nnod_4_surf, node1%istack_nod_smp,     &
     &    ele1%istack_ele_smp, node1%xx, node1%rr, node1%a_r, node1%ss, &
     &    node1%a_s, ele1%ie, ele1%a_vol_ele, ele1%interior_ele,        &
     &    surf1%ie_surf, surf1%isf_4_ele, surf1%iele_4_surf,            &
     &    jac1_3d_q%ntot_int, jac1_3d_q%dnx, jac1_3d_q%xjac,            &
     &    nod_fld1%num_phys, num_tot_nod_phys,                          &
     &    istack_nod_component, d_nod)
!
!
      end subroutine pvr_main_1st
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_1st
