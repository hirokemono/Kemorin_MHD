!
!------- module cal_volume_node_MHD ---------------
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on Aug., 2007
!
!!      subroutine const_MHD_jacobian_and_volumes (node, ele, sf_grp,   &
!!     &          layer_tbl, infty_list, jac_3d_l, jac_3d_q)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scalar_surf_BC_list), intent(in) :: infty_list
!!        type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
!!        type(layering_tbl), intent(inout) :: layer_tbl
!
      module cal_volume_node_MHD
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_boundary
!
      implicit none
!
      real(kind=kreal) :: vol_fl_local
      private :: vol_fl_local
!
      private :: cal_volume_4_area
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_MHD_jacobian_and_volumes (node, ele, sf_grp,     &
     &          layer_tbl, infty_list, jac_3d_l, jac_3d_q)
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_mean_square_values
      use t_jacobians
      use t_layering_ele_list
!
      use const_jacobians_3d
      use int_volume_of_domain
      use cal_layered_volumes
      use sum_volume_of_domain
!
      type(node_data), intent(in) :: node
      type(element_data), intent(inout) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_surf_BC_list), intent(in) :: infty_list
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
      type(layering_tbl), intent(inout) :: layer_tbl
!
!    Construct Jacobians
!
      call max_int_point_by_etype(ele%nnod_4_ele)
      call cal_jacobian_element                                         &
     &   (node, ele, sf_grp, infty_list, jac_3d_l, jac_3d_q)
!
!    Construct volumes
!
      call allocate_volume_4_smp
!
      if (iflag_debug.eq.1) write(*,*) 's_int_volume_of_domain'
      call s_int_volume_of_domain(ele, jac_3d_q)
!
!     ---  lead total volume of each area
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_fluid'
      call cal_volume_4_area(ele, fluid1)
!
      if (fluid1%istack_ele_fld_smp(np_smp)                             &
     &   .eq. fluid1%istack_ele_fld_smp(0)) then
        rms_local(ivol) = vol_local
      else
        rms_local(ivol) = vol_fl_local
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_conduct'
      call cal_volume_4_area(ele, conduct1)
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_insulate'
      call cal_volume_4_area(ele, insulate1)
!
       if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (iflag_debug.eq.1) write(*,*) 's_cal_layered_volumes'
         call s_cal_layered_volumes(ele, layer_tbl)
       end if
!
!
!
!       call s_int_volume_insulate_core(ele, inner_core)
!
      call deallocate_volume_4_smp
      call dealloc_dxi_dx_type(jac_3d_q)
      call dealloc_dxi_dx_type(jac_3d_l)
!
!
      if (iflag_debug.eq.1) then
        write(*,*) 'volume:       ', ele%volume
        write(*,*) 'vol_fluid:    ', fluid1%volume
        write(*,*) 'vol_conduct:  ', conduct1%volume
        write(*,*) 'vol_insulate: ', insulate1%volume
      end if
!
      end subroutine const_MHD_jacobian_and_volumes
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_area(ele, area)
!
      use t_geometry_data_MHD
      use sum_volume_of_domain
!
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(inout) :: area
!
!
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    area%istack_ele_fld_smp, ele%volume_ele, vol_fl_local)
!
      call MPI_allREDUCE (vol_fl_local, area%volume, ione,              &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (area%volume .eq. 0.0d0) then
        area%a_volume = 1.0d30
      else
        area%a_volume = 1.0d0 / area%volume
      end if
!
      end subroutine cal_volume_4_area
!
!  ---------------------------------------------------------------------
!
      end module cal_volume_node_MHD
