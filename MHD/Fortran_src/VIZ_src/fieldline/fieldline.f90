!>@file   fieldline.f90
!!@brief  module fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine field_line_init(node, ele, ele_grp, sf_grp, nod_fld)
!!      subroutine field_line_main(istep_psf, node, ele, surf,          &
!!     &          ele_grp, ele_4_nod, nod_fld, nod_comm)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(group_data), intent(in) :: ele_grp
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(phys_data), intent(in) :: nod_fld
!!@endverbatim
!
      module fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_fline
      use m_geometry_constants
      use m_global_fline
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_next_node_ele_4_node
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_init(node, ele, ele_grp, sf_grp, nod_fld)
!
      use calypso_mpi
      use m_source_4_filed_line
      use m_local_fline
      use set_fline_control
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control(ele%numele, ele%interior_ele,            &
     &    ele_grp%num_grp, ele_grp%num_item, ele_grp%grp_name,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%grp_name,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    nod_fld%num_phys, nod_fld%phys_name)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_local_data_4_fline'
      call allocate_local_data_4_fline(node%numnod)
      call allocate_start_point_fline
      call allocate_num_gl_start_fline(nprocs)
      call allocate_local_fline
      call allocate_global_fline_num
!
      end subroutine field_line_init
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main(istep_psf, node, ele, surf,            &
     &          ele_grp, ele_4_nod, nod_fld, nod_comm)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
!
      integer(kind = kint), intent(in) :: istep_psf
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(group_data), intent(in) :: ele_grp
      type(element_around_node), intent(in) :: ele_4_nod
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: i_fln
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      call set_local_field_4_fline(node%numnod, node%istack_nod_smp,    &
     &    node%xx, node%rr, node%a_r, node%ss, node%a_s,                &
     &    nod_fld%num_phys, nod_fld%ntot_phys,                          &
     &    nod_fld%istack_component, nod_fld%d_fld)
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline(i_fln, node%numnod,             &
     &      ele%numele, surf%numsurf, surf%nnod_4_surf,                 &
     &      ele%iele_global, ele%interior_ele,                          &
     &      surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,             &
     &      surf%x_surf, surf%vnorm_surf, surf%area_surf,               &
     &      ele_grp%num_grp, ele_grp%num_item, ele_grp%istack_grp,      &
     &      ele_grp%item_grp)
      end do
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(i_fln, node%numnod,                    &
     &      ele%numele, surf%numsurf, surf%nnod_4_surf,                 &
     &      node%inod_global, node%xx, ele%iele_global,                 &
     &      surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,             &
     &      surf%interior_surf, surf%vnorm_surf,                        &
     &      ele_4_nod%ntot, ele_4_nod%istack_4_node,                    &
     &      ele_4_nod%iele_4_node, nod_comm)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data(istep_psf, i_fln)
      end do
!
      end subroutine field_line_main
!
!  ---------------------------------------------------------------------
!
      end module fieldline
