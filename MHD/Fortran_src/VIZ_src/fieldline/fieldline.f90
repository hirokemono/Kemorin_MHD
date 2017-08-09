!>@file   fieldline.f90
!!@brief  module fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize(mesh, group, nod_fld)
!!      subroutine FLINE_visualize(istep_fline, mesh, group, ele_mesh,  &
!!     &          ele_4_nod, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
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
      use t_mesh_data
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
      subroutine FLINE_initialize(mesh, group, nod_fld)
!
      use calypso_mpi
      use m_control_data_flines
      use m_source_4_filed_line
      use m_local_fline
      use set_fline_control
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
!
      num_fline = num_fline_ctl
      if (num_fline .le. 0) return
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control                                          &
     &   (mesh%ele, group%ele_grp, group%surf_grp, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_local_data_4_fline'
      call allocate_local_data_4_fline(mesh%node%numnod)
      call allocate_start_point_fline
      call allocate_num_gl_start_fline(nprocs)
      call allocate_local_fline
      call allocate_global_fline_num
!
      end subroutine FLINE_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_visualize(istep_fline, mesh, group, ele_mesh,    &
     &          ele_4_nod, nod_fld)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
!
      integer(kind = kint), intent(in) :: istep_fline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(element_around_node), intent(in) :: ele_4_nod
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: i_fln
!
!
      if (num_fline.le.0 .or. istep_fline .le. 0) return
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      call set_local_field_4_fline(mesh%node, nod_fld)
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline                                 &
     &     (i_fln, mesh%node, mesh%ele, ele_mesh%surf, group%ele_grp)
      end do
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(i_fln, mesh%node, mesh%ele,            &
     &      ele_mesh%surf, ele_4_nod, mesh%nod_comm)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data(istep_fline, i_fln)
      end do
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      end module fieldline
