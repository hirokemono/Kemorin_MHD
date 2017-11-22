!>@file   fieldline.f90
!!@brief  module fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize(mesh, group, nod_fld, fline_ctls)
!!      subroutine FLINE_visualize(istep_fline, mesh, group, ele_mesh,  &
!!     &          ele_4_nod, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!@endverbatim
!
      module fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_fline
      use m_geometry_constants
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_phys_data
      use t_source_of_filed_line
      use t_local_fline
      use t_global_fieldline
!
      implicit  none
!
      type(fieldline_source), save :: fline_src1
      type(fieldline_trace), save :: fline_tce1
      type(local_fieldline), save :: fline_lc1
      type(global_fieldline_data), save :: fline_gl1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_initialize(mesh, group, nod_fld, fline_ctls)
!
      use calypso_mpi
      use t_control_data_flines
      use set_fline_control
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
!
!
      num_fline = fline_ctls%num_fline_ctl
      if (num_fline .le. 0) return
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control                                          &
     &   (mesh%ele, group%ele_grp, group%surf_grp, nod_fld,             &
     &    fline_ctls, fline_src1)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_local_data_4_fline'
      call alloc_local_data_4_fline(mesh%node%numnod, fline_src1)
      call alloc_start_point_fline(fline_src1)
      call allocate_num_gl_start_fline(nprocs)
      call alloc_num_gl_start_fline                                     &
     &   (nprocs, num_fline, ntot_each_field_line, fline_tce1)
      call alloc_local_fline(fline_lc1)
      call alloc_global_fline_num(fline_gl1)
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
      call set_local_field_4_fline(mesh%node, nod_fld, fline_src1)
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline                                 &
     &     (i_fln, mesh%node, mesh%ele, ele_mesh%surf, group%ele_grp,   &
     &      fline_src1, fline_tce1)
      end do
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(i_fln, mesh%node, mesh%ele,            &
     &      ele_mesh%surf, ele_4_nod, mesh%nod_comm,                    &
     &      fline_src1, fline_tce1, fline_lc1)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data                                        &
     &    (istep_fline, i_fln, fline_lc1, fline_gl1)
      end do
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_finalize
!
      call dealloc_local_data_4_fline(fline_src1)
      call dealloc_local_start_grp_item(fline_src1)
      call dealloc_start_point_fline(fline_src1)
!
      call dealloc_num_gl_start_fline(fline_tce1)
      call dealloc_local_fline(fline_lc1)
      call dealloc_global_fline_num(fline_gl1)
!
      end subroutine FLINE_finalize
!
!  ---------------------------------------------------------------------
      end module fieldline
