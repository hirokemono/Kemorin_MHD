!>@file   t_fieldline.f90
!!@brief  module t_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize                                     &
!!     &         (femmesh, nod_fld, fline_ctls, fline)
!!      subroutine FLINE_visualize(istep_fline, femmesh, ele_mesh,      &
!!     &          ele_4_nod, nod_fld, fline)
!!      subroutine FLINE_finalize(fline)
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!@endverbatim
!
      module t_fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_local_fline
      use t_global_fieldline
!
      implicit  none
!
      type fieldline_module
        integer(kind = kint) :: num_fline
!
        type(fieldline_paramter), allocatable :: fln_prm(:)
!
        type(each_fieldline_trace), allocatable :: fln_tce(:)
!
        type(fieldline_paramters) :: fline_prm
        type(all_fieldline_source) :: fline_src
        type(local_fieldline) :: fline_lc
        type(global_fieldline_data) :: fline_gl
      end type fieldline_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_initialize                                       &
     &         (femmesh, nod_fld, fline_ctls, fline)
!
      use calypso_mpi
      use t_control_data_flines
      use set_fline_control
!
      type(mesh_data), intent(in) :: femmesh
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_module), intent(inout) :: fline
!
!
      fline%num_fline = fline_ctls%num_fline_ctl
      if(fline%num_fline .le. 0) return
!
      allocate(fline%fln_prm(fline%num_fline))
      allocate(fline%fln_tce(fline%num_fline))
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control(femmesh%mesh, femmesh%group, nod_fld,    &
     &   fline%num_fline, fline_ctls, fline%fln_prm, fline%fline_prm,   &
     &   fline%fline_src)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_local_data_4_fline'
      call alloc_local_data_4_fline                                     &
     &   (fline%num_fline, femmesh%mesh%node, fline%fline_src)
      call alloc_start_point_fline                                      &
     &   (fline%fline_prm%ntot_each_field_line, fline%fline_src)
      call alloc_num_gl_start_fline(nprocs, fline%num_fline,            &
     &    fline%fline_prm%num_each_field_line, fline%fln_tce)
      call alloc_local_fline(fline%fline_lc)
      call alloc_global_fline_num(fline%fline_gl)
!
      end subroutine FLINE_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_visualize(istep_fline, femmesh, ele_mesh,        &
     &          ele_4_nod, nod_fld, fline)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
!
      integer(kind = kint), intent(in) :: istep_fline
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(element_around_node), intent(in) :: ele_4_nod
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
!
      if (fline%num_fline.le.0 .or. istep_fline .le. 0) return
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      call set_local_field_4_fline                                      &
     &   (fline%num_fline, femmesh%mesh%node, nod_fld,                  &
     &    fline%fln_prm, fline%fline_src)
!
      do i_fln = 1, fline%num_fline
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline                                 &
     &     (i_fln, femmesh%mesh, ele_mesh, femmesh%group,               &
     &      fline%fln_prm(i_fln), fline%fline_prm,                      &
     &      fline%fline_src, fline%fln_tce(i_fln))
      end do
!
      do i_fln = 1, fline%num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines                                        &
     &     (i_fln, femmesh%mesh%node, femmesh%mesh%ele,                 &
     &      ele_mesh%surf, ele_4_nod, femmesh%mesh%nod_comm,            &
     &      fline%fln_prm(i_fln), fline%fline_src,                      &
     &      fline%fln_tce(i_fln), fline%fline_lc)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data(istep_fline, fline%fln_prm(i_fln),     &
     &     fline%fline_lc, fline%fline_gl)
      end do
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_finalize(fline)
!
      type(fieldline_module), intent(inout) :: fline
!
!
      call dealloc_control_params_fline(fline%fline_prm)
      call dealloc_fline_starts_ctl                                     &
     &   (fline%num_fline, fline%fln_prm, fline%fline_prm)
      call dealloc_iflag_fline_used_ele(fline%num_fline, fline%fln_prm)
!
      call dealloc_local_data_4_fline(fline%fline_src)
      call dealloc_local_start_grp_item(fline%fline_src)
      call dealloc_start_point_fline(fline%fline_src)
!
      call dealloc_num_gl_start_fline(fline%num_fline, fline%fln_tce)
      call dealloc_local_fline(fline%fline_lc)
      call dealloc_global_fline_num(fline%fline_gl)
!
      deallocate(fline%fln_tce, fline%fln_prm)
!
      end subroutine FLINE_finalize
!
!  ---------------------------------------------------------------------
      end module t_fieldline
