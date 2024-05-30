!t_source_of_filed_line.f90
!
!      module t_source_of_filed_line
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine alloc_local_start_grp_item(fln_src)
!!      subroutine alloc_local_data_4_fline(node, fln_src)
!!        type(node_data), intent(in) :: node
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      subroutine alloc_start_point_fline(fln_prm, fln_src)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      subroutine alloc_num_gl_start_fline(num_pe, num_each_field_line,&
!!     &                                    viz_fields, fln_tce)
!!        integer, intent(in) :: num_pe
!!        integer(kind = kint), intent(in) :: num_each_field_line
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine copy_global_start_fline(i_copied, i_org,             &
!!     &                                   viz_fields, fln_tce)
!!        integer(kind = kint), intent(in) :: i_copied, i_org
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine dealloc_local_data_4_fline(fln_src)
!!      subroutine dealloc_local_start_grp_item(fln_src)
!!      subroutine dealloc_start_point_fline(fln_src)
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      subroutine dealloc_num_gl_start_fline(fln_tce)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module t_source_of_filed_line
!
      use m_precision
      use t_control_params_4_fline
!
      implicit  none
!
!
      type each_fieldline_source
        real(kind = kreal), allocatable :: vector_nod_fline(:,:)
!
        integer(kind = kint) :: nele_start_grp = 0
        integer(kind = kint), allocatable :: iele_start_item(:,:)
        real(kind = kreal),   allocatable :: flux_start(:)
!
        integer(kind = kint) :: num_line_local = 0
        real(kind = kreal), allocatable :: xx4_initial_fline(:,:)
        real(kind = kreal), allocatable :: flux_start_fline(:)
      end type each_fieldline_source
!
      type each_fieldline_trace
        integer(kind = kint), allocatable :: istack_current_fline(:)
        integer(kind = kint), allocatable :: num_current_fline(:)
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!
        integer(kind= kint), allocatable :: iline_original(:)
        integer(kind= kint), allocatable :: iflag_fline(:)
        integer(kind= kint), allocatable :: icount_fline(:)
        integer(kind= kint), allocatable :: isf_fline_start(:,:)
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
        real(kind = kreal), allocatable ::  v_fline_start(:,:)
        real(kind = kreal), allocatable ::  c_fline_start(:,:)
        real(kind = kreal), allocatable ::  trace_length(:)
!
        integer(kind= kint) :: nitem_export
        integer(kind= kint) :: ncomp_export
        integer(kind= kint), allocatable :: id_fline_export(:,:)
        real(kind = kreal), allocatable ::  fline_export(:,:)
      end type each_fieldline_trace
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      allocate(fln_src%iele_start_item(2,fln_src%nele_start_grp))
      allocate(fln_src%flux_start(fln_src%nele_start_grp))
      if(fln_src%nele_start_grp .gt. 0) fln_src%iele_start_item = 0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_data_4_fline(node, fln_src)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      allocate(fln_src%vector_nod_fline(node%numnod,3))
      fln_src%vector_nod_fline = 0.0d0
!
      end subroutine alloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_start_point_fline(fln_prm, fln_src)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: num
!
!
      num = fln_prm%num_each_field_line
      allocate(fln_src%xx4_initial_fline(4,num))
      allocate(fln_src%flux_start_fline(num))
!
      fln_src%xx4_initial_fline = 0.0d0
      fln_src%flux_start_fline =  0.0d0
!
      end subroutine alloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_gl_start_fline(num_pe, num_each_field_line,  &
     &                                    viz_fields, fln_tce)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_each_field_line
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: num, i
!
!
      allocate(fln_tce%istack_current_fline(0:num_pe))
      allocate(fln_tce%num_current_fline(num_pe))
      allocate(fln_tce%flux_stack_fline(0:num_pe))
      fln_tce%istack_current_fline = 0
      fln_tce%num_current_fline =    0
      fln_tce%flux_stack_fline = 0.0d0
!
      num = 2 * num_each_field_line
      allocate(fln_tce%iline_original(num))
      allocate(fln_tce%iflag_fline(num))
      allocate(fln_tce%icount_fline(num))
      allocate(fln_tce%isf_fline_start(2,num))
!
      do i = 1, num
        fln_tce%iline_original(i) = i
      end do
!
      allocate(fln_tce%xx_fline_start(4,num))
      allocate(fln_tce%v_fline_start(4,num))
      allocate(fln_tce%c_fline_start(viz_fields%ntot_color_comp, num))
      allocate(fln_tce%trace_length(num))
!
      fln_tce%iflag_fline =  0
      fln_tce%icount_fline = 0
      fln_tce%isf_fline_start = 0
      fln_tce%v_fline_start =  0.0d0
      fln_tce%c_fline_start =  0.0d0
      fln_tce%xx_fline_start = 0.0d0
      fln_tce%trace_length = 0.0d0
!
      fln_tce%nitem_export = 6
      fln_tce%ncomp_export = 9 + viz_fields%ntot_color_comp
      allocate(fln_tce%id_fline_export(fln_tce%nitem_export,num))
      allocate(fln_tce%fline_export(fln_tce%ncomp_export,num))
      fln_tce%id_fline_export = 0
      fln_tce%fline_export = 0.0d0
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_global_start_fline(i_copied, i_org,               &
     &                                   viz_fields, fln_tce)
!
      use t_ctl_params_viz_fields
!
      integer(kind = kint), intent(in) :: i_copied, i_org
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
          fln_tce%xx_fline_start(1:4,i_copied)                          &
     &          = fln_tce%xx_fline_start(1:4,i_org)
          fln_tce%v_fline_start(1:4,i_copied)                           &
     &          = fln_tce%v_fline_start(1:4,i_org)
          fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,i_copied)  &
     &      = fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,i_org)
!
      end subroutine copy_global_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_start_grp_item(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      deallocate(fln_src%iele_start_item, fln_src%flux_start)
!
      end subroutine dealloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_data_4_fline(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      deallocate(fln_src%vector_nod_fline)
!
      end subroutine dealloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_start_point_fline(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      deallocate(fln_src%xx4_initial_fline, fln_src%flux_start_fline)
!
      end subroutine dealloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline(fln_tce)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      deallocate(fln_tce%istack_current_fline)
      deallocate(fln_tce%num_current_fline)
      deallocate(fln_tce%flux_stack_fline)
!
      deallocate(fln_tce%iline_original)
      deallocate(fln_tce%iflag_fline)
      deallocate(fln_tce%icount_fline)
      deallocate(fln_tce%isf_fline_start)
      deallocate(fln_tce%xx_fline_start)
      deallocate(fln_tce%v_fline_start)
      deallocate(fln_tce%c_fline_start)
      deallocate(fln_tce%trace_length)
!
      deallocate(fln_tce%id_fline_export)
      deallocate(fln_tce%fline_export)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
