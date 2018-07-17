!t_source_of_filed_line.f90
!
!      module t_source_of_filed_line
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine alloc_local_start_grp_num(num_fline, fline_src)
!!      subroutine alloc_local_start_grp_item(num_fline, fline_src)
!!      subroutine alloc_local_data_4_fline(num_fline, node, fline_src)
!!        type(node_data), intent(in) :: node
!!      subroutine alloc_start_point_fline                              &
!!     &         (ntot_each_field_line, fline_src)
!!      subroutine alloc_num_gl_start_fline                             &
!!     &         (nprocs, num_fline, ntot_each_field_line, fline_tce)
!!      subroutine dealloc_local_data_4_fline(fline_src)
!!      subroutine dealloc_local_start_grp_item(fline_src)
!!      subroutine dealloc_start_point_fline(fline_src)
!!      subroutine dealloc_num_gl_start_fline(fline_tce)
!
      module t_source_of_filed_line
!
      use m_precision
!
      implicit  none
!
!
      type all_fieldline_source
        real(kind = kreal), allocatable :: vector_nod_fline(:,:,:)
        real(kind = kreal), allocatable :: color_nod_fline(:,:)
!
        integer(kind = kint) :: ntot_ele_start_grp
        integer(kind = kint), allocatable :: istack_ele_start_grp(:)
        integer(kind = kint), allocatable :: nele_start_grp(:)
        integer(kind = kint), allocatable :: iele_start_item(:,:)
        real(kind = kreal),   allocatable :: flux_start(:)
!
        integer(kind = kint), allocatable :: num_line_local(:)
!
        real(kind = kreal),   allocatable :: xx_start_fline(:,:)
        real(kind = kreal),   allocatable :: flux_start_fline(:)
      end type all_fieldline_source
!
      type each_fieldline_trace
        integer(kind = kint), allocatable :: istack_current_fline(:)
        integer(kind = kint), allocatable :: num_current_fline(:)
!
        integer(kind= kint), allocatable :: icount_fline(:)
        integer(kind= kint), allocatable :: isf_fline_start(:,:)
        real(kind = kreal), allocatable ::  xx_fline_start(:,:)
        real(kind = kreal), allocatable ::  v_fline_start(:,:)
        real(kind = kreal), allocatable ::  c_fline_start(:)
!
        integer(kind= kint), allocatable :: id_fline_export(:,:)
        real(kind = kreal), allocatable ::  fline_export(:,:)
      end type each_fieldline_trace
!
      type all_fieldline_trace
        integer(kind = kint) :: ntot_gl_fline = 0
!
        integer(kind = kint), allocatable :: istack_all_fline(:)
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!
        integer(kind= kint), allocatable :: iflag_fline(:)
      end type all_fieldline_trace
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_num(num_fline, fline_src)
!
      integer(kind = kint), intent(in) :: num_fline
      type(all_fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%nele_start_grp(num_fline))
      allocate(fline_src%istack_ele_start_grp(0:num_fline))
!
      allocate(fline_src%num_line_local(num_fline))
!
      fline_src%nele_start_grp =       0
      fline_src%istack_ele_start_grp = 0
!
      fline_src%num_line_local = 0
!
      end subroutine alloc_local_start_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(num_fline, fline_src)
!
      integer(kind = kint), intent(in) :: num_fline
      type(all_fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: num
!
!
      fline_src%ntot_ele_start_grp                                      &
     &            = fline_src%istack_ele_start_grp(num_fline)
      num = fline_src%ntot_ele_start_grp
!
      allocate(fline_src%iele_start_item(2,num))
      allocate(fline_src%flux_start(num))
      if(num .gt. 0) fline_src%iele_start_item = 0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_data_4_fline(num_fline, node, fline_src)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: num_fline
      type(node_data), intent(in) :: node
      type(all_fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%vector_nod_fline(node%numnod,3,num_fline))
      allocate(fline_src%color_nod_fline(node%numnod,num_fline))
!
      fline_src%vector_nod_fline = 0.0d0
      fline_src%color_nod_fline =  0.0d0
!
      end subroutine alloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_start_point_fline                                &
     &         (ntot_each_field_line, fline_src)
!
      integer(kind = kint), intent(in) :: ntot_each_field_line
      type(all_fieldline_source), intent(inout) :: fline_src
!
!
      allocate(fline_src%xx_start_fline(3,ntot_each_field_line))
      allocate(fline_src%flux_start_fline(ntot_each_field_line))
!
      fline_src%xx_start_fline = 0.0d0
      fline_src%flux_start_fline =  0.0d0
!
      end subroutine alloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_gl_start_fline(nprocs, num_fline,            &
     &          num_each_field_line, ntot_each_field_line,              &
     &          fln_tce, fline_tce)
!
      integer(kind = kint), intent(in) :: num_fline, nprocs
      integer(kind = kint), intent(in) :: num_each_field_line(num_fline)
      integer(kind = kint), intent(in) :: ntot_each_field_line
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(all_fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: num, i
!
!
      write(*,*) 'ntot_each_field_line', ntot_each_field_line
      write(*,*) 'num_each_field_line', num_each_field_line
      do i = 1, num_fline
        allocate(fln_tce(i)%istack_current_fline(0:nprocs))
        allocate(fln_tce(i)%num_current_fline(nprocs))
        fln_tce(i)%istack_current_fline = 0
        fln_tce(i)%num_current_fline =    0
!
        num = 2*num_each_field_line(i)
        allocate(fln_tce(i)%icount_fline(num))
        allocate(fln_tce(i)%isf_fline_start(3,num))
!
        allocate(fln_tce(i)%xx_fline_start(3,num))
        allocate(fln_tce(i)%v_fline_start(3,num))
        allocate(fln_tce(i)%c_fline_start(num))
!
        fln_tce(i)%icount_fline = 0
        fln_tce(i)%isf_fline_start = 0
        fln_tce(i)%v_fline_start =  0.0d0
        fln_tce(i)%c_fline_start =  0.0d0
        fln_tce(i)%xx_fline_start = 0.0d0
!
        allocate(fln_tce(i)%id_fline_export(7,num))
        allocate(fln_tce(i)%fline_export(7,num))
        fln_tce(i)%id_fline_export = 0
        fln_tce(i)%fline_export = 0.0d0
      end do
!
      allocate(fline_tce%istack_all_fline(num_fline))
      allocate(fline_tce%flux_stack_fline(0:nprocs))
!
      num = 2*ntot_each_field_line
      allocate(fline_tce%iflag_fline(num))
!
      fline_tce%ntot_gl_fline =    0
      fline_tce%istack_all_fline = 0
      fline_tce%flux_stack_fline = 0.0d0
!
      fline_tce%iflag_fline =  0
!
      end subroutine alloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_start_grp_item(fline_src)
!
      type(all_fieldline_source), intent(inout) :: fline_src
!
!
      deallocate(fline_src%nele_start_grp)
      deallocate(fline_src%istack_ele_start_grp)
      deallocate(fline_src%iele_start_item, fline_src%flux_start)
      deallocate(fline_src%num_line_local)
!
      end subroutine dealloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_data_4_fline(fline_src)
!
      type(all_fieldline_source), intent(inout) :: fline_src
!
!
      deallocate(fline_src%vector_nod_fline, fline_src%color_nod_fline)
!
      end subroutine dealloc_local_data_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_start_point_fline(fline_src)
!
      type(all_fieldline_source), intent(inout) :: fline_src
!
      deallocate(fline_src%xx_start_fline, fline_src%flux_start_fline)
!
      end subroutine dealloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_gl_start_fline                             &
     &          (num_fline, fln_tce, fline_tce)
!
      integer(kind = kint), intent(in) :: num_fline
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(all_fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_fline
        deallocate(fln_tce(i)%istack_current_fline)
        deallocate(fln_tce(i)%num_current_fline)
!
        deallocate(fln_tce(i)%icount_fline)
        deallocate(fln_tce(i)%isf_fline_start)
        deallocate(fln_tce(i)%xx_fline_start)
        deallocate(fln_tce(i)%v_fline_start)
        deallocate(fln_tce(i)%c_fline_start)
!
        deallocate(fln_tce(i)%id_fline_export)
        deallocate(fln_tce(i)%fline_export)
      end do
!
      deallocate(fline_tce%istack_all_fline)
      deallocate(fline_tce%flux_stack_fline)
!
      deallocate(fline_tce%iflag_fline)
!
      end subroutine dealloc_num_gl_start_fline
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
